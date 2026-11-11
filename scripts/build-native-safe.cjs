'use strict';

const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const crypto = require('node:crypto');
const childProcess = require('node:child_process');
const { reproducibilityPolicy, normalizeArtifact } = require('./reproducible-native.cjs');
const { checkDependencies, checkArtifact } = require('./check-runtime-artifact.cjs');

function cleanEnvironment(environment) {
    return Object.fromEntries(Object.entries(environment).filter(([name]) =>
        !/^(LANG|LANGUAGE|LC_.*|ELECTRON_RUN_AS_NODE)$/i.test(name)));
}

function sha256(filename) {
    return crypto.createHash('sha256').update(fs.readFileSync(filename)).digest('hex');
}

function inventory(directory) {
    const records = [];
    function visit(current) {
        for (const entry of fs.readdirSync(current, { withFileTypes: true })) {
            const filename = path.join(current, entry.name);
            if (entry.isSymbolicLink()) throw new Error('Symbolic links are not supported: ' + filename);
            if (entry.isDirectory()) visit(filename);
            else records.push({ path: path.relative(directory, filename), sha256: sha256(filename) });
        }
    }
    visit(directory);
    return records.sort((left, right) => left.path < right.path ? -1 : left.path > right.path ? 1 : 0);
}

function outsideRepository(filename, repository) {
    const relative = path.relative(repository, filename);
    if (relative === '' || (!relative.startsWith('..' + path.sep) && relative !== '..' && !path.isAbsolute(relative))) {
        throw new Error('Build work and output must be outside the repository: ' + filename);
    }
}

async function main(args) {
    if (args.includes('--help')) {
        console.log('Windows: node scripts/build-native-safe.cjs --home <jamovi> --compiler <index.js> --dependencies <Rversion-arch-win64> --output <external.jmo> --source-date-epoch <Unix seconds> [--work-root <external directory>]');
        console.log('Locale check only: node scripts/build-native-safe.cjs --home <jamovi> --check');
        return;
    }
    const options = {};
    const names = new Set(['--home', '--compiler', '--dependencies', '--output', '--work-root', '--source-date-epoch']);
    for (let index = 0; index < args.length; index++) {
        const name = args[index];
        if (Object.hasOwn(options, name)) throw new Error('Duplicate option: ' + name);
        if (name === '--check') options[name] = true;
        else {
            if (!names.has(name) || !args[index + 1] || args[index + 1].startsWith('--')) throw new Error('Invalid option or missing value: ' + name);
            options[name] = args[++index];
        }
    }
    if (process.platform !== 'win32') throw new Error('This launcher targets the Windows jamovi build environment.');
    if (!options['--home']) throw new Error('--home is required.');
    const home = fs.realpathSync(options['--home']);
    const rscript = ['Frameworks/R/bin/Rscript.exe', 'Frameworks/R/bin/x64/Rscript.exe']
        .map(relative => path.join(home, relative)).find(filename => fs.existsSync(filename));
    if (!rscript) throw new Error('Bundled Rscript.exe not found under --home.');
    const environment = cleanEnvironment(process.env);
    const preflight = childProcess.spawnSync(rscript, ['--vanilla', path.join(__dirname, 'check-build-locale.R')], { env: environment, encoding: 'utf8' });
    if (preflight.stdout) process.stdout.write(preflight.stdout);
    if (preflight.stderr) process.stderr.write(preflight.stderr);
    if (preflight.error) throw preflight.error;
    if (preflight.status !== 0) throw new Error('Bundled R did not pass the UTF-8 preflight; build aborted.');
    if (options['--check']) return;
    const reproducibility = reproducibilityPolicy(options['--source-date-epoch'] ?? environment.SOURCE_DATE_EPOCH);
    environment.SOURCE_DATE_EPOCH = String(reproducibility.sourceDateEpoch);
    environment.TZ = 'UTC';
    for (const name of ['--compiler', '--dependencies', '--output']) {
        if (!options[name]) throw new Error(name + ' is required.');
    }
    const repository = fs.realpathSync(path.join(__dirname, '..'));
    const compiler = fs.realpathSync(options['--compiler']);
    const dependencies = fs.realpathSync(options['--dependencies']);
    const version = /^BSS_R_VERSION=(.+)$/m.exec(preflight.stdout)?.[1].trim();
    const architecture = /^BSS_R_ARCH=(.+)$/m.exec(preflight.stdout)?.[1].trim();
    const libraryName = 'R' + version + '-' + architecture + '-win64';
    if (path.basename(dependencies) !== libraryName) throw new Error('Dependency library must be named ' + libraryName);
    const outputParent = fs.realpathSync(path.dirname(path.resolve(options['--output'])));
    const output = path.join(outputParent, path.basename(options['--output']));
    outsideRepository(output, repository);
    if (path.extname(output).toLowerCase() !== '.jmo') throw new Error('--output must end in .jmo');
    if (fs.existsSync(output) || fs.existsSync(output + '.json')) throw new Error('Output or manifest already exists; refusing to overwrite.');
    const workRoot = fs.realpathSync(options['--work-root'] || os.tmpdir());
    outsideRepository(workRoot, repository);
    const work = fs.mkdtempSync(path.join(workRoot, 'bss-native-build-'));
    console.log('Isolated build workspace: ' + work);
    const source = path.join(work, 'BSS');
    fs.mkdirSync(source);
    for (const name of ['DESCRIPTION', 'NAMESPACE', 'LICENSE', 'README.md', 'R', 'jamovi', 'inst']) {
        const original = path.join(repository, name);
        const sourceStat = fs.lstatSync(original, { throwIfNoEntry: name !== 'inst' });
        if (!sourceStat) continue;
        if (sourceStat.isSymbolicLink()) throw new Error('Source links are not supported: ' + original);
        if (sourceStat.isDirectory()) inventory(original);
        fs.cpSync(original, path.join(source, name), { recursive: true, errorOnExist: true, force: false });
    }
    const inputs = inventory(source);
    const dependencyInputs = inventory(dependencies);
    const compilerInputs = inventory(path.dirname(compiler));
    const rRuntimeInputs = inventory(path.join(home, 'Frameworks', 'R'));
    const hostLibraryInputs = inventory(path.join(home, 'Resources', 'modules', 'base', 'R'));
    const toolNames = ['build-native-safe.cjs', 'check-build-locale.R', 'reproducible-native.cjs', 'normalize-native.R',
        'check-runtime-artifact.cjs', 'check-runtime-dependencies.R'];
    const buildTools = toolNames.map(name => ({ path: name, sha256: sha256(path.join(__dirname, name)) }));
    fs.cpSync(dependencies, path.join(source, 'build', libraryName), {
        recursive: true,
        filter: filename => filename === dependencies || !/^(BSS|00LOCK.*)$/.test(path.relative(dependencies, filename).split(path.sep)[0])
    });
    const baseLibrary = path.join(home, 'Resources', 'modules', 'base', 'R');
    const dependencyPreflight = checkDependencies({ rscript, description: path.join(source, 'DESCRIPTION'),
        library: path.join(source, 'build', libraryName), baseLibrary, environment,
        phase: 'dependencies', prefix: path.join(work, 'input-dependencies') });
    fs.writeFileSync(path.join(work, 'locale.log'), (preflight.stdout || '') + (preflight.stderr || ''));
    const log = fs.openSync(path.join(work, 'build.log'), 'wx');
    let build;
    try {
        build = childProcess.spawnSync(process.execPath, [compiler, '--build', source, '--home', home,
            '--skip-deps', '--skip-remotes', '--jmo', path.join(work, 'BSS.jmo')], {
            env: environment, cwd: work, stdio: ['ignore', log, log]
        });
    } finally {
        fs.closeSync(log);
    }
    if (build.error) throw build.error;
    if (build.status !== 0) throw new Error('Compiler failed; inspect ' + path.join(work, 'build.log'));
    for (const input of inputs) {
        if (sha256(path.join(repository, input.path)) !== input.sha256) throw new Error('Source changed during build: ' + input.path);
    }
    const rawArtifact = path.join(work, 'BSS.jmo');
    const artifact = path.join(work, 'BSS-reproducible.jmo');
    await normalizeArtifact({ artifact: rawArtifact, output: artifact, source, library: path.join(source, 'build', libraryName),
        compiler, rscript, environment: { ...environment, R_LIBS_SITE: path.join(home, 'Resources', 'modules', 'base', 'R'), R_LIBS_USER: 'notthere' },
        policy: reproducibility, log: path.join(work, 'normalization.log') });
    const artifactGate = await checkArtifact({ artifact, compiler, directory: work, rscript, baseLibrary, environment });
    for (const tool of buildTools) {
        if (sha256(path.join(__dirname, tool.path)) !== tool.sha256) throw new Error('Build tool changed during build: ' + tool.path);
    }
    for (const [directory, records] of [[dependencies, dependencyInputs], [path.dirname(compiler), compilerInputs],
        [path.join(home, 'Frameworks', 'R'), rRuntimeInputs], [path.join(home, 'Resources', 'modules', 'base', 'R'), hostLibraryInputs]]) {
        if (JSON.stringify(inventory(directory)) !== JSON.stringify(records)) throw new Error('Build input tree changed during build: ' + directory);
    }
    const manifest = {
        created: new Date().toISOString(),
        scope: 'Deterministic artifact for identical recorded inputs, runtime, build tools and source-date epoch; not a global functional homologation.',
        home, node: process.version, rVersion: version, architecture, work,
        launcherSha256: sha256(__filename), compiler, compilerSha256: sha256(compiler),
        compilerPackage: JSON.parse(fs.readFileSync(path.join(path.dirname(compiler), 'package.json'), 'utf8')),
        dependencyLibrary: dependencies, dependencyInputs, compilerInputs, rRuntimeInputs, hostLibraryInputs, buildTools, inputs, reproducibility,
        dependencyPreflight, artifactGate,
        rawArtifactSha256: sha256(rawArtifact),
        environmentPolicy: 'LANG, LANGUAGE, LC_* and ELECTRON_RUN_AS_NODE removed in child processes only.',
        sha256: sha256(artifact), bytes: fs.statSync(artifact).size
    };
    fs.writeFileSync(path.join(work, 'manifest.json'), JSON.stringify(manifest, null, 2) + '\n');
    fs.copyFileSync(artifact, output, fs.constants.COPYFILE_EXCL);
    fs.copyFileSync(path.join(work, 'manifest.json'), output + '.json', fs.constants.COPYFILE_EXCL);
    console.log('PASS isolated build: ' + output);
    console.log('SHA-256: ' + manifest.sha256);
}

if (require.main === module) {
    main(process.argv.slice(2)).catch(error => { console.error(error.message); process.exitCode = 1; });
}

module.exports = { cleanEnvironment, outsideRepository };
