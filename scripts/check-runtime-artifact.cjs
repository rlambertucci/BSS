'use strict';

const fs = require('node:fs');
const path = require('node:path');
const childProcess = require('node:child_process');
const crypto = require('node:crypto');
const { createRequire } = require('node:module');

function checkDependencies({ rscript, description, library, baseLibrary, prefix, environment, phase }) {
    const result = childProcess.spawnSync(rscript, ['--vanilla', path.join(__dirname, 'check-runtime-dependencies.R'),
        description, library, baseLibrary, prefix, phase], {
        env: { ...environment, R_LIBS: library, R_LIBS_SITE: baseLibrary, R_LIBS_USER: 'notthere' },
        encoding: 'utf8', maxBuffer: 20000000
    });
    fs.writeFileSync(prefix + '.log', (result.stdout || '') + (result.stderr || ''), { flag: 'wx' });
    if (result.error) throw result.error;
    if (result.status !== 0) throw new Error('Runtime dependency gate failed; inspect ' + prefix + '.log');
    const reports = Object.fromEntries(['.log', '-packages.tsv', '-edges.tsv'].map(suffix => {
        const filename = prefix + suffix;
        return [suffix, { path: filename, sha256: crypto.createHash('sha256').update(fs.readFileSync(filename)).digest('hex') }];
    }));
    return { phase, libraries: [library, baseLibrary, 'bundled R standard library'], reports };
}

async function checkArtifact({ artifact, compiler, directory, rscript, baseLibrary, environment }) {
    const Zip = createRequire(compiler)('jszip');
    const archive = await Zip.loadAsync(fs.readFileSync(artifact));
    const library = path.join(directory, 'runtime-library');
    fs.mkdirSync(library);
    for (const [name, entry] of Object.entries(archive.files)) {
        if (entry.dir || !name.startsWith('BSS/R/')) continue;
        const filename = path.resolve(library, name.slice('BSS/R/'.length));
        const relative = path.relative(library, filename);
        if (relative.startsWith('..' + path.sep) || relative === '..' || path.isAbsolute(relative)) throw new Error('Unsafe module entry: ' + name);
        fs.mkdirSync(path.dirname(filename), { recursive: true });
        fs.writeFileSync(filename, await entry.async('nodebuffer'), { flag: 'wx' });
    }
    return checkDependencies({ rscript, description: path.join(library, 'BSS', 'DESCRIPTION'), library,
        baseLibrary, environment, phase: 'module', prefix: path.join(directory, 'artifact-dependencies') });
}

module.exports = { checkDependencies, checkArtifact };
