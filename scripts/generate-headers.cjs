'use strict';

const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const { createRequire } = require('node:module');
const { pathToFileURL } = require('node:url');

function normalizeHeader(content) {
    return content.replace(/[ \t]+(?=\r?$)/gm, '').replace(/\r?\n/g, '\r\n');
}

async function main(args) {
    if (args.length === 1 && args[0] === '--help') {
        console.log('node scripts/generate-headers.cjs --compiler <jamovi-compiler/index.js> [--check]');
        return;
    }
    let compiler;
    let check = false;
    for (let index = 0; index < args.length; index++) {
        if (args[index] === '--compiler' && !compiler && args[index + 1] && !args[index + 1].startsWith('--')) compiler = fs.realpathSync(args[++index]);
        else if (args[index] === '--check' && !check) check = true;
        else throw new Error('Invalid or duplicate option: ' + args[index]);
    }
    if (!compiler) throw new Error('--compiler is required.');
    const repository = path.resolve(__dirname, '..');
    const directory = path.join(repository, 'jamovi');
    const yaml = createRequire(compiler)('js-yaml');
    const packageName = yaml.load(fs.readFileSync(path.join(directory, '0000.yaml'), 'utf8')).name;
    const compile = (await import(pathToFileURL(path.join(path.dirname(compiler), 'compiler.js')).href)).default;
    const template = path.join(path.dirname(compiler), 'header.template');
    const temporaryRoot = fs.realpathSync(os.tmpdir());
    const work = fs.mkdtempSync(path.join(temporaryRoot, 'bss-headers-'));
    try {
        const headers = [];
        for (const filename of fs.readdirSync(directory).filter(filename => filename.endsWith('.a.yaml')).sort()) {
            const stem = filename.slice(0, -7);
            const generated = path.join(work, stem + '.h.R');
            compile(packageName, path.join(directory, filename), path.join(directory, stem + '.r.yaml'), template, generated);
            const content = normalizeHeader(fs.readFileSync(generated, 'utf8'));
            const target = path.join(repository, 'R', stem + '.h.R');
            headers.push({ target, content, changed: !fs.existsSync(target) || fs.readFileSync(target, 'utf8') !== content });
        }
        if (headers.length === 0) throw new Error('No analysis definitions found.');
        const changed = headers.filter(header => header.changed);
        if (check && changed.length > 0) {
            for (const header of changed) console.error('Generated header differs: ' + path.relative(repository, header.target));
            process.exitCode = 1;
            return;
        }
        if (!check) for (const header of changed) fs.writeFileSync(header.target, header.content);
        console.log('PASS ' + headers.length + ' official headers ' + (check ? 'verified' : 'generated') + '; normalized trailing whitespace and CRLF.');
    } finally {
        const target = fs.realpathSync(work);
        if (path.dirname(target) !== temporaryRoot || !path.basename(target).startsWith('bss-headers-')) throw new Error('Unsafe temporary cleanup target: ' + target);
        fs.rmSync(target, { recursive: true, force: true });
    }
}

if (require.main === module) {
    main(process.argv.slice(2)).catch(error => { console.error(error.message); process.exitCode = 1; });
}

module.exports = { normalizeHeader };
