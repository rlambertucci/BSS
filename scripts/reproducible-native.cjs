'use strict';

const fs = require('node:fs');
const path = require('node:path');
const childProcess = require('node:child_process');
const { createRequire } = require('node:module');

function reproducibilityPolicy(value) {
    if (typeof value !== 'string' || !/^[0-9]+$/.test(value)) {
        throw new Error('Provide --source-date-epoch or SOURCE_DATE_EPOCH as integer Unix seconds.');
    }
    const epoch = Number(value);
    if (!Number.isSafeInteger(epoch) || epoch < 315532800 || epoch > 4354819198) {
        throw new Error('The source-date epoch must be between 1980-01-01 and 2107-12-31T23:59:58Z (ZIP date range).');
    }
    return { version: 1, sourceDateEpoch: epoch, timestamp: new Date(epoch * 1000).toISOString(),
        namespacePath: 'BSS', zipPlatform: 'DOS', compression: 'DEFLATE', compressionLevel: 9 };
}

async function normalizeArtifact({ artifact, output, source, library, compiler, rscript, environment, policy, log }) {
    const stamp = policy.timestamp.slice(0, 19).replace('T', ' ') + ' UTC';
    const result = childProcess.spawnSync(rscript, ['--vanilla', path.join(__dirname, 'normalize-native.R'), source, library, stamp], {
        env: { ...environment, R_LIBS: library }, encoding: 'utf8'
    });
    fs.writeFileSync(log, (result.stdout || '') + (result.stderr || ''), { flag: 'wx' });
    if (result.error) throw result.error;
    if (result.status !== 0) throw new Error('R normalization failed; inspect ' + log);
    const Zip = createRequire(compiler)('jszip');
    const archive = await Zip.loadAsync(fs.readFileSync(artifact));
    const replacements = new Map();
    for (const relative of ['DESCRIPTION', 'Meta/package.rds', 'R/BSS.rdb', 'R/BSS.rdx']) {
        replacements.set('BSS/R/BSS/' + relative, fs.readFileSync(path.join(library, 'BSS', relative)));
    }
    for (const name of ['BSS/jamovi.yaml', 'BSS/jamovi-full.yaml']) {
        const entry = archive.file(name);
        if (!entry) throw new Error('Missing module metadata: ' + name);
        const text = await entry.async('string');
        if ((text.match(/^build-time:.*$/gm) || []).length !== 1) throw new Error('Expected one build-time field: ' + name);
        replacements.set(name, Buffer.from(text.replace(/^build-time:.*$/m, "build-time: '" + policy.timestamp + "'")));
    }
    for (const name of replacements.keys()) if (!archive.file(name)) throw new Error('Missing normalized payload: ' + name);
    const normalized = new Zip();
    for (const name of Object.keys(archive.files).sort()) {
        const entry = archive.files[name];
        const bytes = replacements.get(name) || await entry.async('nodebuffer');
        normalized.file(name, bytes, { dir: entry.dir, date: new Date(policy.timestamp), createFolders: false,
            dosPermissions: entry.dir ? 16 : 0, comment: '' });
    }
    const bytes = await normalized.generateAsync({ type: 'nodebuffer', platform: policy.zipPlatform,
        compression: policy.compression, compressionOptions: { level: policy.compressionLevel }, comment: '' });
    fs.writeFileSync(output, bytes, { flag: 'wx' });
}

module.exports = { reproducibilityPolicy, normalizeArtifact };
