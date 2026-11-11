'use strict';

const fs = require('node:fs');
const crypto = require('node:crypto');
const { createRequire } = require('node:module');
const assert = require('node:assert/strict');

function digest(bytes) {
    return crypto.createHash('sha256').update(bytes).digest('hex');
}

async function compare(first, second) {
    const artifacts = [first, second].map(filename => {
        const bytes = fs.readFileSync(filename);
        const manifest = JSON.parse(fs.readFileSync(filename + '.json', 'utf8'));
        assert.equal(digest(bytes), manifest.sha256, 'Artifact does not match its manifest: ' + filename);
        return { bytes, manifest };
    });
    const [left, right] = artifacts.map(artifact => artifact.manifest);
    for (const field of ['inputs', 'dependencyInputs', 'compilerInputs']) {
        assert.ok(Array.isArray(left[field]) && left[field].length > 0, 'Missing input inventory: ' + field);
        assert.deepEqual(left[field], right[field], 'Build inputs differ: ' + field);
    }
    for (const field of ['node', 'rVersion', 'architecture', 'compilerSha256', 'launcherSha256']) {
        assert.equal(left[field], right[field], 'Build environment differs: ' + field);
    }
    for (const field of ['rRuntimeInputs', 'hostLibraryInputs', 'buildTools', 'reproducibility']) {
        if (left[field] !== undefined || right[field] !== undefined) {
            assert.ok(left[field] && right[field], 'Missing reproducibility input: ' + field);
            assert.deepEqual(left[field], right[field], 'Reproducibility inputs differ: ' + field);
        }
    }
    const zip = createRequire(left.compiler)('jszip');
    async function payload(bytes) {
        const archive = await zip.loadAsync(bytes);
        const files = [];
        for (const name of Object.keys(archive.files).sort()) {
            const entry = archive.files[name];
            if (!entry.dir) files.push({ path: name, sha256: digest(await entry.async('nodebuffer')) });
        }
        return files;
    }
    const firstPayload = await payload(artifacts[0].bytes);
    const secondPayload = await payload(artifacts[1].bytes);
    const secondHashes = new Map(secondPayload.map(entry => [entry.path, entry.sha256]));
    const differences = firstPayload.filter(entry => secondHashes.get(entry.path) !== entry.sha256).map(entry => entry.path);
    const firstNames = new Set(firstPayload.map(entry => entry.path));
    differences.push(...secondPayload.filter(entry => !firstNames.has(entry.path)).map(entry => entry.path));
    return { inputsIdentical: true, artifactIdentical: left.sha256 === right.sha256,
        payloadIdentical: differences.length === 0, entries: firstPayload.length, differences };
}

if (require.main === module) {
    if (process.argv.length !== 4) {
        console.error('Usage: node scripts/compare-native-builds.cjs <first.jmo> <second.jmo>');
        process.exitCode = 1;
    } else {
        compare(process.argv[2], process.argv[3]).then(result => {
            console.log(JSON.stringify(result, null, 2));
            if (!result.payloadIdentical || !result.artifactIdentical) process.exitCode = 1;
        }).catch(error => { console.error(error.message); process.exitCode = 1; });
    }
}

module.exports = { compare };
