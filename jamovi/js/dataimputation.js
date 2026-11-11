'use strict';

function migrateSeeds(ui) {
    for (const name of ['seed_mice_cs', 'seed_mice_long', 'seed_mr']) {
        const control = ui[name];
        const previous = control.value();
        if (typeof previous !== 'string')
            continue;
        const text = previous.trim();
        if (text !== '' && !/^[+-]?(?:(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?|0x[\da-f]+)$/i.test(text))
            continue;
        const sign = text.startsWith('-') ? -1 : 1;
        const value = text === '' ? 0 : sign * Number(text.replace(/^[+-]/, ''));
        if (!Number.isInteger(value) || value < -2147483647 || value > 2147483647)
            continue;
        const mode = ui['use_' + name];
        const fixed = text !== '';
        if (mode.value() !== fixed)
            mode.setValue(fixed);
        if (control.value() !== value)
            control.setValue(value);
    }
}

module.exports = { migrateSeeds };
