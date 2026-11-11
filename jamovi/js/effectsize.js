'use strict';

function updateReferences(ui) {
    const variables = ui.indep_vars_cs.value() || [];
    const current = ui.glass_references.value() || [];
    const references = variables.map(variable => {
        const previous = current.find(reference => reference.variable === variable);
        return previous || { variable, reference: null };
    });
    if (JSON.stringify(current) !== JSON.stringify(references))
        ui.glass_references.setValue(references);
    ui.glass_references.applyToItems(0, (item, index, column) => {
        if (column === 1 && references[index])
            item.setPropertyValue('variable', references[index].variable);
    });
}

module.exports = { updateReferences };
