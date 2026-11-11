# Keyboard assignment in Data Imputation

The editor retains one principal variable supplier per imputation method, one owning control per option, and the existing independent long/wide selections. No client patch, duplicate option binding or custom DOM/focus handler is required.

## Native transfer sequence

1. Focus the source variable list and select the required variable(s). Up/Down moves the selection in that list; the native multi-selection commands can be used where available.
2. Press Tab until a transfer arrow has focus. A search button may receive focus first.
3. While an arrow has focus, use Down or Up to move between destination arrows. Choose the arrow next to the intended field.
4. Press Enter to assign the selected variable(s) to that destination.

This works for later destinations as well as the first one. The arrow's accessible name identifies its action, selected variable(s) and destination. Mouse arrows and drag-and-drop remain available.

## Why repeated Tab is different

Tab traverses both arrows and destination lists. In the tested jamovi client, focusing a destination list clears the source selection and changes the active transfer context. Continuing to a later arrow with Tab therefore does not preserve the earlier source selection. Use Up/Down while on an arrow to choose a later destination without entering intermediate lists.

This documents the native navigation route; it does not claim to change the client's Tab behavior, fix every accessibility issue, or add double-click transfer. If the source selection has already been cleared, return to the source, select the variable(s) again, and follow the arrow-navigation sequence.

## Formats and saved selections

Use Wave Columns for wide repeated measures, and Variables to Impute plus Time / Wave for native-long data. Subject ID, predictors, group/cluster and fixed covariates retain their existing roles. Switching methods or formats must not erase their selections. The modeling supplier is separate from the main imputation supplier.

The shared longitudinal source includes ID columns so they can be assigned to Subject ID. Each destination retains its own permitted types; this does not allow an ID column to be used as an imputation variable or predictor where that destination excludes it.

Validated against the Windows jamovi 28.14.0.0 client. The release evidence records the exercised fields, formats, keyboard paths, persistence and native calculations; this guide is not a guarantee for every client version or assistive technology.
