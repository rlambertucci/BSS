arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments) != 5L || !arguments[5] %in% c('dependencies', 'module'))
    stop('Usage: check-runtime-dependencies.R DESCRIPTION module-library base-library report-prefix dependencies|module')

description <- read.dcf(arguments[1])
module_library <- normalizePath(arguments[2], winslash='/', mustWork=TRUE)
base_library <- normalizePath(arguments[3], winslash='/', mustWork=TRUE)
.libPaths(c(module_library, base_library), include.site=FALSE)
allowed <- normalizePath(c(module_library, base_library, .Library), winslash='/', mustWork=TRUE)
if (!setequal(tolower(.libPaths()), tolower(allowed)))
    stop('Runtime library isolation failed.')

dependencies <- function(record, fields=c('Depends', 'Imports', 'LinkingTo')) {
    text <- unlist(record[1, intersect(fields, colnames(record)), drop=FALSE], use.names=FALSE)
    text <- text[!is.na(text) & nzchar(text)]
    if (!length(text)) return(character())
    trimws(unlist(strsplit(text, ',', fixed=TRUE), use.names=FALSE))
}

pending <- dependencies(description)
owners <- rep(description[1, 'Package'], length(pending))
records <- list()
seen <- character()
failures <- character()
edges <- list()
while (length(pending)) {
    specification <- pending[1]
    owner <- owners[1]
    pending <- pending[-1]
    owners <- owners[-1]
    parts <- regmatches(specification, regexec('^([[:alnum:].]+)(?:\\s*\\((>=|<=|==|!=|>|<)\\s*([^()]+)\\))?$', specification, perl=TRUE))[[1]]
    if (!length(parts)) stop('Invalid dependency specification: ', specification)
    name <- parts[2]
    operator <- parts[3]
    required <- trimws(parts[4])
    edges[[length(edges) + 1L]] <- data.frame(package=name, requiredBy=owner, specification=specification)
    if (name == 'R') {
        version <- as.character(getRversion())
    } else {
        location <- find.package(name, lib.loc=.libPaths(), quiet=TRUE)
        if (!length(location)) {
            failures <- c(failures, paste(owner, 'requires missing', specification))
            next
        }
        metadata <- read.dcf(file.path(location, 'DESCRIPTION'))
        version <- metadata[1, 'Version']
        if (!name %in% seen) {
            seen <- c(seen, name)
            value <- function(field) if (field %in% colnames(metadata)) metadata[1, field] else ''
            records[[length(records) + 1L]] <- data.frame(package=name, version=version,
                library=normalizePath(dirname(location), winslash='/', mustWork=TRUE),
                built=value('Built'), repository=value('Repository'), license=value('License'))
            needed <- dependencies(metadata)
            pending <- c(pending, needed)
            owners <- c(owners, rep(name, length(needed)))
        }
    }
    if (nzchar(operator)) {
        difference <- utils::compareVersion(version, required)
        valid <- switch(operator, '>='=difference >= 0, '<='=difference <= 0,
            '=='=difference == 0, '!='=difference != 0, '>'=difference > 0, '<'=difference < 0)
        if (!valid) failures <- c(failures, paste(owner, 'requires', specification, 'but found', version))
    }
}
if (length(records)) write.table(do.call(rbind, records), paste0(arguments[4], '-packages.tsv'), sep='\t', row.names=FALSE, quote=TRUE)
if (length(edges)) write.table(do.call(rbind, edges), paste0(arguments[4], '-edges.tsv'), sep='\t', row.names=FALSE, quote=TRUE)
if (length(failures)) stop(paste(unique(failures), collapse='\n'))
cat('PASS dependency closure:', length(seen), 'packages; version constraints satisfied.\n')
for (name in sort(seen)) loadNamespace(name, lib.loc=.libPaths())
cat('PASS all dependency namespaces load with module + base + R standard only.\n')

if (arguments[5] == 'module') {
    scope <- loadNamespace(description[1, 'Package'], lib.loc=module_library)
    get('initProtoBuf', asNamespace('jmvcore'))()
    stems <- c('art', 'correlationComparison', 'dataimputation', 'DataPresentation',
        'dataTransform', 'DDA', 'effectSize', 'networkanalysis', 'outliers', 'samplesize')
    for (stem in stems) {
        analysis <- scope[[paste0(stem, 'Class')]]$new(options=scope[[paste0(stem, 'Options')]]$new(), data=data.frame())
        stopifnot(inherits(analysis, paste0(stem, 'Class')))
    }
    cat('PASS ten installed analysis constructors.\n')
    set.seed(116)
    data <- data.frame(subject=sample(seq_len(60)), x=rnorm(60), y=rnorm(60), visit2=rnorm(60))
    data$y[seq(3, 60, 9)] <- NA_real_
    data$visit2[seq(6, 60, 11)] <- NA_real_
    cases <- list(
        list(mode='mice_cs', vars_mice_cs='y', predictor_vars_mice_cs='x',
            imputation_method_mice_cs='norm', run_imputation_mice_cs=TRUE,
            num_imputations_mice_cs=2L, maxiter_mice_cs=2L, use_seed_mice_cs=TRUE, seed_mice_cs=116L),
        list(mode='mice_long', data_format_mice_long='wide', subject_id_var_mice_long='subject', wave_vars_mice_long=c('y', 'visit2'),
            imputation_method_mice_long='2l.pmm', run_imputation_mice_long=TRUE,
            num_imputations_mice_long=2L, maxiter_mice_long=2L, use_seed_mice_long=TRUE, seed_mice_long=116L),
        list(mode='missranger', vars_mr='y', predictor_vars_mr='x', run_imputation_mr=TRUE,
            num_trees_mr=10L, maxiter_mr=2L, use_seed_mr=TRUE, seed_mr=116L))
    for (options in cases) {
        analysis <- scope$dataimputationClass$new(options=do.call(scope$dataimputationOptions$new, options), data=data)
        analysis$run()
        suffix <- if (options$mode == 'missranger') 'mr' else options$mode
        stopifnot(analysis$results[[paste0('imputedData_', suffix)]]$rowCount > 0L)
        cat('PASS installed imputation:', options$mode, '\n')
    }
}
for (name in loadedNamespaces()) {
    if (name == 'base') next
    library <- normalizePath(dirname(getNamespaceInfo(asNamespace(name), 'path')), winslash='/', mustWork=TRUE)
    if (!tolower(library) %in% tolower(allowed)) stop('Namespace escaped runtime isolation: ', name, ' at ', library)
}
cat('PASS loaded namespace provenance restricted to the three allowed libraries.\n')
