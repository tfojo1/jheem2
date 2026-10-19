# Data-manager identity and the registry of data managers by name.
#   Rscript tests/data_manager_provenance.R     (with jheem2 installed)
# JHEEM2_TEST_LIBRARY, if set, is a library to load jheem2 from first.
library.path = Sys.getenv("JHEEM2_TEST_LIBRARY", unset = "")
if (nzchar(library.path)) .libPaths(c(library.path, .libPaths()))
suppressPackageStartupMessages(library(jheem2))

expect.error = function(expr, pattern)
{
    message = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(message) || !grepl(pattern, message))
        stop("expected an error matching '", pattern, "', got: ", if (is.null(message)) "no error" else message)
}
directory = tempfile("jheem2-provenance-")
dir.create(directory)
save.manager = function(data.manager, name)
{
    file = file.path(directory, name)
    saved = data.manager
    save(saved, file = file)
    file
}

#-- A manager created in memory has no provenance --#
fresh = create.data.manager("test.manager", description = "A test manager")
stopifnot(is.null(get.data.manager.provenance(fresh)))

#-- Loading records the file and its digest; nothing is modified yet --#
file = save.manager(fresh, "test.manager.rdata")
loaded = load.data.manager(file)
provenance = get.data.manager.provenance(loaded)
stopifnot(identical(provenance$file, "test.manager.rdata"),
          identical(provenance$md5, unname(tools::md5sum(file))),
          is.null(provenance$sha256), is.null(provenance$release), is.null(provenance$repository),
          inherits(provenance$loaded.at, "POSIXct"),
          identical(provenance$modified, FALSE),
          identical(loaded$name, "test.manager"))

#-- The caller can add the release identity it verified --#
sha = paste(rep("a", 64), collapse = "")
released = load.data.manager(file, provenance = list(sha256 = sha, release = "test-manager-v2026.10.19",
                                                     repository = "example/repository"))
provenance = get.data.manager.provenance(released)
stopifnot(identical(provenance$sha256, sha),
          identical(provenance$release, "test-manager-v2026.10.19"),
          identical(provenance$repository, "example/repository"),
          identical(provenance$md5, unname(tools::md5sum(file))))
expect.error(load.data.manager(file, provenance = list(tag = "x")), "named list")
expect.error(load.data.manager(file, provenance = list(sha256 = "not-a-digest")), "64 lowercase")
expect.error(load.data.manager(file, provenance = list(release = c("a", "b"))), "single")

#-- Changing contents marks the identity as modified; copies keep the identity --#
copied = copy.data.manager(released, name = "copied.manager", description = "A copy")
stopifnot(identical(get.data.manager.provenance(copied), get.data.manager.provenance(released)))
# (The exported wrapper register.parent.data.source() calls register.source(); use the method.)
released$register.parent.source(parent.source = "PARENT", full.name = "A parent source", short.name = "Parent")
stopifnot(identical(get.data.manager.provenance(released)$modified, TRUE),
          identical(get.data.manager.provenance(copied)$modified, FALSE))
copied.after = copy.data.manager(released, name = "copied.after", description = "A later copy")
stopifnot(identical(get.data.manager.provenance(copied.after)$modified, TRUE))

#-- Saving and loading again gives a fresh record for the new file --#
file.2 = save.manager(released, "second.manager.rdata")
reloaded = load.data.manager(file.2)
provenance = get.data.manager.provenance(reloaded)
stopifnot(identical(provenance$file, "second.manager.rdata"),
          identical(provenance$md5, unname(tools::md5sum(file.2))),
          identical(provenance$modified, FALSE), is.null(provenance$release))

#-- An object from an earlier class definition has no provenance field --#
EARLIER = R6::R6Class("jheem.data.manager", private = list(i.name = "earlier"))
stopifnot(is.null(get.data.manager.provenance(EARLIER$new())))
expect.error(get.data.manager.provenance(list()), "jheem.data.manager")

#-- The registry --#
stopifnot(identical(nrow(list.data.managers()), 0L) || all(!list.data.managers()$name %in% c("census", "syphilis")))
returned = register.data.manager("census", loaded)
stopifnot(identical(returned, loaded), identical(get.data.manager("census"), loaded))
register.data.manager("syphilis", released)
listing = list.data.managers()
stopifnot(all(c("census", "syphilis") %in% listing$name),
          identical(listing$release[listing$name == "syphilis"], "test-manager-v2026.10.19"),
          identical(listing$modified[listing$name == "syphilis"], TRUE),
          identical(listing$file[listing$name == "census"], "test.manager.rdata"))
register.data.manager("census", reloaded)
stopifnot(identical(get.data.manager("census"), reloaded))
expect.error(get.data.manager("hiv.surveillance"), "no data manager is registered as 'hiv.surveillance'")
expect.error(get.data.manager("hiv.surveillance"), "'census'")
expect.error(register.data.manager("1census", loaded), "starting with a letter")
expect.error(register.data.manager("census", list()), "jheem.data.manager")
expect.error(get.data.manager(c("a", "b")), "single name")

#-- Unchanged behavior: the default manager is separate from the registry --#
set.default.data.manager(fresh)
stopifnot(identical(get.default.data.manager(), fresh))

unlink(directory, recursive = TRUE)
cat("Data-manager provenance and registry tests passed\n")
