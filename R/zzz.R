.onAttach <- function(libname, pkgname) {
    res <- system("gdalinfo --help-general", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (res != 0)
        warning("GDAL Utilities not found!")
}

utils::globalVariables(c(".", "%>%"))

