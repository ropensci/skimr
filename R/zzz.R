# Create environment for storing summary functions when the package loads

functions <- new.env()

.onLoad <- function(libname, pkgname) {
  functions$default <- .default
  functions$current <- .default
}