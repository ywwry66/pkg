##' List the names of all packages installed.
##'
##' @title List all packages
##' @return A vector consisting of all packages installed.
##' @author Ruiyang Wu
##' @export
pkg_list <- function() {
  return(row.names(installed.packages(.libPaths()[1])))
}

##' Get information about a package such as its version, license, etc.
##'
##' @title Get information about a package
##' @param pkg The package of interest.
##' @return A named vector. It has the same entries as the output of
##'   utils::installed.packages, i.e., "Package", "LibPath",
##'   "Version", "Priority", "Depends", "Imports", "LinkingTo",
##'   "Suggests", "Enhances", "OS_type", "License" and "Built".
##' @author Ruiyang Wu
##' @export
pkg_info <- function(pkg) {
  installed.packages(.libPaths()[1])[pkg, ]
}

##' List the names of all dependency packages installed.
##'
##' @title List all dependency packages
##' @return A vector consisting of all dependency packages installed.
##' @author Ruiyang Wu
##' @export
pkg_deps <- function() {
  pkg_info <- installed.packages(.libPaths()[1])
  pkg_deps <- c(pkg_info[, "Imports"], pkg_info[, "Depends"],
                pkg_info[, "LinkingTo"])
  if (length(pkg_deps) == 0) {
    pkg_deps <- NULL
  } else {
    names(pkg_deps) <- NULL
    pkg_deps <- do.call(c, strsplit(pkg_deps, split = ",\\s"))
    pkg_deps <- do.call(c, strsplit(pkg_deps, split = "(\\s)?\\(.*\\)"))
    pkg_deps <- pkg_deps[which(!(is.na(pkg_deps) | pkg_deps == "R"))]
    pkg_deps <- sort(unique(pkg_deps))
  }
  return(pkg_deps)
}

##' List the names of all leaf packages installed.
##'
##' @title List all leaf packages
##' @return A vector consisting of all leaf packages installed.
##' @author Ruiyang Wu
##' @export
pkg_leaves <- function() {
  pkg_leaves <- setdiff(pkg_list(), pkg_deps())
  return(pkg_leaves)
}
