##' List the names of all packages installed.
##'
##' @title List all packages
##' @return A vector consisting of all packages installed.
##' @author Ruiyang Wu
##' @export
pkg_list <- function() {
  return(row.names(utils::installed.packages(.libPaths()[1])))
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
  utils::installed.packages(.libPaths()[1])[pkg, ]
}

##' List the names of all dependency packages installed.
##'
##' @title List all dependency packages
##' @return A vector consisting of all dependency packages installed.
##' @author Ruiyang Wu
pkg_deps <- function() {
  pkg_info <- utils::installed.packages(.libPaths()[1])
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

##' Purge an installed package and its dependencies
##'
##' @title Purge a package
##' @param pkg The package to be purged
##' @return The purged package name (invisible)
##' @author Ruiyang Wu
##' @export
pkg_purge <- function(pkg) {
  leaves <- pkg_leaves()
  if (!(pkg %in% leaves))
    stop(paste(pkg, "is not installed or is needed by other packages!"))
  pkgs_to_keep <- setdiff(leaves, pkg)
  pkgs_to_rm <- pkg
  while (length(pkgs_to_rm) > 0) {
    utils::remove.packages(pkgs_to_rm, .libPaths()[1])
    pkgs_to_rm <- setdiff(pkg_leaves(), pkgs_to_keep)
  }
  return(invisible(pkg))
}
