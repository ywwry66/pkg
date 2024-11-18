##' Initialization
pkg_init <- function() {
  pkg_user_installed <- character(0)

  pkg_local_path <- "~/.R/pkg/pkg.RData"
  pkg_dir_path <- dirname(pkg_local_path)
  if (!dir.exists(pkg_dir_path))
    dir.create(pkg_dir_path, recursive = TRUE)
  if (!file.exists(pkg_local_path))
    save(pkg_user_installed, file = pkg_local_path)
}

##' List the names of all packages installed.
##'
##' @title List all packages
##' @return A vector consisting of all packages installed.
##' @author Ruiyang Wu
##' @export
pkg_list <- function() {
  pkg_init()
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
  pkg_init()
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

##' Add additional packages to user-installed packages.
pkg_user_add <- function(pkgs, rm = FALSE) {
  pkg_local_path <- "~/.R/pkg/pkg.RData"
  load(pkg_local_path)
  pkg_user_installed <- intersect(pkg_user_installed, pkg_list())
  pkg_user_installed <-
    if (rm) setdiff(pkg_user_installed, pkgs)
    else union(pkg_user_installed, pkgs)
  pkg_user_installed <- sort(pkg_user_installed)
  save(pkg_user_installed, file = pkg_local_path)
  return(pkg_user_installed)
}

##' List all user-installed packages.
##'
##' @title List all user-installed packages
##' @param include_recommended A Boolean variable indicating whether
##'   "recommended" packages should be excluded from the list.
##' @return A vector consisting of all user-installed packages.
##' @author Ruiyang Wu
##' @export
pkg_list_user <- function(exclude_recommended = TRUE) {
  pkg_init()
  pkg_user_installed <- pkg_user_add(NULL)
  if (exclude_recommended) {
    pkg_info <- utils::installed.packages(.libPaths()[1])
    recommended <-
      row.names(pkg_info)[which(pkg_info[, "Priority"] == "recommended")]
    pkg_user_installed <- setdiff(pkg_user_installed, recommended)
  }
  return(pkg_user_installed)
}

##' Purge an installed package and its dependencies
##'
##' @title Purge a package
##' @param pkg The package to be purged.
##' @return The purged package name (invisible).
##' @author Ruiyang Wu
##' @export
pkg_purge <- function(pkg) {
  leaves <- pkg_leaves()
  pkg_user_installed <- pkg_user_add(NULL)
  if (!(pkg %in% leaves))
    stop(paste(pkg, "is not installed or is needed by other packages!"))
  pkgs_to_keep <- setdiff(union(leaves, pkg_user_installed), pkg)
  pkgs_to_rm <- pkg
  while (length(pkgs_to_rm) > 0) {
    utils::remove.packages(pkgs_to_rm, .libPaths()[1])
    pkgs_to_rm <- setdiff(pkg_leaves(), pkgs_to_keep)
  }
  pkg_user_add(pkg, rm = TRUE)
  return(invisible(pkg))
}

##' Install packages
##'
##' @title Install packages
##' @param pkgs A vector of packages to be installed.
##' @return A vector containing packages user requests to install
##'   (invisible).
##' @author Ruiyang Wu
##' @export
pkg_install <- function(pkgs, ...) {
  if (missing(pkgs)) stop("argument 'pkgs' is required.")
  pkg_init()
  utils::install.packages(pkgs, ...)

  ## get package names
  pkgs <- gsub("_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "",
               gsub(.standard_regexps()$valid_package_version, "",
                    basename(pkgs))) # code from install.packages function

  return(invisible(pkg_user_add(pkgs)))
}
