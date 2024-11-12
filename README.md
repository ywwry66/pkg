# pkg
`pkg` provides common utilities of a package manager, such as listing all leaf/dependency packages.

## Installation
Install from GitHub using the `devtools` package:

``` R
devtools::install_github("ywwry66/pkg")
```

## Usage
The following functions are provided:
- List the names of all packages installed

``` R
pkg::pkg_list()
```

- Get information about a package such as its version, license, etc.

``` R
pkg::pkg_info(pkg)
```

- List the names of all leaf (non-dependency) packages installed

``` R
pkg::pkg_leaves()
```

- Purge an installed package and its dependencies

``` R
pkg::pkg_purge(pkg)
```
