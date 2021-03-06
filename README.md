
<!-- README.md is generated from the source: README.Rmd -->

# distcomp

[![Travis build
status](https://travis-ci.org/bnaras/distcomp.svg?branch=master)](https://travis-ci.org/bnaras/distcomp)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/distcomp)](https://cran.r-project.org/package=distcomp)
[![](https://cranlogs.r-pkg.org/badges/distcomp)](https://cran.r-project.org/package=distcomp)

This package is described in detail in the paper [*Software for
Distributed Computation on Medical Databases: A Demonstration
Project.*](https://doi.org/10.18637/jss.v077.i13)

## Installation

Install this package the usual way in R or via:

``` r
library(devtools)
install_github("bnaras/distcomp")
```

Then, you will find a document that describes several examples installed
under the R library tree. For example:

``` r
list.files(system.file("doc", package = "distcomp"))
list.files(system.file("doc_src", package = "distcomp"))
```

The examples described in the reference below are available as follows:

``` r
list.files(system.file("ex", package = "distcomp"))
```

Use of this package requires some configuration. In particular, to run
the examples on a local machine where a single `opencpu` server will be
emulating several sites, a suitable R profile needs to be set up. That
profile will be something along the lines of

``` r
library(distcomp)
distcompSetup(workspace = "full_path_to_workspace_directory",
              ssl_verifyhost = 0L, ssl_verifypeer = 0L)
```

where the workspace is a directory that the `opencpu` server can
serialize objects to. On Unix or Mac, the above can be inserted into an
`.Rprofile` file, but on Windows, we find that the `Rprofile.site` file
needs to contain the above lines.

The effect of this is that *every R* process (including the `opencpu`
process) has access to the `distcomp` library and the workspace.

## Prototyping New Computations

Refer to the vignette in the package for some tips on developing new
distributed computations.

## Examples

Please refer to the following gists to see examples of computations.

  - [Rank K
    SVD](https://gist.github.com/emcramer/515a13bdcfcdca02a6d9b8149d5c07ec)

## References

Balasubramanian Narasimhan and Daniel Rubin and Samuel Gross and Marina
Bendersky and Philip Lavori. Software for Distributed Computation on
Medical Databases: A Demonstration Project. Journal of Statistical
Software, Volume 77, Issue 13, (2017).
[DOI](https://dx.doi.org/10.18637/jss.v077.i13)
