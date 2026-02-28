<!-- README.md is generated from README.Rmd. Please edit that file -->

# masstools <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/masstools?color=green)](https://cran.r-project.org/package=masstools)
[![](https://img.shields.io/github/languages/code-size/tidymass/masstools.svg)](https://github.com/tidymass/masstools)
[![Dependencies](https://tinyverse.netlify.com/badge/masstools)](https://cran.r-project.org/package=masstools)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`masstools` is a part of [tidymass](https://www.tidymass.org/).

-------

# About

`masstools` is a collection of tools for mass spectrometry data processing and analysis.

# Installation

Install the released version from Bioconductor:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("masstools")
```

The development version is available from [GitHub](https://github.com/tidymass/masstools):

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("tidymass/masstools")
```

# Usage

See the package website for reference documentation and articles:
[https://masstools.tidymass.org/](https://masstools.tidymass.org/)

# Need help?

If you have questions or want to report a bug, please use
[GitHub Issues](https://github.com/tidymass/masstools/issues) or email
<xiaotao.shen@outlook.com>.

# Citation

If you use `masstools` in your publications, please cite this paper:

Shen, X., Yan, H., Wang, C. et al. TidyMass an object-oriented reproducible analysis framework for LC–MS data. Nat Commun 13, 4365 (2022). 

[Weblink](https://www.nature.com/articles/s41467-022-32155-w)

Thanks for using `masstools`.
