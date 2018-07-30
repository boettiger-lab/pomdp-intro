plotting experiments
================
Carl Boettiger
7/4/2018

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0.9000     ✔ purrr   0.2.5     
    ## ✔ tibble  1.4.2          ✔ dplyr   0.7.6     
    ## ✔ tidyr   0.8.1          ✔ stringr 1.3.1     
    ## ✔ readr   1.1.1          ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at /Users/cboettig/Documents/boettiger-lab/pomdp-intro

``` r
library(fs)
```

``` r
knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE)
#dev=c("cairo_pdf", "png")
library(Cairo)
library(hrbrthemes)
library(extrafont)
```

    ## Registering fonts with R

``` r
library(ggthemes)
hrbrthemes::import_plex_sans()
```

    ## You will likely need to install these fonts on your system as well.
    ## 
    ## You can find them in [/Library/Frameworks/R.framework/Versions/3.5/Resources/library/hrbrthemes/fonts/plex-sans]

``` r
hrbrthemes::import_roboto_condensed()  
```

    ## You will likely need to install these fonts on your system as well.
    ## 
    ## You can find them in [/Library/Frameworks/R.framework/Versions/3.5/Resources/library/hrbrthemes/fonts/roboto-condensed]

``` r
extrafont::loadfonts()
```

    ## More than one version of regular/bold/italic found for Roboto Condensed. Skipping setup for this font.

    ## IBM Plex Sans already registered with pdfFonts().

    ## IBM Plex Sans Light already registered with pdfFonts().

    ## IBM Plex Sans Medium already registered with pdfFonts().

    ## IBM Plex Sans Text already registered with pdfFonts().

    ## IBM Plex Sans Thin already registered with pdfFonts().

``` r
ggplot2::theme_set(hrbrthemes::theme_ipsum())
scale_colour_discrete <- function(...) scale_colour_manual(..., values=ptol_pal()(3))
```

Try showing replicates individually?

Based on original, still best

![](plotting-experiments_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

![](plotting-experiments_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

![](plotting-experiments_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
