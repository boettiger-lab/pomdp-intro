# Manuscript

The manuscript is prepared as an R Markdown document, `manuscript.Rmd`, and BibTeX file `citations.bib`, with references formatted following the `the-american-naturalist.csl` style file.  the `.tex` file is only an intermediate output format used for further typesetting and is not to be edited directly.  

Manuscript code chunks are limited to loading in pre-prepared `.csv` tables generated in the appendix and plotting those results.  This ensures the manuscript can always be recompiled quickly, and that the appearance of the figures can be adjusted from the manuscript `ggplot` commands as necessary but without regenerating or altering the underlying data. The manuscript uses custom fonts and colors for the figures, code should be sufficient to set these up but may cause issues in portability.  These purely aesthetic settings can also be omitted without impacting the reproducibility of the results.  

