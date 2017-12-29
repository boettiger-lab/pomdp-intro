
rmarkdown::render("appendix/appendix_harvestfirst.Rmd")
rm(list=ls())
rmarkdown::render("appendix/appendixB.Rmd")
rm(list=ls())
#rmarkdown::render("appendix/appendix.Rmd")      ## running on chameleon
rm(list=ls())

## super slow
#rmarkdown::render("appendix/appendix_99.Rmd")   ## running on 24-core
rm(list=ls())
rmarkdown::render("appendix/appendix_harvestfirst_99.Rmd")
rm(list=ls())
rmarkdown::render("appendix/appendixB_99.Rmd")
rm(list=ls())