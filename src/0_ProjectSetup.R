## Initiate project structure
## Created by: Paul Julian (pjulian@sccf.org/pauljulianphd@gmail.com)
## Created on: 06/14/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")

#Paths
wd="C:/Julian_LaCie/_GitHub/REMAP"
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))

# Folder.Maker(paths);#One and done. Creates folders in working directory.
# usethis::use_readme_rmd(open = rlang::is_interactive()); #one and done