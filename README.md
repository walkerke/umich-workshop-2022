# umich-workshop-2022
Slides and code for Census data workshops given at the University of Michigan in 2022

This repository contains materials for a series of workshops on using Census data in R given for the University of Michigan's Social Science Data Analysis Network in March of 2022.  

Workshop slides are available from the links below:

* March 10. 2022: [Analyzing 2020 Census Data with R and tidycensus](https://walker-data.com/umich-workshop-2022/intro-2020-census/#1)


To use the workshop materials, you should do one of the following: 

- Users new to R and RStudio should use the pre-built RStudio Cloud environment available at https://rstudio.cloud/project/3705005.  

- Advanced users familiar with R and RStudio should clone the repository to their computers with the command `git clone https://github.com/walkerke/umich-workshop-2022.git`.  They should then install the following R packages, if not already installed:

```r
pkgs <- c("tidycensus", "tidyverse", "ggridges", "geofacet")
install.packages(pkgs)
```

Experienced users should re-install __tidycensus__ to get the latest updates and ensure that all code used in the workshop will run.  

Other packages used will be picked up as dependencies of these packages on installation. 

A Census API key is required to access the Census API.  Participants should sign up for a key at https://api.census.gov/data/key_signup.html (it generally takes just a few minutes). 