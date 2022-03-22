# hhh4-workshop
Workshop on hhh4 and the endemic-epidemic framework

## Prerequisites for tutorial part of hhh4 workshop:

To participate in the tutorial session you will need a computer with the statistical analysis software R installed. R can be downloaded and installed from https://cran.r-project.org/. You need to install the packages "surveillance" and "hhh4addon". In the tutorial it will be assumed that you have installed R and the required packages.

To install the packages run the commands:

```r
install.packages("surveillance")
remotes::install_github("jbracher/hhh4addon")
```

Once the package is installed, you can load it via

```r
library(surveillance)
```

If you attempt to load a package without first installing it you should encounter an error message along the lines of

```
Error in library(surveillance) : there is no package called ‘surveillance’
```

Please note that certain packages require specific versions of R. You may end up with an error message like

```
Warning in install.packages :
  package ‘<package name>’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

For a description of the surveillance package run the command

```r
package?surveillance
```

To run the code for the tutorials, clone or download this repository onto
your machine using the green button at the top of this page. Open
R/RStudio and change the working directory to wherever you
cloned/downloaded the repository. Read the tutorial instructions and open
the R script provided for the tutorial to execute the code line by line.
