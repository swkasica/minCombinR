## minCombinR: Minimal specification syntax for static charts and combinations in R

1. [Overview](#overview)
    * [Project Status](#project-status)
2. [Getting Started](#getting-started)
    * [Installing minCombinR](#install-mincombinR)
    * [Special Dependencies](#special-dependencies)
3. [Basic Usage](#basic-usage)
3. [Detailed Examples](#detailed-examples)


### Overview

minCombinR was inspired by our prior research to understand how analytic experts in public health visualized their data. This prior study developed a genomic epidemiology visualization typology (GEViT) to describe and systematically appraise the diversity and usage of different types of visualizations; details results are in the [GEViT paper](http://dx.doi.org/10.1093/bioinformatics/bty832). Through the GEViT study we realized that there were a number of gaps with existing tools, and we have developed minCombinR to address those unmet needs.

minCombinR implements an architecture for generating static charts and complex combinations using a minimal specification syntax. We are currently in the process of writing a manuscript that details our theoretical and technical contributions of our architecture. 

#### Project Status
minCombinR is currently stable research code. We are in the process of testing it, finding edge cases, and defining reasonable limits of what we support. Those of you that wish to download and play around with minCombinR are welcomed to do so. Please use the issues in this repository tell us what you've found.


### Getting started
To get started with minCombinR, use the devtools package to install the most recent version from github:

```R
devtools::install_github("amcrisan/mincombinr")
```

Please note: while the *install_github* command will download most of the package dependencies for mincombinr there are two special dependencies that you must install. We recommend that you install those special dependencies *before* you install mincombinr. See the section below for more details:

#### Special dependencies

There are some dependencies that you must install on your own as these are not automatically installed. 

**ggtree**
The ggtree package is used for visualizing phylogenetic tree data. The ggtree package is distributed through bioconductor, which is *not automatically downloaded* as part of minCombinR dependencies. Please install the ggtree package before you install minCombinR

**magick**
For image data, minCombinR uses the [magick](https://cran.r-project.org/web/packages/magick/vignettes/intro.html) package in R. This package has some *system specific installations* that you must install in order for it to work.

As a precaution, please install magick package *ahead of* install minCombinR

### Basic usage

minCombinR lets you create complex visualizations with a small number of commands. Here's a worked example for some simple charts. This example uses Ebola Data from [microreact](https://microreact.org/project/west-african-ebola-epidemic?tt=rc). 

```R
library(mincombinr)
library(dplyr)

# ---- Data Input ----
#input data you data using : input_data
tab_dat<-input_data(file = system.file("extdata", "ebov_metadata.csv", package = "mincombinr"),dataType = "table")

tree_dat<-input_data(file = system.file("extdata", "ebov_tree.nwk", package = "mincombinr"),dataType = "tree")

# ---- Single Charts ----
#create basic single charts using : specify_base
phyloTree_chart<-specify_base(chart_type = "phylogenetic tree",data="tree_dat")

scatter_chart<-specify_base(chart_type = "scatter",data="tab_dat",x = "month",y="site.id")

scatter_chart_two<-specify_base(chart_type = "scatter",data="tab_dat",x = "country",y="site.id",title="Cases by country")

#plot single charts using the plot command
plot(scatter_chart_two)

# ---- COMBINATIONS ----
#create combinations using : specify_combination
composite_combo<-specify_combination(combo_type = "composite", base_charts = c("phyloTree_chart","scatter_chart_two","scatter_chart"),link_by="country")

plot(composite_combo)

```
![image](https://user-images.githubusercontent.com/5395870/53119839-3a41a500-3505-11e9-902d-68f7f8c8e891.png)


### Detailed examples

There are more complex worked examples in the /inst/examples folder. These are r markdown notebooks that show different ways that minCombinR can be used.
