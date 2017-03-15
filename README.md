[![CRAN version](http://www.r-pkg.org/badges/version/ropercenter)](https://cran.r-project.org/package=icpsrdata) ![](http://cranlogs.r-pkg.org/badges/grand-total/ropercenter) [![Travis-CI Build Status](https://travis-ci.org/fsolt/ropercenter.svg?branch=master)](https://travis-ci.org/fsolt/ropercenter)
------------------------------------------------------------------------

ropercenter
=========

The [Roper Center for Public Opinion Research](http://ropercenter.cornell.edu), in its own words, works "to collect, preserve, and disseminate public opinion data; to serve as a resource to help improve the practice of survey research; and to broaden the understanding of public opinion through the use of survey data in the United States and around the world."  It maintains the largest archive of public opinion data in existence, holding data dating back to the 1930s and from over 100 countries.  Researchers taking advantage of these datasets, however, are caught in a bind.  The terms and conditions for downloading any Roper Center dataset state that datasets "may not be resold or re-disseminated." But to ensure that one's work can be reproduced, assessed, and built upon by others, one must provide access to the raw data one employed.  

The `ropercenter` package cuts this knot by providing programmatic, reproducible access to specified Roper Center datasets from within R for [registered users](https://ropercenter.cornell.edu/make-personalized-account/) at the Roper Center's [member institutions](https://ropercenter.cornell.edu/list-of-members/). 


To install:

* the latest released version: `install.packages("ropercenter")`
* the latest development version:

```R
if (!require(ghit)) install.packages("ghit")
ghit::install_github("fsolt/ropercenter")
```

For more details, check out [the vignette](https://cran.r-project.org/package=ropercenter/vignettes/ropercenter-vignette.html).

Please recall that by using Roper Center services, you accept the Center's [Terms and Conditions](https://ropercenter.cornell.edu/CFIDE/cf/action/registration/termsAndConditions.cfm).


