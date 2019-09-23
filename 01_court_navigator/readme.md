# Court Navigator

This application was originally created to assist the Court Administrator of the Philippine Supreme Court in identifying lower-level courts heavily burdened with caseflow. This version is only to demonstrate how the application works and does not include confidential information; all the values, including the branch numbers of stations, are randomly generated.

Live demo at [https://willsheo.shinyapps.io/court_navigator](https://willsheo.shinyapps.io/court_navigator).

To run this application in R, run:
```r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("willsheo/R_coding_samples", subdir = "01_court_navigator")
```


Credit: Some parts of code, such as the Javascript code, are borrowed from [https://github.com/rstudio/shiny-examples](https://github.com/rstudio/shiny-examples).
