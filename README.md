# A systematic framework for spatiotemporal modelling of COVID-19 disease

This code allows to calculate a variety of epidemiological measures used in spatiotemporal modelling of the COVID-19 disease. We introduce a new risk estimate that is biased neither by the differences in population sizes nor by the spatial heterogeneity of testing.

For rapid investigation of the interactive plots used in the paper, please visit https://michalmichalak997.shinyapps.io/shiny_corona/. It may need up to one minute to get the figures visualized. 

For those wanting to try it out:

1. Please install R and RStudio and create RShiny project and install the following R packages: dplyr, ggplot2, ggpubr, reshape2, tibble, sf, tmap, broom, plotly and magrittr. To run the ShinyApp you will need also to install additional packages: shiny, rsconnect, devtools and XML.
2. Download the raw files and the computer code which is split into three .R files https://github.com/michalmichalak997/COVID-19/blob/master/Data_and_code/Data_and_code.zip).
3. Replace the content from the default .app file with the content from the downloaded .app file.
4. Place the helper.R and plots.R files in the /graber folder.
5. Place the files: series.csv, tests.csv, poland_tests.csv and "regions" files in the /data folder.
6. Run helper.R file then run plots.R file.
7. Click the ShinyApp in the app.R file.

You may also download data that are used for generating individual figures https://github.com/michalmichalak997/COVID-19/blob/master/Data_and_code/Data_for_figures.zip

The code has been developed in R programming language (R version 3.6.1 (2019-07-05)). It was tested on the following PC:
System: Windows 10 Home
Intel(R) Core(TM) i7-7500U CPU @ 2.70 GHz 2.90 GHz
RAM: 16GB

If you have questions about the code or the procedure, please contact mimichalak@us.edu.pl.

#Update December 4, 2020

We had a bug in the code (helper.R file): when calculating LPR and GPR we used a truncated data frame. Please use Data and code_December_4_update and Data for figures_December_4_update catalogues.
