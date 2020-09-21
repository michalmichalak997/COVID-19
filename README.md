# A systematic framework for spatiotemporal modelling of COVID-19 disease

This code allows to calculate a variety of epidemiological measures used in spatiotemporal modelling of the COVID-19 disease. We introduce a new risk estimate that is biased neither by the differences in population sizes nor by the spatial heterogeneity of testing.

For rapid investigation of the interactive plots used in the paper, please visit https://michalmichalak997.shinyapps.io/shiny_corona/. It may need up to one minute to get the figures visualized. 

For those wanting to try it out:

1. Please install R and RStudio and create RShiny project and install the following R packages: dplyr, ggplot2, ggpubr, reshape2, tibble, sf, tmap, broom, plotly and magrittr. To run the ShinyApp you will need also to install additional packages: shiny, rsconnect, devtools and XML.
2. Download the files https://github.com/michalmichalak997/COVID-19/tree/master/Data_and_code(https://github.com/michalmichalak997/COVID-19/tree/master/Data_and_code).
3. Replace the content from the default .app file with the content from the downloaded .app file.
4. Place the helper.R and plots.R files in the /graber folder.
5. Place the files: series.csv, tests.csv, poland_tests.csv and "regions" files in the /data folder.
6. Run helper.R file then run plots.R file.
7. Click the ShinyApp in the app.R file.

If you have questions about the code or the procedure, please contact mimichalak@us.edu.pl.
