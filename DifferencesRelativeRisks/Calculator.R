library(DCluster)
library(tmap)
library(ggpubr)
library(plotly)
library(shiny)
library(xts)
library(magrittr)
library(foreign)
library(devtools)
library(sf)
library(tmap)
library(SiZer)
library(rgdal)
library(maps)
library(maptools)
library(sp)
library(stringr)
library(webshot)
library(RCurl)
library(magick)
library(splashr)
library(RSelenium)
library(tidyverse)
library(V8)
library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
library(XML)
library(methods)
library(rsconnect)
library(reshape2)
library(stats)
library(tibble)

regions<- c("1 WRO", "2 BYD", "3 LUB", "4 GOR", "5 LOD", "6 KRA", "7 WAR", "8 OPO", "9 RZE", "10 BIA", "11 GDA", "12 KAT",
            "13 KIE", "14 OLS", "15 POZ", "16 SZC") #labels to regions

series<-read.csv("data/series.csv", sep=";", dec=".", header = TRUE, row.names = 1, check.names = FALSE)
colnames(series)<- as.Date(colnames(series), "%d.%m.%Y" ) #loading the number of observed cases with respect to the administrative regions

tests<-read.csv("data/tests.csv", sep=";", dec=".", header = TRUE, row.names = 1, check.names = FALSE)
colnames(tests)<- as.Date(colnames(tests), "%d.%m.%Y" ) #loading the number of tests conducted with respect to administrative regions


relrisk = function(series){ #function to compute the relative risk independently for each day
  
  population<- c(2865000, 2060000, 2148000, 1004000, 2466000, 3349000, 5385000, 948808, 2129000, 1193000, 2295000, 4501000, 1263000, 1408000, 3466000, 1679000 ) #population in each region
  
  sumj<-c() #variable representing the daily sum of observed cases
  
  for(i in 1:ncol(series)){     #sum the observed number of cases for each day
    sumj[i]<-sum(series[,i])
  }
  
  ratio<-c()    #an auxiliary variable "ratio" that helps calculate the expected number of cases independently for each day
  
  for(i in 1:ncol(series)){  #for each day the ratio is calculate as the proportion between the sum of observed cases for each day and the population in a region
    ratio[i]<-sumj[i]/sum(population)
  }
  
  expect<-data.frame(matrix(ncol=ncol(series), nrow=0)) #a variable representing the expected number of cases
  colnames(expect) <- colnames(series)
  
  for(i in 1:16){   #the expected number of cases is calculated as the product between the daily-wise "ratio" variable and row-wise regional population
    for(j in 1:ncol(series)){
      expect[i,j]<-ratio[j]*population[i]
    }
  }
  
  SMR=series/expect #relative risk is calculated as the proportion between the observed and expected number of cases
  return(SMR)
}

relrisk = function(series){ #function to compute the relative risk independently for each day
  
  population<- c(2865000, 2060000, 2148000, 1004000, 2466000, 3349000, 5385000, 948808, 2129000, 1193000, 2295000, 4501000, 1263000, 1408000, 3466000, 1679000 ) #population in each region
  
  sumj<-c() #variable representing the daily sum of observed cases
  
  for(i in 1:ncol(series)){     #sum the observed number of cases for each day
    sumj[i]<-sum(series[,i])
  }
  
  ratio<-c()    #an auxiliary variable "ratio" that helps calculate the expected number of cases independently for each day
  
  for(i in 1:ncol(series)){  #for each day the ratio is calculate as the proportion between the sum of observed cases for each day and the population in a region
    ratio[i]<-sumj[i]/sum(population)
  }
  
  expect<-data.frame(matrix(ncol=ncol(series), nrow=0)) #a variable representing the expected number of cases
  colnames(expect) <- colnames(series)
  
  for(i in 1:16){   #the expected number of cases is calculated as the product between the daily-wise "ratio" variable and row-wise regional population
    for(j in 1:ncol(series)){
      expect[i,j]<-ratio[j]*population[i]
    }
  }
  
  SMR=series/expect #relative risk is calculated as the proportion between the observed and expected number of cases
  return(SMR)
}

relrisk_cum = function(series){ #a function that calculates the relative risk for every day including the history of previous days
  
  sumw<-data.frame(matrix(ncol=ncol(series), nrow=0)) #variable for storing the cumulative observed number of cases for each day and each region
  colnames(sumw) <- colnames(series)
  
  for(i in 1:16){ #a dummy variable that will help calculate the cumulative number of cases below
    sumw[i,1]<-series[i,1]
  }
  
  for(i in 1:16){ #calculating the cumulative number of cases for each region and each day
    for(j in 2:ncol(series)){
      sumw[i,j]<-sumw[i,j-1]+series[i,j]
    }
  }
  
  population<- c(2865000, 2060000, 2148000, 1004000, 2466000, 3349000, 5385000, 948808, 2129000, 1193000, 2295000, 4501000, 1263000, 1408000, 3466000, 1679000 ) #population in each region
  
  ratio<-c() # an auxiliary variable "ratio" that helps calculate the expected number of cases for each day including the previous history
  
  for(i in 1:ncol(series)){ #for each day the ratio is calculate as the proportion between the cumulative sum of observed cases for each day and the population in a region 
    ratio[i]<-sum(sumw[,i])/sum(population)
  }
  
  expect<-data.frame(matrix(ncol=ncol(series), nrow=0)) #a variable representing the expected number of cases 
  colnames(expect) <- colnames(series)

  for(i in 1:16){ #the expected number of cases is calculated as the product between the cumulative daily-wise "ratio" variable and row-wise regional population
    for(j in 1:ncol(series)){
      expect[i,j]<-population[i]*ratio[j]
    }
  }
  
  SMR=sumw/expect #relative risk is calculated as the proportion between the observed and expected number of cases
  return(SMR)
}

sum_ = function(series){ #function to calculate the daily sum of observed cases in Poland

  sumj<-c() #a vector to store the total of observed number of cases for each day
  
  for(i in 1:ncol(series)){ #calculating the daily sum of observed cases in Poland
    sumj[i]<-sum(series[,i])
  }
  return(sumj)
}

sum_cum = function(series){ #function to calculate the cumulative sum of observed cases in Poland
  
  sumj<-sum_(series) #a variable to store 
  
  sumk<-c(sumj[1]) #a dummy variable that will help calculate the cumulative sum
  
  for(i in 2:ncol(series)){ #calculate the cumulative sum of confirmed number of cases
    sumk[i]<-sumk[i-1]+sumj[i]
  }
  
  return(sumk)
}

interpolate_tests = function (tests_sparse){ #function to interpolate test intensity between days
  
  intercept<-data.frame(matrix(ncol=ncol(tests_sparse)-3, nrow=0)) #a variable representing the intercept between weeks

  for(i in 1:16){ #calculating the intercept for each week
    for(j in 3:(ncol(tests_sparse)-1)){
      if(j==11){
        intercept[i,j]<- ((tests_sparse[i,j+1]-tests_sparse[i,j]) )/10 #a greater-than-week difference between 2020-05-01 and 2020-05-11
      }
      else{
         intercept[i,j]<- ((tests_sparse[i,j+1]-tests_sparse[i,j]) )/7
      }                                           }
  }
  intercept <- dplyr::select(intercept, -c(1,2)) #removing dummy columns
    l=0
  
    for(i in 3:(ncol(tests_sparse)-1)){ #calculating the relative safety (tests) using the previously calculated intercept
      if(i<11){
         for (k in 1:6){
           new<-tests_sparse[,(7*(i-3)+3)]+intercept[,i-2]*k
           tests_sparse<-add_column(tests_sparse, new, .after = k+l+2)
        }
      }
      else if (i==11){ #calculating the interpolation for 10-day period (between 2020-05-01 and 2020-05-11)
        for (k in 1:9){
          new<-tests_sparse[,(7*(i-3)+3)]+intercept[,i-2]*k
          tests_sparse<-add_column(tests_sparse, new, .after = k+l+2)
        }
      }
      else{  #calculating the interpolation for weeks after 2020-05-11
        for (k in 1:6){
          new<-tests_sparse[,(7*(i-3)+6)]+intercept[,i-2]*k
          tests_sparse<-add_column(tests_sparse, new, .after = k+l+2+3)
        }
      }
      l=l+7
    }
    
  return(tests_sparse)
}

weighted_risk = function(risks, tests){ #function to calculate the weighted risk, it takes two arguments: data frames with relative risks (confirmed infections) and relative safety (tests conducted)
  
  prop=1 #proportionality constant, here we assume that relative risks (confirmed infections) and relative safety (tests conducted) equally contribute to the finaal estimate of relative risk

  return(risks*(prop/tests))
}

reorder = function(region_labels){
 reordered_labels<- c(region_labels[12], 
                      region_labels[8], 
                      region_labels[15], 
                      region_labels[16], 
                      region_labels[13], 
                      region_labels[2], 
                      region_labels[10], 
                      region_labels[1], 
                      region_labels[9], 
                      region_labels[6], 
                      region_labels[11], 
                      region_labels[14], 
                      region_labels[5], 
                      region_labels[7], 
                      region_labels[3], 
                      region_labels[4])
  return(reordered_labels)
}

#Calculating the daily observed  number of cases in Poland 

sumdf<-sum_(series)
sumdf<-setNames(sumdf, colnames(series))
sumdf <- stack(sumdf)

colnames(sumdf)<- c("Infections", "Date")
sumdf$Date <- as.Date(sumdf$Date)

#Calculating the cumulative  number of cases in Poland 

sum_c=sum_cum(series)
sumcdf<-setNames(sum_c, colnames(series))
sumcdf <- stack(sumcdf)

colnames(sumcdf)<- c("Infections", "Date")
sumcdf$Date <- as.Date(sumcdf$Date)

#Chart for the daily observed  number of cases in Poland 

chart00<-ggplot(sumdf, aes(Date, Infections, group=1))+ 
  geom_point() + geom_line(colour = "blue")+xlab("Date")+ 
  ylab("Daily infections")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Chart for the cumulative observed  number of cases in Poland 

chart01<-ggplot(sumcdf, aes(Date, Infections, group=1))+ 
  geom_point() + geom_line(colour = "blue")+xlab("Date")+ 
  ylab("Cumulative infections")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Chart for both daily observed and cumulative  number of cases in Poland
tiff("without_titles.tiff", units="in", width = 11.2, height=7, res=300)
 ggpubr::ggarrange(chart00,chart01, ncol = 1, nrow=2, labels=c("A", "B") )
dev.off()

#Chart for relative risk estimated independently for each day
relriskdf_ind<- relrisk(series)
relriskdf_ind<-dplyr::mutate(relriskdf_ind, regions)
relriskdf_ind_names <- relriskdf_ind %>% #remove first columns
  dplyr::select(regions, everything())%>%
  dplyr::select(-c("2020-03-04",   "2020-03-05",   "2020-03-06",   "2020-03-07",   "2020-03-08",   "2020-03-09",   "2020-03-10",  "2020-03-11",  "2020-03-12"))

write.table(relriskdf_ind_names, "FIG1A.csv", sep=";", dec=".", row.names=F)

mdf_ind <- melt(relriskdf_ind_names, id.vars="regions", value.name="SIR", variable.name="Date") #project to long form

chart1<-ggplot(data=mdf_ind, aes(
                              x=Date,
                              y=SIR,
                              group = regions,
                              colour = regions
                              )
               )+
        geom_line(size=0.5) +
        geom_point( size=1, shape=21, fill="white")+
        theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Chart for relative risk estimated for each day including the previous history
relriskdf_cum<- relrisk_cum(series)
relriskdf_cum<-dplyr::mutate(relriskdf_cum, regions)
relriskdfnames_cum <- relriskdf_cum %>%
  dplyr::select(regions, everything())%>%
  dplyr::select(-c("2020-03-04",   "2020-03-05",   "2020-03-06",   "2020-03-07",   "2020-03-08",   "2020-03-09",   "2020-03-10",  "2020-03-11",  "2020-03-12"))

write.table(relriskdfnames_cum, "FIG1B.csv", sep=";", dec=".", row.names=F)

mdf_cum <- melt(relriskdfnames_cum, id.vars="regions", value.name="CSIR", variable.name="Date")

chart2<-  ggplot(data=mdf_cum, aes(
                              x=Date, 
                              y=CSIR, 
                              group = regions, 
                              colour = regions
                             )
               )+
       geom_line(size=0.5) +
       geom_point( size=1, shape=21, fill="white")+
       theme(axis.text.x=element_text(angle = -90, hjust = 0))


#Interpolation charts for testing intensity

reltestsdf<- interpolate_tests(relrisk(tests)) 

colnames(reltestsdf)<- colnames(series)

rt<-reltestsdf

reltestsdf<-dplyr::mutate(reltestsdf, regions)

write.table(reltestsdf, "FIG2.csv", sep=";", dec=".", row.names=F)

mdf_tests <- melt(reltestsdf, id.vars="regions", value.name="TIR", variable.name="Date")

chart3 <-ggplot(data=mdf_tests, aes(  #Chart presenting relative testing safety
                              x=Date, 
                              y=TIR, 
                              group = regions, 
                              colour = regions
                              )
               ) +
        geom_line(size=0.5) +
        geom_point( size=1, shape=21, fill="white")+
        theme(axis.text.x=element_text(angle = -90, hjust = 0))


chart4 <-ggplot(data=mdf_tests, aes( #Chart presenting relative testing risk
                               x=Date, 
                               y=1/TIR, 
                               group = regions, 
                               colour = regions
                              )
               )+
        geom_line(size=0.5) +
        geom_point( size=1, shape=21, fill="white")+
        theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Combining risks related to testing and infections

weighted_riskdf<- weighted_risk( relrisk_cum(series), rt) #multiplying corresponding matrices
weighted_riskdf<-dplyr::mutate(weighted_riskdf, regions)

weighted_riskdf <- weighted_riskdf %>%
  dplyr::select(regions, everything())%>%
  dplyr::select(-c("2020-03-04",   "2020-03-05",   "2020-03-06",   "2020-03-07",   "2020-03-08",   "2020-03-09",   "2020-03-10",  "2020-03-11",  "2020-03-12"))

write.table(weighted_riskdf, "FIG3B.csv", sep=";", dec=".", row.names=F)

mdf_weighted <- melt(weighted_riskdf , id.vars="regions", value.name="WCSIR", variable.name="Date")


  

chart5<-ggplot(data=mdf_weighted , aes(
                                        x=Date, 
                                        y=WCSIR, 
                                        group = regions, 
                                        colour = regions
                                       )
                )+
        geom_line(size=0.5) +
        geom_point( size=1, shape=21, fill="white")+
        theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Chart for relative safety (cumulative version, not weighted)
chart6<-  ggplot(data=mdf_cum, aes(
  x=Date, 
  y=1/CSIR, 
  group = regions, 
  colour = regions
)
) +
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Chart for relative safety (cumulative version, weighted)
chart7<-ggplot(data=mdf_weighted, aes(
  x=Date, 
  y=1/WCSIR, 
  group = regions, 
  colour = regions
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))



#Relative risks calculated independently for every day weighted by the test intensity


weighted_daily_riskdf<- weighted_risk( relrisk(series), r) #multiplying corresponding matrices
weighted_daily_riskdf<-dplyr::mutate(weighted_daily_riskdf, regions)

weighted_daily_riskdf <- weighted_daily_riskdf %>%
  dplyr::select(regions, everything())%>%
  dplyr::select(-c("2020-03-04",   "2020-03-05",   "2020-03-06",   "2020-03-07",   "2020-03-08",   "2020-03-09",   "2020-03-10",  "2020-03-11",  "2020-03-12"))

write.table(weighted_daily_riskdf, "FIG3A.csv", sep=";", dec=".", row.names=F)


mdf_daily_weighted <- melt(weighted_daily_riskdf , id.vars="regions", value.name="WSIR", variable.name="Date")

chart8<-ggplot(data=mdf_daily_weighted, aes(
  x=Date,
  y=WSIR,
  group = regions,
  colour = regions
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#Adding new row observation to weighted cumulative plot
dfsil<-dplyr::filter(relriskdfnames_cum, regions=="12 KAT")
mdf_sil<- melt(dfsil , id.vars="regions", value.name="WCSIR", variable.name="Date")

dfop<-dplyr::filter(relriskdfnames_cum, regions=="8 OPO")
mdf_op<- melt(dfop , id.vars="regions", value.name="WCSIR", variable.name="Date")

tiff("chart-hiadded.tiff", units="in", width = 11.2, height=7, res=300)
ggplot(data= mdf_weighted , aes(
  x=Date, 
  y=WCSIR, 
  group = regions, 
  colour = regions,
  
)
)+
  geom_line()+
  geom_line(data=mdf_sil, aes(x=Date, y=WCSIR, group=regions, colour=regions), size=1.4)+
  geom_line(data=mdf_op, aes(x=Date, y=WCSIR, group=regions, colour=regions), size=1.4)+
  geom_point( size=1, shape=21, fill="white")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
dev.off()


#Saving tiff plots


# tiff("chart1.tiff", units="in", width = 11.2, height=7, res=300)
# chart1
# dev.off()
# 
# tiff("chart2.tiff", units="in", width = 11.2, height=7, res=300)
# chart2
# dev.off()
# 
# tiff("chart3.tiff", units="in", width = 11.2, height=7, res=300)
# chart3
# dev.off()
# 
# tiff("chart4.tiff", units="in", width = 11.2, height=7, res=300)
# chart4
# dev.off()
# 
# tiff("chart5.tiff", units="in", width = 11.2, height=7, res=300)
# chart5
# dev.off()
# 
# tiff("chart6.tiff", units="in", width = 11.2, height=7, res=300)
# chart6
# dev.off()
# 
# tiff("chart7.tiff", units="in", width = 11.2, height=7, res=300)
# chart7
# dev.off()
# 
# tiff("chart8---.tiff", units="in", width = 11.2, height=7, res=300)
# chart8
# dev.off()
# 
# 
# 
# 
# tiff("chart0001.tiff", units="in", width = 11.2, height=7, res=300)
# chart0001
# dev.off()

#Plotting maps
regions <- st_read("data/regions.shp")

regions$III_week_CSIR<-reorder(as.numeric(as.character(round(relriskdfnames_cum[,22], 2))))
regions$VI_week_CSIR<-reorder(as.numeric(as.character(round(relriskdfnames_cum[,43], 2))))
regions$IX_week_CSIR<-reorder(as.numeric(as.character(round(relriskdfnames_cum[,64], 2))))
regions$XII_week_CSIR<-reorder(as.numeric(as.character(round(relriskdfnames_cum[,85], 2))))
regions$XV_week_CSIR<-reorder(as.numeric(as.character(round(relriskdfnames_cum[,106], 2))))

font_size=0.7

CSIR_3_week<- tm_shape(regions) + 
  tm_polygons('III_week_CSIR',legend.show = FALSE)+ 
    tm_text('III_week_CSIR', size=font_size  )

CSIR_6_week<- tm_shape(regions) + 
  tm_polygons('VI_week_CSIR',legend.show = FALSE)+ 
  tm_text('VI_week_CSIR', size=font_size  )

CSIR_9_week<- tm_shape(regions) + 
  tm_polygons('IX_week_CSIR',legend.show = FALSE)+ 
  tm_text('IX_week_CSIR', size=font_size  )

CSIR_12_week<- tm_shape(regions) + 
  tm_polygons('XII_week_CSIR',legend.show = FALSE)+ 
  tm_text('XII_week_CSIR', size=font_size )

CSIR_15_week<- tm_shape(regions) + 
  tm_polygons('XV_week_CSIR',legend.show = FALSE)+ 
  tm_text('XV_week_CSIR', size=font_size  )


regions$III_week_WCSIR<-reorder(as.numeric(as.character(round(weighted_riskdf[,22], 2))))
regions$VI_week_WCSIR<-reorder(as.numeric(as.character(round(weighted_riskdf[,43], 2))))
regions$IX_week_WCSIR<-reorder(as.numeric(as.character(round(weighted_riskdf[,64], 2))))
regions$XII_week_WCSIR<-reorder(as.numeric(as.character(round(weighted_riskdf[,85], 2))))
regions$XV_week_WCSIR<-reorder(as.numeric(as.character(round(weighted_riskdf[,106], 2))))

WCSIR_3_week<- tm_shape(regions) + 
  tm_polygons('III_week_WCSIR',legend.show = FALSE)+ 
  tm_text('III_week_WCSIR', size=font_size  )

WCSIR_6_week<- tm_shape(regions) + 
  tm_polygons('VI_week_WCSIR',legend.show = FALSE)+ 
  tm_text('VI_week_WCSIR', size=font_size  )

WCSIR_9_week<- tm_shape(regions) + 
  tm_polygons('IX_week_WCSIR',legend.show = FALSE)+ 
  tm_text('IX_week_WCSIR', size=font_size  )

WCSIR_12_week<- tm_shape(regions) + 
  tm_polygons('XII_week_WCSIR',legend.show = FALSE)+ 
  tm_text('XII_week_WCSIR', size=font_size  )

WCSIR_15_week<- tm_shape(regions) + 
  tm_polygons('XV_week_WCSIR',legend.show = FALSE)+ 
  tm_text('XV_week_WCSIR', size=font_size  )   

current.mode <- tmap_mode("plot")
tiff("tmaps.tiff", units="in", width = 11.2, height=7, res=300)
tmap_arrange(CSIR_3_week,CSIR_6_week,CSIR_9_week,CSIR_12_week, CSIR_15_week,
             WCSIR_3_week,WCSIR_6_week,WCSIR_9_week,WCSIR_12_week,WCSIR_15_week,
             ncol=5, nrow=2)
dev.off()
tmap_mode(current.mode)

#Legend map

regions$Label<-reorder(relriskdfnames_cum$regions)

tiff("legend_map.tiff", units="in", width = 11.2, height=7, res=300)
 tm_shape(regions) + 
  tm_polygons('Label', legend.show = FALSE)+ 
  tm_text('Label', size=1  )
dev.off()

