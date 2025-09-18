
# LOAD REQUIRED LIBRARIES-------------------------------------------------------


library(tidyverse) #For data manipulation and visualization (dplyr, ggplot2, lubridate)
library(fs)        #For file system operations
library(tsibble)   #For handling time series as tsibble objects

library(fable)     #For time series modeling
library(forecast)
library(fpp3)      #Collection of forecasting packages
library(feasts)    #Decomposition and ETS models
library(fabletools) #Tools for fable models

library(knitr)
library(kableExtra)
library(ggiraph)   #Interactive visualizations