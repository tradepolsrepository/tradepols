#Do political relations colour China's trade with Southeast Asian partners? A vector autoregression approach
#Asia & the Pacific Policy Studies

#notes:
#figures and Tables are labeled accordingly (i.e. 'Table 1', 'Figure 1' etc.)
#the abbreviations for each of the six Southeast Asian countries and China used throughout this analysis are as follows:
  #CHN=China
  #IDN=Indonesia
  #MYS=Malaysia
  #PHL=Philippines
  #SGP=Singapore
  #THA=Thailand
  #VNM=Vietnam

#initial setup:
#close all graphics, clear memory and screen
graphics.off(); remove(list = ls()); cat("\14");
#clear plots
if(!is.null(dev.list())) dev.off()
#clear console
cat("\014") 
#clear workplace
rm(list=ls())
gc()

#install packages
install.packages("ggpubr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggrepel")
#frequency issues
install.packages("forecast")
install.packages("hwwntest")
install.packages("libblas-dev")
install.packages("liblapack-dev")
install.packages("fracdiff")

#load packages
library(ggpubr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggrepel)
library(fracdiff)
library(forecast)
library(hwwntest)

#set working directory
setwd("[INSERT FILE PATH]")

#measuring bilateral political relations with China
#constructing the PRIs
#net cooperation (netcoop)
#get data
RAW_GDELT_CHINA <- read.csv("(02) gdelt.csv")
#convert SQLDATE to date type
RAW_GDELT_CHINA[,2] <- ymd(RAW_GDELT_CHINA[,2])
#add isneg, month and year columns
RAW_GDELT_CHINA <- RAW_GDELT_CHINA %>%
  mutate(year = year(SQLDATE)) %>%
  mutate(month = month(SQLDATE)) %>%
  mutate(isneg = ifelse(GoldsteinScale <0, 1, 0)) %>%
  mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
  mutate(trade = grepl("trade",RAW_GDELT_CHINA$SOURCEURL)) %>%
  filter(Actor1CountryCode  == "PHL" | Actor1CountryCode  == "VNM" | Actor1CountryCode  == "IDN" | Actor1CountryCode  == "MYS" | Actor1CountryCode  == "SGP" | Actor1CountryCode  == "THA")

#extract data
partnerlist <- unique(RAW_GDELT_CHINA$Actor1CountryCode)
actorlist <- unique(RAW_GDELT_CHINA$Actor1Type1Code)

for (i in 1:length(partnerlist)) {
  temp <- filter(RAW_GDELT_CHINA, RAW_GDELT_CHINA$Actor1CountryCode == partnerlist[[i]])
  for (j in 1:length(actorlist)) {
    temp1<- filter(temp, temp$Actor1Type1Code == actorlist[[j]])
    temp2 <-  temp1 %>%
      select(SQLDATE, quarter, Actor1CountryCode, Actor1Type1Code, GoldsteinScale, isneg, trade) %>%
      complete(SQLDATE = seq.Date(min(SQLDATE), ymd("2019-12-31"), by="day")) %>%
      mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
      mutate(year = year(SQLDATE))
    temp2$isneg[is.na(temp2$isneg)] <- 0 
    temp2$GoldsteinScale[is.na(temp2$GoldsteinScale)] <- 0 
    temp2$Actor1Type1Code[is.na(temp2$Actor1Type1Code)] <- actorlist[[j]] 
    temp2$Actor1CountryCode[is.na(temp2$Actor1CountryCode)] <- partnerlist[[i]]
    temp2$trade[is.na(temp2$trade)] <- "FALSE"
    temp2$MY <- floor_date(temp2$SQLDATE, "month") ##Group by month variable
    temp3 <- temp2 %>%
      filter(MY > "1999-12-01" & MY < "2020-01-01") ##Selects events from Jan 2000 to Dec 2019
    temp3_eventcount <- data.frame(table(temp3$MY))
    temp3_eventcount$Var1 <- ymd(temp3_eventcount$Var1) 
    temp3_tradecount <- temp3 %>%
      mutate(tradefinal = ifelse(trade == "NA",FALSE,trade)) %>%
      group_by(MY) %>%
      summarise(tradecount = sum(tradefinal == TRUE))      
    temp4 <- temp3  %>%
      group_by(MY) %>%
      summarise(PRI = sum(GoldsteinScale)) %>%
      mutate(partner = partnerlist[[i]]) %>%
      mutate(actor = actorlist[[j]]) %>%
      select(partner,actor, MY,everything())
    temp5 <-  inner_join(temp4, temp3_tradecount, by = "MY")
    colnames(temp3_eventcount) <- c("MY","eventcount")
    temp6 <- inner_join(temp5,temp3_eventcount, by = "MY" )
    temp6$normalisedPRI <- temp6$PRI/temp6$eventcount
    reg <- lm(temp6$normalisedPRI ~temp6$tradecount)
    res <- as.data.frame(resid(reg))
    temp6$tfPRI <- res$`resid(reg)`
    temp7 <- temp6 %>%
      select(partner, actor, MY, PRI, normalisedPRI, tfPRI)
    data.frame(assign(paste0("PRI",i,j), temp7)) 
  }
}

#Indonesia's net cooperation PRIs 
PRI_IDN <- rbind.data.frame(PRI51)
PRI_IDN$logPRI <- log(PRI_IDN$normalisedPRI - min(PRI_IDN$normalisedPRI) + 2)
PRI_IDN$logtfPRI <- log(PRI_IDN$tfPRI - min(PRI_IDN$tfPRI) + 2)
annual_PRI_IDN <- PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
#layout, labels for figures
colnames(annual_PRI_IDN) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
#monthly unfiltered, trade-filtered PRIs
PRI_IDN$actor <- gsub('GOV','Government',PRI_IDN$actor)
PRI_IDN$partner <- gsub('IDN','Indonesia',PRI_IDN$partner)
monthly_PRI_IDN_plot <- ggplot(PRI_IDN[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_IDN_plot <- ggplot(PRI_IDN[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
#annual unfiltered, trade-filtered PRIs
annual_PRI_IDN <- PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_IDN$actor <- gsub('GOV','Government',annual_PRI_IDN$actor)
annual_PRI_IDN$partner <- gsub('IDN','Indonesia',annual_PRI_IDN$partner)
annual_PRI_IDN_plot <- ggplot(annual_PRI_IDN[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_IDN_plot <- ggplot(annual_PRI_IDN[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Indonesia's annual netcoop PRI
write.csv(annual_PRI_IDN[1:20,],"annual_PRI_IDN.csv")
#save Indonesia's monthly netcoop PRI
write.csv(PRI_IDN[1:240,],"monthly_PRI_IDN.csv")

#Malaysia's net cooperation PRIs 
PRI_MYS <- rbind.data.frame(PRI41)
PRI_MYS$logPRI <- log(PRI_MYS$normalisedPRI - min(PRI_MYS$normalisedPRI) + 2)
PRI_MYS$logtfPRI <- log(PRI_MYS$tfPRI - min(PRI_MYS$tfPRI) + 2)
annual_PRI_MYS <- PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_PRI_MYS) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
PRI_MYS$actor <- gsub('GOV','Government',PRI_MYS$actor)
PRI_MYS$partner <- gsub('MYS','Malaysia',PRI_MYS$partner)
monthly_PRI_MYS_plot <- ggplot(PRI_MYS[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_MYS_plot <- ggplot(PRI_MYS[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_PRI_MYS <- PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_MYS$actor <- gsub('GOV','Government',annual_PRI_MYS$actor)
annual_PRI_MYS$partner <- gsub('MYS','Malaysia',annual_PRI_MYS$partner)
annual_PRI_MYS_plot <- ggplot(annual_PRI_MYS[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_MYS_plot <- ggplot(annual_PRI_MYS[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Malaysia's annual netcoop PRI
write.csv(annual_PRI_MYS[1:20,],"annual_PRI_MYS.csv")
#save Malaysia's monthly netcoop PRI
write.csv(PRI_MYS[1:240,],"monthly_PRI_MYS.csv")

#The Philippines's net cooperation PRIs  
PRI_PHL <- rbind.data.frame(PRI11)
PRI_PHL$logPRI <- log(PRI_PHL$normalisedPRI - min(PRI_PHL$normalisedPRI) + 2)
PRI_PHL$logtfPRI <- log(PRI_PHL$tfPRI - min(PRI_PHL$tfPRI) + 2)
annual_PRI_PHL <- PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_PRI_PHL) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
PRI_PHL$actor <- gsub('GOV','Government',PRI_PHL$actor)
PRI_PHL$partner <- gsub('PHL','Philippines',PRI_PHL$partner)
monthly_PRI_PHL_plot <- ggplot(PRI_PHL[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_PHL_plot <- ggplot(PRI_PHL[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_PRI_PHL <- PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_PHL$actor <- gsub('GOV','Government',annual_PRI_PHL$actor)
annual_PRI_PHL$partner <- gsub('PHL','Philippines',annual_PRI_PHL$partner)
annual_PRI_PHL_plot <- ggplot(annual_PRI_PHL[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_PHL_plot <- ggplot(annual_PRI_PHL[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save the Philippines's annual netcoop PRI
write.csv(annual_PRI_PHL[1:20,],"annual_PRI_PHL.csv")
#save the Philippines's monthly netcoop PRI
write.csv(PRI_PHL[1:240,],"monthly_PRI_PHL.csv")

#Singapore's net cooperation PRIs 
PRI_SGP <- rbind.data.frame(PRI31)
PRI_SGP$logPRI <- log(PRI_SGP$normalisedPRI - min(PRI_SGP$normalisedPRI) + 2)
PRI_SGP$logtfPRI <- log(PRI_SGP$tfPRI - min(PRI_SGP$tfPRI) + 2)
annual_PRI_SGP <- PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_PRI_SGP) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
PRI_SGP$actor <- gsub('GOV','Government',PRI_SGP$actor)
PRI_SGP$partner <- gsub('SGP','Singapore',PRI_SGP$partner)
monthly_PRI_SGP_plot <- ggplot(PRI_SGP[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_SGP_plot <- ggplot(PRI_SGP[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_PRI_SGP <- PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_SGP$actor <- gsub('GOV','Government',annual_PRI_SGP$actor)
annual_PRI_SGP$partner <- gsub('SGP','Singapore',annual_PRI_SGP$partner)
annual_PRI_SGP_plot <- ggplot(annual_PRI_SGP[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_SGP_plot <- ggplot(annual_PRI_SGP[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Singapore's annual netcoop PRI
write.csv(annual_PRI_SGP[1:20,],"annual_PRI_SGP.csv")
#save Singapore's monthly netcoop PRI
write.csv(PRI_SGP[1:240,],"montly_PRI_SGP.csv")

#Thailand's net cooperation PRIs 
PRI_THA <- rbind.data.frame(PRI61)
PRI_THA$logPRI <- log(PRI_THA$normalisedPRI - min(PRI_THA$normalisedPRI) + 2)
PRI_THA$logtfPRI <- log(PRI_THA$tfPRI - min(PRI_THA$tfPRI) + 2)
annual_PRI_THA <- PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_PRI_THA) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
PRI_THA$actor <- gsub('GOV','Government',PRI_THA$actor)
PRI_THA$partner <- gsub('THA','Thailand',PRI_THA$partner)
monthly_PRI_THA_plot <- ggplot(PRI_THA[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_THA_plot <- ggplot(PRI_THA[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_PRI_THA <- PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_THA$actor <- gsub('GOV','Government',annual_PRI_THA$actor)
annual_PRI_THA$partner <- gsub('THA','Thailand',annual_PRI_THA$partner)
annual_PRI_THA_plot <- ggplot(annual_PRI_THA[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_THA_plot <- ggplot(annual_PRI_THA[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Thailand's annual netcoop PRI
write.csv(annual_PRI_THA[1:20,],"annual_PRI_THA.csv")
#save Thailand's monthly netcoop PRI
write.csv(PRI_THA[1:240,],"monthly_PRI_THA.csv")

#Vietnam's net cooperation PRIs 
PRI_VNM <- rbind.data.frame(PRI21)
PRI_VNM$logPRI <- log(PRI_VNM$normalisedPRI - min(PRI_VNM$normalisedPRI) + 2)
PRI_VNM$logtfPRI <- log(PRI_VNM$tfPRI - min(PRI_VNM$tfPRI) + 2)
annual_PRI_VNM <- PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_PRI_VNM) <- c("partner","actor","year","PRI","normalisedPRI","logPRI","logtfPRI") 
PRI_VNM$actor <- gsub('GOV','Government',PRI_VNM$actor)
PRI_VNM$partner <- gsub('VNM','Vietnam',PRI_VNM$partner)
monthly_PRI_VNM_plot <- ggplot(PRI_VNM[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfPRI_VNM_plot <- ggplot(PRI_VNM[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_PRI_VNM <- PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_PRI_VNM$actor <- gsub('GOV','Government',annual_PRI_VNM$actor)
annual_PRI_VNM$partner <- gsub('VNM','Vietnam',annual_PRI_VNM$partner)
annual_PRI_VNM_plot <- ggplot(annual_PRI_VNM[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfPRI_VNM_plot <- ggplot(annual_PRI_VNM[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Vietnam's annual PRI
write.csv(annual_PRI_VNM[1:20,],"annual_PRI_VNM.csv")
#save Vietnam's monthly PRI
write.csv(PRI_VNM[1:240,],"monthly_PRI_VNM.csv")

#Figure 1
monthly_PRI_ALL <- ggarrange(monthly_PRI_IDN_plot + rremove("xlab")+ rremove("ylab"), 
                             monthly_PRI_MYS_plot + rremove("xlab")+ rremove("ylab"),
                             monthly_PRI_PHL_plot + rremove("xlab")+ rremove("ylab"),
                             monthly_PRI_SGP_plot + rremove("xlab")+ rremove("ylab"),
                             monthly_PRI_THA_plot + rremove("xlab")+ rremove("ylab"),
                             monthly_PRI_VNM_plot + rremove("xlab")+ rremove("ylab"), 
                             common.legend = TRUE, legend="right")
annotate_figure(monthly_PRI_ALL, bottom = "Year", left = "log monthly PRI towards China")

#Figure 2
monthly_tfPRI_ALL <- ggarrange(monthly_tfPRI_IDN_plot + rremove("xlab")+ rremove("ylab"), 
                               monthly_tfPRI_MYS_plot + rremove("xlab")+ rremove("ylab"),
                               monthly_tfPRI_PHL_plot + rremove("xlab")+ rremove("ylab"),
                               monthly_tfPRI_SGP_plot + rremove("xlab")+ rremove("ylab"),
                               monthly_tfPRI_THA_plot + rremove("xlab")+ rremove("ylab"),
                               monthly_tfPRI_VNM_plot + rremove("xlab")+ rremove("ylab"), 
                               common.legend = TRUE, legend="right")
annotate_figure(monthly_tfPRI_ALL, bottom = "Year", left = "log monthly trade-filtered PRI towards China")

#aggregate cooperation (aggcoop)
#get the data
RAW_GDELT_CHINA <- read.csv("(02) gdelt.csv")
#convert SQLDATE to date type
RAW_GDELT_CHINA[,2] <- ymd(RAW_GDELT_CHINA[,2])
#add isneg, month and year columns
RAW_GDELT_CHINA <- RAW_GDELT_CHINA %>%
  mutate(year = year(SQLDATE)) %>%
  mutate(month = month(SQLDATE)) %>%
  mutate(isneg = ifelse(GoldsteinScale <0, 1, 0)) %>%
  mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
  mutate(trade = grepl("trade",RAW_GDELT_CHINA$SOURCEURL)) %>%
  filter(Actor1CountryCode  == "PHL" | Actor1CountryCode  == "VNM" | Actor1CountryCode  == "IDN" | Actor1CountryCode  == "MYS" | Actor1CountryCode  == "SGP" | Actor1CountryCode  == "THA")

partnerlist <- unique(RAW_GDELT_CHINA$Actor1CountryCode)
actorlist <- unique(RAW_GDELT_CHINA$Actor1Type1Code)

for (i in 1:length(partnerlist)) {
  temp <- filter(RAW_GDELT_CHINA, RAW_GDELT_CHINA$Actor1CountryCode == partnerlist[[i]])
  for (j in 1:length(actorlist)) {
    temp1<- filter(temp, temp$Actor1Type1Code == actorlist[[j]])
    temp2 <-  temp1 %>%
      select(SQLDATE, quarter, Actor1CountryCode, Actor1Type1Code, GoldsteinScale, isneg, trade) %>%
      complete(SQLDATE = seq.Date(min(SQLDATE), ymd("2019-12-31"), by="day")) %>%
      mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
      mutate(year = year(SQLDATE))
    temp2$isneg[is.na(temp2$isneg)] <- 0 
    temp2$GoldsteinScale[is.na(temp2$GoldsteinScale)] <- 0 
    temp2$Actor1Type1Code[is.na(temp2$Actor1Type1Code)] <- actorlist[[j]] 
    temp2$Actor1CountryCode[is.na(temp2$Actor1CountryCode)] <- partnerlist[[i]]
    temp2$trade[is.na(temp2$trade)] <- "FALSE"
    temp2$MY <- floor_date(temp2$SQLDATE, "month") ##Group by month variable
    temp3 <- temp2 %>%
      filter(MY > "1999-12-01" & MY < "2020-01-01") ##Selects events including and from Jan 2000 to Dec 2019
    temp3_eventcount <- data.frame(table(temp3$MY))
    temp3_eventcount$Var1 <- ymd(temp3_eventcount$Var1) 
    temp3_tradecount <- temp3 %>%
      mutate(tradefinal = ifelse(trade == "NA",FALSE,trade)) %>%
      group_by(MY) %>%
      summarise(tradecount = sum(tradefinal == TRUE))      
    temp4 <- temp3  %>%
      group_by(MY) %>%
      summarise(PRI = sum(GoldsteinScale[which(GoldsteinScale>0)])) %>% #Only summing positive Goldstein scores
      mutate(partner = partnerlist[[i]]) %>%
      mutate(actor = actorlist[[j]]) %>%
      select(partner,actor, MY,everything())
    temp5 <-  inner_join(temp4, temp3_tradecount, by = "MY")
    colnames(temp3_eventcount) <- c("MY","eventcount")
    temp6 <- inner_join(temp5,temp3_eventcount, by = "MY" )
    temp6$normalisedPRI <- temp6$PRI/temp6$eventcount
    reg <- lm(temp6$normalisedPRI ~temp6$tradecount)
    res <- as.data.frame(resid(reg))
    temp6$tfPRI <- res$`resid(reg)`
    temp7 <- temp6 %>%
      select(partner, actor, MY, PRI, normalisedPRI, tfPRI)
    data.frame(assign(paste0("PRI",i,j), temp7)) 
  }
}

#Indonesia's aggregate cooperation PRIs
coop_PRI_IDN <- rbind.data.frame(PRI51)
coop_PRI_IDN$logPRI <- log(coop_PRI_IDN$normalisedPRI - min(coop_PRI_IDN$normalisedPRI) + 2)
coop_PRI_IDN$logtfPRI <- log(coop_PRI_IDN$tfPRI - min(coop_PRI_IDN$tfPRI) + 2)
annual_coop_PRI_IDN <- coop_PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_IDN) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_IDN$actor <- gsub('GOV','Government',coop_PRI_IDN$actor)
coop_PRI_IDN$partner <- gsub('IDN','Indonesia',coop_PRI_IDN$partner)
monthly_coop_PRI_IDN_plot <- ggplot(coop_PRI_IDN[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_IDN_plot <- ggplot(coop_PRI_IDN[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_IDN <- coop_PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_IDN$actor <- gsub('GOV','Government',annual_coop_PRI_IDN$actor)
annual_coop_PRI_IDN$partner <- gsub('IDN','Indonesia',annual_coop_PRI_IDN$partner)
annual_coop_PRI_IDN_plot <- ggplot(annual_coop_PRI_IDN[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_IDN_plot <- ggplot(annual_coop_PRI_IDN[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Indonesia's annual aggcoop PRI
write.csv(annual_coop_PRI_IDN[1:20,],"annual_coop_PRI_IDN.csv")
#save Indonesia's monthly aggcoop PRI
write.csv(coop_PRI_IDN[1:240,],"monthly_coop_PRI_IDN.csv")

#Malaysia's aggregate cooperation PRIs
coop_PRI_MYS <- rbind.data.frame(PRI41)
coop_PRI_MYS$logPRI <- log(coop_PRI_MYS$normalisedPRI - min(coop_PRI_MYS$normalisedPRI) + 2)
coop_PRI_MYS$logtfPRI <- log(coop_PRI_MYS$tfPRI - min(coop_PRI_MYS$tfPRI) + 2)
annual_coop_PRI_MYS <- coop_PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_MYS) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_MYS$actor <- gsub('GOV','Government',coop_PRI_MYS$actor)
coop_PRI_MYS$partner <- gsub('MYS','Malaysia',coop_PRI_MYS$partner)
monthly_coop_PRI_MYS_plot <- ggplot(coop_PRI_MYS[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_MYS_plot <- ggplot(coop_PRI_MYS[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_MYS <- coop_PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_MYS$actor <- gsub('GOV','Government',annual_coop_PRI_MYS$actor)
annual_coop_PRI_MYS$partner <- gsub('MYS','Malaysia',annual_coop_PRI_MYS$partner)
annual_coop_PRI_MYS_plot <- ggplot(annual_coop_PRI_MYS[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_MYS_plot <- ggplot(annual_coop_PRI_MYS[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Malaysia's annual aggcoop PRI
write.csv(annual_coop_PRI_MYS[1:20,],"annual_coop_PRI_MYS.csv")
#save Malaysia's monthly aggcoop PRI
write.csv(coop_PRI_MYS[1:240,],"monthly_coop_PRI_MYS.csv")

#The Philippines's aggregate cooperation PRIs
coop_PRI_PHL <- rbind.data.frame(PRI11)
coop_PRI_PHL$logPRI <- log(coop_PRI_PHL$normalisedPRI - min(coop_PRI_PHL$normalisedPRI) + 2)
coop_PRI_PHL$logtfPRI <- log(coop_PRI_PHL$tfPRI - min(coop_PRI_PHL$tfPRI) + 2)
annual_coop_PRI_PHL <- coop_PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_PHL) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_PHL$actor <- gsub('GOV','Government',coop_PRI_PHL$actor)
coop_PRI_PHL$partner <- gsub('PHL','Philippines',coop_PRI_PHL$partner)
monthly_coop_PRI_PHL_plot <- ggplot(coop_PRI_PHL[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_PHL_plot <- ggplot(coop_PRI_PHL[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_PHL <- coop_PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_PHL$actor <- gsub('GOV','Government',annual_coop_PRI_PHL$actor)
annual_coop_PRI_PHL$partner <- gsub('PHL','Philippines',annual_coop_PRI_PHL$partner)
annual_coop_PRI_PHL_plot <- ggplot(annual_coop_PRI_PHL[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_PHL_plot <- ggplot(annual_coop_PRI_PHL[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save the Philippines's annual aggcoop PRI
write.csv(annual_coop_PRI_PHL[1:20,],"annual_coop_PRI_PHL.csv")
#save the Philippines's monthly aggcoop PRI
write.csv(coop_PRI_PHL[1:240,],"monthly_coop_PRI_PHL.csv")

#Singapore's aggregate cooperation PRIs
coop_PRI_SGP <- rbind.data.frame(PRI31)
coop_PRI_SGP$logPRI <- log(coop_PRI_SGP$normalisedPRI - min(coop_PRI_SGP$normalisedPRI) + 2)
coop_PRI_SGP$logtfPRI <- log(coop_PRI_SGP$tfPRI - min(coop_PRI_SGP$tfPRI) + 2)
annual_coop_PRI_SGP <- coop_PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_SGP) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_SGP$actor <- gsub('GOV','Government',coop_PRI_SGP$actor)
coop_PRI_SGP$partner <- gsub('SGP','Singapore',coop_PRI_SGP$partner)
monthly_coop_PRI_SGP_plot <- ggplot(coop_PRI_SGP[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_SGP_plot <- ggplot(coop_PRI_SGP[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_SGP <- coop_PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_SGP$actor <- gsub('GOV','Government',annual_coop_PRI_SGP$actor)
annual_coop_PRI_SGP$partner <- gsub('SGP','Singapore',annual_coop_PRI_SGP$partner)
annual_coop_PRI_SGP_plot <- ggplot(annual_coop_PRI_SGP[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_SGP_plot <- ggplot(annual_coop_PRI_SGP[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Singapore's annual aggcoop PRI
write.csv(annual_coop_PRI_SGP[1:20,],"annual_coop_PRI_SGP.csv")
#save Singapore's monthly aggcoop PRI
write.csv(coop_PRI_SGP[1:240,],"monthly_coop_PRI_SGP.csv")

#Thailand's aggregate cooperation PRIs
coop_PRI_THA <- rbind.data.frame(PRI61)
coop_PRI_THA$logPRI <- log(coop_PRI_THA$normalisedPRI - min(coop_PRI_THA$normalisedPRI) + 2)
coop_PRI_THA$logtfPRI <- log(coop_PRI_THA$tfPRI - min(coop_PRI_THA$tfPRI) + 2)
annual_coop_PRI_THA <- coop_PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_THA) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_THA$actor <- gsub('GOV','Government',coop_PRI_THA$actor)
coop_PRI_THA$partner <- gsub('THA','Thailand',coop_PRI_THA$partner)
monthly_coop_PRI_THA_plot <- ggplot(coop_PRI_THA[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_THA_plot <- ggplot(coop_PRI_THA[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_THA <- coop_PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_THA$actor <- gsub('GOV','Government',annual_coop_PRI_THA$actor)
annual_coop_PRI_THA$partner <- gsub('THA','Thailand',annual_coop_PRI_THA$partner)
annual_coop_PRI_THA_plot <- ggplot(annual_coop_PRI_THA[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_THA_plot <- ggplot(annual_coop_PRI_THA[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Thailand's annual aggcoop PRI
write.csv(annual_coop_PRI_THA[1:20,],"annual_coop_PRI_THA.csv")
#save Thailand's monthly aggcoop PRI
write.csv(coop_PRI_THA[1:240,],"monthly_coop_PRI_THA.csv")

#Vietnam's aggregate cooperation PRIs
coop_PRI_VNM <- rbind.data.frame(PRI21)
coop_PRI_VNM$logPRI <- log(coop_PRI_VNM$normalisedPRI - min(coop_PRI_VNM$normalisedPRI) + 2)
coop_PRI_VNM$logtfPRI <- log(coop_PRI_VNM$tfPRI - min(coop_PRI_VNM$tfPRI) + 2)
annual_coop_PRI_VNM <- coop_PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_coop_PRI_VNM) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
coop_PRI_VNM$actor <- gsub('GOV','Government',coop_PRI_VNM$actor)
coop_PRI_VNM$partner <- gsub('VNM','Vietnam',coop_PRI_VNM$partner)
monthly_coop_PRI_VNM_plot <- ggplot(coop_PRI_VNM[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfcoop_PRI_VNM_plot <- ggplot(coop_PRI_VNM[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_coop_PRI_VNM <- coop_PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_coop_PRI_VNM$actor <- gsub('GOV','Government',annual_coop_PRI_VNM$actor)
annual_coop_PRI_VNM$partner <- gsub('VNM','Vietnam',annual_coop_PRI_VNM$partner)
annual_coop_PRI_VNM_plot <- ggplot(annual_coop_PRI_VNM[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfcoop_PRI_VNM_plot <- ggplot(annual_coop_PRI_VNM[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Vietnam's annual aggcoop PRI
write.csv(annual_coop_PRI_VNM[1:20,],"annual_coop_PRI_VNM.csv")
#save Vietnam's monthly aggcoop PRI
write.csv(coop_PRI_VNM[1:240,],"monthly_coop_PRI_VNM.csv")

#aggregate conflict (aggconf)
#get the data
RAW_GDELT_CHINA <- read.csv("(02) gdelt.csv")
#convert SQLDATE to date type
RAW_GDELT_CHINA[,2] <- ymd(RAW_GDELT_CHINA[,2])
#add isneg, month and year columns
RAW_GDELT_CHINA <- RAW_GDELT_CHINA %>%
  mutate(year = year(SQLDATE)) %>%
  mutate(month = month(SQLDATE)) %>%
  mutate(isneg = ifelse(GoldsteinScale <0, 1, 0)) %>%
  mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
  mutate(trade = grepl("trade",RAW_GDELT_CHINA$SOURCEURL)) %>%
  filter(Actor1CountryCode  == "PHL" | Actor1CountryCode  == "VNM" | Actor1CountryCode  == "IDN" | Actor1CountryCode  == "MYS" | Actor1CountryCode  == "SGP" | Actor1CountryCode  == "THA")

partnerlist <- unique(RAW_GDELT_CHINA$Actor1CountryCode)
actorlist <- unique(RAW_GDELT_CHINA$Actor1Type1Code)

for (i in 1:length(partnerlist)) {
  temp <- filter(RAW_GDELT_CHINA, RAW_GDELT_CHINA$Actor1CountryCode == partnerlist[[i]])
  for (j in 1:length(actorlist)) {
    temp1<- filter(temp, temp$Actor1Type1Code == actorlist[[j]])
    temp2 <-  temp1 %>%
      select(SQLDATE, quarter, Actor1CountryCode, Actor1Type1Code, GoldsteinScale, isneg, trade) %>%
      complete(SQLDATE = seq.Date(min(SQLDATE), ymd("2019-12-31"), by="day")) %>%
      mutate(quarter = quarter(SQLDATE, with_year = TRUE)) %>%
      mutate(year = year(SQLDATE))
    temp2$isneg[is.na(temp2$isneg)] <- 0 
    temp2$GoldsteinScale[is.na(temp2$GoldsteinScale)] <- 0 
    temp2$Actor1Type1Code[is.na(temp2$Actor1Type1Code)] <- actorlist[[j]] 
    temp2$Actor1CountryCode[is.na(temp2$Actor1CountryCode)] <- partnerlist[[i]]
    temp2$trade[is.na(temp2$trade)] <- "FALSE"
    temp2$MY <- floor_date(temp2$SQLDATE, "month") ##Group by month variable
    temp3 <- temp2 %>%
      filter(MY > "1999-12-01" & MY < "2020-01-01") ##Selects events including and from Jan 2000 to Dec 2019
    temp3_eventcount <- data.frame(table(temp3$MY))
    temp3_eventcount$Var1 <- ymd(temp3_eventcount$Var1) 
    temp3_tradecount <- temp3 %>%
      mutate(tradefinal = ifelse(trade == "NA",FALSE,trade)) %>%
      group_by(MY) %>%
      summarise(tradecount = sum(tradefinal == TRUE))      
    temp4 <- temp3  %>%
      group_by(MY) %>%
      summarise(PRI = sum(GoldsteinScale[which(GoldsteinScale<0)])) %>% #Only summing negative Goldstein scores
      mutate(partner = partnerlist[[i]]) %>%
      mutate(actor = actorlist[[j]]) %>%
      select(partner,actor, MY,everything())
    temp5 <-  inner_join(temp4, temp3_tradecount, by = "MY")
    colnames(temp3_eventcount) <- c("MY","eventcount")
    temp6 <- inner_join(temp5,temp3_eventcount, by = "MY" )
    temp6$normalisedPRI <- temp6$PRI/temp6$eventcount
    reg <- lm(temp6$normalisedPRI ~temp6$tradecount)
    res <- as.data.frame(resid(reg))
    temp6$tfPRI <- res$`resid(reg)`
    temp7 <- temp6 %>%
      select(partner, actor, MY, PRI, normalisedPRI, tfPRI)
    data.frame(assign(paste0("PRI",i,j), temp7)) 
  }
}

#Indonesia's aggregate conflict PRIs
conflict_PRI_IDN <- rbind.data.frame(PRI51)
conflict_PRI_IDN$logPRI <- log(conflict_PRI_IDN$normalisedPRI - min(conflict_PRI_IDN$normalisedPRI) + 2)
conflict_PRI_IDN$logtfPRI <- log(conflict_PRI_IDN$tfPRI - min(conflict_PRI_IDN$tfPRI) + 2)
annual_conflict_PRI_IDN <- conflict_PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_IDN) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_IDN$actor <- gsub('GOV','Government',conflict_PRI_IDN$actor)
conflict_PRI_IDN$partner <- gsub('IDN','Indonesia',conflict_PRI_IDN$partner)
monthly_conflict_PRI_IDN_plot <- ggplot(conflict_PRI_IDN[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_IDN_plot <- ggplot(conflict_PRI_IDN[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_IDN <- conflict_PRI_IDN %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_IDN$actor <- gsub('GOV','Government',annual_conflict_PRI_IDN$actor)
annual_conflict_PRI_IDN$partner <- gsub('IDN','Indonesia',annual_conflict_PRI_IDN$partner)
annual_conflict_PRI_IDN_plot <- ggplot(annual_conflict_PRI_IDN[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_IDN_plot <- ggplot(annual_conflict_PRI_IDN[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Indonesia's annual aggconf PRI
write.csv(annual_conflict_PRI_IDN[1:20,],"annual_conflict_PRI_IDN.csv")
#save Indonesia's monthly aggconf PRI
write.csv(conflict_PRI_IDN[1:240,],"monthly_conflict_PRI_IDN.csv")

#Malaysia's aggregate conflict PRIs
conflict_PRI_MYS <- rbind.data.frame(PRI41)
conflict_PRI_MYS$logPRI <- log(conflict_PRI_MYS$normalisedPRI - min(conflict_PRI_MYS$normalisedPRI) + 2)
conflict_PRI_MYS$logtfPRI <- log(conflict_PRI_MYS$tfPRI - min(conflict_PRI_MYS$tfPRI) + 2)
annual_conflict_PRI_MYS <- conflict_PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_MYS) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_MYS$actor <- gsub('GOV','Government',conflict_PRI_MYS$actor)
conflict_PRI_MYS$partner <- gsub('MYS','Malaysia',conflict_PRI_MYS$partner)
monthly_conflict_PRI_MYS_plot <- ggplot(conflict_PRI_MYS[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_MYS_plot <- ggplot(conflict_PRI_MYS[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_MYS <- conflict_PRI_MYS %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_MYS$actor <- gsub('GOV','Government',annual_conflict_PRI_MYS$actor)
annual_conflict_PRI_MYS$partner <- gsub('MYS','Malaysia',annual_conflict_PRI_MYS$partner)
annual_conflict_PRI_MYS_plot <- ggplot(annual_conflict_PRI_MYS[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_MYS_plot <- ggplot(annual_conflict_PRI_MYS[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Malaysia's annual aggconf PRI
write.csv(annual_conflict_PRI_MYS[1:20,],"annual_conflict_PRI_MYS.csv")
#save Malaysia's monthly aggconf PRI
write.csv(conflict_PRI_MYS[1:240,],"monthly_conflict_PRI_MYS.csv")

#The Philippines's aggregate conflict PRIs
conflict_PRI_PHL <- rbind.data.frame(PRI11)
conflict_PRI_PHL$logPRI <- log(conflict_PRI_PHL$normalisedPRI - min(conflict_PRI_PHL$normalisedPRI) + 2)
conflict_PRI_PHL$logtfPRI <- log(conflict_PRI_PHL$tfPRI - min(conflict_PRI_PHL$tfPRI) + 2)
annual_conflict_PRI_PHL <- conflict_PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_PHL) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_PHL$actor <- gsub('GOV','Government',conflict_PRI_PHL$actor)
conflict_PRI_PHL$partner <- gsub('PHL','Philippines',conflict_PRI_PHL$partner)
monthly_conflict_PRI_PHL_plot <- ggplot(conflict_PRI_PHL[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_PHL_plot <- ggplot(conflict_PRI_PHL[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_PHL <- conflict_PRI_PHL %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_PHL$actor <- gsub('GOV','Government',annual_conflict_PRI_PHL$actor)
annual_conflict_PRI_PHL$partner <- gsub('PHL','Philippines',annual_conflict_PRI_PHL$partner)
annual_conflict_PRI_PHL_plot <- ggplot(annual_conflict_PRI_PHL[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_PHL_plot <- ggplot(annual_conflict_PRI_PHL[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save the Philippines's annual aggconf PRI
write.csv(annual_conflict_PRI_PHL[1:20,],"annual_conflict_PRI_PHL.csv")
#save the Philippines's monthly aggconf PRI
write.csv(conflict_PRI_PHL[1:240,],"monthly_conflict_PRI_PHL.csv")

#Singapore's aggregate conflict PRIs
conflict_PRI_SGP <- rbind.data.frame(PRI31)
conflict_PRI_SGP$logPRI <- log(conflict_PRI_SGP$normalisedPRI - min(conflict_PRI_SGP$normalisedPRI) + 2)
conflict_PRI_SGP$logtfPRI <- log(conflict_PRI_SGP$tfPRI - min(conflict_PRI_SGP$tfPRI) + 2)
annual_conflict_PRI_SGP <- conflict_PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_SGP) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_SGP$actor <- gsub('GOV','Government',conflict_PRI_SGP$actor)
conflict_PRI_SGP$partner <- gsub('SGP','Singapore',conflict_PRI_SGP$partner)
monthly_conflict_PRI_SGP_plot <- ggplot(conflict_PRI_SGP[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_SGP_plot <- ggplot(conflict_PRI_SGP[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_SGP <- conflict_PRI_SGP %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_SGP$actor <- gsub('GOV','Government',annual_conflict_PRI_SGP$actor)
annual_conflict_PRI_SGP$partner <- gsub('SGP','Singapore',annual_conflict_PRI_SGP$partner)
annual_conflict_PRI_SGP_plot <- ggplot(annual_conflict_PRI_SGP[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_SGP_plot <- ggplot(annual_conflict_PRI_SGP[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Singapore's annual aggconf PRI
write.csv(annual_conflict_PRI_SGP[1:20,],"annual_conflict_PRI_SGP.csv")
#save Singapore's monthly aggconf PRI
write.csv(conflict_PRI_SGP[1:240,],"monthly_conflict_PRI_SGP.csv")

#Thailand's aggregate conflict PRIs
conflict_PRI_THA <- rbind.data.frame(PRI61)
conflict_PRI_THA$logPRI <- log(conflict_PRI_THA$normalisedPRI - min(conflict_PRI_THA$normalisedPRI) + 2)
conflict_PRI_THA$logtfPRI <- log(conflict_PRI_THA$tfPRI - min(conflict_PRI_THA$tfPRI) + 2)
annual_conflict_PRI_THA <- conflict_PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_THA) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_THA$actor <- gsub('GOV','Government',conflict_PRI_THA$actor)
conflict_PRI_THA$partner <- gsub('THA','Thailand',conflict_PRI_THA$partner)
monthly_conflict_PRI_THA_plot <- ggplot(conflict_PRI_THA[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_THA_plot <- ggplot(conflict_PRI_THA[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_THA <- conflict_PRI_THA %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_THA$actor <- gsub('GOV','Government',annual_conflict_PRI_THA$actor)
annual_conflict_PRI_THA$partner <- gsub('THA','Thailand',annual_conflict_PRI_THA$partner)
annual_conflict_PRI_THA_plot <- ggplot(annual_conflict_PRI_THA[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_THA_plot <- ggplot(annual_conflict_PRI_THA[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Thailand's annual aggconf PRI
write.csv(annual_conflict_PRI_THA[1:20,],"annual_conflict_PRI_THA.csv")
#save Thailand's monthly aggconf PRI
write.csv(conflict_PRI_THA[1:240,],"monthly_conflict_PRI_THA.csv")

#Vietnam's aggregate conflict PRIs
conflict_PRI_VNM <- rbind.data.frame(PRI21)
conflict_PRI_VNM$logPRI <- log(conflict_PRI_VNM$normalisedPRI - min(conflict_PRI_VNM$normalisedPRI) + 2)
conflict_PRI_VNM$logtfPRI <- log(conflict_PRI_VNM$tfPRI - min(conflict_PRI_VNM$tfPRI) + 2)
annual_conflict_PRI_VNM <- conflict_PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
colnames(annual_conflict_PRI_VNM) <- c("partner","actor","MY","PRI","normalisedPRI","logPRI","logtfPRI") 
conflict_PRI_VNM$actor <- gsub('GOV','Government',conflict_PRI_VNM$actor)
conflict_PRI_VNM$partner <- gsub('VNM','Vietnam',conflict_PRI_VNM$partner)
monthly_conflict_PRI_VNM_plot <- ggplot(conflict_PRI_VNM[1:240,], aes(MY, logPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
monthly_tfconflict_PRI_VNM_plot <- ggplot(conflict_PRI_VNM[1:240,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=0.5)+ theme_classic() + labs(x = "Year", y = "Log Monthly tfPRI towards China") + facet_wrap(partner~.,ncol=5) + guides(scale="none") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) + ylim(0,2)
annual_conflict_PRI_VNM <- conflict_PRI_VNM %>%
  mutate(year = year(MY)) %>%
  group_by(partner,actor,year) %>%
  summarise(PRI=sum(PRI),normalisedPRI=mean(normalisedPRI), logPRI=sum(logPRI),logtfPRI=mean(logtfPRI))
annual_conflict_PRI_VNM$actor <- gsub('GOV','Government',annual_conflict_PRI_VNM$actor)
annual_conflict_PRI_VNM$partner <- gsub('VNM','Vietnam',annual_conflict_PRI_VNM$partner)
annual_conflict_PRI_VNM_plot <- ggplot(annual_conflict_PRI_VNM[1:20,], aes(year, logPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual PRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(5,20) #+ guides(color=F) + ylim(0,10)
annual_tfconflict_PRI_VNM_plot <- ggplot(annual_conflict_PRI_VNM[1:20,], aes(year, logtfPRI)) + geom_line(aes(linetype = actor), size=0.5) + theme_classic() + labs(x = "Year", y = "Log Annual tfPRI towards China") + facet_wrap(partner~.,ncol=5)  + guides(scale="none") + scale_x_continuous(breaks = c(2005,2010,2015)) + ylim(0.7,1.6) #+ guides(color=F) + ylim(1.25,1.75)
#save Vietnam's annual aggconf PRI
write.csv(annual_conflict_PRI_VNM[1:20,],"annual_conflict_PRI_VNM.csv")
#save Vietnam's monthly aggconf PRI
write.csv(conflict_PRI_VNM[1:240,],"monthly_conflict_PRI_VNM.csv")

#ARIMA modelling
#Table 2
sink(file = "Table 2.txt")
#monthly trade-filtered net cooperation PRIs
#Indonesia 
auto.arima(PRI_IDN[1:240,]$logtfPRI)
#Malaysia
auto.arima(PRI_MYS[1:240,]$logtfPRI)
#Philippines
auto.arima(PRI_PHL[1:240,]$logtfPRI)
#Singapore
auto.arima(PRI_SGP[1:240,]$logtfPRI)
#Thailand
auto.arima(PRI_THA[1:240,]$logtfPRI)
#Vietnam
auto.arima(PRI_VNM[1:240,]$logtfPRI)

#monthly trade-filtered aggregate cooperation PRIs
#Indonesia 
auto.arima(coop_PRI_IDN[1:240,]$logtfPRI)
#Malaysia
auto.arima(coop_PRI_MYS[1:240,]$logtfPRI)
#Philippines
auto.arima(coop_PRI_PHL[1:240,]$logtfPRI)
#Singapore
auto.arima(coop_PRI_SGP[1:240,]$logtfPRI)
#Thailand
auto.arima(coop_PRI_THA[1:240,]$logtfPRI)
#Vietnam
auto.arima(coop_PRI_VNM[1:240,]$logtfPRI)

#monthly trade-filtered aggregate conflict PRIs
#Indonesia 
auto.arima(conflict_PRI_IDN[1:240,]$logtfPRI)
#Malaysia
auto.arima(conflict_PRI_MYS[1:240,]$logtfPRI)
#Philippines
auto.arima(conflict_PRI_PHL[1:240,]$logtfPRI)
#Singapore
auto.arima(conflict_PRI_SGP[1:240,]$logtfPRI)
#Thailand
auto.arima(conflict_PRI_THA[1:240,]$logtfPRI)
#Vietnam
auto.arima(conflict_PRI_VNM[1:240,]$logtfPRI)
sink(file = NULL)

#Table 3
#white noise test
sink(file = "Table 3_btest_netcoop.txt")
#monthly trade-filtered net cooperation PRIs
#Indonesia
bartlettB.test(PRI_IDN[1:240,]$logtfPRI)
#Malaysia
bartlettB.test(PRI_MYS[1:240,]$logtfPRI)
#Philippines
bartlettB.test(PRI_PHL[1:240,]$logtfPRI)
#Singapore
bartlettB.test(PRI_SGP[1:240,]$logtfPRI)
#Thailand
bartlettB.test(PRI_THA[1:240,]$logtfPRI)
#Vietnam
bartlettB.test(PRI_VNM[1:240,]$logtfPRI)
sink(file = NULL)

#monthly trade-filtered aggregate cooperation PRIs
sink(file = "Table 3_btest_aggcoop.txt")
#Indonesia
bartlettB.test(coop_PRI_IDN[1:240,]$logtfPRI)
#Malaysia
bartlettB.test(coop_PRI_MYS[1:240,]$logtfPRI)
#Philippines
bartlettB.test(coop_PRI_PHL[1:240,]$logtfPRI)
#Singapore
bartlettB.test(coop_PRI_SGP[1:240,]$logtfPRI)
#Thailand
bartlettB.test(coop_PRI_THA[1:240,]$logtfPRI)
#Vietnam
bartlettB.test(coop_PRI_VNM[1:240,]$logtfPRI)
sink(file = NULL)

#monthly trade-filtered aggregate conflict PRIs
sink(file = "Table 3_btest_aggconf.txt")
#Indonesia
bartlettB.test(conflict_PRI_IDN[1:240,]$logtfPRI)
#Malaysia
bartlettB.test(conflict_PRI_MYS[1:240,]$logtfPRI)
#Philippines
bartlettB.test(conflict_PRI_PHL[1:240,]$logtfPRI)
#Singapore
bartlettB.test(conflict_PRI_SGP[1:240,]$logtfPRI)
#Thailand
bartlettB.test(conflict_PRI_THA[1:240,]$logtfPRI)
#Vietnam
bartlettB.test(conflict_PRI_VNM[1:240,]$logtfPRI)
sink(file = NULL)

#spectral density analysis
#low/high frequency share
#monthly trade-filtered net cooperation PRIs
#Indonesia
spec_IDNtf_netcoop <- spectrum(diff(PRI_IDN[1:240,]$logtfPRI));b_IDNtf_netcoop <- spec_IDNtf_netcoop[["spec"]];b_total_IDNtf_netcoop <- sum(b_IDNtf_netcoop[1:120])
b_low_IDNtf_netcoop <- sum(b_IDNtf_netcoop[1:20]);b_high_IDNtf_netcoop <- sum(b_IDNtf_netcoop[80:120])
low_freq_IDNtf_netcoop <- b_low_IDNtf_netcoop/b_total_IDNtf_netcoop;high_freq_IDNtf_netcoop <- b_high_IDNtf_netcoop/b_total_IDNtf_netcoop
sink(file = "Table 3_fqshare_netcoop_IDN.txt")
low_freq_IDNtf_netcoop
high_freq_IDNtf_netcoop
sink(file = NULL)
#Malaysia
spec_MYStf_netcoop <- spectrum(diff(PRI_MYS[1:240,]$logtfPRI));b_MYStf_netcoop <- spec_MYStf_netcoop[["spec"]];b_total_MYStf_netcoop <- sum(b_MYStf_netcoop[1:120])
b_low_MYStf_netcoop <- sum(b_MYStf_netcoop[1:20]);b_high_MYStf_netcoop <- sum(b_MYStf_netcoop[80:120])
low_freq_MYStf_netcoop <- b_low_MYStf_netcoop/b_total_MYStf_netcoop;high_freq_MYStf_netcoop <- b_high_MYStf_netcoop/b_total_MYStf_netcoop
sink(file = "Table 3_fqshare_netcoop_MYS.txt")
low_freq_MYStf_netcoop
high_freq_MYStf_netcoop
sink(file = NULL)
#Philippines
spec_PHLtf_netcoop <- spectrum(diff(PRI_PHL[1:240,]$logtfPRI));b_PHLtf_netcoop <- spec_PHLtf_netcoop[["spec"]];b_total_PHLtf_netcoop <- sum(b_PHLtf_netcoop[1:120])
b_low_PHLtf_netcoop <- sum(b_PHLtf_netcoop[1:20]);b_high_PHLtf_netcoop <- sum(b_PHLtf_netcoop[80:120])
low_freq_PHLtf_netcoop <- b_low_PHLtf_netcoop/b_total_PHLtf_netcoop;high_freq_PHLtf_netcoop <- b_high_PHLtf_netcoop/b_total_PHLtf_netcoop
sink(file = "Table 3_fqshare_netcoop_PHL.txt")
low_freq_PHLtf_netcoop
high_freq_PHLtf_netcoop
sink(file = NULL)
#Singapore
spec_SGPtf_netcoop <- spectrum(diff(PRI_SGP[1:240,]$logtfPRI));b_SGPtf_netcoop <- spec_SGPtf_netcoop[["spec"]];b_total_SGPtf_netcoop <- sum(b_SGPtf_netcoop[1:120])
b_low_SGPtf_netcoop <- sum(b_SGPtf_netcoop[1:20]);b_high_SGPtf_netcoop <- sum(b_SGPtf_netcoop[80:120])
low_freq_SGPtf_netcoop <- b_low_SGPtf_netcoop/b_total_SGPtf_netcoop;high_freq_SGPtf_netcoop <- b_high_SGPtf_netcoop/b_total_SGPtf_netcoop
sink(file = "Table 3_fqshare_netcoop_SGP.txt")
low_freq_SGPtf_netcoop
high_freq_SGPtf_netcoop
sink(file = NULL)
#Thailand
spec_THAtf_netcoop <- spectrum(diff(PRI_THA[1:240,]$logtfPRI));b_THAtf_netcoop <- spec_THAtf_netcoop[["spec"]];b_total_THAtf_netcoop <- sum(b_THAtf_netcoop[1:120])
b_low_THAtf_netcoop <- sum(b_THAtf_netcoop[1:20]);b_high_THAtf_netcoop <- sum(b_THAtf_netcoop[80:120])
low_freq_THAtf_netcoop <- b_low_THAtf_netcoop/b_total_THAtf_netcoop;high_freq_THAtf_netcoop <- b_high_THAtf_netcoop/b_total_THAtf_netcoop
sink(file = "Table 3_fqshare_netcoop_THA.txt")
low_freq_THAtf_netcoop
high_freq_THAtf_netcoop
sink(file = NULL)
#Vietnam
spec_VNMtf_netcoop <- spectrum(diff(PRI_VNM[1:240,]$logtfPRI));b_VNMtf_netcoop <- spec_VNMtf_netcoop[["spec"]];b_total_VNMtf_netcoop <- sum(b_VNMtf_netcoop[1:120])
b_low_VNMtf_netcoop <- sum(b_VNMtf_netcoop[1:20]);b_high_VNMtf_netcoop <- sum(b_VNMtf_netcoop[80:120])
low_freq_VNMtf_netcoop <- b_low_VNMtf_netcoop/b_total_VNMtf_netcoop;high_freq_VNMtf_netcoop <- b_high_VNMtf_netcoop/b_total_VNMtf_netcoop
sink(file = "Table 3_fqshare_netcoop_VNM.txt")
low_freq_VNMtf_netcoop
high_freq_VNMtf_netcoop
sink(file = NULL)

#monthly trade-filtered aggregate cooperation PRIs
#Indonesia
spec_IDNtf_aggcoop <- spectrum(diff(coop_PRI_IDN[1:240,]$logtfPRI));b_IDNtf_aggcoop <- spec_IDNtf_aggcoop[["spec"]];b_total_IDNtf_aggcoop <- sum(b_IDNtf_aggcoop[1:120])
b_low_IDNtf_aggcoop <- sum(b_IDNtf_aggcoop[1:20]);b_high_IDNtf_aggcoop <- sum(b_IDNtf_aggcoop[80:120])
low_freq_IDNtf_aggcoop <- b_low_IDNtf_aggcoop/b_total_IDNtf_aggcoop;high_freq_IDNtf_aggcoop <- b_high_IDNtf_aggcoop/b_total_IDNtf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_IDN.txt")
low_freq_IDNtf_aggcoop
high_freq_IDNtf_aggcoop
sink(file = NULL)
#Malaysia
spec_MYStf_aggcoop <- spectrum(diff(coop_PRI_MYS[1:240,]$logtfPRI));b_MYStf_aggcoop <- spec_MYStf_aggcoop[["spec"]];b_total_MYStf_aggcoop <- sum(b_MYStf_aggcoop[1:120])
b_low_MYStf_aggcoop <- sum(b_MYStf_aggcoop[1:20]);b_high_MYStf_aggcoop <- sum(b_MYStf_aggcoop[80:120])
low_freq_MYStf_aggcoop <- b_low_MYStf_aggcoop/b_total_MYStf_aggcoop;high_freq_MYStf_aggcoop <- b_high_MYStf_aggcoop/b_total_MYStf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_MYS.txt")
low_freq_MYStf_aggcoop
high_freq_MYStf_aggcoop
sink(file = NULL)
#Philippines
spec_PHLtf_aggcoop <- spectrum(diff(coop_PRI_PHL[1:240,]$logtfPRI));b_PHLtf_aggcoop <- spec_PHLtf_aggcoop[["spec"]];b_total_PHLtf_aggcoop <- sum(b_PHLtf_aggcoop[1:120])
b_low_PHLtf_aggcoop <- sum(b_PHLtf_aggcoop[1:20]);b_high_PHLtf_aggcoop <- sum(b_PHLtf_aggcoop[80:120])
low_freq_PHLtf_aggcoop <- b_low_PHLtf_aggcoop/b_total_PHLtf_aggcoop;high_freq_PHLtf_aggcoop <- b_high_PHLtf_aggcoop/b_total_PHLtf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_PHL.txt")
low_freq_PHLtf_aggcoop
high_freq_PHLtf_aggcoop
sink(file = NULL)
#Singapore
spec_SGPtf_aggcoop <- spectrum(diff(coop_PRI_SGP[1:240,]$logtfPRI));b_SGPtf_aggcoop <- spec_SGPtf_aggcoop[["spec"]];b_total_SGPtf_aggcoop <- sum(b_SGPtf_aggcoop[1:120])
b_low_SGPtf_aggcoop <- sum(b_SGPtf_aggcoop[1:20]);b_high_SGPtf_aggcoop <- sum(b_SGPtf_aggcoop[80:120])
low_freq_SGPtf_aggcoop <- b_low_SGPtf_aggcoop/b_total_SGPtf_aggcoop;high_freq_SGPtf_aggcoop <- b_high_SGPtf_aggcoop/b_total_SGPtf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_SGP.txt")
low_freq_SGPtf_aggcoop
high_freq_SGPtf_aggcoop
sink(file = NULL)
#Thailand
spec_THAtf_aggcoop <- spectrum(diff(coop_PRI_THA[1:240,]$logtfPRI));b_THAtf_aggcoop <- spec_THAtf_aggcoop[["spec"]];b_total_THAtf_aggcoop <- sum(b_THAtf_aggcoop[1:120])
b_low_THAtf_aggcoop <- sum(b_THAtf_aggcoop[1:20]);b_high_THAtf_aggcoop <- sum(b_THAtf_aggcoop[80:120])
low_freq_THAtf_aggcoop <- b_low_THAtf_aggcoop/b_total_THAtf_aggcoop;high_freq_THAtf_aggcoop <- b_high_THAtf_aggcoop/b_total_THAtf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_THA.txt")
low_freq_THAtf_aggcoop
high_freq_THAtf_aggcoop
sink(file = NULL)
#Vietnam
spec_VNMtf_aggcoop <- spectrum(diff(coop_PRI_VNM[1:240,]$logtfPRI));b_VNMtf_aggcoop <- spec_VNMtf_aggcoop[["spec"]];b_total_VNMtf_aggcoop <- sum(b_VNMtf_aggcoop[1:120])
b_low_VNMtf_aggcoop <- sum(b_VNMtf_aggcoop[1:20]);b_high_VNMtf_aggcoop <- sum(b_VNMtf_aggcoop[80:120])
low_freq_VNMtf_aggcoop <- b_low_VNMtf_aggcoop/b_total_VNMtf_aggcoop;high_freq_VNMtf_aggcoop <- b_high_VNMtf_aggcoop/b_total_VNMtf_aggcoop
sink(file = "Table 3_fqshare_aggcoop_VNM.txt")
low_freq_VNMtf_aggcoop
high_freq_VNMtf_aggcoop
sink(file = NULL)

#monthly trade-filtered aggregate conflict PRIs
#Indonesia
spec_IDNtf_aggconf <- spectrum(diff(conflict_PRI_IDN[1:240,]$logtfPRI));b_IDNtf_aggconf <- spec_IDNtf_aggconf[["spec"]];b_total_IDNtf_aggconf <- sum(b_IDNtf_aggconf[1:120])
b_low_IDNtf_aggconf <- sum(b_IDNtf_aggconf[1:20]);b_high_IDNtf_aggconf <- sum(b_IDNtf_aggconf[80:120])
low_freq_IDNtf_aggconf <- b_low_IDNtf_aggconf/b_total_IDNtf_aggconf;high_freq_IDNtf_aggconf <- b_high_IDNtf_aggconf/b_total_IDNtf_aggconf
sink(file = "Table 3_fqshare_aggconf_IDN.txt")
low_freq_IDNtf_aggconf
high_freq_IDNtf_aggconf
sink(file = NULL)
#Malaysia
spec_MYStf_aggconf <- spectrum(diff(conflict_PRI_MYS[1:240,]$logtfPRI));b_MYStf_aggconf <- spec_MYStf_aggconf[["spec"]];b_total_MYStf_aggconf <- sum(b_MYStf_aggconf[1:120])
b_low_MYStf_aggconf <- sum(b_MYStf_aggconf[1:20]);b_high_MYStf_aggconf <- sum(b_MYStf_aggconf[80:120])
low_freq_MYStf_aggconf <- b_low_MYStf_aggconf/b_total_MYStf_aggconf;high_freq_MYStf_aggconf <- b_high_MYStf_aggconf/b_total_MYStf_aggconf
sink(file = "Table 3_fqshare_aggconf_MYS.txt")
low_freq_MYStf_aggconf
high_freq_MYStf_aggconf
sink(file = NULL)
#Philippines
spec_PHLtf_aggconf <- spectrum(diff(conflict_PRI_PHL[1:240,]$logtfPRI));b_PHLtf_aggconf <- spec_PHLtf_aggconf[["spec"]];b_total_PHLtf_aggconf <- sum(b_PHLtf_aggconf[1:120])
b_low_PHLtf_aggconf <- sum(b_PHLtf_aggconf[1:20]);b_high_PHLtf_aggconf <- sum(b_PHLtf_aggconf[80:120])
low_freq_PHLtf_aggconf <- b_low_PHLtf_aggconf/b_total_PHLtf_aggconf;high_freq_PHLtf_aggconf <- b_high_PHLtf_aggconf/b_total_PHLtf_aggconf
sink(file = "Table 3_fqshare_aggconf_PHL.txt")
low_freq_PHLtf_aggconf
high_freq_PHLtf_aggconf
sink(file = NULL)
#Singapore
spec_SGPtf_aggconf <- spectrum(diff(conflict_PRI_SGP[1:240,]$logtfPRI));b_SGPtf_aggconf <- spec_SGPtf_aggconf[["spec"]];b_total_SGPtf_aggconf <- sum(b_SGPtf_aggconf[1:120])
b_low_SGPtf_aggconf <- sum(b_SGPtf_aggconf[1:20]);b_high_SGPtf_aggconf <- sum(b_SGPtf_aggconf[80:120])
low_freq_SGPtf_aggconf <- b_low_SGPtf_aggconf/b_total_SGPtf_aggconf;high_freq_SGPtf_aggconf <- b_high_SGPtf_aggconf/b_total_SGPtf_aggconf
sink(file = "Table 3_fqshare_aggconf_SGP.txt")
low_freq_SGPtf_aggconf
high_freq_SGPtf_aggconf
sink(file = NULL)
#Thailand
spec_THAtf_aggconf <- spectrum(diff(conflict_PRI_THA[1:240,]$logtfPRI));b_THAtf_aggconf <- spec_THAtf_aggconf[["spec"]];b_total_THAtf_aggconf <- sum(b_THAtf_aggconf[1:120])
b_low_THAtf_aggconf <- sum(b_THAtf_aggconf[1:20]);b_high_THAtf_aggconf <- sum(b_THAtf_aggconf[80:120])
low_freq_THAtf_aggconf <- b_low_THAtf_aggconf/b_total_THAtf_aggconf;high_freq_THAtf_aggconf <- b_high_THAtf_aggconf/b_total_THAtf_aggconf
sink(file = "Table 3_fqshare_aggconf_THA.txt")
low_freq_THAtf_aggconf
high_freq_THAtf_aggconf
sink(file = NULL)
#Vietnam
spec_VNMtf_aggconf <- spectrum(diff(conflict_PRI_VNM[1:240,]$logtfPRI));b_VNMtf_aggconf <- spec_VNMtf_aggconf[["spec"]];b_total_VNMtf_aggconf <- sum(b_VNMtf_aggconf[1:120])
b_low_VNMtf_aggconf <- sum(b_VNMtf_aggconf[1:20]);b_high_VNMtf_aggconf <- sum(b_VNMtf_aggconf[80:120])
low_freq_VNMtf_aggconf <- b_low_VNMtf_aggconf/b_total_VNMtf_aggconf;high_freq_VNMtf_aggconf <- b_high_VNMtf_aggconf/b_total_VNMtf_aggconf
sink(file = "Table 3_fqshare_aggconf_VNM.txt")
low_freq_VNMtf_aggconf
high_freq_VNMtf_aggconf
sink(file = NULL)

#Figure 3
#The South China Sea Arbitration (Philippines-China)
#trade-filtered Philippines monthly PRI towards China 2012-2018
casestudy_monthly_tfPRI_PHL_plot <- ggplot(PRI_PHL[145:205,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=1)+ theme_linedraw() + labs(x = "Year", y = "Log Monthly PRI towards China", title = "The Philippines' monthly trade-filtered net cooperation PRI") + guides(scale="none") + rremove("legend") + rremove("grid") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), plot.title = element_text(hjust = 0.5), plot.caption=element_text(hjust = 0.5)) + ylim(0.5,2) + geom_label_repel(data = PRI_PHL[150,], label = stringr::str_wrap("Scarborough Shoal standoff", 2), direction = c("y"), nudge_y = -0.6, force = 1,fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = PRI_PHL[158,], label = stringr::str_wrap("Arbitration begins against China", 2), direction = c("y"), nudge_y = 0.6, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = PRI_PHL[182,], label = stringr::str_wrap("Manila promotes case against China", 2), direction = c("y"), nudge_y = 0.8, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = PRI_PHL[191,], label = stringr::str_wrap("Xi attends APEC in Manila", 2), direction = c("y"), nudge_y = -1, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = PRI_PHL[199,], label = stringr::str_wrap("Duterte meets Xi", 2), direction = c("y"), nudge_y = -1, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2)
#trade-filtered aggregate cooperation monthly PRI towards China 2012-2018
casestudy_monthly_coop_tfPRI_PHL_plot <- ggplot(coop_PRI_PHL[145:205,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=1)+ theme_linedraw() + labs(x = "Year", y = "Log Monthly PRI towards China", title = "The Philippines' monthly trade-filtered aggregate cooperation PRI") + guides(scale="none") + rremove("legend") + rremove("grid") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), plot.title = element_text(hjust = 0.5), plot.caption=element_text(hjust = 0.5)) + ylim(0.5,2) + geom_label_repel(data = coop_PRI_PHL[150,], label = stringr::str_wrap("Scarborough Shoal standoff", 2), direction = c("y"), nudge_y = -0.6, force = 1,fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = coop_PRI_PHL[158,], label = stringr::str_wrap("Arbitration begins against China", 2), direction = c("y"), nudge_y = 0.6, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = coop_PRI_PHL[182,], label = stringr::str_wrap("Manila promotes case against China", 2), direction = c("y"), nudge_y = 0.6, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = coop_PRI_PHL[191,], label = stringr::str_wrap("Xi attends APEC in Manila", 2), direction = c("y"),nudge_y = -1, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = coop_PRI_PHL[199,], label = stringr::str_wrap("Duterte meets Xi", 2), direction = c("y"),nudge_y = -1, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2)
#trade-filtered aggregate conflict monthly PRI towards China 2012-2018
casestudy_monthly_conflict_tfPRI_PHL_plot <- ggplot(conflict_PRI_PHL[145:205,], aes(MY, logtfPRI)) +geom_line(aes(linetype = actor), size=1)+ theme_linedraw() + labs(x = "Year", y = "Log Monthly PRI towards China", title = "The Philippines' monthly trade-filtered aggregate conflict PRI") + guides(scale="none") + rremove("legend") + rremove("grid") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), plot.title = element_text(hjust = 0.5), plot.caption=element_text(hjust = 0.5)) + ylim(0.5,2) + geom_label_repel(data = conflict_PRI_PHL[150,], label = stringr::str_wrap("Scarborough Shoal standoff", 2), direction = c("y"), nudge_y = -0.6, force = 1,fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = conflict_PRI_PHL[158,], label = stringr::str_wrap("Arbitration begins against China", 2), direction = c("y"), nudge_y = 0.6, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = conflict_PRI_PHL[182,], label = stringr::str_wrap("Manila promotes case against China", 2), direction = c("y"), nudge_y = 0.9, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = conflict_PRI_PHL[191,], label = stringr::str_wrap("Xi attends APEC in Manila", 2), direction = c("y"),nudge_y = 0.6, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2) + geom_label_repel(data = conflict_PRI_PHL[199,], label = stringr::str_wrap("Duterte meets Xi", 2), direction = c("y"),nudge_y = -1, force = 1, fontface = "plain", color = "grey22", fill = "white",size = 2)
#combine plots, and annotate
(layout_matrix <- matrix(c(1,1,1,2,2,2,3,3,3), nrow = 3, byrow = TRUE))
casestudy_ALL_grid_PHL <- grid.arrange(casestudy_monthly_tfPRI_PHL_plot + rremove("xlab")+ rremove("ylab") + rremove("legend"), casestudy_monthly_coop_tfPRI_PHL_plot + rremove("xlab")+ rremove("ylab") + rremove("legend"), casestudy_monthly_conflict_tfPRI_PHL_plot+ rremove("xlab")+ rremove("ylab") + rremove("legend"),layout_matrix = layout_matrix)
annotate_figure(casestudy_ALL_grid_PHL, bottom = "Year", left = "log monthly PRI towards China")

#Econometric approach
#install packages
install.packages("vars")
install.packages("tseries")
#load packages
library(vars)
library(tseries)
#data wrangling, transformations, and summary statistics
##get data
exports_ALL = read.csv("(01) exports.csv") #exports data
reer_ALL = read.csv("(04) reer.csv") #real effective exchange rate data
industrial_production_ALL = read.csv("(03) industrialproduction.csv") #industrial production data

#summary statistics
#Table 1
#exports
sink(file = "Table 1_exports.txt")
mean(exports_ALL$exports)
summary(exports_ALL$exports)
sd(exports_ALL$exports)
length(exports_ALL$exports)
sink(file = NULL)
#log monthly trade-filtered net cooperation PRIs
sink(file = "Table 1_logmonthlynetcoopPRIs.txt")
PRIs_ALL_stacked <- rbind(PRI_IDN,PRI_MYS,PRI_PHL,PRI_SGP,PRI_THA,PRI_VNM)
monthly_logtfPRI_summ_gov <- subset(PRIs_ALL_stacked, actor == "Government", select = c("actor", "logtfPRI"))
monthly_logtfPRI_summ_gov <- monthly_logtfPRI_summ_gov[-c(1201:1296),]
summary(monthly_logtfPRI_summ_gov);sd(monthly_logtfPRI_summ_gov$logtfPRI)
sink(file = NULL)
#log monthly trade-filtered aggregate cooperation PRIs
sink(file = "Table 1_logmonthlyaggcoopPRIs.txt")
coop_PRIs_ALL_stacked <- rbind(coop_PRI_IDN,coop_PRI_MYS,coop_PRI_PHL,coop_PRI_SGP,coop_PRI_THA,coop_PRI_VNM)
coop_monthly_logtfPRI_summ_gov <- subset(coop_PRIs_ALL_stacked, actor == "Government", select = c("actor", "logtfPRI"))
coop_monthly_logtfPRI_summ_gov <-  coop_monthly_logtfPRI_summ_gov[-c(1201:1296),]
summary(coop_monthly_logtfPRI_summ_gov);sd(coop_monthly_logtfPRI_summ_gov$logtfPRI)
sink(file = NULL)
#log monthly trade-filtered aggregate conflict PRIs
sink(file = "Table 1_logmonthlyaggconfPRIs.txt")
conflict_PRIs_ALL_stacked <- rbind(conflict_PRI_IDN,conflict_PRI_MYS,conflict_PRI_PHL,conflict_PRI_SGP,conflict_PRI_THA,conflict_PRI_VNM)
conflict_logtfPRI_summ_gov <- subset(conflict_PRIs_ALL_stacked, actor == "Government", select = c("actor", "logtfPRI"))
conflict_logtfPRI_summ_gov <- conflict_logtfPRI_summ_gov[-c(1201:1296),]
summary(conflict_logtfPRI_summ_gov);sd(conflict_logtfPRI_summ_gov$logtfPRI)
sink(file = NULL)
#industrial production index—China
sink(file = "Table 1_industrialproductionindex_CHN.txt")
mean(industrial_production_ALL$IIP[1345:1584])
summary(industrial_production_ALL$IIP[1345:1584])
sd(industrial_production_ALL$IIP[1345:1584])
length(industrial_production_ALL$IIP[1345:1584])
sink(file = NULL)
#industrial production index—Southeast Asian countries
sink(file = "Table 1_industrialproductionindex_SEA.txt")
mean(industrial_production_ALL$IIP[1:1344])
summary(industrial_production_ALL$IIP[1:1344])
sd(industrial_production_ALL$IIP[1:1344])
length(industrial_production_ALL$IIP[1:1344])
sink(file = NULL)
#real effective exchange rate
sink(file = "Table 1_reer.txt")
mean(reer_ALL$reer)
summary(reer_ALL$reer)
sd(reer_ALL$reer)
length(reer_ALL$reer)
sink(file = NULL)

#take natural log and seasonally adjust raw exports data
#Indonesia
CHN_IDN_ExpSen <- ts(log(exports_ALL[1:240,]$exports), start = c(2000,1), frequency = 12)
decompose_CHN_IDN_ExpSen  = decompose(CHN_IDN_ExpSen, "additive")
plot(decompose_CHN_IDN_ExpSen)
adjusted_CHN_IDN_ExpSen  = CHN_IDN_ExpSen - decompose_CHN_IDN_ExpSen$seasonal
plot(adjusted_CHN_IDN_ExpSen)
adj_exports_IDN_df <- data.frame(adjusted_CHN_IDN_ExpSen)
#Malaysia
CHN_MYS_ExpSen <- ts(log(exports_ALL[241:480,]$exports), start = c(2000,1), frequency = 12)
decompose_CHN_MYS_ExpSen  = decompose(CHN_MYS_ExpSen, "additive")
plot(decompose_CHN_MYS_ExpSen)
adjusted_CHN_MYS_ExpSen  = CHN_MYS_ExpSen - decompose_CHN_MYS_ExpSen$seasonal
plot(adjusted_CHN_MYS_ExpSen)
adj_exports_MYS_df <- data.frame(adjusted_CHN_MYS_ExpSen)
#Philippines
CHN_PHL_ExpSen <- ts(log(exports_ALL[481:720,]$exports), start = c(2000,1), frequency = 12)
decompose_CHN_PHL_ExpSen  = decompose(CHN_PHL_ExpSen, "additive")
plot(decompose_CHN_PHL_ExpSen)
adjusted_CHN_PHL_ExpSen  = CHN_PHL_ExpSen - decompose_CHN_PHL_ExpSen$seasonal
plot(adjusted_CHN_PHL_ExpSen)
adj_exports_PHL_df <- data.frame(adjusted_CHN_PHL_ExpSen)
#Singapore
CHN_SGP_ExpSen <- ts(log(exports_ALL[721:960,]$exports), start = c(2000,1), frequency = 12)
decompose_CHN_SGP_ExpSen  = decompose(CHN_SGP_ExpSen, "additive")
plot(decompose_CHN_SGP_ExpSen)
adjusted_CHN_SGP_ExpSen  = CHN_SGP_ExpSen - decompose_CHN_SGP_ExpSen$seasonal
plot(adjusted_CHN_SGP_ExpSen)
adj_exports_SGP_df <- data.frame(adjusted_CHN_SGP_ExpSen)
#Thailand
CHN_THA_ExpSen <- ts(log(exports_ALL[961:1200,]$exports), start = c(2000,1), frequency = 12)
decompose_CHN_THA_ExpSen  = decompose(CHN_THA_ExpSen, "additive")
plot(decompose_CHN_THA_ExpSen)
adjusted_CHN_THA_ExpSen  = CHN_THA_ExpSen - decompose_CHN_THA_ExpSen$seasonal
plot(adjusted_CHN_THA_ExpSen)
adj_exports_THA_df <- data.frame(adjusted_CHN_THA_ExpSen)
#Vietnam
CHN_VNM_ExpSen <- ts(log(exports_ALL[1201:1344,]$exports), start = c(2008,1), frequency = 12)
decompose_CHN_VNM_ExpSen  = decompose(CHN_VNM_ExpSen, "additive")
plot(decompose_CHN_VNM_ExpSen)
adjusted_CHN_VNM_ExpSen  = CHN_VNM_ExpSen - decompose_CHN_VNM_ExpSen$seasonal
plot(adjusted_CHN_VNM_ExpSen)
adj_exports_VNM_df <- data.frame(adjusted_CHN_VNM_ExpSen)

#Augmented Dickey-Fuller (ADF) tests
#levels versus first differences
#monthly trade-filtered net cooperation PRIs
#Indonesia
#00-19
adf.test(PRI_IDN$logtfPRI)
PRI_IDN_diff <- diff(PRI_IDN$logtfPRI, differences = 1)
adf.test(PRI_IDN_diff)
#00-09
adf.test(PRI_IDN$logtfPRI[1:120])
PRI_IDN_diff_09 <- diff(PRI_IDN$logtfPRI[1:120], differences = 1)
adf.test(PRI_IDN_diff_09)
#10-19
adf.test(PRI_IDN$logtfPRI[121:240])
PRI_IDN_diff_19 <- diff(PRI_IDN$logtfPRI[121:240], differences = 1)
adf.test(PRI_IDN_diff_19)
#Malaysia
#00-19
adf.test(PRI_MYS$logtfPRI)
PRI_MYS_diff <- diff(PRI_MYS$logtfPRI, differences = 1)
adf.test(PRI_MYS_diff)
#00-09
adf.test(PRI_MYS$logtfPRI[1:120])
PRI_MYS_diff_09 <- diff(PRI_MYS$logtfPRI[1:120], differences = 1)
adf.test(PRI_MYS_diff_09)
#10-19
adf.test(PRI_MYS$logtfPRI[121:240])
PRI_MYS_diff_19 <- diff(PRI_MYS$logtfPRI[121:240], differences = 1)
adf.test(PRI_MYS_diff_19)
#Philippines
#00-19
adf.test(PRI_PHL$logtfPRI)
PRI_PHL_diff <- diff(PRI_PHL$logtfPRI, differences = 1)
adf.test(PRI_PHL_diff)
#00-09
adf.test(PRI_PHL$logtfPRI[1:120])
PRI_PHL_diff_09 <- diff(PRI_PHL$logtfPRI[1:120], differences = 1)
adf.test(PRI_PHL_diff_09)
#10-19
adf.test(PRI_PHL$logtfPRI[121:240])
PRI_PHL_diff_19 <- diff(PRI_PHL$logtfPRI[121:240], differences = 1)
adf.test(PRI_PHL_diff_19)
#Singapore
#00-19
adf.test(PRI_SGP$logtfPRI)
PRI_SGP_diff <- diff(PRI_SGP$logtfPRI, differences = 1)
adf.test(PRI_SGP_diff)
#00-09
adf.test(PRI_SGP$logtfPRI[1:120])
PRI_SGP_diff_09 <- diff(PRI_SGP$logtfPRI[1:120], differences = 1)
adf.test(PRI_SGP_diff_09)
#10-19
adf.test(PRI_SGP$logtfPRI[121:240])
PRI_SGP_diff_19 <- diff(PRI_SGP$logtfPRI[121:240], differences = 1)
adf.test(PRI_SGP_diff_19)
#Thailand
#00-19
adf.test(PRI_THA$logtfPRI)
PRI_THA_diff <- diff(PRI_THA$logtfPRI, differences = 1)
adf.test(PRI_THA_diff)
#00-09
adf.test(PRI_THA$logtfPRI[1:120])
PRI_THA_diff_09 <- diff(PRI_THA$logtfPRI[1:120], differences = 1)
adf.test(PRI_THA_diff_09)
#10-19
adf.test(PRI_THA$logtfPRI[121:240])
PRI_THA_diff_19 <- diff(PRI_THA$logtfPRI[121:240], differences = 1)
adf.test(PRI_THA_diff_19)
#Vietnam
#08-19
adf.test(PRI_VNM$logtfPRI[97:240])
PRI_VNM_diff <- diff(PRI_VNM$logtfPRI[97:240], differences = 1)
adf.test(PRI_VNM_diff)
#08-09
adf.test(PRI_VNM$logtfPRI[97:120])
PRI_VNM_diff_09 <- diff(PRI_VNM$logtfPRI[97:120], differences = 1)
adf.test(PRI_VNM_diff_09)
#10-19
adf.test(PRI_VNM$logtfPRI[121:240])
PRI_VNM_diff_19 <- diff(PRI_VNM$logtfPRI[121:240], differences = 1)
adf.test(PRI_VNM_diff_19)
#monthly trade-filtered aggregate cooperation PRIs
#Indonesia
#00-19
adf.test(coop_PRI_IDN$logtfPRI)
coop_PRI_IDN_diff <- diff(coop_PRI_IDN$logtfPRI, differences = 1)
adf.test(coop_PRI_IDN_diff)
#00-09
adf.test(coop_PRI_IDN$logtfPRI[1:120])
coop_PRI_IDN_diff_09 <- diff(coop_PRI_IDN$logtfPRI[1:120], differences = 1)
adf.test(coop_PRI_IDN_diff_09)
#10-19
adf.test(coop_PRI_IDN$logtfPRI[121:240])
coop_PRI_IDN_diff_19 <- diff(coop_PRI_IDN$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_IDN_diff_19)
#Malaysia
#00-19
adf.test(coop_PRI_MYS$logtfPRI)
coop_PRI_MYS_diff <- diff(coop_PRI_MYS$logtfPRI, differences = 1)
adf.test(coop_PRI_MYS_diff)
#00-09
adf.test(coop_PRI_MYS$logtfPRI[1:120])
coop_PRI_MYS_diff_09 <- diff(coop_PRI_MYS$logtfPRI[1:120], differences = 1)
adf.test(coop_PRI_MYS_diff_09)
#10-19
adf.test(coop_PRI_MYS$logtfPRI[121:240])
coop_PRI_MYS_diff_19 <- diff(coop_PRI_MYS$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_MYS_diff_19)
#Philippines
#00-19
adf.test(coop_PRI_PHL$logtfPRI)
coop_PRI_PHL_diff <- diff(coop_PRI_PHL$logtfPRI, differences = 1)
adf.test(coop_PRI_PHL_diff)
#00-09
adf.test(coop_PRI_PHL$logtfPRI[1:120])
coop_PRI_PHL_diff_09 <- diff(coop_PRI_PHL$logtfPRI[1:120], differences = 1)
adf.test(coop_PRI_PHL_diff_09)
#10-19
adf.test(coop_PRI_PHL$logtfPRI[121:240])
coop_PRI_PHL_diff_19 <- diff(coop_PRI_PHL$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_PHL_diff_19)
#Singapore
#00-19
adf.test(coop_PRI_SGP$logtfPRI)
coop_PRI_SGP_diff <- diff(coop_PRI_SGP$logtfPRI, differences = 1)
adf.test(coop_PRI_SGP_diff)
#00-09
adf.test(coop_PRI_SGP$logtfPRI[1:120])
coop_PRI_SGP_diff_09 <- diff(coop_PRI_SGP$logtfPRI[1:120], differences = 1)
adf.test(coop_PRI_SGP_diff_09)
#10-19
adf.test(coop_PRI_SGP$logtfPRI[121:240])
coop_PRI_SGP_diff_19 <- diff(coop_PRI_SGP$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_SGP_diff_19)
#Thailand
#00-19
adf.test(coop_PRI_THA$logtfPRI)
coop_PRI_THA_diff <- diff(coop_PRI_THA$logtfPRI, differences = 1)
adf.test(coop_PRI_THA_diff)
#00-09
adf.test(coop_PRI_THA$logtfPRI[1:120])
coop_PRI_THA_diff_09 <- diff(coop_PRI_THA$logtfPRI[1:120], differences = 1)
adf.test(coop_PRI_THA_diff_09)
#10-19
adf.test(coop_PRI_THA$logtfPRI[121:240])
coop_PRI_THA_diff_19 <- diff(coop_PRI_THA$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_THA_diff_19)
#Vietnam
#08-19
adf.test(coop_PRI_VNM$logtfPRI[97:240])
coop_PRI_VNM_diff <- diff(coop_PRI_VNM$logtfPRI[97:240], differences = 1)
adf.test(coop_PRI_VNM_diff)
#08-09
adf.test(coop_PRI_VNM$logtfPRI[97:120])
coop_PRI_VNM_diff_09 <- diff(coop_PRI_VNM$logtfPRI[97:120], differences = 1)
adf.test(coop_PRI_VNM_diff_09)
#10-19
adf.test(coop_PRI_VNM$logtfPRI[121:240])
coop_PRI_VNM_diff_19 <- diff(coop_PRI_VNM$logtfPRI[121:240], differences = 1)
adf.test(coop_PRI_VNM_diff_19)
#monthly trade-filtered aggregate conflict PRIs
#Indonesia
#00-19
adf.test(conflict_PRI_IDN$logtfPRI)
conflict_PRI_IDN_diff <- diff(conflict_PRI_IDN$logtfPRI, differences = 1)
adf.test(conflict_PRI_IDN_diff)
#00-09
adf.test(conflict_PRI_IDN$logtfPRI[1:120])
conflict_PRI_IDN_diff_09 <- diff(conflict_PRI_IDN$logtfPRI[1:120], differences = 1)
adf.test(conflict_PRI_IDN_diff_09)
#10-19
adf.test(conflict_PRI_IDN$logtfPRI[121:240])
conflict_PRI_IDN_diff_19 <- diff(conflict_PRI_IDN$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_IDN_diff_19)
#Malaysia
#00-19
adf.test(conflict_PRI_MYS$logtfPRI)
conflict_PRI_MYS_diff <- diff(conflict_PRI_MYS$logtfPRI, differences = 1)
adf.test(conflict_PRI_MYS_diff)
#00-09
adf.test(conflict_PRI_MYS$logtfPRI[1:120])
conflict_PRI_MYS_diff_09 <- diff(conflict_PRI_MYS$logtfPRI[1:120], differences = 1)
adf.test(conflict_PRI_MYS_diff_09)
#10-19
adf.test(conflict_PRI_MYS$logtfPRI[121:240])
conflict_PRI_MYS_diff_19 <- diff(conflict_PRI_MYS$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_MYS_diff_19)
#Philippines
#00-19
adf.test(conflict_PRI_PHL$logtfPRI)
conflict_PRI_PHL_diff <- diff(conflict_PRI_PHL$logtfPRI, differences = 1)
adf.test(conflict_PRI_PHL_diff)
#00-09
adf.test(conflict_PRI_PHL$logtfPRI[1:120])
conflict_PRI_PHL_diff_09 <- diff(conflict_PRI_PHL$logtfPRI[1:120], differences = 1)
adf.test(conflict_PRI_PHL_diff_09)
#10-19
adf.test(conflict_PRI_PHL$logtfPRI[121:240])
conflict_PRI_PHL_diff_19 <- diff(conflict_PRI_PHL$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_PHL_diff_19)
#Singapore
#00-19
adf.test(conflict_PRI_SGP$logtfPRI)
conflict_PRI_SGP_diff <- diff(conflict_PRI_SGP$logtfPRI, differences = 1)
adf.test(conflict_PRI_SGP_diff)
#00-09
adf.test(conflict_PRI_SGP$logtfPRI[1:120])
conflict_PRI_SGP_diff_09 <- diff(conflict_PRI_SGP$logtfPRI[1:120], differences = 1)
adf.test(conflict_PRI_SGP_diff_09)
#10-19
adf.test(conflict_PRI_SGP$logtfPRI[121:240])
conflict_PRI_SGP_diff_19 <- diff(conflict_PRI_SGP$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_SGP_diff_19)
#Thailand
#00-19
adf.test(conflict_PRI_THA$logtfPRI)
conflict_PRI_THA_diff <- diff(conflict_PRI_THA$logtfPRI, differences = 1)
adf.test(conflict_PRI_THA_diff)
#00-09
adf.test(conflict_PRI_THA$logtfPRI[1:120])
conflict_PRI_THA_diff_09 <- diff(conflict_PRI_THA$logtfPRI[1:120], differences = 1)
adf.test(conflict_PRI_THA_diff_09)
#10-19
adf.test(conflict_PRI_THA$logtfPRI[121:240])
conflict_PRI_THA_diff_19 <- diff(conflict_PRI_THA$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_THA_diff_19)
#Vietnam
#08-19
adf.test(conflict_PRI_VNM$logtfPRI[97:240])
conflict_PRI_VNM_diff <- diff(conflict_PRI_VNM$logtfPRI[97:240], differences = 1)
adf.test(conflict_PRI_VNM_diff)
#08-09
adf.test(conflict_PRI_VNM$logtfPRI[97:120])
conflict_PRI_VNM_diff_09 <- diff(conflict_PRI_VNM$logtfPRI[97:120], differences = 1)
adf.test(conflict_PRI_VNM_diff_09)
#10-19
adf.test(conflict_PRI_VNM$logtfPRI[121:240])
conflict_PRI_VNM_diff_19 <- diff(conflict_PRI_VNM$logtfPRI[121:240], differences = 1)
adf.test(conflict_PRI_VNM_diff_19)
#monthly logged real effective exchange rate series
#Indonesia
#00-19
adf.test(reer_ALL$log_reer[1:240])
reer_IDN_diff <- diff(reer_ALL$log_reer[1:240], differences = 1)
adf.test(reer_IDN_diff)
#00-09
adf.test(reer_ALL$log_reer[1:120])
reer_IDN_diff_09 <- diff(reer_ALL$log_reer[1:120], differences = 1)
adf.test(reer_IDN_diff_09)
#10-19
adf.test(reer_ALL$log_reer[121:240])
reer_IDN_diff_19 <- diff(reer_ALL$log_reer[121:240], differences = 1)
adf.test(reer_IDN_diff_19)
#Malaysia
#00-19
adf.test(reer_ALL$log_reer[241:480])
reer_MYS_diff <- diff(reer_ALL$log_reer[241:480], differences = 1)
adf.test(reer_MYS_diff)
#00-09
adf.test(reer_ALL$log_reer[241:360])
reer_MYS_diff_09 <- diff(reer_ALL$log_reer[241:360], differences = 1)
adf.test(reer_MYS_diff_09)
#10-19
adf.test(reer_ALL$log_reer[361:480])
reer_MYS_diff_19 <- diff(reer_ALL$log_reer[361:480], differences = 1)
adf.test(reer_MYS_diff_19)
#Philippines
#00-19
adf.test(reer_ALL$log_reer[481:720])
reer_PHL_diff <- diff(reer_ALL$log_reer[481:720], differences = 1)
adf.test(reer_PHL_diff)
#00-09
adf.test(reer_ALL$log_reer[481:600])
reer_PHL_diff_09 <- diff(reer_ALL$log_reer[481:600], differences = 1)
adf.test(reer_PHL_diff_09)
#10-19
adf.test(reer_ALL$log_reer[601:720])
reer_PHL_diff_19 <- diff(reer_ALL$log_reer[601:720], differences = 1)
adf.test(reer_PHL_diff_19)
#Singapore
#00-19
adf.test(reer_ALL$log_reer[721:960])
reer_SGP_diff <- diff(reer_ALL$log_reer[721:960], differences = 1)
adf.test(reer_SGP_diff)
#00-09
adf.test(reer_ALL$log_reer[721:840])
reer_SGP_diff_09 <- diff(reer_ALL$log_reer[721:840], differences = 1)
adf.test(reer_SGP_diff_09)
#10-19
adf.test(reer_ALL$log_reer[841:960])
reer_SGP_diff_19 <- diff(reer_ALL$log_reer[841:960], differences = 1)
adf.test(reer_SGP_diff_19)
#Thailand
#00-19
adf.test(reer_ALL$log_reer[961:1200])
reer_THA_diff <- diff(reer_ALL$log_reer[961:1200], differences = 1)
adf.test(reer_THA_diff)
#00-09
adf.test(reer_ALL$log_reer[961:1080])
reer_THA_diff_09 <- diff(reer_ALL$log_reer[961:1080], differences = 1)
adf.test(reer_THA_diff_09)
#10-19
adf.test(reer_ALL$log_reer[1081:1200])
reer_THA_diff_19 <- diff(reer_ALL$log_reer[1081:1200], differences = 1)
adf.test(reer_THA_diff_19)
#Vietnam
#08-19
adf.test(reer_ALL$log_reer[1201:1344])
reer_VNM_diff <- diff(reer_ALL$log_reer[1201:1344], differences = 1)
adf.test(reer_VNM_diff)
#08-09
adf.test(reer_ALL$log_reer[1201:1224])
reer_VNM_diff_09 <- diff(reer_ALL$log_reer[1201:1224], differences = 1)
adf.test(reer_VNM_diff_09)
#10-19
adf.test(reer_ALL$log_reer[1225:1344])
reer_VNM_diff_19 <- diff(reer_ALL$log_reer[1225:1344], differences = 1)
adf.test(reer_VNM_diff_19)
#monthly logged industrial production index—China
#00-19
adf.test(industrial_production_ALL$log_IIP[1345:1584])
ip_CHN_diff <- diff(industrial_production_ALL$log_IIP[1345:1584], differences = 1)
adf.test(ip_CHN_diff)
#08-19
adf.test(industrial_production_ALL$log_IIP[1441:1584])
ip_CHN_diff_0819 <- diff(industrial_production_ALL$log_IIP[1441:1584], differences = 1)
adf.test(ip_CHN_diff_0819)
#00-09
adf.test(industrial_production_ALL$log_IIP[1345:1464])
ip_CHN_diff_09 <- diff(industrial_production_ALL$log_IIP[1345:1464], differences = 1)
adf.test(ip_CHN_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[1465:1584])
ip_CHN_diff_19 <- diff(industrial_production_ALL$log_IIP[1465:1584], differences = 1)
adf.test(ip_CHN_diff_19)
#monthly logged industrial production index—Southeast Asia
#Indonesia
#00-19
adf.test(industrial_production_ALL$log_IIP[1:240])
ip_IDN_diff <- diff(industrial_production_ALL$log_IIP[1:240], differences = 1)
adf.test(ip_IDN_diff)
#00-09
adf.test(industrial_production_ALL$log_IIP[1:120])
ip_IDN_diff_09 <- diff(industrial_production_ALL$log_IIP[1:120], differences = 1)
adf.test(ip_IDN_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[121:240])
ip_IDN_diff_19 <- diff(industrial_production_ALL$log_IIP[121:240], differences = 1)
adf.test(ip_IDN_diff_19)
#Malaysia
#00-19
adf.test(industrial_production_ALL$log_IIP[241:480])
ip_MYS_diff <- diff(industrial_production_ALL$log_IIP[241:480], differences = 1)
adf.test(ip_MYS_diff)
#00-09
adf.test(industrial_production_ALL$log_IIP[241:360])
ip_MYS_diff_09 <- diff(industrial_production_ALL$log_IIP[241:360], differences = 1)
adf.test(ip_MYS_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[361:480])
ip_MYS_diff_19 <- diff(industrial_production_ALL$log_IIP[361:480], differences = 1)
adf.test(ip_MYS_diff_19)
#Philippines
#00-19
adf.test(industrial_production_ALL$log_IIP[481:720])
ip_PHL_diff <- diff(industrial_production_ALL$log_IIP[481:720], differences = 1)
adf.test(ip_PHL_diff)
#00-09
adf.test(industrial_production_ALL$log_IIP[481:600])
ip_PHL_diff_09 <- diff(industrial_production_ALL$log_IIP[481:600], differences = 1)
adf.test(ip_PHL_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[601:720])
ip_PHL_diff_19 <- diff(industrial_production_ALL$log_IIP[601:720], differences = 1)
adf.test(ip_PHL_diff_19)
#Singapore
#00-19
adf.test(industrial_production_ALL$log_IIP[721:960])
ip_SGP_diff <- diff(industrial_production_ALL$log_IIP[721:960], differences = 1)
adf.test(ip_SGP_diff)
#00-09
adf.test(industrial_production_ALL$log_IIP[721:840])
ip_SGP_diff_09 <- diff(industrial_production_ALL$log_IIP[721:840], differences = 1)
adf.test(ip_SGP_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[841:960])
ip_SGP_diff_19 <- diff(industrial_production_ALL$log_IIP[841:960], differences = 1)
adf.test(ip_SGP_diff_19)
#Thailand
#00-19
adf.test(industrial_production_ALL$log_IIP[961:1200])
ip_THA_diff <- diff(industrial_production_ALL$log_IIP[961:1200], differences = 1)
adf.test(ip_THA_diff)
#00-09
adf.test(industrial_production_ALL$log_IIP[961:1080])
ip_THA_diff_09 <- diff(industrial_production_ALL$log_IIP[961:1080], differences = 1)
adf.test(ip_THA_diff_09)
#10-19
adf.test(industrial_production_ALL$log_IIP[1081:1200])
ip_THA_diff_19 <- diff(industrial_production_ALL$log_IIP[1081:1200], differences = 1)
adf.test(ip_THA_diff_19)
#Vietnam
#08-19
adf.test(industrial_production_ALL$log_IIP[1201:1344])
ip_VNM_diff <- diff(industrial_production_ALL$log_IIP[1201:1344])
adf.test(ip_VNM_diff)
#10-19
adf.test(industrial_production_ALL$log_IIP[1225:1344])
ip_VNM_diff_19 <- diff(industrial_production_ALL$log_IIP[1225:1344])
adf.test(ip_VNM_diff_19)
#monthly logged and seasonally adjusted exports series
#Indonesia
#00-19
adf.test(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen)
adj_exports_IDN_df_diff <- diff(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, differences = 1)
adf.test(adj_exports_IDN_df_diff)
#00-09
adf.test(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120])
adj_exports_IDN_df_diff_09 <- diff(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], differences = 1)
adf.test(adj_exports_IDN_df_diff_09)
#10-19
adf.test(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240])
adj_exports_IDN_df_diff_19 <- diff(adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], differences = 1)
adf.test(adj_exports_IDN_df_diff_19)
#Malaysia
#00-19
adf.test(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen)
adj_exports_MYS_df_diff <- diff(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, differences = 1)
adf.test(adj_exports_MYS_df_diff)
#00-09
adf.test(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120])
adj_exports_MYS_df_diff_09 <- diff(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120], differences = 1)
adf.test(adj_exports_MYS_df_diff_09)
#10-19
adf.test(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240])
adj_exports_MYS_df_diff_19 <- diff(adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], differences = 1)
adf.test(adj_exports_MYS_df_diff_19)
#Philippines
#00-19
adf.test(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen)
adj_exports_PHL_df_diff <- diff(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, differences = 1)
adf.test(adj_exports_PHL_df_diff)
#00-09
adf.test(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120])
adj_exports_PHL_df_diff_09 <- diff(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], differences = 1)
adf.test(adj_exports_PHL_df_diff_09)
#10-19
adf.test(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240])
adj_exports_PHL_df_diff_19 <- diff(adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], differences = 1)
adf.test(adj_exports_PHL_df_diff_19)
#Singapore
#00-19
adf.test(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen)
adj_exports_SGP_df_diff <- diff(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, differences = 1)
adf.test(adj_exports_SGP_df_diff)
#00-09
adf.test(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120])
adj_exports_SGP_df_diff_09 <- diff(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], differences = 1)
adf.test(adj_exports_SGP_df_diff_09)
#10-19
adf.test(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240])
adj_exports_SGP_df_diff_19 <- diff(adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], differences = 1)
adf.test(adj_exports_SGP_df_diff_19)
#Thailand
#00-19
adf.test(adj_exports_THA_df$adjusted_CHN_THA_ExpSen)
adj_exports_THA_df_diff <- diff(adj_exports_THA_df$adjusted_CHN_THA_ExpSen, differences = 1)
adf.test(adj_exports_THA_df_diff)
#00-09
adf.test(adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120])
adj_exports_THA_df_diff_09 <- diff(adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], differences = 1)
adf.test(adj_exports_THA_df_diff_09)
#10-19
adf.test(adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240])
adj_exports_THA_df_diff_19 <- diff(adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], differences = 1)
adf.test(adj_exports_THA_df_diff_19)
#Vietnam
#08-19
adf.test(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen)
adj_exports_VNM_df_diff <- diff(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, differences = 1)
adf.test(adj_exports_VNM_df_diff)
#08-09
adf.test(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[1:24])
adj_exports_VNM_df_diff_09 <- diff(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[1:24], differences = 1)
adf.test(adj_exports_VNM_df_diff_09)
#10-19
adf.test(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[25:144])
adj_exports_VNM_df_diff_19 <- diff(adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[25:144], differences = 1)
adf.test(adj_exports_VNM_df_diff_19)

#results
#Table 4 (netcoop)
sink(file = "Table 4_netcoop_IDN.txt")
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfpri),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run
irf_IDN_VAR_diff <- irf(IDN_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VAR_diff_cum <- irf(IDN_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Malaysia
sink(file = "Table 4_netcoop_MYS.txt")
#00-19
MYS_VAR <- cbind.data.frame(PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480])
colnames(MYS_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff <- cbind.data.frame(diff(MYS_VAR$tfpri),diff(MYS_VAR$reer),diff(MYS_VAR$exports),diff(MYS_VAR$ipc),diff(MYS_VAR$ipsea))
colnames(MYS_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff <- na.omit(MYS_VAR_diff)
VARselect(MYS_VAR_diff)
MYS_VAR_diff <- VAR(MYS_VAR_diff,p = 1, type = "const")
summary(MYS_VAR_diff)
#short-run
irf_MYS_VAR_diff <- irf(MYS_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_MYS <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS
#long-run
irf_MYS_VAR_diff_cum <- irf(MYS_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum) + title(main = "Malaysia")
lower_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR <- rbind.data.frame(lower_MYS_LR*100,irf_MYS_LR*100,upper_MYS_LR*100)
effects_MYS_LR
sink(file = NULL)

#Philippines
sink(file = "Table 4_netcoop_PHL.txt")
#00-19
PHL_VAR <- cbind.data.frame(PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfpri),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run
irf_PHL_VAR_diff <- irf(PHL_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VAR_diff_cum <- irf(PHL_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 4_netcoop_SGP.txt")
#00-19
SGP_VAR <- cbind.data.frame(PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfpri),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run
irf_SGP_VAR_diff <- irf(SGP_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VAR_diff_cum <- irf(SGP_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 4_netcoop_THA.txt")
#00-19
THA_VAR <- cbind.data.frame(PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfpri),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run
irf_THA_VAR_diff <- irf(THA_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VAR_diff_cum <- irf(THA_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 4_netcoop_VNM.txt")
#08-19
VNM_VAR <- cbind.data.frame(PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfpri),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run
irf_VNM_VAR_diff <- irf(VNM_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Lower"]][["diff_tfpri"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["irf"]][["diff_tfpri"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Upper"]][["diff_tfpri"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VAR_diff_cum <- irf(VNM_VAR_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#Figure 4 (netcoop)
#00-19 (08-19 for Vietnam)
IDN_net_plot_SR <- plot(irf_IDN_VAR_diff, main="Indonesia", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)
MYS_net_plot_SR <- plot(irf_MYS_VAR_diff, main="Malaysia", xlab="", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)
PHL_net_plot_SR <- plot(irf_PHL_VAR_diff, main="Philippines", xlab="", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)
SGP_net_plot_SR <- plot(irf_SGP_VAR_diff, main="Singapore", xlab="", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)
THA_net_plot_SR <- plot(irf_THA_VAR_diff, main="Thailand", xlab="", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)
VNM_net_plot_SR <- plot(irf_VNM_VAR_diff, main="Vietnam", xlab="", ylab="", sub="months post political shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.2, cex = 2.5)

#Table 4 (aggcoop)
sink(file = "Table 4_aggcoop_IDN.txt")
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(coop_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfPRI),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run
irf_IDN_VAR_diff <- irf(IDN_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR <- plot(irf_IDN_VAR_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VAR_diff_cum <- irf(IDN_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Malaysia
sink(file = "Table 4_aggcoop_MYS.txt")
#00-19
MYS_VAR <- cbind.data.frame(coop_PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480])
colnames(MYS_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
MYS_VAR_diff <- cbind.data.frame(diff(MYS_VAR$tfPRI),diff(MYS_VAR$reer),diff(MYS_VAR$exports),diff(MYS_VAR$ipc),diff(MYS_VAR$ipsea))
colnames(MYS_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff <- na.omit(MYS_VAR_diff)
VARselect(MYS_VAR_diff)
MYS_VAR_diff <- VAR(MYS_VAR_diff,p = 1, type = "const")
summary(MYS_VAR_diff)
#short-run
irf_MYS_VAR_diff <- irf(MYS_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR <- plot(irf_MYS_VAR_diff) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_MYS <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS
#long-run
irf_MYS_VAR_diff_cum <- irf(MYS_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum) + title(main = "Malaysia")
lower_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_MYS_LR <- as.data.frame(t(irf_MYS_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_MYS_LR <- rbind.data.frame(lower_MYS_LR*100,irf_MYS_LR*100,upper_MYS_LR*100)
effects_MYS_LR
sink(file = NULL)

#Philippines
sink(file = "Table 4_aggcoop_PHL.txt")
#00-19
PHL_VAR <- cbind.data.frame(coop_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfPRI),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run
irf_PHL_VAR_diff <- irf(PHL_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR <- plot(irf_PHL_VAR_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VAR_diff_cum <- irf(PHL_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 4_aggcoop_SGP.txt")
#00-19
SGP_VAR <- cbind.data.frame(coop_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfPRI),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run
irf_SGP_VAR_diff <- irf(SGP_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR <- plot(irf_SGP_VAR_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VAR_diff_cum <- irf(SGP_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 4_aggcoop_THA.txt")
#00-19
THA_VAR <- cbind.data.frame(coop_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfPRI),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run
irf_THA_VAR_diff <- irf(THA_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR <- plot(irf_THA_VAR_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VAR_diff_cum <- irf(THA_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 4_aggcoop_VNM.txt")
#08-19
VNM_VAR <- cbind.data.frame(coop_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfPRI),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run
irf_VNM_VAR_diff <- irf(VNM_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
VNM_coop_plot_SR <- plot(irf_VNM_VAR_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VAR_diff_cum <- irf(VNM_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#Table 4 (aggconf)
sink(file = "Table 4_aggconf_IDN.txt")
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(conflict_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfPRI),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run
irf_IDN_VAR_diff <- irf(IDN_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VAR_diff_cum <- irf(IDN_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Philippines
sink(file = "Table 4_aggconf_PHL.txt")
#00-19
PHL_VAR <- cbind.data.frame(conflict_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfPRI),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run
irf_PHL_VAR_diff <- irf(PHL_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VAR_diff_cum <- irf(PHL_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 4_aggconf_SGP.txt")
#00-19
SGP_VAR <- cbind.data.frame(conflict_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfPRI),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run
irf_SGP_VAR_diff <- irf(SGP_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VAR_diff_cum <- irf(SGP_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 4_aggconf_THA.txt")
#00-19
THA_VAR <- cbind.data.frame(conflict_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfPRI),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run
irf_THA_VAR_diff <- irf(THA_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VAR_diff_cum <- irf(THA_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 4_aggconf_VNM.txt")
#08-19
VNM_VAR <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfPRI","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfPRI),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run
irf_VNM_VAR_diff <- irf(VNM_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Lower"]][["diff_tfPRI"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["irf"]][["diff_tfPRI"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff[["Upper"]][["diff_tfPRI"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VAR_diff_cum <- irf(VNM_VAR_diff, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Lower"]][["diff_tfPRI"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["irf"]][["diff_tfPRI"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VAR_diff_cum[["Upper"]][["diff_tfPRI"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#Table 5 (netcoop)
sink(file = "Table 5_netcoop_IDN.txt")
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfpri),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run
irf_IDN_VAR_diff_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Lower"]][["diff_tfpri"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["irf"]][["diff_tfpri"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Upper"]][["diff_tfpri"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VAR_diff_cum_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Malaysia
sink(file = "Table 5_netcoop_MYS.txt")
#00-09
MYS_VAR_09 <- cbind.data.frame(PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360])
colnames(MYS_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_09 <- cbind.data.frame(diff(MYS_VAR_09$tfpri),diff(MYS_VAR_09$reer),diff(MYS_VAR_09$exports),diff(MYS_VAR_09$ipc),diff(MYS_VAR_09$ipsea))
colnames(MYS_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_09 <- na.omit(MYS_VAR_diff_09)
VARselect(MYS_VAR_diff_09)
MYS_VAR_diff_09 <- VAR(MYS_VAR_diff_09,p = 1, type = "const")
summary(MYS_VAR_diff_09)
#short-run
irf_MYS_VAR_diff_09 <- irf(MYS_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["Lower"]][["diff_tfpri"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["irf"]][["diff_tfpri"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["Upper"]][["diff_tfpri"]]))
effects_MYS_09 <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09
#long-run
irf_MYS_VAR_diff_cum_09 <- irf(MYS_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum_09) + title(main = "Malaysia")
lower_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_09 <- rbind.data.frame(lower_MYS_LR_09*100,irf_MYS_LR_09*100,upper_MYS_LR_09*100)
effects_MYS_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 5_netcoop_PHL.txt")
#00-09
PHL_VAR_09 <- cbind.data.frame(PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfpri),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run
irf_PHL_VAR_diff_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Lower"]][["diff_tfpri"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["irf"]][["diff_tfpri"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Upper"]][["diff_tfpri"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VAR_diff_cum_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 5_netcoop_SGP.txt")
#00-09
SGP_VAR_09 <- cbind.data.frame(PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfpri),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run
irf_SGP_VAR_diff_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Lower"]][["diff_tfpri"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["irf"]][["diff_tfpri"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Upper"]][["diff_tfpri"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VAR_diff_cum_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 5_netcoop_THA.txt")
#00-09
THA_VAR_09 <- cbind.data.frame(PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfpri),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run
irf_THA_VAR_diff_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Lower"]][["diff_tfpri"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["irf"]][["diff_tfpri"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Upper"]][["diff_tfpri"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VAR_diff_cum_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#Table 5 (aggcoop)
sink(file = "Table 5_aggcoop_IDN.txt")
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfPRI),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run
irf_IDN_VAR_diff_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR_09 <- plot(irf_IDN_VAR_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VAR_diff_cum_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Malaysia
sink(file = "Table 5_aggcoop_MYS.txt")
#00-09
MYS_VAR_09 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360])
colnames(MYS_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
MYS_VAR_diff_09 <- cbind.data.frame(diff(MYS_VAR_09$tfPRI),diff(MYS_VAR_09$reer),diff(MYS_VAR_09$exports),diff(MYS_VAR_09$ipc),diff(MYS_VAR_09$ipsea))
colnames(MYS_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_09 <- na.omit(MYS_VAR_diff_09)
VARselect(MYS_VAR_diff_09)
MYS_VAR_diff_09 <- VAR(MYS_VAR_diff_09,p = 1, type = "const")
summary(MYS_VAR_diff_09)
#short-run
irf_MYS_VAR_diff_09 <- irf(MYS_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR_09 <- plot(irf_MYS_VAR_diff_09) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_MYS_09 <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09
#long-run
irf_MYS_VAR_diff_cum_09 <- irf(MYS_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum_09) + title(main = "Malaysia")
lower_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_MYS_LR_09 <- as.data.frame(t(irf_MYS_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_MYS_LR_09 <- rbind.data.frame(lower_MYS_LR_09*100,irf_MYS_LR_09*100,upper_MYS_LR_09*100)
effects_MYS_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 5_aggcoop_PHL.txt")
#00-09
PHL_VAR_09 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfPRI),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run
irf_PHL_VAR_diff_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR_09 <- plot(irf_PHL_VAR_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VAR_diff_cum_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 5_aggcoop_SGP.txt")
#00-09
SGP_VAR_09 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfPRI),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run
irf_SGP_VAR_diff_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR_09 <- plot(irf_SGP_VAR_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VAR_diff_cum_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 5_aggcoop_THA.txt")
#00-09
THA_VAR_09 <- cbind.data.frame(coop_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfPRI),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run
irf_THA_VAR_diff_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR_09 <- plot(irf_THA_VAR_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VAR_diff_cum_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#Table 5 (aggconf)
sink(file = "Table 5_aggconf_IDN.txt")
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfPRI),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run
irf_IDN_VAR_diff_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VAR_diff_cum_09 <- irf(IDN_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 5_aggconf_PHL.txt")
#00-09
PHL_VAR_09 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfPRI),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run
irf_PHL_VAR_diff_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VAR_diff_cum_09 <- irf(PHL_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 5_aggconf_SGP.txt")
#00-09
SGP_VAR_09 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfPRI),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run
irf_SGP_VAR_diff_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VAR_diff_cum_09 <- irf(SGP_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 5_aggconf_THA.txt")
#00-09
THA_VAR_09 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfPRI),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run
irf_THA_VAR_diff_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Lower"]][["diff_tfPRI"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["irf"]][["diff_tfPRI"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09[["Upper"]][["diff_tfPRI"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VAR_diff_cum_09 <- irf(THA_VAR_diff_09, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VAR_diff_cum_09[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#Table 6 (netcoop)
sink(file = "Table 6_netcoop_IDN.txt")
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfpri),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run
irf_IDN_VAR_diff_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VAR_diff_cum_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 6_netcoop_MYS.txt")
#10-19
MYS_VAR_19 <- cbind.data.frame(PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfpri),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run
irf_MYS_VAR_diff_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VAR_diff_cum_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 6_netcoop_PHL.txt")
#10-19
PHL_VAR_19 <- cbind.data.frame(PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfpri),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run
irf_PHL_VAR_diff_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VAR_diff_cum_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 6_netcoop_SGP.txt")
#10-19
SGP_VAR_19 <- cbind.data.frame(PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfpri),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run
irf_SGP_VAR_diff_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VAR_diff_cum_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 6_netcoop_THA.txt")
#10-19
THA_VAR_19 <- cbind.data.frame(PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfpri),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run
irf_THA_VAR_diff_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VAR_diff_cum_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 6_netcoop_VNM.txt")
#10-19
VNM_VAR_19 <- cbind.data.frame(PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfpri),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run
irf_VNM_VAR_diff_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Lower"]][["diff_tfpri"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["irf"]][["diff_tfpri"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Upper"]][["diff_tfpri"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VAR_diff_cum_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#Table 6 (aggcoop)
sink(file = "Table 6_aggcoop_IDN.txt")
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfPRI),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run
irf_IDN_VAR_diff_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR_19 <- plot(irf_IDN_VAR_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VAR_diff_cum_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 6_aggcoop_MYS.txt")
#10-19
MYS_VAR_19 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfPRI),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run
irf_MYS_VAR_diff_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR_19 <- plot(irf_MYS_VAR_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VAR_diff_cum_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 6_aggcoop_PHL.txt")
#10-19
PHL_VAR_19 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfPRI),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run
irf_PHL_VAR_diff_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR_19 <- plot(irf_PHL_VAR_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VAR_diff_cum_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 6_aggcoop_SGP.txt")
#10-19
SGP_VAR_19 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfPRI),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run
irf_SGP_VAR_diff_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR_19 <- plot(irf_SGP_VAR_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VAR_diff_cum_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 6_aggcoop_THA.txt")
#10-19
THA_VAR_19 <- cbind.data.frame(coop_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfPRI),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run
irf_THA_VAR_diff_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR_19 <- plot(irf_THA_VAR_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VAR_diff_cum_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 6_aggcoop_VNM.txt")
#10-19
VNM_VAR_19 <- cbind.data.frame(coop_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfPRI),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run
irf_VNM_VAR_diff_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
VNM_coop_plot_SR_19 <- plot(irf_VNM_VAR_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VAR_diff_cum_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#Table 6 (aggregate conflict, 2010-2019)
sink(file = "Table 6_aggconf_IDN.txt")
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfPRI),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run
irf_IDN_VAR_diff_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VAR_diff_cum_19 <- irf(IDN_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VAR_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 6_aggconf_MYS.txt")
###10-19
MYS_VAR_19 <- cbind.data.frame(conflict_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfPRI),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run
irf_MYS_VAR_diff_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VAR_diff_cum_19 <- irf(MYS_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VAR_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 6_aggconf_PHL.txt")
#10-19
PHL_VAR_19 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfPRI),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run
irf_PHL_VAR_diff_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VAR_diff_cum_19 <- irf(PHL_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VAR_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 6_aggconf_SGP.txt")
#10-19
SGP_VAR_19 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfPRI),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run
irf_SGP_VAR_diff_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VAR_diff_cum_19 <- irf(SGP_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VAR_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 6_aggconf_THA.txt")
#10-19
THA_VAR_19 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfPRI),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run
irf_THA_VAR_diff_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VAR_diff_cum_19 <- irf(THA_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VAR_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 6_aggconf_VNM.txt")
#10-19
VNM_VAR_19 <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfPRI","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfPRI),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfPRI","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run
irf_VNM_VAR_diff_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Lower"]][["diff_tfPRI"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["irf"]][["diff_tfPRI"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19[["Upper"]][["diff_tfPRI"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VAR_diff_cum_19 <- irf(VNM_VAR_diff_19, impulse = "diff_tfPRI", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VAR_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Lower"]][["diff_tfPRI"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["irf"]][["diff_tfPRI"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VAR_diff_cum_19[["Upper"]][["diff_tfPRI"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#robustness tests: reordering VAR model system variables
#Table 7
#using net cooperation PRIs, 2000-2019
sink(file = "Table 7_netcoop_IDN.txt")
#Indonesia
#00-19
IDN_VARrobustaltspec <- cbind.data.frame(PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen)
colnames(IDN_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff <- cbind.data.frame(diff(IDN_VARrobustaltspec$tfpri),diff(IDN_VARrobustaltspec$reer),diff(IDN_VARrobustaltspec$ipc),diff(IDN_VARrobustaltspec$ipsea),diff(IDN_VARrobustaltspec$exports))
colnames(IDN_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff <- na.omit(IDN_VARrobustaltspec_diff)
VARselect(IDN_VARrobustaltspec_diff)
IDN_VARrobustaltspec_diff <- VAR(IDN_VARrobustaltspec_diff,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff)
#short-run
irf_IDN_VARrobustaltspec_diff <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VARrobustaltspec_diff_cum <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Malaysia
sink(file = "Table 7_netcoop_MYS.txt")
#00-19
MYS_VARrobustaltspec <- cbind.data.frame(PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen)
colnames(MYS_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff <- cbind.data.frame(diff(MYS_VARrobustaltspec$tfpri),diff(MYS_VARrobustaltspec$reer),diff(MYS_VARrobustaltspec$ipc),diff(MYS_VARrobustaltspec$ipsea),diff(MYS_VARrobustaltspec$exports))
colnames(MYS_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff <- na.omit(MYS_VARrobustaltspec_diff)
VARselect(MYS_VARrobustaltspec_diff)
MYS_VARrobustaltspec_diff <- VAR(MYS_VARrobustaltspec_diff,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff)
#short-run
irf_MYS_VARrobustaltspec_diff <- irf(MYS_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VARrobustaltspec_diff) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_MYS <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS
#long-run
irf_MYS_VARrobustaltspec_diff_cum <- irf(MYS_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum) + title(main = "Malaysia")
lower_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR <- rbind.data.frame(lower_MYS_LR*100,irf_MYS_LR*100,upper_MYS_LR*100)
effects_MYS_LR
sink(file = NULL)

#Philippines
sink(file = "Table 7_netcoop_PHL.txt")
#00-19
PHL_VARrobustaltspec <- cbind.data.frame(PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen)
colnames(PHL_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff <- cbind.data.frame(diff(PHL_VARrobustaltspec$tfpri),diff(PHL_VARrobustaltspec$reer),diff(PHL_VARrobustaltspec$ipc),diff(PHL_VARrobustaltspec$ipsea),diff(PHL_VARrobustaltspec$exports))
colnames(PHL_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff <- na.omit(PHL_VARrobustaltspec_diff)
VARselect(PHL_VARrobustaltspec_diff)
PHL_VARrobustaltspec_diff <- VAR(PHL_VARrobustaltspec_diff,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff)
#short-run
irf_PHL_VARrobustaltspec_diff <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VARrobustaltspec_diff_cum <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 7_netcoop_SGP.txt")
#00-19
SGP_VARrobustaltspec <- cbind.data.frame(PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen)
colnames(SGP_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff <- cbind.data.frame(diff(SGP_VARrobustaltspec$tfpri),diff(SGP_VARrobustaltspec$reer),diff(SGP_VARrobustaltspec$ipc),diff(SGP_VARrobustaltspec$ipsea),diff(SGP_VARrobustaltspec$exports))
colnames(SGP_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff <- na.omit(SGP_VARrobustaltspec_diff)
VARselect(SGP_VARrobustaltspec_diff)
SGP_VARrobustaltspec_diff <- VAR(SGP_VARrobustaltspec_diff,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff)
#short-run
irf_SGP_VARrobustaltspec_diff <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VARrobustaltspec_diff_cum <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 7_netcoop_THA.txt")
#00-19
THA_VARrobustaltspec <- cbind.data.frame(PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen)
colnames(THA_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff <- cbind.data.frame(diff(THA_VARrobustaltspec$tfpri),diff(THA_VARrobustaltspec$reer),diff(THA_VARrobustaltspec$ipc),diff(THA_VARrobustaltspec$ipsea),diff(THA_VARrobustaltspec$exports))
colnames(THA_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff <- na.omit(THA_VARrobustaltspec_diff)
VARselect(THA_VARrobustaltspec_diff)
THA_VARrobustaltspec_diff <- VAR(THA_VARrobustaltspec_diff,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff)
#short-run
irf_THA_VARrobustaltspec_diff <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VARrobustaltspec_diff_cum <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 7_netcoop_VNM.txt")
#08-19
VNM_VARrobustaltspec <- cbind.data.frame(PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen)
colnames(VNM_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff <- cbind.data.frame(diff(VNM_VARrobustaltspec$tfpri),diff(VNM_VARrobustaltspec$reer),diff(VNM_VARrobustaltspec$ipc),diff(VNM_VARrobustaltspec$ipsea),diff(VNM_VARrobustaltspec$exports))
colnames(VNM_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff <- na.omit(VNM_VARrobustaltspec_diff)
VARselect(VNM_VARrobustaltspec_diff)
VNM_VARrobustaltspec_diff <- VAR(VNM_VARrobustaltspec_diff,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff)
#short-run
irf_VNM_VARrobustaltspec_diff <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VARrobustaltspec_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VARrobustaltspec_diff_cum <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#using aggregate cooperation PRIs, 2000-2019
#Indonesia
sink(file = "Table 7_aggcoop_IDN.txt")
#00-19
IDN_VARrobustaltspec <- cbind.data.frame(coop_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen)
colnames(IDN_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff <- cbind.data.frame(diff(IDN_VARrobustaltspec$tfpri),diff(IDN_VARrobustaltspec$reer),diff(IDN_VARrobustaltspec$ipc),diff(IDN_VARrobustaltspec$ipsea),diff(IDN_VARrobustaltspec$exports))
colnames(IDN_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff <- na.omit(IDN_VARrobustaltspec_diff)
VARselect(IDN_VARrobustaltspec_diff)
IDN_VARrobustaltspec_diff <- VAR(IDN_VARrobustaltspec_diff,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff)
#short-run
irf_IDN_VARrobustaltspec_diff <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR <- plot(irf_IDN_VARrobustaltspec_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VARrobustaltspec_diff_cum <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Malaysia
sink(file = "Table 7_aggcoop_MYS.txt")
#00-19
MYS_VARrobustaltspec <- cbind.data.frame(coop_PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen)
colnames(MYS_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff <- cbind.data.frame(diff(MYS_VARrobustaltspec$tfpri),diff(MYS_VARrobustaltspec$reer),diff(MYS_VARrobustaltspec$ipc),diff(MYS_VARrobustaltspec$ipsea),diff(MYS_VARrobustaltspec$exports))
colnames(MYS_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff <- na.omit(MYS_VARrobustaltspec_diff)
VARselect(MYS_VARrobustaltspec_diff)
MYS_VARrobustaltspec_diff <- VAR(MYS_VARrobustaltspec_diff,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff)
#short-run
irf_MYS_VARrobustaltspec_diff <- irf(MYS_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR <- plot(irf_MYS_VARrobustaltspec_diff) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_MYS <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS
#long-run
irf_MYS_VARrobustaltspec_diff_cum <- irf(MYS_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum) + title(main = "Malaysia")
lower_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR <- rbind.data.frame(lower_MYS_LR*100,irf_MYS_LR*100,upper_MYS_LR*100)
effects_MYS_LR
sink(file = NULL)

#Philippines
sink(file = "Table 7_aggcoop_PHL.txt")
#00-19
PHL_VARrobustaltspec <- cbind.data.frame(coop_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen)
colnames(PHL_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff <- cbind.data.frame(diff(PHL_VARrobustaltspec$tfpri),diff(PHL_VARrobustaltspec$reer),diff(PHL_VARrobustaltspec$ipc),diff(PHL_VARrobustaltspec$ipsea),diff(PHL_VARrobustaltspec$exports))
colnames(PHL_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff <- na.omit(PHL_VARrobustaltspec_diff)
VARselect(PHL_VARrobustaltspec_diff)
PHL_VARrobustaltspec_diff <- VAR(PHL_VARrobustaltspec_diff,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff)
#short-run
irf_PHL_VARrobustaltspec_diff <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR <- plot(irf_PHL_VARrobustaltspec_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VARrobustaltspec_diff_cum <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 7_aggcoop_SGP.txt")
#00-19
SGP_VARrobustaltspec <- cbind.data.frame(coop_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen)
colnames(SGP_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff <- cbind.data.frame(diff(SGP_VARrobustaltspec$tfpri),diff(SGP_VARrobustaltspec$reer),diff(SGP_VARrobustaltspec$ipc),diff(SGP_VARrobustaltspec$ipsea),diff(SGP_VARrobustaltspec$exports))
colnames(SGP_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff <- na.omit(SGP_VARrobustaltspec_diff)
VARselect(SGP_VARrobustaltspec_diff)
SGP_VARrobustaltspec_diff <- VAR(SGP_VARrobustaltspec_diff,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff)
#short-run
irf_SGP_VARrobustaltspec_diff <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR <- plot(irf_SGP_VARrobustaltspec_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VARrobustaltspec_diff_cum <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 7_aggcoop_THA.txt")
#00-19
THA_VARrobustaltspec <- cbind.data.frame(coop_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen)
colnames(THA_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff <- cbind.data.frame(diff(THA_VARrobustaltspec$tfpri),diff(THA_VARrobustaltspec$reer),diff(THA_VARrobustaltspec$ipc),diff(THA_VARrobustaltspec$ipsea),diff(THA_VARrobustaltspec$exports))
colnames(THA_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff <- na.omit(THA_VARrobustaltspec_diff)
VARselect(THA_VARrobustaltspec_diff)
THA_VARrobustaltspec_diff <- VAR(THA_VARrobustaltspec_diff,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff)
#short-run
irf_THA_VARrobustaltspec_diff <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR <- plot(irf_THA_VARrobustaltspec_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VARrobustaltspec_diff_cum <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 7_aggcoop_VNM.txt")
#08-19
VNM_VARrobustaltspec <- cbind.data.frame(coop_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen)
colnames(VNM_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff <- cbind.data.frame(diff(VNM_VARrobustaltspec$tfpri),diff(VNM_VARrobustaltspec$reer),diff(VNM_VARrobustaltspec$ipc),diff(VNM_VARrobustaltspec$ipsea),diff(VNM_VARrobustaltspec$exports))
colnames(VNM_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff <- na.omit(VNM_VARrobustaltspec_diff)
VARselect(VNM_VARrobustaltspec_diff)
VNM_VARrobustaltspec_diff <- VAR(VNM_VARrobustaltspec_diff,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff)
#short-run
irf_VNM_VARrobustaltspec_diff <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
VNM_coop_plot_SR <- plot(irf_VNM_VARrobustaltspec_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VARrobustaltspec_diff_cum <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#using aggregate conflict PRIs, 2000-2019
#Indonesia
sink(file = "Table 7_aggconf_IDN.txt")
#00-19
IDN_VARrobustaltspec <- cbind.data.frame(conflict_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen)
colnames(IDN_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff <- cbind.data.frame(diff(IDN_VARrobustaltspec$tfpri),diff(IDN_VARrobustaltspec$reer),diff(IDN_VARrobustaltspec$ipc),diff(IDN_VARrobustaltspec$ipsea),diff(IDN_VARrobustaltspec$exports))
colnames(IDN_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff <- na.omit(IDN_VARrobustaltspec_diff)
VARselect(IDN_VARrobustaltspec_diff)
IDN_VARrobustaltspec_diff <- VAR(IDN_VARrobustaltspec_diff,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff)
#short-run
irf_IDN_VARrobustaltspec_diff <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_IDN <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN
#long-run
irf_IDN_VARrobustaltspec_diff_cum <- irf(IDN_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum) + title(main = "Indonesia")
lower_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR <- rbind.data.frame(lower_IDN_LR*100,irf_IDN_LR*100,upper_IDN_LR*100)
effects_IDN_LR
sink(file = NULL)

#Philippines
sink(file = "Table 7_aggconf_PHL.txt")
#00-19
PHL_VARrobustaltspec <- cbind.data.frame(conflict_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen)
colnames(PHL_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff <- cbind.data.frame(diff(PHL_VARrobustaltspec$tfpri),diff(PHL_VARrobustaltspec$reer),diff(PHL_VARrobustaltspec$ipc),diff(PHL_VARrobustaltspec$ipsea),diff(PHL_VARrobustaltspec$exports))
colnames(PHL_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff <- na.omit(PHL_VARrobustaltspec_diff)
VARselect(PHL_VARrobustaltspec_diff)
PHL_VARrobustaltspec_diff <- VAR(PHL_VARrobustaltspec_diff,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff)
#short-run
irf_PHL_VARrobustaltspec_diff <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_PHL <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL
#long-run
irf_PHL_VARrobustaltspec_diff_cum <- irf(PHL_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum) + title(main = "Philippines")
lower_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR <- rbind.data.frame(lower_PHL_LR*100,irf_PHL_LR*100,upper_PHL_LR*100)
effects_PHL_LR
sink(file = NULL)

#Singapore
sink(file = "Table 7_aggconf_SGP.txt")
#00-19
SGP_VARrobustaltspec <- cbind.data.frame(conflict_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen)
colnames(SGP_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff <- cbind.data.frame(diff(SGP_VARrobustaltspec$tfpri),diff(SGP_VARrobustaltspec$reer),diff(SGP_VARrobustaltspec$ipc),diff(SGP_VARrobustaltspec$ipsea),diff(SGP_VARrobustaltspec$exports))
colnames(SGP_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff <- na.omit(SGP_VARrobustaltspec_diff)
VARselect(SGP_VARrobustaltspec_diff)
SGP_VARrobustaltspec_diff <- VAR(SGP_VARrobustaltspec_diff,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff)
#short-run
irf_SGP_VARrobustaltspec_diff <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_SGP <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP
#long-run
irf_SGP_VARrobustaltspec_diff_cum <- irf(SGP_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum) + title(main = "Singapore")
lower_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR <- rbind.data.frame(lower_SGP_LR*100,irf_SGP_LR*100,upper_SGP_LR*100)
effects_SGP_LR
sink(file = NULL)

#Thailand
sink(file = "Table 7_aggconf_THA.txt")
#00-19
THA_VARrobustaltspec <- cbind.data.frame(conflict_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen)
colnames(THA_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff <- cbind.data.frame(diff(THA_VARrobustaltspec$tfpri),diff(THA_VARrobustaltspec$reer),diff(THA_VARrobustaltspec$ipc),diff(THA_VARrobustaltspec$ipsea),diff(THA_VARrobustaltspec$exports))
colnames(THA_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff <- na.omit(THA_VARrobustaltspec_diff)
VARselect(THA_VARrobustaltspec_diff)
THA_VARrobustaltspec_diff <- VAR(THA_VARrobustaltspec_diff,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff)
#short-run
irf_THA_VARrobustaltspec_diff <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_THA <- as.data.frame(t(irf_THA_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_THA <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA
#long-run
irf_THA_VARrobustaltspec_diff_cum <- irf(THA_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum) + title(main = "Thailand")
lower_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR <- rbind.data.frame(lower_THA_LR*100,irf_THA_LR*100,upper_THA_LR*100)
effects_THA_LR
sink(file = NULL)

#Vietnam
sink(file = "Table 7_aggconf_VNM.txt")
#08-19
VNM_VARrobustaltspec <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen)
colnames(VNM_VARrobustaltspec) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff <- cbind.data.frame(diff(VNM_VARrobustaltspec$tfpri),diff(VNM_VARrobustaltspec$reer),diff(VNM_VARrobustaltspec$ipc),diff(VNM_VARrobustaltspec$ipsea),diff(VNM_VARrobustaltspec$exports))
colnames(VNM_VARrobustaltspec_diff) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff <- na.omit(VNM_VARrobustaltspec_diff)
VARselect(VNM_VARrobustaltspec_diff)
VNM_VARrobustaltspec_diff <- VAR(VNM_VARrobustaltspec_diff,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff)
#short-run
irf_VNM_VARrobustaltspec_diff <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VARrobustaltspec_diff) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Lower"]][["diff_tfpri"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["irf"]][["diff_tfpri"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff[["Upper"]][["diff_tfpri"]]))
effects_VNM <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM
#long-run
irf_VNM_VARrobustaltspec_diff_cum <- irf(VNM_VARrobustaltspec_diff, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum) + title(main = "Vietnam")
lower_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR <- rbind.data.frame(lower_VNM_LR*100,irf_VNM_LR*100,upper_VNM_LR*100)
effects_VNM_LR
sink(file = NULL)

#Table 8
#using net cooperation PRIs, 2000-2009
sink(file = "Table 8_netcoop_IDN.txt")
#Indonesia
#00-09
IDN_VARrobustaltspec_09 <- cbind.data.frame(PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120],adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120])
colnames(IDN_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(IDN_VARrobustaltspec_09$tfpri),diff(IDN_VARrobustaltspec_09$reer),diff(IDN_VARrobustaltspec_09$ipc),diff(IDN_VARrobustaltspec_09$ipsea),diff(IDN_VARrobustaltspec_09$exports))
colnames(IDN_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_09 <- na.omit(IDN_VARrobustaltspec_diff_09)
VARselect(IDN_VARrobustaltspec_diff_09)
IDN_VARrobustaltspec_diff_09 <- VAR(IDN_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_09)
#short-run
irf_IDN_VARrobustaltspec_diff_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VARrobustaltspec_diff_cum_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Malaysia
sink(file = "Table 8_netcoop_MYS.txt")
#00-09
MYS_VARrobustaltspec_09 <- cbind.data.frame(PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120])
colnames(MYS_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(MYS_VARrobustaltspec_09$tfpri),diff(MYS_VARrobustaltspec_09$reer),diff(MYS_VARrobustaltspec_09$ipc),diff(MYS_VARrobustaltspec_09$ipsea),diff(MYS_VARrobustaltspec_09$exports))
colnames(MYS_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff_09 <- na.omit(MYS_VARrobustaltspec_diff_09)
VARselect(MYS_VARrobustaltspec_diff_09)
MYS_VARrobustaltspec_diff_09 <- VAR(MYS_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff_09)
#short-run
irf_MYS_VARrobustaltspec_diff_09 <- irf(MYS_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VARrobustaltspec_diff_09) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_MYS_09 <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09
#long-run
irf_MYS_VARrobustaltspec_diff_cum_09 <- irf(MYS_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum_09) + title(main = "Malaysia")
lower_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_09 <- rbind.data.frame(lower_MYS_LR_09*100,irf_MYS_LR_09*100,upper_MYS_LR_09*100)
effects_MYS_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 8_netcoop_PHL.txt")
#00-09
PHL_VARrobustaltspec_09 <- cbind.data.frame(PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120])
colnames(PHL_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(PHL_VARrobustaltspec_09$tfpri),diff(PHL_VARrobustaltspec_09$reer),diff(PHL_VARrobustaltspec_09$ipc),diff(PHL_VARrobustaltspec_09$ipsea),diff(PHL_VARrobustaltspec_09$exports))
colnames(PHL_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_09 <- na.omit(PHL_VARrobustaltspec_diff_09)
VARselect(PHL_VARrobustaltspec_diff_09)
PHL_VARrobustaltspec_diff_09 <- VAR(PHL_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_09)
#short-run
irf_PHL_VARrobustaltspec_diff_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VARrobustaltspec_diff_cum_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 8_netcoop_SGP.txt")
#00-09
SGP_VARrobustaltspec_09 <- cbind.data.frame(PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120])
colnames(SGP_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(SGP_VARrobustaltspec_09$tfpri),diff(SGP_VARrobustaltspec_09$reer),diff(SGP_VARrobustaltspec_09$ipc),diff(SGP_VARrobustaltspec_09$ipsea),diff(SGP_VARrobustaltspec_09$exports))
colnames(SGP_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_09 <- na.omit(SGP_VARrobustaltspec_diff_09)
VARselect(SGP_VARrobustaltspec_diff_09)
SGP_VARrobustaltspec_diff_09 <- VAR(SGP_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_09)
#short-run
irf_SGP_VARrobustaltspec_diff_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VARrobustaltspec_diff_cum_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 8_netcoop_THA.txt")
#00-09
THA_VARrobustaltspec_09 <- cbind.data.frame(PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120])
colnames(THA_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(THA_VARrobustaltspec_09$tfpri),diff(THA_VARrobustaltspec_09$reer),diff(THA_VARrobustaltspec_09$ipc),diff(THA_VARrobustaltspec_09$ipsea),diff(THA_VARrobustaltspec_09$exports))
colnames(THA_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_09 <- na.omit(THA_VARrobustaltspec_diff_09)
VARselect(THA_VARrobustaltspec_diff_09)
THA_VARrobustaltspec_diff_09 <- VAR(THA_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_09)
#short-run
irf_THA_VARrobustaltspec_diff_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VARrobustaltspec_diff_cum_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#using aggregate cooperation PRIs, 2000-2009
#Indonesia
sink(file = "Table 8_aggcoop_IDN.txt")
#00-09
IDN_VARrobustaltspec_09 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120],adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120])
colnames(IDN_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(IDN_VARrobustaltspec_09$tfpri),diff(IDN_VARrobustaltspec_09$reer),diff(IDN_VARrobustaltspec_09$ipc),diff(IDN_VARrobustaltspec_09$ipsea),diff(IDN_VARrobustaltspec_09$exports))
colnames(IDN_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_09 <- na.omit(IDN_VARrobustaltspec_diff_09)
VARselect(IDN_VARrobustaltspec_diff_09)
IDN_VARrobustaltspec_diff_09 <- VAR(IDN_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_09)
#short-run
irf_IDN_VARrobustaltspec_diff_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR_09 <- plot(irf_IDN_VARrobustaltspec_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VARrobustaltspec_diff_cum_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Malaysia
sink(file = "Table 8_aggcoop_MYS.txt")
#00-09
MYS_VARrobustaltspec_09 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120])
colnames(MYS_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(MYS_VARrobustaltspec_09$tfpri),diff(MYS_VARrobustaltspec_09$reer),diff(MYS_VARrobustaltspec_09$ipc),diff(MYS_VARrobustaltspec_09$ipsea),diff(MYS_VARrobustaltspec_09$exports))
colnames(MYS_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff_09 <- na.omit(MYS_VARrobustaltspec_diff_09)
VARselect(MYS_VARrobustaltspec_diff_09)
MYS_VARrobustaltspec_diff_09 <- VAR(MYS_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff_09)
#short-run
irf_MYS_VARrobustaltspec_diff_09 <- irf(MYS_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR_09 <- plot(irf_MYS_VARrobustaltspec_diff_09) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_MYS_09 <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09
#long-run
irf_MYS_VARrobustaltspec_diff_cum_09 <- irf(MYS_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum_09) + title(main = "Malaysia")
lower_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_09 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_09 <- rbind.data.frame(lower_MYS_LR_09*100,irf_MYS_LR_09*100,upper_MYS_LR_09*100)
effects_MYS_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 8_aggcoop_PHL.txt")
#00-09
PHL_VARrobustaltspec_09 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120])
colnames(PHL_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(PHL_VARrobustaltspec_09$tfpri),diff(PHL_VARrobustaltspec_09$reer),diff(PHL_VARrobustaltspec_09$ipc),diff(PHL_VARrobustaltspec_09$ipsea),diff(PHL_VARrobustaltspec_09$exports))
colnames(PHL_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_09 <- na.omit(PHL_VARrobustaltspec_diff_09)
VARselect(PHL_VARrobustaltspec_diff_09)
PHL_VARrobustaltspec_diff_09 <- VAR(PHL_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_09)
#short-run
irf_PHL_VARrobustaltspec_diff_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR_09 <- plot(irf_PHL_VARrobustaltspec_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VARrobustaltspec_diff_cum_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 8_aggcoop_SGP.txt")
#00-09
SGP_VARrobustaltspec_09 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120])
colnames(SGP_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(SGP_VARrobustaltspec_09$tfpri),diff(SGP_VARrobustaltspec_09$reer),diff(SGP_VARrobustaltspec_09$ipc),diff(SGP_VARrobustaltspec_09$ipsea),diff(SGP_VARrobustaltspec_09$exports))
colnames(SGP_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_09 <- na.omit(SGP_VARrobustaltspec_diff_09)
VARselect(SGP_VARrobustaltspec_diff_09)
SGP_VARrobustaltspec_diff_09 <- VAR(SGP_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_09)
#short-run
irf_SGP_VARrobustaltspec_diff_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR_09 <- plot(irf_SGP_VARrobustaltspec_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VARrobustaltspec_diff_cum_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 8_aggcoop_THA.txt")
#00-09
THA_VARrobustaltspec_09 <- cbind.data.frame(coop_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120])
colnames(THA_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(THA_VARrobustaltspec_09$tfpri),diff(THA_VARrobustaltspec_09$reer),diff(THA_VARrobustaltspec_09$ipc),diff(THA_VARrobustaltspec_09$ipsea),diff(THA_VARrobustaltspec_09$exports))
colnames(THA_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_09 <- na.omit(THA_VARrobustaltspec_diff_09)
VARselect(THA_VARrobustaltspec_diff_09)
THA_VARrobustaltspec_diff_09 <- VAR(THA_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_09)
#short-run
irf_THA_VARrobustaltspec_diff_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR_09 <- plot(irf_THA_VARrobustaltspec_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VARrobustaltspec_diff_cum_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#using aggregate conflict PRIs, 2000-2009
#Indonesia
sink(file = "Table 8_aggconf_IDN.txt")
#00-09
IDN_VARrobustaltspec_09 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120],adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120])
colnames(IDN_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(IDN_VARrobustaltspec_09$tfpri),diff(IDN_VARrobustaltspec_09$reer),diff(IDN_VARrobustaltspec_09$ipc),diff(IDN_VARrobustaltspec_09$ipsea),diff(IDN_VARrobustaltspec_09$exports))
colnames(IDN_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_09 <- na.omit(IDN_VARrobustaltspec_diff_09)
VARselect(IDN_VARrobustaltspec_diff_09)
IDN_VARrobustaltspec_diff_09 <- VAR(IDN_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_09)
#short-run
irf_IDN_VARrobustaltspec_diff_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff_09) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_IDN_09 <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09
#long-run
irf_IDN_VARrobustaltspec_diff_cum_09 <- irf(IDN_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_09) + title(main = "Indonesia")
lower_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_09 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_09 <- rbind.data.frame(lower_IDN_LR_09*100,irf_IDN_LR_09*100,upper_IDN_LR_09*100)
effects_IDN_LR_09
sink(file = NULL)

#Philippines
sink(file = "Table 8_aggconf_PHL.txt")
#00-09
PHL_VARrobustaltspec_09 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120])
colnames(PHL_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(PHL_VARrobustaltspec_09$tfpri),diff(PHL_VARrobustaltspec_09$reer),diff(PHL_VARrobustaltspec_09$ipc),diff(PHL_VARrobustaltspec_09$ipsea),diff(PHL_VARrobustaltspec_09$exports))
colnames(PHL_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_09 <- na.omit(PHL_VARrobustaltspec_diff_09)
VARselect(PHL_VARrobustaltspec_diff_09)
PHL_VARrobustaltspec_diff_09 <- VAR(PHL_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_09)
#short-run
irf_PHL_VARrobustaltspec_diff_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff_09) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_PHL_09 <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09
#long-run
irf_PHL_VARrobustaltspec_diff_cum_09 <- irf(PHL_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_09) + title(main = "Philippines")
lower_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_09 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_09 <- rbind.data.frame(lower_PHL_LR_09*100,irf_PHL_LR_09*100,upper_PHL_LR_09*100)
effects_PHL_LR_09
sink(file = NULL)

#Singapore
sink(file = "Table 8_aggconf_SGP.txt")
#00-09
SGP_VARrobustaltspec_09 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120])
colnames(SGP_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(SGP_VARrobustaltspec_09$tfpri),diff(SGP_VARrobustaltspec_09$reer),diff(SGP_VARrobustaltspec_09$ipc),diff(SGP_VARrobustaltspec_09$ipsea),diff(SGP_VARrobustaltspec_09$exports))
colnames(SGP_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_09 <- na.omit(SGP_VARrobustaltspec_diff_09)
VARselect(SGP_VARrobustaltspec_diff_09)
SGP_VARrobustaltspec_diff_09 <- VAR(SGP_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_09)
#short-run
irf_SGP_VARrobustaltspec_diff_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff_09) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_SGP_09 <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09
#long-run
irf_SGP_VARrobustaltspec_diff_cum_09 <- irf(SGP_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_09) + title(main = "Singapore")
lower_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_09 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_09 <- rbind.data.frame(lower_SGP_LR_09*100,irf_SGP_LR_09*100,upper_SGP_LR_09*100)
effects_SGP_LR_09
sink(file = NULL)

#Thailand
sink(file = "Table 8_aggconf_THA.txt")
#00-09
THA_VARrobustaltspec_09 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120])
colnames(THA_VARrobustaltspec_09) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_09 <- cbind.data.frame(diff(THA_VARrobustaltspec_09$tfpri),diff(THA_VARrobustaltspec_09$reer),diff(THA_VARrobustaltspec_09$ipc),diff(THA_VARrobustaltspec_09$ipsea),diff(THA_VARrobustaltspec_09$exports))
colnames(THA_VARrobustaltspec_diff_09) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_09 <- na.omit(THA_VARrobustaltspec_diff_09)
VARselect(THA_VARrobustaltspec_diff_09)
THA_VARrobustaltspec_diff_09 <- VAR(THA_VARrobustaltspec_diff_09,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_09)
#short-run
irf_THA_VARrobustaltspec_diff_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff_09) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Lower"]][["diff_tfpri"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["irf"]][["diff_tfpri"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_09[["Upper"]][["diff_tfpri"]]))
effects_THA_09 <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09
#long-run
irf_THA_VARrobustaltspec_diff_cum_09 <- irf(THA_VARrobustaltspec_diff_09, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_09) + title(main = "Thailand")
lower_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_09 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_09[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_09 <- rbind.data.frame(lower_THA_LR_09*100,irf_THA_LR_09*100,upper_THA_LR_09*100)
effects_THA_LR_09
sink(file = NULL)

#Table 9
#using net cooperation PRIs, 2010-2019
sink(file = "Table 9_netcoop_IDN.txt")
#Indonesia
#10-19
IDN_VARrobustaltspec_19 <- cbind.data.frame(PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240])
colnames(IDN_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(IDN_VARrobustaltspec_19$tfpri),diff(IDN_VARrobustaltspec_19$reer),diff(IDN_VARrobustaltspec_19$ipc),diff(IDN_VARrobustaltspec_19$ipsea),diff(IDN_VARrobustaltspec_19$exports))
colnames(IDN_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_19 <- na.omit(IDN_VARrobustaltspec_diff_19)
VARselect(IDN_VARrobustaltspec_diff_19)
IDN_VARrobustaltspec_diff_19 <- VAR(IDN_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_19)
#short-run
irf_IDN_VARrobustaltspec_diff_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VARrobustaltspec_diff_cum_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 9_netcoop_MYS.txt")
#10-19
MYS_VARrobustaltspec_19 <- cbind.data.frame(PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240])
colnames(MYS_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(MYS_VARrobustaltspec_19$tfpri),diff(MYS_VARrobustaltspec_19$reer),diff(MYS_VARrobustaltspec_19$ipc),diff(MYS_VARrobustaltspec_19$ipsea),diff(MYS_VARrobustaltspec_19$exports))
colnames(MYS_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff_19 <- na.omit(MYS_VARrobustaltspec_diff_19)
VARselect(MYS_VARrobustaltspec_diff_19)
MYS_VARrobustaltspec_diff_19 <- VAR(MYS_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff_19)
#short-run
irf_MYS_VARrobustaltspec_diff_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VARrobustaltspec_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VARrobustaltspec_diff_cum_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 9_netcoop_PHL.txt")
#10-19
PHL_VARrobustaltspec_19 <- cbind.data.frame(PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240])
colnames(PHL_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(PHL_VARrobustaltspec_19$tfpri),diff(PHL_VARrobustaltspec_19$reer),diff(PHL_VARrobustaltspec_19$ipc),diff(PHL_VARrobustaltspec_19$ipsea),diff(PHL_VARrobustaltspec_19$exports))
colnames(PHL_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_19 <- na.omit(PHL_VARrobustaltspec_diff_19)
VARselect(PHL_VARrobustaltspec_diff_19)
PHL_VARrobustaltspec_diff_19 <- VAR(PHL_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_19)
#short-run
irf_PHL_VARrobustaltspec_diff_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VARrobustaltspec_diff_cum_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 9_netcoop_SGP.txt")
#10-19
SGP_VARrobustaltspec_19 <- cbind.data.frame(PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240])
colnames(SGP_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(SGP_VARrobustaltspec_19$tfpri),diff(SGP_VARrobustaltspec_19$reer),diff(SGP_VARrobustaltspec_19$ipc),diff(SGP_VARrobustaltspec_19$ipsea),diff(SGP_VARrobustaltspec_19$exports))
colnames(SGP_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_19 <- na.omit(SGP_VARrobustaltspec_diff_19)
VARselect(SGP_VARrobustaltspec_diff_19)
SGP_VARrobustaltspec_diff_19 <- VAR(SGP_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_19)
#short-run
irf_SGP_VARrobustaltspec_diff_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VARrobustaltspec_diff_cum_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 9_netcoop_THA.txt")
#10-19
THA_VARrobustaltspec_19 <- cbind.data.frame(PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240])
colnames(THA_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(THA_VARrobustaltspec_19$tfpri),diff(THA_VARrobustaltspec_19$reer),diff(THA_VARrobustaltspec_19$ipc),diff(THA_VARrobustaltspec_19$ipsea),diff(THA_VARrobustaltspec_19$exports))
colnames(THA_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_19 <- na.omit(THA_VARrobustaltspec_diff_19)
VARselect(THA_VARrobustaltspec_diff_19)
THA_VARrobustaltspec_diff_19 <- VAR(THA_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_19)
#short-run
irf_THA_VARrobustaltspec_diff_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VARrobustaltspec_diff_cum_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 9_netcoop_VNM.txt")
#10-19
VNM_VARrobustaltspec_19 <- cbind.data.frame(PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144])
colnames(VNM_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(VNM_VARrobustaltspec_19$tfpri),diff(VNM_VARrobustaltspec_19$reer),diff(VNM_VARrobustaltspec_19$ipc),diff(VNM_VARrobustaltspec_19$ipsea),diff(VNM_VARrobustaltspec_19$exports))
colnames(VNM_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff_19 <- na.omit(VNM_VARrobustaltspec_diff_19)
VARselect(VNM_VARrobustaltspec_diff_19)
VNM_VARrobustaltspec_diff_19 <- VAR(VNM_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff_19)
#short-run
irf_VNM_VARrobustaltspec_diff_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VARrobustaltspec_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VARrobustaltspec_diff_cum_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#using aggregate cooperation PRIs, 2010-2019
#Indonesia
sink(file = "Table 9_aggcoop_IDN.txt")
#10-19
IDN_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240])
colnames(IDN_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(IDN_VARrobustaltspec_19$tfpri),diff(IDN_VARrobustaltspec_19$reer),diff(IDN_VARrobustaltspec_19$ipc),diff(IDN_VARrobustaltspec_19$ipsea),diff(IDN_VARrobustaltspec_19$exports))
colnames(IDN_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_19 <- na.omit(IDN_VARrobustaltspec_diff_19)
VARselect(IDN_VARrobustaltspec_diff_19)
IDN_VARrobustaltspec_diff_19 <- VAR(IDN_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_19)
#short-run
irf_IDN_VARrobustaltspec_diff_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
IDN_coop_plot_SR_19 <- plot(irf_IDN_VARrobustaltspec_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VARrobustaltspec_diff_cum_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 9_aggcoop_MYS.txt")
#10-19
MYS_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240])
colnames(MYS_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(MYS_VARrobustaltspec_19$tfpri),diff(MYS_VARrobustaltspec_19$reer),diff(MYS_VARrobustaltspec_19$ipc),diff(MYS_VARrobustaltspec_19$ipsea),diff(MYS_VARrobustaltspec_19$exports))
colnames(MYS_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff_19 <- na.omit(MYS_VARrobustaltspec_diff_19)
VARselect(MYS_VARrobustaltspec_diff_19)
MYS_VARrobustaltspec_diff_19 <- VAR(MYS_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff_19)
#short-run
irf_MYS_VARrobustaltspec_diff_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
MYS_coop_plot_SR_19 <- plot(irf_MYS_VARrobustaltspec_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VARrobustaltspec_diff_cum_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 9_aggcoop_PHL.txt")
#10-19
PHL_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240])
colnames(PHL_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(PHL_VARrobustaltspec_19$tfpri),diff(PHL_VARrobustaltspec_19$reer),diff(PHL_VARrobustaltspec_19$ipc),diff(PHL_VARrobustaltspec_19$ipsea),diff(PHL_VARrobustaltspec_19$exports))
colnames(PHL_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_19 <- na.omit(PHL_VARrobustaltspec_diff_19)
VARselect(PHL_VARrobustaltspec_diff_19)
PHL_VARrobustaltspec_diff_19 <- VAR(PHL_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_19)
#short-run
irf_PHL_VARrobustaltspec_diff_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
PHL_coop_plot_SR_19 <- plot(irf_PHL_VARrobustaltspec_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VARrobustaltspec_diff_cum_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 9_aggcoop_SGP.txt")
#10-19
SGP_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240])
colnames(SGP_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(SGP_VARrobustaltspec_19$tfpri),diff(SGP_VARrobustaltspec_19$reer),diff(SGP_VARrobustaltspec_19$ipc),diff(SGP_VARrobustaltspec_19$ipsea),diff(SGP_VARrobustaltspec_19$exports))
colnames(SGP_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_19 <- na.omit(SGP_VARrobustaltspec_diff_19)
VARselect(SGP_VARrobustaltspec_diff_19)
SGP_VARrobustaltspec_diff_19 <- VAR(SGP_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_19)
#short-run
irf_SGP_VARrobustaltspec_diff_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
SGP_coop_plot_SR_19 <- plot(irf_SGP_VARrobustaltspec_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VARrobustaltspec_diff_cum_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 9_aggcoop_THA.txt")
#10-19
THA_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240])
colnames(THA_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(THA_VARrobustaltspec_19$tfpri),diff(THA_VARrobustaltspec_19$reer),diff(THA_VARrobustaltspec_19$ipc),diff(THA_VARrobustaltspec_19$ipsea),diff(THA_VARrobustaltspec_19$exports))
colnames(THA_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_19 <- na.omit(THA_VARrobustaltspec_diff_19)
VARselect(THA_VARrobustaltspec_diff_19)
THA_VARrobustaltspec_diff_19 <- VAR(THA_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_19)
#short-run
irf_THA_VARrobustaltspec_diff_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
THA_coop_plot_SR_19 <- plot(irf_THA_VARrobustaltspec_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VARrobustaltspec_diff_cum_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 9_aggcoop_VNM.txt")
#10-19
VNM_VARrobustaltspec_19 <- cbind.data.frame(coop_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144])
colnames(VNM_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(VNM_VARrobustaltspec_19$tfpri),diff(VNM_VARrobustaltspec_19$reer),diff(VNM_VARrobustaltspec_19$ipc),diff(VNM_VARrobustaltspec_19$ipsea),diff(VNM_VARrobustaltspec_19$exports))
colnames(VNM_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff_19 <- na.omit(VNM_VARrobustaltspec_diff_19)
VARselect(VNM_VARrobustaltspec_diff_19)
VNM_VARrobustaltspec_diff_19 <- VAR(VNM_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff_19)
#short-run
irf_VNM_VARrobustaltspec_diff_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
VNM_coop_plot_SR_19 <- plot(irf_VNM_VARrobustaltspec_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VARrobustaltspec_diff_cum_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#using aggregate conflict PRIs, 2010-2019
#Indonesia
sink(file = "Table 9_aggconf_IDN.txt")
#10-19
IDN_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240])
colnames(IDN_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
IDN_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(IDN_VARrobustaltspec_19$tfpri),diff(IDN_VARrobustaltspec_19$reer),diff(IDN_VARrobustaltspec_19$ipc),diff(IDN_VARrobustaltspec_19$ipsea),diff(IDN_VARrobustaltspec_19$exports))
colnames(IDN_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
IDN_VARrobustaltspec_diff_19 <- na.omit(IDN_VARrobustaltspec_diff_19)
VARselect(IDN_VARrobustaltspec_diff_19)
IDN_VARrobustaltspec_diff_19 <- VAR(IDN_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(IDN_VARrobustaltspec_diff_19)
#short-run
irf_IDN_VARrobustaltspec_diff_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VARrobustaltspec_diff_19) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_IDN_19 <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19
#long-run
irf_IDN_VARrobustaltspec_diff_cum_19 <- irf(IDN_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_IDN_VARrobustaltspec_diff_cum_19) + title(main = "Indonesia")
lower_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_IDN_LR_19 <- as.data.frame(t(irf_IDN_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_IDN_LR_19 <- rbind.data.frame(lower_IDN_LR_19*100,irf_IDN_LR_19*100,upper_IDN_LR_19*100)
effects_IDN_LR_19
sink(file = NULL)

#Malaysia
sink(file = "Table 9_aggconf_MYS.txt")
###10-19
MYS_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240])
colnames(MYS_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
MYS_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(MYS_VARrobustaltspec_19$tfpri),diff(MYS_VARrobustaltspec_19$reer),diff(MYS_VARrobustaltspec_19$ipc),diff(MYS_VARrobustaltspec_19$ipsea),diff(MYS_VARrobustaltspec_19$exports))
colnames(MYS_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
MYS_VARrobustaltspec_diff_19 <- na.omit(MYS_VARrobustaltspec_diff_19)
VARselect(MYS_VARrobustaltspec_diff_19)
MYS_VARrobustaltspec_diff_19 <- VAR(MYS_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(MYS_VARrobustaltspec_diff_19)
#short-run
irf_MYS_VARrobustaltspec_diff_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VARrobustaltspec_diff_19) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_MYS_19 <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19
#long-run
irf_MYS_VARrobustaltspec_diff_cum_19 <- irf(MYS_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_MYS_VARrobustaltspec_diff_cum_19) + title(main = "Malaysia")
lower_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_MYS_LR_19 <- as.data.frame(t(irf_MYS_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_MYS_LR_19 <- rbind.data.frame(lower_MYS_LR_19*100,irf_MYS_LR_19*100,upper_MYS_LR_19*100)
effects_MYS_LR_19
sink(file = NULL)

#Philippines
sink(file = "Table 9_aggconf_PHL.txt")
#10-19
PHL_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240])
colnames(PHL_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
PHL_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(PHL_VARrobustaltspec_19$tfpri),diff(PHL_VARrobustaltspec_19$reer),diff(PHL_VARrobustaltspec_19$ipc),diff(PHL_VARrobustaltspec_19$ipsea),diff(PHL_VARrobustaltspec_19$exports))
colnames(PHL_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
PHL_VARrobustaltspec_diff_19 <- na.omit(PHL_VARrobustaltspec_diff_19)
VARselect(PHL_VARrobustaltspec_diff_19)
PHL_VARrobustaltspec_diff_19 <- VAR(PHL_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(PHL_VARrobustaltspec_diff_19)
#short-run
irf_PHL_VARrobustaltspec_diff_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VARrobustaltspec_diff_19) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_PHL_19 <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19
#long-run
irf_PHL_VARrobustaltspec_diff_cum_19 <- irf(PHL_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_PHL_VARrobustaltspec_diff_cum_19) + title(main = "Philippines")
lower_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_PHL_LR_19 <- as.data.frame(t(irf_PHL_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_PHL_LR_19 <- rbind.data.frame(lower_PHL_LR_19*100,irf_PHL_LR_19*100,upper_PHL_LR_19*100)
effects_PHL_LR_19
sink(file = NULL)

#Singapore
sink(file = "Table 9_aggconf_SGP.txt")
#10-19
SGP_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240])
colnames(SGP_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
SGP_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(SGP_VARrobustaltspec_19$tfpri),diff(SGP_VARrobustaltspec_19$reer),diff(SGP_VARrobustaltspec_19$ipc),diff(SGP_VARrobustaltspec_19$ipsea),diff(SGP_VARrobustaltspec_19$exports))
colnames(SGP_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
SGP_VARrobustaltspec_diff_19 <- na.omit(SGP_VARrobustaltspec_diff_19)
VARselect(SGP_VARrobustaltspec_diff_19)
SGP_VARrobustaltspec_diff_19 <- VAR(SGP_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(SGP_VARrobustaltspec_diff_19)
#short-run
irf_SGP_VARrobustaltspec_diff_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VARrobustaltspec_diff_19) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_SGP_19 <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19
#long-run
irf_SGP_VARrobustaltspec_diff_cum_19 <- irf(SGP_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_SGP_VARrobustaltspec_diff_cum_19) + title(main = "Singapore")
lower_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_SGP_LR_19 <- as.data.frame(t(irf_SGP_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_SGP_LR_19 <- rbind.data.frame(lower_SGP_LR_19*100,irf_SGP_LR_19*100,upper_SGP_LR_19*100)
effects_SGP_LR_19
sink(file = NULL)

#Thailand
sink(file = "Table 9_aggconf_THA.txt")
#10-19
THA_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240])
colnames(THA_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
THA_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(THA_VARrobustaltspec_19$tfpri),diff(THA_VARrobustaltspec_19$reer),diff(THA_VARrobustaltspec_19$ipc),diff(THA_VARrobustaltspec_19$ipsea),diff(THA_VARrobustaltspec_19$exports))
colnames(THA_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
THA_VARrobustaltspec_diff_19 <- na.omit(THA_VARrobustaltspec_diff_19)
VARselect(THA_VARrobustaltspec_diff_19)
THA_VARrobustaltspec_diff_19 <- VAR(THA_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(THA_VARrobustaltspec_diff_19)
#short-run
irf_THA_VARrobustaltspec_diff_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VARrobustaltspec_diff_19) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_THA_19 <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19
#long-run
irf_THA_VARrobustaltspec_diff_cum_19 <- irf(THA_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_THA_VARrobustaltspec_diff_cum_19) + title(main = "Thailand")
lower_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_THA_LR_19 <- as.data.frame(t(irf_THA_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_THA_LR_19 <- rbind.data.frame(lower_THA_LR_19*100,irf_THA_LR_19*100,upper_THA_LR_19*100)
effects_THA_LR_19
sink(file = NULL)

#Vietnam
sink(file = "Table 9_aggconf_VNM.txt")
#10-19
VNM_VARrobustaltspec_19 <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144])
colnames(VNM_VARrobustaltspec_19) <- c("tfpri","reer","ipc","ipsea","exports")
VNM_VARrobustaltspec_diff_19 <- cbind.data.frame(diff(VNM_VARrobustaltspec_19$tfpri),diff(VNM_VARrobustaltspec_19$reer),diff(VNM_VARrobustaltspec_19$ipc),diff(VNM_VARrobustaltspec_19$ipsea),diff(VNM_VARrobustaltspec_19$exports))
colnames(VNM_VARrobustaltspec_diff_19) <- c("diff_tfpri","diff_reer","diff_ipc","diff_ipsea","diff_exports")
VNM_VARrobustaltspec_diff_19 <- na.omit(VNM_VARrobustaltspec_diff_19)
VARselect(VNM_VARrobustaltspec_diff_19)
VNM_VARrobustaltspec_diff_19 <- VAR(VNM_VARrobustaltspec_diff_19,p = 1, type = "const")
summary(VNM_VARrobustaltspec_diff_19)
#short-run
irf_VNM_VARrobustaltspec_diff_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VARrobustaltspec_diff_19) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Lower"]][["diff_tfpri"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["irf"]][["diff_tfpri"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_19[["Upper"]][["diff_tfpri"]]))
effects_VNM_19 <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19
#long-run
irf_VNM_VARrobustaltspec_diff_cum_19 <- irf(VNM_VARrobustaltspec_diff_19, impulse = "diff_tfpri", response = "diff_exports", ortho = TRUE, n.ahead = 24, ci = 0.90, cumulative = TRUE)
plot(irf_VNM_VARrobustaltspec_diff_cum_19) + title(main = "Vietnam")
lower_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Lower"]][["diff_tfpri"]][25,]))
irf_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["irf"]][["diff_tfpri"]][25,]))
upper_VNM_LR_19 <- as.data.frame(t(irf_VNM_VARrobustaltspec_diff_cum_19[["Upper"]][["diff_tfpri"]][25,]))
effects_VNM_LR_19 <- rbind.data.frame(lower_VNM_LR_19*100,irf_VNM_LR_19*100,upper_VNM_LR_19*100)
effects_VNM_LR_19
sink(file = NULL)

#robustness tests: VAR model stability
#shock in each of the six Southeast Asian countries' REER series (using net cooperation PRIs, 2000-2019)
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfpri),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run (check reer)
irf_IDN_VAR_diff_check_reer <- irf(IDN_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_reer) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_check_reer <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_reer
#Malaysia
#00-19
MYS_VAR <- cbind.data.frame(PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480])
colnames(MYS_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff <- cbind.data.frame(diff(MYS_VAR$tfpri),diff(MYS_VAR$reer),diff(MYS_VAR$exports),diff(MYS_VAR$ipc),diff(MYS_VAR$ipsea))
colnames(MYS_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff <- na.omit(MYS_VAR_diff)
VARselect(MYS_VAR_diff)
MYS_VAR_diff <- VAR(MYS_VAR_diff,p = 1, type = "const")
summary(MYS_VAR_diff)
#short-run (check reer)
irf_MYS_VAR_diff_check_reer <- irf(MYS_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_reer) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_check_reer <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_reer
#Philippines
#00-19
PHL_VAR <- cbind.data.frame(PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfpri),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run (check reer)
irf_PHL_VAR_diff_check_reer <- irf(PHL_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_reer) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_check_reer <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_reer
#Singapore
#00-19
SGP_VAR <- cbind.data.frame(PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfpri),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run (check reer)
irf_SGP_VAR_diff_check_reer <- irf(SGP_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_reer) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_check_reer <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_reer
#Thailand
#00-19
THA_VAR <- cbind.data.frame(PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfpri),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run (check reer)
irf_THA_VAR_diff_check_reer <- irf(THA_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_reer) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_check_reer <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_reer
#Vietnam
#08-19
VNM_VAR <- cbind.data.frame(PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfpri),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run (check reer)
irf_VNM_VAR_diff_check_reer <- irf(VNM_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_reer) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_check_reer <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_reer

#Figure 5
#IRF plots
#00-19
IDN_net_plot_SR_1 <- plot(irf_IDN_VAR_diff_check_reer, main="Indonesia (REER shock)", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)
MYS_net_plot_SR_1 <- plot(irf_MYS_VAR_diff_check_reer, main="Malaysia (REER shock)", xlab="", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)
PHL_net_plot_SR_1 <- plot(irf_PHL_VAR_diff_check_reer, main="Philippines (REER shock)", xlab="", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)
SGP_net_plot_SR_1 <- plot(irf_SGP_VAR_diff_check_reer, main="Singapore (REER shock)", xlab="", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)
THA_net_plot_SR_1 <- plot(irf_THA_VAR_diff_check_reer, main="Thailand (REER shock)", xlab="", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)
VNM_net_plot_SR_1 <- plot(irf_VNM_VAR_diff_check_reer, main="Vietnam (REER shock)", xlab="", ylab="", sub="months post REER shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
mtext("change in exports/100 (%)", side = 2, line = 3.5, cex = 2.5)

#robustness tests cont.: VAR model stability; not reported in paper
#shock in China's industrial production series (using net cooperation PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipc)
irf_IDN_VAR_diff_check_ipc <- irf(IDN_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipc) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_check_ipc <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipc
#Malaysia
#00-19
#short-run (check ipc)
irf_MYS_VAR_diff_check_ipc <- irf(MYS_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipc) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_check_ipc <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipc
#Philippines
#00-19
#short-run (check ipc)
irf_PHL_VAR_diff_check_ipc <- irf(PHL_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipc) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_check_ipc <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipc
#Singapore
#00-19
#short-run (check ipc)
irf_SGP_VAR_diff_check_ipc <- irf(SGP_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipc) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_check_ipc <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipc
#Thailand
#00-19
#short-run (check ipc)
irf_THA_VAR_diff_check_ipc <- irf(THA_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipc) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_check_ipc <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipc
#Vietnam
#00-19
#short-run (check ipc)
irf_VNM_VAR_diff_check_ipc <- irf(VNM_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipc) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_check_ipc <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using net cooperation PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipsea)
irf_IDN_VAR_diff_check_ipsea <- irf(IDN_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipsea) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_check_ipsea <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipsea
#Malaysia
#00-19
#short-run (check ipsea)
irf_MYS_VAR_diff_check_ipsea <- irf(MYS_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipsea) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_check_ipsea <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipsea
#Philippines
#00-19
#short-run (check ipsea)
irf_PHL_VAR_diff_check_ipsea <- irf(PHL_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipsea) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_check_ipsea <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipsea
#Singapore
#00-19
#short-run (check ipsea)
irf_SGP_VAR_diff_check_ipsea <- irf(SGP_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipsea) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_check_ipsea <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipsea
#Thailand
#00-19
#short-run (check ipsea)
irf_THA_VAR_diff_check_ipsea <- irf(THA_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipsea) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_check_ipsea <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipsea
#Vietnam
#00-19
#short-run (check ipsea)
irf_VNM_VAR_diff_check_ipsea <- irf(VNM_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipsea) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_check_ipsea <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using net cooperation PRIs, 2000-2009)
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfpri),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run (check reer)
irf_IDN_VAR_diff_09_check_reer <- irf(IDN_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_reer) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_09_check_reer <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09_check_reer
#Malaysia
#00-09
MYS_VAR_09 <- cbind.data.frame(PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360])
colnames(MYS_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_09 <- cbind.data.frame(diff(MYS_VAR_09$tfpri),diff(MYS_VAR_09$reer),diff(MYS_VAR_09$exports),diff(MYS_VAR_09$ipc),diff(MYS_VAR_09$ipsea))
colnames(MYS_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_09 <- na.omit(MYS_VAR_diff_09)
VARselect(MYS_VAR_diff_09)
MYS_VAR_diff_09 <- VAR(MYS_VAR_diff_09,p = 1, type = "const")
summary(MYS_VAR_diff_09)
#short-run (check reer)
irf_MYS_VAR_diff_09_check_reer <- irf(MYS_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_reer) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_09_check_reer <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_reer
#Philippines
#00-09
PHL_VAR_09 <- cbind.data.frame(PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfpri),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run (check reer)
irf_PHL_VAR_diff_09_check_reer <- irf(PHL_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_reer) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_09_check_reer <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_reer
#Singapore
#00-09
SGP_VAR_09 <- cbind.data.frame(PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfpri),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run (check reer)
irf_SGP_VAR_diff_09_check_reer <- irf(SGP_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_reer) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_09_check_reer <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_reer
#Thailand
#00-09
THA_VAR_09 <- cbind.data.frame(PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfpri),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run (check reer)
irf_THA_VAR_diff_09_check_reer <- irf(THA_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_reer) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_09_check_reer <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_reer

#shock in China's industrial production series (using net cooperation PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipc)
irf_IDN_VAR_diff_09_check_ipc <- irf(IDN_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipc, main="Indonesia (IIP China shock)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
irf_IDN_VAR_diff_09_check_ipc <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipc
#Malaysia
#00-09
#short-run (check ipc)
irf_MYS_VAR_diff_09_check_ipc <- irf(MYS_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_ipc) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_09_check_ipc <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_ipc
#Philippines
#00-09
#short-run (check ipc)
irf_PHL_VAR_diff_09_check_ipc <- irf(PHL_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipc) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_09_check_ipc <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipc
#Singapore
#00-09
#short-run (check ipc)
irf_SGP_VAR_diff_09_check_ipc <- irf(SGP_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipc) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_09_check_ipc <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipc
#Thailand
#00-09
#short-run (check ipc)
irf_THA_VAR_diff_09_check_ipc <- irf(THA_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipc) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_09_check_ipc <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using net cooperation PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipsea)
irf_IDN_VAR_diff_09_check_ipsea <- irf(IDN_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipsea, main="Indonesia (IIP Indonesia)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
irf_IDN_VAR_diff_09_check_ipsea <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipsea
#Malaysia
#00-09
#short-run (check ipsea)
irf_MYS_VAR_diff_09_check_ipsea <- irf(MYS_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_ipsea) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_09_check_ipsea <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_ipsea
#Philippines
#00-09
#short-run (check ipsea)
irf_PHL_VAR_diff_09_check_ipsea <- irf(PHL_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipsea) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_09_check_ipsea <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipsea
#Singapore
#00-09
#short-run (check ipsea)
irf_SGP_VAR_diff_09_check_ipsea <- irf(SGP_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipsea) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_09_check_ipsea <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipsea
#Thailand
#00-09
#short-run (check ipsea)
irf_THA_VAR_diff_09_check_ipsea <- irf(THA_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipsea) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_09_check_ipsea <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipsea

#shock in in each of the six countries' REER series (using net cooperation PRIs, 2010-2019)
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfpri),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run (check reer)
irf_IDN_VAR_diff_19_check_reer <- irf(IDN_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_reer) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_19_check_reer <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_reer
#Malaysia
#10-19
MYS_VAR_19 <- cbind.data.frame(PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfpri),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run (check reer)
irf_MYS_VAR_diff_19_check_reer <- irf(MYS_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_reer) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_19_check_reer <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_reer
#Philippines
#10-19
PHL_VAR_19 <- cbind.data.frame(PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfpri),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run (check reer)
irf_PHL_VAR_diff_19_check_reer <- irf(PHL_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_reer) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_19_check_reer <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_reer
#Singapore
#10-19
SGP_VAR_19 <- cbind.data.frame(PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfpri),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run (check reer)
irf_SGP_VAR_diff_19_check_reer <- irf(SGP_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_reer) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_19_check_reer <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_reer
#Thailand
#10-19
THA_VAR_19 <- cbind.data.frame(PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfpri),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run (check reer)
irf_THA_VAR_diff_19_check_reer <- irf(THA_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_reer) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_19_check_reer <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_reer
#Vietnam
#10-19
VNM_VAR_19 <- cbind.data.frame(PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfpri),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run (check reer)
irf_VNM_VAR_diff_19_check_reer <- irf(VNM_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_reer) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_19_check_reer <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_reer

#shock in China's industrial production series (using net cooperation PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipc)
irf_IDN_VAR_diff_19_check_ipc <- irf(IDN_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipc) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_19_check_ipc <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipc
#Malaysia
#10-19
#short-run (check ipc)
irf_MYS_VAR_diff_19_check_ipc <- irf(MYS_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipc) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_19_check_ipc <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipc
#Philippines
#10-19
#short-run (check ipc)
irf_PHL_VAR_diff_19_check_ipc <- irf(PHL_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipc) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_19_check_ipc <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipc
#Singapore
#10-19
#short-run (check ipc)
irf_SGP_VAR_diff_19_check_ipc <- irf(SGP_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipc) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_19_check_ipc <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipc
#Thailand
#10-19
#short-run (check ipc)
irf_THA_VAR_diff_19_check_ipc <- irf(THA_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipc) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_19_check_ipc <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipc
#Vietnam
#10-19
#short-run (check ipc)
irf_VNM_VAR_diff_19_check_ipc <- irf(VNM_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipc) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_19_check_ipc <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using net cooperation PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipsea)
irf_IDN_VAR_diff_19_check_ipsea <- irf(IDN_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipsea) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_19_check_ipsea <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipsea
#Malaysia
#10-19
#short-run (check ipsea)
irf_MYS_VAR_diff_19_check_ipsea <- irf(MYS_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipsea) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_19_check_ipsea <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipsea
#Philippines
#10-19
#short-run (check ipsea)
irf_PHL_VAR_diff_19_check_ipsea <- irf(PHL_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipsea) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_19_check_ipsea <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipsea
#Singapore
#10-19
#short-run (check ipsea)
irf_SGP_VAR_diff_19_check_ipsea <- irf(SGP_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipsea) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_19_check_ipsea <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipsea
#Thailand
#10-19
#short-run (check ipsea)
irf_THA_VAR_diff_19_check_ipsea <- irf(THA_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipsea) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_19_check_ipsea <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipsea
#Vietnam
#10-19
#short-run (check ipsea)
irf_VNM_VAR_diff_19_check_ipsea <- irf(VNM_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipsea) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_19_check_ipsea <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using aggregate cooperation PRIs, 2000-2019)
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(coop_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfpri),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run (check reer)
irf_IDN_VAR_diff_check_reer <- irf(IDN_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_reer) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_check_reer <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_reer
#Malaysia
#00-19
MYS_VAR <- cbind.data.frame(coop_PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480])
colnames(MYS_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff <- cbind.data.frame(diff(MYS_VAR$tfpri),diff(MYS_VAR$reer),diff(MYS_VAR$exports),diff(MYS_VAR$ipc),diff(MYS_VAR$ipsea))
colnames(MYS_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff <- na.omit(MYS_VAR_diff)
VARselect(MYS_VAR_diff)
MYS_VAR_diff <- VAR(MYS_VAR_diff,p = 1, type = "const")
summary(MYS_VAR_diff)
#short-run (check reer)
irf_MYS_VAR_diff_check_reer <- irf(MYS_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_reer) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_check_reer <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_reer
#Philippines
#00-19
PHL_VAR <- cbind.data.frame(coop_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfpri),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run (check reer)
irf_PHL_VAR_diff_check_reer <- irf(PHL_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_reer) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_check_reer <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_reer
#Singapore
#00-19
SGP_VAR <- cbind.data.frame(coop_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfpri),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run (check reer)
irf_SGP_VAR_diff_check_reer <- irf(SGP_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_reer) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_check_reer <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_reer
#Thailand
#00-19
THA_VAR <- cbind.data.frame(coop_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfpri),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run (check reer)
irf_THA_VAR_diff_check_reer <- irf(THA_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_reer) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_check_reer <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_reer
#Vietnam
#08-19
VNM_VAR <- cbind.data.frame(coop_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfpri),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run (check reer)
irf_VNM_VAR_diff_check_reer <- irf(VNM_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_reer) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_check_reer <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_reer

#shock in China's industrial production series (using aggregate cooperation PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipc)
irf_IDN_VAR_diff_check_ipc <- irf(IDN_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipc) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_check_ipc <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipc
#Malaysia
#00-19
#short-run (check ipc)
irf_MYS_VAR_diff_check_ipc <- irf(MYS_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipc) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_check_ipc <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipc
#Philippines
#00-19
#short-run (check ipc)
irf_PHL_VAR_diff_check_ipc <- irf(PHL_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipc) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_check_ipc <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipc
#Singapore
#00-19
#short-run (check ipc)
irf_SGP_VAR_diff_check_ipc <- irf(SGP_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipc) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_check_ipc <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipc
#Thailand
#00-19
#short-run (check ipc)
irf_THA_VAR_diff_check_ipc <- irf(THA_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipc) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_check_ipc <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipc
#Vietnam
#00-19
#short-run (check ipc)
irf_VNM_VAR_diff_check_ipc <- irf(VNM_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipc) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_check_ipc <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate cooperation PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipsea)
irf_IDN_VAR_diff_check_ipsea <- irf(IDN_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipsea) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_check_ipsea <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipsea
#Malaysia
#00-19
#short-run (check ipsea)
irf_MYS_VAR_diff_check_ipsea <- irf(MYS_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipsea) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_check_ipsea <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipsea
#Philippines
#00-19
#short-run (check ipsea)
irf_PHL_VAR_diff_check_ipsea <- irf(PHL_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipsea) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_check_ipsea <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipsea
#Singapore
#00-19
#short-run (check ipsea)
irf_SGP_VAR_diff_check_ipsea <- irf(SGP_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipsea) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_check_ipsea <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipsea
#Thailand
#00-19
#short-run (check ipsea)
irf_THA_VAR_diff_check_ipsea <- irf(THA_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipsea) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_check_ipsea <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipsea
#Vietnam
#00-19
#short-run (check ipsea)
irf_VNM_VAR_diff_check_ipsea <- irf(VNM_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipsea) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_check_ipsea <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using aggregate cooperation PRIs, 2000-2009)
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfpri),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run (check reer)
irf_IDN_VAR_diff_09_check_reer <- irf(IDN_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_reer) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_09_check_reer <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09_check_reer
#Malaysia
#00-09
MYS_VAR_09 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[1:120], reer_ALL$log_reer[241:360], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[241:360])
colnames(MYS_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_09 <- cbind.data.frame(diff(MYS_VAR_09$tfpri),diff(MYS_VAR_09$reer),diff(MYS_VAR_09$exports),diff(MYS_VAR_09$ipc),diff(MYS_VAR_09$ipsea))
colnames(MYS_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_09 <- na.omit(MYS_VAR_diff_09)
VARselect(MYS_VAR_diff_09)
MYS_VAR_diff_09 <- VAR(MYS_VAR_diff_09,p = 1, type = "const")
summary(MYS_VAR_diff_09)
#short-run (check reer)
irf_MYS_VAR_diff_09_check_reer <- irf(MYS_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_reer) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_09_check_reer <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_reer
#Philippines
#00-09
PHL_VAR_09 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfpri),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run (check reer)
irf_PHL_VAR_diff_09_check_reer <- irf(PHL_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_reer) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_09_check_reer <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_reer
#Singapore
#00-09
SGP_VAR_09 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfpri),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run (check reer)
irf_SGP_VAR_diff_09_check_reer <- irf(SGP_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_reer) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_09_check_reer <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_reer
#Thailand
#00-09
THA_VAR_09 <- cbind.data.frame(coop_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfpri),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run (check reer)
irf_THA_VAR_diff_09_check_reer <- irf(THA_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_reer) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_09_check_reer <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_reer

#shock in China's industrial production series (using aggregate cooperation PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipc)
irf_IDN_VAR_diff_09_check_ipc <- irf(IDN_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipc, main="Indonesia (IIP China shock)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
irf_IDN_VAR_diff_09_check_ipc <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipc
#Malaysia
#00-09
#short-run (check ipc)
irf_MYS_VAR_diff_09_check_ipc <- irf(MYS_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_ipc) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_09_check_ipc <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_ipc
#Philippines
#00-09
#short-run (check ipc)
irf_PHL_VAR_diff_09_check_ipc <- irf(PHL_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipc) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_09_check_ipc <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipc
#Singapore
#00-09
#short-run (check ipc)
irf_SGP_VAR_diff_09_check_ipc <- irf(SGP_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipc) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_09_check_ipc <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipc
#Thailand
#00-09
#short-run (check ipc)
irf_THA_VAR_diff_09_check_ipc <- irf(THA_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipc) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_09_check_ipc <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate cooperation PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipsea)
irf_IDN_VAR_diff_09_check_ipsea <- irf(IDN_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipsea, main="Indonesia (IIP Indonesia)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
irf_IDN_VAR_diff_09_check_ipsea <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipsea
#Malaysia
#00-09
#short-run (check ipsea)
irf_MYS_VAR_diff_09_check_ipsea <- irf(MYS_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_09_check_ipsea) + title(main = "Malaysia")
lower_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS_09 <- as.data.frame(t(irf_MYS_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_09_check_ipsea <- rbind.data.frame(lower_MYS_09*100,irf_MYS_09*100,upper_MYS_09*100)
effects_MYS_09_check_ipsea
#Philippines
#00-09
#short-run (check ipsea)
irf_PHL_VAR_diff_09_check_ipsea <- irf(PHL_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipsea) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_09_check_ipsea <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipsea
#Singapore
#00-09
#short-run (check ipsea)
irf_SGP_VAR_diff_09_check_ipsea <- irf(SGP_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipsea) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_09_check_ipsea <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipsea
#Thailand
#00-09
#short-run (check ipc)
irf_THA_VAR_diff_09_check_ipsea <- irf(THA_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipsea) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_09_check_ipsea <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using aggregate cooperation PRIs, 2010-2019)
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(coop_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfpri),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run (check reer)
irf_IDN_VAR_diff_19_check_reer <- irf(IDN_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_reer) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_19_check_reer <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_reer
#Malaysia
#10-19
MYS_VAR_19 <- cbind.data.frame(coop_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfpri),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run (check reer)
irf_MYS_VAR_diff_19_check_reer <- irf(MYS_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_reer) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_19_check_reer <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_reer
#Philippines
#10-19
PHL_VAR_19 <- cbind.data.frame(coop_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfpri),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run (check reer)
irf_PHL_VAR_diff_19_check_reer <- irf(PHL_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_reer) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_19_check_reer <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_reer
#Singapore
#10-19
SGP_VAR_19 <- cbind.data.frame(coop_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfpri),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run (check reer)
irf_SGP_VAR_diff_19_check_reer <- irf(SGP_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_reer) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_19_check_reer <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_reer
#Thailand
#10-19
THA_VAR_19 <- cbind.data.frame(coop_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfpri),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run (check reer)
irf_THA_VAR_diff_19_check_reer <- irf(THA_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_reer) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_19_check_reer <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_reer
#Vietnam
#10-19
VNM_VAR_19 <- cbind.data.frame(coop_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfpri),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run (check reer)
irf_VNM_VAR_diff_19_check_reer <- irf(VNM_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_reer) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_19_check_reer <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_reer

#shock in China's industrial production series (using aggregate cooperation PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipc)
irf_IDN_VAR_diff_19_check_ipc <- irf(IDN_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipc) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_19_check_ipc <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipc
#Malaysia
#10-19
#short-run (check ipc)
irf_MYS_VAR_diff_19_check_ipc <- irf(MYS_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipc) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_19_check_ipc <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipc
#Philippines
#10-19
#short-run (check ipc)
irf_PHL_VAR_diff_19_check_ipc <- irf(PHL_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipc) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_19_check_ipc <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipc
#Singapore
#10-19
#short-run (check ipc)
irf_SGP_VAR_diff_19_check_ipc <- irf(SGP_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipc) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_19_check_ipc <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipc
#Thailand
#10-19
#short-run (check ipc)
irf_THA_VAR_diff_19_check_ipc <- irf(THA_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipc) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_19_check_ipc <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipc
#Vietnam
#10-19
#short-run (check ipc)
irf_VNM_VAR_diff_19_check_ipc <- irf(VNM_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipc) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_19_check_ipc <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate cooperation PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipsea)
irf_IDN_VAR_diff_19_check_ipsea <- irf(IDN_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipsea) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_19_check_ipsea <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipsea
#Malaysia
#10-19
#short-run (check ipsea)
irf_MYS_VAR_diff_19_check_ipsea <- irf(MYS_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipsea) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_19_check_ipsea <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipsea
#Philippines
#10-19
#short-run (check ipsea)
irf_PHL_VAR_diff_19_check_ipsea <- irf(PHL_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipsea) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_19_check_ipsea <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipsea
#Singapore
#10-19
#short-run (check ipsea)
irf_SGP_VAR_diff_19_check_ipsea <- irf(SGP_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipsea) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_19_check_ipsea <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipsea
#Thailand
#10-19
#short-run (check ipsea)
irf_THA_VAR_diff_19_check_ipsea <- irf(THA_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipsea) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_19_check_ipsea <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipsea
#Vietnam
#10-19
#short-run (check ipsea)
irf_VNM_VAR_diff_19_check_ipsea <- irf(VNM_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipsea) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_19_check_ipsea <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using aggregate conflict PRIs, 2000-2019)
#Indonesia
#00-19
IDN_VAR <- cbind.data.frame(conflict_PRI_IDN$logtfPRI, reer_ALL$log_reer[1:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[1:240])
colnames(IDN_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff <- cbind.data.frame(diff(IDN_VAR$tfpri),diff(IDN_VAR$reer),diff(IDN_VAR$exports),diff(IDN_VAR$ipc),diff(IDN_VAR$ipsea))
colnames(IDN_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff <- na.omit(IDN_VAR_diff)
VARselect(IDN_VAR_diff)
IDN_VAR_diff <- VAR(IDN_VAR_diff,p = 1, type = "const")
summary(IDN_VAR_diff)
#short-run (check reer)
irf_IDN_VAR_diff_check_reer <- irf(IDN_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_reer) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_check_reer <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_reer
#Malaysia
#00-19
MYS_VAR <- cbind.data.frame(conflict_PRI_MYS$logtfPRI, reer_ALL$log_reer[241:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[241:480])
colnames(MYS_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff <- cbind.data.frame(diff(MYS_VAR$tfpri),diff(MYS_VAR$reer),diff(MYS_VAR$exports),diff(MYS_VAR$ipc),diff(MYS_VAR$ipsea))
colnames(MYS_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff <- na.omit(MYS_VAR_diff)
VARselect(MYS_VAR_diff)
MYS_VAR_diff <- VAR(MYS_VAR_diff,p = 1, type = "const")
summary(MYS_VAR_diff)
#short-run (check reer)
irf_MYS_VAR_diff_check_reer <- irf(MYS_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_reer) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_check_reer <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_reer
#Philippines
#00-19
PHL_VAR <- cbind.data.frame(conflict_PRI_PHL$logtfPRI, reer_ALL$log_reer[481:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[481:720])
colnames(PHL_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff <- cbind.data.frame(diff(PHL_VAR$tfpri),diff(PHL_VAR$reer),diff(PHL_VAR$exports),diff(PHL_VAR$ipc),diff(PHL_VAR$ipsea))
colnames(PHL_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff <- na.omit(PHL_VAR_diff)
VARselect(PHL_VAR_diff)
PHL_VAR_diff <- VAR(PHL_VAR_diff,p = 1, type = "const")
summary(PHL_VAR_diff)
#short-run (check reer)
irf_PHL_VAR_diff_check_reer <- irf(PHL_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_reer) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_check_reer <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_reer
#Singapore
#00-19
SGP_VAR <- cbind.data.frame(conflict_PRI_SGP$logtfPRI, reer_ALL$log_reer[721:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[721:960])
colnames(SGP_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff <- cbind.data.frame(diff(SGP_VAR$tfpri),diff(SGP_VAR$reer),diff(SGP_VAR$exports),diff(SGP_VAR$ipc),diff(SGP_VAR$ipsea))
colnames(SGP_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff <- na.omit(SGP_VAR_diff)
VARselect(SGP_VAR_diff)
SGP_VAR_diff <- VAR(SGP_VAR_diff,p = 1, type = "const")
summary(SGP_VAR_diff)
#short-run (check reer)
irf_SGP_VAR_diff_check_reer <- irf(SGP_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_reer) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_check_reer <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_reer
#Thailand
#00-19
THA_VAR <- cbind.data.frame(conflict_PRI_THA$logtfPRI, reer_ALL$log_reer[961:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen, industrial_production_ALL$log_IIP[1345:1584],industrial_production_ALL$log_IIP[961:1200])
colnames(THA_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff <- cbind.data.frame(diff(THA_VAR$tfpri),diff(THA_VAR$reer),diff(THA_VAR$exports),diff(THA_VAR$ipc),diff(THA_VAR$ipsea))
colnames(THA_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff <- na.omit(THA_VAR_diff)
VARselect(THA_VAR_diff)
THA_VAR_diff <- VAR(THA_VAR_diff,p = 1, type = "const")
summary(THA_VAR_diff)
#short-run (check reer)
irf_THA_VAR_diff_check_reer <- irf(THA_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_reer) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_check_reer <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_reer
#Vietnam
#08-19
VNM_VAR <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[97:240], reer_ALL$log_reer[1201:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen, industrial_production_ALL$log_IIP[1441:1584],industrial_production_ALL$log_IIP[1201:1344])
colnames(VNM_VAR) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff <- cbind.data.frame(diff(VNM_VAR$tfpri),diff(VNM_VAR$reer),diff(VNM_VAR$exports),diff(VNM_VAR$ipc),diff(VNM_VAR$ipsea))
colnames(VNM_VAR_diff) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff <- na.omit(VNM_VAR_diff)
VARselect(VNM_VAR_diff)
VNM_VAR_diff <- VAR(VNM_VAR_diff,p = 1, type = "const")
summary(VNM_VAR_diff)
#short-run (check reer)
irf_VNM_VAR_diff_check_reer <- irf(VNM_VAR_diff, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_reer) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["irf"]][["diff_reer"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_check_reer <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_reer

#shock in China's industrial production series (using aggregate conflict PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipc)
irf_IDN_VAR_diff_check_ipc <- irf(IDN_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipc) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_check_ipc <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipc
#Malaysia
#00-19
#short-run (check ipc)
irf_MYS_VAR_diff_check_ipc <- irf(MYS_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipc) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_check_ipc <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipc
#Philippines
#00-19
#short-run (check ipc)
irf_PHL_VAR_diff_check_ipc <- irf(PHL_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipc) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_check_ipc <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipc
#Singapore
#00-19
#short-run (check ipc)
irf_SGP_VAR_diff_check_ipc <- irf(SGP_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipc) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_check_ipc <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipc
#Thailand
#00-19
#short-run (check ipc)
irf_THA_VAR_diff_check_ipc <- irf(THA_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipc) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_check_ipc <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipc
#Vietnam
#00-19
#short-run (check ipc)
irf_VNM_VAR_diff_check_ipc <- irf(VNM_VAR_diff, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipc) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_check_ipc <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate conflict PRIs, 2000-2019)
#Indonesia
#00-19
#short-run (check ipsea)
irf_IDN_VAR_diff_check_ipsea <- irf(IDN_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_check_ipsea) + title(main = "Indonesia")
lower_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN <- as.data.frame(t(irf_IDN_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_check_ipsea <- rbind.data.frame(lower_IDN*100,irf_IDN*100,upper_IDN*100)
effects_IDN_check_ipsea
#Malaysia
#00-19
#short-run (check ipsea)
irf_MYS_VAR_diff_check_ipsea <- irf(MYS_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_check_ipsea) + title(main = "Malaysia")
lower_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS <- as.data.frame(t(irf_MYS_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_check_ipsea <- rbind.data.frame(lower_MYS*100,irf_MYS*100,upper_MYS*100)
effects_MYS_check_ipsea
#Philippines
#00-19
#short-run (check ipsea)
irf_PHL_VAR_diff_check_ipsea <- irf(PHL_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_check_ipsea) + title(main = "Philippines")
lower_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL <- as.data.frame(t(irf_PHL_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_check_ipsea <- rbind.data.frame(lower_PHL*100,irf_PHL*100,upper_PHL*100)
effects_PHL_check_ipsea
#Singapore
#00-19
#short-run (check ipsea)
irf_SGP_VAR_diff_check_ipsea <- irf(SGP_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_check_ipsea) + title(main = "Singapore")
lower_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP <- as.data.frame(t(irf_SGP_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_check_ipsea <- rbind.data.frame(lower_SGP*100,irf_SGP*100,upper_SGP*100)
effects_SGP_check_ipsea
#Thailand
#00-19
#short-run (check ipsea)
irf_THA_VAR_diff_check_ipsea <- irf(THA_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_check_ipsea) + title(main = "Thailand")
lower_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA <- as.data.frame(t(irf_THA_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_check_ipsea <- rbind.data.frame(lower_THA*100,irf_THA*100,upper_THA*100)
effects_THA_check_ipsea
#Vietnam
#00-19
#short-run (check ipsea)
irf_VNM_VAR_diff_check_ipsea <- irf(VNM_VAR_diff, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_check_ipsea) + title(main = "Vietnam")
lower_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM <- as.data.frame(t(irf_VNM_VAR_diff_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_check_ipsea <- rbind.data.frame(lower_VNM*100,irf_VNM*100,upper_VNM*100)
effects_VNM_check_ipsea

#shock in each of the six Southeast Asian countries' REER series (using aggregate conflict PRIs, 2000-2019)
#Indonesia
#00-09
IDN_VAR_09 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[1:120], reer_ALL$log_reer[1:120], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[1:120])
colnames(IDN_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_09 <- cbind.data.frame(diff(IDN_VAR_09$tfpri),diff(IDN_VAR_09$reer),diff(IDN_VAR_09$exports),diff(IDN_VAR_09$ipc),diff(IDN_VAR_09$ipsea))
colnames(IDN_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_09 <- na.omit(IDN_VAR_diff_09)
VARselect(IDN_VAR_diff_09)
IDN_VAR_diff_09 <- VAR(IDN_VAR_diff_09,p = 1, type = "const")
summary(IDN_VAR_diff_09)
#short-run (check reer)
irf_IDN_VAR_diff_09_check_reer <- irf(IDN_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_reer) + title(main = "Indonesia")
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_09_check_reer <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
effects_IDN_09_check_reer
#Philippines
#00-09
PHL_VAR_09 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[1:120], reer_ALL$log_reer[481:600], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[481:600])
colnames(PHL_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_09 <- cbind.data.frame(diff(PHL_VAR_09$tfpri),diff(PHL_VAR_09$reer),diff(PHL_VAR_09$exports),diff(PHL_VAR_09$ipc),diff(PHL_VAR_09$ipsea))
colnames(PHL_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_09 <- na.omit(PHL_VAR_diff_09)
VARselect(PHL_VAR_diff_09)
PHL_VAR_diff_09 <- VAR(PHL_VAR_diff_09,p = 1, type = "const")
summary(PHL_VAR_diff_09)
#short-run (check reer)
irf_PHL_VAR_diff_09_check_reer <- irf(PHL_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_reer) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_09_check_reer <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_reer
#Singapore
#00-09
SGP_VAR_09 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[1:120], reer_ALL$log_reer[721:840], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[721:840])
colnames(SGP_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_09 <- cbind.data.frame(diff(SGP_VAR_09$tfpri),diff(SGP_VAR_09$reer),diff(SGP_VAR_09$exports),diff(SGP_VAR_09$ipc),diff(SGP_VAR_09$ipsea))
colnames(SGP_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_09 <- na.omit(SGP_VAR_diff_09)
VARselect(SGP_VAR_diff_09)
SGP_VAR_diff_09 <- VAR(SGP_VAR_diff_09,p = 1, type = "const")
summary(SGP_VAR_diff_09)
#short-run (check reer)
irf_SGP_VAR_diff_09_check_reer <- irf(SGP_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_reer) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_09_check_reer <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_reer
#Thailand
#00-09
THA_VAR_09 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[1:120], reer_ALL$log_reer[961:1080], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[1:120], industrial_production_ALL$log_IIP[1345:1464],industrial_production_ALL$log_IIP[961:1080])
colnames(THA_VAR_09) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_09 <- cbind.data.frame(diff(THA_VAR_09$tfpri),diff(THA_VAR_09$reer),diff(THA_VAR_09$exports),diff(THA_VAR_09$ipc),diff(THA_VAR_09$ipsea))
colnames(THA_VAR_diff_09) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_09 <- na.omit(THA_VAR_diff_09)
VARselect(THA_VAR_diff_09)
THA_VAR_diff_09 <- VAR(THA_VAR_diff_09,p = 1, type = "const")
summary(THA_VAR_diff_09)
#short-run (check reer)
irf_THA_VAR_diff_09_check_reer <- irf(THA_VAR_diff_09, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_reer) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["irf"]][["diff_reer"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_09_check_reer <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_reer

#shock in China's industrial production series (using aggregate conflict PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipc)
irf_IDN_VAR_diff_09_check_ipc <- irf(IDN_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipc, main="Indonesia (IIP China shock)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
irf_IDN_VAR_diff_09_check_ipc <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipc
#Philippines
#00-09
#short-run (check ipc)
irf_PHL_VAR_diff_09_check_ipc <- irf(PHL_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipc) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_09_check_ipc <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipc
#Singapore
#00-09
#short-run (check ipc)
irf_SGP_VAR_diff_09_check_ipc <- irf(SGP_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipc) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_09_check_ipc <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipc
#Thailand
#00-09
#short-run (check ipc)
irf_THA_VAR_diff_09_check_ipc <- irf(THA_VAR_diff_09, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipc) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_09_check_ipc <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate conflict PRIs, 2000-2009)
#Indonesia
#00-09
#short-run (check ipsea)
irf_IDN_VAR_diff_09_check_ipsea <- irf(IDN_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_09_check_ipsea, main="Indonesia (IIP Indonesia)", ylab="", sub="months post IIP shock", ylim=c(-0.04,0.04), cex=2.5, cex.axis = 2, cex.lab=2.5)
lower_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_09 <- as.data.frame(t(irf_IDN_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
irf_IDN_VAR_diff_09_check_ipsea <- rbind.data.frame(lower_IDN_09*100,irf_IDN_09*100,upper_IDN_09*100)
irf_IDN_VAR_diff_09_check_ipsea
#Philippines
#00-09
#short-run (check ipsea)
irf_PHL_VAR_diff_09_check_ipsea <- irf(PHL_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_09_check_ipsea) + title(main = "Philippines")
lower_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_09 <- as.data.frame(t(irf_PHL_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_09_check_ipsea <- rbind.data.frame(lower_PHL_09*100,irf_PHL_09*100,upper_PHL_09*100)
effects_PHL_09_check_ipsea
#Singapore
#00-09
#short-run (check ipsea)
irf_SGP_VAR_diff_09_check_ipsea <- irf(SGP_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_09_check_ipsea) + title(main = "Singapore")
lower_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_09 <- as.data.frame(t(irf_SGP_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_09_check_ipsea <- rbind.data.frame(lower_SGP_09*100,irf_SGP_09*100,upper_SGP_09*100)
effects_SGP_09_check_ipsea
#Thailand
#00-09
#short-run (check ipsea)
irf_THA_VAR_diff_09_check_ipsea <- irf(THA_VAR_diff_09, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_09_check_ipsea) + title(main = "Thailand")
lower_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_09 <- as.data.frame(t(irf_THA_VAR_diff_09_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_09_check_ipsea <- rbind.data.frame(lower_THA_09*100,irf_THA_09*100,upper_THA_09*100)
effects_THA_09_check_ipsea

#shock in each of the six Southeast Asian countries REER series (using aggregate conflict PRIs, 2010-2019)
#Indonesia
#10-19
IDN_VAR_19 <- cbind.data.frame(conflict_PRI_IDN$logtfPRI[121:240], reer_ALL$log_reer[121:240], adj_exports_IDN_df$adjusted_CHN_IDN_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[121:240])
colnames(IDN_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
IDN_VAR_diff_19 <- cbind.data.frame(diff(IDN_VAR_19$tfpri),diff(IDN_VAR_19$reer),diff(IDN_VAR_19$exports),diff(IDN_VAR_19$ipc),diff(IDN_VAR_19$ipsea))
colnames(IDN_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
IDN_VAR_diff_19 <- na.omit(IDN_VAR_diff_19)
VARselect(IDN_VAR_diff_19)
IDN_VAR_diff_19 <- VAR(IDN_VAR_diff_19,p = 1, type = "const")
summary(IDN_VAR_diff_19)
#short-run (check reer)
irf_IDN_VAR_diff_19_check_reer <- irf(IDN_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_reer) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_IDN_19_check_reer <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_reer
#Malaysia
#10-19
MYS_VAR_19 <- cbind.data.frame(conflict_PRI_MYS$logtfPRI[121:240], reer_ALL$log_reer[361:480], adj_exports_MYS_df$adjusted_CHN_MYS_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[361:480])
colnames(MYS_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
MYS_VAR_diff_19 <- cbind.data.frame(diff(MYS_VAR_19$tfpri),diff(MYS_VAR_19$reer),diff(MYS_VAR_19$exports),diff(MYS_VAR_19$ipc),diff(MYS_VAR_19$ipsea))
colnames(MYS_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
MYS_VAR_diff_19 <- na.omit(MYS_VAR_diff_19)
VARselect(MYS_VAR_diff_19)
MYS_VAR_diff_19 <- VAR(MYS_VAR_diff_19,p = 1, type = "const")
summary(MYS_VAR_diff_19)
#short-run (check reer)
irf_MYS_VAR_diff_19_check_reer <- irf(MYS_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_reer) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_MYS_19_check_reer <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_reer
#Philippines
#10-19
PHL_VAR_19 <- cbind.data.frame(conflict_PRI_PHL$logtfPRI[121:240], reer_ALL$log_reer[601:720], adj_exports_PHL_df$adjusted_CHN_PHL_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[601:720])
colnames(PHL_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
PHL_VAR_diff_19 <- cbind.data.frame(diff(PHL_VAR_19$tfpri),diff(PHL_VAR_19$reer),diff(PHL_VAR_19$exports),diff(PHL_VAR_19$ipc),diff(PHL_VAR_19$ipsea))
colnames(PHL_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
PHL_VAR_diff_19 <- na.omit(PHL_VAR_diff_19)
VARselect(PHL_VAR_diff_19)
PHL_VAR_diff_19 <- VAR(PHL_VAR_diff_19,p = 1, type = "const")
summary(PHL_VAR_diff_19)
#short-run (check reer)
irf_PHL_VAR_diff_19_check_reer <- irf(PHL_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_reer) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_PHL_19_check_reer <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_reer
#Singapore
#10-19
SGP_VAR_19 <- cbind.data.frame(conflict_PRI_SGP$logtfPRI[121:240], reer_ALL$log_reer[841:960], adj_exports_SGP_df$adjusted_CHN_SGP_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[841:960])
colnames(SGP_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
SGP_VAR_diff_19 <- cbind.data.frame(diff(SGP_VAR_19$tfpri),diff(SGP_VAR_19$reer),diff(SGP_VAR_19$exports),diff(SGP_VAR_19$ipc),diff(SGP_VAR_19$ipsea))
colnames(SGP_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
SGP_VAR_diff_19 <- na.omit(SGP_VAR_diff_19)
VARselect(SGP_VAR_diff_19)
SGP_VAR_diff_19 <- VAR(SGP_VAR_diff_19,p = 1, type = "const")
summary(SGP_VAR_diff_19)
#short-run (check reer)
irf_SGP_VAR_diff_19_check_reer <- irf(SGP_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_reer) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_SGP_19_check_reer <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_reer
#Thailand
#10-19
THA_VAR_19 <- cbind.data.frame(conflict_PRI_THA$logtfPRI[121:240], reer_ALL$log_reer[1081:1200], adj_exports_THA_df$adjusted_CHN_THA_ExpSen[121:240], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1081:1200])
colnames(THA_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
THA_VAR_diff_19 <- cbind.data.frame(diff(THA_VAR_19$tfpri),diff(THA_VAR_19$reer),diff(THA_VAR_19$exports),diff(THA_VAR_19$ipc),diff(THA_VAR_19$ipsea))
colnames(THA_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
THA_VAR_diff_19 <- na.omit(THA_VAR_diff_19)
VARselect(THA_VAR_diff_19)
THA_VAR_diff_19 <- VAR(THA_VAR_diff_19,p = 1, type = "const")
summary(THA_VAR_diff_19)
#short-run (check reer)
irf_THA_VAR_diff_19_check_reer <- irf(THA_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_reer) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_THA_19_check_reer <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_reer
#Vietnam
#10-19
VNM_VAR_19 <- cbind.data.frame(conflict_PRI_VNM$logtfPRI[121:144], reer_ALL$log_reer[1225:1344], adj_exports_VNM_df$adjusted_CHN_VNM_ExpSen[121:144], industrial_production_ALL$log_IIP[1465:1584],industrial_production_ALL$log_IIP[1225:1344])
colnames(VNM_VAR_19) <- c("tfpri","reer","exports","ipc","ipsea")
VNM_VAR_diff_19 <- cbind.data.frame(diff(VNM_VAR_19$tfpri),diff(VNM_VAR_19$reer),diff(VNM_VAR_19$exports),diff(VNM_VAR_19$ipc),diff(VNM_VAR_19$ipsea))
colnames(VNM_VAR_diff_19) <- c("diff_tfpri","diff_reer","diff_exports","diff_ipc","diff_ipsea")
VNM_VAR_diff_19 <- na.omit(VNM_VAR_diff_19)
VARselect(VNM_VAR_diff_19)
VNM_VAR_diff_19 <- VAR(VNM_VAR_diff_19,p = 1, type = "const")
summary(VNM_VAR_diff_19)
#short-run (check reer)
irf_VNM_VAR_diff_19_check_reer <- irf(VNM_VAR_diff_19, impulse = "diff_reer", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_reer) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Lower"]][["diff_reer"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["irf"]][["diff_reer"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_reer[["Upper"]][["diff_reer"]]))
effects_VNM_19_check_reer <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_reer

#shock in China's industrial production series (using aggregate conflict PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipc)
irf_IDN_VAR_diff_19_check_ipc <- irf(IDN_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipc) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_IDN_19_check_ipc <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipc
#Malaysia
#10-19
#short-run (check ipc)
irf_MYS_VAR_diff_19_check_ipc <- irf(MYS_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipc) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_MYS_19_check_ipc <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipc
#Philippines
#10-19
#short-run (check ipc)
irf_PHL_VAR_diff_19_check_ipc <- irf(PHL_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipc) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_PHL_19_check_ipc <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipc
#Singapore
#10-19
#short-run (check ipc)
irf_SGP_VAR_diff_19_check_ipc <- irf(SGP_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipc) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_SGP_19_check_ipc <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipc
#Thailand
#10-19
#short-run (check ipc)
irf_THA_VAR_diff_19_check_ipc <- irf(THA_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipc) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_THA_19_check_ipc <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipc
#Vietnam
#10-19
#short-run (check ipc)
irf_VNM_VAR_diff_19_check_ipc <- irf(VNM_VAR_diff_19, impulse = "diff_ipc", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipc) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Lower"]][["diff_ipc"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["irf"]][["diff_ipc"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipc[["Upper"]][["diff_ipc"]]))
effects_VNM_19_check_ipc <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipc

#shock in each of the six Southeast Asian countries' industrial production series (using aggregate conflict PRIs, 2010-2019)
#Indonesia
#10-19
#short-run (check ipsea)
irf_IDN_VAR_diff_19_check_ipsea <- irf(IDN_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_IDN_VAR_diff_19_check_ipsea) + title(main = "Indonesia")
lower_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_IDN_19 <- as.data.frame(t(irf_IDN_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_IDN_19_check_ipsea <- rbind.data.frame(lower_IDN_19*100,irf_IDN_19*100,upper_IDN_19*100)
effects_IDN_19_check_ipsea
#Malaysia
#10-19
#short-run (check ipsea)
irf_MYS_VAR_diff_19_check_ipsea <- irf(MYS_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_MYS_VAR_diff_19_check_ipsea) + title(main = "Malaysia")
lower_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_MYS_19 <- as.data.frame(t(irf_MYS_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_MYS_19_check_ipsea <- rbind.data.frame(lower_MYS_19*100,irf_MYS_19*100,upper_MYS_19*100)
effects_MYS_19_check_ipsea
#Philippines
#10-19
#short-run (check ipsea)
irf_PHL_VAR_diff_19_check_ipsea <- irf(PHL_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_PHL_VAR_diff_19_check_ipsea) + title(main = "Philippines")
lower_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_PHL_19 <- as.data.frame(t(irf_PHL_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_PHL_19_check_ipsea <- rbind.data.frame(lower_PHL_19*100,irf_PHL_19*100,upper_PHL_19*100)
effects_PHL_19_check_ipsea
#Singapore
#10-19
#short-run (check ipsea)
irf_SGP_VAR_diff_19_check_ipsea <- irf(SGP_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_SGP_VAR_diff_19_check_ipsea) + title(main = "Singapore")
lower_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_SGP_19 <- as.data.frame(t(irf_SGP_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_SGP_19_check_ipsea <- rbind.data.frame(lower_SGP_19*100,irf_SGP_19*100,upper_SGP_19*100)
effects_SGP_19_check_ipsea
#Thailand
#10-19
#short-run (check ipsea)
irf_THA_VAR_diff_19_check_ipsea <- irf(THA_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_THA_VAR_diff_19_check_ipsea) + title(main = "Thailand")
lower_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_THA_19 <- as.data.frame(t(irf_THA_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_THA_19_check_ipsea <- rbind.data.frame(lower_THA_19*100,irf_THA_19*100,upper_THA_19*100)
effects_THA_19_check_ipsea
#Vietnam
#10-19
#short-run (check ipsea)
irf_VNM_VAR_diff_19_check_ipsea <- irf(VNM_VAR_diff_19, impulse = "diff_ipsea", response = "diff_exports", ortho = TRUE, n.ahead = 12, ci = 0.90)
plot(irf_VNM_VAR_diff_19_check_ipsea) + title(main = "Vietnam")
lower_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Lower"]][["diff_ipsea"]]))
irf_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["irf"]][["diff_ipsea"]]))
upper_VNM_19 <- as.data.frame(t(irf_VNM_VAR_diff_19_check_ipsea[["Upper"]][["diff_ipsea"]]))
effects_VNM_19_check_ipsea <- rbind.data.frame(lower_VNM_19*100,irf_VNM_19*100,upper_VNM_19*100)
effects_VNM_19_check_ipsea