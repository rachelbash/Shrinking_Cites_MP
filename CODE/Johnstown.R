#################################################################################################################################################
# Scripts for Official Statements from Bond Documents
#################################################################################################################################################

# Load libraries
library(trend); library(calibrate); library(lubridate); library(reshape2)
library(rjson); library(dplyr); library(magrittr);  library(tidyr)
library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
library(stringr); library(PBSmapping); library(spData); library(sf)
library(readxl); library(tidyverse); #in tidyverse


rm(list=ls()) #removes anything stored in memory
#################################################################################################################################################

#################################################################################################################################################
# Load Data for Basic Info Sheet
#################################################################################################################################################
swd_osData <- "C:/Users/19524/Documents/DUKE/MP/Shrinking_Cities_MP/DATA/RAW/"
fileName <- "os_PA4110034_Johnstown.xlsx"

#################################################################################################################################################
# Basic Info spreadsheet
#################################################################################################################################################
basicInfo <- read_excel(paste0(swd_osData, fileName), sheet="basicInfo")
#basicInfo$startYear <- as.Date(basicInfo$startYear, origin = "1970-01-01", format="Y")

#plot bond value and start year

plot(basicInfo$startYear, basicInfo$bondAmount, pch=19, col="navy", cex=1.8, xlab="Bond Year", ylab="Bond Value (Millions)")
#group by year and plot
group.bonds <- basicInfo %>% group_by(OSYear) %>% summarize(TotalValue = sum(bondAmount, na.rm=TRUE)) %>% as.data.frame();
plot(group.bonds$OSYear, (group.bonds$TotalValue/1000000), pch=19, col="navy", cex=1.8, xlab="Bond Year", ylab="Bond Value (Millions)")

all.years <- seq(min(basicInfo$OSYear), max(basicInfo$OSYear),1) %>% as.data.frame(); colnames(all.years)=c("OSYear")
all.years$TotalValue=0
group.bonds <- rbind(all.years, group.bonds); 
group.bonds <- group.bonds %>% group_by(OSYear) %>% summarize(TotalValue = sum(TotalValue, na.rm=TRUE)) %>% as.data.frame();

par(mar=c(4,4,3,1))  #par(mar = c(bottom, left, top, right))
barplot((group.bonds$TotalValue/1000000), names.arg=group.bonds$OSYear, main="Bonds with Official Statements", xlab="Bond Year", ylab="Total Bond Value ($Millions)", ylim=c(0,25))
#################################################################################################################################################


#################################################################################################################################################
# other Debt spreadsheet
#################################################################################################################################################
otherDebt <- read_excel(paste0(swd_osData, fileName), sheet="otherDebt"); head(otherDebt)
uniqueDebt <- otherDebt %>% distinct(debtName, .keep_all = TRUE)  %>% as.data.frame() 
  uniqueDebt <- uniqueDebt %>% dplyr::select(PWSID, debtName, type, amount, aveRate, startYear, endYear)
  
#plot type of loan and amount - combine bond series
typeDebt <- uniqueDebt %>%
  group_by(startYear, type) %>%
  summarise(totalamount = sum(amount),
            avgrate = mean(aveRate)
            )

ggplot(typeDebt, aes(x = startYear, y= totalamount/1000000, color = type, fill = type)) +
  geom_bar(stat = "identity")
  
  
  

#################################################################################################################################################
# Percent of Treated and Metered Water to Supply Capacity
#################################################################################################################################################
usage <- read_excel(paste0(swd_osData, fileName), sheet="usage"); head(usage)
capacity <- usage %>% filter(groupBy=="Capacity" & class=="Total") %>% group_by(year) %>% 
  summarize(capVol_MGD = median(volume_MGD, na.rm=TRUE))
treated <- usage %>% filter(groupBy=="Treated Water" & class=="Total") %>% group_by(year) %>% 
  summarize(treatVol_MGD = median(volume_MGD, na.rm=TRUE))
metered <- usage %>% filter(groupBy=="Metered Consumption") %>% group_by(year, class) %>% 
  summarize(meterVol_MGD = median(volume_MGD, na.rm=TRUE))
#combine total into one dataframe to get percent
totalUse <- merge(capacity, treated, by.x="year", by.y="year", all=TRUE)
totalUse <- merge(totalUse, subset(metered, class=="Total"), by.x="year", by.y="year", all=TRUE)
totalUse$treatPer <- round(totalUse$treatVol_MGD/totalUse$capVol_MGD*100,2)
totalUse$meterPer <- round(totalUse$meterVol_MGD/totalUse$capVol_MGD*100,2)


#Plot capacity, treated, and metered over time
par(mar=c(2,4,3,1))  #par(mar = c(bottom, left, top, right))
plot(totalUse$year, totalUse$capVol_MGD, type="n", xlab="", ylab="Average Daily Volume (MGD)", main=paste0(usage$Name[1]," Water Supply and Usage Over Time"), ylim=c(0,max(totalUse$capVol_MGD+2)))
  lines(totalUse$year, totalUse$capVol_MGD, lwd=2, col="navy")
  lines(totalUse$year, totalUse$treatVol_MGD, lwd=2, col="blue")
  lines(totalUse$year, totalUse$meterVol_MGD, lwd=2, col="darkred")
legend("topleft", c("Capacity","Treated Water","Metered Water"), col=c("navy","blue","darkred"), lwd=2)

par(mar=c(2,4,3,1))  #par(mar = c(bottom, left, top, right))
plot(totalUse$year, totalUse$treatPer, type="n", xlab="", ylab="Percent of Total Capacity", main=paste0(usage$Name[1]," Water Supply and Usage Over Time"), ylim=c(0,100))
  lines(totalUse$year, totalUse$treatPer, lwd=2, col="blue")
  lines(totalUse$year, totalUse$meterPer, lwd=2, col="darkred")
  abline(h = 100, col="navy", lwd=2)
  abline(h = 80, col="red", lwd=3, lty=2)
legend("bottomleft", c("Capacity","Treated Water","Metered Water", "Demand is 80% of Supply"), col=c("navy","blue","darkred", "red"), lwd=2, lty=c(1,1,1,2))


#################################################################################################################################################
# Unaccounted Water Loss
#################################################################################################################################################
unaccount <- read_excel(paste0(swd_osData, fileName), sheet="unaccounted"); head(unaccount)
#adjusted percent column may be a character if first values are NA
unaccount$adjustedPercent <- as.numeric(as.character(unaccount$adjustedPercent))
#summarize for duplicate years with different values
unaccount <- unaccount %>% 
  group_by(year, method) %>% 
  summarize(GrossPer = median(grossPercent, na.rm=TRUE), 
            AdjustedPer = median(adjustedPercent, na.rm=TRUE)) %>% 
  as.data.frame()

#add missing years so doesn't draw a line between
all.years <- seq(min(unaccount$year), max(unaccount$year),1) %>% 
  as.data.frame(); colnames(all.years)=c("year")
  all.years$method = NA;       all.years$GrossPer=NA;           all.years$AdjustedPer=NA;
all.years <- rbind(unaccount, all.years); 
#get rid of duplicate years
unaccount2 <- all.years %>% 
    group_by(year) %>% 
    summarize(GrossPer = median(GrossPer, na.rm=TRUE), 
              AdjustedPer = median(AdjustedPer, na.rm=TRUE)) %>% 
    arrange(year)
unaccount <- merge(unaccount2, unaccount[,c(1:2)], by.x="year", by.y="year", all.x=TRUE)
#leak estimate 
unaccount$leakEst = unaccount$GrossPer - unaccount$AdjustedPer

unaccount$pch <- ifelse(unaccount$method=="Reported", 19, 1);    
unaccount$pch <- ifelse(is.na(unaccount$method)==TRUE, NA, unaccount$pch);
unaccount$col <- ifelse(unaccount$method=="Reported", "black", rgb(0.2,0.2,0.2));    
unaccount$col <- ifelse(is.na(unaccount$method)==TRUE, NA, unaccount$col);

par(mar=c(2,4,3,1))  #par(mar = c(bottom, left, top, right))
plot(unaccount$year, unaccount$GrossPer, type="n", xlab="", ylab="Percent of Treated Water", 
     main=paste0(usage$name[1]," Unaccounted Water Loss"), ylim=c(0,100), las=1) 
#las sets all labels horizontal
abline(h=c(10,20,30,40,50,60,70,80,90,100), lty=3, col="lightgray")
  lines(unaccount$year, unaccount$GrossPer, lwd=2, col="black");    
    points(unaccount$year, unaccount$GrossPer, pch=unaccount$pch, col=unaccount$col, cex=1.2)
  lines(unaccount$year, unaccount$AdjustedPer, lwd=2, col="goldenrod4");
  lines(unaccount$year, unaccount$leakEst, lwd=2, lty=2, col="red");    
legend("topleft", c("Estimated Gross Unaccounted Water","Reported Gross Unaccounted Water","Adjusted Unaccounted Water (known)", "Estimated Leaks"), 
       col=c("black","black","goldenrod4","red"), lwd=2, lty=c(1,1,1,2), pch=c(1,19,NA,NA), cex = 1.1)
  


#################################################################################################################################################
# Number of connections by customer class and revenue by customer class over time
#################################################################################################################################################
#read in data
customers <- read_excel(paste0(swd_osData, fileName), sheet="customers"); head(customers)
  customers$nConnections = as.numeric(as.character(customers$nConnections))
#remove duplicates and pull out only by customer type
custType <- customers %>% filter(GroupBy=="Customer") %>% group_by(Year, Class) %>% summarize(nConnections = median(nConnections, na.rm=TRUE))

#reshape the table
connects <- custType %>% spread(Class, value=nConnections)
#reorder dataframe
connects <- connects[c("Year", "Residential", "Industrial","Commercial","Public","Other Utilities")]

#plot customers over time
par(mar=c(2,5,3,1))  #par(mar = c(bottom, left, top, right))
barplot(t(as.matrix(connects[,c(2:6)])), names.arg=connects$Year, main=paste0(customers$Name[1],": Number of Connections by Customer Class"),
        col=c("navy","darkred","goldenrod4","lightblue","black"), ylim=c(0,25000), las=1)
  abline(h=0, col="black")
  mtext("Number of Connections", side=2, line=3.5)
  legend("topright",c("Residential", "Industrial","Commercial","Public","Other Utilities"), fill=c("navy","darkred","goldenrod4","lightblue","black"),
         cex=0.8, ncol=2)
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#read in fiscal data
fiscal <- read_excel(paste0(swd_osData, fileName), sheet="fiscal"); head(fiscal)
#fix first row issue
names(fiscal) <- as.matrix(fiscal[1, ])
fiscal <- fiscal[-1, ]
#convert from character to numeric
fiscal[,c(5:dim(fiscal)[2])] <- sapply(fiscal[c(5:dim(fiscal)[2])],as.numeric)
revenues <- fiscal %>% filter(Category=="Revenues")

names.revenue = seq(1988,2017,1)
#read in usage data and demand data
par(mar=c(2,4,3,1))  #par(mar = c(bottom, left, top, right))
#remove last row, which is the total
#create a bar plot of revenues over time
barplot((as.matrix(fiscal[c(1:dim(revenues)[1]-1),c(5:dim(revenues)[2])])/1000000), names.arg=names.revenue, main=paste0(fiscal$Name[1],": Operating Revenue Generated"),
        col=c("navy","goldenrod4","darkred","lightblue","black","yellow","yellow2","seagreen3","plum2","azure4","olivedrab"), ylim=c(0,12), las=1, ylab="Revenues ($Millions)")
abline(h=0, col="black")
legend("topleft",head(unique(revenues$SubCategory),-1), fill=c("navy","goldenrod4","darkred","lightblue","black","yellow","yellow2","seagreen3","plum2","azure4","olivedrab"),
       cex=0.8, ncol=2)

#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#read in rate data to get years of known rate changes
rates <- read_excel(paste0(swd_osData, fileName), sheet="rates"); head(rates)
rateYearSet <- unique(rates$yearSet)
#abline(v=rateYearSet, col="darkgray", lwd=2)  
#need to do this in plotly or another program to draw correctly
  
  
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#Plot total number of connections with total revenue generated
connects$Total = connects$Residential+connects$Industrial+connects$Commercial+connects$Public+connects$`Other Utilities`
  
classRevenue <- revenues %>% filter(SubCategory=="Residential" | SubCategory=="Industrial" | SubCategory=="Commercial" | 
                                        SubCategory=="Public Buildings / Public Authorities" | SubCategory=="Sales to other Water Authorities" |
                                      SubCategory=="Total Operating Revenues")
head(classRevenue)
gathRev <- classRevenue %>% gather(Year, Revenue, y1988:y2017)
gathRev$Year<- as.numeric(substr(gathRev$Year,2,5))
gathRev$Millions <- gathRev$Revenue/1000000

#subset categories
totRev <- subset(gathRev, SubCategory=="Total Operating Revenues")
resRev <- subset(gathRev, SubCategory=="Residential");                        indRev <- subset(gathRev, SubCategory=="Industrial");
comRev <- subset(gathRev, SubCategory=="Commercial");                         pubRev <- subset(gathRev, SubCategory=="Public Buildings / Public Authorities");
utilRev <- subset(gathRev, SubCategory=="Sales to other Water Authorities");

#plot customers over time
par(mar=c(2,5,3,4))  #par(mar = c(bottom, left, top, right))
plot(connects$Year, connects$Total, type="n", xlab="", ylab="", ylim=c(0,max(connects$Total)+1000), xlim=c(min(totRev$Year, connects$Year), 2020), xaxs="i", yaxs="i",
     main=paste0(usage$Name[1]," Total Connections & Revenue"), las=1) #las sets all labels horizontal
  mtext("Number of Connections", side=2, line=3.8)
  lines(connects$Year, connects$Total, lwd=2, col="black");    

par(new=TRUE)
plot(totRev$Year, totRev$Millions, type="n", axes=F, ylab="", xlab="", xlim=c(min(totRev$Year, connects$Year), 2020), xaxs="i", yaxs="i",
     ylim=c(0,max(totRev$Millions, na.rm=TRUE)+2)) #las sets all labels horizontal
  lines(totRev$Year, totRev$Millions, lwd=2, col="olivedrab4")
  abline(v=rateYearSet, col="darkgray", lty=3)  
  mtext("Total Operating Revenue ($Millions)", side=4, col="olivedrab4", line=2.5)
  axis(side = 4, col="olivedrab4", las=2, col.axis="olivedrab4")
legend("bottomright", c("Number of Connections","Operating Revenues","Year of Known Rate Change"), col=c("black","olivedrab4","darkgray"), lwd=c(2,2,1), lty=c(1,1,3))


#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#Plot Ave Dollar Per Connection
revCon.total <- merge(connects[,c("Year","Total")], totRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.total$PerConnect = revCon.total$Revenue/revCon.total$Total

revCon.res <- merge(connects[,c("Year","Residential")], resRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.res$PerConnect = revCon.res$Revenue/revCon.res$Residential

revCon.ind <- merge(connects[,c("Year","Industrial")], indRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.ind$PerConnect = revCon.ind$Revenue/revCon.ind$Industrial
  
revCon.com <- merge(connects[,c("Year","Commercial")], comRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.com$PerConnect = revCon.com$Revenue/revCon.com$Commercial

revCon.pub <- merge(connects[,c("Year","Public")], pubRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.pub$PerConnect = revCon.pub$Revenue/revCon.pub$Public
  
revCon.util <- merge(connects[,c("Year","Other Utilities")], utilRev[,c("Year","Revenue")], by.x="Year", by.y="Year", all=TRUE)
  revCon.util$PerConnect = revCon.util$Revenue/revCon.util$'Other Utilities'
  
    
#plot per connection costs over time
par(mar=c(2,5,3,1))  #par(mar = c(bottom, left, top, right))
plot(revCon.total$Year, revCon.total$PerConnect, type="n", xlab="", ylab="Revenue per Connection ($)", ylim=c(0,max(revCon.ind$PerConnect, na.rm=TRUE)), 
     xlim=c(min(revCon.total$Year), 2020), xaxs="i", yaxs="i",
       main=paste0(usage$Name[1]," Average Annual Cost Per Connection"), las=1) #las sets all labels horizontal
    lines(revCon.total$Year, revCon.total$PerConnect, lwd=4, col="black");        points(revCon.total$Year, revCon.total$PerConnect, pch=19, col="black");
    lines(revCon.res$Year, revCon.res$PerConnect, lwd=2, col="navy");             points(revCon.res$Year, revCon.res$PerConnect, pch=19, col="navy");
    lines(revCon.com$Year, revCon.com$PerConnect, lwd=2, col="goldenrod4");       points(revCon.com$Year, revCon.com$PerConnect, pch=19, col="goldenrod4");
    #par(new=TRUE)
    #plot(revCon.total$Year, revCon.total$PerConnect, type="n", axes=F, ylab="", xlab="", xlim=c(min(revCon.total$Year), 2020), xaxs="i", yaxs="i", ylim=c(0,7000)) #las sets all labels horizontal
    lines(revCon.ind$Year, revCon.ind$PerConnect, lwd=2, col="darkred");          points(revCon.ind$Year, revCon.ind$PerConnect, pch=19, col="darkred");
    lines(revCon.pub$Year, revCon.pub$PerConnect, lwd=2, col="lightblue");        points(revCon.pub$Year, revCon.pub$PerConnect, pch=19, col="lightblue");    
    abline(v=rateYearSet, col="darkgray", lty=3)  
#   mtext("Ave Annual Cost for Industrial & Public Classes", side=4, line=3.5)
#    axis(side = 4,  las=2)
legend("topleft",,c("Residential", "Industrial","Commercial","Public","Total"), col=c("navy","darkred","goldenrod4","lightblue","black"), lwd=c(2,2,2,2,4),
       cex=0.8, ncol=2)


#plot change in revenue by class over time
par(mar=c(2,5,3,1))  #par(mar = c(bottom, left, top, right))
plot(revCon.total$Year, revCon.total$Revenue/1000000, type="n", xlab="", ylab="Revenue ($Millions)", ylim=c(0,7), xlim=c(min(revCon.total$Year), 2020), xaxs="i", yaxs="i",
     main=paste0(usage$Name[1]," Revenue by Customer Class"), las=1) #las sets all labels horizontal
  #lines(revCon.total$Year, revCon.total$Revenue/1000000, lwd=4, col="black");
  lines(revCon.res$Year, revCon.res$Revenue/1000000, lwd=2, col="navy");
  lines(revCon.com$Year, revCon.com$Revenue/1000000, lwd=2, col="goldenrod4");
  lines(revCon.ind$Year, revCon.ind$Revenue/1000000, lwd=2, col="darkred");
  lines(revCon.pub$Year, revCon.pub$Revenue/1000000, lwd=2, col="lightblue");
  lines(revCon.util$Year, revCon.util$Revenue/1000000, lwd=2, col="purple");    
  abline(v=rateYearSet, col="darkgray", lty=3)  
  legend("topleft",,c("Residential", "Industrial","Commercial","Public","Other Utilities"), col=c("navy","darkred","goldenrod4","lightblue","purple"), lwd=2,
         cex=0.8, ncol=2)
  


#################################################################################################################################################
# Percent of Water and Revenue from Top 10 Customrs and Diversity of Customer Class
#################################################################################################################################################
#read in data
top10 <- read_excel(paste0(swd_osData, fileName), sheet="largestCust"); head(top10)
#convert to numeric
top10[,c("Gallons","Revenue","percentTotalGal","percentTotalRev")] <- sapply(top10[,c("Gallons","Revenue","percentTotalGal","percentTotalRev")],as.numeric)  

top10.type <- top10 %>% group_by(Year, Type) %>% summarize(nType=n(), sumPerGal = sum(percentTotalGal, na.rm=TRUE), sumPerRev = sum(percentTotalRev, na.rm=TRUE)) %>% as.data.frame()
#add missing years so doesn't draw a line between
all.years <- seq(min(top10$Year, na.rm=TRUE), max(top10$Year, na.rm=TRUE),1) %>% as.data.frame(); colnames(all.years)=c("Year")
  all.years$Type = "Total";       all.years$nType=NA;           all.years$sumPerGal=NA;    all.years$sumPerRev=NA;

top10.tot <- top10.type %>% filter(Type=="Total") %>% as.data.frame()
top10.tot <- rbind(top10.tot, all.years)
top10.tot <- top10.tot %>% group_by(Year, Type) %>% summarize(nType=median(nType, na.rm=TRUE), sumPerGal = median(sumPerGal, na.rm=TRUE), sumPerRev = median(sumPerRev, na.rm=TRUE))
top10.tot$sumPerGal <- ifelse(top10.tot$sumPerGal==0,NA,top10.tot$sumPerGal)

#plot total over time
par(mar=c(2,5,3,1))  #par(mar = c(bottom, left, top, right))
plot(top10.tot$Year, top10.tot$sumPerGal, type="n", xlab="", ylab="Percent of Total Revenue or Treated Water", ylim=c(0,50), xlim=c(min(top10.tot$Year), 2020), xaxs="i", yaxs="i",
     main=paste0(usage$Name[1]," Top 10 Customers Each Year"), las=1) #las sets all labels horizontal
  lines(top10.tot$Year, top10.tot$sumPerGal, lwd=2, col="blue");          points(top10.tot$Year, top10.tot$sumPerGal, pch=19, cex=1.2, col="blue")
  lines(top10.tot$Year, top10.tot$sumPerRev, lwd=2, col="olivedrab4");    points(top10.tot$Year, top10.tot$sumPerRev, pch=19, cex=1.2, col="olivedrab4")
legend("topright",c("Treated Water Usage","Revenue"), col=c("blue","olivedrab4"), pch=19, cex=1.2)


top10.type <- top10.type %>% filter(Type != "Total" & Year>1960)
  
top10.gal <- top10.type %>% dplyr::select(Year,Type,sumPerRev) %>% spread(Type, sumPerRev)
top10.gal[is.na(top10.gal)] <- 0
all.years <- seq(min(top10$Year), max(top10$Year),1) %>% as.data.frame(); colnames(all.years)=c("Year")
  all.years[c(2:dim(top10.gal)[2])]<-0;
  #keep only those years missing
    all.years <-all.years[!(all.years$Year %in% unique(top10.gal$Year)),]
    colnames(all.years) <- names(top10.gal)
top10.gal <- rbind(top10.gal, all.years)
top10.gal <- top10.gal %>% arrange(Year)


#plot customers over time
par(mar=c(2,5,3,1))  #par(mar = c(bottom, left, top, right))
barplot(t(as.matrix(top10.gal[,-1])), names.arg=top10.gal$Year, main=paste0(top10$Name[1],": Main Users by Type"), ylim=c(0,16), las=1, ylab="Percent of Revenue",
        col=c("navy","darkgreen","brown","red","darkred","goldenrod1", "darkgray","lightblue","salmon"))
abline(h=0, col="black")
legend("topright",c("Academic", "Country Club","Dairy Plant","Hopsital","Housing Authority","Laundry","Manufacturing","Municipality","Rehab Center"), 
       fill=c("navy","darkgreen","brown","red","darkred","goldenrod1", "darkgray","lightblue","salmon"), cex=0.9, ncol=2)

#Contribution from first rank
top1 <- top10 %>% group_by(Year) %>% filter(Revenue == max(Revenue, na.rm=TRUE)) %>% as.data.frame()
all.years <- seq(min(top10$Year, na.rm=TRUE), max(top10$Year, na.rm=TRUE),1) %>% as.data.frame(); colnames(all.years)=c("Year")
all.years <-all.years[!(all.years$Year %in% unique(top10$Year)),] %>% as.data.frame(); colnames(all.years)=c("Year")
all.years[c(2:dim(top10)[2])]<-NA;  
all.years <- all.years[,c("V7","V2","V3","V4","V5","V6","Year","V8","V9","V10","V11","V12","V13","V14")]
colnames(all.years) <- names(top10)

top1 <- rbind(top1, all.years) %>% arrange(Year)
plot(top1$Year, top1$percentTotalRev, pch=19,col="darkgray", cex=1.5, ylim=c(0,50), ylab="Contribution from Largest Revenue Source (%)", xlab="", main=paste0(top10$Name[1],": Largest Revenue Customer"))
lines(top1$Year, top1$percentTotalRev, lwd=3, col="black")


#Range of prices paid
top10$perkGal = top10$Revenue/top10$Gallons*1000
all.years <- seq(min(top10$Year, na.rm=TRUE), max(top10$Year, na.rm=TRUE),1) %>% as.data.frame(); colnames(all.years)=c("Year")
#keep only those years missing
all.years <- seq(min(top10$Year, na.rm=TRUE), max(top10$Year, na.rm=TRUE),1) %>% as.data.frame(); colnames(all.years)=c("Year")
  all.years <-all.years[!(all.years$Year %in% unique(top10$Year)),] %>% as.data.frame(); colnames(all.years)=c("Year")
  all.years[c(2:dim(top10)[2])]<-NA;  
  all.years <- all.years[,c("V7","V2","V3","V4","V5","V6","Year","V8","V9","V10","V11","V12","V13","V14","V15")]
colnames(all.years) <- names(top10)
top10.all <- rbind(top10 %>% as.data.frame(), all.years)
top10.all <- top10.all %>% filter(is.na(Year) == FALSE) %>% arrange(Year)

par(mar=c(2,4,3,1))  #par(mar = c(bottom, left, top, right))
boxplot(top10.all$perkGal~top10.all$Year, na.rm=TRUE,las=1, xaxs="i", yaxs="i", ylim=c(0,6), col="gray", ylab="Cost Per Thousand Gallons", 
        main=paste0(top10$Name[1],": Average Cost Per Thousand Gallons"))
        abline(h=0, lwd=2, col="black")

#no zero values
top10.nozero <- subset(top10.all, perkGal >0 & Type != "Total")
boxplot(top10.nozero$perkGal~top10.nozero$Type, na.rm=TRUE,las=1, xaxs="i", yaxs="i", ylim=c(0,6), col=c("navy","darkgreen","yellow4","red","darkred","goldenrod1", "darkgray","lightblue","salmon"),
        ylab="Cost Per Thousand Gallons", main=paste0(top10$Name[1],"Average Cost Per Thousand Gallons"), cex.axis=0.8)
legend("topleft",c("Academic", "Country Club","Dairy Plant","Hopsital","Housing Authority","Laundry","Manufacturing","Municipality","Rehab Center"), 
       fill=c("navy","darkgreen","yellow4","red","darkred","goldenrod1", "darkgray","lightblue","salmon"), cex=0.9, ncol=2)


top10.notot <- subset(top10, Customer != "Total")
zt <- table(top10.notot$Customer, top10.notot$Year) 
zt
grow_colors <- c("white", "blue", "midnightblue")
col_breaks = c(0,1,2,4)
heatmap(zt, Colv = NA, Rowv = NA, scale="column", col=grow_colors, breaks=col_breaks, main=paste0(top10$Name[1],": Blue = in top 10"))



#################################################################################################################################################
# RATE CHANGES OVER TIME
#################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#read in rate data to get years of known rate changes
rates <- read_excel(paste0(swd_osData, fileName), sheet="rates"); head(rates)
rateYearSet <- unique(rates$yearSet)

#Average Rate for someone with a 0.75 inch pipe that uses 5000 gallons a month
hhrates <- subset(rates, Charges=="Consumption Charge" & supplySystem=="Pumping" | is.na(supplySystem)==TRUE) %>% as.data.frame();  
hhmeter <- subset(rates, class=="0.75")

unique.year <- unique(hhrates$rateYear)
hh.quarter <- as.data.frame(matrix(nrow=length(unique.year),ncol=5)); colnames(hh.quarter) <- c("PWSID","Name","Year","Flat","cost15k")
hh.quarter$PWSID <- as.character(rates$PWSID[1])
hh.quarter$Name <- as.character(rates$Name[1])  
hh.quarter$Year <- unique.year

for(i in 1:length(unique.year)){
  zt.flat <- subset(hhmeter, rateYear==unique.year[i])$cost[1]
  
  if(unique.year[i]<=1998){
    zt.rates1 <- subset(hhrates, rateYear==unique.year[i] & class=="3000")$cost[1];       zt.rates2 <- subset(hhrates, rateYear==unique.year[i] & class=="6000")$cost[1];
    zt.rates3 <- subset(hhrates, rateYear==unique.year[i] & class=="20000")$cost[1]; 
    
    zt.quarter = zt.flat + zt.rates1*3 + zt.rates2*3 + zt.rates3*(15-6)
    hh.quarter[i,"Flat"] <- zt.flat;
    hh.quarter[i,"cost15k"] <- zt.quarter;
  }
  
  if(unique.year[i]>1998){
    #zt.rates1 <- subset(hhrates, rateYear==unique.year[i] & class=="20000" & costUnit=="flat fee")$cost[1];       
    zt.rates2 <- subset(hhrates, rateYear==unique.year[i] & class=="20000" & costUnit=="per thousand gallons")$cost[1];
    
    zt.quarter = zt.flat + zt.rates2*15
    hh.quarter[i,"Flat"] <- zt.flat;
    hh.quarter[i,"cost15k"] <- zt.quarter;
  }
  
  }
hh.quarter
hh.quarter$Monthly = round(hh.quarter$cost15k/3,2)

#Plot Estimated monthly rates (note... I cannot duplicate their estimated values)
plot(hh.quarter$Year, hh.quarter$Monthly, pch=19,col="darkgray", cex=1.5, ylim=c(0,50), ylab="Estimated Monthly Cost for 5kgal", 
     xlab="", main=paste0(rates$Name[1],": Estimated Monthly Bill"))
lines(hh.quarter$Year, hh.quarter$Monthly, lwd=2, col="black")



