#Geoffrey Rozo

#MSDS 410
#Modeling Assignment 1

rm(list=ls())

library(sqldf)
library(moments)
library(rockchalk)

#mydata <- read.csv(file="/Users/grozo/Documents/Northwestern Graduate School Classes/MSDS-410 Data Modeling for Supervised Learning/410 Modeling Assignments/MSDS-410 Model Assign 1/ames_housing_data.csv",head=TRUE,sep=",")
mydata<-data.frame(ames_housing_data)


str(mydata)
head(mydata, 2)
names(mydata)

###################################################################################

mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF

#combined square feet on 1st + 2nd floor of properties

mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt

#age of property

mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond


mydata$logSalePrice <- log(mydata$SalePrice)

#transforming the SalePrice to look in different representation

mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF


summary(mydata$price_sqft)
hist(mydata$price_sqft)
subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","GarageArea", "QualityIndex",
                                  "OverallQual", "OverallCond", "FullBath",
                                  "price_sqft", "SalePrice", "logSalePrice",
                                  "LotArea", "BsmtFinSF1","Neighborhood","HouseStyle",
                                  "LotShape","logSalePrice", "PavedDrive",
                                  "TotalBsmtSF", "TotRmsAbvGrd", "YearRemodel",
                                  "YrSold", "BsmtQual", "BsmtCond",
                                  "ExterQual", "ExterCond"))

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","OverallQual","logSalePrice"))




#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#should we drop the small number of properties with the IR3 = irregular parking lot shape? only a couple..

Irregular <- subdat[subdat$LotShape %in% "IR3", ]

dim(Irregular)
#this shows that there are 16 properties with IR3...

list <- c("IR1", "IR2", "Reg")

TypicalLots <- subdat[subdat$LotShape %in% list, ]

dim(TypicalLots)

ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

rockchalk::skewness(subdat$SalePrice)   #real bad right/positive skewness of 1.74

#######

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#SalePrice and TotalFloorSF is skewed to the right

ggplot(subdat, aes(x=QualityIndex)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#Quality index just has a couple outliers, but normal.

sqldf("select * from subdat where QualityIndex > 80")

#could cut out these last 2 properties, do they really represent what a "typical" property is?



#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(subdat, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#box plot of house age vs. lot shape ...
#lot of outliers in IR2, different boxplot... most homes that are IR2 are newer...

############################################################
################ model focused EDA #######################
###########################################################




ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(subdat)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdatnum)

jpeg("corrplot 7 vars.jpg")
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.9)
dev.off()

#####################################################################
############# Define the sample data ###########################
##################################################################

#could do this various ways... this is provided to us...


subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

dim(subdat2)   #shows that it is now 2925 and 14 variables.. only got rid of 5 properties.


###THE FOLLOWING IS MY WAY....


subdat10 <- sqldf("select * from subdat where SalePrice < 400000")

dim(subdat10)  #shows that there are 2866 properties under 400k SalePrice

ggplot(subdat10, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

rockchalk::skewness(subdat10$SalePrice)   #0.94 positive/right skewing

#could try a smaller range (50k - 250k)

subdat11 <- sqldf("select * from subdat where SalePrice < 300000 and SalePrice > 50000")

dim(subdat11) #shows 2680 properties in this new range... visualize it now

ggplot(subdat11, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))



subdat12 <- sqldf("select * from subdat where SalePrice <= 250000 and SalePrice > 50000")

dim(subdat12)  #2473 properties out of the 2930 total data set.

ggplot(subdat12, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

##################################################################################################

#I like the subdat12 set, it has 2473 properties out of the 2930, most normal distrib. of Sale Price

rockchalk::skewness(subdat12$SalePrice)   #0.1939 positive/right skew...
rockchalk::kurtosis(subdat12$SalePrice, excess=FALSE)    #kurtosis/peakness of 2.47 out of norm of 3...


#OK so use subdat12 right now...

subdat12 <- subdat12[subdat12$LotShape %in% list, ]
dim(subdat12)

#2463 total properties, just excluded the irregular lots (IR3)...

ggplot(subdat12) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dim(subdat12[subdat12$LotShape %in% "IR2", ])

#I am fine with this now

ggplot(subdat12, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

sqldf("select * from subdat12 where TotalFloorSF > 4000")
#so, I would like to only take 'typical' homes, exclude the outliers
#of homes with TotalFloorSF above 4000... only 2 properties to exclude.

subdat12 <- sqldf("select * from subdat12 where TotalFloorSF < 4000")
dim(subdat12)
#2461 left, with 14 variables...

subdat12 <- sqldf("Select * from subdat12 where LotArea < 150000")
dim(subdat12)

#got rid of 1 more with a crazy outlier LotArea over 150,000...

#so, 2460...


##############################################################

ggplot(subdat12, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat12, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat12, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

str(subdat12)

#need to change characters -> factors
#BsmtQual, BsmtCond ...

subdat12$BsmtQual <- as.factor(subdat12$BsmtQual)
subdat12$BsmtCond <- as.factor(subdat12$BsmtCond)

str(subdat12)

summary(subdat12)
summary(subdat12$Neighborhood)

quantile(subdat12$TotalFloorSF)
quantile(subdat12$HouseAge)
quantile(subdat12$LotArea)
quantile(subdat12$YearRemodel)
quantile(subdat12$SalePrice)

subd1 <- sqldf("Select TotalFloorSF, HouseAge, GarageArea,
                OverallQual, OverallCond, price_sqft, LotArea,
                TotRmsAbvGrd, SalePrice, logSalePrice
                from subdat12")

str(subd1)

mcor <- cor(subd1)

jpeg("corrplotA.jpg")
# 2. Create the plot


corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=1.0)

# 3. Close the file
dev.off()

subd2 <- sqldf("Select FullBath,
                YearRemodel, YrSold,
                SalePrice,
                logSalePrice from subdat12")

str(subd2)

mcor <- cor(subd2)

jpeg("corrplotB.jpg")

corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=1.0)

dev.off()

########################################################################
########################################################################
#**************INITIAL EDA************

jpeg("scatterplotA.jpg")
ggplot(subdat12, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=1.2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###
dev.off()

jpeg("scatterplotB.jpg")
ggplot(subdat12, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotC.jpg")
ggplot(subdat12, aes(x=LotArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Lot Area") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotD.jpg")
ggplot(subdat12, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Garage Area") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

#################################################################################


jpeg("scatterplotOverallQual.jpg")
ggplot(subdat12, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotTotRmsAbvGrd.jpg")
ggplot(subdat12, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Rooms Above Grade") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotHouseAge.jpg")
ggplot(subdat12, aes(x=HouseAge, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs House Age") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotFullBath.jpg")
ggplot(subdat12, aes(x=FullBath, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Full Bath") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotYearRemodel.jpg")
ggplot(subdat12, aes(x=YearRemodel, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Year Remodel") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()

jpeg("scatterplotYrSold.jpg")
ggplot(subdat12, aes(x=YrSold, y=SalePrice)) + 
  geom_point(color="purple", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Year Sold") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
dev.off()



subdat12$PavedDrive   # N , P , Y

subdat12$PavedDriveNum <- as.numeric(subdat12$PavedDrive)

subdat12$PavedDriveNum

jpeg("histPavedDrive.jpg")
hist(subdat12$PavedDriveNum, main="Histogram of Paved Driveways of Properties",
                          xlab="Paved driveway (1=No, 2=Partial, 3=Yes)",
                          border="blue",
                          col="orange")
dev.off()


jpeg("histFullBath.jpg")
hist(subdat12$FullBath, main="Histogram of Full Bathrooms of Properties",
     xlab="Full Bathrooms",
     border="blue",
     col="purple")
dev.off()

jpeg("histHouseAge.jpg")
hist(subdat12$HouseAge, main="Histogram of House Age of Properties",
     xlab="House Age",
     border="blue",
     col="purple")
dev.off()

jpeg("histYearRemodel.jpg")
hist(subdat12$YearRemodel, main="Histogram of Remodel Year of Properties",
     xlab="Remodel Year",
     border="blue",
     col="purple")
dev.off()

jpeg("histYearSold.jpg")
hist(subdat12$YrSold, main="Histogram of Year Sold of Properties",
     xlab="Year Sold",
     border="blue",
     col="purple")
dev.off()

#################################################
#################################################
#################################################

jpeg("scatterplotHouseAge.jpg")
ggplot(subdat12, aes(x=HouseAge, y=SalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs House Age") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotHouseAgeLOG.jpg")
ggplot(subdat12, aes(x=HouseAge, y=logSalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Log of Sale Price vs House Age") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotOverallQual.jpg")
ggplot(subdat12, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotOverallQualLOG.jpg")
ggplot(subdat12, aes(x=OverallQual, y=logSalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Log of Sale Price vs Overall Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotTotalFloorSF.jpg")
ggplot(subdat12, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()

jpeg("scatterplotTotalFloorSFLOG.jpg")
ggplot(subdat12, aes(x=TotalFloorSF, y=logSalePrice)) + 
  geom_point(color="red", shape=1) +
  ggtitle("Scatter Plot of Log of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)
dev.off()


#######################################################################
########################################################################
#######################################################################

#DOWN HERE IS COMPUTATIONAL ASSIGNMENT #2


subdatX <- subset(mydata, select=c("TotalFloorSF","GarageArea",
                                  "LotArea", "TotalBsmtSF", "GrLivArea",
                                  "MiscVal", "PoolArea", "ScreenPorch",
                                  "OpenPorchSF", "WoodDeckSF", "SalePrice"))

str(subdatX)

dim(subdatX)

subdatX <- sqldf("select * from subdatX where SalePrice <= 250000 and SalePrice > 50000")
dim(subdatX)
subdatX <- sqldf("select * from subdatX where TotalFloorSF < 4000")
dim(subdatX)
#2461 left, with 14 variables...

subdatX <- sqldf("Select * from subdatX where LotArea < 150000")
dim(subdatX)


y<-subdatX$SalePrice
TFarea<-subdatX$TotalFloorSF
Garage<-subdatX$GarageArea
Lot<-subdatX$LotArea
TBmnt<-subdatX$TotalBsmtSF
GrLivearea<-subdatX$GrLivArea
Misc<-subdatX$MiscVal
Poolarea<-subdatX$PoolArea
ScreenP<-subdatX$ScreenPorch
OpenP<-subdatX$OpenPorchSF
WoodP<-subdatX$WoodDeckSF






Model_3 <- lm(y~TFarea+TBmnt+GrLivearea+Garage+Lot)
summary(Model_3)
anova(Model_3)





Model_4<-lm(y~TFarea+TBmnt+GrLivearea+Garage+Lot+ScreenP+OpenP+WoodP)
summary(Model_4)
anova(Model_4)






