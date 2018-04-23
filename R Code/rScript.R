
#install.packages("gdata")
require(gdata)

#install.packages("xlsx")
require(xlsx)

#install.packages("gmapsdistance")
#install.packages("RCurl")
#devtools::install_github("rodazuero/gmapsdistance@058009e8d77ca51d8c7dbc6b0e3b622fb7f489a2")
require(gmapsdistance)

setwd("C:/Users/Tim/Documents/Masters of Data Science/FIT5120 - Industry Experience/Data Sets/Iteration Two/")
perlLocation = "C:/Strawberry/perl/bin/perl5.26.1.exe"

#perlLocation = "C:/Program Files/MATLAB/R2017b/sys/perl/win32/bin/perl.exe"
#setwd("//ad.monash.edu/home/User021/tric0003/Desktop/FIT5120 - IE/Iteration Two/")


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

housePricesDf = read.xls("Property Prices/Houses/suburb_house_2016.xls", sheet = 1, header = TRUE, perl = perlLocation  )
#housePricesDf = housePricesDf[,c(-16,-15,-14,-13)]
housePricesDf = housePricesDf[,c(-ncol(housePricesDf),-(ncol(housePricesDf) - 1),-(ncol(housePricesDf) - 2),-(ncol(housePricesDf) - 3))]



tempDF <- housePricesDf
tempDF[] <- lapply(housePricesDf, as.character)
colnames(housePricesDf) <- c("Suburb", tempDF[1, -1])
housePricesDf <- housePricesDf[c(-1,-2) ,]
tempDF <- NULL


housePricesDf[,1] = as.character(housePricesDf[,1])

head(housePricesDf)

housePricesDf[housePricesDf == "-"] = NA
housePricesDf <- as.data.frame(lapply(housePricesDf, function (x) if (is.factor(x)) factor(x) else x)) 
str(housePricesDf)

housePricesDf[,1] = as.character(housePricesDf[,1])

indx <- sapply(housePricesDf, is.factor)
housePricesDf[indx] <- lapply(housePricesDf[indx], function(x) as.numeric(as.character(x)))


houseFeaturesDf = as.data.frame(housePricesDf[,1])
colnames(houseFeaturesDf)[1] = "Suburb"
houseFeaturesDf$Price2006 = housePricesDf$X2006
houseFeaturesDf$Price2006.Median.Factor = houseFeaturesDf$Price2006/median(houseFeaturesDf$Price2006, na.rm = TRUE)
houseFeaturesDf$Price2011 = housePricesDf$X2011
houseFeaturesDf$Price2011.Median.Factor = houseFeaturesDf$Price2011/median(houseFeaturesDf$Price2011, na.rm = TRUE)
houseFeaturesDf$PriceIncrease2006to2011 = houseFeaturesDf$Price2011 - houseFeaturesDf$Price2006
houseFeaturesDf$Price2016 = housePricesDf$X2016
houseFeaturesDf$Price2016.Median.Factor = houseFeaturesDf$Price2016/median(houseFeaturesDf$Price2016, na.rm = TRUE)
houseFeaturesDf$PriceIncrease2011to2016 = houseFeaturesDf$Price2016 - houseFeaturesDf$Price2011

rownames(housePricesDf) = housePricesDf[,1]
housePricesDf = housePricesDf[, -1]

housePricesTableau <- data.frame(rows = rownames(housePricesDf), stack(housePricesDf))
housePricesTableau <- housePricesTableau[order(housePricesTableau$rows), ]
str(housePricesTableau)

housePricesTableau$ind = as.character(housePricesTableau$ind)

housePricesTableau[housePricesTableau == "X2006"] = 2006
housePricesTableau[housePricesTableau == "X2007"] = 2007
housePricesTableau[housePricesTableau == "X2008"] = 2008
housePricesTableau[housePricesTableau == "X2009"] = 2009
housePricesTableau[housePricesTableau == "X2010"] = 2010
housePricesTableau[housePricesTableau == "X2011"] = 2011
housePricesTableau[housePricesTableau == "X2012"] = 2012
housePricesTableau[housePricesTableau == "X2013"] = 2013
housePricesTableau[housePricesTableau == "X2014"] = 2014
housePricesTableau[housePricesTableau == "X2015"] = 2015
housePricesTableau[housePricesTableau == "X2016"] = 2016

head(housePricesTableau)






income2005to2010Df = read.xls("Income/6524055002do004_200506201011.xls", sheet = 3, header = TRUE, perl = perlLocation)
income2005to2010Df = income2005to2010Df[-c(1,2,3,4),]
income2005to2010Df = income2005to2010Df[,c(5,10,23,24,25,26,27,28)]
income2005to2010Df = income2005to2010Df[!(income2005to2010Df[1]==""), ]

income2010to2015Df = read.xls("Income/6524055002do0006_201115.xls", sheet = 5, header = TRUE, perl = perlLocation)
income2010to2015Df = income2010to2015Df[-c(1,2,3,4),]
income2010to2015Df = rbind(income2010to2015Df[1,], income2010to2015Df[grep('^[2]', income2010to2015Df[,1]),])

income2010to2015Df = income2010to2015Df[c(1,2,23,24,25,26,27)]

colnames(income2005to2010Df) = c("SA2", "SA2 Name", "2006", "2007", "2008", "2009", "2010", "2011")
colnames(income2010to2015Df) = c("SA2", "SA2 Name", "2011", "2012", "2013", "2014", "2015")

income2005to2010Df = income2005to2010Df[-1,]
income2010to2015Df = income2010to2015Df[-1,]

SA2011toSA2016 = read.xls("Suburb Correspondence/CG_SA2_2011_SA2_2016.xls", sheet = 4, header = TRUE, perl = perlLocation)
SA2011toSA2016 = SA2011toSA2016[-c(1,2),]

tempDF <- SA2011toSA2016
tempDF[] <- lapply(SA2011toSA2016, as.character)
colnames(SA2011toSA2016) <- tempDF[1,]
tempDF <- NULL
SA2011toSA2016 = SA2011toSA2016[-1,]


income2010to2015Df = merge(x = income2010to2015Df, y = SA2011toSA2016, by.x = "SA2", by.y="SA2_MAINCODE_2016")

income2010to2015Df = income2010to2015Df[,c(3,4,5,6,7,8,9)]
colnames(income2010to2015Df) = c("2011", "2012", "2013", "2014", "2015", "SA2", "SA2 Name")

income2006to2015Df = merge(x = income2005to2010Df, y = income2010to2015Df, by = "SA2", all = TRUE)

income2006to2015Df = income2006to2015Df[!is.na(income2006to2015Df$`SA2 Name.x`),]
income2006to2015Df = income2006to2015Df[!is.na(income2006to2015Df$`SA2 Name.y`),]
income2006to2015Df = income2006to2015Df[,-14]
colnames(income2006to2015Df)[2] = "SA2 Name"

indx <- sapply(income2006to2015Df[3:13], is.factor)
income2006to2015Df[3:13][indx] <- lapply(income2006to2015Df[3:13][indx], function(x) (as.character(x)))

income2006to2015Df[3:13][indx] <- lapply(income2006to2015Df[3:13][indx], function(x) (as.numeric(sub(",", "", x, fixed = TRUE))))


incomeFeaturesDf = income2006to2015Df[,1:2]
incomeFeaturesDf$Income2006 = income2006to2015Df$`2006`
incomeFeaturesDf$Income2006.Median.Factor = incomeFeaturesDf$Income2006/median(incomeFeaturesDf$Income2006, na.rm = TRUE)
incomeFeaturesDf$Income2011 = (income2006to2015Df$`2011.x` + income2006to2015Df$`2011.y`)/2
incomeFeaturesDf$Income2011.Median.Factor = incomeFeaturesDf$Income2011/median(incomeFeaturesDf$Income2011, na.rm = TRUE)
incomeFeaturesDf$IncomeIncrease2006to2011 = incomeFeaturesDf$Income2011 - incomeFeaturesDf$Income2006
incomeFeaturesDf$Income2016 = income2006to2015Df$`2015`
incomeFeaturesDf$Income2016.Median.Factor = incomeFeaturesDf$Income2016/median(incomeFeaturesDf$Income2016, na.rm = TRUE)
incomeFeaturesDf$IncomeIncrease2011to2016 = incomeFeaturesDf$Income2016 - incomeFeaturesDf$Income2011


incomeTableauDf = income2006to2015Df
incomeTableauDf$'2011' = (incomeTableauDf$`2011.x` + incomeTableauDf$`2011.y`)/2
incomeTableauDf = incomeTableauDf[, -c(8,9)]
incomeTableauDf = data.frame(rows = incomeTableauDf[1:2], stack(incomeTableauDf[3:12]))
incomeTableauDf = incomeTableauDf[order(incomeTableauDf$rows.SA2,incomeTableauDf$ind), ]
colnames(incomeTableauDf) = c("SA2", "SA2 Names", "Income", "Year")




ageDf = read.xls("Age/Complete Age Data - with Median One Sheet.xls", sheet = 1, header = TRUE, perl = perlLocation)
ageFeaturesDf = reshape(ageDf, idvar=c('SA2','SA2.Name'), timevar='Year', direction='wide')
ageFeaturesDf = ageFeaturesDf[,c(1,2,7,8,37,38,67,68)]

ageFeaturesDf$Age2006.Median.Factor = ageFeaturesDf$Median.Age.2006/median(ageFeaturesDf$Median.Age.2006, na.rm = TRUE)
ageFeaturesDf$Age2011.Median.Factor = ageFeaturesDf$Median.Age.2011/median(ageFeaturesDf$Median.Age.2011, na.rm = TRUE)
ageFeaturesDf$AgeIncrease2006to2011 = ageFeaturesDf$Median.Age.2011 - ageFeaturesDf$Median.Age.2006
ageFeaturesDf$Age.2016.Median.Factor = ageFeaturesDf$Median.Age.2016/median(ageFeaturesDf$Median.Age.2016, na.rm = TRUE)
ageFeaturesDf$AgeIncrease2011to2016 = ageFeaturesDf$Median.Age.2016 - ageFeaturesDf$Median.Age.2011

ageTableauDf = ageDf


education2006Df = read.xlsx2("Education/Raw/education_level_2006.xlsx", sheetIndex = 1, startRow = 9, as.data.frame = TRUE)
education2006Df = education2006Df[,-1]
colnames(education2006Df)[1] = "Suburb"
education2006Df = education2006Df[-1,]
education2006Df = education2006Df[,-c(2,3,10)]

indx <- sapply(education2006Df[2:7], is.factor)
education2006Df[2:7][indx] <- lapply(education2006Df[2:7][indx], function(x) as.numeric(as.character(x)))

education2006Df$Total = rowSums(education2006Df[2:7])

for (column in colnames(education2006Df[2:7])) {
  education2006Df[,(paste0(column,".percent"))] = education2006Df[,column]/education2006Df$Total
}
education2006Df$Year = "2006"
education2006Df = education2006Df[-c(1498:1507),]




education2011Df = read.xlsx2("Education/Raw/education_level_2011.xlsx", sheetIndex = 1, startRow = 9, as.data.frame = TRUE)
education2011Df = education2011Df[,-1]
colnames(education2011Df)[1] = "Suburb"
education2011Df = education2011Df[-1,]
education2011Df = education2011Df[,-c(7,8,10)]

indx <- sapply(education2011Df[2:7], is.factor)
education2011Df[2:7][indx] <- lapply(education2011Df[2:7][indx], function(x) as.numeric(as.character(x)))

education2011Df$Total = rowSums(education2011Df[2:7])

for (column in colnames(education2011Df[2:7])) {
  education2011Df[,(paste0(column,".percent"))] = education2011Df[,column]/education2011Df$Total
}
education2011Df$Year = "2011"
education2011Df = education2011Df[-c(1545:1555),]
education2011Df$Suburb = sub("\\s\\(Vic.\\)$", "", education2011Df$Suburb)



education2016Df = read.xlsx2("Education/Raw/education_level_2016.xlsx", sheetIndex = 1, startRow = 9, as.data.frame = TRUE)
education2016Df = education2016Df[,-1]
colnames(education2016Df)[1] = "Suburb"
education2016Df = education2016Df[-1,]
education2016Df = education2016Df[,-c(7,8,10)]

indx <- sapply(education2016Df[2:7], is.factor)
education2016Df[2:7][indx] <- lapply(education2016Df[2:7][indx], function(x) as.numeric(as.character(x)))

education2016Df$Total = rowSums(education2016Df[2:7])

for (column in colnames(education2016Df[2:7])) {
  education2016Df[,(paste0(column,".percent"))] = education2016Df[,column]/education2016Df$Total
}


education2016Df$Year = "2016"
education2016Df$Suburb <- sub("\\s\\(Vic.\\)$", "", education2016Df$Suburb)

educationSuburbList = merge(x = education2006Df[1], y = education2011Df[1], by = "Suburb")
educationSuburbList = merge(x = educationSuburbList[1], y = education2016Df[1], by = "Suburb")

education2006Df = merge(x = educationSuburbList[1], y = education2006Df, by = "Suburb")
education2011Df = merge(x = educationSuburbList[1], y = education2011Df, by = "Suburb")
education2016Df = merge(x = educationSuburbList[1], y = education2016Df, by = "Suburb")


education2006FeaturesDf = reshape(education2006Df, idvar=c('Suburb'), timevar='Year', direction='wide')
education2011FeaturesDf = reshape(education2011Df, idvar=c('Suburb'), timevar='Year', direction='wide')
education2016FeaturesDf = reshape(education2016Df, idvar=c('Suburb'), timevar='Year', direction='wide')


educationFeaturesDf = merge(x=education2006FeaturesDf, y=education2011FeaturesDf, by="Suburb")
educationFeaturesDf = merge(x=educationFeaturesDf, y=education2016FeaturesDf, by="Suburb")

educationFeaturesDf = educationFeaturesDf[,c(1,9,10,11,12,13,14,22,23,24,25,26,27,35,36,37,38,39,40)]

educationFeaturesDf$PostGradPercentIncrease2006to2011 = educationFeaturesDf[,8] - educationFeaturesDf[,2]
educationFeaturesDf$GradDipPercentIncrease2006to2011 = educationFeaturesDf[,9] - educationFeaturesDf[,3]
educationFeaturesDf$BaDegPercentIncrease2006to2011 = educationFeaturesDf[,10] - educationFeaturesDf[,4]
educationFeaturesDf$AdvDipPercentIncrease2006to2011 = educationFeaturesDf[,11] - educationFeaturesDf[,5]
educationFeaturesDf$CertPercentIncrease2006to2011 = educationFeaturesDf[,12] - educationFeaturesDf[,6]
educationFeaturesDf$NotAppPercentIncrease2006to2011 = educationFeaturesDf[,13] - educationFeaturesDf[,7]

educationFeaturesDf$PostGradPercentIncrease2011to2016 = educationFeaturesDf[,14] - educationFeaturesDf[,8]
educationFeaturesDf$GradDipPercentIncrease2011to2016 = educationFeaturesDf[,15] - educationFeaturesDf[,9]
educationFeaturesDf$BaDegPercentIncrease2011to2016 = educationFeaturesDf[,16] - educationFeaturesDf[,10]
educationFeaturesDf$AdvDipPercentIncrease2011to2016 = educationFeaturesDf[,17] - educationFeaturesDf[,11]
educationFeaturesDf$CertPercentIncrease2011to2016 = educationFeaturesDf[,18] - educationFeaturesDf[,12]
educationFeaturesDf$NotAppPercentIncrease2011to2016 = educationFeaturesDf[,19] - educationFeaturesDf[,13]


#education2006to2011Df = merge(x = education2006Df, y = education2011Df, by = "Suburb", all = TRUE)                                                                                  
education2006to2011Df = merge(x = education2006Df, y = education2011Df, by = "Suburb")

#education2006to2016Df = merge(x = education2006to2011Df, y = education2016Df, by = "Suburb", all = TRUE)
education2006to2016Df = merge(x = education2006to2011Df, y = education2016Df, by = "Suburb")


educationTableauDf = rbind(education2006Df,education2011Df, education2016Df)




rentalDf = read.xls("Rental/Median Rent by LGA and Property Type One Sheet.xls", sheet = 1, header = TRUE, perl = perlLocation)
rentalDf = rentalDf[,-4]
colnames(rentalDf) = c("SA2", "SA2 Name", "LGA", "LGA Name", "Year", "One Bedroom Unit", "Two Bedroom Unit", "Three Bedroom Unit", "Two Bedroom House", "Three Bedroom House", "Four Bedroom House", "All Properties")
rentalDfTemp = rentalDf[, -c(3,4)]

rentalFeaturesDf = reshape(rentalDfTemp, idvar=c('SA2','SA2 Name'), timevar='Year', direction='wide')


indx <- sapply(rentalFeaturesDf[3:79], is.factor)
rentalFeaturesDf[3:79][indx] <- lapply(rentalFeaturesDf[3:79][indx], function(x) (as.character(x)))
rentalFeaturesDf[rentalFeaturesDf == "-"] = NA
rentalFeaturesDf[3:79][indx] <- lapply(rentalFeaturesDf[3:79][indx], function(x) (as.numeric(x)))


rentalFeaturesDf$OneBedUnitRental2006to2011Increase = rentalFeaturesDf[,38] - rentalFeaturesDf[,3]
rentalFeaturesDf$TwoBedUnitRental2006to2011Increase = rentalFeaturesDf[,39] - rentalFeaturesDf[,4]
rentalFeaturesDf$ThreeBedUnitRental2006to2011Increase = rentalFeaturesDf[,40] - rentalFeaturesDf[,5]
rentalFeaturesDf$TwoBedHouseRental2006to2011Increase = rentalFeaturesDf[,41] - rentalFeaturesDf[,6]
rentalFeaturesDf$ThreeBedHouseRental2006to2011Increase = rentalFeaturesDf[,42] - rentalFeaturesDf[,7]
rentalFeaturesDf$FourBedHouseRental2006to2011Increase = rentalFeaturesDf[,43] - rentalFeaturesDf[,8]
rentalFeaturesDf$AllProperties2006to2011Increase = rentalFeaturesDf[,44] - rentalFeaturesDf[,9]

rentalFeaturesDf$OneBedUnitRental2011to2016Increase = rentalFeaturesDf[,73] - rentalFeaturesDf[,38]
rentalFeaturesDf$TwoBedUnitRental2011to2016Increase = rentalFeaturesDf[,74] - rentalFeaturesDf[,39]
rentalFeaturesDf$ThreeBedUnitRental2011to2016Increase = rentalFeaturesDf[,75] - rentalFeaturesDf[,40]
rentalFeaturesDf$TwoBedHouseRental2011to2016Increase = rentalFeaturesDf[,76] - rentalFeaturesDf[,41]
rentalFeaturesDf$ThreeBedHouseRental2011to2016Increase = rentalFeaturesDf[,77] - rentalFeaturesDf[,42]
rentalFeaturesDf$FourBedHouseRental2011to2016Increase = rentalFeaturesDf[,78] - rentalFeaturesDf[,43]
rentalFeaturesDf$AllProperties2011to2016Increase = rentalFeaturesDf[,79] - rentalFeaturesDf[,44]

rentalFeaturesDf = rentalFeaturesDf[,c(1,2,3,4,5,6,7,8,9,38,39,40,41,42,43,44,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93)]


rentalTableauDf = rentalDfTemp
rentalTableauDf[rentalTableauDf == "-"] = NA


crimeDf = read.xlsx2("Crime/Crime by LGA.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE)
crimeTableauDf = crimeDf


crimeDf$Rate.per.100.000.population = as.numeric(as.character(crimeDf$Rate.per.100.000.population))
crimeAggregatedDf = aggregate(crimeDf$Rate.per.100.000.population, by=list(Category=crimeDf$Year.ending.September,crimeDf$Local.Government.Area,crimeDf$Offence.Division), FUN=sum)
colnames(crimeAggregatedDf) = c("Year", "LGA", "Crime Division", "Rate per 100,000 population")
crimeFeaturesDf = reshape(crimeAggregatedDf, idvar=c('LGA', "Crime Division"), timevar='Year', direction='wide')
crimeFeaturesDf$Increase2008to2011 = crimeFeaturesDf$`Rate per 100,000 population.2011`-crimeFeaturesDf$`Rate per 100,000 population.2008`
crimeFeaturesDf$Increase2013to2016 = crimeFeaturesDf$`Rate per 100,000 population.2016`-crimeFeaturesDf$`Rate per 100,000 population.2013`
crimeFeaturesDf = crimeFeaturesDf[,c(1,2,3,6,8,11,13,14)]
crimeFeaturesDf = reshape(crimeFeaturesDf, idvar=c('LGA'), timevar='Crime Division', direction='wide')






suburbToSA2 = read.xls("Suburb Correspondence/Suburb to SA2.xls", sheet = 1, header = TRUE, perl = perlLocation)
suburbToSA2[suburbToSA2 == "#N/A!"] = NA
suburbToSA2 = na.omit(suburbToSA2)

SA2ToLGA = read.xls("Suburb Correspondence/1270055006_CG_SA2_2011_LGA_2011.xls", sheet = 4, header = TRUE, perl = perlLocation)
SA2ToLGA = SA2ToLGA[-c(1:4),1:5]
colnames(SA2ToLGA) = c("SA2", "SA2 Name", "LGA", "LGA Name", "Ratio")

SA2ToLGA$Ratio = as.numeric(as.character(SA2ToLGA$Ratio))

SA2ToLGAMax = aggregate(Ratio ~ SA2, data = SA2ToLGA, FUN = max)

SA2ToLGA = merge(x = SA2ToLGAMax, y = SA2ToLGA)
SA2ToLGA = SA2ToLGA[, -c(2,3)]
SA2ToLGA$`LGA Name` = gsub("\\s*\\([^\\)]+\\)","",as.character(SA2ToLGA$`LGA Name`))
SA2ToLGA$`LGA Name` = trim(SA2ToLGA$`LGA Name`)
SA2ToLGA$`LGA Name` = toupper(SA2ToLGA$`LGA Name`)

suburbDetails = merge(x = suburbToSA2, y = SA2ToLGA, by.x = "SA2", by.y = "SA2", all.x = TRUE)

allFeaturesDf = educationFeaturesDf
allFeaturesDf$Suburb = toupper(allFeaturesDf$Suburb)


allFeaturesDf = merge(x = suburbDetails, y = allFeaturesDf, by.x = "Suburb", by.y = "Suburb")

allFeaturesDf = merge(x = allFeaturesDf, y = ageFeaturesDf, by.x = "SA2", by.y = "SA2")

allFeaturesDf = merge(x = allFeaturesDf, y = crimeFeaturesDf, by.x = "LGA Name", by = "LGA")

allFeaturesDf = merge(x = allFeaturesDf, y = houseFeaturesDf, by.x = "Suburb", by.y = "Suburb")

allFeaturesDf = merge(x = allFeaturesDf, y = incomeFeaturesDf, by.x = "SA2", by.y = "SA2")

allFeaturesDf = merge(x = allFeaturesDf, y = rentalFeaturesDf, by.x = "SA2", by.y = "SA2")

transportDf = read.csv(file = "Transport/transport_by_suburb.csv", header = TRUE)

transportDf$Suburb = toupper(transportDf$Suburb)

allFeaturesDf = merge(x = allFeaturesDf, y = transportDf, by.x = "Suburb", by.y = "Suburb")








performance2011to2016 = as.data.frame(rownames(housePricesDf))
performance2011to2016$Increase = (housePricesDf$X2016 - housePricesDf$X2011)/housePricesDf$X2011


colnames(performance2011to2016) = c("Suburb", "Performance")


allFeaturesDf = merge(x = allFeaturesDf, y= performance2011to2016, by.x = "Suburb", by.y = "Suburb")
allFeaturesDf$Rating = ifelse(allFeaturesDf$Performance <= summary(allFeaturesDf$Performance)[[2]], "Under Perform", ifelse(allFeaturesDf$Performance >= summary(allFeaturesDf$Performance)[[5]], "Out Perform", "Average"))

allFeaturesDf = allFeaturesDf[, -which(names(allFeaturesDf) %in% c("SA2", "LGA", "SA2.Name.x", "SA2.Name.y"))]



write.csv(allFeaturesDf, file = "allFeatures.csv", row.names = FALSE)


allFeatures2016 = allFeaturesDf[, c(2:14,21:26, 33:36, 39:41,44:45, 48, 50:51, 54, 56:57, 60, 62:63, 66, 68:69, 72, 74:75, 78, 80:84, 89:93, 98:111, 119:125, 133:134, 136)]


#write.csv(allFeatures2016, file = "allFeatures2016.csv", row.names = FALSE)
colnames(allFeatures2016) = c("LGA Name", "Postgraduate.Degree.Level.percent.10YearsPrior", 
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.10YearsPrior", 
                              "Bachelor.Degree.Level.percent.10YearsPrior", 
                              "Advanced.Diploma.and.Diploma.Level.percent.10YearsPrior", 
                              "Certificate.Level.percent.10YearsPrior", 
                              "Not.applicable.percent.10YearsPrior", 
                              "Postgraduate.Degree.Level.percent.5YearsPrior", 
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.5YearsPrior", 
                              "Bachelor.Degree.Level.percent.5YearsPrior", 
                              "Advanced.Diploma.and.Diploma.Level.percent.5YearsPrior", 
                              "Certificate.Level.percent.5YearsPrior", 
                              "Not.applicable.percent.5YearsPrior", 
                              "PostGradPercentIncrease10YearsPriorto5YearsPrior", 
                              "GradDipPercentIncrease10YearsPriorto5YearsPrior", 
                              "BaDegPercentIncrease10YearsPriorto5YearsPrior", 
                              "AdvDipPercentIncrease10YearsPriorto5YearsPrior", 
                              "CertPercentIncrease10YearsPriorto5YearsPrior", 
                              "NotAppPercentIncrease10YearsPriorto5YearsPrior", 
                              "Total.Population.10YearsPrior", 
                              "Median.Age.10YearsPrior", 
                              "Total.Population.5YearsPrior", 
                              "Median.Age.5YearsPrior", 
                              "Age10YearsPrior.Median.Factor", 
                              "Age5YearsPrior.Median.Factor", 
                              "AgeIncrease10YearsPriorto5YearsPrior", 
                              "Rate per 100,000 population.8YearsPrior.A Crimes against the person", 
                              "Rate per 100,000 population.5YearsPrior.A Crimes against the person", 
                              "Increase8YearsPriorto5YearsPrior.A Crimes against the person", 
                              "Rate per 100,000 population.8YearsPrior.B Property and deception offences", 
                              "Rate per 100,000 population.5YearsPrior.B Property and deception offences", 
                              "Increase8YearsPriorto5YearsPrior.B Property and deception offences", 
                              "Rate per 100,000 population.8YearsPrior.C Drug offences", 
                              "Rate per 100,000 population.5YearsPrior.C Drug offences", 
                              "Increase8YearsPriorto5YearsPrior.C Drug offences", 
                              "Rate per 100,000 population.8YearsPrior.D Public order and security offences", 
                              "Rate per 100,000 population.5YearsPrior.D Public order and security offences", 
                              "Increase8YearsPriorto5YearsPrior.D Public order and security offences", 
                              "Rate per 100,000 population.8YearsPrior.E Justice procedures offences", 
                              "Rate per 100,000 population.5YearsPrior.E Justice procedures offences", 
                              "Increase8YearsPriorto5YearsPrior.E Justice procedures offences", 
                              "Rate per 100,000 population.8YearsPrior.F Other offences", 
                              "Rate per 100,000 population.5YearsPrior.F Other offences", 
                              "Increase8YearsPriorto5YearsPrior.F Other offences", 
                              "Price10YearsPrior", 
                              "Price10YearsPrior.Median.Factor", 
                              "Price5YearsPrior", 
                              "Price5YearsPrior.Median.Factor", 
                              "PriceIncrease10YearsPriorto5YearsPrior", 
                              "Income10YearsPrior", 
                              "Income10YearsPrior.Median.Factor", 
                              "Income5YearsPrior", 
                              "Income5YearsPrior.Median.Factor", 
                              "IncomeIncrease10YearsPriorto5YearsPrior", 
                              "One Bedroom Unit.10YearsPrior", 
                              "Two Bedroom Unit.10YearsPrior", 
                              "Three Bedroom Unit.10YearsPrior", 
                              "Two Bedroom House.10YearsPrior", 
                              "Three Bedroom House.10YearsPrior", 
                              "Four Bedroom House.10YearsPrior", 
                              "All Properties.10YearsPrior", 
                              "One Bedroom Unit.5YearsPrior", 
                              "Two Bedroom Unit.5YearsPrior", 
                              "Three Bedroom Unit.5YearsPrior", 
                              "Two Bedroom House.5YearsPrior", 
                              "Three Bedroom House.5YearsPrior", 
                              "Four Bedroom House.5YearsPrior", 
                              "All Properties.5YearsPrior", 
                              "OneBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "TwoBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "ThreeBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "TwoBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "ThreeBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "FourBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "AllProperties10YearsPriorto5YearsPriorIncrease", 
                              "Driving.Time", 
                              "Public.Transport.Time", 
                              "Rating")


write.csv(colnames(allFeatures2016), file = "featureNames.csv")

allFeatures2021 = allFeaturesDf[, c(2, 9:20, 27:32, 35:38, 40, 42:43, 46:47, 49, 52:53, 55, 58:59, 61, 64:65, 67, 70:71, 73, 76:77, 79, 82:83, 85:87, 91:92, 94:96, 105:118, 126:134, 136)]

#write.csv(colnames(allFeatures2021), file = "featureNames2021.csv")

colnames(allFeatures2021) = c("LGA Name",
                              "Postgraduate.Degree.Level.percent.10YearsPrior",
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.10YearsPrior",
                              "Bachelor.Degree.Level.percent.10YearsPrior",
                              "Advanced.Diploma.and.Diploma.Level.percent.10YearsPrior",
                              "Certificate.Level.percent.10YearsPrior",
                              "Not.applicable.percent.10YearsPrior",
                              "Postgraduate.Degree.Level.percent.5YearsPrior",
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.5YearsPrior",
                              "Bachelor.Degree.Level.percent.5YearsPrior",
                              "Advanced.Diploma.and.Diploma.Level.percent.5YearsPrior",
                              "Certificate.Level.percent.5YearsPrior",
                              "Not.applicable.percent.5YearsPrior",
                              "PostGradPercentIncrease10YearsPriorto5YearsPrior",
                              "GradDipPercentIncrease10YearsPriorto5YearsPrior",
                              "BaDegPercentIncrease10YearsPriorto5YearsPrior",
                              "AdvDipPercentIncrease10YearsPriorto5YearsPrior",
                              "CertPercentIncrease10YearsPriorto5YearsPrior",
                              "NotAppPercentIncrease10YearsPriorto5YearsPrior",
                              "Total.Population.10YearsPrior",
                              "Median.Age.10YearsPrior",
                              "Total.Population.5YearsPrior",
                              "Median.Age.5YearsPrior",
                              "Age10YearsPrior.Median.Factor",
                              "Age5YearsPrior.Median.Factor",
                              "AgeIncrease10YearsPriorto5YearsPrior",
                              "Rate per 100,000 population.8YearsPrior.A Crimes against the person",
                              "Rate per 100,000 population.5YearsPrior.A Crimes against the person",
                              "Increase8YearsPriorto5YearsPrior.A Crimes against the person",
                              "Rate per 100,000 population.8YearsPrior.B Property and deception offences",
                              "Rate per 100,000 population.5YearsPrior.B Property and deception offences",
                              "Increase8YearsPriorto5YearsPrior.B Property and deception offences",
                              "Rate per 100,000 population.8YearsPrior.C Drug offences",
                              "Rate per 100,000 population.5YearsPrior.C Drug offences",
                              "Increase8YearsPriorto5YearsPrior.C Drug offences",
                              "Rate per 100,000 population.8YearsPrior.D Public order and security offences",
                              "Rate per 100,000 population.5YearsPrior.D Public order and security offences",
                              "Increase8YearsPriorto5YearsPrior.D Public order and security offences",
                              "Rate per 100,000 population.8YearsPrior.E Justice procedures offences",
                              "Rate per 100,000 population.5YearsPrior.E Justice procedures offences",
                              "Increase8YearsPriorto5YearsPrior.E Justice procedures offences",
                              "Rate per 100,000 population.8YearsPrior.F Other offences",
                              "Rate per 100,000 population.5YearsPrior.F Other offences",
                              "Increase8YearsPriorto5YearsPrior.F Other offences",
                              "Price10YearsPrior",
                              "Price10YearsPrior.Median.Factor",
                              "Price5YearsPrior",
                              "Price5YearsPrior.Median.Factor",
                              "PriceIncrease10YearsPriorto5YearsPrior",
                              "Income10YearsPrior",
                              "Income10YearsPrior.Median.Factor",
                              "Income5YearsPrior",
                              "Income5YearsPrior.Median.Factor",
                              "IncomeIncrease10YearsPriorto5YearsPrior",
                              "One Bedroom Unit.10YearsPrior",
                              "Two Bedroom Unit.10YearsPrior",
                              "Three Bedroom Unit.10YearsPrior",
                              "Two Bedroom House.10YearsPrior",
                              "Three Bedroom House.10YearsPrior",
                              "Four Bedroom House.10YearsPrior",
                              "All Properties.10YearsPrior",
                              "One Bedroom Unit.5YearsPrior",
                              "Two Bedroom Unit.5YearsPrior",
                              "Three Bedroom Unit.5YearsPrior",
                              "Two Bedroom House.5YearsPrior",
                              "Three Bedroom House.5YearsPrior",
                              "Four Bedroom House.5YearsPrior",
                              "All Properties.5YearsPrior",
                              "OneBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "TwoBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "ThreeBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "TwoBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "ThreeBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "FourBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "AllProperties10YearsPriorto5YearsPriorIncrease",
                              "Driving.Time",
                              "Public.Transport.Time",
                              "Rating")

write.csv(allFeatures2021, file = "allFeatures2021.csv", row.names = FALSE)













allFeatures2016Performance = allFeaturesDf[, c(2:14,21:26, 33:36, 39:41,44:45, 48, 50:51, 54, 56:57, 60, 62:63, 66, 68:69, 72, 74:75, 78, 80:84, 89:93, 98:111, 119:125, 133:134, 135)]

#write.csv(allFeatures2016, file = "allFeatures2016.csv", row.names = FALSE)
colnames(allFeatures2016Performance) = c("LGA Name", "Postgraduate.Degree.Level.percent.10YearsPrior", 
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.10YearsPrior", 
                              "Bachelor.Degree.Level.percent.10YearsPrior", 
                              "Advanced.Diploma.and.Diploma.Level.percent.10YearsPrior", 
                              "Certificate.Level.percent.10YearsPrior", 
                              "Not.applicable.percent.10YearsPrior", 
                              "Postgraduate.Degree.Level.percent.5YearsPrior", 
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.5YearsPrior", 
                              "Bachelor.Degree.Level.percent.5YearsPrior", 
                              "Advanced.Diploma.and.Diploma.Level.percent.5YearsPrior", 
                              "Certificate.Level.percent.5YearsPrior", 
                              "Not.applicable.percent.5YearsPrior", 
                              "PostGradPercentIncrease10YearsPriorto5YearsPrior", 
                              "GradDipPercentIncrease10YearsPriorto5YearsPrior", 
                              "BaDegPercentIncrease10YearsPriorto5YearsPrior", 
                              "AdvDipPercentIncrease10YearsPriorto5YearsPrior", 
                              "CertPercentIncrease10YearsPriorto5YearsPrior", 
                              "NotAppPercentIncrease10YearsPriorto5YearsPrior", 
                              "Total.Population.10YearsPrior", 
                              "Median.Age.10YearsPrior", 
                              "Total.Population.5YearsPrior", 
                              "Median.Age.5YearsPrior", 
                              "Age10YearsPrior.Median.Factor", 
                              "Age5YearsPrior.Median.Factor", 
                              "AgeIncrease10YearsPriorto5YearsPrior", 
                              "Rate per 100,000 population.8YearsPrior.A Crimes against the person", 
                              "Rate per 100,000 population.5YearsPrior.A Crimes against the person", 
                              "Increase8YearsPriorto5YearsPrior.A Crimes against the person", 
                              "Rate per 100,000 population.8YearsPrior.B Property and deception offences", 
                              "Rate per 100,000 population.5YearsPrior.B Property and deception offences", 
                              "Increase8YearsPriorto5YearsPrior.B Property and deception offences", 
                              "Rate per 100,000 population.8YearsPrior.C Drug offences", 
                              "Rate per 100,000 population.5YearsPrior.C Drug offences", 
                              "Increase8YearsPriorto5YearsPrior.C Drug offences", 
                              "Rate per 100,000 population.8YearsPrior.D Public order and security offences", 
                              "Rate per 100,000 population.5YearsPrior.D Public order and security offences", 
                              "Increase8YearsPriorto5YearsPrior.D Public order and security offences", 
                              "Rate per 100,000 population.8YearsPrior.E Justice procedures offences", 
                              "Rate per 100,000 population.5YearsPrior.E Justice procedures offences", 
                              "Increase8YearsPriorto5YearsPrior.E Justice procedures offences", 
                              "Rate per 100,000 population.8YearsPrior.F Other offences", 
                              "Rate per 100,000 population.5YearsPrior.F Other offences", 
                              "Increase8YearsPriorto5YearsPrior.F Other offences", 
                              "Price10YearsPrior", 
                              "Price10YearsPrior.Median.Factor", 
                              "Price5YearsPrior", 
                              "Price5YearsPrior.Median.Factor", 
                              "PriceIncrease10YearsPriorto5YearsPrior", 
                              "Income10YearsPrior", 
                              "Income10YearsPrior.Median.Factor", 
                              "Income5YearsPrior", 
                              "Income5YearsPrior.Median.Factor", 
                              "IncomeIncrease10YearsPriorto5YearsPrior", 
                              "One Bedroom Unit.10YearsPrior", 
                              "Two Bedroom Unit.10YearsPrior", 
                              "Three Bedroom Unit.10YearsPrior", 
                              "Two Bedroom House.10YearsPrior", 
                              "Three Bedroom House.10YearsPrior", 
                              "Four Bedroom House.10YearsPrior", 
                              "All Properties.10YearsPrior", 
                              "One Bedroom Unit.5YearsPrior", 
                              "Two Bedroom Unit.5YearsPrior", 
                              "Three Bedroom Unit.5YearsPrior", 
                              "Two Bedroom House.5YearsPrior", 
                              "Three Bedroom House.5YearsPrior", 
                              "Four Bedroom House.5YearsPrior", 
                              "All Properties.5YearsPrior", 
                              "OneBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "TwoBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "ThreeBedUnitRental10YearsPriorto5YearsPriorIncrease", 
                              "TwoBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "ThreeBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "FourBedHouseRental10YearsPriorto5YearsPriorIncrease", 
                              "AllProperties10YearsPriorto5YearsPriorIncrease", 
                              "Driving.Time", 
                              "Public.Transport.Time", 
                              "Performance")

write.csv(allFeatures2016Performance, file = "allFeatures2016Performance.csv", row.names = FALSE)



allFeatures2021Performance = allFeaturesDf[, c(2, 9:20, 27:32, 35:38, 40, 42:43, 46:47, 49, 52:53, 55, 58:59, 61, 64:65, 67, 70:71, 73, 76:77, 79, 82:83, 85:87, 91:92, 94:96, 105:118, 126:134, 135)]

#write.csv(colnames(allFeatures2021), file = "featureNames2021.csv")

colnames(allFeatures2021Performance) = c("LGA Name",
                              "Postgraduate.Degree.Level.percent.10YearsPrior",
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.10YearsPrior",
                              "Bachelor.Degree.Level.percent.10YearsPrior",
                              "Advanced.Diploma.and.Diploma.Level.percent.10YearsPrior",
                              "Certificate.Level.percent.10YearsPrior",
                              "Not.applicable.percent.10YearsPrior",
                              "Postgraduate.Degree.Level.percent.5YearsPrior",
                              "Graduate.Diploma.and.Graduate.Certificate.Level.percent.5YearsPrior",
                              "Bachelor.Degree.Level.percent.5YearsPrior",
                              "Advanced.Diploma.and.Diploma.Level.percent.5YearsPrior",
                              "Certificate.Level.percent.5YearsPrior",
                              "Not.applicable.percent.5YearsPrior",
                              "PostGradPercentIncrease10YearsPriorto5YearsPrior",
                              "GradDipPercentIncrease10YearsPriorto5YearsPrior",
                              "BaDegPercentIncrease10YearsPriorto5YearsPrior",
                              "AdvDipPercentIncrease10YearsPriorto5YearsPrior",
                              "CertPercentIncrease10YearsPriorto5YearsPrior",
                              "NotAppPercentIncrease10YearsPriorto5YearsPrior",
                              "Total.Population.10YearsPrior",
                              "Median.Age.10YearsPrior",
                              "Total.Population.5YearsPrior",
                              "Median.Age.5YearsPrior",
                              "Age10YearsPrior.Median.Factor",
                              "Age5YearsPrior.Median.Factor",
                              "AgeIncrease10YearsPriorto5YearsPrior",
                              "Rate per 100,000 population.8YearsPrior.A Crimes against the person",
                              "Rate per 100,000 population.5YearsPrior.A Crimes against the person",
                              "Increase8YearsPriorto5YearsPrior.A Crimes against the person",
                              "Rate per 100,000 population.8YearsPrior.B Property and deception offences",
                              "Rate per 100,000 population.5YearsPrior.B Property and deception offences",
                              "Increase8YearsPriorto5YearsPrior.B Property and deception offences",
                              "Rate per 100,000 population.8YearsPrior.C Drug offences",
                              "Rate per 100,000 population.5YearsPrior.C Drug offences",
                              "Increase8YearsPriorto5YearsPrior.C Drug offences",
                              "Rate per 100,000 population.8YearsPrior.D Public order and security offences",
                              "Rate per 100,000 population.5YearsPrior.D Public order and security offences",
                              "Increase8YearsPriorto5YearsPrior.D Public order and security offences",
                              "Rate per 100,000 population.8YearsPrior.E Justice procedures offences",
                              "Rate per 100,000 population.5YearsPrior.E Justice procedures offences",
                              "Increase8YearsPriorto5YearsPrior.E Justice procedures offences",
                              "Rate per 100,000 population.8YearsPrior.F Other offences",
                              "Rate per 100,000 population.5YearsPrior.F Other offences",
                              "Increase8YearsPriorto5YearsPrior.F Other offences",
                              "Price10YearsPrior",
                              "Price10YearsPrior.Median.Factor",
                              "Price5YearsPrior",
                              "Price5YearsPrior.Median.Factor",
                              "PriceIncrease10YearsPriorto5YearsPrior",
                              "Income10YearsPrior",
                              "Income10YearsPrior.Median.Factor",
                              "Income5YearsPrior",
                              "Income5YearsPrior.Median.Factor",
                              "IncomeIncrease10YearsPriorto5YearsPrior",
                              "One Bedroom Unit.10YearsPrior",
                              "Two Bedroom Unit.10YearsPrior",
                              "Three Bedroom Unit.10YearsPrior",
                              "Two Bedroom House.10YearsPrior",
                              "Three Bedroom House.10YearsPrior",
                              "Four Bedroom House.10YearsPrior",
                              "All Properties.10YearsPrior",
                              "One Bedroom Unit.5YearsPrior",
                              "Two Bedroom Unit.5YearsPrior",
                              "Three Bedroom Unit.5YearsPrior",
                              "Two Bedroom House.5YearsPrior",
                              "Three Bedroom House.5YearsPrior",
                              "Four Bedroom House.5YearsPrior",
                              "All Properties.5YearsPrior",
                              "OneBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "TwoBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "ThreeBedUnitRental10YearsPriorto5YearsPriorIncrease",
                              "TwoBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "ThreeBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "FourBedHouseRental10YearsPriorto5YearsPriorIncrease",
                              "AllProperties10YearsPriorto5YearsPriorIncrease",
                              "Driving.Time",
                              "Public.Transport.Time",
                              "Performance")

write.csv(allFeatures2021Performance, file = "allFeatures2021Performance.csv", row.names = FALSE)











































##########################################################################################################
## TABLEAU MERGING 
##########################################################################################################

colnames(housePricesTableau) = c("Suburb", "House.Price", "Year")
allTableauDf = housePricesTableau


allTableauDf = merge(x = allTableauDf, y = suburbDetails, by.x = "Suburb", by.y = "Suburb")
allTableauDf$Year = as.integer(allTableauDf$Year)

#allTableauDf$SA2 = as.integer(as.character(allTableauDf$SA2))
ageTableauDf$SA2 = (as.factor(ageTableauDf$SA2))

allTableauDf = merge(x = allTableauDf, y = ageTableauDf, by.x = c("SA2", "Year"), by.y = c("SA2", "Year"))

crimeTableauDf$Year.ending.September = as.integer(as.character(crimeTableauDf$Year.ending.September))

allTableauDf = merge(x = allTableauDf, y = crimeTableauDf, by.x = c("LGA Name","Year"), by = c("Local.Government.Area", "Year.ending.September"), all.x = TRUE)

educationTableauDf$Year = as.integer(educationTableauDf$Year)
educationTableauDf$Suburb = toupper(educationTableauDf$Suburb)
allTableauDf = merge(x = allTableauDf, y = educationTableauDf, by.x = c("Suburb","Year"), by.y = c("Suburb","Year"), all.x = TRUE)


incomeTableauDf$Year = as.integer(as.character(incomeTableauDf$Year))
#incomeTableauDf$SA2 = as.integer(as.character(incomeTableauDf$SA2))
allTableauDf = merge(x = allTableauDf, y = incomeTableauDf, by.x = c("SA2", "Year"), by.y = c("SA2", "Year"), all.x = TRUE)

###############################################
## Could improve this by changing rental shape to same as crime
###############################################
rentalTableauDf$SA2 = as.factor(rentalTableauDf$SA2)
allTableauDf = merge(x = allTableauDf, y = rentalTableauDf, by.x = c("SA2", "Year"), by.y = c("SA2", "Year"), all.x = TRUE)

transportDf = read.csv(file = "Transport/transport_by_suburb.csv", header = TRUE)

transportDf$Suburb = toupper(transportDf$Suburb)

allTableauDf = merge(x = allTableauDf, y = transportDf, by.x = "Suburb", by.y = "Suburb", all.x = TRUE)

allTableauDf = allTableauDf[, -c(8,33,35)]

colnames(allTableauDf) = c("Suburb",
                            "SA2",
                            "Year",
                            "LGA Name",
                            "House Price",
                            "SA2 Name",
                            "LGA",
                            "Male Population",
                            "Male Median Age",
                            "Female Population",
                            "Female Median Age",
                            "Total Population",
                            "Median Age",
                            "Offence Division",
                            "Offence Subdivision",
                            "Offence Subgroup",
                            "Incidents Recorded",
                            "Rate per 100,000 population",
                            "Postgraduate Degree Level",
                            "Graduate Diploma and Graduate Certificate Level",
                            "Bachelor Degree Level",
                            "Advanced Diploma and Diploma Level",
                            "Certificate Level",
                            "Not applicable",
                            "Total",
                            "Postgraduate Degree Level percent",
                            "Graduate Diploma and Graduate Certificate Level percent",
                            "Bachelor Degree Level percent",
                            "Advanced Diploma and Diploma Level percent",
                            "Certificate Level percent",
                            "Not applicable percent",
                            "Income",
                            "One Bedroom Unit",
                            "Two Bedroom Unit",
                            "Three Bedroom Unit",
                            "Two Bedroom House",
                            "Three Bedroom House",
                            "Four Bedroom House",
                            "All Properties",
                            "Driving Time to CBD",
                            "Public Transport Time to CBD")


write.csv(allTableauDf, file = "allTableauDf.csv", row.names = FALSE)







suburbScores = as.data.frame(allFeaturesDf$Suburb)
suburbScores$bachAndPostGrad = allFeaturesDf$Postgraduate.Degree.Level.percent.2016 + allFeaturesDf$Graduate.Diploma.and.Graduate.Certificate.Level.percent.2016 + allFeaturesDf$Bachelor.Degree.Level.percent.2016
suburbScores$crime = allFeaturesDf$`Rate per 100,000 population.2016.A Crimes against the person` + allFeaturesDf$`Rate per 100,000 population.2016.B Property and deception offences` + allFeaturesDf$`Rate per 100,000 population.2016.C Drug offences` + allFeaturesDf$`Rate per 100,000 population.2016.D Public order and security offences` + allFeaturesDf$`Rate per 100,000 population.2016.E Justice procedures offences` + allFeaturesDf$`Rate per 100,000 population.2016.F Other offences`
suburbScores$income = allFeaturesDf$Income2016
suburbScores$price = allFeaturesDf$Price2016
suburbScores$rent2BedUnit = allFeaturesDf$`Two Bedroom Unit.2016`
suburbScores$rent3BedHouse = allFeaturesDf$`Three Bedroom House.2016`

bachAndPostGradQuintiles = quantile(suburbScores$bachAndPostGrad, seq(0, 1, 0.2), na.rm = TRUE)
crimeQuintiles  = quantile(suburbScores$crime, seq(0, 1, 0.2), na.rm = TRUE)
incomeQuintiles = quantile(suburbScores$income, seq(0, 1, 0.2), na.rm = TRUE)
priceQuintiles = quantile(suburbScores$price, seq(0, 1, 0.2), na.rm = TRUE)
rent2BedUnitQuintiles = quantile(suburbScores$rent2BedUnit, seq(0, 1, 0.2), na.rm = TRUE)
rent3BedHouseQuintiles = quantile(suburbScores$rent3BedHouse, seq(0, 1, 0.2), na.rm = TRUE)

suburbScores$bachAndPostGradRating = findInterval(suburbScores$bachAndPostGrad , bachAndPostGradQuintiles, all.inside = TRUE)
suburbScores$crimeRating = 6 - findInterval(suburbScores$crime , crimeQuintiles, all.inside = TRUE)
suburbScores$incomeRating = findInterval(suburbScores$income , incomeQuintiles, all.inside = TRUE)
suburbScores$priceRating = findInterval(suburbScores$price , priceQuintiles, all.inside = TRUE)
suburbScores$rent2BedUnitRating = findInterval(suburbScores$rent2BedUnit, rent2BedUnitQuintiles, all.inside = TRUE)
suburbScores$rent3BedHouseRating = findInterval(suburbScores$rent3BedHouse , rent3BedHouseQuintiles, all.inside = TRUE)
  
write.csv(suburbScores, file = "suburbScores.csv")



