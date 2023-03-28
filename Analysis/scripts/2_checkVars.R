#This script does some consistency checks across the variables
#my functions
dropZero <- function(x)(ifelse(x<0, NA, x))

interp <- function(x)(na.approx(x, na.rm="FALSE"))

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} 

is_outlierWithin <- function(x) {
  return(x < quantile(x, 0.25) - 5 * IQR(x) | x > quantile(x, 0.75) + 5 * IQR(x))
}

is_outlierWithin1 <- function(x) {
  return(x < quantile(x, 0.25) - 10 * IQR(x) | x > quantile(x, 0.75) + 10 * IQR(x))
}

#-----------------------------------------------------------------------------------------
#load data from script 1, if needed
#myData <- read_csv("./myData/1_colonizingAtmosphereData_21dec2021.csv", col_types=list(col_character(), col_character(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()))
#-----------------------------------------------------------------------------------------
#Drop eora values less than 0
myData1 <- myData %>%
	mutate(across(c(eoraOldTerr, eoraOldFoot, eoraNewTerr, eoraNewFoot, eoraScaledFoot),
		~dropZero(.x))) 

#Interpolate to replace the dropped negative values
myData2 <- myData1 %>%
	group_by(country, iso3c) %>%
	mutate(across(c(eoraOldTerr, eoraOldFoot, eoraNewTerr, eoraNewFoot, eoraScaledFoot),
		~interp(.x))) %>%
	ungroup()

#---------------------------------------------------------------------------------------------
#Check out ratio outliers of old/new eora for 1990-2015
oldNew <- myData2 %>%
	select(country, iso3c, date, eoraOldFoot, eoraNewFoot) %>%
	filter(date >= 1990, date <= 2015) %>%
	rowwise() %>%
	mutate(oldNewRatio = eoraOldFoot / eoraNewFoot) %>%
	ungroup() %>%
	drop_na(oldNewRatio) %>%
	group_by(date) %>%
	mutate(oldNewOutlier = ifelse(is_outlier(oldNewRatio), round(oldNewRatio, digits=1),
		as.numeric(NA))) %>%
	ungroup() %>%
	select(-eoraOldFoot, -eoraNewFoot)
	#filter(oldNewRatio <= 150) %>%
	
ggplot(oldNew %>% filter(oldNewRatio <= 50), aes(x = factor(date), y = oldNewRatio)) + 
	  geom_boxplot() +
	  geom_text(aes(y = oldNewOutlier, label = iso3c), na.rm = TRUE, hjust = -0.3)
#ggsave("eoraOldOverNew_outliers.png")

#Pull out list of outliers and number of times they were outliers over the 26-year period
outlierList <- oldNew %>%
	drop_na(oldNewOutlier) %>% 
	count(country, iso3c) %>%
	add_column(oldNewOutlier = TRUE)
#write_csv(outlierList, "1_eoraOldNewOutlierList-19902015.csv")

#Join outliers to myData
myData3 <- left_join(myData2, outlierList, by=c("country", "iso3c")) %>%
	select(-n)

rm(myData1, myData2, oldNew, outlierList)
#-------------------------------------------------------------------------------------
#Create conditional CO2 series based on whether or not a country has any oldNew outliers
myData4 <- myData3 %>%
	rowwise() %>%
	mutate(eoraFoot = ifelse(date >= 1990, eoraScaledFoot,
		ifelse(date >= 1970 & date < 1990 & is.na(oldNewOutlier), eoraScaledFoot,
		ifelse(date >= 1970 & date < 1990 & !is.na(oldNewOutlier), primapHist,
		ifelse(date < 1970, primapHist, "ERROR"))))) %>%
	ungroup() %>%
	select(-oldNewOutlier)

rm(myData3)

#-------------------------------------------------------------------------------------
#Drop really wacky eora outlier countries
terrFoot <- myData4 %>%
	select(country, iso3c, date, primapHist, eoraFoot) %>%
	filter(date >= 1990) %>%
	rowwise() %>%
	mutate(terrFootRatio = primapHist / eoraFoot,
		footTerrRatio = eoraFoot / primapHist) %>%
	ungroup() %>%
	drop_na(terrFootRatio) %>%
	group_by(date) %>%
	mutate(terrFootOutlier = ifelse(is_outlier(terrFootRatio), round(terrFootRatio, digits=1),
		as.numeric(NA)),
		footTerrOutlier = ifelse(is_outlier(footTerrRatio), round(footTerrRatio, digits=1),
		as.numeric(NA))) %>%
	ungroup() %>%
	select(-primapHist, -eoraFoot)
	
ggplot(terrFoot %>% filter(terrFootRatio <= 50), aes(x = factor(date), y = terrFootRatio)) + 
	  geom_boxplot() +
	  geom_text(aes(y = terrFootOutlier, label = iso3c), na.rm = TRUE, hjust = -0.3)
#ggsave("eoraTerrFoot_outliers.png")

#Pull out list of outliers and number of times they were outliers over the 30-year period
terrFootOutlierList <- terrFoot %>%
	drop_na(terrFootOutlier) %>% 
	count(country, iso3c) %>%
	add_column(terrFootOutlier = TRUE)

#Gather list of terrFoot countries that have lots of outliers
dropTerrFootN <- terrFootOutlierList %>%
	filter(n >= 10) %>%
	select(-terrFootOutlier)

#Pull out list of outliers and number of times they were outliers over the 30-year period
footTerrOutlierList <- terrFoot %>%
	drop_na(footTerrOutlier) %>% 
	count(country, iso3c) %>%
	add_column(footTerrOutlier = TRUE)

#Gather list of footTerr countries that have lots of outliers
dropFootTerrN <- footTerrOutlierList %>%
	filter(n >= 10) %>%
	select(-footTerrOutlier)

dropN <- full_join(dropTerrFootN, dropFootTerrN, by=c("country", "iso3c")) %>%
	filter(iso3c %in% c("BLR", "ETH", "GUY", "MCO", "MDA", "SSD", "SDN"))
#write_csv(dropN, "1_droppedEoraOutlierCountries.csv")

#Drop these big outlier countries from the main dataset
myData5 <- myData4 %>%
	filter(!iso3c %in% c("BLR", "ETH", "GUY", "MCO", "MDA", "SSD", "SDN"))

rm(terrFoot, terrFootOutlierList, footTerrOutlierList, dropTerrFootN, dropFootTerrN, dropN, myData4)

#-------------------------------------------------------------------------------------
#Check for eora spikes and troughs within countries

within <- myData5 %>%
	select(country, iso3c, date, primapHist, eoraFoot) %>%
	filter(date >= 1990) %>%
	rowwise() %>%
	mutate(terrFootRatio = primapHist / eoraFoot,
		footTerrRatio = eoraFoot / primapHist) %>%
	ungroup() %>%
	drop_na(terrFootRatio) %>%
	group_by(country, iso3c) %>%
	mutate(terrFootOutlier = ifelse(is_outlierWithin(terrFootRatio), round(terrFootRatio, digits=1),
		as.numeric(NA)),
		footTerrOutlier = ifelse(is_outlierWithin1(footTerrRatio), round(footTerrRatio, digits=1),
		as.numeric(NA))) %>%
	ungroup() %>%
	select(-primapHist, -eoraFoot)

ggplot(within %>% filter(terrFootRatio < 50), aes(x = factor(iso3c), y = terrFootRatio)) + # 
	  geom_boxplot() +
	  geom_text(aes(y = terrFootOutlier, label = iso3c), na.rm = TRUE, hjust = -0.3)
#ggsave("eoraTerrFoot_outliers.png")

ggplot(within, aes(x = factor(iso3c), y = footTerrRatio)) + #%>% filter(footTerrRatio < 50)
	  geom_boxplot() +
	  geom_text(aes(y = footTerrOutlier, label = iso3c), na.rm = TRUE, hjust = -0.3)
#ggsave("eoraFootTerr_outliers.png")



#Pull out list of outliers within each country-level series and number of times they were outliers 
#over the 30-year period
withinOutlierList <- within %>%
	drop_na(terrFootOutlier) %>% 
	count(country, iso3c) %>%
	add_column(withinTerrFootOutlier = TRUE)

withinOutlierList1 <- within %>%
	drop_na(footTerrOutlier) %>% 
	count(country, iso3c) %>%
	add_column(withinFootTerrOutlier = TRUE)

#write_csv(withinOutlierList, "1_eoraWithinOutlierList-19902015.csv")

#Join outliers to myData
within1 <- within %>%
	select(-terrFootRatio, -footTerrRatio)

myData6 <- left_join(myData5, within1, by=c("country", "iso3c", "date"))

rm(within, within1, withinOutlierList, withinOutlierList1, myData5)

#-------------------------------------------------------------------------------------
#Drop within-country observations with BIG outliers (i.e. 5 x IQR) and interpolate
myData7 <- myData6 %>%
	rowwise() %>%
	mutate(eoraFoot1 = ifelse(is.na(terrFootOutlier), eoraFoot, NA),
	  eoraFoot2 = ifelse(is.na(footTerrOutlier), eoraFoot1, NA)) %>% #drops 109 big outliers
	ungroup() %>% #drops another 10 big outliers
	select(-terrFootOutlier, -footTerrOutlier) %>%
	group_by(country, iso3c) %>%
	mutate(eoraFoot1 = na.approx(eoraFoot2, na.rm="FALSE")) %>% #interpolates the dropped outlier values
	ungroup() %>%
	select(-eoraFoot2)

rm(myData6)

#-------------------------------------------------------------------------------------
#Calculate global aggregates
world <- myData7 %>%
	group_by(date) %>%
	summarise(population = ifelse(sum(population, na.rm=T) == 0, NA, sum(population, na.rm=T)), 
		primapHist = ifelse(sum(primapHist, na.rm=T) == 0, NA, sum(primapHist, na.rm=T)),
		eoraOldTerr = ifelse(sum(eoraOldTerr, na.rm=T) == 0, NA, sum(eoraOldTerr, na.rm=T)),
		eoraOldFoot = ifelse(sum(eoraOldFoot, na.rm=T) == 0, NA, sum(eoraOldFoot, na.rm=T)),
		eoraNewTerr = ifelse(sum(eoraNewTerr, na.rm=T) == 0, NA, sum(eoraNewTerr, na.rm=T)),
		eoraNewFoot = ifelse(sum(eoraNewFoot, na.rm=T) == 0, NA, sum(eoraNewFoot, na.rm=T)),
		eoraScaledFoot = ifelse(sum(eoraScaledFoot, na.rm=T) == 0, NA, sum(eoraScaledFoot, na.rm=T)),
		eoraFoot = ifelse(sum(eoraFoot, na.rm=T) == 0, NA, sum(eoraFoot, na.rm=T)),
		eoraFoot1 = ifelse(sum(eoraFoot1, na.rm=T) == 0, NA, sum(eoraFoot1, na.rm=T))) %>%
	ungroup() %>%
	add_column(country = "World", iso3c = "WLD") %>%
	relocate(country, iso3c)

#Add world to myData
myData8 <- rbind(myData7, world) %>%
	arrange(iso3c)

rm(myData7, world)

#---------------------------------------------------------------------------------------------------
#				CHECK OUT COUNTRIES
#---------------------------------------------------------------------------------------------------

checkVars <- myData8 %>%
	select(-population, -eoraOldTerr, -eoraNewTerr, -eoraScaledFoot, -eoraNewFoot, -eoraOldFoot) %>%
	filter(date >= 1960, date <= 2020) %>%
	gather(CO2indicator, CO2value, c(-country, -iso3c, -date))

countries <- checkVars %>%
	filter(iso3c %in% c("EST", "TTO", "HTI", "HND", "KAZ", "LIE", "UZB", "WLD")) 
		#"ARG", "ARM", "AUS", "BRA", "CRI", "CUB", "KAZ", "MEX", "MWI", "CAN",  "COL", "DEU", "SRB", 
		#"PAK", , "CHN","RUS", "USA","IND", "SDN", "ZWE" "AND", "BOL", "AUT", "FIN", "NPL",
		# "NZL", "MKD", "RUS", "URY", "KAZ", "UZB")

colours <- c("#41b6c4", "darkred", "#253494", "#c7e9b4", "#7fcdbb") #"#c7e9b4", "#7fcdbb", "#2c7fb8", , 

ggplot(countries, aes(x = date, y = CO2value)) + 
  geom_line(aes(color = CO2indicator), size=1) + #, linetype = CO2indicator 
  facet_wrap(~ country, scales = "free") +
  scale_color_manual(values = colours) +
  theme_chart_SMALLM

#ggsave("World_PrimapEoraFoot_mergedComparison_v1.png")

rm(checkVars, countries, colours)

#---------------------------------------------------------------------------------------------------
#	DROP COUNTRIES WITH MISSING VALUES TO GET FINAL LIST OF COUNTRIES
#---------------------------------------------------------------------------------------------------
myData9 <- myData8 %>%
	select(country, iso3c, date, population, CO2 = eoraFoot1)

myData9 %>% count(iso3c)
#N=211 including countries with missing values;

missingCO2 <- myData9 %>% filter(date <= 2019) %>%
	group_by(country, iso3c) %>%
	count(is.na(CO2)) %>%
	ungroup() %>%
	filter(`is.na(CO2)` != FALSE) %>%  #N=37 with missing CO2 observations
	select(iso3c)

missingCO21990 <- myData9 %>% 
	filter(date <= 2019, date >= 1990) %>%
	group_by(country, iso3c) %>%
	count(is.na(CO2)) %>%
	ungroup() %>%
	filter(`is.na(CO2)` != FALSE) %>%  #N=37 with missing CO2 observations
	select(iso3c)

#drop countries with missing values
myData10 <- myData9 %>%
	filter(!iso3c %in% c("ASM","BMU","CYM","CHI","COM","CUW","DMA","GNQ","FRO","PYF","GIB","GRL",
		"GRD","GUM", "GNB", "IMN", "KIR", "XKX", "MHL", "FSM", "NRU", "NCL",
		"MNP", "PLW", "PRI", "SXM", "SLB", "KNA", "LCA", "MAF", "VCT", "TLS", "TON", "TCA", 
		"TUV", "VIR", "PSE"))

myData1990 <- myData9 %>%
	filter(date >= 1990, date <= 2019, !iso3c %in% c("ASM","COM","CUW","DMA","GNQ","FRO","GIB",
		"GRD","GUM", "GNB", "IMN", "KIR", "MHL", "FSM", "NRU",
		"MNP","PLW","PRI","SXM","SLB","KNA","LCA","MAF","VCT","TLS","TON","TCA", 
		"TUV", "VIR"))

missingPop <- myData10 %>% filter(date <= 2019) %>%
	group_by(country, iso3c) %>%
	count(is.na(population)) %>%
	ungroup() %>%
	filter(`is.na(population)` != FALSE) %>%  #N=3 with missing CO2 observations
	select(iso3c)

myData10 <- myData10 %>%
	filter(!iso3c %in% c("ABW", "VGB", "MAC"))

countryList <- myData10 %>% count(iso3c) %>% select(iso3c)
#N=171 without missing CO2 and population values;


countryList1990 <- myData1990 %>% count(iso3c) %>% select(iso3c)
#N=180 without missing CO2 values from 1990-2019;

#---------------------------------------------------------------------------------------------------
#	WRITE CLEANED UP DATA TO FILE (N = 171)
#---------------------------------------------------------------------------------------------------

#write_csv(myData10, "001_cleanCO2-populationData_1850-2050_20211222.csv")
#write_csv(myData1990 %>% select(country, iso3c, date, CO2foot_eora = CO2),
  "./myData/001_cleanCO2foot_EORA_1990-2019.csv")

rm(myData, myData8, myData9, myData1990, missingCO2, missingPop, dropZero, interp, is_outlier, is_outlierWithin)

