#Creating Supplementary Data spreadsheet and additional stats used in the text

#Read in datafiles from previous scripts (if needed)
#myData21 <- read_csv("./myData/16_myDataCprice_v4.csv")

#overshoot5 <- read_csv("./myData/16_myDataOvershootGDPreparations_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_double(), col_double()))

#bindReparations6 <- read_csv("./myData/18_cumulativeReparations-PerCap-GDPshare_v4.csv")

#co2gdp4 <- read_csv("./myData/15_cumulativeCO2-GDP-POP_v4.csv", col_types=list(col_double(), col_character(), col_character(),
	col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_character()))

#countryRegion <- read_csv("./CleanData/country_Region.csv") %>%
	select(iso3c=iso, eu)

#rank4 <- read_csv("./myData/16_countryOvershootShareReparations-1850-2050_v4.csv", col_types=list(col_double(), col_character(), col_character(), 
	col_character(), col_double(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double(), 
	col_double(), col_character(), col_double(), col_double(), col_double(), col_double(), col_character(), col_character(), col_double(), 
	col_double(), col_double()))

#---------------------------------------------------------------------------------------------------------
#			WRITE SUPPLEMENTARY DATA FILE
#---------------------------------------------------------------------------------------------------------
#load library
library(openxlsx)

#create historical, bau, and net zero files to save
#historical
hist <- overshoot5 %>% 
	filter(scenario == "Historical", startDate == 1960, date <= 2019, iso3c!="WLD") %>%
	select(country, iso3c, date, `1.5C overshoot ratio`=overshoot15C) %>%
	spread(date, `1.5C overshoot ratio`)

#BAU median
bauMed <- overshoot5 %>%
	filter(scenario == "BAU", startDate == 1960, date >= 2020, iso3c != "WLD") %>%
	select(country, iso3c, date, `1.5C overshoot ratio_med`=overshoot15C) %>%
	spread(date, `1.5C overshoot ratio_med`)

#BAU lower 66% PI
bauLow <- overshoot5 %>%
	filter(scenario == "BAU", startDate == 1960, date >= 2020, iso3c != "WLD") %>%
	select(country, iso3c, date, `1.5C overshoot ratio_lwr`=overshoot15Clwr) %>%
	spread(date, `1.5C overshoot ratio_lwr`)

#BAU upper 66% PI
bauHigh <- overshoot5 %>%
	filter(scenario == "BAU", startDate == 1960, date >= 2020, iso3c != "WLD") %>%
	select(country, iso3c, date, `1.5C overshoot ratio_upr`=overshoot15Cupr) %>%
	spread(date, `1.5C overshoot ratio_upr`)

#Net zero series
netZero <- overshoot5 %>%
	filter(scenario == "NetZero", startDate == 1960, date >= 2020, iso3c != "WLD") %>%
	select(country, iso3c, date, `1.5C overshoot ratio_netZero`=overshoot15C) %>%
	spread(date, `1.5C overshoot ratio_netZero`)

#Cumulative reparations in 2050
#get fair share - cumCO2 difference by country in 2050 net zero

diff <- rank4 %>%
	filter(scenario == "NetZero", startDate == 1960, date == 2050) %>%
	select(country, iso3c, `1.5C overshoot diff`=overshoot15C)

reparations <- bindReparations6 %>%
	filter(startDate == 1960) %>%
	select(country, iso3c, `country group`=group15C, type, share, `Reparations_med`=cumReparationsAR6,
	  `Reparations_lwr`=cumReparationsAR6lwr, `Reparations_upr`=cumReparationsAR6upr,
	  `Reparations_perCap_med`=cumRepPerCap, `Reparations_perCap_lwr`=cumRepPerCapLwr,
	  `Reparations_perCap_upr`=cumRepPerCapUpr) %>%
	left_join(., diff, by =c("country", "iso3c")) %>%
	relocate(`1.5C overshoot diff`, .after=type)

#fair shares
fairShare <- overshoot5 %>%
	filter(scenario == "BAU", startDate == 1960, date == 2050, iso3c != "WLD") %>%
	select(country, iso3c, `350ppm fair-share`=fairShare350, `1.5C fair-share`=fairShare15C,
	  `2C fair-share`=fairShare2C)
#----------------------------------------------------------------------------------

#write to file and create excel Supplementary Data spreadsheet with multiple tabs manually
write_csv(hist, "./SIdata/Historical_R1.csv", na="")
write_csv(bauMed, "./SIdata/BAU_median_R1.csv", na="")
write_csv(bauLow, "./SIdata/BAU_lower_R1.csv", na="")
write_csv(bauHigh, "./SIdata/BAU_upper_R1.csv", na="")
write_csv(netZero, "./SIdata/Net-zero_R1.csv", na="")
write_csv(reparations, "./SIdata/Reparations_2050_R1.csv", na="")
write_csv(fairShare, "./SIdata/Fair-shares_R1.csv", na="")

#----------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#			ADDITIONAL STATS
#---------------------------------------------------------------------------------------------------------

#Number of countries in GS and GN (N=168)
myData21 %>% filter(date==2019, northSouth=="STH") %>% count(country) #N=129
myData21 %>% filter(date==2019, northSouth=="NRT") %>% count(country) #N=39
#----------------------------------------------------------------------------------
#Number of overshooting global South countries in 2019 (N=20)
overshoot5 %>% filter(startDate == 1960, date == 2019, northSouth == "STH", overshoot15C >= 1)

#Number of overshooting global South countries in 2050 net zero (N=28)
overshoot5 %>% filter(startDate == 1960, date == 2050, scenario == "NetZero", northSouth == "STH", overshoot15C >= 1)

#Number of overshooting global South countries in 2050 BAU (N=46)
overshoot5 %>% filter(startDate == 1960, date == 2050, scenario == "BAU", northSouth == "STH", overshoot15C >= 1)
#----------------------------------------------------------------------------------
#share of population
myData21 %>% filter(date == 2019) %>% group_by(northSouth) %>% 
  summarise(popShare1960 = sum(popShare_1960, na.rm=T),
	popShare1850 = sum(popShare_1850, na.rm=T), popShare1992 = sum(popShare_1992, na.rm=T)) %>% ungroup()
#----------------------------------------------------------------------------------
#confirm USA CHina and India
myData21 %>% select(date, country, iso3c, CO2) %>% filter(date == 2019) %>% arrange(-CO2)


#look at USA- CHN and India. share of budget remaining in 2050 
overshoot5 %>%
	filter(iso3c %in% c("USA", "CHN", "IND"), date == 2050, scenario == "NetZero") %>%
	select(startDate, scenario, country, iso3c, date, overshoot15C, overshoot350) %>%
	arrange(country)

#find out how much of emissions are missed by 1992 etc startdates
overshoot5 %>%
	filter(iso3c %in% c("USA", "CHN", "IND"), date == 2019) %>%
	select(startDate, scenario, country, iso3c, date, cumCO2, overshoot15C) %>%
	arrange(country)
#----------------------------------------------------------------------------------
#find out earliest year of overshoot by regional group (1971 by EU and UK ; 1980 in USA)
read_csv("./Figures/14_countryGroupOvershootShare-1850-2050_v3.csv") %>%
 filter(startDate == 1960, scenario == "Historical", overshoot > 0)

#find out earliest year of overshoot by country
print(read_csv("./myData/14_countryOvershootShare-1850-2050_v3.csv") %>%
  select(startDate, scenario, country, iso3c, date, overshoot15C, debtGroup15C) %>%
  filter(startDate == 1960, scenario == "Historical", overshoot15C > 0), n=100)
#----------------------------------------------------------------------------------
#find out relationship between cumulative GDP per capita and overshoot15C
gdpChart <- read_csv("./Figures/15_cumulativeCO2-GDPchartData_v4.csv") #%>%
	#filter(!iso3c %in% c("QAT", "ARE"))
relp <- lm(cumGDPperCap ~ overshoot15C, data=gdpChart)
summary(relp)

cor.test(gdpChart$cumGDPperCap, gdpChart$overshoot15C, method=c("pearson"))
rm(gdpChart, relp)
#----------------------------------------------------------------------------------
#find out average per capita reparations per year across all overshooters
read_csv("./myData/17_debtorsAndCreditors-perCap_v4.csv") %>% 
  filter(startDate == 1960) %>%
  group_by(type) %>%
  summarise(avgCumRepPerCap = mean(cumRepPerCap),
	avgCumRepPerCapLwr = mean(cumRepPerCapLwr),
	avgCumRepPerCapUpr = mean(cumRepPerCapUpr),
	avgOvershoot15C = mean(overshoot15C)) %>%
  ungroup()
#----------------------------------------------------------------------------------
#find out how many undershooting countries would have % shares appropriated (10 >= 95%; 55 >= 75%)
read_csv("./myData/17_debtorsAndCreditors-perCap_v4.csv") %>% 
  filter(startDate == 1960, type == "Undershooters", overshoot15C <= 0.25)

#find out average reparations per capita of undershooting countries with more than 75% and less than 25% of fair shares appropriated
read_csv("./myData/17_debtorsAndCreditors-perCap_v4.csv") %>% 
  filter(startDate == 1960, type == "Undershooters", overshoot15C >= 0.75) %>%
  summarise(avgCumRepPerCap = mean(cumRepPerCap),
	avgCumRepPerCapLwr = mean(cumRepPerCapLwr),
	avgCumRepPerCapUpr = mean(cumRepPerCapUpr),
	avgOvershoot15C = mean(overshoot15C))

#find out average reparations per capita of overshooting countries with more than 3x and less than 50% beyond fair shares
read_csv("./myData/17_debtorsAndCreditors-perCap_v4.csv") %>% 
  filter(startDate == 1960, type == "Overshooters", overshoot15C <= 1.5) %>%
  summarise(avgCumRepPerCap = mean(cumRepPerCap),
	avgCumRepPerCapLwr = mean(cumRepPerCapLwr),
	avgCumRepPerCapUpr = mean(cumRepPerCapUpr),
	avgOvershoot15C = mean(overshoot15C))




