## This script estimates country-level BAU pathways

#Read in datafile from previous script (if needed)
#myData10 <- read_csv("001_cleanCO2-populationData_1850-2050_20211222.csv")
#------------------------------------------------------------------------------------------------------------
#MY FUNCTIONS
#-------------------------------------------------------------------------------------
#				FUNCTIONS TO COMPARE ETS AND ARIMA MODELS
#-------------------------------------------------------------------------------------
#exponential smoothing
fets <- function(x, h) {
  forecast(ets(x), h = h)
}

#ARIMA
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# WRITE FUNCTION TO FORECAST COUNTRY TRENDS BY BEST-FITTING MODEL (ETS OR ARIMA) 
#-------------------------------------------------------------------------------------
fcast <- function(x, end){ 
	#create list and preallocate iso3c names
	models <- vector('list',length(colnames(x)))
	names(models) <- colnames(x)

	#for loop through each country and forecast to 2050 by best-fitting auto-regression model 
	for(i in 1:length(colnames(x))){
		#get country data
		y <- x[,i]

		#select either ETS or ARIMA model class by smaller mean standard error
		e1 <- tsCV(y, fets, h=1)
		e2 <- tsCV(y, farima, h=1)
		if(mean(e1^2, na.rm=TRUE) < mean(e2^2, na.rm=TRUE)){
			models[[i]] <- forecast(ets(y), h=(2050-end), level=c(50,66))
		}
		else{	models[[i]] <- forecast(auto.arima(y), h=(2050-end), level=c(50,66))
		}
	}
	models
} #end fcast() function
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# WRITE FUNCTION TO CONSTRUCT TIBBLE FROM COUNTRY LIST OF FORECAST MODEL MATRICES 
#-------------------------------------------------------------------------------------
#Pull out relevant vars from country list of model matrices, and convert back to tibble 
build_tbl <- function(models){
	#Create named list container
	listDF <- vector('list', length(models))
	names(listDF) <- names(models)
	
	#Fill list container with selected model values (mean and prediction interval)
	for(i in seq_along(models)){
		listDF[[i]] = tibble(iso3c = names(models[i]), 
			date = time(models[[i]]$mean),
			forecast = as.numeric(models[[i]]$mean),
			lower66 = as.numeric(models[[i]]$lower[,2]),
			upper66 = as.numeric(models[[i]]$upper[,2]))
	}

	#Convert list to tidy tibble
	listDF %>% enframe() %>% unnest() %>% select(-name)
}


#------------------------------------------------------------------------------------------------------------
# Project CO2 BAU from 2020-2050, based on 1960-2019 emissions
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

co2 <- myData10 %>%
	filter(date <= 2019, date >= 1960) %>%
	select(date, iso3c, CO2)


#Convert to ts object
co2ts <- read.zoo(co2, split="iso3c", index="date", FUN=identity) %>% as.ts()

#Run world data through my fcast_world function, which chooses an ETS or ARIMA model based on smallest MSE
modelCO2 <- fcast(co2ts, 2019)

#Convert trend model back into tibble with my build_tbl function, and rename
bauCO2 <- build_tbl(modelCO2) %>%
	rename(CO2_bau = forecast, CO2_lwr66 = lower66, CO2_upr66 = upper66)

#-------------------------------------------------------------------------------------
#Join country-level data and BAU projections
myData11 <- left_join(myData10, bauCO2, by = c("iso3c", "date"))

rm(bauCO2, co2ts, co2, myData10)

#look at countries
countries <- myData11 %>%
	filter(iso3c %in% c("BGD", "BRA", "CRI", "IND", "USA", "GBR", "CHN", "NGA", "WLD", "KAZ", "UZB", "RUS", "SRB"))

ggplot(data = countries, aes(x = date)) + 
	#geom_vline(xintercept = 1988, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_vline(xintercept = 2029, colour = "grey", size = 0.8, linetype="dashed") +
	#geom_line(aes(y = fairShare_350ppm/1000000), size=1.2, colour = "#387f30") +
	#geom_line(aes(y = fairShare_15C/1000000), size=1.2, colour = "#a1d99b") +
	geom_line(aes(y = CO2/1000), size=1.2, colour = "black") +
	geom_ribbon(aes(ymin = CO2_lwr66/1000, ymax = CO2_upr66/1000), alpha=0.3, fill="#ff4e4e") +
	geom_line(aes(y = CO2_bau/1000), size=1.2, colour = "red") +
	#geom_line(aes(y = cumCO2data_net0/1000000), size=1.2, colour = "#0000a5") +
	#geom_line(aes(y = cumCO2data_grn/1000000), size=1.2, colour = "#a5a500") +
	facet_wrap(~ country, scales="free") +
	labs(x="Year", y="Carbon dioxide emissions (Mt CO2)") +
	theme_chart_SMALLM


#ggsave("bauCountrySelection.png")
rm(countries, build_tbl, fcast, fets, farima)

#---------------------------------------------------------------------
#		WRITE TO FILE
#--------------------------------------------------------------------
#write_csv(myData11, "./myData/03_CO2-BAU_v1.csv")


