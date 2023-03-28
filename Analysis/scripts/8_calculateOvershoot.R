## This script calculates cumulative overshoot in 2019 and 2050 from 
# 1850, 1960, and 1992 for 350 ppm, 1.5C and 2C

#Read in datafile from previous script (if needed)
#myData20 <- read_csv("./myData/07_myDataCO2convergenceWithGroups_v3.csv")
#--------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# Calculate national cumulative overshoot from 1850 for each country (and world)
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

overshoot1850_hist <- myData20 %>%
	filter(date <= 2019) %>%
	select(country, iso3c, date, cumCO2 = cumCO21850, fairShare350 = fairShare350_1850,
		fairShare15C = fairShare15C_1850, fairShare2C = fairShare2C_1850, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1850, scenario = "Historical") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1850_bau <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_bau1850, fairShare350 = fairShare350_1850,
		fairShare15C = fairShare15C_1850, fairShare2C = fairShare2C_1850, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1850, scenario = "BAU") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1850_net0 <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_net01850, fairShare350 = fairShare350_1850,
		fairShare15C = fairShare15C_1850, fairShare2C = fairShare2C_1850, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1850, scenario = "NetZero") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1850_neg15C <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumNeg15C_1850, fairShare350 = fairShare350_1850,
		fairShare15C = fairShare15C_1850, fairShare2C = fairShare2C_1850, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1850, scenario = "Converge15C") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = NA,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = NA) %>%
	ungroup()

#merge
overshoot1850 <- rbind(overshoot1850_hist, overshoot1850_bau, overshoot1850_net0, overshoot1850_neg15C)

overshoot1850_ribbon <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2_lwr = cumCO2_lwr661850, cumCO2_upr = cumCO2_upr661850,
		fairShare350 = fairShare350_1850, fairShare15C = fairShare15C_1850, 
		fairShare2C = fairShare2C_1850) %>%
	add_column(startDate = 1850, scenario = "BAU") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350lwr = cumCO2_lwr / fairShare350,
		overshoot350upr = cumCO2_upr / fairShare350,
		overshoot15Clwr = cumCO2_lwr / fairShare15C,
		overshoot15Cupr = cumCO2_upr / fairShare15C,
		overshoot2Clwr = cumCO2_lwr / fairShare2C,
		overshoot2Cupr = cumCO2_upr / fairShare2C) %>%
	ungroup() %>%
	select(-fairShare350, -fairShare15C, -fairShare2C)

overshoot1850 <- left_join(overshoot1850, overshoot1850_ribbon, by=c("startDate", "scenario", "country",
	"iso3c", "date")) %>%
	relocate(northAndRegions, northSouth, annex1, .after=iso3c)

#clean up
rm(overshoot1850_hist, overshoot1850_bau, overshoot1850_net0, overshoot1850_neg15C, overshoot1850_ribbon)

#------------------------------------------------------------------------------------------------------------
# Calculate national cumulative overshoot from 1960 for each country (and world)
#------------------------------------------------------------------------------------------------------------

overshoot1960_hist <- myData20 %>%
	filter(date >= 1960, date <= 2019) %>%
	select(country, iso3c, date, cumCO2 = cumCO21960, fairShare350 = fairShare350_1960,
		fairShare15C = fairShare15C_1960, fairShare2C = fairShare2C_1960, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1960, scenario = "Historical") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1960_bau <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_bau1960, fairShare350 = fairShare350_1960,
		fairShare15C = fairShare15C_1960, fairShare2C = fairShare2C_1960, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1960, scenario = "BAU") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1960_net0 <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_net01960, fairShare350 = fairShare350_1960,
		fairShare15C = fairShare15C_1960, fairShare2C = fairShare2C_1960, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1960, scenario = "NetZero") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1960_neg15C <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumNeg15C_1960, fairShare350 = fairShare350_1960,
		fairShare15C = fairShare15C_1960, fairShare2C = fairShare2C_1960, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1960, scenario = "Converge15C") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350 = NA,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = NA) %>%
	ungroup()

#merge
overshoot1960 <- rbind(overshoot1960_hist, overshoot1960_bau, overshoot1960_net0, overshoot1960_neg15C)

overshoot1960_ribbon <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2_lwr = cumCO2_lwr661960, cumCO2_upr = cumCO2_upr661960,
		fairShare350 = fairShare350_1960, fairShare15C = fairShare15C_1960, 
		fairShare2C = fairShare2C_1960) %>%
	add_column(startDate = 1960, scenario = "BAU") %>%
	relocate(startDate, scenario) %>%
	rowwise() %>%
	mutate(overshoot350lwr = cumCO2_lwr / fairShare350,
		overshoot350upr = cumCO2_upr / fairShare350,
		overshoot15Clwr = cumCO2_lwr / fairShare15C,
		overshoot15Cupr = cumCO2_upr / fairShare15C,
		overshoot2Clwr = cumCO2_lwr / fairShare2C,
		overshoot2Cupr = cumCO2_upr / fairShare2C) %>%
	ungroup() %>%
	select(-fairShare350, -fairShare15C, -fairShare2C)

overshoot1960 <- left_join(overshoot1960, overshoot1960_ribbon, by=c("startDate", "scenario", "country",
	"iso3c", "date")) %>%
	relocate(northAndRegions, northSouth, annex1, .after=iso3c)

#clean up
rm(overshoot1960_hist, overshoot1960_bau, overshoot1960_net0, overshoot1960_neg15C, overshoot1960_ribbon)

#------------------------------------------------------------------------------------------------------------
# Calculate national cumulative overshoot from 1992 for each country (and world)
#------------------------------------------------------------------------------------------------------------

overshoot1992_hist <- myData20 %>%
	filter(date >= 1992, date <= 2019) %>%
	select(country, iso3c, date, cumCO2 = cumCO21992, 
		fairShare15C = fairShare15C_1992, fairShare2C = fairShare2C_1992, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1992, scenario = "Historical", fairShare350 = NA) %>%
	relocate(startDate, scenario) %>%
	relocate(fairShare350, .before=fairShare15C) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1992_bau <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_bau1992,
		fairShare15C = fairShare15C_1992, fairShare2C = fairShare2C_1992, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1992, scenario = "BAU", fairShare350 = NA) %>%
	relocate(startDate, scenario) %>%
	relocate(fairShare350, .before=fairShare15C) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1992_net0 <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumCO2_net01992,
		fairShare15C = fairShare15C_1992, fairShare2C = fairShare2C_1992, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1992, scenario = "NetZero", fairShare350 = NA) %>%
	relocate(startDate, scenario) %>%
	relocate(fairShare350, .before=fairShare15C) %>%
	rowwise() %>%
	mutate(overshoot350 = cumCO2 / fairShare350,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = cumCO2 / fairShare2C) %>%
	ungroup()

overshoot1992_neg15C <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2 = cumNeg15C_1992,
		fairShare15C = fairShare15C_1992, fairShare2C = fairShare2C_1992, northAndRegions, 
		northSouth, annex1) %>%
	add_column(startDate = 1992, scenario = "Converge15C", fairShare350 = NA) %>%
	relocate(startDate, scenario) %>%
	relocate(fairShare350, .before=fairShare15C) %>%
	rowwise() %>%
	mutate(overshoot350 = NA,
		overshoot15C = cumCO2 / fairShare15C,
		overshoot2C = NA) %>%
	ungroup()

#merge
overshoot1992 <- rbind(overshoot1992_hist, overshoot1992_bau, overshoot1992_net0, overshoot1992_neg15C)

overshoot1992_ribbon <- myData20 %>%
	filter(date >= 2020) %>%
	select(country, iso3c, date, cumCO2_lwr = cumCO2_lwr661992, cumCO2_upr = cumCO2_upr661992,
		fairShare15C = fairShare15C_1992, fairShare2C = fairShare2C_1992) %>%
	add_column(startDate = 1992, scenario = "BAU", fairShare350 = NA) %>%
	relocate(startDate, scenario) %>%
	relocate(fairShare350, .before=fairShare15C) %>%
	rowwise() %>%
	mutate(overshoot350lwr = NA,
		overshoot350upr = NA,
		overshoot15Clwr = cumCO2_lwr / fairShare15C,
		overshoot15Cupr = cumCO2_upr / fairShare15C,
		overshoot2Clwr = cumCO2_lwr / fairShare2C,
		overshoot2Cupr = cumCO2_upr / fairShare2C) %>%
	ungroup() %>%
	select(-fairShare350, -fairShare15C, -fairShare2C)

overshoot1992 <- left_join(overshoot1992, overshoot1992_ribbon, by=c("startDate", "scenario", "country",
	"iso3c", "date")) %>%
	relocate(northAndRegions, northSouth, annex1, .after=iso3c)

#clean up
rm(overshoot1992_hist, overshoot1992_bau, overshoot1992_net0, overshoot1992_neg15C, overshoot1992_ribbon)


#------------------------------------------------------------------------------------------------------------
#merge 1850, 1960 and 1992 analyses and write to file
#------------------------------------------------------------------------------------------------------------
overshoot <- rbind(overshoot1992, overshoot1960, overshoot1850)

#write_csv(overshoot, "./myData/08_myDataOvershoot_v3.csv")

#clean up
rm(overshoot1850, overshoot1960, overshoot1992)

#----------------------------------------------------------------------------------------------------------
#			LOOK AT COUNTRY-LEVEL 1850/1960 OVERSHOOT IN 2019
#----------------------------------------------------------------------------------------------------------
ratioStart350 <- overshoot %>%
	filter(date == 2019) %>%
	select(startDate, country, iso3c, date, overshoot350) %>%
	spread(startDate, overshoot350) %>%
	rowwise() %>%
	mutate(ratio1850over1960 = `1960`/`1850`) %>%
	ungroup() %>%
	add_column(boundary2019 = "350ppm") %>%
	select(-date)

ratioStart15C <- overshoot %>%
	filter(date == 2019) %>%
	select(startDate, country, iso3c, date, overshoot15C) %>%
	spread(startDate, overshoot15C) %>%
	rowwise() %>%
	mutate(ratio1850over1960 = `1960`/`1850`) %>%
	ungroup() %>%
	add_column(boundary2019 = "1.5C") %>%
	select(-date)

ratioStart2C <- overshoot %>%
	filter(date == 2019) %>%
	select(startDate, country, iso3c, date, overshoot2C) %>%
	spread(startDate, overshoot2C) %>%
	rowwise() %>%
	mutate(ratio1850over1960 = `1960`/`1850`) %>%
	ungroup() %>%
	add_column(boundary2019 = "2C") %>%
	select(-date)

ratioStart <- rbind(ratioStart350, ratioStart15C, ratioStart2C)

#Look at 1850 / 1960 overshoot ratios by country
ggsave("ratioOvershoot1850over1960start.png",
  ggplot(ratioStart, 
    aes(x=reorder(factor(country), ratio1850over1960, sum), y=ratio1850over1960)) +
	geom_col(width=0.5, col="darkred") +
	geom_hline(yintercept = 1, linetype="dashed", col="grey50") +
	scale_y_continuous(labels = scales::percent) +
	coord_flip() +
	#geom_abline(intercept=0, slope=1) +
	facet_wrap(~ boundary2019, scales="free") +
	labs(x ="Country", y="Ratio overshoot 1960-2019 relative to 1850-2019 (100% = parity)") +
  	  theme_chart_SMALLM + 
	  theme(axis.text = element_text(size=6, family="sans", color="#666666")),
width = 160, height = 300, dpi = 300, units="mm", device="png")


