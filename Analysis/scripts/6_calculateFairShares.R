## This script calculates cumulative national fair shares of global boundaries from 
# 1850 and 1960 for 350 ppm, 1.5C and 2C

#Read in datafile from previous script (if needed)
#myData17 <- read_csv("./myData/05_cumCO2andBoundaries_v3.csv")
#--------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# Calculate national fair shares from 1850, 1960 and 1992 for each country (and world)
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

#calculate global average population for different countries
nationalPop1850 <- myData17 %>%
	group_by(country, iso3c) %>%
	summarise(avgPop_18502050 = mean(population, na.rm=T),
		budget350_1850 = mean(budget350_1850, na.rm =T),
		budget15C_1850 = mean(budget15C_1850, na.rm =T),
		budget2C_1850 = mean(budget2C_1850, na.rm =T)) %>%
	ungroup()

nationalPop1960 <- myData17 %>%
	filter(date >= 1960) %>%
	group_by(country, iso3c) %>%
	summarise(avgPop_19602050 = mean(population, na.rm=T),
		budget350_1960 = mean(budget350_1960, na.rm =T),
		budget15C_1960 = mean(budget15C_1960, na.rm =T),
		budget2C_1960 = mean(budget2C_1960, na.rm =T)) %>%
	ungroup()

nationalPop <- left_join(nationalPop1850, nationalPop1960, by=c("country", "iso3c"))

nationalPop1992 <- myData17 %>%
	filter(date >= 1992) %>%
	group_by(country, iso3c) %>%
	summarise(avgPop_19922050 = mean(population, na.rm=T),
		budget15C_1992 = mean(budget15C_1992, na.rm =T),
		budget2C_1992 = mean(budget2C_1992, na.rm =T)) %>%
	ungroup()

nationalPop1 <- left_join(nationalPop, nationalPop1992, by=c("country", "iso3c"))


rm(nationalPop1850, nationalPop1960, nationalPop1992, nationalPop)

worldPop <- nationalPop1 %>%
	filter(iso3c == "WLD") %>%
	select(avgPop_18502050, avgPop_19602050, avgPop_19922050)

nationalPop2 <- nationalPop1 %>% 
	add_column(avgWLDpop18502050 = worldPop$avgPop_18502050,
		avgWLDpop19602050 = worldPop$avgPop_19602050,
		avgWLDpop19922050 = worldPop$avgPop_19922050)


rm(worldPop, nationalPop1)

#Calculate fair shares
fairShares <- nationalPop2 %>%
	rowwise() %>%
	mutate(popShare_1850 = avgPop_18502050/avgWLDpop18502050,
		popShare_1960 = avgPop_19602050/avgWLDpop19602050,
		popShare_1992 = avgPop_19922050/avgWLDpop19922050,
		fairShare350_1850 = budget350_1850*(avgPop_18502050/avgWLDpop18502050),
		fairShare15C_1850 = budget15C_1850*(avgPop_18502050/avgWLDpop18502050),
		fairShare2C_1850 = budget2C_1850*(avgPop_18502050/avgWLDpop18502050),
		fairShare350_1960 = budget350_1960*(avgPop_19602050/avgWLDpop19602050),
		fairShare15C_1960 = budget15C_1960*(avgPop_19602050/avgWLDpop19602050),
		fairShare2C_1960 = budget2C_1960*(avgPop_19602050/avgWLDpop19602050),
		fairShare15C_1992 = budget15C_1992*(avgPop_19922050/avgWLDpop19922050),
		fairShare2C_1992 = budget2C_1992*(avgPop_19922050/avgWLDpop19922050)) %>%
	ungroup() %>%
	select(country, iso3c, popShare_1850, popShare_1960, popShare_1992,
	  fairShare350_1850, fairShare350_1960, fairShare15C_1850, fairShare15C_1960, fairShare15C_1992, 
	  fairShare2C_1850, fairShare2C_1960, fairShare2C_1992)

#Prep for visualising facets
popShare <- fairShares %>%
	select(country, iso3c, fairShare_1850 = popShare_1850, fairShare_1960 = popShare_1960,
	  fairShare_1992 = popShare_1992) %>%
	add_column(group="populationShare")

fairShare350 <- fairShares %>%
	select(country, iso3c, fairShare_1850 = fairShare350_1850, fairShare_1960 = fairShare350_1960) %>%
	add_column(fairShare_1992 = NA, group="fairShare350")

fairShare15C <- fairShares %>%
	select(country, iso3c, fairShare_1850 = fairShare15C_1850, fairShare_1960 = fairShare15C_1960,
	  fairShare_1992 = fairShare15C_1992) %>%
	add_column(group="fairShare15C")

fairShare2C <- fairShares %>%
	select(country, iso3c, fairShare_1850 = fairShare2C_1850, fairShare_1960 = fairShare2C_1960,
	  fairShare_1992 = fairShare2C_1992) %>%
	add_column(group="fairShare2C")

fairShares1 <- rbind(popShare, fairShare350, fairShare15C, fairShare2C) %>%
	relocate(group, .before=fairShare_1850)

rm(popShare, fairShare350, fairShare15C, fairShare2C)

#Calculate ratios between different periods
fairShares2 <- fairShares1 %>% 
	rowwise() %>%
	mutate(ratio19601850 = fairShare_1960 / fairShare_1850,
	  ratio19921850 = fairShare_1992 / fairShare_1850) %>%
	ungroup()

test <- fairShares2 %>% filter(iso3c != "WLD", group == "populationShare")

#Visualise	
ggsave("avgPopSharesComparison92.png",
  ggplot(fairShares2 %>% filter(iso3c != "WLD", group == "populationShare"), 
    aes(x=reorder(factor(country), ratio19921850, sum), y=ratio19921850)) +
	geom_col(width=0.5, col="darkred") +
	geom_hline(yintercept = 1, linetype="dashed", col="grey50") +
	scale_y_continuous(labels = scales::percent) +
	coord_flip() +
	#geom_abline(intercept=0, slope=1) +
	facet_wrap(~ group, scales="free") +
	labs(x ="Country", y="Ratio population share 1992-2050 relative to 1850-2050 (100% = equality)") +
  	  theme_chart_SMALLM + 
	  theme(axis.text = element_text(size=6, family="sans", color="#666666")),
width = 150, height = 300, dpi = 300, units="mm", device="png")

#-------------------------------------------------------------------------------------------------------
#		MERGE FAIRSHARES WITH MYDATA AND WRITE TO FILE
#-------------------------------------------------------------------------------------------------------

myData18 <- left_join(myData17, fairShares, by=c("country", "iso3c"))

#write_csv(myData18, "./myData/06_myDataCO2andFairShares_v3.csv")
#write_csv(fairShares2, "./myData/06_popFairSharesByCountry-1850-1960-1992_v3.csv")
rm(fairShares, fairShares1, fairShares2, myData17)



