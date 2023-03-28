# Mitigation rate SI figure

#rCO2 <- read_csv("./myData/4_netZeroMitigationRates_v3.csv")

#Look at bar chart of mitigation rates
ggsave("./Figures/FigureS8_netZeroMitigationRates_v5.pdf",
	ggplot(rCO2, aes(x=reorder(factor(country), -rCO2, sum), y=rCO2)) +
  	  geom_col(width=0.5, col="darkred") +
  	  coord_flip() +
  	  labs(x ="Country", y="Net zero by 2050 mitigation rate (%)") +
  	  theme_chart_SMALLM + 
	  theme(axis.text = element_text(size=6, family="sans", color="#666666"),
		axis.title = element_text(size=7, family="sans", color="#666666")),
width = 183, height = 287, units="mm", device="pdf")
