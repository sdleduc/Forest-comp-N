library(dplyr)
library(tidyr)
library(ggplot2)
##read in csv file
forest=read.csv("forest_comp_2005-2100_county.csv", header=TRUE)
head(forest)
str(forest)
###subset out 2100 and 2005 data, for only climate scenario 4 
forest.2005 = filter(forest, yr==2005, climate_scenario == 4)
head(forest.2005)
forest.2100 = filter(forest, yr==2100, climate_scenario == 4)
head(forest.2100)

##wrote them to own .csv files
write.csv(forest.2005, "forest_2005.csv")
write.csv(forest.2100, "forest_2100.csv")

##read them back in
Forest.2005=read.csv("forest_2005.csv", header=TRUE)
Forest.2100=read.csv("forest_2100.csv", header=TRUE)

##Merged based on fips, Ndep scenario, spid
forest.merge1=merge(Forest.2005, Forest.2100, by=c("fips", "ndep_scenario", "spid"))
head(forest.merge1)

##Changed column names
colnames(forest.merge1)[colnames(forest.merge1) == 'biomass_kg.x'] <- 'biomass_kg.2005'
colnames(forest.merge1)[colnames(forest.merge1) == 'biomass_kg.y'] <- 'biomass_kg.2100'
colnames(forest.merge1)[colnames(forest.merge1) == 'stems.x'] <- 'stems.2005'
colnames(forest.merge1)[colnames(forest.merge1) == 'stems.y'] <- 'stems.2100'

head(forest.merge1)

##selected out unwanted columns
forest.merge2 = select(forest.merge1, -X.x, -climate_scenario.x, -yr.x, -X.y, -yr.y, -climate_scenario.y)
head(forest.merge2)

##added a column minusing the biomass and stem counts
forest.merge3<- mutate(forest.merge2,
                           delta_biomass = biomass_kg.2100-biomass_kg.2005)
forest.merge4<- mutate(forest.merge3,
                       delta_stems = stems.2100-stems.2005)
head(forest.merge4)

##wrote this file out to a csv 
write.csv(forest.merge4, "forest_merge.2100_2005.csv")

##designated labels for each facet by writing a function##

ndep_names = list('1'="2025 Red", '2'="Pre-Euro", '3'="Constant")
ndep_labeller =function(variable, value) {return(ndep_names[value])}

##plotted change in biomass by species for each of three scenarios"
ggplot(data = forest.merge4,
                          aes(x = spid, y = delta_biomass)) +
  geom_boxplot() +
  geom_point(stat = "summary",
             fun.y = "mean",
             color = "red") +
  facet_grid(ndep_scenario~., labeller=ndep_labeller)+
  labs(title="Tree N Response", x="Species", y="Change in Biomass (kg), 2100-2005")+
  theme_minimal()
