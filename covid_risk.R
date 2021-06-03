
currentNewCasesPerDay = 220 #Current as of 6/1/21
population = 6893000 #MA population
daysContagious = 10
riskofDeathFromDrivingOneMile <- (150 / 10000000000) #150 fatalities per 10 billion miles
toddlerRiskOfDeathPerInfection <- 4/88089 #Rough, from https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/COVID-19-Cases-by-Age-Group.aspx
chanceOfBreakthroughInfection <- 0.05 #Based on 95% Moderna efficacy, assumes that exposure without vaccine would be a 100% chance of infection
chanceOfInfectedParentTransmittingToToddler <- 1.0

##Assumptions:
# 1. The relative overall risk of serious adverse events from toddlers getting COVID vs driving is the same as the relative risk of death from driving and COVID
# 2. MA numbers are accurate. For safety, you could multiply by a factor of 10 to get a reasonable upper bound on true current infections


totalContagiousIndividuals <- currentNewCasesPerDay * daysContagious
chanceOfIndividualBeingContagious <- totalContagiousIndividuals / population #This seems to be ~2x as conservative as the method MA uses to estimate total active cases
riskOfDeathFromSinglePersonExposureOfVaccinatedPersonToToddler <- chanceOfIndividualBeingContagious * chanceOfBreakthroughInfection * toddlerRiskOfDeathPerInfection



##Charting:
library(ggplot2)
toddlerRiskTable <- data.frame(groups=c("Risk to Toddler From Vaccinated Parent Being Exposed to Random MA Individual", "Risk from Driving 1 Mile"),
                               risk=c(riskOfDeathFromSinglePersonExposureOfVaccinatedPersonToToddler*100, riskofDeathFromDrivingOneMile*100))

ggplot(toddlerRiskTable, aes(x=groups, y=risk)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=scales::percent(risk)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

