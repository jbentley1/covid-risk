library(ggplot2)

currentNewCasesPerDay = 200
population = 6893000
daysContagious = 10

totalContagiousIndividuals <- currentNewCasesPerDay * daysContagious
chanceOfIndividualBeingContagious <- totalContagiousIndividuals / population #This seems to be ~2x as conservative as the method MA uses to estimate total active cases

riskTable <- data.frame(attendees=1:70)
riskTable$chanceOfAtLeastOneContagiousAttendee <- 1-((1-chanceOfIndividualBeingContagious)^riskTable$attendees)
riskTable$expectedInfections <- riskTable$chanceOfAtLeastOneContagiousAttendee * riskTable$attendees

print(riskTable %>% mutate(percChanceOfAtLeastOneContagiousAttendee = chanceOfAtLeastOneContagiousAttendee*100))

ggplot(riskTable, aes(x=attendees, y=chanceOfAtLeastOneContagiousAttendee)) + geom_point() + geom_line()  + xlab("Number of people invited")

ggplot(riskTable, aes(x=attendees, y=expectedInfections)) +
  geom_line() +
  ylab("Expected number of new infections") +
  xlab("Number of attendees") +
  ggtitle("Expected number of new COVID infections when attending a party where each attendee coughs in the mouth of every other attendee as of 7/6/2020")



toddlerRiskOfDeathPerInfection <- 4/88089 #Rough, from https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/COVID-19-Cases-by-Age-Group.aspx

riskOfDeathFromSinglePersonExposureOfVaccinatedPersonToToddler <- chanceOfIndividualBeingContagious * 0.05 * toddlerRiskOfDeath

riskofDeathFromDrivingOneMile <- (150 / 10000000000) #150 fatalities per 10 billion miles


toddlerRiskTable <- data.frame(groups=c("Risk to Toddler From Vaccinated Parent Being Exposed to Random MA Individual", "Risk from Driving 1 Mile"),
                        risk=c(riskOfDeathFromSinglePersonExposureOfVaccinatedPersonToToddler*100, riskofDeathFromDrivingOneMile*100))

ggplot(toddlerRiskTable, aes(x=groups, y=risk)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=scales::percent(risk)), position=position_dodge(width=0.9), vjust=-0.25)


##Assumptions:
# 1. If a parent gets COVID, there is a 100% chance the toddler will get it
# 2. The relative overall risk from toddlers getting COVID is the same as the relative risk of death from driving and COVID
# 3. Assumes that the chance of a vaccinated person getting COVID from an exposure is 5% (this assumes it's such a strong exposure they would have gotten it if not vaccinated)
# 4. MA numbers are accurate

## All except (4) make the data skew towards COVID being less dangerous than the model suggests. Possibly scale risk 10x to compensate for (4).
