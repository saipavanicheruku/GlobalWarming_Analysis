###########  1)
library(data.table)
library(tidyverse)
require(data.table)
mydata1 = fread("yale_climate_cty_data.csv",
                stringsAsFactors = F,
                data.table = F,
                colClasses=list(character=c(1)))
mydata2 = fread("ACS_16_5YR_DP05_with_ann.csv",
                stringsAsFactors = F,
                data.table = F)
mydata3 = fread("ACS_16_5YR_DP05_metadata.csv",
                stringsAsFactors = F,
                data.table = F)

##################
str(mydata1)
str(mydata2)
mydata2 = mydata2 %>%
  rename(GeoName = 'GEO.display-label')

mergedDataSet = inner_join(x = mydata1, y=mydata2, by="GeoName")
str(mergedDataSet)

#mainDataSet$whiteAlonePctage = mainDataSet$whiteAloneCount / mainDataSet$population
#mydata= merge(mydata1, mydata2)

str(mergedDataSet)
#str(mydata)
#######################Q3
names(mergedDataSet)
mergedDataSet = mergedDataSet %>%
  rename(County_ID ='cty_FIPS')
mergedDataSet = mergedDataSet %>%
  rename(Total_Population ='HC01_VC03')
mergedDataSet = mergedDataSet %>%
  rename(White_Population ='HC01_VC49')
mergedDataSet = mergedDataSet %>%
  rename(Black_Population ='HC01_VC50')
mergedDataSet = mergedDataSet %>%
  rename(Hispanic_Population ='HC01_VC88')
mergedDataSet = mergedDataSet %>%
  rename(Asian_Population ='HC01_VC56')
mergedDataSet = mergedDataSet %>%
  rename(Median_Age ='HC01_VC23')
mergedDataSet = mergedDataSet %>%
  rename(Female_Population ='HC01_VC05')
mergedDataSet = mergedDataSet %>%
  rename(percentage_of_Respondents_that_believe_global_warming_is_Happening ='happening')
mergedDataSet = mergedDataSet %>%
  rename(percenatge_of_Respondents_that_believe_global_warming_is_caused_by_human_activities ='human')
mergedDataSet = mergedDataSet %>%
  rename(percentage_of_Respondent_that_are_somewhat_very_worried_about_global_warming = 'worried')
#######################Question4

mergedDataSet$Total_Population =
  strtoi(mergedDataSet$Total_Population)
mergedDataSet$White_Population =
  strtoi(mergedDataSet$White_Population)
mergedDataSet$Black_Population =
  strtoi(mergedDataSet$Black_Population)
mergedDataSet$Hispanic_Population =
  strtoi(mergedDataSet$Hispanic_Population)
mergedDataSet$Asian_Population =
  strtoi(mergedDataSet$Asian_Population)
mergedDataSet$Female_Population =
  strtoi(mergedDataSet$Female_Population)


mergedDataSet =  mutate(mergedDataSet,
                        white_percentage = (White_Population/ Total_Population)*100,
                        black_percentage = (Black_Population / Total_Population)*100,
                        hispanic_percentage = (Hispanic_Population / Total_Population)*100,
                        asian_percentage = (Asian_Population / Total_Population)*100,
                        female_percentage = (Female_Population / Total_Population)*100)

summary(mergedDataSet)
#######################Question6
mydata4 = fread("Region.csv",
                stringsAsFactors = F,
                data.table = F)

mergedDataSet$State_FIPS = substr(mergedDataSet$County_ID, 0, 2) 

str(mydata4)
str(mergedDataSet$State_FIPS)

mergedDataSet$State_FIPS = 
  strtoi(mergedDataSet$State_FIPS)
glimpse(mergedDataSet)

finalMergedDataSet = inner_join(x = mergedDataSet, y=mydata4, by="State_FIPS")
glimpse(finalMergedDataSet)

################Question7
byState =
  finalMergedDataSet %>%
  group_by(State) %>%
  summarize(avg_percent = mean(percentage_of_Respondents_that_believe_global_warming_is_Happening))

arrange(byState, desc(avg_percent))

###############Question8
byRegion =
  finalMergedDataSet %>%
  group_by(Region) %>%
  summarize(avg_percent = mean(percenatge_of_Respondents_that_believe_global_warming_is_caused_by_human_activities))

(byRegion = arrange(byRegion, desc(avg_percent)))

###############Question9
#library(tidyverse)
finalMergedDataSet$white_percentage =  as.double(finalMergedDataSet$white_percentage)
str(finalMergedDataSet$white_percentage)
#finalMergedDataSet$white_percentage = formatC(finalMergedDataSet$white_percentage, digits = 0, format = "f")
stop_dist_model = lm(white_percentage ~ percentage_of_Respondent_that_are_somewhat_very_worried_about_global_warming, data = finalMergedDataSet)

ggplot(finalMergedDataSet, aes(y = percentage_of_Respondent_that_are_somewhat_very_worried_about_global_warming, x = white_percentage, color = Region, shape = Region)) +
  geom_point() + 
  geom_smooth(method="lm",color="black",size=1.5,aes(fill=Region))
  facet_wrap(~Region)
#  geom_abline(slope=slope, intercept=intercept.energy, color="red") +
#  geom_abline(slope=slope, intercept=intercept.utilities)
