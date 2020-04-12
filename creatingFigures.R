#Script for creating figures for the twitter data
library(ggplot2)
library(dplyr)


#function to graph line graph
create.linegraph <- function(laFile,nycFile,graphName,outputName){
  avgPolLA <- read.csv(laFile)
  avgPolNY <- read.csv(nycFile)
  #make line graph
  #creates one dategrame with the two relevant csv files 
  dfLA <- data.frame(avgPolLA)
  dfLA <- mutate(dfLA, City = "L.A.")
  dfNY <- data.frame(avgPolNY)
  dfNY <- mutate(dfNY, City = "N.Y.C.")
  avgPol = rbind(dfNY,dfLA) #combines csv files into one dataframe
  
  g <- ggplot() + #makes and saves graph to defined directory
    geom_line(data=avgPol, aes(x=test_num, y=avg_sentiment_polarity, group=City,color=City))+
    geom_point(color="City") + #color the lines by city
    labs(x = "Test Number", y = "Average Sentiment Polarity") +
    ggtitle(graphName) +
    coord_cartesian(ylim = c(-1, 1))+ 
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    theme(legend.position="right")
  
  ggsave(g,filename = paste0("C:/Users/matth/Desktop/School/4-Senior/spring20/CSC450/Project/figures/figs/",outputName))
}

### Averages############################################################################################################
#avg day 1
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day1/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day1/avgPol.csv",
                 "Average Polarites for Day 1, March 31st Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day1.png")
#avg day 2
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day2/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day2/avgPol.csv",
                 "Average Polarites for Day 2, April 1st Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day2.png")
#avg day 3
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day3/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day3/avgPol.csv",
                 "Average Polarites for Day 3, April 2st Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day3.png")
#avg day 4
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day4/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day4/avgPol.csv",
                 "Average Polarites for Day 4, April 3t Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day4.png")
#avg day 5
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day5/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day5/avgPol.csv",
                 "Average Polarites for Day 5, April 4st Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day5.png")
#avg day 6
create.linegraph("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day6/avgPol.csv",
                 "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day6/avgPol.csv",
                 "Average Polarites for Day 6, April 5st Starting at 5:00pm",
                 "Average_Sentment_LAandNYC_Day6.png")

#function to make boxplots
create.boxplot <- function(laFile,nycFile,graphName,outputName){
  la<- read.csv(laFile)
  nyc<- read.csv(nycFile)
  #adds city column to data
  la <- mutate(la, City = 'L.A.') 
  nyc <- mutate(nyc, City = 'N.Y.C.')
  #puts lists together
  pols <- rbind(la,nyc) 
  #makes boxplot for first day
  g <- ggplot(pols) + 
    geom_boxplot(aes(City, polarities, fill = City))+
    labs(x = "City", y = "Sentiment Polarity")+
    ggtitle(graphName) +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    coord_cartesian(ylim = c(-1, 1))
  ggsave(g,filename = paste0("C:/Users/matth/Desktop/School/4-Senior/spring20/CSC450/Project/figures/figs/",outputName))
}

#### Day 1 ###############################################################################################################
#day 1 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day1/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day1/polaritiesTest1.csv",
               "Polarities for March 31st, 5:00pm",
               "Polarities_day1_test1.png")
#day 1 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day1/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day1/polaritiesTest2.csv",
               "Polarities for March 31st, 5:30pm",
               "Polarities_day1_test2.png")
#day 1 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day1/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day1/polaritiesTest3.csv",
               "Polarities for March 31st, 6:00pm",
               "Polarities_day1_test3.png")
#day 1 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day1/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day1/polaritiesTest4.csv",
               "Polarities for March 31st, 6:30pm",
               "Polarities_day1_test4.png")

#### Day 2 ###############################################################################################################
#day 2 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day2/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day2/polaritiesTest1.csv",
               "Polarities for April 1st, 5:00pm",
               "Polarities_day2_test1.png")
#day 2 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day2/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day2/polaritiesTest2.csv",
               "Polarities for April 1st, 5:30pm",
               "Polarities_day2_test2.png")
#day 2 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day2/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day2/polaritiesTest3.csv",
               "Polarities for April 1st, 6:00pm",
               "Polarities_day2_test3.png")
#day 2 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day2/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day2/polaritiesTest4.csv",
               "Polarities for April 1st, 6:30pm",
               "Polarities_day2_test4.png")

#### Day 3 ##############################################################################################################
#day 3 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day3/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day3/polaritiesTest1.csv",
               "Polarities for April 2nd, 5:00pm",
               "Polarities_day3_test1.png")
#day 3 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day3/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day3/polaritiesTest2.csv",
               "Polarities for April 2nd, 5:30pm",
               "Polarities_day3_test2.png")
#day 3 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day3/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day3/polaritiesTest3.csv",
               "Polarities for April 2nd, 6:00pm",
               "Polarities_day3_test3.png")
#day 3 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day3/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day3/polaritiesTest4.csv",
               "Polarities for April 2nd, 6:30pm",
               "Polarities_day3_test4.png")


#### Day 4 ##############################################################################################################
#day 4 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day4/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day4/polaritiesTest1.csv",
               "Polarities for April 3rd, 5:00pm",
               "Polarities_day4_test1.png")
#day 4 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day4/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day4/polaritiesTest2.csv",
               "Polarities for April 3rd, 5:30pm",
               "Polarities_day4_test2.png")
#day 4 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day4/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day4/polaritiesTest3.csv",
               "Polarities for April 3rd, 6:00pm",
               "Polarities_day4_test3.png")
#day 4 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day4/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day4/polaritiesTest4.csv",
               "Polarities for April 3rd, 6:30pm",
               "Polarities_day4_test4.png")


#### Day 5 ##############################################################################################################
#day 5 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day5/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day5/polaritiesTest1.csv",
               "Polarities for April 4th, 5:00pm",
               "Polarities_day5_test1.png")
#day 5 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day5/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day5/polaritiesTest2.csv",
               "Polarities for April 4th, 5:30pm",
               "Polarities_day5_test2.png")
#day 5 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day5/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day5/polaritiesTest3.csv",
               "Polarities for April 4th, 6:00pm",
               "Polarities_day5_test3.png")
#day 5 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day5/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day5/polaritiesTest4.csv",
               "Polarities for April 4th, 6:30pm",
               "Polarities_day5_test4.png")

#### Day 6 ##############################################################################################################
#day 6 test 1
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day6/polaritiesTest1.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day6/polaritiesTest1.csv",
               "Polarities for April 5th, 5:00pm",
               "Polarities_day6_test1.png")
#day 6 test 2
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day6/polaritiesTest2.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day6/polaritiesTest2.csv",
               "Polarities for April 5th, 5:30pm",
               "Polarities_day6_test2.png")
#day 6 test 3
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day6/polaritiesTest3.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day6/polaritiesTest3.csv",
               "Polarities for April 5th, 6:00pm",
               "Polarities_day6_test3.png")
#day 6 test 4
create.boxplot("https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/laData/day6/polaritiesTest4.csv",
               "https://raw.githubusercontent.com/MMcClureECSU/senior_research/master/nycData/day6/polaritiesTest4.csv",
               "Polarities for April 5th, 6:30pm",
               "Polarities_day6_test4.png")
