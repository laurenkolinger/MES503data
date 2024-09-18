###########################
# Lab Activity Week 3: Summarizing TCRMP Fish Data
# Author: 
# Date: 
###########################

# Introduction -------------------------------------------------------------------------

# This exercise focuses on the analysis of some fish count data from TCRMP fish surveys. Fish surveys have been historically conducted at 14 sites around St. Croix and 10 sites around St. Thomas and, starting in 2012, are conducted at 32 of the 33 monitoring site. Ten replicate belt transects and three replicate roving dive surveys are conducted at each site. Belt transects are 25 x 4 m and are conducted in 15 minutes per replicate. Roving replicates are also 15 minutes. All fish encountered are recorded except blennies and most gobies. In this dataset, we are looking at counts of these fish data from sites in 2014 and 2015. You will compare:

# - fish abundance at Sprat Hole and College Shoal East from surveys conducted in 2014

# - fish diversity (defined as the number of unique species) at Jacks Bay and Hind Bank East FSA from surveys conducted in 2015.

# This week we are doing some exploratory data analysis (EDA), but we will add to this next week, and it will be your lab 2 assignment. In many ways this approach mirrors your data analysis, where you will spend many days/weeks on a single dataset to learn as much about it as you can.

# Initial Setup --------------------------------

# Example for Setting Working Directory
    setwd("YOUR_WORKING_DIRECTORY")

# Example for Loading Packages
    library(dplyr)
    library(ggplot2)

# Data Import --------------------------------

# 1. Import the dataset using read.csv() and give it a descriptive name. You can do this by downloading directly from github in the script (there is a command for this in the week 3 manual), or by manually downloading from github, saving to your working directory, and loading from there. You can view you YOUR_DATA_HERE using the View() command, but be sure to comment this out before submitting your homeworks.

    YOUR_DATA_HERE <- read.csv(IMPORT_PATH)
    
    #View(YOUR_DATA_HERE)

# Preliminary Data Exploration and Summary --------------------------------

# 2. View the First Few Rows of the Dataset using head() . You see the columns? There are definitions for those columns in the associated metadata (see manual for link). A good idea would be to download the .txt file to store in your working directory for future reference (so you know what this is in the future).

    head(YOUR_DATA_HERE)

# 3.  Get a Summary of the Dataset using summary . What does this tell you?

    summary(YOUR_DATA_HERE)

    # Answer: the output of summary() shows ....

# 4. Get the Data Structure using str() or glimpse() . What does this tell you?

    glimpse(YOUR_DATA_HERE)

    # Answer: the output of glimpse() shows ....

# Data Cleaning --------------------------------
  
# 5.  **Remove Missing Values.** use this example code (replace `YOUR_DATA_HERE` with your data name) to see if any data are NA, and if so, remove them.

    # find out the indices of the NAs
    which(is.na(YOUR_DATA_HERE), arr.ind = T)
    
    # remove them using na.omit()
    YOUR_DATA_HERE <- 
      YOUR_DATA_HERE |> na.omit()
    
# did na.omit() work? run which(is.na(YOUR_DATA_HERE), arr.ind = T) to find out!
      
    # Answ er: na.omit did/did not work
    
    
# 6. aggregate (condense) counts by summing across species. A common practice with this kind of data is to reduce the amount of detail you want to work with in order to answer the question at hand. We have a bunch of cool data on species, but for this exercise we dont really need that level of detail!
      
    YOUR_DATA_HERE <- YOUR_DATA_HERE|>
      select(site, year, replicate, sppname, counts) |>
      group_by(site, year, replicate) |> 
      summarise(
        counts = sum(counts),
        diversity = length(sppname))
    

# Comparing fish abundance at Sprat Hole and College Shoal  --------

# 7. Filter Data to include 2014 surveys conducted at Sprat Hole and College Shoal East.

# Here we have some example code, that is slightly less filled in. Here you will have to reassign variables/values in uppercase letters to the variables/values you desire. Starting with a pretty straightforward example:
      
    # Example Command
    YOUR_DATA_HERE_ABUND <- YOUR_DATA_HERE |>
      filter(year == YEAR, site %in% c("SITE1", "SITE2"))   
    
# 8. Calculate some descriptive statistics for (i.e., summarize) Fish Abundance in 2014, including mean, standard deviation, and SEM.
    
# Think about the goal of this part of the activity. How do you want to group your data, so that you can compare the sites? If done correctly, YOUR_DATA_HERE_ABUND_SUM should only have two rows. Replace GROUPING_VAR with the name of the column corresponding to that grouping variable. Also, what is the measurement that we care about? replace MEASUREDVAR with the name of the column corresponding to that measured variable.

    # Example Command
    YOUR_DATA_HERE_ABUND_SUM <- YOUR_DATA_HERE_ABUND |>
      group_by(GROUPING_VAR) |>
      summarize(
        MEAN_ABUNDANCE = mean(MEASUREDVAR),
        SD_ABUNDANCE = sd(MEASUREDVAR),
        SEM_ABUNDANCE = sd(MEASUREDVAR) / sqrt(length(MEASUREDVAR))
      )
    
# 9. Visualize Fish Abundance in 2014 with Error Bars. Here is a basic command to start with. Don’t forget to label your axes, and include a descriptive title. You can look up how to do this very easily, for example see the ggplot cheat sheet (see manual).
    # also check out ??ggplot
    
    # Example Command
    ggplot(YOUR_DATA_HERE_ABUND_SUM, aes(x=GROUPING_VAR, y=MEAN_ABUNDANCE)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymin=MEAN_ABUNDANCE-SEM_ABUNDANCE, ymax=MEAN_ABUNDANCE+SEM_ABUNDANCE), width=.2)    
# 10. Interpret these differences in fish abundance between sites.
    
# Report the calculated means ± SEM as if you were reporting this in a publication, or your thesis:
      
    # Answer: eg “Surveys in YEAR resulted in an average (± SEM) of ### (± ###) fish at SITE1 and ### (± ###) fish at SITE2.”
    
# Which site had higher fish abundance? Do you think a statistical test would indicate significant differences in their abundances?
      
    # Answer: 
    
# (copy steps 7-10 to compare fsh diversity at Jacks Bay and Hind Bank East FSA in 2015)
      
# Save this script: you will add to it next week! 

# ALSO this script should be able to run smoothly on my computer! So make sure if you import data from your local folder that your copy of the data are in the same directory as your working directory. 