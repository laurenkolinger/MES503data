###########################
# Lab Activity Week 3: Lab assignment pivot tables done in R
# Author: Lauren Olinger
# Date: 2 Sept 2024
###########################

# set working directory 
# setwd("/Users/laurenkay/POSTDOC_UVI/courses/MES503_F23/503Demos")

# load libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(httr)

# download excel file from github and read in as dat
url <- "https://github.com/laurenkolinger/MES503data/raw/main/week1/TCRMP-RAPID-Dec2017-Health-intercept.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file))
dat <- read_excel(temp_file, sheet = "DATA")

# Create a summary of the length data for the specified species
length_summary <- dat %>% 
  filter(SPP %in% c("AA", "OA", "OFAV", "OFRA", "PA", "SS")) |>  # Filter rows to include only the specified species
  group_by(SPP) |>  # Group data by species
  summarise (
    mean_length = mean(LENGTH),
    sd_length = sd(LENGTH),
    SEM_length = sd(LENGTH)/sqrt(length(LENGTH))
  )  # Compute summary statistics: mean, standard deviation, and standard error of mean for length

# Plot the average length of the specified species with error bars
ggplot(length_summary, aes(x=SPP,y=mean_length))+
  geom_bar(stat="identity") +  # Create bars for mean length
  geom_errorbar(aes(ymin = mean_length-SEM_length, ymax = mean_length+SEM_length), width =0.2) +  # Add error bars 
  xlab("species") +    # Label the x-axis
  ylab("length (cm)") +   # Label the y-axis
  ggtitle("average length of 6 common coral species ± SEM")  # Add a title to the plot

# Create a summary of the count and percentage of observations for each transect and species
prev_summary <- dat |> 
  group_by(TRANSECT, SPP) |>   # Group data by transect and species
  summarise(count = n()) |>    # Compute count of observations for each group
  mutate(percentage = (count/sum(count))*100) |>    # Compute the percentage of observations for each group relative to the total count
  filter(SPP=="MC")  # Filter rows to include only the species "MC"

prev_summary



      # no pipe used: 
      
      # Filter rows to include only the specified species
      filtered_dat <- filter(dat, SPP %in% c("AA", "OA", "OFAV", "OFRA", "PA", "SS"))
      
      # Group data by species
      grouped_dat <- group_by(filtered_dat, SPP)
      
      # Compute summary statistics: mean, standard deviation, and standard error of mean for length
      length_summary <- summarise(
        grouped_dat, 
        mean_length = mean(LENGTH),
        sd_length = sd(LENGTH),
        SEM_length = sd(LENGTH) / sqrt(length(LENGTH))
      )

      # split up ggplot step by step
      
      # Create the base plot
      plot <- ggplot(length_summary, aes(x=SPP, y=mean_length))
      
      # Add bars for mean length
      plot <- plot + geom_bar(stat="identity")
      
      # Add error bars
      plot <- plot + geom_errorbar(aes(ymin = mean_length-SEM_length, ymax = mean_length + SEM_length), width=0.2)
      
      # Label the x-axis
      plot <- plot + xlab("species")
      
      # Label the y-axis
      plot <- plot + ylab("mean length")
      
      # Add a title to the plot
      plot <- plot + ggtitle("average length of 6 common coral species ± SEM")
      
      # Print the plot
      print(plot)
