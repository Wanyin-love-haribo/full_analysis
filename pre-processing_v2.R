#========================================================================================================#
#                                            load the packages                                           #
#========================================================================================================#
# If you haven't installed tidyverse, please uncomment the following line
# install.packages("tidyverse")

# load the package tidyverse
library(tidyverse)
library(lme4)


#========================================================================================================#
#                                     Import Eye-tracking data (text)                                    #
#========================================================================================================#
GECOs <- read.delim(file.choose(),header = TRUE, sep = "\t", encoding = "UTF-8", quote = "", na.strings=".")

#------------ total reading time ------------# 
GECOs$IA_DWELL_TIME_100 <- GECOs$IA_DWELL_TIME
GECOs$IA_DWELL_TIME_100[GECOs$IA_DWELL_TIME_100 == 0] <- NA
GECOs$IA_DWELL_TIME_100[GECOs$IA_DWELL_TIME_100 <100 ] <- NA

GECOs$IA_FIRST_FIXATION_DURATION[GECOs$IA_FIRST_FIXATION_DURATION <100] <- NA
GECOs$IA_FIRST_RUN_DWELL_TIME[GECOs$IA_FIRST_RUN_DWELL_TIME <100] <- NA
GECOs$IA_REGRESSION_PATH_DURATION[GECOs$IA_REGRESSION_PATH_DURATION <100] <- NA
GECOs$IA_SELECTIVE_REGRESSION_PATH_DURATION[GECOs$IA_SELECTIVE_REGRESSION_PATH_DURATION <100] <- NA



LEXTALE <- read.delim(file.choose(),header = TRUE, sep = "\t", encoding = "UTF-8", quote = "", na.strings=".")

Data_full <-GECOs %>% left_join(LEXTALE )

Data_full$MWS_Frequency [Data_full$MWS_Frequency == 0] <- 1
Data_full$Adj_Word_Frequency  [Data_full$Adj_Word_Frequency == 0] <- 1
Data_full$Noun_Word_Frequency [Data_full$Noun_Word_Frequency == 0] <- 1

#========================================================================================================#
#                                 Exclude outliers based on standard deviation                           #
#========================================================================================================#
# Encode the sd value that we want to treat as outliers
# e.g. values over 2.5 sd will be treated as outliers
sd_value <- 2.5

# Here we calculate the +- 2.5 sd values of each participant in each language condition.
sd <- Data_full %>% group_by( PP_ID ) %>%    
  dplyr::summarise(n = n(),  
                   a_min = mean(IA_FIRST_FIXATION_DURATION, na.rm = TRUE) - sd_value * sd(IA_FIRST_FIXATION_DURATION, na.rm = TRUE), 
                   a_max = mean(IA_FIRST_FIXATION_DURATION, na.rm = TRUE) + sd_value * sd(IA_FIRST_FIXATION_DURATION, na.rm = TRUE),
                   b_min = mean(IA_FIRST_RUN_DWELL_TIME, na.rm = TRUE) - sd_value * sd(IA_FIRST_RUN_DWELL_TIME, na.rm = TRUE), 
                   b_max = mean(IA_FIRST_RUN_DWELL_TIME, na.rm = TRUE) + sd_value * sd(IA_FIRST_RUN_DWELL_TIME, na.rm = TRUE),
                   c_min = mean(IA_SELECTIVE_REGRESSION_PATH_DURATION, na.rm = TRUE) - sd_value * sd(IA_SELECTIVE_REGRESSION_PATH_DURATION, na.rm = TRUE), 
                   c_max = mean(IA_SELECTIVE_REGRESSION_PATH_DURATION, na.rm = TRUE) + sd_value * sd(IA_SELECTIVE_REGRESSION_PATH_DURATION, na.rm = TRUE),
                   d_min = mean(IA_DWELL_TIME_100, na.rm = TRUE) - sd_value * sd(IA_DWELL_TIME_100, na.rm = TRUE), 
                   d_max = mean(IA_DWELL_TIME_100, na.rm = TRUE) + sd_value * sd(IA_DWELL_TIME_100, na.rm = TRUE),
                   e_min = mean(IA_REGRESSION_PATH_DURATION, na.rm = TRUE) - sd_value * sd(IA_REGRESSION_PATH_DURATION, na.rm = TRUE), 
                   e_max = mean(IA_REGRESSION_PATH_DURATION, na.rm = TRUE) + sd_value * sd(IA_REGRESSION_PATH_DURATION, na.rm = TRUE) 
  ) %>% ungroup()  # Ungroup all the time to avoid incorrect analysis in the future


# group data and + -sd into one file
Data_full_sd <- Data_full %>% left_join(sd)

# Ungroup all the time to avoid incorrect analysis in the future
Data_full_sd %>% ungroup()   


# Exclude outliers
Data_full_sd$IA_FIRST_FIXATION_DURATION            [Data_full_sd$IA_FIRST_FIXATION_DURATION             < Data_full_sd$a_min | Data_full_sd$IA_FIRST_FIXATION_DURATION             > Data_full_sd$a_max ] <- NA
Data_full_sd$IA_FIRST_RUN_DWELL_TIME               [Data_full_sd$IA_FIRST_RUN_DWELL_TIME                < Data_full_sd$b_min | Data_full_sd$IA_FIRST_RUN_DWELL_TIME                > Data_full_sd$b_max ] <- NA
Data_full_sd$IA_SELECTIVE_REGRESSION_PATH_DURATION [Data_full_sd$IA_SELECTIVE_REGRESSION_PATH_DURATION  < Data_full_sd$c_min | Data_full_sd$IA_SELECTIVE_REGRESSION_PATH_DURATION  > Data_full_sd$c_max ] <- NA
Data_full_sd$IA_DWELL_TIME_100                     [Data_full_sd$IA_DWELL_TIME_100                      < Data_full_sd$d_min | Data_full_sd$IA_DWELL_TIME_100                      > Data_full_sd$d_max ] <- NA
Data_full_sd$IA_REGRESSION_PATH_DURATION           [Data_full_sd$IA_REGRESSION_PATH_DURATION            < Data_full_sd$e_min | Data_full_sd$IA_REGRESSION_PATH_DURATION            > Data_full_sd$e_max ] <- NA

# Remove un-used data 
Data_all <- Data_full_sd %>% select(-c(n, a_min, a_max, c_min, c_max,
                                       d_min, d_max, e_min, e_max) ) 

sum(Data_all$GROUP =="Monolinguals")
sum(Data_all$GROUP =="Bilinguals")



#========================================================================================================#
#                                             Scale Variables                                            #
#========================================================================================================#
Data_all$GROUP_sum <- as.factor(Data_all$GROUP)
Data_all <- filter(Data_all, PP_ID != "pp24")

contrasts(Data_all$GROUP_sum) <- contr.sum(2)
contrasts(Data_all$GROUP_sum)

# Frequency
Data_all$MWS_Frequency_scale_center<- c(scale(Data_all$MWS_Frequency, scale = TRUE, center = TRUE))
Data_all$Adj_Word_Frequency_scale_center<- c(scale(Data_all$Adj_Word_Frequency, scale = TRUE, center = TRUE))
Data_all$Noun_Word_Frequency_scale_center<- c(scale(Data_all$Noun_Word_Frequency, scale = TRUE, center = TRUE))

# Length
Data_all$MWS_Length_CENTER <- c(scale(Data_all$MWS_Length, scale = TRUE, center = TRUE))

# Repetition
#Data_all$MWS_Repetition_Within_One_Session_CENTER <- c(scale(Data_all$MWS_Repetition_Within_One_Session, scale = TRUE, center = TRUE))
#Data_all$Adj_Word_Repetition_Within_One_Session_CENTER <- c(scale(Data_all$Adj_Word_Repetition_Within_One_Session, scale = TRUE, center = TRUE))
#Data_all$Noun_Word_Repetition_Within_One_Session_CENTER <- c(scale(Data_all$Noun_Word_Repetition_Within_One_Session, scale = TRUE, center = TRUE))


Data_all$MWS_Repetition_Across_Sessions_CENTER <- c(scale(Data_all$MWS_Repetition_Across_Sessions, scale = TRUE, center = TRUE))
Data_all$Adj_Word_Repetition_Across_Sessions_CENTER <- c(scale(Data_all$Adj_Word_Repetition_Across_Sessions, scale = TRUE, center = TRUE))
Data_all$Noun_Word_Repetition_Across_Sessions_CENTER <- c(scale(Data_all$Noun_Word_Repetition_Across_Sessions, scale = TRUE, center = TRUE))

# Proficiency
Data_all$Proficiency_CENTER <- c(scale(Data_all$LEXTALE_ENG, scale = TRUE, center = TRUE))


#========================================================================================================#
#                                         Log Fixation Duration                                          #
#========================================================================================================#
Data_all$IA_DWELL_TIME_100_LOG<- log(Data_all$IA_DWELL_TIME_100)
Data_all$IA_FIRST_FIXATION_DURATION_LOG<- log(Data_all$IA_FIRST_FIXATION_DURATION )
Data_all$IA_FIRST_RUN_DWELL_TIME_LOG<- log(Data_all$IA_FIRST_RUN_DWELL_TIME)
Data_all$IA_REGRESSION_PATH_DURATION_LOG<- log(Data_all$IA_REGRESSION_PATH_DURATION)
Data_all$IA_SELECTIVE_REGRESSION_PATH_DURATION_LOG<- log(Data_all$IA_SELECTIVE_REGRESSION_PATH_DURATION)


# Get ready for plots
transparent_degree <- 0.2
point_size <- 3
line_size <- 1