#recreate Table from Lancet Paper
#read in data directly from the web
#or as file
#we will use data from an older Lancet study
#
#load libraries
library(tidyverse)
library(formattable)
#read in data
my_data<-read_csv("IST_corrected.csv")
#directly from online source
#my_data<-read.csv(url("https://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_corrected.csv?sequence=5&isAllowed=y"))
#we will try to redo some of the results from the paper
#redo Table 1 from Lancet study
########
#recreate example from Table 1
#Gender balance
#######
### kl. Veränderung

table_dt<-   
  my_data %>% 
  group_by(SEX) %>% #create a table with a cell for each gender
  summarise(gender_no=n()) %>% #count cells
  mutate(gender_per= gender_no/sum(gender_no)) #add percentages
######
#another example
#delay until treatment
######
#create low and high bounds for categories in paper
#categiories are uneven which makes automatation problematic
break_seq_low<-c(0,4,7,13,25)
break_seq_high<-c(3,6,12,24,48)
#name the categories
names_seq<-NULL
for (i in 1:length(break_seq_low))
  names_seq<-c(names_seq,paste(break_seq_low[i],"-",break_seq_high[i],sep=""))
#create table
#note the use of the cut command to create categories
table_dt<-
  my_data %>% 
  #first create a variable with the labels for each category
  #think carefully why the group_by command comes only in the next line
  mutate(category=cut(RDELAY, breaks=c(0,break_seq_high), labels=names_seq)) %>% 
  group_by(category) %>% 
  summarise(delay_cases = n()) %>% 
  #a bit more elegant for the percentages to get the table nearer to the paper
  mutate(delay_per = paste("(",percent(delay_cases / sum(delay_cases)),")",sep=""))
######
#make your own table with age
#follow the above example
######
break_seq_low<-c(0,50,60,70,80)
break_seq_high<-c(49,59,69,79,120)
names_seq<-NULL
for (i in 1:length(break_seq_low))
  names_seq<-c(names_seq,paste(break_seq_low[i],"-",break_seq_high[i],sep=""))
names_seq[1]<-c("<50")
names_seq[5]<-c(">80")
#create table
table_dt<-
  my_data %>% 
  mutate(category=cut(AGE, breaks=c(0,break_seq_high), labels=names_seq)) %>% 
  group_by(category) %>% 
  summarise(age_cases = n()) %>% 
  mutate(age_per = paste("(",percent(age_cases / sum(age_cases)),")",sep=""))
######
#now for Table 2 
#outcome Deaths at 14 days
######
# first create Variables for Heparin (coded in RXHEP) and Aspirin (RXASP) that state whether Heparin or Aspirin was given
#use the ifelse statement!
#Create Variable for Hep/Asp
my_data$HEP<-ifelse((my_data$RXHEP=="N"),"No Heparin","Heparin")
my_data$ASP<-ifelse(my_data$RXASP=="Y","Aspirin","No Aspirin")
#recreate the upper part of Table 2
#Variable DDEADD gives the days at death of a patient after randomisation
# Is patient dead after 14 days? Code alive as 9
my_data$DEAD<-ifelse(my_data$DDEADD<=14,my_data$DDEADC,9)
#recode NAs as alive (again using 9 as a code)
my_data$DEAD<-ifelse(is.na(my_data$DEAD<=14),9,my_data$DEAD)
#take values for death and assign verbatim descriptions
my_data$DEAD_verb<-factor(my_data$DEAD,labels=c("Initial stroke","Recurrent ischaemic stroke","Haemorrhagic stroke","Pneumonia","Coronary heart disease","Pulmonary embolism","Other vascular", "Non-vascular","ALIVE"))
#create a results table
table_dt<-
  my_data %>% #use my_data as initial variable
  # We want a table that counts cases Reason of death vs Heparin (Y/N)
  group_by(HEP,DEAD_verb) %>% 
  #now take out some nuisance values (perhaps not needed)
  filter(!is.na(DEAD_verb)&DEAD_verb!="unknown") %>% 
  #calculate the number of cases per cell
  summarise(likely_cause=n())%>% 
  #transfrom this into wide format for the table
  spread(HEP,likely_cause)
#give a meaningful first column header
names(table_dt)[1]<-c("Deaths and likely causes")

#further exercises
#read some background on pipes at http://r4ds.had.co.nz/pipes.html
#explore the data with the six categories from Figure 1
#recreate the part of Table2 with outcomes at 6 Month








