##########################################useful libraries################################################################
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readr')) install.packages('readr'); library('readr')
if (!require('knitr')) install.packages('knitr'); library('knitr')
library(lubridate, warn.conflicts = FALSE)



#########################################################################################################################
##########################################uploading files

######	List of countries and centers
urlfilecountry_site="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/country_site.csv"
country_site <- read_csv(url(urlfilecountry_site))
##view(country_site)

######	List of randomized subjects
urlrandomized="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/randomized.csv"
randomized <- read_csv(url(urlrandomized))
randomized <- randomized[, -1] 
randomized

######	List of centers and subjects
urlsite_subject="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/site_subject.csv"
site_subject <- read_csv(url(urlsite_subject))


######	List of subjects with an assessment of whether 
##### the patient is eligible or not eligible for the study (principal investigator's assessment)
eligibility_PI_assessmenturl="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/eligibility_PI_assessment.csv"
eligibility_PI_assessment <- read_csv(url(eligibility_PI_assessmenturl))
eligibility_PI_assessment <- eligibility_PI_assessment[, -1]  


######	List of subjects for whom the principal investigator has entered information 
###### on the inclusion/exclusion criteria that were triggered by the subject
eligibility_PI_criteria_violatedurl="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/eligibility_PI_criteria_violated.csv"
eligibility_PI_criteria_violated <- read_csv(url(eligibility_PI_criteria_violatedurl))



###### Assessment of the subject’s ability to participate in the study, not considering the assessment 
##### of the principal investigator (non-PI assessment = Centralized Monitoring assessment)
eligibility_nonPI_assessmenturl="https://raw.githubusercontent.com/jodekpotasu/cstudy/master/eligibility_nonPI_assessment.csv"
eligibility_nonPI_assessment <- read_csv(url(eligibility_nonPI_assessmenturl))
  


###check for duplicates
length(unique(eligibility_nonPI_assessment$subject))== nrow(eligibility_nonPI_assessment)
length(unique(eligibility_PI_criteria_violated$subject))== nrow(eligibility_PI_criteria_violated)
length(unique(eligibility_PI_assessment$subject))== nrow(eligibility_PI_assessment)
length(unique(randomized$subject))== nrow(randomized)
length(unique(site_subject$subject))== nrow(site_subject)

#####eligibility_nonPI_assessment and 
###eligibility_PI_criteria_violated have duplicates
eligibility_nonPI_assessment %>% 
  count(subject) %>% 
  filter(n > 1)
eligibility_PI_criteria_violated %>% 
  count(subject) %>% 
  filter(n > 1)

#######for PI -    1 E5709008     2
################   2 E6203006     2
#######for nonPI - 1 E2804004     2
###################2 E4302010     2
###################3 E4307012     2

dupfornonPI <-eligibility_nonPI_assessment[duplicated(eligibility_nonPI_assessment$subject)|duplicated(eligibility_nonPI_assessment$subject, fromLast=TRUE),]
dupfornonPI
dupforPI <-eligibility_PI_criteria_violated[duplicated(eligibility_PI_criteria_violated$subject)|duplicated(eligibility_PI_criteria_violated$subject, fromLast=TRUE),]
dupforPI



####for non pi E2804004 should be marked as signal 
#####other duplicates in one table do not give different results
###this should be automatized

#####drop first column, not useful for now, maybe we will need surogate key
###eligibility_PI_criteria_violated <- eligibility_PI_criteria_violated[, -1]
###eligibility_nonPI_assessment <- eligibility_nonPI_assessment[, -1]
#########################################################################################################################
##########################################Question 

###cleaning
randomized2<-randomized

mutated<- site_subject %>%
  select(-X1) %>% 
  right_join(randomized2, by = "subject")

mutated2<- country_site %>%
  select(-X1) %>% 
  right_join(mutated, by = "site")


mutated3<- eligibility_PI_criteria_violated %>%
  select(-X1) %>% 
  full_join(eligibility_PI_assessment, by = "subject")


mutated4<- eligibility_nonPI_assessment %>%
  select(-X1) %>% 
  full_join(mutated3, by = "subject")



mutated4 %>% 
  count(subject) %>% 
  filter(n > 1)

dupformutated4 <-mutated4[duplicated(mutated4$subject)|duplicated(mutated4$subject, fromLast=TRUE),]
dupformutated4


###division for different analysis - to check ratio I need cleaned data, 
##to check missing values I need all data (as far as I understand)
notcleaneddata<-mutated2 %>%
  left_join(mutated4, by = "subject")

dupfornotcleaneddata <-notcleaneddata[duplicated(notcleaneddata$subject)|duplicated(notcleaneddata$subject, fromLast=TRUE),]
dupfornotcleaneddata


####I am truly sorry for this method... I will find better way, but let say for now... it is working
m<-filter(notcleaneddata, pi_crit_type != "NA" | cm_crit_type != "NA"|eligi_pi_assessment=="NA")
m$eligi_pi_assessment<-"Ineligible"




n<-filter(notcleaneddata, eligi_pi_assessment!="NA")


n<-n %>% replace_na(list( pi_crit_type= "unknown", cm_crit_type = "unknown"))


n<-filter(n, pi_crit_type != "Exclusion")
n<-filter(n, pi_crit_type != "Inclusion")
n<-filter(n, cm_crit_type != "Exclusion")
n<-filter(n, cm_crit_type != "Inclusion")

cleaneddata<-  rbind(n,m)
dupfornotcleaneddata <-cleaneddata[duplicated(cleaneddata$subject)|duplicated(cleaneddata$subject, fromLast=TRUE),]
dupfornotcleaneddata

####ok :D

cleanedwithoutdub <-distinct(cleaneddata, subject, .keep_all = TRUE)



## 1.	What is the number and ratio of incorrectly randomized (ineligible) subjects on study, country and site levels?
#number on study
Ineligible<-filter(cleanedwithoutdub, eligi_pi_assessment == "Ineligible")
numberofincorectlyrandomized <- nrow(Ineligible)
numberofincorectlyrandomized
##without doubles and with changed ineligible to rihgt ones there is 48

ratioofincorectlyrandomized <- nrow(Ineligible)/nrow(cleanedwithoutdub)
ratioofincorectlyrandomized

##without doubles and with changed ineligible to rihgt ones ratio = 0.05536332

#number on country
oncountryin<-count(Ineligible, country) 
#view(oncountryin)


#ratio on country
oncountryall<-count(cleanedwithoutdub, country) 
#view(oncountryall)

oncountry<- oncountryin %>%
  full_join(oncountryall, by = "country")
#view(oncountry)

ratiooncoutry<-oncountry$n.x/oncountry$n.y
g<-oncountry$country
ratiooncoutry<-cbind(g,ratiooncoutry)
ratiooncoutry<-as.data.frame(ratiooncoutry)
#view(ratiooncoutry)

#number on site

onsitein<-count(Ineligible, site) 
#view(onsitein)

onsiteall<-count(cleanedwithoutdub, site) 


onsite<- onsitein %>%
  full_join(onsiteall, by = "site")



ratioonsite<-onsite$n.x/oncountry$n.y
z<-onsite$site
ratioonsite<-cbind(z,ratioonsite)
ratioonsite<-as.data.frame(ratioonsite)
#view(ratioonsite)


#########################################################################################################################
##########################################Question 
## 2.	Which site and country has the biggest problem with correct randomization of subjects?

#site
arrange(onsitein,  desc(n))
###5709 in terms of n

ratioonsite[order(ratioonsite$ratioonsite),]
##7011 with 0.25; 5709 with 0.21

#country
arrange(oncountryin,  desc(n))
###country 6 and 8 in terms of n

ratiooncoutry[order(ratiooncoutry$ratiooncoutry),]
##country 7=0.15; country 3 = 0.12


#########################################################################################################################
##########################################Question 
## 3.	What is the number and ratio of randomized subjects for which status for eligibility cannot be assessed 
######due to missing data – on study, country and site levels?
#on study

#without doubles

Missing<-filter(cleanedwithoutdub, cm_detail_type != "Signal")
#view(Missing)

numberofmissing <- nrow(Missing)
numberofmissing

ratioofmissing <- nrow(Missing)/nrow(cleanedwithoutdub)
ratioofmissing



#with doubles,not cleaned

Missingwith<-filter(notcleaneddata, cm_detail_type != "Signal")
numberofmissingwith <- nrow(Missingwith)
numberofmissingwith

ratioofmissingwith <- nrow(Missingwith)/nrow(notcleaneddata)
ratioofmissingwith

#on country

#ratio on country without doubles


oncountrymiss<-count(Missing, country) 
#view(oncountrymiss)

oncountrymissm<- oncountrymiss %>%
  full_join(oncountryall, by = "country")
#view(oncountrymissm)

ratiooncoutrymiss<-oncountrymissm$n.x/oncountrymissm$n.y
g<-oncountrymissm$country
ratiooncoutrymiss<-cbind(g,ratiooncoutrymiss)
ratiooncoutrymiss<-as.data.frame(ratiooncoutrymiss)
#view(ratiooncoutrymiss)


#ratio on country with doubles

oncountryallw<-count(notcleaneddata, country)
oncountrymisswith<-count(Missingwith, country) 
#view(oncountrymisswith)

oncountrymissmwith<- oncountrymisswith %>%
  full_join(oncountryallw, by = "country")
#view(oncountrymissmwith)

ratiooncoutrymisswith<-oncountrymissmwith$n.x/oncountrymissmwith$n.y
g<-oncountrymissmwith$country
ratiooncoutrymisswith<-cbind(g,ratiooncoutrymisswith)
ratiooncoutrymisswith<-as.data.frame(ratiooncoutrymisswith)
#view(ratiooncoutrymisswith)


#number on site
#ratio on site without doubles

#ratio on country without doubles


onsitemiss<-count(Missing, site) 
#view(onsitemiss)

onsitemissm<- onsitemiss %>%
  full_join(onsiteall, by = "site")
#view(onsitemissm)

ratioonsitemiss<-onsitemissm$n.x/onsitemissm$n.y
site<-onsitemissm$site
ratioonsitemiss<-cbind(site,ratioonsitemiss)
ratioonsitemiss<-as.data.frame(ratioonsitemiss)
#view(ratioonsitemiss)


#ratio on site with doubles
onsiteallw<-count(notcleaneddata, site)
onsitemisswith<-count(Missingwith, site) 
#view(onsitemisswith)

onsitemissmwith<- onsitemisswith %>%
  full_join(onsiteallw, by = "site")
#view(onsitemissmwith)

ratioonsitemisswith<-onsitemissmwith$n.x/onsitemissmwith$n.y
site<-onsitemissmwith$site
ratioonsitemisswith<-cbind(site,ratioonsitemisswith)
ratioonsitemisswith<-as.data.frame(ratioonsitemisswith)
#view(ratioonsitemisswith)

#########################################################################################################################
##########################################Question 
## 4.	Which site and country has the biggest problem with entering data into system (most missing data cases)?

#site
arrange(onsitemiss,  desc(n))
###307,501,2303, 4302, 4308, 7009, 7011 =2 in terms of n

ratioonsitemiss[order(ratioonsitemiss$ratioonsitemiss),]
##2303 with 0.5; 403 with 0.5

###with doubles
#site
arrange(onsitemisswith,  desc(n))
###4302 = 3 in terms of n

ratioonsitemisswith[order(ratioonsitemisswith$ratioonsitemisswith),]
##2303 with 0.5; 403 with 0.5

##############country 
#country without
arrange(oncountrymiss,  desc(n))
###C9 - 8, c6 - 7 in terms of n

ratiooncoutrymiss[order(ratiooncoutrymiss$ratiooncoutrymiss),]
##c07 with 0.15; 03 with 0.12

#country with
arrange(oncountrymisswith,  desc(n))
###C9 - 10, c6 - 7 in terms of n

ratiooncoutrymisswith[order(ratiooncoutrymisswith$ratiooncoutrymisswith),]
##c07 with 0.15; 03 with 0.12



#########################################################################################################################
##########################################Question 
## 5.	Which is the most often violated eligibility criterion (considering both PI and CM assessments)?
##so so late:P 
#view(Missingwith)



eligibilitycritcm<-count(Missingwith, cm_crit_num) 
eligibilitycritpi<-count(Missingwith, pi_crit_num)
eligibilitycritcm<-rename(eligibilitycritcm, crit_num=cm_crit_num)
w<-drop_na(eligibilitycritpi)
eligibilitycritpi<-rename(w, crit_num=pi_crit_num)


eligibilitycrit<- eligibilitycritcm %>%
  full_join(eligibilitycritpi, by = "crit_num")
eligibilitycrit<-eligibilitycrit %>% replace_na(list( n.x= 0, n.y = 0))

eligibilitycrit <- eligibilitycrit %>% mutate(sumrow= n.x + n.y)
eligibilitycrit
arrange(eligibilitycrit,  desc(sumrow))

#####7 and 4

#########################################################################################################################
##########################################Question 
## 6.	Do incorrect randomizations occur at some specific time or is distributed across whole study time (defined as time from first randomization to today)?
cleanedwithoutdub
cleanedwithoutdub$eligi_pi_assessment = factor(cleanedwithoutdub$eligi_pi_assessment,levels = c('Ineligible', 'Eligible'),labels = c(0, 1))

Ineligible$eligi_pi_assessment = factor(Ineligible$eligi_pi_assessment,levels = c('Ineligible', 'Eligible'),labels = c(1, 0))


p <- ggplot(Ineligible, aes(x=rnd_date, y=eligi_pi_assessment)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

p + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

#view(Ineligible)

desc<-arrange(Ineligible,  desc(rnd_date))

view(desc)

#the tableand plot can show us that some months are represented more than others.

p<-ggplot(desc, aes(rnd_date, eligi_pi_assessment)) +
  geom_point(na.rm=TRUE)
p #+ scale_x_date(date_breaks = "1 month", date_labels = "%m %Y")

desc<-desc$rnd_date(date_breaks = "1 month")
desc



#it will be worth to draw a more sophisticated plot with ggplot, and cut time series by months to look for patterns
#however I won't be able do it on time ;(


#I am truly sorry for not so cleaned code :(



