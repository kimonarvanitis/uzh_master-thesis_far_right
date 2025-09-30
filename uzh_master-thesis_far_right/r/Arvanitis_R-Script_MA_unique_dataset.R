#####################################################################################################################
#####################################################################################################################  

#=============================================================================================#
#======                                                                                 ======#
#======                        Kimon Arvanitis - MA Thesis                              ======#
#======                                                                                 ======#
#=============================================================================================#


#####################################################################################################################
#####################################################################################################################
rm(list = ls())

library(haven)
library(foreign)
library(labelled)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(readstata13)
library(effects)
library(texreg)
library(ggplot2)
library(ggcorrplot)
library(stargazer)
library(lme4)
library(ggrepel)
library(ggpubr)
library(interplot)
library(sjPlot)

erc <- read.dta13("ERC.dta")

#=============================================================================================#
#                        Dependent Variable: FR (Farright) (close)                            #
#=============================================================================================#
#FR_int:
table(erc$X5_1_Vote_intention)
erc$FR_int[erc$X5_1_Vote_intention=="Danish People's Party"]<-1
erc$FR_int[erc$X5_1_Vote_intention=="AFD"]<-1 
erc$FR_int[erc$X5_1_Vote_intention=="PVV"]<-1  
erc$FR_int[erc$X5_1_Vote_intention=="UKIP"]<-1
erc$FR_int[erc$X5_1_Vote_intention=="LN"]<-1
erc$FR_int[erc$X5_1_Vote_intention=="FDI"]<-1
erc$FR_int[erc$X5_1_Vote_intention=="SD"]<-1
erc$FR_int[erc$X5_1_Vote_intention=="abstain"]<-2
erc$FR_int[is.na(erc$FR_int)] <- 0
erc$FR_int[erc$FR_int==2]<-NA
#FR_vote:
table(erc$X5_4_Party_choice_last_election)
erc$FR_vote[erc$X5_4_Party_choice_last_election=="Danish People's Party"]<-1
erc$FR_vote[erc$X5_4_Party_choice_last_election=="AFD"]<-1 
erc$FR_vote[erc$X5_4_Party_choice_last_election=="PVV"]<-1  
erc$FR_vote[erc$X5_4_Party_choice_last_election=="UKIP"]<-1
erc$FR_vote[erc$X5_4_Party_choice_last_election=="LN"]<-1
erc$FR_vote[erc$X5_4_Party_choice_last_election=="FDI"]<-1
erc$FR_vote[erc$X5_4_Party_choice_last_election=="SD"]<-1
erc$FR_vote[erc$X5_4_Party_choice_last_election=="NA"]<-2
erc$FR_vote[is.na(erc$FR_vote)] <- 0
erc$FR_vote[erc$FR_vote==2]<-NA
#Compare two dependent variable
table(erc$FR_int)
table(erc$FR_vote)
#######################################################################################################################################################################
#######################################################################################################################################################################


#=============================================================================================#
#                                     Independe Variables                                     #
#=============================================================================================#

#============================#
#    Past social mobility    #
#============================#

#Nostalgic Deprivation I#
#=======================#
#nostalgia variable (1): (1=became worse; 10=became better) 
#Würden Sie sagen, dass sich das Leben in Deutschland in den letzten 30 Jahren verbessert oder verschlechtert hat?
table(erc$X4_12_Long_term_evaluatn_f_scty)
erc$nostalgic_dep <- erc$X4_12_Long_term_evaluatn_f_scty
table(erc$nostalgic_dep)
#Create a dummy varaible (0=became better; 1=became worse)
erc <- erc %>% mutate(D_nostalgic_dep = ifelse(nostalgic_dep<6,1, ifelse(nostalgic_dep>5,0,NA)))
table(erc$D_nostalgic_dep)
#Reverse the variable (1=became better; 10=became worse) 
erc$nostalgic_dep <- car::recode(erc$nostalgic_dep, "10=1; 9=2; 8=3; 7=4; 6=5; 5=6; 4=7; 3=8; 2=9; 1=10")
table(erc$nostalgic_dep)


#Nostalgic Deprivation II: Robustness test#
#=========================================#
#nostalgia variable (2): (1=became worse; 11=became better) ======> NOSTALGIA2  FOR ROBUSTNESS CHECKS
#Würden Sie sagen, dass sich Ihr wirtschaftlicher Lebensstandard im Vergleich zu vor 10 Jahren insgesamt verschlechtert oder verbessert hat?
table(erc$X4_8_Standard_of_living_dvlpmnt)
erc$nostalgia2 <- erc$X4_8_Standard_of_living_dvlpmnt
table(erc$nostalgia2)
#Create a dummy varaible (1=became worse; 0=Became better)
erc <- erc %>% mutate(D_nostalgia2= ifelse(nostalgia2<6,1, ifelse(nostalgia2>5,0,NA)))
table(erc$D_nostalgia2)
#Reverse the variable (1=became better; 10=became worse) 
erc$nostalgia2 <- car::recode(erc$nostalgia2, "11=1; 10=2; 9=3; 8=4; 7=5; 6=6; 5=7; 4=8; 3=9; 2=10; 1=11")
table(erc$nostalgia2)

#Nostalgic Deprivation III: Robustness test + Matching Design#
#============================================================#
#social mobiltiy: (1=niederiger;2=same;3=Höher)
#Wenn Sie Ihre Position in der Gesellschaft mit der vergleichen, die Ihre Eltern in Ihrem Alter hatten, 
#würden Sie dann sagen, dass Ihre Position heute niedriger oder höher ist als die Ihrer Eltern damals? 
table(erc$X4_11_Social_mobility_status)
erc$socmob <- erc$X4_11_Social_mobility_status
table(erc$socmob)
summary(erc$socmob)

#Create Variable 1=lower level than parents
erc$socmob_treat[erc$socmob==1]<-1
erc$socmob_treat[erc$socmob==2]<-0
erc$socmob_treat[erc$socmob==3]<-0
table(erc$socmob_treat)
#Reverse the variable (1=höher;2=same;3=niedriger)
erc$socmob <- car::recode(erc$socmob, "1=3; 2=2; 3=1")
table(erc$socmob)

#==============================#
#    Future social mobility    #
#==============================#

#Future Opportunities I#
#======================#
#futOp_eco: Wenn Sie an die Zukunft denken, wie beurteilen Sie dann Ihre persönlichen Chancen auf ein gutes, 
#stabiles Beschäftigungsverhältnis bis zur Rente? (1=bad; 11=good)
table(erc$X8_17_Opportunity_1)
erc$futOp_eco <- erc$X8_17_Opportunity_1
table(erc$futOp_eco)
#Create a dummy varaible (0=good; 1=bad)
erc <- erc %>% mutate(D_futOp_eco=ifelse(futOp_eco>5,0, ifelse(futOp_eco<6,1,NA)))
table(erc$D_futOp_eco)
#Create a second dummy varaible (0=good; 1=bad)
erc <- erc %>% mutate(D_futOp_eco_r=ifelse(futOp_eco>6,0, ifelse(futOp_eco<7,1,NA)))
table(erc$D_futOp_eco_r)


#futOp_soc:Denken Sie nun bitte über den Arbeitsmarkt hinaus an Ihre Lebensqualität insgesamt. 
#Wie beurteilen Sie Ihre persönlichen Chancen auf ein sicheres, erfülltes Leben?	 (1=bad; 11=god)
table(erc$X8_18_Opportunity_2)
erc$futOp_soc <- erc$X8_18_Opportunity_2
table(erc$futOp_soc)
#Create a dummy varaible (0=good; 1=bad)
erc <- erc %>% mutate(D_futOp_soc= ifelse(futOp_soc>5,0, ifelse(futOp_soc<6,1,NA)))
table(erc$D_futOp_soc)



#######################################################################################################################################################################
#######################################################################################################################################################################

#=================================================================#
#   Past & Future: Nostalgic Deprivation + Future Opportunities   #
#=================================================================#

#==================================#
#==================================#
#Create Quadrant Variable -    Q   #
#==================================#
#    (D_nostalgic_dep+D_futOp_soc) #
#==================================#
#Q1
erc$Q1[erc$D_nostalgic_dep==1&erc$D_futOp_soc==1]<-1
erc$Q1[erc$D_nostalgic_dep==0&erc$D_futOp_soc==1]<-0
erc$Q1[erc$D_nostalgic_dep==1&erc$D_futOp_soc==0]<-0
erc$Q1[erc$D_nostalgic_dep==0&erc$D_futOp_soc==0]<-0
table(erc$Q1)
summary(erc$Q1)
#Q2
erc$Q2[erc$D_nostalgic_dep==1&erc$D_futOp_soc==0]<-1
erc$Q2[erc$D_nostalgic_dep==0&erc$D_futOp_soc==1]<-0
erc$Q2[erc$D_nostalgic_dep==1&erc$D_futOp_soc==1]<-0
erc$Q2[erc$D_nostalgic_dep==0&erc$D_futOp_soc==0]<-0
table(erc$Q2)
summary(erc$Q2)
#Q3
erc$Q3[erc$D_nostalgic_dep==0 & erc$D_futOp_soc==1] <- 1
erc$Q3[erc$D_nostalgic_dep==1 & erc$D_futOp_soc==0] <- 0
erc$Q3[erc$D_nostalgic_dep==1 & erc$D_futOp_soc==1] <- 0
erc$Q3[erc$D_nostalgic_dep==0 & erc$D_futOp_soc==0] <- 0
table(erc$Q3)
summary(erc$Q3)

#Q4
erc$Q4[erc$D_nostalgic_dep==0&erc$D_futOp_soc==0]<-1
erc$Q4[erc$D_nostalgic_dep==1&erc$D_futOp_soc==0]<-0
erc$Q4[erc$D_nostalgic_dep==0&erc$D_futOp_soc==1]<-0
erc$Q4[erc$D_nostalgic_dep==1&erc$D_futOp_soc==1]<-0
table(erc$Q4)
summary(erc$Q4)


#==================================#
#Create Quadrant Variable -    Qb  #
#==================================#
#    (D_nostalgic_dep+D_futOp_eco) #
#==================================#
#Qb1
erc$Qb1[erc$D_nostalgic_dep==1&erc$D_futOp_eco==1]<-1
erc$Qb1[erc$D_nostalgic_dep==0&erc$D_futOp_eco==1]<-0
erc$Qb1[erc$D_nostalgic_dep==1&erc$D_futOp_eco==0]<-0
erc$Qb1[erc$D_nostalgic_dep==0&erc$D_futOp_eco==0]<-0
table(erc$Qb1)
summary(erc$Qb1)
#Qb2
erc$Qb2[erc$D_nostalgic_dep==1&erc$D_futOp_eco==0]<-1
erc$Qb2[erc$D_nostalgic_dep==0&erc$D_futOp_eco==1]<-0
erc$Qb2[erc$D_nostalgic_dep==1&erc$D_futOp_eco==1]<-0
erc$Qb2[erc$D_nostalgic_dep==0&erc$D_futOp_eco==0]<-0
table(erc$Qb2)
#Qb3
erc$Qb3[erc$D_nostalgic_dep==0 & erc$D_futOp_eco==1] <- 1
erc$Qb3[erc$D_nostalgic_dep==1 & erc$D_futOp_eco==0] <- 0
erc$Qb3[erc$D_nostalgic_dep==1 & erc$D_futOp_eco==1] <- 0
erc$Qb3[erc$D_nostalgic_dep==0 & erc$D_futOp_eco==0] <- 0
table(erc$Qb3)
summary(erc$Qb3)
#Qb4
erc$Qb4[erc$D_nostalgic_dep==0&erc$D_futOp_eco==0]<-1
erc$Qb4[erc$D_nostalgic_dep==1&erc$D_futOp_eco==0]<-0
erc$Qb4[erc$D_nostalgic_dep==0&erc$D_futOp_eco==1]<-0
erc$Qb4[erc$D_nostalgic_dep==1&erc$D_futOp_eco==1]<-0
table(erc$Qb4)
summary(erc$Qb4)

#==================================#
#Create Quadrant Variable -   Qbr  #
#==================================#
#   (D_nostalgic_dep+D_futOp_eco_r) #
#==================================#
#Qbr1
erc$Qbr1[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==1]<-1
erc$Qbr1[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==1]<-0
erc$Qbr1[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==0]<-0
erc$Qbr1[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==0]<-0
table(erc$Qbr1)
summary(erc$Qbr1)
#Qbr2
erc$Qbr2[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==0]<-1
erc$Qbr2[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==1]<-0
erc$Qbr2[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==1]<-0
erc$Qbr2[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==0]<-0
table(erc$Qbr2)
#Qbr3
erc$Qbr3[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==1]<-1
erc$Qbr3[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==0]<-0
erc$Qbr3[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==1]<-0
erc$Qbr3[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==0]<-0
table(erc$Qbr3)
summary(erc$Qbr3)
#Qbr4
erc$Qbr4[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==0]<-1
erc$Qbr4[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==0]<-0
erc$Qbr4[erc$D_nostalgic_dep==0&erc$D_futOp_eco_r==1]<-0
erc$Qbr4[erc$D_nostalgic_dep==1&erc$D_futOp_eco_r==1]<-0
table(erc$Qbr4)
summary(erc$Qbr4)


#######################################################################################################################################################################
#######################################################################################################################################################################

#=====================#
#  Control Variables  #
#=====================#


#Income                                         
table(erc$X8_35_Income)
erc$income <- erc$X8_35_Income
#age
table(erc$age)
#sex - female (1=female, 0=male)
table(erc$sex)
erc$female <- as.numeric(erc$sex)
erc$female[erc$female==2]<-0
table(erc$female)
#education: 1=low; 8=high educ         
table(erc$educ)
#Subjective social status 
#Es gibt Menschen am oberen Rand der Gesellschaft und Menschen am unteren Rand. 
#Wo würden Sie sich selbst auf dieser Skala von „oben“ (10) bis „unten“ (1) einordnen?
table(erc$X4_10_Subjective_Social_Status)
erc$socstatus <- erc$X4_10_Subjective_Social_Status
#Citizenship ==> Lieber Born in Country Daher nicht ins Modell
table(erc$X8_1_Citizenship)
erc$citizen <- as.numeric(erc$X8_1_Citizenship)
erc$citizen[erc$citizen==2]<-0
#Religion gibt es nicht

#Econmic Dimension: unemployed#
###############################
summary(erc$X8_8_Employment_situation)
erc$unemployed[erc$X8_8_Employment_situation=="7"]<-6666 #unemployed
#erc$unemployed[erc$X8_8_Employment_situation=="8"]<-7777 #other
#erc$unemployed[erc$X8_8_Employment_situation=="4"]<-7777 #only in school
erc$unemployed[is.na(erc$unemployed)] <- 0
erc$unemployed[erc$unemployed==6666]<-1
#erc$unemployed[erc$unemployed==7777]<-NA
table(erc$unemployed)

#Cultural Dimension: Immigration treat# 
#######################################
#attitudes towards immigration cultural
#Einwanderung ist eine Bedrohung für unsere nationale Kultur (1=don't agree at all, 4=agree at all)
table(erc$X4_1_5_General_Attitudes)
erc$im_cul <- erc$X4_1_5_General_Attitudes
#attitudes towards immigration economical
#Einwanderung ist eine Bedrohung für den nationalen Arbeitsmarkt (1=don't agree at all, 4=agree at all)
table(erc$X4_1_8_General_Attitudes)
erc$im_eco <- erc$X4_1_8_General_Attitudes
#combine both variables
erc$im <- erc$im_cul+erc$im_eco
erc$im_treat[erc$im==2]<-1
erc$im_treat[erc$im==3]<-1
erc$im_treat[erc$im==4]<-2
erc$im_treat[erc$im==5]<-3
erc$im_treat[erc$im==6]<-3
erc$im_treat[erc$im==7]<-4
erc$im_treat[erc$im==8]<-4
table(erc$im_treat)

#WEIGHTS for models
erc$m_weigt <- erc$Weight_Age_sex___education

#attitudes towards inequality 
#In einer fairen Gesellschaft sollte es keine großen Einkommensunterschiede geben (1=don't agree at all, 4=agree at all)
table(erc$X4_1_1_General_Attitudes)
erc$perc_inq <- erc$X4_1_1_General_Attitudes

#Predicted probability plots
#create variables for plots
erc$unemployed.f <- as.factor(erc$unemployed)
erc$im_treat.f <- as.factor(erc$im_treat)
erc$perc_inq.f <- as.factor(erc$perc_inq)
erc$nostalgic_dep.f <- as.factor(erc$nostalgic_dep)
erc$futOp_soc.f <- as.factor(erc$futOp_soc)
erc$futOp_eco.f <- as.factor(erc$futOp_eco)
erc$nostalgia2.f <- as.factor(erc$nostalgia2)
erc$socmob.f <- as.factor(erc$socmob)
erc$Qb1.f <- as.factor(erc$Qb1)
erc$Qb2.f <- as.factor(erc$Qb2)
erc$Qb3.f <- as.factor(erc$Qb3)
erc$Qb4.f <- as.factor(erc$Qb4)
erc$Q1.f <- as.factor(erc$Q1)
erc$Q2.f <- as.factor(erc$Q2)
erc$Q3.f <- as.factor(erc$Q3)
erc$Q4.f <- as.factor(erc$Q4)

#Job variable for qudrant analysis
table(erc$X8_16_Sector_of_employment)

erc$employ_sec <- (erc$X8_16_Sector_of_employment)
table(erc$employ_sec)
#Create variables with names
erc$employ_sec_name[erc$employ_sec==1]<-"a) Government"
erc$employ_sec_name[erc$employ_sec==2]<-"b) Public sector"
erc$employ_sec_name[erc$employ_sec==3]<-"c) State enterprise"
erc$employ_sec_name[erc$employ_sec==4]<-"d) Private Company"
erc$employ_sec_name[erc$employ_sec==5]<-"e) Self employed"
erc$employ_sec_name[erc$employ_sec==6]<-"f) Other"
table(erc$employ_sec_name)

#general attitudes # (1-4); 1=stimme überhaupt nicht zu ; 4= stimme voll und ganz zu
#==================#

#attitudes towards homosexuality
#Schwule und lesbische Paare sollten in Bezug auf die Adoption von Kindern dieselben Rechte haben wie heterosexuelle Paare
#(1=don't agree at all, 4=agree at all)
table(erc$X4_1_6_General_Attitudes)
erc$progay <- erc$X4_1_6_General_Attitudes
#attitudes towards gender equality
#Unter dem Strich leidet die Familie, wenn die Frau in Vollzeit berufstätig ist (1=don't agree at all, 4=agree at all)
table(erc$X4_1_7_General_Attitudes)
erc$gendrequal <- erc$X4_1_7_General_Attitudes
#attitudes towards european integration
#Die europäische Integration ist zu weit gegangen (1=don't agree at all, 4=agree at all)
table(erc$X4_1_9_General_Attitudes)
erc$pro_eu <- erc$X4_1_9_General_Attitudes

#Left right perception
#In der Politik wird häufig von „links“ und „rechts“ gesprochen. 
#Wo würden Sie sich selbst auf einer Skala von 0 bis 10 platzieren, wenn 0 für „links“ und 10 für „rechts“ steht?
table(erc$X4_2_Left_Right)
erc$left_right <- erc$X4_2_Left_Right

#Denken Sie ganz allgemein, dass man den meisten Menschen vertrauen kann? (1=Nein; 10=JA)
table(erc$X4_3_Social_Trust)
erc$socialtrust <- erc$X4_3_Social_Trust

#Vertrauen Sie im Allgemeinen den Politikern in Ihrem Land? 1=Nein; 10=JA)
table(erc$X4_5_Political_Trust)
erc$poltrust <- erc$X4_5_Political_Trust


#Save Dataset for describtives and models
erc_final <- erc
save(erc_final,file="erc_final.Rda")

#Dataset for matching - Relevante Variablen für Matching
erc_MA_int <- dplyr::select(erc, c(FR_int,socmob_treat,country,age,female,income,educ,unemployed,im_treat,perc_inq,
                                   progay,gendrequal,pro_eu,left_right,socialtrust,poltrust))
erc_MA_vote <- dplyr::select(erc, c(FR_vote,socmob_treat,country,age,female,income,educ,unemployed,im_treat,perc_inq,
                                    progay,gendrequal,pro_eu,left_right,socialtrust,poltrust))
#Alle NA's raus nehmen
erc_MA_int <- na.omit(erc_MA_int)
erc_MA_vote <- na.omit(erc_MA_vote)
#Save Dataset for matching
save(erc_MA_int,file="erc_MA_int.Rda")
save(erc_MA_vote,file="erc_MA_vote.Rda")


#######################################################################################################################################################################
#######################################################################################################################################################################
rm(list = ls())


library(haven)
library(foreign)
library(labelled)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(readstata13)
library(effects)
library(texreg)
library(ggplot2)
library(ggcorrplot)
library(stargazer)
library(lme4)
library(ggrepel)
library(ggpubr)
library(interplot)
library(sjPlot)
require(nnet)
require(reshape2)

load("erc_final.Rda")
erc <- erc_final

#=============================================================================================#
#======                                                                                 ======#
#======                                Descriptives                                     ======#
#======                                                                                 ======#
#=============================================================================================#

#Correlation Matrix
#==================#
#Create correlation.matrix: UV core variables
corr.matrix.ind <- cor(erc[,c("nostalgic_dep","nostalgia2","socmob","futOp_soc","futOp_eco","age","female",
                              "income","educ","unemployed","im_treat","perc_inq")], 
                       use="na.or.complete", method = "pearson")
#Label the variables in the Matrix
row.names(corr.matrix.ind) <- c("Nostalgic deprivation", "Nostalgic deprivation (10 years)","Intergenerational social mobility",
                                "Future opportunity","Future economic opportunity","Age","Female",
                                "Income","Education","Unemployment","Attitudes towards immigration",
                                "Attitudes towards inequality")
colnames(corr.matrix.ind) <- c("Nostalgic deprivation", "Nostalgic deprivation (10 years)","Intergenerational social mobility",
                               "Future opportunity","Future economic opportunity","Age","Female",
                               "Income","Education","Unemployment","Attitudes towards immigration",
                               "Attitudes towards inequality")

#Create Plot
ggcorrplot(corr.matrix.ind, hc.order = F, type = "lower",
           lab = TRUE, show.legend = TRUE, lab_size = 2.5)

ggsave("plot1.pdf", width = 16, height = 12, units = "cm")

#Correlation matrix with quadrants
corr.matrix.ind1 <- cor(erc[,c("Q1","Q2","Q3","Q4","age","female",
                               "income","educ","unemployed","im_treat","perc_inq")], 
                        use="na.or.complete", method = "pearson")
#Label the variables in the Matrix
row.names(corr.matrix.ind1) <- c("1st quadrant", "2nd quadrant","3rd quadrant",
                                 "4th quadrant","Age","Female",
                                 "Income","Education","Unemployment","Attitudes towards immigration",
                                 "Attitudes towards inequality")
colnames(corr.matrix.ind1) <- c("1st quadrant", "2nd quadrant","3rd quadrant",
                                "4th quadrant","Age","Female",
                                "Income","Education","Unemployment","Attitudes towards immigration",
                                "Attitudes towards inequality")

#Create Plot
ggcorrplot(corr.matrix.ind1, hc.order = F, type = "lower",
           lab = TRUE, show.legend = TRUE, lab_size = 2.5)

ggsave("plot1_quadrants.pdf", width = 16, height = 12, units = "cm")

############ descriptive table: *Table 1*
stargazer(as.data.frame(erc[c("FR_vote","FR_int","nostalgic_dep","nostalgia2","socmob"
                              ,"futOp_soc","futOp_eco","age","female",
                              "income","educ","unemployed","im_treat","perc_inq")]), 
          style = "apsr", type = "latex",
          covariate.labels = c("Voted for far-right party","Far-right party vote intension","Nostalgic deprivation", "Nostalgic deprivation (10 years)",
                               "Intergenerational social mobility",
                               "Future opportunity","Future (economic) opportunity","Age","Female",
                               "Income","Education","Unemployment","Attitudes towards immigration",
                               "Attitudes towards inequality"), 
          title="Descriptive statistics", header = FALSE,
          font.size = "small", no.space = TRUE, omit.summary.stat = c("p25", "p75"))

#compare the two dependent variables
plot2.1<- ggplot(erc, aes(x=FR_vote))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Voted for far-right party", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot2.2 <- ggplot(erc, aes(x=FR_int))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Far-right party vote intension", y="")+ 
  theme(axis.title.x=element_text(size = 10))
figure2 <- ggarrange(plot2.1, plot2.2,  ncol = 2, nrow = 1)

ggsave("plot2.pdf", width = 12, height = 5, units = "cm")

########### Histograms of relevant varibales
## Individual level ## 

#Plot version - final version for data essay
plot3.1 <- ggplot(erc, aes(x=nostalgic_dep))+geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Nostalgic deprivation", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.2 <- ggplot(erc, aes(x=nostalgia2))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Nostalgic deprivation (10 years)", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.3 <- ggplot(erc, aes(x=socmob))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Intergenerational social mobility", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.4 <- ggplot(erc, aes(x=futOp_soc))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Future opportunity", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.5 <- ggplot(erc, aes(x=futOp_eco))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Future (economic) opportunity", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.6 <- ggplot(erc, aes(x=unemployed))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Unemployment", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.7 <- ggplot(erc, aes(x=im_treat))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Attitudes towards immigration", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.8 <- ggplot(erc, aes(x=perc_inq))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Attitudes towards inequality", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.9 <- ggplot(erc, aes(x=educ))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Education", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot3.10 <- ggplot(erc, aes(x=income))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Income", y="")+ 
  theme(axis.title.x=element_text(size = 10))



figure3 <- ggarrange(plot3.1, plot3.2,plot3.3, plot3.4,plot3.5,plot3.9,plot3.10,
                     plot3.6, plot3.7,plot3.8,ncol = 4, nrow = 3)

ggsave("plot3.pdf", width = 25, height = 20, units = "cm")

##########################################################################################################################################
##########################################################################################################################################



#==================================================#
#                                                  #
#               Analysis of Quadants               #
#                                                  #
#==================================================#

#=============================================================#
#    Main group of Quadrant: (D_nostalgic_dep+D_futOp_soc)    #
#=============================================================#

erc$quadrant2[erc$Q1==1]<-1
erc$quadrant2[erc$Q2==1]<-2
erc$quadrant2[erc$Q3==1]<-3
erc$quadrant2[erc$Q4==1]<-4
#Cluster variable
erc$quadrant2_cluster[erc$Q1==1]<-"1st Quadrant"
erc$quadrant2_cluster[erc$Q2==1]<-"2nd Quadrant"
erc$quadrant2_cluster[erc$Q3==1]<-"3rd Quadrant"
erc$quadrant2_cluster[erc$Q4==1]<-"4th Quadrant"
table(erc$quadrant2_cluster)

plot4.1 <- ggplot(erc, aes(x=quadrant1))+geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Quadrants based on nostalgic deprivation and future (economic) opportunity", y="")+ 
  theme(axis.title.x=element_text(size = 10))
plot4.2 <- ggplot(erc, aes(x=quadrant2))+geom_bar(stat = "count", fill='#999999')+
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Quadrants constructed with variables nostalgic deprivation and future opportunity", y="")+ 
  theme(axis.title.x=element_text(size = 10))

figure4 <- ggarrange(plot4.2,ncol = 1, nrow = 1)

ggsave("plot4.pdf", width = 13, height = 8, units = "cm")


#Quadrant 1
erc_q1 <- erc %>% filter(quadrant2==1)
erc_q1 <- dplyr::select(erc_q1, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q1 <- na.omit(erc_q1)
mean(erc_q1$educ) #4.43
mean(erc_q1$income) #4.05
mean(erc_q1$unemployed) #0.22
mean(erc_q1$age) #44.36
mean(erc_q1$female) #0.57
mean(erc_q1$im_treat) #2.91

#Quadrant 2
erc_q2 <- erc %>% filter(quadrant2==2)
erc_q2 <- dplyr::select(erc_q2, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q2 <- na.omit(erc_q2)
mean(erc_q2$educ) #4.99
mean(erc_q2$income) #5.68
mean(erc_q2$unemployed) #0.08
mean(erc_q2$age) #43.64
mean(erc_q2$female) #0.52
mean(erc_q2$im_treat) #2.77

#Quadrant 3
erc_q3 <- erc %>% filter(quadrant2==3)
erc_q3 <- dplyr::select(erc_q3, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q3 <- na.omit(erc_q3)
mean(erc_q3$educ) #4.77
mean(erc_q3$income) #4.79
mean(erc_q3$unemployed) #0.18
mean(erc_q3$age) #43.5
mean(erc_q3$female) #0.51
mean(erc_q3$im_treat) #2.53

#Quadrant 4
erc_q4 <- erc %>% filter(quadrant2==4)
erc_q4 <- dplyr::select(erc_q4, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q4 <- na.omit(erc_q4)
mean(erc_q4$educ) #5.42
mean(erc_q4$income) #6.50
mean(erc_q4$unemployed) #0.04
mean(erc_q4$age) #43.38
mean(erc_q4$female) #0.45
mean(erc_q4$im_treat) #2.46

#Create Dataset to test wether difference in means are stastically signficant 
df <- dplyr::select(erc, c(age,female,income,educ,unemployed,quadrant2,im_treat,employ_sec_name,employ_sec))
df$quadrant2[is.na(df$quadrant2)] <- 0
table(df$quadrant2)
df <- df %>% dplyr::filter(quadrant2 != 0)
#create the variable in the dataset to group the data
df$quadrant2_cluster[df$quadrant2==1]<-"1st Quadrant"
df$quadrant2_cluster[df$quadrant2==2]<-"2nd Quadrant"
df$quadrant2_cluster[df$quadrant2==3]<-"3rd Quadrant"
df$quadrant2_cluster[df$quadrant2==4]<-"4th Quadrant"
table(df$quadrant2_cluster)

p1 <-ggplot(df, aes(x=educ))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Education", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p2 <-ggplot(df, aes(x=income))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Income", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p3 <-ggplot(df, aes(x=age))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Age", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p4 <-ggplot(df, aes(x=female))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Female", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p5 <-ggplot(df, aes(x=unemployed))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Unemployment", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p6 <-ggplot(df, aes(x=im_treat))+facet_wrap(~ quadrant2_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Attitudes towards immigration", y="")+ 
  theme(axis.title.x=element_text(size = 10))

figure_q <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2, nrow = 3)

ggsave("plot_quadrants.pdf", width = 17, height = 20, units = "cm")


#Compare Quadrant 1 and 2#
##########################
df_12 <- df %>% dplyr::filter(quadrant2_cluster == "1st Quadrant" | quadrant2_cluster == "2nd Quadrant")
df_12_12 <- df_12 %>% dplyr::filter(employ_sec == 1 | quadrant2_cluster == 2)

#Age
t.test(age ~ quadrant2_cluster, data = df_12)
wilcox.test(age ~ quadrant2_cluster, data = df_12)
#Education
t.test(educ ~ quadrant2_cluster, data = df_12)
wilcox.test(educ ~ quadrant2_cluster, data = df_12)
#Income
t.test(income ~ quadrant2_cluster, data = df_12)
wilcox.test(income ~ quadrant2_cluster, data = df_12)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_12)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_12)
#Female
t.test(female~ quadrant2_cluster, data = df_12)
wilcox.test(female ~ quadrant2_cluster, data = df_12)
#Immigration
t.test(im_treat~ quadrant2_cluster, data = df_12)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_12)
#Compare Quadrant 2 and 3
##########################
df_23 <- df %>% dplyr::filter(quadrant2_cluster == "3rd Quadrant" | quadrant2_cluster == "2nd Quadrant")
#Age #No statical snificant difference #=======================RESULT=======================#
t.test(age ~ quadrant2_cluster, data = df_23)
wilcox.test(age ~ quadrant2_cluster, data = df_23)
#Education
t.test(educ ~ quadrant2_cluster, data = df_23)
wilcox.test(educ ~ quadrant2_cluster, data = df_23)
#Income
t.test(income ~ quadrant2_cluster, data = df_23)
wilcox.test(income ~ quadrant2_cluster, data = df_23)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_23)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_23)
#Female #No statical snificant difference #=======================RESULT=======================#
t.test(female~ quadrant2_cluster, data = df_23)
wilcox.test(female ~ quadrant2_cluster, data = df_23)
#Immigration
t.test(im_treat~ quadrant2_cluster, data = df_23)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_23)
#Compare Quadrant 3 and 4
##########################
df_34 <- df %>% dplyr::filter(quadrant2_cluster == "3rd Quadrant" | quadrant2_cluster == "4th Quadrant")
#Age   #No statical snificant difference #=======================RESULT=======================#
t.test(age ~ quadrant2_cluster, data = df_34) 
wilcox.test(age ~ quadrant2_cluster, data = df_34)
#Education
t.test(educ ~ quadrant2_cluster, data = df_34)
wilcox.test(educ ~ quadrant2_cluster, data = df_34)
#Income
t.test(income ~ quadrant2_cluster, data = df_34)
wilcox.test(income ~ quadrant2_cluster, data = df_34)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_34)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_34)
#Female  
t.test(female~ quadrant2_cluster, data = df_34)
wilcox.test(female ~ quadrant2_cluster, data = df_34)
#Immigration #No statical snificant difference #=======================RESULT=======================#
t.test(im_treat~ quadrant2_cluster, data = df_34)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_34)
#Compare Quadrant 1 and 4
##########################
df_14 <- df %>% dplyr::filter(quadrant2_cluster == "1st Quadrant" | quadrant2_cluster == "4th Quadrant")
#Age
t.test(age ~ quadrant2_cluster, data = df_14)
wilcox.test(age ~ quadrant2_cluster, data = df_14)
#Education
t.test(educ ~ quadrant2_cluster, data = df_14)
wilcox.test(educ ~ quadrant2_cluster, data = df_14)
#Income
t.test(income ~ quadrant2_cluster, data = df_14)
wilcox.test(income ~ quadrant2_cluster, data = df_14)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_14)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_14)
#Female 
t.test(female~ quadrant2_cluster, data = df_14)
wilcox.test(female ~ quadrant2_cluster, data = df_14)
#Immigration
t.test(im_treat~ quadrant2_cluster, data = df_14)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_14)
#Compare Quadrant 1 and 3
##########################
df_13 <- df %>% dplyr::filter(quadrant2_cluster == "1st Quadrant" | quadrant2_cluster == "3rd Quadrant")
#Age #No statical snificant difference  #================ RESULT =================#
t.test(age ~ quadrant2_cluster, data = df_13)
wilcox.test(age ~ quadrant2_cluster, data = df_13)
#Education
t.test(educ ~ quadrant2_cluster, data = df_13)
wilcox.test(educ ~ quadrant2_cluster, data = df_13)
#Income
t.test(income ~ quadrant2_cluster, data = df_13)
wilcox.test(income ~ quadrant2_cluster, data = df_13)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_13)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_13)
#Female 
t.test(female~ quadrant2_cluster, data = df_13)
wilcox.test(female ~ quadrant2_cluster, data = df_13)
#Immigration
t.test(im_treat~ quadrant2_cluster, data = df_13)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_13)
#Compare Quadrant 2 and 4
##########################
df_24 <- df %>% dplyr::filter(quadrant2_cluster == "2nd Quadrant" | quadrant2_cluster == "4th Quadrant")
#Age 
t.test(age ~ quadrant2_cluster, data = df_24)
wilcox.test(age ~ quadrant2_cluster, data = df_24)
#Education
t.test(educ ~ quadrant2_cluster, data = df_24)
wilcox.test(educ ~ quadrant2_cluster, data = df_24)
#Income
t.test(income ~ quadrant2_cluster, data = df_24)
wilcox.test(income ~ quadrant2_cluster, data = df_24)
#Unemplyoment
t.test(unemployed~ quadrant2_cluster, data = df_24)
wilcox.test(unemployed ~ quadrant2_cluster, data = df_24)
#Female 
t.test(female~ quadrant2_cluster, data = df_24)
wilcox.test(female ~ quadrant2_cluster, data = df_24)
#Immigration
t.test(im_treat~ quadrant2_cluster, data = df_24)
wilcox.test(im_treat ~ quadrant2_cluster, data = df_24)


test1 <- glm(Q1 ~ age+female+income+educ+unemployed+im_treat+country,data=erc, family=binomial(link="logit"))
test2 <- glm(Q2 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))
test3 <- glm(Q3 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))
test4 <- glm(Q4 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))
screenreg(list(test1,test2,test3,test4), digits = 2)

stargazer(test1,test2,test3,test4, type = "latex", title = "Regression results - Quadrants", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("1st Quadrant","2nd Quadrant",
                                                                                        "3rd Quadrant","4th Quadrant"),
          covariate.labels = c("Intercept", "Age", "Female","Income","Education", "Unemployment","Attitudes towards immigration", 
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(T1)", "(T2)", "(T3)", "(T4)"),
          notes = "SE in parentheses. T1-T4 Logistic regressions with country fixed effects.")



#Analysis of job sectors of the quadrants#
##########################################
library(janitor)
#Create tables with percentages
total_data <- tabyl(erc, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q1 <- erc %>% filter(quadrant2==1)
Q1_data <- tabyl(erc_Q1, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q2 <- erc %>% filter(quadrant2==2)
Q2_data <- tabyl(erc_Q2, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q3 <- erc %>% filter(quadrant2==3)
Q3_data <- tabyl(erc_Q3, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q4 <- erc %>% filter(quadrant2==4)
Q4_data <- tabyl(erc_Q4, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
#Rename colums for better overview
names(total_data)[names(total_data) == "employ_sec_name"] <- "Total: employ_sec_name"
names(Q1_data)[names(Q1_data) == "employ_sec_name"] <- "Q1: employ_sec_name"
names(Q2_data)[names(Q2_data) == "employ_sec_name"] <- "Q2: employ_sec_name"
names(Q3_data)[names(Q3_data) == "employ_sec_name"] <- "Q3: employ_sec_name"
names(Q4_data)[names(Q4_data) == "employ_sec_name"] <- "Q4: employ_sec_name"
#combine all tables into one dataset containing all percentages
Test <- cbind(total_data,Q1_data)
Test1 <- cbind(Q2_data,Q3_data)
Test2 <- cbind(Test,Test1)
Test3 <- cbind(Test2,Q4_data)
Quadrant_job_sector <- as.data.frame(Test3)
#Save as excel to reshape the data
library(openxlsx)

#write.xlsx(Quadrant_job_sector, 'Quadrant_job_sector.xlsx')
#Read xcel into R

Quadrant_job_sector <- read.xlsx('Quadrant_job_sector.xlsx')
library(xtable)
xtable(Quadrant_job_sector)
###################################################################################################################################

#===============================================================#
#    Second group of Quadrant: (D_nostalgic_dep+D_futOp_eco)    #
#===============================================================#

erc$quadrant1[erc$Qb1==1]<-1
erc$quadrant1[erc$Qb2==1]<-2
erc$quadrant1[erc$Qb3==1]<-3
erc$quadrant1[erc$Qb4==1]<-4
#Cluster variable
erc$quadrant1_cluster[erc$Qb1==1]<-"1st Quadrant"
erc$quadrant1_cluster[erc$Qb2==1]<-"2nd Quadrant"
erc$quadrant1_cluster[erc$Qb3==1]<-"3rd Quadrant"
erc$quadrant1_cluster[erc$Qb4==1]<-"4th Quadrant"
table(erc$quadrant1_cluster)

plot4.1 <- ggplot(erc, aes(x=quadrant1))+geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Quadrants based on nostalgic deprivation and future (economic) opportunity", y="")+ 
  theme(axis.title.x=element_text(size = 10))

figure4 <- ggarrange(plot4.1,ncol = 1, nrow = 1)

ggsave("plot4_v2.pdf", width = 17, height = 10, units = "cm")


#Quadrant 1
erc_q1 <- erc %>% filter(quadrant1==1)
erc_q1 <- dplyr::select(erc_q1, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q1 <- na.omit(erc_q1)
mean(erc_q1$educ) #4.43
mean(erc_q1$income) #4.08
mean(erc_q1$unemployed) #0.24
mean(erc_q1$age) #45
mean(erc_q1$female) #0.60
mean(erc_q1$im_treat) #2.86

#Quadrant 2
erc_q2 <- erc %>% filter(quadrant1==2)
erc_q2 <- dplyr::select(erc_q2, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q2 <- na.omit(erc_q2)
mean(erc_q2$educ) #5.03
mean(erc_q2$income) #5.77
mean(erc_q2$unemployed) #0.06
mean(erc_q2$age) #43.41
mean(erc_q2$female) #0.50
mean(erc_q2$im_treat) #2.80

#Quadrant 3
erc_q3 <- erc %>% filter(quadrant1==3)
erc_q3 <- dplyr::select(erc_q3, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q3 <- na.omit(erc_q3)
mean(erc_q3$educ) #4.85
mean(erc_q3$income) #5.00
mean(erc_q3$unemployed) #0.18
mean(erc_q3$age) #45.08
mean(erc_q3$female) #0.51
mean(erc_q3$im_treat) #2.45

#Quadrant 4
erc_q4 <- erc %>% filter(quadrant1==4)
erc_q4 <- dplyr::select(erc_q4, c(age,female,income,educ,unemployed,im_treat,perc_inq,employ_sec_name,employ_sec))
erc_q4 <- na.omit(erc_q4)
mean(erc_q4$educ) #5.44
mean(erc_q4$income) #6.57
mean(erc_q4$unemployed) #0.04
mean(erc_q4$age) #43.14
mean(erc_q4$female) #0.45
mean(erc_q4$im_treat) #2.47

#Create Dataset to test wether difference in means are stastically signficant 
df <- dplyr::select(erc, c(age,female,income,educ,unemployed,quadrant1,im_treat,employ_sec_name,employ_sec))
df$quadrant1[is.na(df$quadrant1)] <- 0
table(df$quadrant1)
df <- df %>% dplyr::filter(quadrant1 != 0)
#create the variable in the dataset to group the data
df$quadrant1_cluster[df$quadrant1==1]<-"1st Quadrant (v2)"
df$quadrant1_cluster[df$quadrant1==2]<-"2nd Quadrant (v2)"
df$quadrant1_cluster[df$quadrant1==3]<-"3rd Quadrant (v2)"
df$quadrant1_cluster[df$quadrant1==4]<-"4th Quadrant (v2)"
table(df$quadrant1_cluster)

p1 <-ggplot(df, aes(x=educ))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Education", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p2 <-ggplot(df, aes(x=income))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Income", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p3 <-ggplot(df, aes(x=age))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Age", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p4 <-ggplot(df, aes(x=female))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Female", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p5 <-ggplot(df, aes(x=unemployed))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Unemployment", y="")+ 
  theme(axis.title.x=element_text(size = 10))
p6 <-ggplot(df, aes(x=im_treat))+facet_wrap(~ quadrant1_cluster)+
  geom_bar(stat = "count", fill='#999999')+ 
  theme_bw()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = -0.4, unit = "cm"))+
  theme(axis.text.x = element_text(size = 10), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(axis.text.y = element_text(size = 10))+
  labs(x="Attitudes towards immigration", y="")+ 
  theme(axis.title.x=element_text(size = 10))

figure_qv2 <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2, nrow = 3)

ggsave("plot_quadrants_v2.pdf", width = 17, height = 20, units = "cm")


#Compare Quadrant 1 and 2#
##########################
df_12 <- df %>% dplyr::filter(quadrant1_cluster == "1st Quadrant" | quadrant1_cluster == "2nd Quadrant")
df_12_12 <- df_12 %>% dplyr::filter(employ_sec == 1 | quadrant1_cluster == 2)

#Age
t.test(age ~ quadrant1_cluster, data = df_12)
wilcox.test(age ~ quadrant1_cluster, data = df_12)
#Education
t.test(educ ~ quadrant1_cluster, data = df_12)
wilcox.test(educ ~ quadrant1_cluster, data = df_12)
#Income
t.test(income ~ quadrant1_cluster, data = df_12)
wilcox.test(income ~ quadrant1_cluster, data = df_12)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_12)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_12)
#Female
t.test(female~ quadrant1_cluster, data = df_12)
wilcox.test(female ~ quadrant1_cluster, data = df_12)
#Immigration
t.test(im_treat~ quadrant1_cluster, data = df_12)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_12)
#Compare Quadrant 2 and 3
##########################
df_23 <- df %>% dplyr::filter(quadrant1_cluster == "3rd Quadrant" | quadrant1_cluster == "2nd Quadrant")
#Age #No statical snificant difference
t.test(age ~ quadrant1_cluster, data = df_23)
wilcox.test(age ~ quadrant1_cluster, data = df_23)
#Education
t.test(educ ~ quadrant1_cluster, data = df_23)
wilcox.test(educ ~ quadrant1_cluster, data = df_23)
#Income
t.test(income ~ quadrant1_cluster, data = df_23)
wilcox.test(income ~ quadrant1_cluster, data = df_23)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_23)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_23)
#Female #No statical snificant difference #=======================RESULT=======================#
t.test(female~ quadrant1_cluster, data = df_23)
wilcox.test(female ~ quadrant1_cluster, data = df_23)
#Immigration
t.test(im_treat~ quadrant1_cluster, data = df_23)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_23)
#Compare Quadrant 3 and 4
##########################
df_34 <- df %>% dplyr::filter(quadrant1_cluster == "3rd Quadrant" | quadrant1_cluster == "4th Quadrant")
#Age   #No statical snificant difference 
t.test(age ~ quadrant1_cluster, data = df_34) 
wilcox.test(age ~ quadrant1_cluster, data = df_34)
#Education
t.test(educ ~ quadrant1_cluster, data = df_34)
wilcox.test(educ ~ quadrant1_cluster, data = df_34)
#Income
t.test(income ~ quadrant1_cluster, data = df_34)
wilcox.test(income ~ quadrant1_cluster, data = df_34)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_34)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_34)
#Female  
t.test(female~ quadrant1_cluster, data = df_34)
wilcox.test(female ~ quadrant1_cluster, data = df_34)
#Immigration #No statical snificant difference #=======================RESULT=======================#
t.test(im_treat~ quadrant1_cluster, data = df_34)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_34)
#Compare Quadrant 1 and 4
##########################
df_14 <- df %>% dplyr::filter(quadrant1_cluster == "1st Quadrant" | quadrant1_cluster == "4th Quadrant")
#Age
t.test(age ~ quadrant1_cluster, data = df_14)
wilcox.test(age ~ quadrant1_cluster, data = df_14)
#Education
t.test(educ ~ quadrant1_cluster, data = df_14)
wilcox.test(educ ~ quadrant1_cluster, data = df_14)
#Income
t.test(income ~ quadrant1_cluster, data = df_14)
wilcox.test(income ~ quadrant1_cluster, data = df_14)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_14)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_14)
#Female 
t.test(female~ quadrant1_cluster, data = df_14)
wilcox.test(female ~ quadrant1_cluster, data = df_14)
#Immigration
t.test(im_treat~ quadrant1_cluster, data = df_14)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_14)
#Compare Quadrant 1 and 3
##########################
df_13 <- df %>% dplyr::filter(quadrant1_cluster == "1st Quadrant" | quadrant1_cluster == "3rd Quadrant")
#Age #No statical snificant difference  #================ RESULT =================#
t.test(age ~ quadrant1_cluster, data = df_13)
wilcox.test(age ~ quadrant1_cluster, data = df_13)
#Education
t.test(educ ~ quadrant1_cluster, data = df_13)
wilcox.test(educ ~ quadrant1_cluster, data = df_13)
#Income
t.test(income ~ quadrant1_cluster, data = df_13)
wilcox.test(income ~ quadrant1_cluster, data = df_13)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_13)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_13)
#Female 
t.test(female~ quadrant1_cluster, data = df_13)
wilcox.test(female ~ quadrant1_cluster, data = df_13)
#Immigration
t.test(im_treat~ quadrant1_cluster, data = df_13)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_13)
#Compare Quadrant 2 and 4
##########################
df_24 <- df %>% dplyr::filter(quadrant1_cluster == "2nd Quadrant" | quadrant1_cluster == "4th Quadrant")
#Age 
t.test(age ~ quadrant1_cluster, data = df_24)
wilcox.test(age ~ quadrant1_cluster, data = df_24)
#Education
t.test(educ ~ quadrant1_cluster, data = df_24)
wilcox.test(educ ~ quadrant1_cluster, data = df_24)
#Income
t.test(income ~ quadrant1_cluster, data = df_24)
wilcox.test(income ~ quadrant1_cluster, data = df_24)
#Unemplyoment
t.test(unemployed~ quadrant1_cluster, data = df_24)
wilcox.test(unemployed ~ quadrant1_cluster, data = df_24)
#Female 
t.test(female~ quadrant1_cluster, data = df_24)
wilcox.test(female ~ quadrant1_cluster, data = df_24)
#Immigration
t.test(im_treat~ quadrant1_cluster, data = df_24)
wilcox.test(im_treat ~ quadrant1_cluster, data = df_24)


test1.1 <- glm(Qb1 ~ age+female+income+educ+unemployed+im_treat+country,data=erc, family=binomial(link="logit"))
test2.1 <- glm(Qb2 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))
test3.1 <- glm(Qb3 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))
test4.1 <- glm(Qb4 ~ age+female+income+educ+unemployed+im_treat+country, data=erc, family=binomial(link="logit"))


stargazer(test1.1,test2.1,test3.1,test4.1, type = "latex", title = "Regression results - quadrants (v2)", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("1st Quadrant (v2)","2nd Quadrant (v2)",
                                                                                        "3rd Quadrant (v2)","4th Quadrant (v2)"),
          covariate.labels = c("Intercept", "Age", "Female","Income","Education", "Unemployment","Attitudes towards immigration", 
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(T5)", "(T6)", "(T7)", "(T8)"),
          notes = "SE in parentheses. T5-T8 Logistic regressions with country fixed effects.")



#Analysis of job sectors of the quadrants#
##########################################
library(janitor)
#Create tables with percentages
total_data <- tabyl(erc, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q1 <- erc %>% filter(quadrant1==1)
Q1_data <- tabyl(erc_Q1, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q2 <- erc %>% filter(quadrant1==2)
Q2_data <- tabyl(erc_Q2, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q3 <- erc %>% filter(quadrant1==3)
Q3_data <- tabyl(erc_Q3, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
erc_Q4 <- erc %>% filter(quadrant1==4)
Q4_data <- tabyl(erc_Q4, employ_sec_name)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
#Rename colums for better overview
names(total_data)[names(total_data) == "employ_sec_name"] <- "Total: employ_sec_name"
names(Q1_data)[names(Q1_data) == "employ_sec_name"] <- "Q1: employ_sec_name"
names(Q2_data)[names(Q2_data) == "employ_sec_name"] <- "Q2: employ_sec_name"
names(Q3_data)[names(Q3_data) == "employ_sec_name"] <- "Q3: employ_sec_name"
names(Q4_data)[names(Q4_data) == "employ_sec_name"] <- "Q4: employ_sec_name"
#combine all tables into one dataset containing all percentages
Test <- cbind(total_data,Q1_data)
Test1 <- cbind(Q2_data,Q3_data)
Test2 <- cbind(Test,Test1)
Test3 <- cbind(Test2,Q4_data)
Quadrant_job_sector2 <- as.data.frame(Test3)
#Save as excel to reshape the data
library(openxlsx)

write.xlsx(Quadrant_job_sector2, 'Quadrant_job_sector2.xlsx')
#Read xcel into R

Quadrant_job_sector2 <- read.xlsx('Quadrant_job_sector2.xlsx')
library(xtable)
xtable(Quadrant_job_sector2)

###############################################################################################################################################################################
###############################################################################################################################################################################


#=============================================================================================#
#======                                                                                 ======#
#======                                     Models                                      ======#
#======                                                                                 ======#
#=============================================================================================#

#Model: nostaglic_dep
a1  <- glm(FR_vote ~ nostalgic_dep, data=erc, weights = m_weigt, family=binomial(link="logit"))
a2  <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ, data=erc,weights = m_weigt, family=binomial(link="logit"))
a3  <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc,weights = m_weigt, family=binomial(link="logit"))
a4 <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, family=binomial(link="logit"))
screenreg(list(a1,a2,a3,a4), digits = 2)

stargazer(a1,a2,a3,a4, type = "latex", title = "Regression results: Nostalgic deprivation", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party"),
          covariate.labels = c("Intercept", "Nostalgic deprivation", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(M5)", "(M6)", "(M7)", "(M8)"),
          notes = "SE in parentheses. M5-M8 Logistic regressions, M8 also with country fixed effects.")



#estimate models with factor variables
a4.1 <- glm(FR_vote ~ nostalgic_dep.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
            data=erc, weights = m_weigt, family=binomial(link="logit"))
a4.2 <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ+unemployed.f+im_treat+perc_inq+country, 
            data=erc, weights = m_weigt, family=binomial(link="logit"))
a4.3 <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
            data=erc, weights = m_weigt, family=binomial(link="logit"))
a4.4 <- glm(FR_vote ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq.f+country, 
            data=erc, weights = m_weigt, family=binomial(link="logit"))
#create plot
plot6.1.1 <- plot_model(a4.1, type = "pred", terms = c("nostalgic_dep.f"),title = "",
                        axis.title = c("Nostalgic deprivation","Voted for far-right party"))
plot6.1.2 <- plot_model(a4.2, type = "pred", terms = c("unemployed.f"),title = "",
                        axis.title = c("Unemploymend","Voted for far-right party"))
plot6.1.3 <- plot_model(a4.3, type = "pred", terms = c("im_treat.f"),title = "",
                        axis.title = c("Attitudes towards immigration","Voted for far-right party"))
plot6.1.4 <- plot_model(a4.4, type = "pred", terms = c("perc_inq.f"),title = "",
                        axis.title = c("Attitudes towards inequality","Voted for far-right party"))
figure6.1 <- ggarrange(plot6.1.1,plot6.1.3,plot6.1.4, ncol = 3, nrow = 1)
ggsave("plot6.1.pdf", width = 25, height = 10, units = "cm")



#Model: Future Opportunities
b1  <- glm(FR_vote ~ futOp_soc, data=erc, weights = m_weigt,family=binomial(link="logit"))
b2  <- glm(FR_vote ~ futOp_soc+age+female+income+educ, data=erc, weights = m_weigt,family=binomial(link="logit"))
b3  <- glm(FR_vote ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc, weights = m_weigt,family=binomial(link="logit"))
b4 <- glm(FR_vote ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt,family=binomial(link="logit"))


stargazer(b1,b2,b3,b4, type = "latex", title = "Regression results: Future opportunities", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party"),
          covariate.labels = c("Intercept", "Future opportunity", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(M9)", "(M10)", "(M11)", "(M12)"),
          notes = "SE in parentheses. M9-M12 Logistic regressions, M12 also with country fixed effects.")


#estimate models with factor variables
b4.1 <- glm(FR_vote ~ futOp_soc.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
b4.2 <- glm(FR_vote ~ futOp_soc+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
b4.3 <- glm(FR_vote ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq.f+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
#create plot
plot7.1.1 <- plot_model(b4.1, type = "pred", terms = c("futOp_soc.f"),title = "",
                        axis.title = c("Future opportunity","Voted for far-right party"))
plot7.1.2 <- plot_model(b4.2, type = "pred", terms = c("im_treat.f"),title = "",
                        axis.title = c("Attitudes towards immigration","Voted for far-right party"))
plot7.1.3 <- plot_model(b4.3, type = "pred", terms = c("perc_inq.f"),title = "",
                        axis.title = c("Attitudes towards inequality","Voted for far-right party"))
figure7.1 <- ggarrange(plot7.1.1,plot7.1.2,plot7.1.3, ncol = 3, nrow = 1)
ggsave("plot7.1.pdf", width = 25, height = 10, units = "cm")

#===============================#
#     Quadrant Variable -  Q    #
#===============================#
# (D_nostalgic_dep+D_futOp_soc) #
#===============================#
#AV: FR_vote 
q6  <- glm(FR_vote ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q1+Q2+Q3+Q4, data=erc, weights = m_weigt, family=binomial(link="logit"))
q7  <- glm(FR_vote ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q1, data=erc, weights = m_weigt, family=binomial(link="logit"))
q8  <- glm(FR_vote ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q2, data=erc, weights = m_weigt, family=binomial(link="logit"))
q9  <- glm(FR_vote ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q3, data=erc, weights = m_weigt, family=binomial(link="logit"))
q10  <- glm(FR_vote ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Q4, data=erc, weights = m_weigt, family=binomial(link="logit"))

stargazer(q6,q7,q8,q9,q10, type = "latex", title = "Regression results: quadrant analysis", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party"),
          covariate.labels = c("Intercept", "Country fixed effects","3löschen","löschen","löschen","löschen","löschen","löschen","Age","Female",
                               "Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Quadrant 1","Quadrant 2","Quadrant 3","Quadrant 4"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(M13)", "(M14)", "(M15)", "(M16)","(M17)"),
          notes = "SE in parentheses. M13-M17 Logistic regressions with country fixed effects.")

#create plot
q7.1  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q1.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q8.1  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q2.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q9.1  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q3.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q10.1  <- glm(FR_vote ~ country+
                age+female+income+educ+unemployed+im_treat+perc_inq+
                Q4.f, data=erc, weights = m_weigt, family=binomial(link="logit"))

plot8.2.1 <- plot_model(q7.1, type = "pred", terms = c("Q1.f"),title = "",
                        axis.title = c("Quadrant 1","Voted for far-right party"))
plot8.2.2 <- plot_model(q8.1, type = "pred", terms = c("Q2.f"),title = "",
                        axis.title = c("Quadrant 2","Voted for far-right party"))
plot8.2.3 <- plot_model(q9.1, type = "pred", terms = c("Q3.f"),title = "",
                        axis.title = c("Quadrant 3","Voted for far-right party"))
plot8.2.4 <- plot_model(q10.1, type = "pred", terms = c("Q4.f"),title = "",
                        axis.title = c("Quadrant 4","Voted for far-right party"))
figure8 <- ggarrange(plot8.2.1,plot8.2.2,plot8.2.3,plot8.2.4 ,ncol = 2, nrow = 2)
ggsave("plot8.pdf", width = 25, height = 20, units = "cm")
#=============================================================================================#
#======                                                                                 ======#
#======                          Robustness Checks main model                           ======#
#======                                                                                 ======#
#=============================================================================================#
#Model: nostalgic_dep with vote intension
a5  <- glm(FR_int ~ nostalgic_dep, data=erc, weights = m_weigt, family=binomial(link="logit"))
a6  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ, data=erc, weights = m_weigt, family=binomial(link="logit"))
a7  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc, weights = m_weigt, family=binomial(link="logit"))
a8  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt, family=binomial(link="logit"))

stargazer(a5,a6,a7,a8, type = "latex", title = "Robustness test -  Far-right party vote intension: Nostalgic deprivation", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Far-right party vote intension"),
          covariate.labels = c("Intercept", "Nostalgic deprivation", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R37)", "(R38)", "(R39)", "(R40)"),
          notes = "SE in parentheses. R37-R40 Logistic regressions, R40 also with country fixed effects.")

#estimate models with factor variables
a8.1  <- glm(FR_int ~ nostalgic_dep.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
             data=erc, weights = m_weigt, family=binomial(link="logit"))
a8.2  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ+unemployed.f+im_treat+perc_inq+country, 
             data=erc, weights = m_weigt, family=binomial(link="logit"))
a8.3  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
             data=erc, weights = m_weigt, family=binomial(link="logit"))
a8.4  <- glm(FR_int ~ nostalgic_dep+age+female+income+educ+unemployed+im_treat+perc_inq.f+country, 
             data=erc, weights = m_weigt, family=binomial(link="logit"))
#create plot
plot6.2.1 <- plot_model(a8.1, type = "pred", terms = c("nostalgic_dep.f"),title = "",
                        axis.title = c("Nostalgic deprivation","Far-right party vote intension"))
plot6.2.2 <- plot_model(a8.2, type = "pred", terms = c("unemployed.f"),title = "",
                        axis.title = c("Unemploymend","Far-right party vote intension"))
plot6.2.3 <- plot_model(a8.3, type = "pred", terms = c("im_treat.f"),title = "",
                        axis.title = c("Attitudes towards immigration","Far-right party vote intension"))
plot6.2.4 <- plot_model(a8.4, type = "pred", terms = c("perc_inq.f"),title = "",
                        axis.title = c("Attitudes towards inequality","Far-right party vote intension"))
figure6.2 <- ggarrange(plot6.2.1,plot6.2.3,plot6.2.4, ncol = 3, nrow = 1)
ggsave("plot6.2.pdf", width = 25, height = 10, units = "cm")


#Model: Future Oportunity with vote intension
b5  <- glm(FR_int ~ futOp_soc, data=erc, weights = m_weigt,family=binomial(link="logit"))
b6  <- glm(FR_int ~ futOp_soc+age+female+income+educ, data=erc,weights = m_weigt, family=binomial(link="logit"))
b7  <- glm(FR_int ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq, weights = m_weigt,data=erc, family=binomial(link="logit"))
b8  <- glm(FR_int ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt, family=binomial(link="logit"))

stargazer(b5,b6,b7,b8, type = "latex", title = "Robustness test -  Far-right party vote intension: Future opportunities", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Far-right party vote intension"),
          covariate.labels = c("Intercept", "Future opportunity", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R56)", "(R57)", "(R58)", "(R59)"),
          notes = "SE in parentheses. R56-R59 Logistic regressions, R59 also with country fixed effects.")



#estimate models with factor variables
b8.1 <- glm(FR_int ~ futOp_soc.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
b8.2 <- glm(FR_int ~ futOp_soc+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
            data=erc,weights = m_weigt, family=binomial(link="logit"))
b8.3 <- glm(FR_int ~ futOp_soc+age+female+income+educ+unemployed+im_treat+perc_inq.f+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
#create plot
plot7.2.1 <- plot_model(b8.1, type = "pred", terms = c("futOp_soc.f"),title = "",
                        axis.title = c("Future opportunity","Far-right party vote intension"))
plot7.2.2 <- plot_model(b8.2, type = "pred", terms = c("im_treat.f"),title = "",
                        axis.title = c("Attitudes towards immigration","Far-right party vote intension"))
plot7.2.3 <- plot_model(b8.3, type = "pred", terms = c("perc_inq.f"),title = "",
                        axis.title = c("Attitudes towards inequality","Far-right party vote intension"))
figure7.2 <- ggarrange(plot7.2.1,plot7.2.2,plot7.2.3, ncol = 3, nrow = 1)
ggsave("plot7.2.pdf", width = 25, height = 10, units = "cm")





#=============================================================================================#
#======                                                                                 ======#
#======                        Robustness Checks similar variables model                ======#
#======                                                                                 ======#
#=============================================================================================#
#Model: nostalgia2
c1  <- glm(FR_vote ~ nostalgia2, data=erc, weights = m_weigt, family=binomial(link="logit"))
c2  <- glm(FR_vote ~ nostalgia2+age+female+income+educ, data=erc,weights = m_weigt, family=binomial(link="logit"))
c3  <- glm(FR_vote ~ nostalgia2+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc,weights = m_weigt, family=binomial(link="logit"))
c4 <- glm(FR_vote ~ nostalgia2+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, family=binomial(link="logit"))
c5  <- glm(FR_int ~ nostalgia2, data=erc,weights = m_weigt, family=binomial(link="logit"))
c6  <- glm(FR_int ~ nostalgia2+age+female+income+educ, data=erc,weights = m_weigt, family=binomial(link="logit"))
c7  <- glm(FR_int ~ nostalgia2+age+female+income+educ+unemployed+im_treat+perc_inq,weights = m_weigt, data=erc, family=binomial(link="logit"))
c8  <- glm(FR_int ~ nostalgia2+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, family=binomial(link="logit"))
screenreg(list(c1,c2,c3,c4,c5,c6,c7,c8), digits = 2)

stargazer(c1,c2,c3,c4,c5,c6,c7,c8, type = "latex", title = "Robustness test - Nostalgic deprivation (10 years)", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party","Far-right party vote intension"),
          covariate.labels = c("Intercept", "Nostalgic deprivation (10 years)", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R41)", "(R42)", "(R43)", "(R44)","(R45)", "(R46)", "(R47)", "(R48)"),
          notes = "SE in parentheses. R50-R57 Logistic regressions, R44 and R48 also with country fixed effects.")

#create plot
c4.1 <- glm(FR_vote ~ nostalgia2.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, 
            family=binomial(link="logit"))
c4.2 <- glm(FR_vote ~ nostalgia2+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, data=erc,weights = m_weigt, 
            family=binomial(link="logit"))
c8.1  <- glm(FR_int ~ nostalgia2.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, 
             family=binomial(link="logit"))
c8.2  <- glm(FR_int ~ nostalgia2+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, data=erc,weights = m_weigt, 
             family=binomial(link="logit"))
plot_rb_nostalgia2_1 <- plot_model(c4.1, type = "pred", terms = c("nostalgia2.f"),title = "",
                                   axis.title = c("Nostalgic deprivation (10 years)","Voted for far-right party"))
plot_rb_nostalgia2_1.1 <- plot_model(c4.2, type = "pred", terms = c("im_treat.f"),title = "",
                                     axis.title = c("Attitudes towards immigration","Voted for far-right party"))
plot_rb_nostalgia2_2 <- plot_model(c8.1, type = "pred", terms = c("nostalgia2.f"),title = "",
                                   axis.title = c("Nostalgic deprivation (10 years)","Far-right party vote intension"))
plot_rb_nostalgia2_2.1 <- plot_model(c8.2, type = "pred", terms = c("im_treat.f"),title = "",
                                     axis.title = c("Attitudes towards immigration","Far-right party vote intension"))
figure_rb_nostalgia2 <- ggarrange(plot_rb_nostalgia2_1,plot_rb_nostalgia2_1.1,plot_rb_nostalgia2_2,plot_rb_nostalgia2_2.1,ncol = 2, nrow = 2)

ggsave("plotrb_nostalgia2.pdf", width = 15, height = 20, units = "cm")


#Model: socmob
d1  <- glm(FR_vote ~ socmob, data=erc, weights = m_weigt, family=binomial(link="logit"))
d2  <- glm(FR_vote ~ socmob+age+female+income+educ, data=erc, weights = m_weigt, family=binomial(link="logit"))
d3  <- glm(FR_vote ~ socmob+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc, weights = m_weigt, family=binomial(link="logit"))
d4 <- glm(FR_vote ~ socmob+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt, family=binomial(link="logit"))
d5  <- glm(FR_int ~ socmob, data=erc, weights = m_weigt, family=binomial(link="logit"))
d6  <- glm(FR_int ~ socmob+age+female+income+educ, data=erc, weights = m_weigt, family=binomial(link="logit"))
d7  <- glm(FR_int ~ socmob+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc, weights = m_weigt, family=binomial(link="logit"))
d8 <- glm(FR_int ~ socmob+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt, family=binomial(link="logit"))
screenreg(list(d1,d2,d3,d4,d5,d6,d7,d8), digits = 2)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8, type = "latex", title = "Robustness test - Intergenerational social mobility", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party","Far-right party vote intension"),
          covariate.labels = c("Intercept", "Intergenerational social mobility", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R48)", "(R49)", "(R50)", "(R51)","(R52)", "(R53)", "(R54)", "(R55)"),
          notes = "SE in parentheses. R48-R55 Logistic regressions, R51 and R55 also with country fixed effects.")
#create plot
d4.1 <- glm(FR_vote ~ socmob.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, 
            family=binomial(link="logit"))
d4.2 <- glm(FR_vote ~ socmob+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, data=erc,weights = m_weigt, 
            family=binomial(link="logit"))
d8.1  <- glm(FR_int ~ socmob.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, 
             family=binomial(link="logit"))
d8.2  <- glm(FR_int ~ socmob+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, data=erc,weights = m_weigt, 
             family=binomial(link="logit"))
plot_rb_socmob_1 <- plot_model(d4.1, type = "pred", terms = c("socmob.f"),title = "",
                               axis.title = c("Intergenerational social mobility","Voted for far-right party"))
plot_rb_socmob_1.1 <- plot_model(d4.2, type = "pred", terms = c("im_treat.f"),title = "",
                                 axis.title = c("Attitudes towards immigration","Voted for far-right party"))
plot_rb_socmob_2 <- plot_model(d8.1, type = "pred", terms = c("socmob.f"),title = "",
                               axis.title = c("Intergenerational social mobility","Far-right party vote intension"))
plot_rb_socmob_2.1 <- plot_model(d8.2, type = "pred", terms = c("im_treat.f"),title = "",
                                 axis.title = c("Attitudes towards immigration","Far-right party vote intension"))
figure_rb_socmob <- ggarrange(plot_rb_socmob_1,plot_rb_socmob_1.1,plot_rb_socmob_2,plot_rb_socmob_2.1,ncol = 2, nrow = 2)

ggsave("plotrb_socmob.pdf", width =15, height = 20, units = "cm")


#Model: Future economic Opportunities
e1  <- glm(FR_vote ~ futOp_eco, data=erc, weights = m_weigt,family=binomial(link="logit"))
e2  <- glm(FR_vote ~ futOp_eco+age+female+income+educ, data=erc,weights = m_weigt, family=binomial(link="logit"))
e3  <- glm(FR_vote ~ futOp_eco+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc,weights = m_weigt, family=binomial(link="logit"))
e4 <- glm(FR_vote ~ futOp_eco+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc,weights = m_weigt, family=binomial(link="logit"))
e5  <- glm(FR_int ~ futOp_eco, data=erc,weights = m_weigt, family=binomial(link="logit"))
e6  <- glm(FR_int ~ futOp_eco+age+female+income+educ,weights = m_weigt, data=erc, family=binomial(link="logit"))
e7  <- glm(FR_int ~ futOp_eco+age+female+income+educ+unemployed+im_treat+perc_inq, data=erc, weights = m_weigt,family=binomial(link="logit"))
e8 <- glm(FR_int ~ futOp_eco+age+female+income+educ+unemployed+im_treat+perc_inq+country, data=erc, weights = m_weigt,family=binomial(link="logit"))
screenreg(list(e1,e2,e3,e4,e5,e6,e7,e8), digits = 2)

stargazer(e1,e2,e3,e4,e5,e6,e7,e8, type = "latex", title = "Robustness test - Future economic opportunities", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party","Far-right party vote intension"),
          covariate.labels = c("Intercept", "Future (economic) opportunity", "Age","Female","Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Country fixed effects"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R60)", "(R61)", "(R62)", "(R63)","(R64)", "(R65)", "(R66)", "(R67)"),
          notes = "SE in parentheses. R60-R67 Logistic regressions, R63 and R67 also with country fixed effects.")

#create plot
e4.1 <- glm(FR_vote ~ futOp_eco.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
            data=erc,weights = m_weigt, family=binomial(link="logit"))
e4.2 <- glm(FR_vote ~ futOp_eco+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
            data=erc,weights = m_weigt, family=binomial(link="logit"))
e8.1 <- glm(FR_int ~ futOp_eco.f+age+female+income+educ+unemployed+im_treat+perc_inq+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
e8.2 <- glm(FR_int ~ futOp_eco+age+female+income+educ+unemployed+im_treat.f+perc_inq+country, 
            data=erc, weights = m_weigt,family=binomial(link="logit"))
plot_rb_futOp_1 <- plot_model(e4.1, type = "pred", terms = c("futOp_eco.f"),title = "",
                              axis.title = c("Future (economic) opportunity","Voted for far-right party"))
plot_rb_futOp_1.1 <- plot_model(e4.2, type = "pred", terms = c("im_treat.f"),title = "",
                                axis.title = c("Attitudes towards immigration","Voted for far-right party"))
plot_rb_futOp_2 <- plot_model(e8.1, type = "pred", terms = c("futOp_eco.f"),title = "",
                              axis.title = c("Future (economic) opportunity","Far-right party vote intension"))
plot_rb_futOp_2.1 <- plot_model(e8.2, type = "pred", terms = c("im_treat.f"),title = "",
                                axis.title = c("Attitudes towards immigration","Far-right party vote intension"))
figure_rb_socmob <- ggarrange(plot_rb_futOp_1,plot_rb_futOp_1.1,plot_rb_futOp_2,plot_rb_futOp_2.1,ncol = 2, nrow = 2)
ggsave("plotrb_futOp.pdf", width =15, height = 20, units = "cm")


#===============================#
#     Quadrant Variable -  Q    #
#===============================#
# (D_nostalgic_dep+D_futOp_soc) #
#===============================#
#AV: FR_int 
q1  <- glm(FR_int ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q1+Q2+Q3+Q4, data=erc, weights = m_weigt, family=binomial(link="logit"))
q2  <- glm(FR_int ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q1, data=erc, weights = m_weigt, family=binomial(link="logit"))
q3  <- glm(FR_int ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q2, data=erc, weights = m_weigt, family=binomial(link="logit"))
q4  <- glm(FR_int ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q3, data=erc, weights = m_weigt, family=binomial(link="logit"))
q5  <- glm(FR_int ~ country+
             age+female+income+educ+unemployed+im_treat+perc_inq+
             Q4, data=erc, weights = m_weigt, family=binomial(link="logit"))

stargazer(q1,q2,q3,q4,q5, type = "latex", title = "Robustness test - Far-right party vote intension: quadrant analysis", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Far-right party vote intension"),
          covariate.labels = c("Intercept", "Country fixed effects","löschen","löschen","löschen","löschen","löschen","löschen","Age","Female",
                               "Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Quadrant 1","Quadrant 2","Quadrant 3","Quadrant 4"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R68)", "(R69)", "(R70)", "(R71)","(R72)"),
          notes = "SE in parentheses. R73-R82 Logistic regressions with country fixed effects.")

#create plot
q2.1  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q1.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q3.1  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q2.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q4.1  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Q3.f, data=erc, weights = m_weigt, family=binomial(link="logit"))
q5.1 <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Q4.f, data=erc, weights = m_weigt, family=binomial(link="logit"))

plot8.1 <- plot_model(q2.1, type = "pred", terms = c("Q1.f"),title = "",
                      axis.title = c("Quadrant 1","Far-right party vote intension"))
plot8.2 <- plot_model(q3.1, type = "pred", terms = c("Q2.f"),title = "",
                      axis.title = c("Quadrant 2","Far-right party vote intension"))
plot8.3 <- plot_model(q4.1, type = "pred", terms = c("Q3.f"),title = "",
                      axis.title = c("Quadrant 3","Far-right party vote intension"))
plot8.4 <- plot_model(q5.1, type = "pred", terms = c("Q4.f"),title = "",
                      axis.title = c("Quadrant 4","Far-right party vote intension"))
figure8 <- ggarrange(plot8.1,plot8.2,plot8.3,plot8.4 ,ncol = 2, nrow = 2)
ggsave("plot8_2.pdf", width = 25, height = 20, units = "cm")
#===============================#
#     Quadrant Variable -  Qb   #
#===============================#
# (D_nostalgic_dep+D_futOp_eco) #
#===============================#
#AV: FR_vote 
qb6  <- glm(FR_vote ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb1+Qb2+Qb3+Qb4, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb7  <- glm(FR_vote ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb1, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb8  <- glm(FR_vote ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb2, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb9  <- glm(FR_vote ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb3, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb10  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qb4, data=erc, weights = m_weigt,family=binomial(link="logit"))
#AV: FR_int 
qb1  <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb1+Qb2+Qb3+Qb4, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb2  <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb1, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb3  <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb2, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb4  <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb3, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb5  <- glm(FR_int ~ country+
              age+female+income+educ+unemployed+im_treat+perc_inq+
              Qb4, data=erc,weights = m_weigt, family=binomial(link="logit"))

stargazer(qb6,qb7,qb8,qb9,qb10,qb1,qb2,qb3,qb4,qb5, type = "latex", title = "Robustness test -  quadrant analysis  (v2)", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party","Far-right party vote intension"),
          covariate.labels = c("Intercept", "Country fixed effects","löschen","löschen","löschen","löschen","löschen","löschen","Age","Female",
                               "Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Quadrant 1 (v2)","Quadrant 2 (v2)","Quadrant 3 (v2)","Quadrant 4 (v2)"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R73)", "(R74)", "(R75)", "(R76)","(R77)","(R78)","(R79)","(R80)","(R81)","(R82)"),
          notes = "SE in parentheses. R73-R82 Logistic regressions with country fixed effects.")

#Create plots for predicted probabilites
#FR_vote
qb7.1  <- glm(FR_vote ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb1.f, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb8.1  <- glm(FR_vote ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb2.f, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb9.1  <- glm(FR_vote ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb3.f, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb10.1  <- glm(FR_vote ~ country+  age+female+income+educ+unemployed+im_treat+perc_inq+
                 Qb4.f, data=erc,weights = m_weigt, family=binomial(link="logit"))
#FR_int
qb1.1  <- glm(FR_int ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb1.f, data=erc,weights = m_weigt, family=binomial(link="logit"))
qb2.1  <- glm(FR_int ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb2.f, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb3.1  <- glm(FR_int ~ country+age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb3.f, data=erc, weights = m_weigt,family=binomial(link="logit"))
qb4.1  <- glm(FR_int ~ country+  age+female+income+educ+unemployed+im_treat+perc_inq+
                Qb4.f, data=erc,weights = m_weigt, family=binomial(link="logit"))

#Predicted probability plots
plot9.1 <- plot_model(qb7.1, type = "pred", terms = c("Qb1.f"),title = "",
                      axis.title = c("Quadrant 1 (v2)","Voted for far-right party"))
plot9.2 <- plot_model(qb8.1, type = "pred", terms = c("Qb2.f"),title = "",
                      axis.title = c("Quadrant 2 (v2)","Voted for far-right party"))
plot9.3 <- plot_model(qb9.1, type = "pred", terms = c("Qb3.f"),title = "",
                      axis.title = c("Quadrant 3 (v2)","Voted for far-right party"))
plot9.4 <- plot_model(qb10.1, type = "pred", terms = c("Qb4.f"),title = "",
                      axis.title = c("Quadrant 4 (v2)","Voted for far-right party"))
plot9.5 <- plot_model(qb1.1, type = "pred", terms = c("Qb1.f"),title = "",
                      axis.title = c("Quadrant 1 (v2)","Far-right party vote intension"))
plot9.6 <- plot_model(qb2.1, type = "pred", terms = c("Qb2.f"),title = "",
                      axis.title = c("Quadrant 2 (v2)","Far-right party vote intension"))
plot9.7 <- plot_model(qb3.1, type = "pred", terms = c("Qb3.f"),title = "",
                      axis.title = c("Quadrant 3 (v2)","Far-right party vote intension"))
plot9.8 <- plot_model(qb4.1, type = "pred", terms = c("Qb4.f"),title = "",
                      axis.title = c("Quadrant 4 (v2)","Far-right party vote intension"))

figure9 <- ggarrange(plot9.1,plot9.2,plot9.3,plot9.4,plot9.5,plot9.6,plot9.7,plot9.8 ,ncol = 4, nrow = 2)
ggsave("plot9.pdf", width = 25, height = 20, units = "cm")


#===============================#
#     Quadrant Variable -  Qb   #
#===============================#
# (D_nostalgic_dep+D_futOp_eco) #
#===============================#
#AV: FR_vote 
qbr6  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr1+Qbr2+Qbr3+Qbr4, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr7  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr1, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr8  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr2, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr9  <- glm(FR_vote ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr3, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr10  <- glm(FR_vote ~ country+
                age+female+income+educ+unemployed+im_treat+perc_inq+
                Qbr4, data=erc, weights = m_weigt,family=binomial(link="logit"))
#AV: FR_int 
qbr1  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr1+Qbr2+Qbr3+Qbr4, data=erc,weights = m_weigt, family=binomial(link="logit"))
qbr2  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr1, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr3  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr2, data=erc,weights = m_weigt, family=binomial(link="logit"))
qbr4  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr3, data=erc, weights = m_weigt,family=binomial(link="logit"))
qbr5  <- glm(FR_int ~ country+
               age+female+income+educ+unemployed+im_treat+perc_inq+
               Qbr4, data=erc,weights = m_weigt, family=binomial(link="logit"))

stargazer(qbr6,qbr7,qbr8,qbr9,qbr10,qbr1,qbr2,qbr3,qbr4,qbr5, type = "latex", title = "Robustness test -  quadrant analysis  (v2)", 
          style = "default", header = F, dep.var.labels.include = T, dep.var.labels = c("Voted for far-right party","Far-right party vote intension"),
          covariate.labels = c("Intercept", "Country fixed effects","löschen","löschen","löschen","löschen","löschen","löschen","Age","Female",
                               "Income","Education", "Unemployment", 
                               "Attitudes towards immigration", "Attitudes towards inequality",
                               "Quadrant 1 (r1)","Quadrant 2 (r1)","Quadrant 3 (r1)","Quadrant 4 (r1)"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = "footnotesize", no.space = T, model.numbers = F, model.names = T, intercept.top = T, intercept.bottom = F,
          column.labels = c("(R83)", "(R84)", "(R85)", "(R86)","(R87)","(R88)","(R89)","(R90)","(R91)","(R92)"),
          notes = "SE in parentheses. R83-R92 Logistic regressions with country fixed effects.")

###############################################################################################################################################################################
###############################################################################################################################################################################

rm(list = ls())

load("erc_MA_int.Rda")
load("erc_MA_vote.Rda")

library(MatchIt)
library(Matching)
library(optmatch)
library(knitr)
library(ggplot2)

#===================================#
#                                   #
#     Nearest Neighbor Matching     #
#                                   #
#===================================#

#=======================#
#     MatchIt package   #
#=======================#
attach(erc_MA_vote)
#Matching with FR_vote
near_out_vote <- matchit(socmob_treat ~ age+female+income+educ+unemployed+im_treat+perc_inq+
                       progay+gendrequal+pro_eu+left_right+socialtrust+poltrust,
                       data = erc_MA_vote, method="nearest", ratio=1)
summary(near_out_vote)
b <- summary(near_out_vote)
#he standardized difference in means of covariates
s.out <- summary(near_out_vote, standardize = TRUE)
#plot(s.out)

#create the matched data from the MatchIt output object by excluding unmatched units from the original data
data_n_vote <- match.data(near_out_vote)
#Run Regressions with Balanced Datasets: without country effects
matchit_near_reg_vote1 <- glm(FR_vote ~ socmob_treat, data = data_n_vote)
summary(matchit_near_reg_vote1)
#Run Regressions with Balanced Datasets: with country effects
data_n_vote$socmob_treat.f <- factor(data_n_vote$socmob_treat, labels = c("No", "Yes"))
matchit_near_reg_vote2 <- glm(FR_vote ~ socmob_treat.f+country, data = data_n_vote, family = binomial())
summary(matchit_near_reg_vote2)

exp(confint(matchit_near_reg_vote2, parm = "socmob_treat.fYes"))

#Create table for Sample Size
kable(b$nn, digits = 2, align = 'c', 
      caption = 'Table 1: Sample sizes')

#Summary of balance for matched data
kable(b$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table X: Summary of balance for matched data')

# perform paired t-tests on matched data for Dependent variable: Far_vote
t.test(data_n_vote$FR_vote[data_n_vote$socmob_treat==1],data_n_vote$FR_vote[data_n_vote$socmob_treat==0],paired=TRUE)

#Estimating treatment effects
with(data_n_vote, t.test(FR_vote ~ socmob_treat))

detach(erc_MA_vote)

attach(erc_MA_int)
#Set seed
set.seed(1234)


#Matching with FR_int
near_out_int <- matchit(socmob_treat ~ age+female+income+educ+unemployed+im_treat+perc_inq+
                          progay+gendrequal+pro_eu+left_right+socialtrust+poltrust, 
                        data = erc_MA_int, method="nearest")
summary(near_out_int)
a <- summary(near_out_int)

#he standardized difference in means of covariates
s.out <- summary(near_out_int, standardize = TRUE)
#plot(s.out)

#create the matched data from the MatchIt output object by excluding unmatched units from the original data
data_n_int <- match.data(near_out_int)
#Run Regressions with Balanced Datasets: without country effects
matchit_near_reg_int1 <- glm(FR_int ~ socmob_treat, data = data_n_int)
summary(matchit_near_reg_int1)
#Run Regressions with Balanced Datasets: with country effects
matchit_near_reg_int2 <- glm(FR_int ~ socmob_treat+country, data = data_n_int)
summary(matchit_near_reg_int2)
#Run Regressions with Balanced Datasets: full model country effects
matchit_near_reg_int3 <- glm(FR_int ~ socmob_treat+age+female+income+educ+unemployed+im_treat+perc_inq+
                               progay+gendrequal+pro_eu+left_right+socialtrust+poltrust+country, data = data_n_int)
summary(matchit_near_reg_int3)

#Create table for Sample Size
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 1: Sample sizes')
#Summary of balance for matched data
kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table X: Summary of balance for matched data')



# perform paired t-tests on matched data for Dependent variable: Far_int
t.test(data_n_int$FR_int[data_n_int$socmob_treat==1],data_n_int$FR_int[data_n_int$socmob_treat==0],paired=TRUE)

#Estimating treatment effects
with(data_n_int, t.test(FR_int ~ socmob_treat))


detach(erc_MA_int)


#=======================================================================================================================================#
#=======================================================================================================================================#

#===================================#
#                                   #
#          Genetic Matching         #
#                                   #
#===================================#


#=======================#
#     MatchIt package   #
#=======================#


attach(erc_MA_vote)
#Matching with FR_vote#
#=====================#
genetic_out_vote <- matchit(socmob_treat ~ age+female+income+educ+unemployed+im_treat+perc_inq+ #ca. 60min 
                           progay+gendrequal+pro_eu+left_right+socialtrust+poltrust,
                           data = erc_MA_vote, method="genetic")
summary(genetic_out_vote)
d <- summary(genetic_out_vote)

#create the matched data from the MatchIt output object by excluding unmatched units from the original data
data_g_vote <- match.data(genetic_out_vote)

#Run Regressions with Balanced Datasets: without country effects
matchit_genetic_reg_vote1 <- glm(FR_vote ~ socmob_treat, data = data_g_vote)
summary(matchit_genetic_reg_vote1)

#Run Regressions with Balanced Datasets: with country effects
matchit_genetic_reg_vote2 <- glm(FR_vote ~ socmob_treat+country, data = data_g_vote)
summary(matchit_genetic_reg_vote2)

#Create table for Sample Size
kable(d$nn, digits = 2, align = 'c', 
      caption = 'Table 1: Sample sizes')
#Summary of balance for matched data
kable(d$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table X: Summary of balance for matched data')


#perform paired t-tests on matched data for Dependent variable: Far_int
t.test(data_g_vote$FR_vote[data_g_vote$socmob_treat==1],
       data_g_vote$FR_vote[data_g_vote$socmob_treat==0],paired=F)
#Estimating treatment effects
with(data_g_vote, t.test(FR_vote ~ socmob_treat))

detach(erc_MA_vote)



attach(erc_MA_int)
#Set seed
set.seed(1234)


#Matching with FR_int#
#====================#
genetic_out_int <- matchit(socmob_treat ~ age+female+income+educ+unemployed+im_treat+perc_inq+ #ca. 30min 
                                          progay+gendrequal+pro_eu+left_right+socialtrust+poltrust, 
                                          data = erc_MA_int, method="genetic")
summary(genetic_out_int)
c <- summary(genetic_out_int)
#create the matched data from the MatchIt output object by excluding unmatched units from the original data
data_g_int <- match.data(genetic_out_int)

#Run Regressions with Balanced Datasets: without country effects
matchit_gen_reg_int1 <- glm(FR_int ~ socmob_treat, data = data_g_int)
summary(matchit_gen_reg_int1)

#Run Regressions with Balanced Datasets: with country effects
matchit_gen_reg_int2 <- glm(FR_int ~ socmob_treat+country, data = data_g_int)
summary(matchit_gen_reg_int2)

#Create table for Sample Size
kable(c$nn, digits = 2, align = 'c', 
      caption = 'Table 1: Sample sizes')
#Summary of balance for matched data
kable(c$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table X: Summary of balance for matched data')



#Create plots of balance
plot(genetic_out_int, type = 'jitter', interactive = FALSE)

# perform paired t-tests on matched data for Dependent variable: Far_int
t.test(data_g_int$FR_int[data_g_int$socmob_treat==1],data_g_int$FR_int[data_g_int$socmob_treat==0],paired=F)

#Estimating treatment effects
with(data_g_int, t.test(FR_int ~ socmob_treat))

detach(erc_MA_int)