#Inspire a Better Tomorrow
set.seed(11)

#Packages
library(tidyverse)    #Gravy
library(rvest)        #Web Scraping
library(fitdistrplus) #Distribution Fitting


# Data for analysis is included in the github repository (https://github.com/VinnyC-Analytics/Simulating_NDvsLOU)
# Obfuscated code for web scraping included as comments for guide to using rvest

# To execute this code, read-in data from github repo as "Scores.BrianKelly" and "Scores.ScottSatt" as appropriate.


#------------------------------------GAME SCORES FOR DISTRIBUTION FITTING-------------------------------------------------
# 
# GAME SCORES FOR DISTRIBUTION FITTING
# 
#

Capacity <- 40000

# Functions:

# Web Scrape Game Score. URL has been removed. Use function as reference only.
# GetGameScores <- function(INPUT_YEAR , INPUT_TEAM){
#   templist <- list()
#   
#   for(year in INPUT_YEAR){
#     
#     url <- paste0("SOME_WEBSITE_FOR_DATA",year,INPUT_TEAM,"/index.html")
#     
#     temp <- url %>%
#       read_html() %>%
#       html_nodes(xpath='   INSERT XPATH HERE      ') %>%
#       html_table() %>%
#       as.data.frame()
#     
#     templist[[year]] <- temp
#     
#   }
#   
#   OUTPUT.GetGameScores <- bind_rows(templist)
#   return(OUTPUT.GetGameScores)
#   
#   remove(templist)
#   gc()
#   
# }


CleanGameScores <- function(INPUT){
 
     bind_rows(INPUT) %>%
     dplyr::select(1:3,5) %>%
     filter(!grepl("@" , Result)) %>%
     mutate(GameMonth = 
              case_when(
                substr(Date,1,2) == "08" ~ "August/September",
                substr(Date,1,2) == "09" ~ "August/September",
                substr(Date,1,2) == "10" ~ "October",
                substr(Date,1,2) == "11" ~ "November",
                substr(Date,1,2) == "12" ~ "December/January",
                substr(Date,1,2) == "01" ~ "December/January",
                TRUE ~ as.character(Date)
              )
     ) %>%
     dplyr::select(GameMonth,
                   Result,
                   Attendance) %>%
     separate(Result,c("PtsFor","PtsAgainst"), sep = "-") %>%
     mutate(PtsFor = gsub("W ","",PtsFor),
            PtsFor = gsub("L ","",PtsFor)) %>%
     mutate(Attendance = gsub(",","",Attendance)) %>%
     mutate(Attendance = as.numeric(Attendance),
            PtsFor = as.numeric(PtsFor),
            PtsAgainst = as.numeric(PtsAgainst)) %>%
     
     mutate(PtsForAdj = ifelse(Attendance >= Capacity , PtsFor , PtsFor * 0.70),
            PtsAgainstAdj = ifelse(Attendance >= Capacity , PtsAgainst , PtsAgainst * 0.70)) %>%
     
     mutate(PtsForAdj = ifelse(PtsForAdj == 0 , 0.00001 , PtsForAdj),
            PtsAgainstAdj = ifelse(PtsAgainstAdj == 0 , 0.00001 , PtsAgainstAdj))
   
 }
 
 Years.BK <- as.character(c(2010:2018)) #Brian Kelly Coaching Years at ND
 Years.SS <- as.character(c(2014:2018)) #Scott Satterfield Total Coaching Years (limited experience). Data starts at 2014
 
 tempSCORES_BK <- GetGameScores(Years.BK , "Notre Dame")
 tempSCORES_SS <- GetGameScores(Years.SS , "Appalachian State")
 
 Scores.BrianKelly <- CleanGameScores(tempSCORES_BK)
 Scores.ScottSatt  <- CleanGameScores(tempSCORES_SS)
 
 
 remove(Capacity , Years.BK , Years.SS)  
 remove(tempSCORES_BK , tempSCORES_SS)
 gc()



#--------------------------------------------DISTRIBUTION FITTING: Brian Kelly--------------------------------------------------

#August / September
#
#
BK.AugSept <- Scores.BrianKelly %>%
  filter(GameMonth == "August/September") %>%
  dplyr::select(5:6)

#Step 1 - Eyeballs on the distribution
#Both have a positive skew
plotdist(BK.AugSept$PtsForAdj , histo = TRUE , demp = TRUE)      #normal-ish
plotdist(BK.AugSept$PtsAgainstAdj , histo = TRUE , demp = TRUE)  #positive skew

#Step2 - Distribution Fitting
#PtsFor
fitND.n <- fitdist(BK.AugSept$PtsForAdj , "norm")     #Why not try it
fitND.w <- fitdist(BK.AugSept$PtsForAdj , "weibull")
fitND.ln <- fitdist(BK.AugSept$PtsForAdj , "lnorm")
fitND.g <- fitdist(BK.AugSept$PtsForAdj , "gamma")

#PtsAgainst
fitND.n2 <- fitdist(BK.AugSept$PtsAgainstAdj , "norm")   linke
fitND.w2 <- fitdist(BK.AugSept$PtsAgainstAdj , "weibull")
fitND.ln2 <- fitdist(BK.AugSept$PtsAgainstAdj , "lnorm")
fitND.g2 <- fitdist(BK.AugSept$PtsAgainstAdj , "gamma")


TheLegend <- c("Weibull", "Lognormal","Gamma")

denscomp(list(fitND.w, fitND.ln, fitND.g), legendtext = TheLegend)
cdfcomp(list(fitND.w, fitND.ln,fitND.g), legendtext = TheLegend)

fitND.g$aic
fitND.g$estimate
summary(fitND.g)

rand.ND_PF <- rgamma(n = 1000000,
                     shape = fitND.g$estimate[[1]],
                     rate = fitND.g$estimate[[2]])

denscomp(list(fitND.n2, fitND.w2, fitND.g2))
cdfcomp(list(fitND.n2, fitND.w2, fitND.g2))

fitND.n2$aic
fitND.w2$aic
fitND.g2$aic

fitND.n2$estimate

rand.ND_PA <- rnorm(n = 1000000,
                    mean = fitND.n2$estimate[[1]],
                    sd = fitND.n2$estimate[[2]])


sim.ND_Scores <- cbind(rand.ND_PF,rand.ND_PA) %>% as.data.frame() 

remove(rand.ND_PF , rand.ND_PA)
remove(fitND.g , fitND.ln , fitND.n , fitND.w)
remove(fitND.g2 , fitND.ln2 , fitND.n2 , fitND.w2)
gc()


#-----------------------------------------DISTRIBUTION FITTING: Scott Satterfield--------------------------------------------------

#Step 1 - Eyeballs on the distribution

#Points For Adjusted
temp.SCOTTPF <-filter(Scores.ScottSatt , GameMonth == "August/September")[,5]
plotdist(temp.SCOTTPF , histo = TRUE , demp = TRUE) 


#Step2 - Distribution Fitting
#PtsFor - August/September
fit.n <- fitdist(filter(Scores.ScottSatt , GameMonth == "August/September")[,5] , "norm")
fit.w <- fitdist(filter(Scores.ScottSatt , GameMonth == "August/September")[,5] , "weibull")
fit.ln <- fitdist(filter(Scores.ScottSatt , GameMonth == "August/September")[,5] , "lnorm")
fit.g <- fitdist(filter(Scores.ScottSatt , GameMonth == "August/September")[,5] , "gamma")

TheLegend <- c("Weibull", "Lognormal","Gamma")

denscomp(list(fit.n,fit.w, fit.ln, fit.g))
cdfcomp(list(fit.n,fit.w, fit.ln, fit.g))

fit.n$aic
fit.w$aic
fit.ln$aic
fit.g$aic

fit.w$estimate

rand.L_PF <- rweibull(n = 1000000,
                      shape = fit.w$estimate[[1]],
                      scale = fit.w$estimate[[2]])

#Points Against Adjusted
temp.SCOTTPA <- filter(Scores.ScottSatt , GameMonth == "August/September")[,6]
hist(temp.SCOTTPA)

plotdist(temp.SCOTTPA , histo = TRUE , demp = TRUE) #Negative Skew

#PtsAgainst - August/September
fit.n <- fitdist(temp.SCOTTPA , "norm")
fit.w <- fitdist(temp.SCOTTPA , "weibull")
fit.ln <- fitdist(temp.SCOTTPA , "lnorm")
fit.g <- fitdist(temp.SCOTTPA , "gamma")

denscomp(list(fit.w, fit.ln, fit.g))
cdfcomp(list(fit.w, fit.ln, fit.g))

fit.n$aic
fit.w$aic
fit.ln$aic
fit.g$aic

#gamma has the lowest aic
summary(fit.g)

fit.g$estimate

rand.L_PA <- rgamma(n = 1000000,
                    shape = fit.g$estimate[[1]],
                    rate = fit.g$estimate[[2]])

hist(rand.L_PA)

sim.L_Scores <- cbind(rand.L_PF,rand.L_PA) %>% as.data.frame() 

remove(temp.SCOTTPA , temp.SCOTTPF)
remove(fit.g , fit.ln , fit.n , fit.w)
remove(rand.L_PA , rand.L_PF)
gc()


#------------------------------------SIMULATED GAME SCORES-------------------------------------------------
# 
# GAME SCORES FOR PREDICTION
# 
#


sim.tempbind <- cbind(sim.ND_Scores , sim.L_Scores) %>%
  apply(2 , function(x) ifelse(x < 0 , 0 , x)) %>%
  as.data.frame() %>%
  mutate(ND_Score = (rand.ND_PF * 0.8) + (rand.L_PA * 0.2),
         Lou_Score = (rand.L_PF * 0.8) + (rand.ND_PA * 0.2),
         ND_Victory = ifelse(ND_Score > Lou_Score , 1 , 0))


#WIN OUTCOME: ND vs Louisville
prop.table(table(sim.tempbind$ND_Victory))



