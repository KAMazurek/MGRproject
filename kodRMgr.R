print(polaczone3)
write.csv(polaczone3,"C:\\Users\\Asus\\Desktop\\MyData.csv", row.names = FALSE)
write.csv(polaczone3, file = "dane.csv")

polaczone3$age_range <- ifelse(polaczone3$age %in% c(20, 21, 22, 23, 24), "20-24", ifelse(polaczone3$age %in% c(25, 26, 27, 28, 29, 30), "25-30", ifelse(polaczone3$age %in% c(31, 32, 33, 34, 35), "31-35", ifelse(polaczone3$age %in% c(36, 37, 38, 39, 40), "36-40","error"))))
polaczone3$education_level <- ifelse(polaczone3$education == 1, "high school degree or equivalent", ifelse(polaczone3$education ==  2, "bachelor’s degree", ifelse(polaczone3$education == 3, "master’s degree", ifelse(polaczone3$education == 4, "doctorate","error"))))


library(caTools)
library(dplyr)
library(ggplot2)
library(psych)
library(gtsummary)
library(corrplot)
library(survival)
library(devtools)
library(caret)
library(mclust)
library(cowplot)
library(randomForest)
library(vegan)
library(cluster)
library(gridExtra)
library(ROCR)

dim(polaczone3)

row.names(polaczone3) <- NULL

colnames(polaczone3)<- c("FK","CHOICE_I", "CHANGE_I", "CHANGE_I_GAFA", "PRICE_I", "CONVENIENCE_I","TRUST_I", "CHOICE_P", "PRICE_P", "CONVENIENCE_P", "TRUST_P", "CHOICE_L", "CHANGE_L", "CHANGE_L_GAFA", "PRICE_L", "CONVENIENCE_L", "EASINESS_L", "TRUST_L",  "ROR",
                         "SAT_PRICE", "SAT_CONVENIENCE", "SAT_SECURITY", "test1", "test2", "test3", "test4", "country", "year_of_birth", "female", "education", "student", "RT_sum", "RT_scale", "university_degree", "age", "liczba_F", "count_F", "at_least_1_F", "count_F_not_P", "at_least_1_F_not_P", "country_name")

colnames(danePL3)<- c("FK","CHOICE_I", "CHANGE_I", "CHANGE_I_GAFA", "PRICE_I", "CONVENIENCE_I","TRUST_I", "CHOICE_P", "PRICE_P", "CONVENIENCE_P", "TRUST_P", "CHOICE_L", "CHANGE_L", "CHANGE_L_GAFA", "PRICE_L", "CONVENIENCE_L", "EASINESS_L", "TRUST_L",  "ROR",
                      "SAT_PRICE", "SAT_CONVENIENCE", "SAT_SECURITY", "test1", "test2", "test3", "test4", "country", "year_of_birth", "female", "education", "student", "RT_sum", "RT_scale", "university_degree", "age", "liczba_F", "count_F", "at_least_1_F", "count_F_not_P", "at_least_1_F_not_P")

colnames(daneUS2)<- c("FK","CHOICE_I", "CHANGE_I", "CHANGE_I_GAFA", "PRICE_I", "CONVENIENCE_I","TRUST_I", "CHOICE_P", "PRICE_P", "CONVENIENCE_P", "TRUST_P", "CHOICE_L", "CHANGE_L", "CHANGE_L_GAFA", "PRICE_L", "CONVENIENCE_L", "EASINESS_L", "TRUST_L",  "ROR",
                      "SAT_PRICE", "SAT_CONVENIENCE", "SAT_SECURITY", "test1", "test2", "test3", "test4", "country", "year_of_birth", "female", "education", "student", "RT_sum", "RT_scale", "university_degree", "age", "liczba_F", "count_F", "at_least_1_F", "count_F_not_P")


## 1 Matryce korelacji:

Matryca_korelacji_PL <- cor(danePL3[, c(1,2,8,12,29,32, 33, 34,35,36, 37, 38,39, 40)])
corrplot(Matryca_korelacji_PL, method="circle", type = "upper")

Matryca_korelacji_US <- cor(daneUS2[, c(1,2,8,12,29,32, 33, 34,35,36, 37, 38,39)])
corrplot(Matryca_korelacji_US, method="circle", type = "upper")

Matryca_korelacji_pol3 <- cor(polaczone3[, c(1,2,8,12,27,28,29,32, 33, 34,35,36, 37, 38,39,40)])
corrplot(Matryca_korelacji_pol3, method="circle", type = "upper")
corrplot(Matryca_korelacji_pol3, method="number", type = "upper")
corrplot.mixed(Matryca_korelacji_pol3, lower.col = "black", number.cex = .7, tl.cex=.4)


MK_RT_FK_F <- cor(polaczone3[, c(1, 2,12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_kob, method="number", type="lower",  number.cex = .7, tl.cex = .4)


################ TEZA 1 i 2

############ POLKI
MK_RT_FK_F_KPL <- cor(KPL[, c(1, 2,12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_KPL, method="number", type="lower",  number.cex = .7, tl.cex = .4)

MK_RT_FK_F_męż <- cor(M_polaczone[, c(1, 2,12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_męż, method="number", type="lower",  number.cex = .7, tl.cex = .4)


############ POLKI
MK_RT_FK_F_KPL <- cor(KPL[, c(1, 2, 8, 12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_KPL, method="number", type="lower",  number.cex = .7, tl.cex = .4)


############ AMERYKANKI
MK_RT_FK_F_KUS <- cor(KUS[, c(1, 2, 8, 12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_KUS, method="number", type="lower",  number.cex = .7, tl.cex = .4)


############ POLACY
MK_RT_FK_F_MPL <- cor(MPL[, c(1, 2, 8, 12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_MPL, method="number", type="lower",  number.cex = .7, tl.cex = .4)

############ AMERYKANIE
MK_RT_FK_F_MUS <- cor(MUS[, c(1, 2, 8, 12, 33, 37,38,39, 40)])
corrplot(MK_RT_FK_F_MUS, method="number", type="lower",  number.cex = .7, tl.cex = .4)



## 2 Alpha Cronbacha 1 i 3 spełnia warunek:
test13US <- select(daneUS2,test3, test1)
alpha(test13US)

test13PL <- select(danePL3,test3, test1)
alpha(test13PL)


## 3 rozkład tolerancji ryzyka i wiedzy finansowej
ggplot(data=daneUS2, aes(x=RT_scale))+geom_histogram(bins = 10)
ggplot(data=danePL3, aes(x=RT_scale))+geom_histogram(bins = 10)


ggplot(data=daneUS2, aes(x=FK))+geom_histogram(bins = 10)
ggplot(data=danePL3, aes(x=FK))+geom_histogram(bins = 10)


## RT a FK z zaznaczonymi kolorem wyborami, można z cieniowaniem z webinaru
inv=ggplot(polaczone3, aes(x=FK, y=RT_scale, color = CHOICE_I))+geom_point()+geom_jitter()
inv

lo=ggplot(polaczone3, aes(x=FK, y=RT_scale, color = CHOICE_L))+geom_point()+geom_jitter()
lo

pay=ggplot(polaczone3, aes(x=FK, y=RT_scale, color = CHOICE_P))+geom_point()+geom_jitter()
pay

at_least_1F=ggplot(polaczone3, aes(x=FK, y=RT_scale, color = at_least_1_F))+geom_point()+geom_jitter()
at_least_1F

at_least_1F_NP=ggplot(polaczone3, aes(x=FK, y=RT_scale, color = at_least_1_F_not_P))+geom_point()+geom_jitter()
at_least_1F_NP



## SUMMARY STATISTICS _______________________________________________

podsumowanie1 <-
  polaczone3 %>%                                                                                      
  select (country_name, age_range, female, university_degree, student) %>% 
  tbl_summary(
    by = country_name, label = list(age_range ~ "Age", female ~ "Female", university_degree ~ "University degree", student ~ "Student"),
    statistic = list(all_continuous() ~ "{mean} ({sd})",all_categorical() ~ "{p}%")) %>%
  add_p(test = all_continuous() ~ "t.test", pvalue_fun = function(x) style_pvalue(x, digits = 2))  %>%                                                                                       
  modify_header(label = "**Summary statistics 1**") %>%
  bold_labels() %>%   
  bold_p()

podsumowanie1



## podsumowanie z podziałem na płeć (odrębna tabela dla kobiet, odrębna dla mężczyzn)
KPL <- subset(polaczone3, polaczone3$female=="1"&polaczone3$country=="1", as.rm=TRUE)
KUS <- subset(polaczone3, polaczone3$female=="1"&polaczone3$country=="0", as.rm=TRUE)

MPL <- subset(polaczone3, polaczone3$female=="0"&polaczone3$country=="1", as.rm=TRUE)
MUS <- subset(polaczone3, polaczone3$female=="0"&polaczone3$country=="0", as.rm=TRUE)

K_polaczone <- rbind(KPL, KUS)
M_polaczone <- rbind(MPL, MUS)


podsM <-
  M_polaczone %>%                                                                                                                                                                                                                                                                                 
  select (country_name, age, university_degree, student, RT_scale, FK) %>% 
  tbl_summary(by = country_name, label = list(age ~ "Age", university_degree ~ "University degree", student ~ "Student", RT_scale ~ "Risk tolerance", FK ~ "Financial knowledge"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",all_categorical() ~ "{p}%")) %>%
  add_p(test = all_continuous() ~ "t.test", pvalue_fun = function(x) style_pvalue(x, digits = 2))  %>%                                                                                       
  modify_header(label = "**Summary statistics 2**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM




podsK <-
  K_polaczone %>%                                                                                                                                                                                                                                                                                 
  select (country_name, age, university_degree, student, RT_scale, FK) %>% 
  tbl_summary(by = country_name, label = list(age ~ "Age", university_degree ~ "University degree",
                                              student ~ "Student", RT_scale ~ "Risk tolerance", FK ~ "Financial knowledge"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",all_categorical() ~ "{p}%")) %>%
  add_p(test = all_continuous() ~ "t.test", pvalue_fun = function(x) style_pvalue(x, digits = 2))  %>%                                                                                       
  modify_header(label = "**Summary statistics 2**") %>%
  bold_labels() %>%   
  bold_p()  %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK


## połączenie podsumowań dla K i M

K_M <-
  tbl_merge(
    tbls = list(podsK, podsM),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M

## między PL a US wśród kobiet nie ma isotnej statystycznie różnicy w RT

######### między PL a US wśród mężczyzn jest różnica w RT, która nieznacznie 
## przekroczyła próg istotności statystycznej

cor(M_polaczone$country, M_polaczone$RT_scale, method = "spearman")
## PL to 1, więc męźczyźni w PL mają większą RT, bo kor:
## 0.02492234

######### między PL a US wśród kobiet jest istotna różnica w FK i jest ona wyższa w PL
cor(K_polaczone$country, K_polaczone$FK, method = "spearman")
##0.06480225

######### między PL a US wśród mężczyzn jest istotna różnica w FK
cor(M_polaczone$country, M_polaczone$FK, method = "spearman")
## 0.214638

######################################### podsumowanie wybory
podsum3 <- 
  polaczone3 %>%                                                                                    
  select (country_name, CHOICE_P, CHOICE_I, CHOICE_L, at_least_1_F, 
          count_F, at_least_1_F_not_P, count_F_not_P) %>% 
  tbl_summary( 
    by = country_name, label = list( CHOICE_P ~ "Choice in favor of Fintech (payment)", 
                                     CHOICE_I ~ "Choice in favor of Fintech (investition)", 
                                     CHOICE_L ~ "Choice in favor of Fintech (loan)",
                                     at_least_1_F ~ "At least one decision in favor of Fintech",
                                     count_F ~ "Number of choices in favor of Fintech", 
                                     at_least_1_F_not_P ~ "At least one decision in favor of Fintech (payment excluded)",
                                     count_F_not_P ~ "Number of choices in favor of Fintech (payment excluded)"),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 3**") %>%
  bold_labels() %>%   
  bold_p()


podsum3


podsK3 <-
  K_polaczone %>%                                                                                    
  select (country_name, CHOICE_P, CHOICE_I, CHOICE_L, at_least_1_F, 
          count_F, at_least_1_F_not_P, count_F_not_P) %>% 
  tbl_summary( 
    by = country_name, label = list( CHOICE_P ~ "Choice in favor of Fintech (payment)", 
                                     CHOICE_I ~ "Choice in favor of Fintech (investition)", 
                                     CHOICE_L ~ "Choice in favor of Fintech (loan)",
                                     at_least_1_F ~ "At least one decision in favor of Fintech",
                                     count_F ~ "Number of choices in favor of Fintech", 
                                     at_least_1_F_not_P ~ "At least one decision in favor of Fintech (payment excluded)",
                                     count_F_not_P ~ "Number of choices in favor of Fintech (payment excluded)"),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 3**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK3


podsM3 <-
  M_polaczone %>%                                                                                    
  select (country_name, CHOICE_P, CHOICE_I, CHOICE_L, at_least_1_F, 
          count_F, at_least_1_F_not_P, count_F_not_P) %>% 
  tbl_summary( 
    by = country_name, label = list( CHOICE_P ~ "Choice in favor of Fintech (payment)", 
                                     CHOICE_I ~ "Choice in favor of Fintech (investition)", 
                                     CHOICE_L ~ "Choice in favor of Fintech (loan)",
                                     at_least_1_F ~ "At least one decision in favor of Fintech",
                                     count_F ~ "Number of choices in favor of Fintech", 
                                     at_least_1_F_not_P ~ "At least one decision in favor of Fintech (payment excluded)",
                                     count_F_not_P ~ "Number of choices in favor of Fintech (payment excluded)"),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 3**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM3

K_M3 <-
  tbl_merge(
    tbls = list(podsK3, podsM3),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M3


######################################### podsumowanie priorytety

podsum4 <- 
  polaczone3 %>%                                                                                    
  select (country_name, PRICE_P, CONVENIENCE_P, TRUST_P, PRICE_I, CONVENIENCE_I, TRUST_I,
          PRICE_L, CONVENIENCE_L, TRUST_L, EASINESS_L ) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_P ~ "Importance of price (payment)",
                                     CONVENIENCE_P ~ "Importance of convenience (payment)",
                                     TRUST_P ~ "Importance of trust (payment)",
                                     PRICE_I ~ "Importance of price (investition)", 
                                     CONVENIENCE_I ~ "Importance of convenience (investition)",
                                     TRUST_I ~ "Importance of trust (investition)",
                                     PRICE_L ~ "Importance of price (loan)",
                                     CONVENIENCE_L ~ "Importance of convenience (loan)",
                                     TRUST_L ~ "Importance of trust (loan)",
                                     EASINESS_L ~ "Importance of easimess (loan)"
    ),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4**") %>%
  bold_labels() %>%   
  bold_p()


podsum4



podsK4 <-
  K_polaczone %>%                                                                                    
  select (country_name, PRICE_P, CONVENIENCE_P, TRUST_P, PRICE_I, CONVENIENCE_I, TRUST_I,
          PRICE_L, CONVENIENCE_L, TRUST_L, EASINESS_L) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_P ~ "Importance of price (payment)",
                                     CONVENIENCE_P ~ "Importance of convenience (payment)",
                                     TRUST_P ~ "Importance of trust (payment)",
                                     PRICE_I ~ "Importance of price (investition)", 
                                     CONVENIENCE_I ~ "Importance of convenience (investition)",
                                     TRUST_I ~ "Importance of trust (investition)",
                                     PRICE_L ~ "Importance of price (loan)",
                                     CONVENIENCE_L ~ "Importance of convenience (loan)",
                                     TRUST_L ~ "Importance of trust (loan)",
                                     EASINESS_L ~ "Importance of easimess (loan)")
    ,
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK4


podsM4 <-
  M_polaczone %>%                                                                                    
  select (country_name, PRICE_P, CONVENIENCE_P, TRUST_P, PRICE_I, CONVENIENCE_I, TRUST_I,
          PRICE_L, CONVENIENCE_L, TRUST_L, EASINESS_L) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_P ~ "Importance of price (payment)",
                                     CONVENIENCE_P ~ "Importance of convenience (payment)",
                                     TRUST_P ~ "Importance of trust (payment)",
                                     PRICE_I ~ "Importance of price (investition)", 
                                     CONVENIENCE_I ~ "Importance of convenience (investition)",
                                     TRUST_I ~ "Importance of trust (investition)",
                                     PRICE_L ~ "Importance of price (loan)",
                                     CONVENIENCE_L ~ "Importance of convenience (loan)",
                                     TRUST_L ~ "Importance of trust (loan)",
                                     EASINESS_L ~ "Importance of easimess (loan)"
    ),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 Importance of:**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM4

### podzielić na 3 (rodzajem usług)

K_M4 <-
  tbl_merge(
    tbls = list(podsK4, podsM4),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M4

##################################### podział
################################# payment 


podsK4P <-
  K_polaczone %>%                                                                                    
  select (country_name, PRICE_P, CONVENIENCE_P, TRUST_P) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_P ~ "Importance of price (payment)",
                                     CONVENIENCE_P ~ "Importance of convenience (payment)",
                                     TRUST_P ~ "Importance of trust (payment)"
                                     )
    ,
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (payment)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK4P


podsM4P <-
  M_polaczone %>%                                                                                    
  select (country_name, PRICE_P, CONVENIENCE_P, TRUST_P) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_P ~ "Importance of price (payment)",
                                     CONVENIENCE_P ~ "Importance of convenience (payment)",
                                     TRUST_P ~ "Importance of trust (payment)"
    ),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (payment)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM4P

### podzielić na 3 (rodzajem usług)

K_M4P <-
  tbl_merge(
    tbls = list(podsK4P, podsM4P),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M4P



pdf("tab4.1.pdf")       # Export PDF
grid.table(K_M4P)
dev.off()


################# inwestycje
podsK4I <-
  K_polaczone %>%                                                                                    
  select (country_name, PRICE_I, CONVENIENCE_I, TRUST_I) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_I ~ "Importance of price (investition)", 
                                     CONVENIENCE_I ~ "Importance of convenience (investition)",
                                     TRUST_I ~ "Importance of trust (investition)") 
    ,
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (investition)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK4I


podsM4I <-
  M_polaczone %>%                                                                                    
  select (country_name, PRICE_I, CONVENIENCE_I, TRUST_I) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_I ~ "Importance of price (investition)", 
                                     CONVENIENCE_I ~ "Importance of convenience (investition)",
                                     TRUST_I ~ "Importance of trust (investition)"
    ),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (investition)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM4I



K_M4I <-
  tbl_merge(
    tbls = list(podsK4I, podsM4I),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M4I



############################ pożyczki

podsK4L <-
  K_polaczone %>%                                                                                    
  select (country_name, PRICE_L, CONVENIENCE_L, TRUST_L, EASINESS_L) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_L ~ "Importance of price (loan)",
                                     CONVENIENCE_L ~ "Importance of convenience (loan)",
                                     TRUST_L ~ "Importance of trust (loan)",
                                     EASINESS_L ~ "Importance of easimess (loan)")
    ,
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (loan)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK4L


podsM4L <-
  M_polaczone %>%                                                                                    
  select (country_name, PRICE_L, CONVENIENCE_L, TRUST_L, EASINESS_L) %>% 
  tbl_summary( 
    by = country_name, label = list( PRICE_L ~ "Importance of price (loan)",
                                     CONVENIENCE_L ~ "Importance of convenience (loan)",
                                     TRUST_L ~ "Importance of trust (loan)",
                                     EASINESS_L ~ "Importance of easimess (loan)"
    ),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 4 (loan)**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM4L

### podzielić na 3 (rodzajem usług)

K_M4L <-
  tbl_merge(
    tbls = list(podsK4L, podsM4L),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M4L



############################################### podsumowanie zadowolenie

podsK5 <-
  K_polaczone %>%                                                                                    
  select (country_name, ROR, SAT_PRICE, SAT_CONVENIENCE, SAT_SECURITY) %>% 
  tbl_summary( 
    by = country_name, label = list( ROR ~ "Ownership of at least one bank account",
                                     SAT_PRICE ~ "Satisfaction with prices",
                                     SAT_CONVENIENCE ~ "Satisfaction with convenience",
                                     SAT_SECURITY ~ "Satisfaction with security"),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 5**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Female**")

podsK5


podsM5 <-
  M_polaczone %>%                                                                                    
  select (country_name, ROR, SAT_PRICE, SAT_CONVENIENCE, SAT_SECURITY) %>% 
  tbl_summary( 
    by = country_name, label = list( ROR ~ "Ownership of at least one bank account",
                                     SAT_PRICE ~ "Satisfaction with prices",
                                     SAT_CONVENIENCE ~ "Satisfaction with convenience",
                                     SAT_SECURITY ~ "Satisfaction with security"),
    statistic = list(all_dichotomous() ~ "{p}%", 
                     all_categorical() ~ "{p}%")) %>%
  add_p() %>%
  modify_header(label = "**Summary statistics 5**") %>%
  bold_labels() %>%   
  bold_p() %>%
  modify_spanning_header(starts_with("stat_") ~ "**Male**")

podsM5


K_M5 <-
  tbl_merge(
    tbls = list(podsK5, podsM5),
    tab_spanner = c("**Female**", "**Male**")
  )

K_M5



######################## HIPOTEZA 3 
## https://stats.idre.ucla.edu/stata/dae/interval-regression/#




######################## CEL GŁÓWNY 

pol3 = subset(polaczone3 [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])
set.seed(88)
split1 = sample.split(pol3$at_least_1_F_not_P, SplitRatio=0.75) 
pol3Train = subset(pol3, split == TRUE)
pol3Test = subset(pol3, split == FALSE)
pol3TrainScaled = scale(pol3Train)
nrow(pol3TrainScaled)

k = list()
for (i in 1:5) {
  k[[i]] = kmeans(na.omit(pol3TrainScaled), i)
}
k



between_totss <- list()
for (i in 1:10) {
  between_totss[[i]] <- k[[i]]$between/k[[i]]$totss
}

plot(1:10, between_totss, type="b", ylab = "Between SS/Total SS", xlab = "Clusters (k)")

PLTrainCLUST <- kmeans(na.omit(pol3TrainScaled), 3)
PLTrainCLUST

##PL = subset(PL [, c(1,2,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])
##opcja bez preferencji:
##PL = subset(PL [, c(1,2,8,12,19,20,21,22,29,30,31,33,35,40)])
##opcja z odjętymi również ocenami:
##PL = subset(PL [, c(1,2,8,12,29,30,31,33,35,40)])
##jeszcze bez studenta
##PL = subset(PL [, c(1,2,8,12,29,30,33,35,40)])
##bez wyborów, ale ze wszystkim innym

PL <- subset(polaczone3, country==1) 

PL = subset(PL [, c(1,5,6,7,9,10,11,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])


set.seed(88)
split1 = sample.split(PL$at_least_1_F_not_P, SplitRatio=0.75) 
PLTrain = subset(PL, split == TRUE)
PLTest = subset(PL, split == FALSE)


## wyskalować tylko te zmienne, które nie są kategorycznymi!!!!!!
PLTrainScaled2 = scale(PLTrain)
##PLAgeScaled = scale(PLTrain$age)

##PLTrain2 = subset(PLTrain[, -23])
##PLTestScaled = scale(PLTest)


##PLTrain3 = cbind(PLAgeScaled, PLTrain2)



####### tworzę klastry na train KMEANS
PLTrainCLUST <- kmeans(na.omit(PLTrainScaled2), 3)
PLTrainCLUST



##################################################################3 tworzę klastry na train
PLTrainCLUST2 <- kmeans(na.omit(PLTrainScaled), 10)
PLTrainCLUST2

for (i in 1:10) {
  plot(PLTrain, col = k[[i]]$cluster)
}


str(PLTrainCLUST)

plot(PLTrain, col=PLTrainCLUST$cluster)

plot(PLTrain)

k = list()
for (i in 1:10) {
  k[[i]] = kmeans(na.omit(PLTrainScaled2), i)
}
k

between_totss <- list()
for (i in 1:10) {
  between_totss[[i]] <- k[[i]]$between/k[[i]]$totss
}

plot(1:10, between_totss, type="b", ylab = "Between SS/Total SS", xlab = "Clusters (k)")

PLTrainCLUST <- kmeans(na.omit(PLTrainScaled2), 3)
PLTrainCLUST

############################# tworzę klastry na train HIERARCHICAL ##############

pol3 = subset(polaczone3 [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])
pol3Scaled = scale(pol3)


d = dist(na.omit(pol3Scaled))
clust = hclust(d, "ward.D2")

plot(clust)

rect.hclust(clust, k=4, border = "red")

clusters = cutree(cluster, 4)
clusters







PL <- subset(polaczone3, country==1) 

PL = subset(PL [, c(1,5,6,7,9,10,11,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])

set.seed(88)
split1 = sample.split(PL$at_least_1_F_not_P, SplitRatio=0.75) 
PLTrain = subset(PL, split == TRUE)
PLTest = subset(PL, split == FALSE)


PLTrainScaled2 = scale(PLTrain)

d = dist(na.omit(PLTrainScaled2))
clust = hclust(d, "ward.D2")

plot(clust)

rect.hclust(clust, k=3, border = "red")

clusters = cutree(cluster, 3)
clusters

PLTrain$clusters <-  cutree(cluster, 3)

pdf('plot1.pdf',width = 8.267, height = 11.692)
plot1 = plot(PLTrain, col = clusters)
dev.off()

PLTrainK1 <- subset(PLTrain, cluster==1)

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  PLTrainCLUST <- kmeans(na.omit(PLTrainScaled2), centers = k, nstart = 25)
  ss <- silhouette(PLTrainCLUST$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

####### tworzę klastry na train MCLUST
Mclust1 = Mclust(na.omit(PLTrainScaled2))
Mclust1
plot(Mclust1)

###################################################### REGRESJA LOGISTYCZNA


############################# wszystkie dane

pol_nowe = subset(polaczone3 [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])

library(caTools)
set.seed(88)
split2 = sample.split(pol_nowe$at_least_1_F_not_P, SplitRatio=0.75) 
Train = subset(pol_nowe, split == TRUE)
Test = subset(pol_nowe, split == FALSE)

LR1 <- glm(at_least_1_F_not_P ~., data=na.omit(Train), family=binomial)

LR1
summary(LR1)

predictTest = predict(LR1, type="response", newdata = Test)

table(Test$at_least_1_F_not_P, predictTest>0.5)

##sensitivity:
8/(8+65)
##specificity:
3/(3+16)

##overall error rate (bardzo duży błąd)
(16+65)/(65+16+3+8)

##accuracy
(3+8)/(65+16+3+8)

##precision
8/(8+16)

##false positive rate
16/(65+3)

########################## PL

PL <- subset(polaczone3, country==1) 

PL_nowe = subset(PL [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])

library(caTools)
set.seed(88)
split2 = sample.split(PL_nowe$at_least_1_F_not_P, SplitRatio=0.75) 
Train2 = subset(PL_nowe, split == TRUE)
Test2 = subset(PL_nowe, split == FALSE)

LR2 <- glm(at_least_1_F_not_P ~., data=na.omit(Train2), family=binomial)

LR2
summary(LR2)

predictTest2 = predict(LR2, type="response", newdata = Test2)

table(Test2$at_least_1_F_not_P, predictTest>0.5)

##sensitivity:
4/(4+56)
##specificity:
3/(3+9)

##overall error
(56+9)/(56+9+3+4)

########################## US

US <- subset(polaczone3, country==0) 

US_nowe = subset(US [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)])

library(caTools)
set.seed(88)
split3 = sample.split(US_nowe$at_least_1_F_not_P, SplitRatio=0.75) 
Train3 = subset(US_nowe, split == TRUE)
Test3 = subset(US_nowe, split == FALSE)

LR3 <- glm(at_least_1_F_not_P ~., data=na.omit(Train3), family=binomial)

LR3
summary(LR3)

predictTest3 = predict(LR3, type="response", newdata = Test3)

table(Test3$at_least_1_F_not_P, predictTest>0.5)

##sensitivity:
2/(2+6)
##specificity:
1/(1+8)
## overall error
(6+8)/(6+8+1+2)


##################################### RANDOM FOREST

############################# wszystkie dane

library(caTools)
library(randomForest)


pol_nowe2 = subset(polaczone3 [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)], rm.na=true)

pol_nowe2 <- na.omit(pol_nowe2) 

avector <- as.vector(pol_nowe2['at_least_1_F_not_P'])
class(avector) 


avector <- pol_nowe2[,18]
class(avector)

pol_nowe2$at_least_1_F_not_P = as.factor(pol_nowe2$at_least_1_F_not_P)

set.seed(88)
split2 = sample.split(pol_nowe2$at_least_1_F_not_P, SplitRatio=0.75) 
TrainRF = subset(na.omit(pol_nowe2), split == TRUE)
TestRF = subset(na.omit(pol_nowe2), split == FALSE)

TrainRF  <- na.omit(TrainRF) 
TestRF <- na.omit(TestRF) 

set.seed(88)
pol_rf <- randomForest(at_least_1_F_not_P ~., data = na.omit(TrainRF), ntree=96) 
print(pol_rf)

# Out-of-bag estimate, is a method of measuring the prediction error of random forests utilising bootstrap aggregating (bagging) to sub-sample data samples used for training. 
# OOB is the mean prediction error on each training sample xᵢ, using only the trees that did not have xᵢ in their bootstrap sample.

floor(sqrt(ncol(TrainRF) - 1))

mtry <- tuneRF(TrainRF[-18],TrainRF$at_least_1_F_not_P, ntreeTry=96,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(88)
pol_rf <- randomForest(at_least_1_F_not_P ~., data = TrainRF, mtry=best.m, ntree=96) 
print(pol_rf)
importance(pol_rf)
varImpPlot(pol_rf)


################## RF na danych testowych 

dim( TrainRF )

pred1 = predict(pol_rf, type = "prob", newdata = TestRF)
library(ROCR)
pred1

perf = prediction(pred1[,2], TestRF$at_least_1_F_not_P)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")




#####################  random forest dla USA 
## test dzielę na klastry i na każdym testuję  random forest

USrf <- subset(polaczone3, polaczone3$country=="0", as.rm=TRUE)

pol_noweUS = subset(USrf [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)], rm.na=true)

pol_noweUS <- na.omit(pol_noweUS) 

avectorUS <- as.vector(pol_noweUS['at_least_1_F_not_P'])
class(avectorUS) 


avectorUS <- pol_noweUS[,18]
class(avectorUS)

pol_noweUS$at_least_1_F_not_P = as.factor(pol_noweUS$at_least_1_F_not_P)

set.seed(88)
split2 = sample.split(pol_noweUS$at_least_1_F_not_P, SplitRatio=0.75) 
TrainRFUS = subset(na.omit(pol_noweUS), split == TRUE)
TestRFUS = subset(na.omit(pol_noweUS), split == FALSE)

TrainRFUS  <- na.omit(TrainRFUS) 
TestRFUS <- na.omit(TestRFUS) 

set.seed(88)
pol_rfUS <- randomForest(at_least_1_F_not_P ~., data = na.omit(TrainRFUS), ntree=96) 
print(pol_rfUS)

# Out-of-bag estimate, is a method of measuring the prediction error of random forests utilising bootstrap aggregating (bagging) to sub-sample data samples used for training. 
# OOB is the mean prediction error on each training sample xᵢ, using only the trees that did not have xᵢ in their bootstrap sample.

floor(sqrt(ncol(TrainRFUS) - 1))

mtry <- tuneRF(TrainRFUS[-18],TrainRFUS$at_least_1_F_not_P, ntreeTry=96,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(88)
pol_rf <- randomForest(at_least_1_F_not_P ~., data = TrainRFUS, mtry=best.m, ntree=96) 
print(pol_rfUS)
importance(pol_rfUS)
varImpPlot(pol_rfUS)


################## RF na danych testowych 

dim( TrainRFUS )

pred1 = predict(pol_rfUS, type = "prob", newdata = TestRFUS)
library(ROCR)
pred1

perfUS = prediction(pred1[,2], TestRFUS$at_least_1_F_not_P)
# 1. Area under curve
auc = performance(perfUS, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perfUS, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")




###################################  random forest dla PL 
## test dzielę na klastry i na każdym testuję  random forest

PLrf <- subset(polaczone3, polaczone3$country=="1", as.rm=TRUE)

pol_nowePL = subset(PLrf [, c(1,5,6,7,15,16,17,18,19,20,21,22,29,30,31,33,35,40)], rm.na=true)

pol_nowePL <- na.omit(pol_nowePL) 

avectorPL <- as.vector(pol_nowePL['at_least_1_F_not_P'])
class(avectorPL) 


avectorPL <- pol_nowePL[,18]
class(avectorPL)

pol_nowePL$at_least_1_F_not_P = as.factor(pol_nowePL$at_least_1_F_not_P)

set.seed(88)
split2 = sample.split(pol_nowePL$at_least_1_F_not_P, SplitRatio=0.75) 
TrainRFPL = subset(na.omit(pol_nowePL), split == TRUE)
TestRFPL = subset(na.omit(pol_nowePL), split == FALSE)

TrainRFPL  <- na.omit(TrainRFPL) 
TestRFPL <- na.omit(TestRFPL) 

set.seed(88)
pol_rfPL <- randomForest(at_least_1_F_not_P ~., data = na.omit(TrainRFPL), ntree=96) 
print(pol_rfPL)

# Out-of-bag estimate, is a method of measuring the prediction error of random forests utilising bootstrap aggregating (bagging) to sub-sample data samples used for training. 
# OOB is the mean prediction error on each training sample xᵢ, using only the trees that did not have xᵢ in their bootstrap sample.

floor(sqrt(ncol(TrainRFPL) - 1))

mtry <- tuneRF(TrainRFPL[-18],TrainRFPL$at_least_1_F_not_P, ntreeTry=96,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(88)
pol_rfPL <- randomForest(at_least_1_F_not_P ~., data = TrainRFPL, mtry=best.m, ntree=96) 
print(pol_rfPL)
importance(pol_rfPL)
varImpPlot(pol_rfPL)


################## RF na danych testowych 

dim( TrainRFPL )

pred1 = predict(pol_rfPL, type = "prob", newdata = TestRFPL)
library(ROCR)
pred1

perfPL = prediction(pred1[,2], TestRFPL$at_least_1_F_not_P)
# 1. Area under curve
auc = performance(perfPL, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perfPL, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

















US <- subset(polaczone3, country==0)
## US dzielę na zestaw train (.75) i test (.25) i normalizuję zmienne
set.seed(88)
split2 = sample.split(US$at_least_1_F_not_P, SplitRatio=0.75) 
USTrain = subset(US, split == TRUE)
USTest = subset(US, split == FALSE)

## tworzę klastry
## dla każdego robię random forest dla co najmniej 1 nie p
## train dzielę na klastry i na każdym testuję  random forest

####### tworzę klastry na train KMEANS
PLTrainCLUST <- kmeans(na.omit(PLTrainScaled2), 9)
PLTrainCLUST


## NORMALIZACJA
## najpierw usunięcie z data framów kolumn z danymi innymi niż numeryczne



PLscale <- scale(PL)


## polaczone 3
distances1 = dist(polaczone3, method = "euclidean")
cluster1 = hclust (distances1, method = "ward.D")
plot(cluster1)

## PL3
distancesPL = dist(polaczone3, method = "euclidean")
clusterPL = hclust (distancesPL, method = "ward.D")
plot(clusterPL)

## US2
distancesUS = dist(polaczone3, method = "euclidean")
clusterUS = hclust (distancesUS, method = "ward.D")
plot(clusterUS)




