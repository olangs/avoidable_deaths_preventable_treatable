###############################################
#
# Net survival and Avoidable deaths 
# Date: 20/01/2026
# Version 4.0 - 
#
#Load files and packages in AD_2.R file
#
# Estimates age-specific numbers, proportions and rates of avodiable deaths for 35 cancer sites 
#
#
###############################################


#setwd("")

# Expected survival

ES3<-p%>%
  as.data.frame()%>%
  dplyr::filter(year==2019)%>%
  dplyr::rename(ES="es")%>%
  select(-country_label, -year)


ES2<-ES3



Survival_Modelled


# Anchored and combined data set at t=5 with anchored values from Israel and Thailand

popmort<-popmort2%>%
  dplyr::filter(sex!=0)%>%
  dplyr::filter(year==2015)%>%
  dplyr::mutate(mx=1-prob)%>%
  dplyr::mutate(country_code=as.numeric(country_code))%>%
  dplyr::group_by(country_code,age,year)%>%
  dplyr::summarize(country_label, 
                   country_code,
                   age_label)%>% 
  as.data.frame()%>%
  distinct()



countries_5y<-Survival_Modelled%>%
  filter(cancer_label!="Nasophar")%>%
  dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
                                   rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  dplyr::mutate(country_label = str_remove( country_label,"'"))%>%
  dplyr::mutate(country_label = str_remove( country_label,"`"))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
  distinct()



Countries_Simulated <-countries_5y%>%
  dplyr::mutate(age = case_when(
    age==0~ 0,
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    #   age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::mutate(
    age_cat = case_when(
        age>=4 & age<11 ~ "15-49",
        age>=11 & age<=14 ~ "50-69",
        age>14 ~ "70-99",
        age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  dplyr::group_by(country_label,cancer_label,age_cat)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,
                   age, age_cat,
                   rel_surv, hdi_group)%>%
  as.data.frame()




simulated_overall<-Countries_Simulated%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_label,cancer_label, age_cat,age)%>%
  dplyr::summarize(country_code,country_label, cancer_code, cancer_label,rel_surv,
                   age_cat, age, hdi_group)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)



# PAF combinations

PAFs_age_Cat <- PAFs%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::mutate(age = case_when(
    age==0~ 0, 
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    #age>=16 ~ 16,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  as.data.frame()%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<11 ~ "15-49",
      age>=11 & age<=14 ~ "50-69",
      age>14 ~ "70-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  droplevels()%>%
  as.data.frame()%>%
  left_join(ES2, by=c("country_code","age","sex"))%>%
  ungroup()%>%
  as.data.frame()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, sex,age_cat, 
         cases, cases.prev, 
         cases.notprev, af.comb,
         ES, py)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  distinct()%>%
  as.data.frame()%>%
  group_by(country_label,cancer_label, age, sex) %>%
  # dplyr::mutate(af.comb = case_when(cases!=0 ~  sum(cases.prev, na.rm=T)/sum(cases, na.rm=T),
  #                                  cases.prev==0 ~ 0))
  dplyr::mutate(country_code,country_label, cancer_code, cancer_label,
                age, sex,age_cat, 
                cases=sum(cases, na.rm=T),
                cases.prev=sum(cases.prev, na.rm=T), 
                cases.notprev=sum(cases.notprev, na.rm=T),
                total_overall=cases,
                py=sum(py),
                ES)%>%
  ungroup()%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  as.data.frame()%>%
  dplyr::mutate(af.comb = case_when(cases!=0 ~  cases.prev/cases,
                                    cases==0 ~ 0))%>%
  distinct()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age, sex,age_cat, 
         cases, cases.prev, 
         cases.notprev, af.comb,
         total_overall, ES, py)%>%
  distinct()%>%
  dplyr::group_by(country_label,cancer_label, age)


PAFs2 <- PAFs_age_Cat%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
  as.data.frame()%>%
  dplyr::group_by(country_code,cancer_code,age,sex)%>%
  dplyr::select(country_label, cancer_label,
                age_cat, age,  total_overall,cases,
                cases.prev, ES, af.comb, py)%>%
  distinct()%>%
  arrange(cancer_label,
          age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1 <- simulated_overall%>%
  ungroup()%>%
  dplyr::filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  dplyr::select(country_code, 
                country_label, 
                cancer_code, cancer_label,
                age_cat, age, total_overall,
                rel_surv,
                af.comb,
                rel_surv,
                ES,
                total_overall,
                hdi_group, py)%>%
  droplevels()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


#regional mapping 

HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  
  dplyr::filter(area<=21)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>% 
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
    ))%>% 
  select(-country_label)%>%
  distinct()




Simulated_Data_PAF_All <- Simulated_Data_PAF_1%>%
  dplyr::mutate(rel_surv=as.double(rel_surv))%>%
  dplyr::mutate(af.comb=as.double(af.comb))%>% 
  dplyr::mutate(total_overall=as.double(total_overall))%>% 
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code"))%>%
  select(-cancer_label)%>%
  left_join(globocan_cancer_names,by=c("cancer_code"))%>%
  left_join(HDI_Region_Mapping2)
#filter(continent==5) # change for region speciic analysiss




# Avoidable deaths

Avoidable_Deaths_Simulated_All3<-Simulated_Data_PAF_All%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev= total_overall * af.comb *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_treat=(1-af.comb) *total_overall * (surv_ref-rel_surv) * ES)%>%
  dplyr::mutate(AD_unavoid =total_overall*(1-ES*surv_ref-af.comb*ES*(1-surv_ref)))%>%
  dplyr::mutate(total_deaths3=total_overall*(1-ES*rel_surv))%>% #expected deaths
  dplyr::mutate(total_deaths2=AD_prev+AD_treat+AD_unavoid)%>%#expected deaths
  select("country_code","country_label","age_cat","age","sex","cancer_code","cancer_label",   
         "AD_treat",
         # "AD_treat_not_prev",
         "AD_prev",
         "AD_unavoid",
         "total_deaths2", "total_deaths3",
         "total_overall",
         "hdi_group", py)%>%
  dplyr::rename("cancer"="cancer_label")
colorectal<-Avoidable_Deaths_Simulated_All3%>%
  dplyr::filter(cancer_code%in%c("8","9"))%>%
  dplyr::mutate(cancer="Colorectal",
                cancer_code=8)%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                AD_treat=sum(AD_treat),
                AD_unavoid=sum(AD_unavoid),
                total_deaths2=sum(total_deaths2),
                total_overall=sum(total_overall))%>%
  distinct()%>%
  as.data.frame()

colorectal


Avoidable_Deaths_Simulated_All2<-Avoidable_Deaths_Simulated_All3%>%
  dplyr::filter(!cancer_code%in%c("8","9"))%>%
  full_join(colorectal)%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Liver and intrahepatic bile ducts", "Liver"))%>%
  dplyr::mutate(cancer = replace(cancer, cancer == "Trachea, bronchus and lung", "Lung"))%>%
  distinct()


Avoidable_Deaths_Simulated_All2

# Weights
w2 <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500) #SEGI_pop weights
age <- c(1:18)
weights2 <- data.frame(w2, age)


weights2 <- weights2%>%dplyr::mutate(age = case_when(
  age==1~ 1,
  age==2~ 2,
  age==3~ 3,
  age==4~ 4,
  age>4 & age<9 ~ 4,
  age==9 ~ 9,
  age==10 ~ 10,
  age==11 ~ 11,
  age==12 ~ 12,
  age==13 ~ 13,
  age==14 ~ 14,
  age==15 ~ 15,
  age==16 ~ 16,
  age==17 ~ 17,
  age==18 ~ 18))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  dplyr::group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


Avoidable_Deaths_Simulated_All<- Avoidable_Deaths_Simulated_All2%>%
  as.data.frame()%>%
  dplyr::mutate(age=as.numeric(as.character(age)))%>%
  dplyr::mutate(AD_prev=as.numeric(as.character(AD_prev)))%>%
  dplyr::mutate(AD_unavoid=as.numeric(as.character(AD_unavoid)))%>%
  dplyr::mutate(total_overall=as.numeric(as.character(total_overall)))%>%
  dplyr::mutate(AD_treat=as.numeric(as.character(AD_treat)))%>%
  dplyr::mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  dplyr::mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  dplyr::mutate(country_code=as.integer(country_code))%>%
  left_join(weights2)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0, 
                                     # Numerical calculation error causes the countries which are references  to go slightly negative. 
                                     # By definition this is zero This is rounded to 0
                                     TRUE ~ AD_treat))%>%
  mutate(sex=0)%>%
  dplyr::group_by(country_code, cancer_code, sex, age)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                AD_treat=sum(AD_treat),
                AD_unavoid=sum(AD_unavoid),
                total_deaths2=sum(total_deaths2),
                total_overall=sum(total_overall),
                py=sum(py),
                AD_sum=sum(AD_sum))%>%
  distinct()%>%
  as.data.frame()


sum(Avoidable_Deaths_Simulated_All$total_deaths2)## added new rate code below


Avoidable_Deaths_Simulated_All_overall <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_sum.rate=sum(AD_sum/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>% 
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select(-age, -sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code,age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()





Avoidable_Deaths_Simulated_All_age_cat <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  dplyr::group_by(country_code, cancer_code, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_sum.rate=sum(AD_sum/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age, -sex)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code, age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  as.data.frame()%>%
  ungroup()






# Data by country, HDI, etc

AD_by_HDI <- Avoidable_Deaths_Simulated_All%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  ungroup()%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev, 
         AD_unavoid, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group,cancer, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_sum.rate=sum(AD_sum/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::group_by(hdi_group,cancer, age_cat)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()





AD_by_HDI_all <-Avoidable_Deaths_Simulated_All%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, AD_sum,-sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  dplyr::mutate(hdi_group=case_when(
    hdi_group==1~ 984,
    hdi_group==2~ 983,
    hdi_group==3~ 982,
    hdi_group==4~ 981
  )
  
  )%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_sum.rate=sum(AD_sum/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::rename("country_code"="hdi_group")


AD_by_HDI_all


# By country for all cancer sites (number and proportion):

AD_country_all_cancers <-Avoidable_Deaths_Simulated_All%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(country_label, cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-sex,-total_overall,-AD_sum)%>% 
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  )%>%
  dplyr::group_by(country_code, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=40)%>%
  select(-py,-w)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# By cancer site

AD_cancer <- Avoidable_Deaths_Simulated_All%>%
  dplyr::group_by(cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-age,-sex)%>%
  dplyr::mutate(country_code=900)%>%
  dplyr::mutate(country_label="All Countries")%>%
  dplyr::group_by(cancer, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()



# Calculating by region. Need a file that links countries to region
HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  
  dplyr::filter(area<=21)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>%
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)   ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
      
    ))%>% 
  select(-country_label)%>%
  distinct()


areas <- HDI_Region_Mapping%>%
  dplyr::filter(country_code>=910& country_code<=931 |
                  country_code==905 | country_code==906|
                  country_code==954| country_code==957 )%>%
  select(area, country_label)%>%
  dplyr::mutate( area = case_when(
    area %in% c(19,20,21)  ~ 19,
    !area %in% c(19,20,21)  ~ area
  ))%>%
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
    ))%>%
  distinct()


# By region
AD_Region2 <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
 # dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age_cat, sex)%>%
 dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=40)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()

#region and cancer site

AD_Region_cancer_sites <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  
  select(-country_label,-country_code,-cancer_code, hdi_group,  AD_treat, AD_prev, 
         AD_unavoid, total_deaths2, -sex, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area,cancer, age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()%>%
  dplyr::group_by(area, age_cat)

# age standardizing by region - aggregate by region and then age standardize

countries_regions<-Avoidable_Deaths_Simulated_All%>%
  select(country_code)%>%
  distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))


# Table 1 in the manuscript

# Gives us number, percentage of total deaths 
table_1_1_no_chin_ind <- Avoidable_Deaths_Simulated_All %>%
  filter(!country_label%in%c("China","India"))%>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::group_by(cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat + AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  ungroup()%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  
  select(AD_prev, AD_treat,          AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat,

         pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.rate,AD_treat.rate ,
         AD_treat_prev.rate, AD_unavoid.rate,total.deaths.rate)%>%
  distinct()


# By Country and all cancer sites

AD_country_all_cancers2 <- AD_country_all_cancers%>%  
  ungroup()%>%
  distinct()%>%
  #adding confidence intervals from Globocan uncertainty
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low= pAD_treat-pAD_treat*var.low,
         pAD_treat.high= pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low= AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high= AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low= pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high= pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low= AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high= AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low= pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high= pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low= total_deaths-total_deaths*var.low,
         total_deaths.high= total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "country_code","country_label","cancer_code", "cancer", "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          # AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  dplyr::mutate(across(c(AD_prev, AD_treat, AD_treat_prev, total_deaths), 
                       ~ ifelse(. < 10, "<10", format(., big.mark = ","))))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("country_code","country_label","cancer_code", "cancer","age_cat", 
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()



  


# Gives us number, percentage of total deaths 
table_1_1 <- Avoidable_Deaths_Simulated_All %>%
  select(-country_label, -country_code, -cancer_code, hdi_group, age_cat)%>%
  #dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age_cat, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>% 
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  group_by(age_cat)%>%
  dplyr::mutate(AD_prev.rate=sum(AD_prev/py*100000 ), #rate calculation here
                AD_treat.rate=sum(AD_treat/py*100000 ),
                AD_treat_prev.rate=sum((AD_treat+AD_prev)/py*100000 ),
                AD_unavoid.rate=sum(AD_unavoid/py*100000 ),
                total.deaths.rate=sum(total_deaths2/py*100000 )) %>%
  ungroup()%>%
  group_by(age_cat)%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(age_cat,AD_prev, AD_treat, 
         AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat,
         pAD_prev,pAD_treat_prev, pAD_unavoid,
         AD_prev.rate,AD_treat.rate ,
         AD_treat_prev.rate, AD_unavoid.rate,total.deaths.rate)%>%
  distinct()


# By country AND cancer site
# AD by country and cancer site

Avoidable_Deaths_Simulated_All
Avoidable_Deaths_Simulated_All_age_cat
Avoidable_Deaths_Simulated_All_age_cat_overall<-Avoidable_Deaths_Simulated_All_age_cat%>% #Object to plot overall age standardized on world maps 
  as.data.frame()%>%
  dplyr::rename("AD_treat_prev.rate"="AD_sum.rate")%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code, age_cat)%>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat, AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  
  #When total_deaths= 0 set case
  
  dplyr::mutate(pAD_treat=case_when(total_deaths!=0~ AD_treat/total_deaths,
                                    total_deaths==0~ 0
                                    ))%>%
  dplyr::mutate(pAD_prev=case_when(total_deaths!=0~ AD_prev/total_deaths,
                                   total_deaths==0~ 0
  ))%>%
  dplyr::mutate(pAD_unavoid=case_when(total_deaths!=0~ AD_unavoid/total_deaths,
                                       total_deaths==0~ 0
  ))%>%
  dplyr::mutate(pAD_treat_prev=case_when(total_deaths!=0~ AD_treat_prev/total_deaths,
                                         total_deaths==0~ 0
  ))%>%
  as.data.frame()%>%
  select( "country_code","country_label","cancer_code", "cancer",  "age_cat",
          "AD_prev","pAD_prev",    
          "AD_treat",  "pAD_treat" ,
          "AD_treat_prev", "pAD_treat_prev",
          "AD_unavoid",   "pAD_unavoid" ,        
          "total_deaths", "AD_prev.rate", "AD_treat.rate", "AD_treat_prev.rate", "AD_unavoid.rate", "total.deaths.rate")%>%
  #adding confidence intervals from Globocan uncertainty
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%#
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low= pAD_treat-pAD_treat*var.low,
         pAD_treat.high= pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low= AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high= AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low= pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high= pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low= AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high= AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low= pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high= pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low= total_deaths-total_deaths*var.low,
         total_deaths.high= total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "country_code","country_label","cancer_code", "cancer",  "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("country_code","country_label","cancer_code", "cancer", "age_cat",
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()








#By cancer site

AD_cancer2 <- AD_cancer%>%
  select( "country_code", "country_label","age_cat",
          "cancer_code", "cancer", 
          "AD_prev",          
          "AD_treat",   
          "AD_treat_prev",  
          "AD_unavoid",     
          "total_deaths",
          "pAD_prev",     "pAD_treat" ,"pAD_treat_prev","pAD_unavoid" ,     
          "AD_prev.rate",
          "AD_treat.rate",
          "AD_treat_prev.rate",
          "AD_unavoid.rate",  
          "total.deaths.rate",
  )%>%
  as.data.frame()%>%
  #adding confidence intervals from Globocan uncertainty
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low = pAD_treat-pAD_treat*var.low,
         pAD_treat.high = pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low = AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high = AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low = pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high = pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low = AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high = AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low = pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high = pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low = total_deaths-total_deaths*var.low,
         total_deaths.high = total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "country_code","country_label","cancer_code", "cancer", "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("country_code","country_label","cancer_code", "cancer", "age_cat",
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         #  "Unavoidable Deaths", "Proportion Unavoidable Deaths", "Unavoidable Deaths (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()




#By region
AD_Region<-AD_Region2%>%
  arrange(area)%>%
  dplyr::mutate(
    country_code= case_when(
      area %in% c(19)   ~ 999,
      !area %in% c(19)  ~ country_code
    ))%>% 
  #adding confidence intervals from Globocan uncertainty
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low= pAD_treat-pAD_treat*var.low,
         pAD_treat.high= pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low= AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high= AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low= pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high= pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low= AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high= AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low= pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high= pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low= total_deaths-total_deaths*var.low,
         total_deaths.high= total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "continent","area","country_code","country_label","cancer_code", "cancer", "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          # AD_treat_not_prev, pAD_treat_not_prev,
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("area","country_code","country_label","cancer_code", "cancer", 
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)","age_cat",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()





#World total

table_1_11<-table_1_1%>%
  mutate(country_code=900,cancer_code=39)%>%
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low= pAD_treat-pAD_treat*var.low,
         pAD_treat.high= pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low= AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high= AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low= pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high= pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low= AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high= AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low= pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high= pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low= total_deaths-total_deaths*var.low,
         total_deaths.high= total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "country_code","cancer_code", "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("country_code","cancer_code", "age_cat",
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()



#HDI

AD_by_HDI

AD_by_HDI_all2<-AD_by_HDI_all%>%
  mutate(cancer_code=39)%>%
  #adding confidence intervals from Globocan uncertainty
  left_join(globocan_UIs, by=c("country_code","cancer_code" ))%>%
  mutate(AD_prev.low= AD_prev-AD_prev*var.low,
         AD_prev.high= AD_prev+AD_prev*var.high,
         pAD_prev.low= pAD_prev-pAD_prev*var.low,
         pAD_prev.high= pAD_prev+pAD_prev*var.high,
         AD_treat.low= AD_treat-AD_treat*var.low,
         AD_treat.high= AD_treat+AD_treat*var.high,
         pAD_treat.low= pAD_treat-pAD_treat*var.low,
         pAD_treat.high= pAD_treat+pAD_treat*var.high,
         AD_treat_prev.low= AD_treat_prev-AD_treat_prev*var.low,
         AD_treat_prev.high= AD_treat_prev+AD_treat_prev*var.high,
         pAD_treat_prev.low= pAD_treat_prev-pAD_treat_prev*var.low,
         pAD_treat_prev.high= pAD_treat_prev+pAD_treat_prev*var.high,
         AD_unavoid.low = AD_unavoid-AD_unavoid*var.low,
         AD_unavoid.high = AD_unavoid+AD_unavoid*var.high,
         pAD_unavoid.low = pAD_unavoid-pAD_unavoid*var.low,
         pAD_unavoid.high= pAD_unavoid+pAD_unavoid*var.high,
         total_deaths.low= total_deaths-total_deaths*var.low,
         total_deaths.high= total_deaths+total_deaths*var.high,
         AD_prev.rate.low  = AD_prev.rate-AD_prev.rate*var.low,
         AD_prev.rate.high = AD_prev.rate+AD_prev.rate*var.high,
         AD_treat.rate.low = AD_treat.rate-AD_treat.rate*var.low,
         AD_treat.rate.high= AD_treat.rate+AD_treat.rate*var.high,
         AD_treat_prev.rate.low = AD_treat_prev.rate-AD_treat_prev.rate*var.low,
         AD_treat_prev.rate.high= AD_treat_prev.rate+AD_treat_prev.rate*var.high,
         AD_unavoid.rate.low = AD_unavoid.rate-AD_unavoid.rate*var.low,
         AD_unavoid.rate.high= AD_unavoid.rate+AD_unavoid.rate*var.high,
         total.deaths.rate.low = total.deaths.rate-total.deaths.rate*var.low,
         total.deaths.rate.high= total.deaths.rate+total.deaths.rate*var.high
  )%>%
  select( "country_code","cancer_code", "cancer", "age_cat",
          "AD_prev", "AD_prev.low", "AD_prev.high",
          "AD_treat",
          "AD_treat.low", "AD_treat.high",
          "AD_treat_prev", 
          "AD_treat_prev.low", "AD_treat_prev.high",
          "AD_unavoid", 
          "AD_unavoid.low", "AD_unavoid.high",
          "total_deaths",
          "total_deaths.low", "total_deaths.high",
          "pAD_prev", 
          "pAD_prev.low", "pAD_prev.high",    
          "pAD_treat" ,
          "pAD_treat.low", "pAD_treat.high",
          "pAD_treat_prev",
          "pAD_treat_prev.low", "pAD_treat_prev.high",
          "pAD_unavoid" ,        
          "pAD_unavoid.low", "pAD_unavoid.high",
          "total_deaths", "total_deaths.low", "total_deaths.high",
          "AD_prev.rate",  "AD_prev.rate.low", "AD_prev.rate.high",
          "AD_treat.rate", "AD_treat.rate.low", "AD_treat.rate.high",
          "AD_treat_prev.rate", "AD_treat_prev.rate.low", "AD_treat_prev.rate.high",
          "AD_unavoid.rate", "AD_unavoid.rate.low", "AD_unavoid.rate.high",
          "total.deaths.rate", "total.deaths.rate.low", "total.deaths.rate.high")%>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100),
         across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ round(., -2)))%>%
  mutate(across(c("AD_prev", "AD_prev.low", "AD_prev.high",
                  "AD_treat", "AD_treat.low", "AD_treat.high",
                  "AD_treat_prev", "AD_treat_prev.low", "AD_treat_prev.high",
                  "AD_unavoid", "AD_unavoid.low", "AD_unavoid.high",
                  "total_deaths", "total_deaths.low", "total_deaths.high"), ~ paste0(format(.x, big.mark = ","))))%>%
  mutate(across(contains("rate"), ~ round(., 1)))%>%
  mutate("Preventable Deaths"= paste0(AD_prev, " (", AD_prev.low, ", ", AD_prev.high, ")"),
         "Treatable Deaths"= paste0(AD_treat, " (", AD_treat.low, ", ", AD_treat.high, ")"),
         "Treatable Deaths (including preventable)"= paste0(AD_treat_prev, " (", AD_treat_prev.low, ", ", AD_treat_prev.high, ")"),
         "Unavoidable Deaths"= paste0(AD_unavoid, " (", AD_unavoid.low, ", ", AD_unavoid.high, ")"),
         "Total Deaths"= paste0(total_deaths, " (", total_deaths.low, ", ", total_deaths.high, ")"),
         "Proportion Preventable Deaths"= paste0(pAD_prev, " (", pAD_prev.low, ", ", pAD_prev.high, ")"),
         "Proportion Treatable Deaths"= paste0(pAD_treat, " (", pAD_treat.low, ", ", pAD_treat.high, ")"),
         "Proportion Treatable Deaths (including preventable)"= paste0(pAD_treat_prev, " (", pAD_treat_prev.low, ", ", pAD_treat_prev.high, ")"),
         "Preventable Deaths (rate)"= paste0(AD_prev.rate, " (", AD_prev.rate.low, ", ", AD_prev.rate.high, ")"),
         "Treatable Deaths (rate)"= paste0(AD_treat.rate, " (", AD_treat.rate.low, ", ", AD_treat.rate.high, ")"),
         "Treatable Deaths (including preventable) (rate)"= paste0(AD_treat_prev.rate, " (", AD_treat_prev.rate.low, ", ", AD_treat_prev.rate.high, ")"),
         "Unavoidable Deaths (rate)"= paste0(AD_unavoid.rate, " (", AD_unavoid.rate.low, ", ", AD_unavoid.rate.high, ")"),
         "Total Deaths (rate)"= paste0(total.deaths.rate, " (", total.deaths.rate.low, ", ", total.deaths.rate.high, ")") )%>%
  select("country_code","cancer_code", "cancer", "age_cat",
         "Preventable Deaths", "Proportion Preventable Deaths", "Preventable Deaths (rate)",
         "Treatable Deaths", "Proportion Treatable Deaths", "Treatable Deaths (rate)",
         "Treatable Deaths (including preventable)", "Proportion Treatable Deaths (including preventable)", "Treatable Deaths (including preventable) (rate)",
         "Total Deaths", "Total Deaths (rate)")%>%
  as.data.frame()











# Combined table for paper (99%) ready to copy paste


AD_Region
AD_by_HDI
table_1_11
AD_Region
AD_by_HDI_all2
AD_country_all_cancers2
AD_cancer2
Avoidable_Deaths_Simulated_All_age_cat_overall




# writing the files

# 



#Summing up the number of cases for the ppt and checking so cases in data set are same before and after


