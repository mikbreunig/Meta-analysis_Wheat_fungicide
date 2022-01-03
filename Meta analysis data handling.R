#####created 3/26/2021#####
##edited 4/1/21 running final data####
###re-ran 8/15/21 for final figures for submission

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("VCA","ggpubr","multcomp","ggridges","emmeans","broom","ggplot2", "dr4pl", "tidyverse","ggplot2", "drc", "lme4", "lsmeans", "plyr", "plotrix", "knitr", "ggplot2", "lmtest", "lmerTest", "Rmisc", "gridExtra", "plotly", "webshot", "ggpmisc", "ggsci","scales")
ipak(packages)

setwd(dir="C:/Users/mbreu/OneDrive - Michigan State University/Meta-analysis/final data work//")

##All missing values were removed from data sheets prior to uploading 
chilvers<-read_delim(file="chilvers concatenated_final.csv",delim=",") %>% 
  select(c(site,PI,timing,block,year,variety,yield,project))
kurt<-read_delim(file="Kurt concatenated_final.csv",delim=",") %>% 
  select(c(site,timing,block,year,variety,yield,PI,project))

nagelkirk<-read_delim(file="Nagelkirk Concatenated_final.csv", delim="," , na=c("."," ")) %>% 
  select(c(site,timing,block,year,variety,yield,PI,project))

all.raw.data<-rbind(chilvers,kurt,nagelkirk) %>% 
  mutate(StudyCode=str_c(PI,year,site,project,variety,sep="_"))
View(kurt)

###create table with Meta_Data about studies to be joined later with results####
#make unique study ID
Study.name.df<-read_delim(file = "originalNames.csv",delim=",")

#StudyCode<-unique(all.raw.data$StudyCode)
#ID<-seq(1:106)
#Study.name.df<- data.frame(StudyCode,ID)
#names(Study.name.df) <- c("StudyCode","ID")

all.raw.wcode<-full_join(all.raw.data,Study.name.df,by="StudyCode")
View(all.raw.wcode)
MetaData<-all.raw.wcode %>% 
  select(c(site,PI,year,variety,project,StudyCode,ID)) %>% 
  distinct(ID,.keep_all = TRUE)

#distinct(study, harvest_year,trtname,timing, .keep_all = TRUE)

#write.csv(all.raw.wcode,"RawOutput.csv")
#trtmeans<-read_delim(file="EffectCalc.csv",delim=",") 
  View(n.trt.meta)
###decided that some of the treatments did not have good represenatation (less than 10 studies).
#So excluding them, and re-doing means and variance estimations
only6<-all.raw.wcode %>% 
  filter(timing!="T1_T2_T3") %>% 
  filter(timing!="T1_T2")%>%
  filter(timing!="T3_2X") %>% 
  filter(timing!="T3_E")
View(only6)  
write.csv(only6,"RawOutput_6trt.csv")

#determine number of replicates for each treatment/study combo
n.trt.meta<-only6 %>% 
  group_by(StudyCode,timing) %>% 
  summarise(n=n()) 
#%>% 
  filter(timing!="UTC")
View(n.trt.meta)
n.study.meta<-only6 %>% 
  group_by(StudyCode) %>% 
  summarise(n=length(unique(block)))
View(n.trt.meta)
n.trt.meta<-only6 %>% 
  group_by(timing,StudyCode) %>% 
  summarise(n=n()) 
####Load meta-data from each trial and join with estimates of each mean generated from SAS#####
#MeanEstimates<-read_delim(file = "CompiledMeans040121.csv",delim=",")
MeanEstimates<-read_delim(file = "CompiledMeans040121_Only6Trt.csv",delim=",")

mean.studies<-left_join(MeanEstimates,Study.name.df, by="ID")
mean.meta<-left_join(mean.studies,n.trt.meta,by=c("StudyCode","timing"))
View(mean.meta)
#StudiesMETA<-all.raw.wcode %>% 
#distinct(study, harvest_year,trtname,timing, .keep_all = TRUE)


n.studies.timing<-only6 %>% 
  group_by(timing) %>% 
  summarise(n=n()) 
##### create new variable of yield difference as well as %diff#### 
Calculation<-mean.meta %>% 
  group_by(ID) %>% 
  mutate(UTC_estimate=estimate[timing=="UTC"]) %>% 
  mutate(UTC_n=n[timing=="UTC"]) %>% 
  mutate(NomDiff=estimate-UTC_estimate) %>% 
  mutate(PerDiff= 100*(NomDiff /UTC_estimate)) %>% 
  left_join(MetaData,by="ID") %>% 
  filter(timing!="UTC")
  
View(Calculation)

##########The code below is for either exploratory analysis or Figure###
###the Actual meta-analysis was completed in SAS, and can be found in a separate file###


NomAvg<-Calculation %>% 
  filter(timing!="T1") %>% 
  filter(timing!="T1_T3") %>% 
  filter(timing!="T2") %>% 
  filter(timing!="T2_T3") %>% 
  filter(timing!="T3_L") %>% 
  summary(NomDiff)

#write.csv(Calculation,"EffectCalc.csv")
#(Calculation,"EffectCalc_6trt.csv")
write.csv(Calculation,"EffectCalc_6trt_META.csv")

##Added column with High or Low yield of NTC to experimetn with moderator analysis 
ModTest<-read_delim(file="EffectCalc_6trt_META.csv",delim=",")
  

View(all.raw.data)

###Plots to investigate data, normality ####
ggqqplot(all.raw.data$yield)
ggqqplot(Calculation$D)

ggdensity(all.raw.data$yield, 
          xlab = "Yield ")

ggdensity(Calculation$D, 
          xlab = "Yield estimate of treatments")


###IF shapiro test is significant, means it is not normal #####

shapiro.test(Calculation$D)
shapiro.test(all.raw.data$yield)


####Data visualization to explore trends####
View(Calculation)
Viz<-Calculation %>% 
  mutate(IndTrt=str_c(timing,ID,sep="_")) %>% 
  mutate(SED = sqrt(
    res_variance*((1/n)+(1/UTC_n))
    ))
Viz$year<-as.factor(Viz$year)
View(Viz)
ggplot(Viz, aes(x=timing, y=NomDiff)) +
  geom_boxplot(size=1,aes(color = YieldMod), position="dodge")+
  geom_point(aes(fill=YieldMod),position=position_jitterdodge(jitter.width=0))+
  ylab(expression("D"[t]*" (bu/ac)"))+
  xlab("Timing")+
#geom_jitter(aes(x=timing, y=NomDiff),position="dodge",alpha=0.5,width=0.1)+
  #geom_dotplot(binaxis="y",stackdir="center",dotsize=0.2)+
  theme(strip.text = element_text(size = 20),axis.text = element_text(size=20),axis.title = element_text(size=18))

ggplot(Viz, aes(x=timing, y=NomDiff)) +
  geom_boxplot(size=1, position="dodge")+
  #geom_point())+
  ylab(expression("D"[t]*" (bu/ac)"))+
  xlab("Timing")+
  geom_jitter(aes(x=timing, y=NomDiff),alpha=0.5,width=0.1)+
  #geom_dotplot(binaxis="y",stackdir="center",dotsize=0.2)+
  theme(strip.text = element_text(size = 20),axis.text = element_text(size=20),axis.title = element_text(size=18))


Site.ord<-c("campus", "sanilac","SVRC")
Viz$site<-factor(x=Viz$site,levels=Site.ord)
  
  
ggplot(Viz, aes(x=timing, y=NomDiff)) +
  geom_boxplot(size=1,color = "black", outlier.shape = NA, position="dodge")+
  geom_jitter(width=0.15)+
  #facet_grid(cols=vars(chem))+
  ylab("Yield Response (bu/ac)")+
  xlab("Timing")+
  theme_bw()+
  theme(strip.text = element_text(size = 20),axis.text = element_text(size=20),axis.title = element_text(size=18))
View(Viz)



ggplot(Viz, aes(x=estimate, y=NomDiff,fill=site)) +
  geom_point(size=3,aes(color = site))+
  ylab(expression("D"[t]*" (bu/ac)"))+
  xlab("Timing")+
  theme(strip.text = element_text(size = 20),axis.text = element_text(size=20),axis.title = element_text(size=18))
  #theme(strip.text = element_text(size = 18),axis.text = element_text(size=20))




ggplot(Calculation, aes(x=timing, y=NomDiff,)) +
  geom_boxplot(size=1,color = "black", position="dodge")+
  #geom_dotplot(binaxis="y",stackdir="center",dotsize=0.2)+
  #facet_grid(cols=vars(chem))+
  theme_bw()+
  theme(strip.text = element_text(size = 18),axis.text = element_text(size=20))

ggplot(Viz, aes(x=year,y=NomDiff, fill=timing))+
  geom_jitter(aes(color=timing),width = 0.2,size=3)+
  ylab(expression("D"[t]*" (bu/ac)"))+
  xlab("Timing")+
  theme(plot.margin = unit(c(6,6,6,6),"mm"),
        plot.title = element_text(size=20, face="bold"),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=15,angle=90),
        axis.text.y = element_text(size=15),
  )
View(Viz)

Viz$IndTrt<-as.factor(Viz$IndTrt)

ggplot(data=Viz, aes(x=reorder(IndTrt,-NomDiff), y=NomDiff,fill=timing)) + 
  geom_bar(stat = "identity", width=0.2, aes(color=timing)) +
  geom_errorbar(aes(ymax=NomDiff+SED,ymin=NomDiff-SED))+
  facet_grid(~PI,scale="free_x")

             

ggplot(data=Viz, aes(x=reorder(IndTrt,-NomDiff), y=NomDiff,fill=timing)) + 
  geom_bar(stat = "identity", width=0.4, aes(color=timing)) +
  geom_errorbar(aes(ymax=NomDiff+SED,ymin=NomDiff-SED))+
  ylab("Difference in yield (bu/ac) from non-treated")+
  xlab(NULL)+
  #theme(axis.text.x = element_text(size=1))+
  #facet_wrap(~timing,scale="free_x")+
  theme_classic(base_size=18)

View(Viz)
ggplot(data=Viz, aes(x=reorder(IndTrt,-NomDiff), y=NomDiff,fill=timing)) + 
  geom_point(shape=18,aes(color=timing),size=5) +
  geom_errorbar(aes(ymax=NomDiff+SED,ymin=NomDiff-SED))+
  ylab("Difference in yield (bu/ac) from non-treated")+
  geom_hline(yintercept=0,linetype="dashed")+
  #facet_wrap(~timing,scale="free_x")+
   theme(
    axis.text.x = element_blank(),axis.ticks = element_blank())+
  theme_classic(base_size=20)+  xlab(NULL)

#####confidence intervals ####
conf.int.fig<-read_delim(file="MetaFigures.csv",delim=",")
conf.int.fig$Type<-revalue(conf.int.fig$Type, c("conf95"="95% Confidence Interval", "Pred95"="95% Prediction Interval","Pred50"= "50% Prediction Interval"))
  
conf.int.fig$timing<-revalue(conf.int.fig$timing, c("T1_T3"="1+3", "T2_T3"="2+3","T3_L"= "T3L"))

ord<-c("95% Confidence Interval", "95% Prediction Interval","50% Prediction Interval")
Tord<-c("T1","T2","T3","T3L","1+3","2+3")
conf.int.fig$Type<-factor(x=conf.int.fig$Type,levels=ord,ordered=TRUE)
conf.int.fig$timing<-factor(x=conf.int.fig$timing,levels=Tord,ordered=TRUE)

ggplot(data=conf.int.fig, aes(x=timing, y=NomDiff))+
  geom_point(size=5,shape=18)+
    geom_errorbar(aes(ymax=Lower,ymin=Upper), width=0.3)+
    facet_grid(cols=vars(Type))+
    ylab("Difference in yield (bu/ac) from non-treated")+
    theme(strip.text = element_text(size = 20),axis.text = element_text(size=20))+
    theme_bw(base_size = 18)


