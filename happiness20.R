library(tidyverse)
library(ggplot2)
library(countrycode)
library(ggthemes)
library(ggpubr)
library(ggflags)


setwd("~/R- CLASS DATASETS")
happiness20<-read_csv("happiness20.csv")
attach(happiness20)

#map each country to its respective continent
#i customed matched Kosovo coz it wasnt mapping for some reason
happiness20$region<-countrycode(sourcevar = happiness20[[1]],origin = "country.name",destination = "continent",custom_match = c(Kosovo="Europe"))

#geom point of the various life factors in relation to happiness score
#i added geom smooth with a linear model method to properly see the type oflinear  relationship
#you will note that i added a y axis title to only two graphs, this will make sense why in the grid plot i created
a<-ggplot(happiness20,aes(x=`Logged GDP per capita`,y=`Ladder score`))+
  geom_point(size=3,aes(colour=region))+theme_fivethirtyeight()+geom_smooth(method = lm)+
    labs(title = "GPD per capita")+
  theme(axis.title = element_text(face="bold",size = 18),axis.title.x = element_blank())+
  ylab("Happiness score\n")+scale_color_viridis_d(direction = -1)


b<-ggplot(happiness20,aes(y=`Ladder score`,x=`Social support`))+
  geom_point(size=3,aes(colour=region))+scale_colour_viridis_d(direction = -1)+
  theme_fivethirtyeight()+geom_smooth(method=lm)+labs(title = "Social support")

c<-ggplot(happiness20,aes(y=`Ladder score`,x=`Healthy life expectancy`))+
  geom_point(size=3,aes(colour=factor(region)))+scale_colour_viridis_d(direction = -1)+
  theme_fivethirtyeight()+geom_smooth(method=lm)+labs(title = "Healthy life expectancy")

d<-ggplot(happiness20,aes(y=`Ladder score`,x=`Freedom to make life choices`))+
  geom_point(size=3,aes(colour=region))+scale_colour_viridis_d(direction = -1)+theme_fivethirtyeight()+
  geom_smooth(method=lm)+labs(title="Freedom to make life choices")+
  theme(axis.title = element_text(face="bold",size = 18),axis.title.x = element_blank())+
  ylab("Happiness score\n")

e<-ggplot(happiness20,aes(y=`Ladder score`,x=Generosity))+geom_point(size=3,aes(colour=region))+
  scale_colour_viridis_d(direction = -1)+theme_fivethirtyeight()+geom_smooth(method="lm")+
  labs(title="Generosity")

f<-ggplot(happiness20,aes(y=`Ladder score`,x=`Perceptions of corruption`))+
  geom_point(size=3,aes(colour=region))+scale_colour_viridis_d(direction = -1)+
  theme_fivethirtyeight()+geom_smooth(method=lm)+labs(title="Perceptions of corruption")


g<-ggarrange(a,b,c,d,e,f,ncol=3,nrow=2,common.legend = TRUE,legend = "top")
annotate_figure(g,top=text_grob("Correlation between national happiness and various life factors",face ="bold",family="sans",size=25,color ="black" ),bottom=text_grob("The world happiness report is an annual publication of the United Nations Sustainable Development Solutions Network. It contains articles \n and rankings of national happiness based on respondent ratings of their own lives, which the report also correlates with various life factors.\n Source:Our World in Data \n Github:@joanweru",hjust=1,x=1,color = "#3C3C3C",face="bold"))

#code to match each country to iso2c code so that i could use geom flags, otherwise geom flags doesnt work
happiness20$iso<-countrycode(sourcevar = happiness20[[1]],origin = "country.name",destination = "iso2c",custom_match = c(Kosovo="Europe"))

happiness20_arranged<-arrange(happiness20,desc(`Ladder score`))
top<-head(happiness20_arranged,10) #countries with highest happiness score
bottom<-tail(happiness20_arranged,10) #countries with lowest happiness score


top%>%mutate(code=tolower(iso))%>%
  ggplot(aes(x=reorder(`Country name`,-`Ladder score`),y=`Ladder score`,fill=region))+
  geom_col(fill="#CC9900")+theme_fivethirtyeight()+geom_flag(y=0,aes(country=code),size=14)+
  labs(title = "World's happiest countries, 2020",subtitle ="The world happiness report is an annual publication of the United Nations Sustainable Development  Solutions Network it contains articles, \n and rankings of national happiness based  on respondent ratings of their own lives, which the report also correlates with various life factors.",caption = "Source:Our World in Data \n Github:@joanweru")+
  theme(plot.title = element_text(size=25),plot.subtitle = element_text(face="bold"))


bottom%>%mutate(code=tolower(iso))%>%
  ggplot(aes(x=reorder(`Country name`,`Ladder score`),y=`Ladder score`,fill=region))+
  geom_col(fill="#339999")+scale_fill_viridis_d()+theme_fivethirtyeight()+
  geom_flag(y=0,aes(country=code),size=14)+
  labs(title = "World's unhappiest countries, 2020",subtitle ="The world happiness report is an annual publication of the United Nations Sustainable Development Solutions Network. It contains articles \n and rankings of national happiness based  on respondent ratings of their own lives, which the report also correlates with various life factors.",caption = "Source:Our World in Data \n Github:@joanweru ")+
  theme(plot.title = element_text(size=25),plot.subtitle = element_text(face = "bold"))


