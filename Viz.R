library(tidyverse)
cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))
cces <- drop_na(read_csv(url("https://www.dropbox.com/s/ahmt12y39unicd2/cces_sample_coursera.csv?raw=1")))
ggplot(cces, aes(x=educ,y=ideo5))+geom_jitter()

ggplot(cces, aes(x=educ,y=ideo5))+geom_jitter()+
  geom_smooth() ### the default line is a loess curve

ggplot(cces, aes(x=educ,y=ideo5))+
  geom_jitter()+
  geom_smooth(method = "lm", level=.9) ###specify confidence level

ggplot(cces, aes(x=educ,y=ideo5))+
  geom_jitter()+
  geom_smooth(method = "lm") ###specify confidence level


### scatterplot matrix
#https://rkabacoff.github.io/datavis/Other.html#scatterplot-matrix
library(GGally)

var1<- runif(100,min=0,max = 1)
var2<- var1+rnorm(100,1,.2)
var3<- var1-rnorm(100,1,.2)

df<-tibble(var1,var2,var3)

ggpairs(df)


###### customize the matrix figures
my_scatter<-function(data,mapping){
  ggplot(data=data, mapping = mapping)+
    geom_jitter(color='red')
}

my_density<-function(data,mapping){
  ggplot(data=data, mapping = mapping)+
    geom_density(alpha=.05,
      fill='red')
}


ggpairs(df,
        lower = list(continuous=my_scatter),
        diag = list(continuous=my_density)
        )

#### correlation plots
df<- as.data.frame(cces%>% select("educ","pid7","pew_religimp"))
head(df)

#### calculate correlation coefficients

r<-as.data.frame(cor(df,use="complete.obs"))


ggcorrplot(r)

ggcorrplot(r,type="lower")

ggcorrplot(r,type="lower",
           title="correlations",
           colors=c("yellow","green", "blue"),
           outline.color = "purple"
           )

ggcorrplot(r,type="lower",
           title="correlations",
           ggtheme=theme_wsj()
          
)

#### cleveland Dot Plots
#Adated from https://r-graphics.org/recipe-bar-graph-dot-plot

### use some of the congress data

cel_114<- cel %>% filter(congress==114)
head(cel_114)
members<- sample_n(cel_114,25)
head(members)


#points
ggplot(members,aes(x=les,y=thomas_name))+
  geom_point()

#### some refinements

ggplot(members,aes(x=les,y=reorder(thomas_name,les)))+
  geom_point(size=5)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype = "dashed",color="blue")
        )+
  labs(x="Legislative Effectivness",y="")


#Lollipop figure
ggplot(members,aes(x=reorder(thomas_name,les),y=les))+
  geom_point()+
  geom_segment(aes(x=thomas_name,xend=thomas_name,y=0,yend=les))+
  theme(axis.text.x = element_text(angle = 90))
  
