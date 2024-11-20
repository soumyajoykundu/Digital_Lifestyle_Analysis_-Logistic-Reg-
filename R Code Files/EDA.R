install.packages("tidyverse")
library("tidyverse")
View(mydata)

ggplot(mydata,aes(Gender,OS_Avail,colour=OS_Opinion))+
  geom_point()+facet_wrap(~Income)

# Single Categorical variable
mydata %>%
  drop_na(OS_App_1) %>%
  ggplot(aes(fct_infreq(OS_App_1)))+
  theme_bw()+
  geom_bar(fill=c("Royal Blue","Red","Yellow","Green","Purple","Orange","Pink"))+
  #coord_flip()+
  labs(y="Number of individuals",
       x="App Pref 1")
  
# Two Categorical Variables
# Type 1
mydata %>%
  filter(OS_Avail %in% c("Never","Rarely","Sometimes","Often"))%>%
  drop_na(OS_App_1)%>%
  ggplot(aes(OS_Rating,fill=OS_Rating))+
  geom_bar(position="dodge",alpha=0.5)+
  theme_bw()+labs(y="Frequency",x="Rate of Experience (Shopping)")+
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),legend.position = c(1,2))




# if we remove "dodge" , the graph will be stack wise

  mydata %>%
    drop_na(Gender)
  ggplot(data=mydata,aes(Gender))+
    geom_bar(aes(fill=Gender),alpha=0.5)+
    facet_wrap(~OS_Opinion)+labs(y="Frequency",x="Gender")+
    theme_bw()+theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank())
  
  
  
# Type 2
mydata %>%
  filter(OS_Opinion %in% c("Yes","No"))%>%
  #drop_na(Gender)
  ggplot(aes(Age))+
  geom_bar(aes(Age,fill=OS_Opinion),position="dodge",alpha=0.5)+
  facet_wrap(~Gender)+
  theme_bw()

ggplot(mydata)+
  geom_bar(aes(x=Gender,fill=OS_Opinion),position="dodge")+
  facet_wrap(~Age)

#total apps selected
total<-c(mydata$OS_App_1,mydata$OS_App_2,mydata$OS_App_3)
total

#Merging apps
mydata$Shopping_Apps=paste(mydata$X.OS_App_Pref_1,mydata$OS_App_Pref_2,mydata$OS_App_Pref_3,sep=",")
view(mydata)

#totalcount of apps used by all
as.mydata.frame(total)
total=mydata.frame(total)
total%>%
  ggplot(aes(fct_infreq(total)))+
         theme_bw()+
         geom_bar(fill="2")+
         coord_flip()+
         labs(y="Number of individuals",
              x="Shopping Apps")
view(total)


# computing the pie chart 
labels=c("Below Rs 1000","Rs 1000 to Rs 5000","Rs 5000 to Rs 20000
","Above Rs 20000")
mydata%>%
drop_na(OS_Price)%>%
ggplot(aes(x=" ", y=OS_Opinion, fill=OS_Opinion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=1) + theme_void()
  #facet_grid(.~ subject) +

#PIE CHART
#mydata%>%
#drop_na(OS_Price)%>%
 # filter(OS_Avail %in% c("Never","Rarely","Sometimes","Often"))%>%
ggplot(mydata,aes(x = " ", y = F_Opinion, fill = F_Opinion)) +
  geom_col() +
  coord_polar(theta = "y",start=1) +theme_void()
df=data.frame(as.data.frame(mydata$OS_Avail))
df
View(mydata)

# printing the percentage
print(pie_chart)
mydata%>%
pie(OS_Price,labels=OS_Price)


gfg <- data.frame(x = total, 
                  grp = rep(c("OS_App_1", "OS_App_2",
                              "OS_App_3"),
                            each = 522),
                  subgroup = LETTERS[1:3])
gfg


ggplot(mydata, aes(x=grp, y=x, fill = subgroup)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")





