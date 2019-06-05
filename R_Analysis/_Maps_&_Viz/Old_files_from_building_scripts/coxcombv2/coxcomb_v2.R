library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)
setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/")
load("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/processed_data.RData")


head(merged_data_record_all_long)
coxc("n_unique_FIPS") #call function; parameters are time 1, y axis limit, variable

##############function
coxc <- function(measure){
srt<-paste0("desc(", "`time 1`",")") #for sorting by t1

#create df of unique values by tribe, time, measure of interest, drop NA, sort by t1
uniqueFIPS<-merged_data_record_all_long  %>% distinct_("tribe","time",.keep_all=TRUE) %>% 
  select_("tribe","time",measure) %>% spread_("time",measure) %>% drop_na() %>% arrange(!!parse_expr(srt)) 
  
# ymax<-max(uniqueFIPS$`time 1`, na.rm=TRUE) +.25*sd(uniqueFIPS$`time 1`,na.rm=TRUE) #calculate ymax
ymax<-max(uniqueFIPS$`time 1`, na.rm=TRUE) +.05*sd(uniqueFIPS$`time 1`,na.rm=TRUE) #calculate ymax

uniqueFIPS$tribe_label<-str_wrap(uniqueFIPS$tribe, width=10) #wrap labels

#plot 
t1<-ggplot(uniqueFIPS[1:50,],aes_string(x="tribe_label", y="`time 1`"))+geom_col(fill="#7dce94",alpha=.75)+ coord_polar()+ylim(0,ymax)+theme_bw()+ ylab("Number of Counties") +xlab("")

t1+geom_col(aes_string(x="tribe_label", y="`time 2`"), fill="#cd5554",alpha = 1)
}                          
