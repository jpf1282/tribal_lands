
coxc <- function(measure,sortdir,sortby,plottype){

  if (sortby==1){sortvar<-"diff"} else if (sortby==2) {sortvar<-"time1"}else if (sortby==3) {sortvar="tribe"} 
  if(sortdir=="d"){srt<-paste0("desc(", sortvar,")")} else if (sortdir=="a") {srt<-sortvar}


#create df of unique values by tribe, time, measure of interest, drop NA, sort by t1 or t2-t1 diff

  uniqueFIPS <- coxcomb_tribes  %>% distinct_("tribe","time",.keep_all=TRUE) %>% mutate(time=gsub(" ","",time)) %>%
    select_("tribe","time",measure) %>% spread_("time",measure) %>% drop_na() %>% mutate(diff = time2-time1) %>% arrange(!!parse_expr(srt))

  uniqueFIPS$tribe<-factor(uniqueFIPS$tribe,levels=uniqueFIPS$tribe[order(unlist(uniqueFIPS[sortvar]))]) #factorize for sorting
  uniqueFIPS$tribe_label<-str_wrap(uniqueFIPS$tribe, width=10) #create text wrapped variable
  uniqueFIPS$tribe_label<-factor(uniqueFIPS$tribe_label,levels=uniqueFIPS$tribe_label[order(unlist(uniqueFIPS[sortvar]))])  #factorize for sorting
  # ymax<-max(uniqueFIPS$time1, na.rm=TRUE) +.25*sd(uniqueFIPS$time1,na.rm=TRUE) #calculate ymax based on t1 - max value + .25 x standard deviation
  uniqueFIPS<-uniqueFIPS[1:20,] #take first 25

  print(head(uniqueFIPS,n=15))

  #rearrange df for coxcomb
  uniqueFIPS_melted<-uniqueFIPS %>% select(tribe, tribe_label,time1,time2) %>% gather('time','value',3:4)

  dir<-ifelse(sortvar=="tribe",1,-1) #arrange plot anticlockwise unless ordered by tribe
  print(dir)
  coxc<-ggplot(uniqueFIPS_melted,aes_string(x="tribe_label", y="value"))+
    geom_col(aes(fill=time),position="identity")+ #creates bars, "identity" produces overlay, unless facet_grid is added
    coord_polar(direction=dir)+ #make circular
    scale_fill_manual(name="", values=c('#7dce94','#cd5554'), labels=c("Time 1", "Time 2"))+
    theme_bw()+
    ylab(measure)+
    xlab("")

  if (plottype==1){#plot single coxcomb
    coxc
  } else if (plottype==2){#plot facetted coxcomb
    labels<-c(time1="Time 1", time2 = "Time 2")
    coxc+facet_grid(. ~time,labeller=labeller(time = labels))

  }


}
