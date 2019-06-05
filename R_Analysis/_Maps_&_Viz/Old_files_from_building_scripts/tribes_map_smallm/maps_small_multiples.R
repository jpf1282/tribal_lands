


##########Create coordinate-type picker reference dfs for T1 and T2 ###########
coord_all1<-c("lat1","lon1")
coord_ave1<-c("avg_lat_t1","avg_lon_t1")
coord_mid1<-c("mid_lat_t1","mid_lon_t1")
coord_all2<-c("lat2","lon2")
coord_ave2<-c("avg_lat_t2","avg_lon_t2")
coord_mid2<-c("mid_lat_t2","mid_lon_t2")

coord_picker_t1<-data.frame(coord_all1,coord_ave1,coord_mid1)
coord_picker_t2<-data.frame(coord_all2,coord_ave2,coord_mid2)

####SELECT TRIBE FUNCTION#####

# "n_unique_FIPS",sortdir="d",sortby=1
selectTribes<-function(measure,sortdir,sortby,ntribes1,ntribes2){
if (sortby==1){sortvar<-"diff"} else if (sortby==2) {sortvar<-"time1"}else if (sortby==3) {sortvar="tribe"} 
if(sortdir=="d"){srt<-paste0("desc(", sortvar,")")} else if (sortdir=="a") {srt<-sortvar}


#create df of unique values by tribe, time, measure of interest, drop NA, sort by t1 or t2-t1 diff

uniqueFIPS <- merged_data_record_all_long  %>% distinct_("tribe","time",.keep_all=TRUE) %>% mutate(time=gsub(" ","",time)) %>%
  select_("tribe","time",measure) %>% spread_("time",measure) %>% drop_na() %>% mutate(diff = time2-time1) %>% arrange(!!parse_expr(srt))

print(uniqueFIPS)
uniqueFIPS$tribe<-factor(uniqueFIPS$tribe,levels=uniqueFIPS$tribe[order(unlist(uniqueFIPS[sortvar]))]) #factorize for sorting
uniqueFIPS$tribe_label<-str_wrap(uniqueFIPS$tribe, width=10) #create text wrapped variable
uniqueFIPS$tribe_label<-factor(uniqueFIPS$tribe_label,levels=uniqueFIPS$tribe_label[order(unlist(uniqueFIPS[sortvar]))])  #factorize for sorting
# ymax<-max(uniqueFIPS$time1, na.rm=TRUE) +.25*sd(uniqueFIPS$time1,na.rm=TRUE) #calculate ymax based on t1 - max value + .25 x standard deviation
uniqueFIPS<-uniqueFIPS[ntribes1:ntribes2,] #take first 25

return(as.vector(uniqueFIPS$tribe))
}
###MAP FUNCTION####

createmap<-function(t1_c_type,t2_c_type,z,selected_tribe){
  
  tr_plotlist<-list()
  for(t in selected_tribe){
    tribe_recs<-subset(merged_data_record_all_long, tribe==t) 
    tribe_recs<-tribe_recs %>% select("ID","tribe","FIPS","time","lon","lat","avg_lat","avg_lon","mid_lat","mid_lon")
    tribe_recs_t1<-subset(tribe_recs, time=="time 1")
    tribe_recs_t2<-subset(tribe_recs, time=="time 2")
    
    # zoomf<-calc_zoom(lon,lat,data=tribe_recs, f=0.001)
    zoomf<-6
   
   
    jf_map_t1<-subset(county_map, FIPS %in% unique(tribe_recs_t1$FIPS) & !(FIPS %in% unique(tribe_recs_t2$FIPS)))
    jf_map_t2<-subset(county_map, FIPS %in% unique(tribe_recs_t2$FIPS))
    jf_map_t1$time<-"time1"
    jf_map_t2$time<-"time2"
    jf_map<-bind_rows(jf_map_t1,jf_map_t2)
    jf_map<-subset(jf_map, FIPS !="000NA") #remove NA's
    jf_map<-jf_map %>% arrange(group,order) #sort 
    # jf_map$lon<-as.numeric(jf_map$lon)
    # jf_map$lat<-as.numeric(jf_map$lat)
  

     
      # tribe_df<-tribe_df %>% arrange(group,order) #rorder by group and order value for proper fill rendering
      
      states<-unique(unlist(jf_map$scode)) #select states for outlining
      states_df<-subset(county_map,scode %in% states) #states df
      
      ########## Create options for Zoom ######################
      xlim_mid =  (min(jf_map$cm_long)+max(jf_map$cm_long))/2
      ylim_mid =  (min(jf_map$cm_lat)+max(jf_map$cm_lat))/2
      zoom<-list()
      zoom[[1]]<-coord_fixed(xlim = c(min(jf_map$cm_long)-1, max(jf_map$cm_long)+1),  ylim = c(min(jf_map$cm_lat)-1, max(jf_map$cm_lat)+1), ratio = 1.3)
      zoom[[2]]<-coord_fixed(xlim = c(xlim_mid-zoomf*3,xlim_mid+zoomf*3),  ylim = c(ylim_mid-zoomf*3, ylim_mid+zoomf), ratio = 1.3)
      zoom[[3]]<-coord_fixed(xlim = c(-124.5129896, -95.3209668), ratio = 1.3)
      zoom[[4]]<-coord_fixed(xlim = c(-95.3209668, -66.8531988), ratio = 1.3)
      zoom[[5]]<-coord_fixed(ratio = 1.3)
      
      
      
      # zoom[[5]]<-coord_fixed(xlim = c(median(jf_map$cm_long)-zoomf, median(jf_map$cm_long)+zoomf*3),  ylim = c(median(jf_map$cm_lat)-zoomf, median(jf_map$cm_lat)+zoomf), ratio = 1.3)
      
     
       ####create migration line df
      tribe_df_unique<-subset(tribe_recs,select=c("ID","lon","lat","avg_lat","avg_lon",'mid_lat','mid_lon',"time"))
     
      #   #t1 df
      tribe_df_unique_t1<-tribe_df_unique %>% filter(time=="time 1") %>%
        rename(lat1=lat,lon1=lon,avg_lat_t1=avg_lat, avg_lon_t1=avg_lon,mid_lat_t1=mid_lat, mid_lon_t1=mid_lon) %>%
        select(ID,as.character(coord_picker_t1[1,t1_c_type]),as.character(coord_picker_t1[2,t1_c_type])) %>% arrange(ID)
      # 
      #   #t2 df
      tribe_df_unique_t2<-tribe_df_unique %>% filter(time=="time 2") %>%
        rename(lat2=lat,lon2=lon,avg_lat_t2=avg_lat, avg_lon_t2=avg_lon,mid_lat_t2=mid_lat, mid_lon_t2=mid_lon) %>%
        select(ID,as.character(coord_picker_t2[1,t2_c_type]),as.character(coord_picker_t2[2,t2_c_type])) %>% arrange(ID)
      
      # #merge t1 and t2 df
      tribe_df_mig<-merge(tribe_df_unique_t1,tribe_df_unique_t2,by="ID") %>% arrange(ID) %>% select(-ID)
     
      # #get unique combinations of lat, long, remove the ones where lat1==lat2 and long1==long2
      tribe_df_mig<-unique(tribe_df_mig)
      tribe_df_mig<-tribe_df_mig[!((tribe_df_mig[,1]==tribe_df_mig[,3]) & (tribe_df_mig[,2]==tribe_df_mig[,4])),]
      # 
      colnames(tribe_df_mig)<-c("l_lat1","l_lon1","l_lat2","l_lon2") #rename columns to use w/ coordinate type picker
      
      ####Map
      
      
      maptheme<-theme(
                      plot.title = element_text(size = 10, face = "bold",hjust=.5),
                      # line = element_blank(), #remove axes
                      axis.text=element_blank(),#remove axis labels
                      axis.ticks=element_blank(),
                      axis.title=element_blank(),
                      legend.position = "bottom",
                      legend.text = element_text(size=10)
                      
                      ) #remove title
      #background plot
      usa<-ggplot()+geom_polygon(data=county_map, aes(x=cm_long, y=cm_lat,group=group),fill="lightgrey",colour="white",size=.2)+
       theme_bw()+maptheme 
     
      #add state outline
      usa_tribe_states<-usa+geom_polygon(data=states_df, aes(x=cm_long, y=cm_lat,group=group),fill="lightgrey",colour="steelblue",size=.2)
      
      #add color to T1 and T2 counties
      tribe_df_map<-usa_tribe_states+geom_polygon(data=jf_map, aes(x=cm_long, y=cm_lat, fill=time,group=group)) +
                      scale_fill_manual(values=c("#7dce94","#cd5554"),labels=c("Time 1 ", "Time 2"),name="Time")+
                      zoom[[z]]+ggtitle(t)
         
      # add migration lines
    
      nlines<-nrow(tribe_df_mig)
      opacity<-ifelse(nlines>1000, .01, ifelse(nlines<1000 & nlines>500, 0.05, .25))
      
      
       tribe_df_mig$label<-"Migration from Time 1 to Time 2"
       tribe_df_map <- tribe_df_map + geom_curve(data = tribe_df_mig,aes(x = l_lon1, y = l_lat1, xend = l_lon2, yend = l_lat2, color=label),
                                                  alpha=opacity,size = 1,curvature = -0.2,arrow = arrow(type="closed",angle=5,length = unit(0.1, "inches")))+
       theme(legend.title=element_blank())

      # }
      
      relh<-vector()
      if(t==selected_tribe[length(selected_tribe)]){ #set number of rows and columns for display
        n<-length(selected_tribe)
        rows<-n/4
        extra<-n%%4
        rows<-ifelse(extra>0, ceiling(rows)+1,rows+1)
        lgnd<-get_legend(tribe_df_map)
        relh[1:rows]<-1
        relh[length(relh)]<-.5
        cols<-ifelse(n<4, n,4)
      } 
     
        tr_plotlist[[t]]<-tribe_df_map+theme(legend.position="none")
     
  }
  
  #plot out as grid using cowplot, and add the legend via draw_grob
  plot_grid(plotlist = tr_plotlist, ncol=cols, nrow = rows, rel_heights = relh)+draw_grob(lgnd,x = 0, y = -0.45, width = 1, height = 1, scale = 1)
  
  
}



