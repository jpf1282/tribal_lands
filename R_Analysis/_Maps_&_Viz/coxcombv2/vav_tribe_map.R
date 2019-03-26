library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
setwd("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps_&_Viz/")
load("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/_Maps/county_FIPS.RData")
load("~/Dropbox/__Papers_in_Progress/_Indian_Removal/_Data_and_R/x_Github/tribal_lands/R_Analysis/processed_data.RData")


head(merged_data_record_all_long)
head(county_map)
county_map<-county_map %>% rename(cm_long=long, cm_lat = lat)


jf_map<-merge(merged_data_record_all_long,county_map,by="FIPS") #merge JF file w/ county map
jf_map<-subset(jf_map, FIPS !="000NA") #remove NA's
jf_map<-jf_map %>% arrange(group,order) #sort 
jf_map$lon<-as.numeric(jf_map$lon)
jf_map$lat<-as.numeric(jf_map$lat)



##########Create coordinate-type picker reference dfs for T1 and T2 ###########
coord_all1<-c("lat1","lon1")
coord_ave1<-c("avg_lat_t1","avg_lon_t1")
coord_mid1<-c("mid_lat_t1","mid_lon_t1")
coord_all2<-c("lat2","lon2")
coord_ave2<-c("avg_lat_t2","avg_lon_t2")
coord_mid2<-c("mid_lat_t2","mid_lon_t2")

coord_picker_t1<-data.frame(coord_all1,coord_ave1,coord_mid1)
coord_picker_t2<-data.frame(coord_all2,coord_ave2,coord_mid2)



###MAP FUNCTION####

createmap<-function(t1_c_type,t2_c_type,z,selected_tribe){
      tribe_df<-subset(jf_map, tribe %in% selected_tribe) #subset selected tribe(s)
      tribe_df<-tribe_df %>% arrange(group,order) #rorder by group and order value for proper fill rendering
      
      states<-unique(unlist(tribe_df$scode)) #select states for outlining
      states_df<-subset(county_map,scode %in% states) #states df
      
      ########## Create options for Zoom ######################
      zoom<-list()
      zoom[[1]]<-coord_fixed(xlim = c(min(tribe_df$lon)-1, max(tribe_df$lon)+1),  ylim = c(min(tribe_df$lat)-1, max(tribe_df$lat)+1), ratio = 1.3)
      zoom[[2]]<-coord_fixed(xlim = c(-124.5129896, -95.3209668), ratio = 1.3)
      zoom[[3]]<-coord_fixed(xlim = c(-95.3209668, -66.8531988), ratio = 1.3)
      zoom[[4]]<-coord_fixed(ratio = 1.3)
     
       ####create migration line df
      tribe_df_unique<-unique(subset(tribe_df,select=c("ID","lon","lat","avg_lat","avg_lon",'mid_lat','mid_lon',"time")))
      
        #t1 df
      tribe_df_unique_t1<-tribe_df_unique %>% filter(time=="time 1") %>%  
        rename(lat1=lat,lon1=lon,avg_lat_t1=avg_lat, avg_lon_t1=avg_lon,mid_lat_t1=mid_lat, mid_lon_t1=mid_lon) %>% 
        select(ID,as.character(coord_picker_t1[1,t1_c_type]),as.character(coord_picker_t1[2,t1_c_type])) %>% arrange(ID)
       
        #t2 df
      tribe_df_unique_t2<-tribe_df_unique %>% filter(time=="time 2") %>% 
        rename(lat2=lat,lon2=lon,avg_lat_t2=avg_lat, avg_lon_t2=avg_lon,mid_lat_t2=mid_lat, mid_lon_t2=mid_lon) %>% 
        select(ID,as.character(coord_picker_t2[1,t2_c_type]),as.character(coord_picker_t2[2,t2_c_type])) %>% arrange(ID)
        
      #merge t1 and t2 df
      tribe_df_mig<-merge(tribe_df_unique_t1,tribe_df_unique_t2,by="ID") %>% arrange(ID) %>% select(-ID)
      #get unique combinations of lat, long, remove the ones where lat1==lat2 and long1==long2
      tribe_df_mig<-unique(tribe_df_mig)
      tribe_df_mig<-tribe_df_mig[!((tribe_df_mig[,1]==tribe_df_mig[,3]) & (tribe_df_mig[,2]==tribe_df_mig[,4])),]
      
      colnames(tribe_df_mig)<-c("l_lat1","l_lon1","l_lat2","l_lon2") #rename columns to use w/ coordinate type picker
      
      ####Map
      maptheme<-theme(panel.background=element_blank(), #remove background grey
                      line = element_blank(), #remove axes
                      text = element_blank(), #remove axis labels
                      title = element_blank()) #remove title
      #background plot
      usa<-ggplot()+geom_polygon(data=county_map, aes(x=cm_long, y=cm_lat,group=group),fill="lightgrey",colour="white",size=.2)+
       guides(fill=FALSE)
     
      #add state outline
      usa_tribe_states<-usa+geom_polygon(data=states_df, aes(x=cm_long, y=cm_lat,group=group),fill="lightgrey",colour="black",size=.2)
      
      #add color to T1 and T2 counties
      tribe_df_map<-usa_tribe_states+geom_polygon(data=tribe_df, aes(x=cm_long, y=cm_lat, fill=time,group=group)) +
                      scale_fill_manual(values=c("#7dce94","#cd5554"))+
                      zoom[[z]]+guides(fill=FALSE)+
                      maptheme
      
      #add migration lines
      for (i in 1:nrow(tribe_df_mig)){
      # for (i in 1:2){
       lines <- tribe_df_mig[i,]
        tribe_df_map <- tribe_df_map + geom_curve(data = lines,aes(x = l_lon1, y = l_lat1, xend = l_lon2, yend = l_lat2),color="darkorange",alpha=.25,size = 1,curvature = -0.2,arrow = arrow(type="closed",angle=5,length = unit(0.1, "inches")))+maptheme
      }
      tribe_df_map
}

##########Call map function
# t[1 or 2]_c_type: 
  #1: (lat#, lon#)
  #2: (avg_lat_#, avg_lon_# )
  #3: (mid_lat_#, mid_lat_#) 

#z (zoom)
  #1 Zoomed in
  #2 Western half of U.S.
  #3 Eastern half of U.S.
  #4 All U.S.

createmap(t1_c_type=1,t2_c_type=3,z=1,c("Cherokee")) #one tribe
createmap(t1_c_type=1,t2_c_type=1,z=4,c("Umpqua", "Modoc")) #two tribes
createmap(t1_c_type=1,t2_c_type=2,z=4,c("Chippewa")) #t2 set to avg
createmap(t1_c_type=1,t2_c_type=3,z=1,c("Comanche")) #t2 set to mid

