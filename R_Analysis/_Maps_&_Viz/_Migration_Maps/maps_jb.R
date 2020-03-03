
####SELECT TRIBE FUNCTION#####

# "n_unique_FIPS",sortdir="d",sortby=1
selectTribes<-function(measure,sortdir,sortby,ntribes1,ntribes2){
  if (sortby==1){sortvar<-"diff"} else if (sortby==2) {sortvar<-"time1"}  else if (sortby==3) {sortvar<-"time2"} else if (sortby==4) {sortvar="tribe"} 
  if(sortdir=="d"){srt<-paste0("desc(", sortvar,")")} else if (sortdir=="a") {srt<-sortvar}
  
  
  #create df of unique values by tribe, time, measure of interest, drop NA, sort by t1 or t2-t1 diff
  
  uniqueFIPS <- merged_data_record_all_long  %>% distinct_("tribe","time",.keep_all=TRUE) %>% mutate(time=gsub(" ","",time)) %>%
    select_("tribe","time",measure) %>% spread_("time",measure) %>% drop_na() %>% mutate(diff = time2-time1) %>% arrange(!!parse_expr(srt))
  
  
  uniqueFIPS$tribe<-factor(uniqueFIPS$tribe,levels=uniqueFIPS$tribe[order(unlist(uniqueFIPS[sortvar]))]) #factorize for sorting
  uniqueFIPS$tribe_label<-str_wrap(uniqueFIPS$tribe, width=10) #create text wrapped variable
  uniqueFIPS$tribe_label<-factor(uniqueFIPS$tribe_label,levels=uniqueFIPS$tribe_label[order(unlist(uniqueFIPS[sortvar]))])  #factorize for sorting
  # ymax<-max(uniqueFIPS$time1, na.rm=TRUE) +.25*sd(uniqueFIPS$time1,na.rm=TRUE) #calculate ymax based on t1 - max value + .25 x standard deviation
  uniqueFIPS<-uniqueFIPS[ntribes1:ntribes2,] #take first 25
  
  return(as.vector(uniqueFIPS$tribe))
}
###MAP FUNCTION####

create_map <- function(selected_tribe,z,legend.tf=T){
  #Args:
    #selected_tribe: character vector of string names
    #z: zoom level with several defitions below
  
  #Required packages
  require(sf)
  require(tigris)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(stringr)
  require(cowplot)
  
  ######################################
  #Error checking
  
  #Is tribe name valid
  if(!any(merged_data_record_all_long$tribe %in% selected_tribe)){
    stop(str_c(selected_tribe," is not a valid tribe name.  Check spelling."))
    
  }


  ######################################
  #Setting up county spatial data from Tigris
  us_co <- counties(resolution = "500k", class = "sf") %>% 
     dplyr::filter(!(STATEFP %in% c("15","02","72"))) %>% # Remove AK, HI, PR
     st_transform(4326) #Arbitrary lat and lon projection

  co_cent <- suppressWarnings(st_centroid(us_co)) 
  
  centroids <- us_co %>%
    select(geoid) %>%
    st_set_geometry(NULL) %>%
    mutate(lon=st_coordinates(co_cent)[,1],
           lat=st_coordinates(co_cent)[,2])
  
  ###############################################
   #Setup tribal data and make spatial
   tribe.records <- merged_data_record_all_long %>%
     dplyr::filter(tribe %in% selected_tribe) %>%
     select(tribe, FIPS, time) %>%
     ungroup() %>%
     distinct() 
   
   to.plot <- tribe.records %>%
     group_by(FIPS) %>%
     summarise(n=n()) %>%
     left_join(tribe.records,.) %>%
     mutate(time=ifelse(n==2,"both",time),
            time=factor(time,levels = c("time 1","time 2","both"))) %>%
     select(-n) %>%
     distinct()
   
   
   to.plot <- to.plot %>%  
     inner_join(us_co %>% select(geoid),
                .,
                by = c("geoid"="FIPS"))
   
   ###################
   #Use expand.grid for all combinations of counties
   
   curve.coords <- expand_grid(t1=tribe.records$FIPS[tribe.records$time=="time 1"],
                               t2=tribe.records$FIPS[tribe.records$time=="time 2"]) %>%
     dplyr::filter(t1!=t2) %>%   #remove self-intersections
     left_join(.,
               centroids %>% rename(t1_lat=lat,t1_lon=lon),
               by=c("t1"="geoid")) %>%
     left_join(.,
               centroids %>% rename(t2_lat=lat,t2_lon=lon),
               by=c("t2"="geoid"))
   
   ########## Create options for Zoom ######################
   x.box <- st_bbox(to.plot)[c(1,3)]
   y.box <- st_bbox(to.plot)[c(2,4)]
   xlim_mid =  mean(x.box)
   ylim_mid =  mean(y.box)
   
   zoomf<-5
   zoom<-list()
   
   zoom[[1]] <- coord_sf(xlim = x.box, ylim = y.box)
   zoom[[2]] <- coord_sf(xlim = c(xlim_mid-zoomf*3,xlim_mid+zoomf*3),  ylim = c(ylim_mid-zoomf*3, ylim_mid+zoomf))
   zoom[[3]] <- coord_sf(xlim = c(-124.5129896, -95.3209668))
   zoom[[4]] <- coord_sf(xlim = c(-95.3209668, -66.8531988))
   zoom[[5]] <- coord_sf()
   

   
   #########################################################
   #Mapping
   #########################################################
   #Theme options
   maptheme <- theme(
     plot.title = element_text(size = 10, face = "bold",hjust=.5),
     # line = element_blank(), #remove axes
     axis.text=element_blank(),#remove axis labels
     axis.ticks=element_blank(),
     axis.title=element_blank(),
     legend.position = "bottom",
     legend.text = element_text(size=10)
   )
   
   ##########################################################
   base.map <- ggplot() +
     geom_sf(data = us_co,fill="black",colour="darkgrey",size=.06) +
     theme_bw() +
     maptheme
   
   #Color historical and present-day lands
   lands <- base.map +
     geom_sf(data = to.plot,aes(fill=time),colour="darkgrey") +
     zoom[[z]] +
     scale_fill_manual(values=c("#ffff00","#ff0000","orange"),
                       labels=c(" Historical Areas ", " Present-day Areas", " Historical and Present-day Areas "),
                       name="") +
     ggtitle(selected_tribe) 
   
   
   
   #Defining dynamic opacity
   nlines<-nrow(curve.coords)
   if(nlines>0){
     opacity<-ifelse(nlines>1000, .08, ifelse(nlines<1000 & nlines>500, 0.35, .7))
   }
     
   lands.lines <- lands +
       geom_curve(data = curve.coords,
                  aes(x = t1_lon, y = t1_lat, xend = t2_lon, yend = t2_lat,color="red"),
                  size = .3,curvature = -0.4,arrow = arrow(type="closed",angle=5,length = unit(0, "inches"))) +
       scale_color_manual(name="",values = alpha("red",opacity),labels = "Migration Flows from Historical to Present-day") 
       #guides(color = F)  #to remove Migration label

   
   
   if(legend.tf==F){
     #Extract legend
     lgnd <- get_legend(lands.lines)
     
     #Remove legend from ggplot object
     lands.lines <- lands.lines + theme(legend.position="none")
     
     return(list(lands.lines,lgnd))
   } else {
     return(list(lands.lines))
   }
   
   
}


