
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

create_map <- function(selected_tribe,z){  #legend.tf=T
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
  # us_co <- counties(resolution = "500k", class = "sf",refresh=F) %>% 
  #    dplyr::filter(!(STATEFP %in% c("15","02","72"))) %>% # Remove AK, HI, PR
  #   rename_all(str_to_lower) %>%
  #    st_transform(4326) #Arbitrary lat and lon projection
  
  us_co <- get(load("us_counties.Rdata")) %>%
    select(geoid) 
  
  if(z==1){
    us_co <- us_co %>%
    st_transform(5070)
  } 
  if(z==2){
    us_co <- us_co %>%
      st_transform(4326)
  }

  co_cent <- suppressWarnings(st_centroid(us_co)) 
  
  centroids <- us_co  %>%
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
   bbox.poly <- st_bbox(to.plot)
   xlim_mid <-  mean(bbox.poly[c(1,3)])
   ylim_mid <-  mean(bbox.poly[c(2,4)])
   
   bbox.all <- st_bbox(us_co)
   x.box <- c(bbox.all[1],bbox.all[3])
   y.box <- c(bbox.all[2],bbox.all[4])
   # x.mid <- mean(bbox.all[c(1,3)])
   # y.mid <- mean(bbox.all[c(2,4)])
   # new.dist <- (bbox.all[3]-x.mid)
   
   zoomf <- 5
   zoom <- list()
   
   # zoom[[1]] <- coord_sf(xlim = c(x.mid-new.dist,x.mid+new.dist), 
   #                       ylim = c(y.mid-new.dist,y.mid+new.dist))
   zoom[[1]] <- coord_sf(xlim = x.box, 
                         ylim = y.box)
   zoom[[2]] <- coord_sf(xlim = c(xlim_mid-15,xlim_mid+15),  
                         ylim = c(ylim_mid-9, ylim_mid+6))

   
   
   #########################################################
   #Mapping
   #########################################################
   #Theme options
   maptheme <- theme(
     plot.title = element_text(size = 10, face = "bold",hjust=.5),
     axis.text=element_blank(),#remove axis labels
     axis.ticks=element_blank(),
     axis.title=element_blank(),
     legend.position = "bottom", 
     legend.box="vertical", 
     legend.margin=margin(),
     legend.text = element_text(size=10)
   )
   
   ##########################################################
   base.map <- ggplot() +
     geom_sf(data = us_co,fill="black",colour="gray28",size=.06) +
     theme_bw() +
     maptheme
   
   #Color historical and present-day lands
   lands <- base.map +
     geom_sf(data = to.plot,aes(fill=time),colour=NA) +
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
                  aes(x = t1_lon, y = t1_lat, xend = t2_lon, yend = t2_lat,color="#ff0000"),alpha=opacity, 
                  size = .1,curvature = -0.4,arrow = arrow(type="closed",angle=5,length = unit(0, "inches"))) 

   lands.lines <- lands.lines + theme(legend.position="none")
   
   return(lands.lines)
   
}


#Function to create legend for all plots
create_legend <- function(file_name="map_legend"){  
  
  #Required packages
  require(sf)
  require(tigris)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(stringr)
  require(cowplot)
  
  selected_tribe="Modoc"
  
  ######################################
  #Setting up county spatial data from Tigris
  us_co <- get(load("us_counties.Rdata")) %>%
    select(geoid) %>%
      st_transform(4326)

  
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
    legend.box="vertical", 
    legend.margin=margin(),
    legend.text = element_text(size=10)
  )
  
  ##########################################################
  base.map <- ggplot() +
    geom_sf(data = us_co,fill="black",colour="gray28",size=.06) +
    theme_bw() +
    maptheme
  
  #Color historical and present-day lands
  lands <- base.map +
    geom_sf(data = to.plot,aes(fill=time),colour=NA) +
    scale_fill_manual(values=c("#ffff00","#ff0000","orange"),
                      labels=c(" Historical Areas ", " Present-day Areas", " Historical and Present-day Areas "),
                      name="") +
    ggtitle(selected_tribe) 
  
  lands.lines <- lands +
    geom_curve(data = curve.coords,
               aes(x = t1_lon, y = t1_lat, xend = t2_lon, yend = t2_lat,color="#EE6363"),
               size = .1,curvature = -0.4,arrow = arrow(type="closed",angle=5,length = unit(0, "inches"))) +
  scale_color_manual(name="",values = "#EE6363",labels = "Migration Flows from Historical to Present-day") 
  
  lgnd <- get_legend(lands.lines)
  
  save(lgnd,file = str_c(file_name,".Rdata"))
  
  return(NULL)
  
}



##################################
#Create figure for ms
ms_figure <- function(){
  #Required packages
  require(dplyr)
  require(stringr)
  require(cowplot)
  require(conflicted)
  require(purrr)
  
  
  #Load legend - object called lgnd
  if(!file.exists("map_legend.Rdata")){
    create_legend()
  }
  lgnd <- get(load("map_legend.Rdata"))
  
  #Tribes for plot
  tribe_list <- c("Lenape","Modoc","Absentee-Shawnee","Kickapoo")
  
  #Construct individual maps
  tr_plotlist <- map(tribe_list,~create_map(.,z=2)) 
  
  #plot out as grid using cowplot, and add the legend via draw_grob
  to.print <- plot_grid(tr_plotlist[[1]],tr_plotlist[[2]],tr_plotlist[[3]],tr_plotlist[[4]],NULL,ncol = 2,nrow = 3,rel_heights = c(1,1,.35)) +
    draw_grob(lgnd,x = .3, y = 0.01, width = .4, height = .15)
  
  cowplot::ggsave(filename = "ms_fig.pdf",plot = to.print,width = 5.8,height = 4.8)
}


app_figures <- function(legend=F){
  #Required packages
  require(dplyr)
  require(stringr)
  require(cowplot)
  require(conflicted)
  require(purrr)
  
  #get list of all tribes and convert to list
  all.tribes <- unique(merged_data_record_all_long$tribe) 
  tribe.list <- split(all.tribes,sort(rep(c(1:67),length.out=length(all.tribes))))
  
  tribe.list <- tribe.list[1:3]
  map(tribe.list,function(x){
    tr_plotlist <- map(x,~create_map(.,1))
    
    message(str_c("Printing: ",str_c(x,collapse = ", ")))
    
    if(legend==F){
    #without legend
    print.noleg <- plot_grid(plotlist = tr_plotlist,ncol = 2,nrow = 3,rel_heights = c(1,1,1)) 
    cowplot::ggsave(filename = str_c("app_fig_",
                                     str_sub(x[1],1,5),"-",
                                     str_sub(x[length(x)],1,5),
                                     ".pdf"),
                    plot = print.noleg,width = 7,height = 9)
    } else {
    
    
    #with legend
    print.leg <- plot_grid(tr_plotlist[[1]],tr_plotlist[[2]],tr_plotlist[[3]],tr_plotlist[[4]],tr_plotlist[[5]],tr_plotlist[[6]],NULL,
                           ncol = 2,nrow = 4,rel_heights = c(1,1,1,.25)) +
      draw_grob(lgnd,x = .3, y = 0.01, width = .4, height = .1)
    cowplot::ggsave(filename = str_c("app_fig_",
                                     str_sub(x[1],1,5),"-",
                                     str_sub(x[length(x)],1,5),
                                     "_legend.pdf"),
                    plot = print.leg,width = 7,height = 9.5)
    }
  })
  
  
}
