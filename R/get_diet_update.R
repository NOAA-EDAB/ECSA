library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2);library(ggiraph)
library(profvis)

sss <- read.csv("data/seasonal_stock_strata.csv")
sixcode <- c('acared','alewif','amepla','atlcod','atlhal','atlher','atlmac','atlwol',
             'barska','blabas','bluefi','bluher','butter','cleska','haddoc',
             'litska','monkfh','ocpout','offhak','polloc','redhak','rosska',
             'scupzz','silhak','smodog','smoska','spidog','sumflo','thoska',
             'whihak','window','winflo','winska','witflo','yelflo','amlobs','joncra')
svspp_list <- c(155,33,102,73,101,32,121,192,22,141,
                135,34,131,24,74,26,197,193,69,
                75,77,25,143,72,13,27,15,103,
                28,76,108,106,23,107,105,301,312)
load("data/allfhsg.RData")


get_diet <- function(species_code){
  #profvis({
  #Get data, manipulate, set constants
  
  sss$season <- toupper(sss$season)
  sss$stratum <- sss$strata
  sss <- sss %>% mutate(sp, svspp = plyr::mapvalues(sp, from = sixcode, to = svspp_list))
  sssin <- sss %>% filter(svspp == species_code)
  allfh <- allfhsg
  names(allfh) <- tolower(names(allfh))
  
  #Select grouping variables
  nbyvar <- 4
  
  allfh$byvar1 <- allfh$year
  name_byvar1 <- 'year'
  
  allfh$byvar2 <- allfh$season
  name_byvar2 <- 'season'
  
  allfh$byvar3 <- 'x'
  name_byvar3 <- 'x'
  
  allfh$byvar4 <- 'x'
  
  #Grouping variable names
  name_byvar4=c('x')
  name_byvar1=c('year')
  name_byvar2=c('season')
  name_byvar3=c(NA)
  name_byvar4=c('x')

  #Filter out extras and by seasonal strata
  allsum <- allfh %>%
    filter(pynam != 'BLOWN', pynam != 'PRESERVED', pynam != ' ', purcode == 10, svspp == species_code) %>%
    filter((season == "FALL" & stratum %in% sssin$stratum[sssin$season=='FALL'])  |
             (season == "SPRING" & stratum %in% sssin$stratum[sssin$season == 'SPRING']) |
             (season == "WINTER"& stratum %in% sssin$stratum[sssin$season == 'WINTER']))
  
  #This chunk is for n stomachs ONLY-----------------------------------------------------------------------------------
  #if (nstom){
        #Sum pyamtw and get other summary statistics
        # allsuma <- allsum %>% group_by(byvar1, byvar2, byvar3, byvar4, svspp, cruise, station, pdid, pdsex, pdlen) %>%
        #   dplyr::summarise(totwt = sum(pyamtw)) %>% na.omit() %>%
        #   group_by(byvar1, byvar2, byvar3, byvar4, svspp) %>%
        #   dplyr::summarise(nstom = n(), meanstom = mean(totwt, na.rm = T),
        #                    varstom = var(totwt, na.rm = T), meanlen = mean(pdlen, na.rm = T),
        #                    numlen = n(), minstom = min(totwt, na.rm = T),
        #                    maxstom = max(totwt,na.rm = T), minlen = min(pdlen, na.rm = T),
        #                    maxlen = max(pdlen, na.rm = T))
        # #Sum
        # allsumb <- allsum %>% group_by(byvar1, byvar2, byvar3, byvar4, svspp, cruise, station, pdid, pdsex, pdwgt, pdlen) %>%
        #   dplyr::summarise(totwt = sum(pyamtw, na.rm = T)) %>% na.omit() %>%
        #   group_by(byvar1, byvar2, byvar3, byvar4, svspp) %>%
        #     dplyr::summarise(meanwgt = mean(pdwgt, na.rm = T),
        #                      numwgt = n(),
        #                      minwgt = suppressWarnings(min(pdwgt, na.rm = T)),
        #                      maxwgt = suppressWarnings(max(pdwgt, na.rm = T)))
        # 
        # stomstats2 <- allsuma %>% left_join(.,allsumb, by = c("byvar1","byvar2","byvar3","byvar4","svspp")) %>%
        #                          mutate(std.error = sqrt(varstom/nstom))
        # 
        # #Drop columns where all values == "x"
        # stomstats2[sapply(stomstats2, function(x) all(x=='x'))] <- NULL
        # 
        # #stomstats with nstom, saving svspp, year, season, nstom
        # nstom <- stomstats2 %>% ungroup() %>% dplyr::rename(season = byvar2,year = byvar1) %>%
        #   dplyr::select(svspp, year, season, nstom)
  #}
  #----------------------------------------------------------------------------------------------------------#
  
  #----------------------------------Start weighted diet proportion code-------------------------------------#
  allfh1=allsum
  allfh1$taxnam=allfh1$collsci
  
  #Define svspp
  pred=allfh1$svspp%in%species_code
  
  #Get data
  allfh1$tax <- allfh1$taxnam
  allfh1$perpy <- allfh1$perpyw
  allfh1$pyamt <- allfh1$pyamtw
  
  #pyamt is the variable for prey weight
  allfh2 <- allfh1 %>% dplyr::select(c(cruise6, stratum, station, byvar1,byvar2, byvar3, byvar4, svspp,
                                       pdid, pdsex, pdlen, tax, pyamt, catnum, numlen, tot_catnum_stratum,
                                       tot_catwgt_stratum, tot_tows_spp_stratum, stratum_area))
  
  allfh <- allfh2 %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdsex, pdlen) %>%
    dplyr::summarise(xxxx = n()) %>%
    group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdsex, pdlen) %>%
    dplyr::summarise(numlen2 = n()) %>%
    merge(allfh2,. , by=c('cruise6', 'station','byvar1', 'byvar2', 'byvar3', 'byvar4',  'svspp', 'pdsex', 'pdlen'), all.x=T) %>% 
    mutate(numlen_fin = ifelse(numlen < numlen2, numlen2, ifelse(is.na(numlen), numlen, numlen))) %>%
    dplyr::select(c(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdsex, pdlen, tax, pyamt, 
                    catnum, tot_catnum_stratum, tot_catwgt_stratum, tot_tows_spp_stratum, stratum_area, numlen, numlen_fin))
  

  allfh2 <- allfh %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdsex, pdlen, numlen_fin) %>%
    dplyr::summarise(dummy_var = sum(numlen, na.rm = T)) %>% na.omit() %>%
    group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdlen) %>%
    dplyr::summarise(rnumlen_fin = sum(numlen_fin)) %>%
    merge(allfh, ., by=c('cruise6', 'station', 'byvar1',
                         'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdlen'), all.x=T ) %>%
    mutate(rnumlen_fin = ifelse(allfh$svspp!=13 | allfh$svspp!=15, allfh$numlen_fin, allfh$rnumlen_fin)) %>%
    group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdsex, catnum) %>%
    dplyr::summarise(dummy_var = sum(numlen)) %>% na.omit()
  
  
  allfh <- allfh2 %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp) %>%
    dplyr::summarise(rcatnum = sum(catnum)) %>% na.omit() %>%
    merge(allfh,. , by = c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x = T) %>%
    mutate(rcatnum = ifelse(svspp != 13 | svspp != 15, catnum, rcatnum))
  
  allfh_catstratum <- allfh %>% group_by(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp, pdsex, tot_tows_spp_stratum,
                                         tot_catnum_stratum,tot_catwgt_stratum) %>% dplyr::summarise(dum_var = sum(rcatnum)) %>% na.omit()
  
  ####################################################################################################################################
  
  max_tot_tows_spp_stratum <- allfh_catstratum %>% group_by(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp) %>% 
    dplyr::summarise(tot_tows_spp_stratum = max(tot_tows_spp_stratum, na.rm = T))

  allfh_rcatstratum <- allfh_catstratum %>% group_by(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp) %>%
    dplyr::summarise(rtot_catnum_stratum = sum(tot_catnum_stratum, na.rm = T),
                     rtot_catwgt_stratum = sum(tot_catwgt_stratum, na.rm = T)) %>% na.omit() %>%
    merge(., max_tot_tows_spp_stratum, by=c('cruise6', 'stratum', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x=T) %>%
    mutate(rtot_tows_spp_stratum = tot_tows_spp_stratum) %>%
    dplyr::select(-c(tot_tows_spp_stratum))
  
  allfh <- allfh_rcatstratum %>% merge(allfh, ., by=c('cruise6', 'stratum', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x=T)

  #fix rcatstratum for svspp ne 015 and ne 013, also created rtot_tows_spp_stratum need to keep until for diet calc
  allfh$rtot_catnum_stratum=ifelse(allfh$svspp!=13|allfh$svspp!=15, allfh$tot_catnum_stratum, allfh$rtot_catnum_stratum)
  allfh$rtot_catwgt_stratum=ifelse(allfh$svspp!=13|allfh$svspp!=15, allfh$tot_catwgt_stratum, allfh$rtot_catwgt_stratum)
  allfh$rtot_tows_spp_stratum=ifelse(allfh$svspp!=13|allfh$svspp!=15, allfh$tot_tows_spp_stratum, allfh$rtot_tows_spp_stratum)
  
  
  ####################################################################################################################################
  fish <- allfh %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdlen, tax) %>%
    dplyr::summarise(pysum = sum(pyamt, na.rm = T)) %>% na.omit() 
  
  pred <- fish %>% dplyr::select(cruise6, station, pdid, pysum, byvar1,  byvar2, byvar3, byvar4, svspp) %>%
    arrange(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid)
  
  
  new <- pred %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid) %>% 
     top_n(-1, wt = pysum) %>%
    group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp) %>% dplyr::summarise(nostom = n())
  
  pylist=data.frame(tax = unique(fish$tax),
                    pycode = paste('prey',1:length(unique(fish$tax)),sep=''))
  
  fish <- new %>% merge(fish,., by=c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp')) %>%
    merge(pylist, . , by=c('tax'), all.x=T)
  
  length2 <- allfh %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdlen) %>% 
    top_n(-1, wt = stratum_area) 
  
  
  missingvars <- length2 %>% group_by(cruise6, station, byvar1, byvar2, byvar3, byvar4,svspp, pdlen) %>%
    dplyr::summarise(rnumlen_fin = sum(numlen_fin, na.rm = T)) %>% na.omit()
  
  length3 <- length2 %>% merge(missingvars,.,by = c('cruise6', 'station', 'byvar1',
                                                    'byvar2','byvar3', 'byvar4','svspp','pdlen'), all.x = T)
  
  
  length2 <- length3 %>% dplyr::select(c(cruise6, station, stratum, byvar1, byvar2,
                                         byvar3, byvar4, svspp,  pdid, pdlen,
                                         rcatnum, rtot_catnum_stratum,
                                         tot_catwgt_stratum, rtot_catwgt_stratum,
                                         rtot_tows_spp_stratum, stratum_area, rnumlen_fin))
  
  
  #Working with fish data.frame----------------------------------------------------------------------------------#
  fish <- fish %>% dplyr::select(-c(tax)) %>% 
    merge(., length2, by=c('cruise6','station',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdid', 'pdlen'),
          all.x=T)    
  fish=data.table(fish)
  
  ####################################################################################################################################
  
  #transpose cast pycode data and merge table to fish

  #THIS KEEPS ALL OF THE DATA FROM 'FISH'
  # tran1=data.frame(dcast.data.table(fish,cruise6 + stratum + station +
  #                                     byvar1 + byvar2 + byvar3 + byvar4 +
  #                                     svspp + pdid + pdlen + catnum + rcatnum +
  #                                     tot_catnum_stratum + rtot_catnum_stratum +
  #                                     tot_catwgt_stratum + rtot_catwgt_stratum +
  #                                     tot_tows_spp_stratum + rtot_tows_spp_stratum +
  #                                     stratum_area + numlen+rnumlen_fin + nostom ~ pycode,
  #                                     fun.aggregate=median, value.var='pysum') )   #add byvars before svspp
  
  #THIS IS A STRIPPED DOWN VERSION
  tran1=data.frame(dcast.data.table(fish,cruise6 + stratum + station +
                                      byvar1 + byvar2 + byvar3 + byvar4 +
                                      svspp + rcatnum +
                                      rtot_catnum_stratum +
                                      rtot_catwgt_stratum +
                                      rtot_tows_spp_stratum +
                                      stratum_area + rnumlen_fin  ~ pycode,
                                      fun.aggregate=median, value.var='pysum') )   #add byvars before svspp
  
  #set prey NA to zero in fish
  preycol <- as.character(pylist$pycode)
  tran1[,preycol][is.na(tran1[,preycol])]=0
  
  tran1=data.table(tran1)
  #Am I missing something, or does this do not do anything?
  # new1=tran2[,lapply(.SD,mean, na.rm=T),by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdlen, catnum, rcatnum, tot_catnum_stratum, rtot_catnum_stratum, tot_catwgt_stratum,  #add byvars before svspp
  #                                               rtot_catwgt_stratum, tot_tows_spp_stratum, rtot_tows_spp_stratum, stratum_area, numlen, rnumlen_fin, nostom)]
  # #see here -> all.equal(tran2, new1)  
  #NA for many rnumlen_fin records??
  
  #remove NAs here
  new1rm <- tran1[!is.na(tran1$rnumlen_fin)]   

  new11 = new1rm[,lapply(.SD,  weighted.mean, 
                         rnumlen_fin, na.rm=TRUE), by=list(cruise6,stratum, station, byvar1,
                                                          byvar2, byvar3, byvar4, svspp,
                                                          rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum,
                                                          stratum_area)]
  
  #need to remove unnecessary columns
  #This is not working for me?
  # new11[,':='(pdid=NULL, pdlen=NULL ,catnum=NULL,
  #             tot_catnum_stratum=NULL, tot_catwgt_stratum=NULL,
  #             rtot_catwgt_stratum=NULL, tot_tows_spp_stratum=NULL,
  #             numlen=NULL, rnumlen_fin=NULL, nostom=NULL)]
  new2=new11[,lapply(.SD,mean, na.rm=T), by=list(cruise6, stratum, station, byvar1,
                                                 byvar2, byvar3, byvar4, svspp, rcatnum,
                                                 rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)]
  
  
  musw2=new11[,lapply(.SD,function(x) x*rcatnum),by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp,
                                                         rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp
  
  
  new2s=musw2[,lapply(.SD,sum ), by=list(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp,
                                         rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp
  
  
  musw_strat=new2s[,lapply(.SD, function(x) x/rtot_tows_spp_stratum),by=list(cruise6, stratum, byvar1, byvar2,
                                                                             byvar3, byvar4, svspp,rcatnum,
                                                                             rtot_catnum_stratum, rtot_tows_spp_stratum,
                                                                             stratum_area)] #add byvars before svspp
  
  musw_strat$munfish_strat = musw_strat$rtot_catnum_stratum / musw_strat$rtot_tows_spp_stratum
  
  #retain munfish_strat
  munfish_stratdat=musw_strat[, c('cruise6', 'stratum',  'byvar1', 'byvar2', 'byvar3', 'byvar4',
                                  'svspp', 'munfish_strat'), with=F] #add byvars before 
  
  musw_strat=musw_strat[order(musw_strat$svspp, musw_strat$byvar1,
                              musw_strat$byvar2,musw_strat$byvar3,musw_strat$byvar4),] #byvars AFTER svspp
  
  #weight by stratum area
  #first remove NA stratum areas
  musw_stratum=musw_strat[!is.na(musw_strat$stratum_area)]
  
  new3s=musw_stratum[,lapply(.SD, weighted.mean,stratum_area, na.rm=T), by=list(svspp,byvar1,byvar2,byvar3,byvar4)] #add byvars AFTER svspp   #msw_strat
  
  new3s[,':='(cruise6=NULL, stratum=NULL ,station=NULL, rcatnum=NULL, rtot_catnum_stratum=NULL, rtot_tows_spp_stratum=NULL, stratum_area=NULL)]
  
  #new3s$num_stra=NROW(musw_stratum) #fix for multiple svspp
  num_stra=musw_stratum[, .N,by=list(svspp,byvar1,byvar2,byvar3,byvar4)] #add byvars AFTER svspp
  names(num_stra)=gsub('N','num_stra',names(num_stra))
  
  new3s=merge(new3s, num_stra, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)  #add byvars AFTER svspp
  
  meansw_s=new3s[,lapply(.SD,function(x) x/munfish_strat),by=list(svspp,byvar1,byvar2,byvar3,byvar4)] #add byvars AFTER svspp 
  
  meansw_s[,':='(munfish_strat=NULL, num_stra=NULL)]
  
  #CHANGE labels to meansw_s by prey number
  
  three=new2[order(new2$svspp,new2$byvar1,new2$byvar2,new2$byvar3,new2$byvar4),] #add byvars AFTER svspp
  
  meansw=three[,lapply(.SD,weighted.mean, rcatnum, na.rm=T), by=list(svspp,byvar1,byvar2,byvar3,byvar4)] #add byvars AFTER svspp
  meansw[,':='(cruise6=NULL, stratum=NULL ,station=NULL, rcatnum=NULL, rtot_catnum_stratum=NULL, rtot_tows_spp_stratum=NULL, stratum_area=NULL)]
  
  
  #meansw$num_tows=NROW(three) fix for multiple svspp
  num_tows=three[, .N,by=list(svspp,byvar1,byvar2,byvar3,byvar4)] #add byvars AFTER svspp
  names(num_tows)=gsub('N','num_tows',names(num_tows))
  
  meansw=merge(meansw, num_tows, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)  #add byvars AFTER svspp
  
  #melt three to start prod
  master_musw=melt(three, id.vars=c("cruise6", "stratum", "station", "svspp",  "byvar1", "byvar2", "byvar3", "byvar4", "rcatnum", "rtot_catnum_stratum", "rtot_tows_spp_stratum", "stratum_area"),
                   measure.vars=preycol, variable.name='pycode', value.name='musw')
  
  master_tmsw_strat=melt(new2s, id.vars=c("cruise6", "stratum", "svspp", "byvar1", "byvar2", "byvar3", "byvar4"),
                         measure.vars=preycol, variable.name='pycode', value.name='tmsw_strat')
  
  master_musw2=melt(musw2, id.vars=c("cruise6", "stratum", "station", "svspp", "byvar1", "byvar2", "byvar3", "byvar4"),
                    measure.vars=preycol, variable.name='pycode', value.name='musw2')
  
  master_musw_strat=melt(musw_strat, id.vars=c("cruise6", "stratum",  "svspp", "byvar1", "byvar2", "byvar3", "byvar4" ),
                         measure.vars=preycol, variable.name='pycode', value.name='musw_strat')
  
  
  master_meansw=melt(meansw, id.vars=c("svspp", "byvar1", "byvar2", "byvar3", "byvar4"),     #add byvars AFTER svspp
                     measure.vars=preycol, variable.name='pycode', value.name='meansw')
  
  master_meansw_s=melt(meansw_s, id.vars=c("svspp", "byvar1", "byvar2", "byvar3", "byvar4"),   #add byvars AFTER svspp
                       measure.vars=preycol, variable.name='pycode', value.name='meansw_s')
  
  master_msw_strat=melt(new3s, id.vars=c("svspp", "byvar1", "byvar2", "byvar3", "byvar4"),   #add byvars AFTER svspp
                        measure.vars=preycol, variable.name='pycode', value.name='msw_strat')
  
  keep7=c('cruise6', 'stratum',  'svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'munfish_strat') #add byvars AFTER svspp
  master_munfish_strat=musw_strat[,keep7, with=F]
  
  num_stra_fish=new3s[,c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','munfish_strat', 'num_stra'),with=F] #add byvars AFTER svspp
  names(num_stra_fish)=gsub('munfish_strat','m_nfish_strat',names(num_stra_fish))
  
  master_num_tows=meansw[,c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','num_tows'), with=F] #add byvars AFTER svspp
  
  #merge masters
  #memory.size(4095)
  merge1=merge(master_musw, master_musw_strat, by=c('cruise6', 'stratum',  'svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode'), all.x=T)  #add byvars AFTER svspp
  merge2=merge(merge1, master_tmsw_strat, by=c('cruise6', 'stratum',  'svspp',   'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode'), all.x=T)   #add byvars AFTER svspp
  merge3=merge(merge2, master_munfish_strat, by=c('cruise6', 'stratum',  'svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)      #add byvars AFTER svspp
  merge4=merge(merge3, master_musw2, by=c('cruise6', 'stratum', 'station', 'svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)   #add byvars AFTER svspp
  merge5=merge(merge4, master_meansw, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)   #add byvars AFTER svspp
  merge6=merge(merge5, master_num_tows, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)          #add byvars AFTER svspp
  merge7=merge(merge6, master_meansw_s, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)  #add byvars AFTER svspp
  merge8=merge(merge7, num_stra_fish, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)           #add byvars AFTER svspp
  master=merge(merge8, master_msw_strat, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode'), all.x=T)  #add byvars AFTER svspp
  
  
  #prod    
  master$prod=master$rcatnum^2*((master$musw-master$meansw))^2
  master$prodf= (master$rcatnum-master$munfish_strat)^2
  master$prodd= (master$musw2-master$musw_strat)^2
  master$prod_cov=((master$rcatnum-master$munfish_strat)*(master$musw2-master$musw_strat))
  
  #master=master[order(master$svspp),] #add byvars AFTER svspp ##causes crash?
  
  
  #sprod   better way?  remove cols except svspp and pycode?
  sprod=master[,lapply(.SD, sum, na.rm=T), by=list(svspp,byvar1,byvar2,byvar3,byvar4, pycode)]#, num_tows, meansw, meansw_s, msw_strat, m_nfish_strat, num_stra)] #add byvars AFTER svspp
  keep8=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode', 'prod')#'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra', 'prod') #add byvars AFTER svspp
  sprod2=sprod[,keep8, with=F]
  names(sprod2)=gsub('prod','sprod',names(sprod2))
  
  #mprod and mnumfish
  mprod_mnumfish=master[,lapply(.SD, mean, na.rm=T), by=list(svspp, byvar1,byvar2,byvar3,byvar4, pycode, pycode)]#, num_tows, meansw, meansw_s, msw_strat, m_nfish_strat, num_stra)] #add byvars AFTER svspp
  keep9=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode', 'prod', 'rcatnum')#'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra', 'prod', 'rcatnum')        #add byvars AFTER svspp
  mprod_mnumfish2=mprod_mnumfish[,keep9,with=F]
  names(mprod_mnumfish2)=gsub('prod', 'mprod', names(mprod_mnumfish2))
  names(mprod_mnumfish2)=gsub('rcatnum', 'mnumfish', names(mprod_mnumfish2))
  
  new4=merge(sprod2, mprod_mnumfish2, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)#, 'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra'), all.x=T)
  #add byvars?
  
  sprodf_d_cov=master[,lapply(.SD, sum, na.rm=T), by=list(svspp, byvar1,byvar2,byvar3,byvar4, pycode, cruise6, stratum, stratum_area, rtot_tows_spp_stratum, m_nfish_strat)]      #add byvars after svspp
  keep10=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode', 'cruise6', 'stratum', 'stratum_area', 'rtot_tows_spp_stratum', 'm_nfish_strat', 'prodf', 'prodd', 'prod_cov')  #add byvars after svspp
  sprodf_d_cov2=sprodf_d_cov[,keep10, with=F]
  names(sprodf_d_cov2)=gsub('prodf', 'sprodf', names(sprodf_d_cov2))
  names(sprodf_d_cov2)=gsub('prodd', 'sprodd', names(sprodf_d_cov2))
  names(sprodf_d_cov2)=gsub('prod_cov', 'sprod_cov', names(sprodf_d_cov2))
  
  sprodf_d_cov2$dfntows_strat= sprodf_d_cov2$rtot_tows_spp_stratum-1
  sprodf_d_cov2$varprodf=ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprodf/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)
  sprodf_d_cov2$varprodd= ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprodd/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)
  sprodf_d_cov2$varprod_cov=ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprod_cov/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)
  
  new6=sprodf_d_cov2[order(sprodf_d_cov2$svspp,sprodf_d_cov2$byvar1,sprodf_d_cov2$byvar2,sprodf_d_cov2$byvar3,sprodf_d_cov2$byvar4),] #add byvars AFTER svspp
  
  #sumvarprod     some vars still zero?
  sumvarprod1=new6[,lapply(.SD,sum, na.rm=T), by=list(svspp, byvar1,byvar2,byvar3,byvar4,pycode)] #add byvars AFTER svspp
  keep11=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode', 'varprodf', 'varprodd', 'varprod_cov', 'stratum_area')   #add byvars AFTER svspp
  sumvarprod=sumvarprod1[,keep11, with=F]
  names(sumvarprod)=gsub('varprodf', 'svarprodf', names(sumvarprod))
  names(sumvarprod)=gsub('varprodd', 'svarprodd', names(sumvarprod))
  names(sumvarprod)=gsub('varprod_cov', 'svarprod_cov', names(sumvarprod))
  names(sumvarprod)=gsub('stratum_area', 'sstratum_area', names(sumvarprod))
  
  sumvarprod$varf=sumvarprod$svarprodf/sumvarprod$sstratum_area^2
  sumvarprod$vard=sumvarprod$svarprodd/sumvarprod$sstratum_area^2
  sumvarprod$var_cov=sumvarprod$svarprod_cov/sumvarprod$sstratum_area^2
  
  #new4_strat
  new4_strat=merge(new4, sumvarprod, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode'), all.x=T) #add byvars AFTER svspp
  
  #merge with  select master columns

  six1=merge(new4_strat, master_meansw, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)  #add byvars AFTER svspp
  six2= merge(six1, master_meansw_s, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'pycode'), all.x=T)      #add byvars AFTER svspp
  six3=merge(six2, master_msw_strat, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4','pycode'), all.x=T)     #add byvars AFTER svspp
  six4=merge(six3, master_num_tows, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T) #add byvars AFTER svspp
  six=merge(six4, num_stra_fish, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T) #add byvars AFTER svspp 
  
  
  six$variance=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0, 1/(six$num_tows*(six$mnumfish)^2)*(six$sprod/(six$num_tows-1)),0)
  six$cv=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0,((six$variance)^0.5)/six$meansw,0)
  six$var_s=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0, (six$meansw_s^2)*((six$varf/(six$m_nfish_strat^2))+(six$vard/(six$msw_strat^2))-(2*six$var_cov/six$m_nfish_strat/six$msw_strat)),0)  
  six$cv_s=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0,((six$var_s)^0.5)/six$meansw_s,0)
  
  #renamed meansw since used earlier
  meanswf=six[,':='(sprod=NULL, mprod=NULL, mnumfish=NULL, msw_strat=NULL, svarprodf=NULL, svarprodd=NULL, svarprod_cov=NULL, sstratum_area=NULL, varf=NULL, vard=NULL, var_cov=NULL)]
  
  #merge tax names back in
  output=merge(meanswf, pylist, by=c('pycode'), all.x=T)
  
  
  names(output)=gsub('tax', 'prey', names(output))
  
  totwt1=output[,lapply(.SD,sum, na.rm=T), by=list(svspp,byvar1,byvar2,byvar3,byvar4), .SDcols=c('meansw', 'meansw_s')] #add byvars AFTER svspp
  keep12=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'meansw', 'meansw_s')  #add byvars AFTER svspp
  totwt=totwt1[,keep12,with=F]
  names(totwt)=gsub('meansw', 'totwt', names(totwt))
  names(totwt)=gsub('meansw_s', 'totwt_s', names(totwt))
  
  final=merge(output, totwt, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)  #add byvars AFTER svspp
  
  final=final[,':='(pycode=NULL, m_nfish_strat=NULL)]
  
  final$relmsw=100*(final$meansw/final$totwt)
  final$relmsw_s=100*(final$meansw_s/final$totwt_s)
  final$ci=sqrt(final$variance/final$num_tows)*2
  final$relci=(final$ci/final$totwt)*100
  final$ci_s=sqrt(final$var_s/final$num_tows)*2
  final$relci_s=(final$ci_s/final$totwt_s)*100
  
  #remove strata-based metrics for now; only minor differences from original weighted metrics by tow  
  final[,':=' (meansw_s=NULL, num_stra=NULL, var_s=NULL, cv_s=NULL, totwt_s=NULL, relmsw_s=NULL, ci_s=NULL, relci_s=NULL )]
  

  names(final)[names(final)=='byvar1']=name_byvar1
  names(final)[names(final)=='byvar2']=name_byvar2
  names(final)[names(final)=='byvar3']=name_byvar3
  names(final)[names(final)=='byvar4']=name_byvar4
  #})
  
  diet <- final %>%
    dplyr::select(year, season, prey, relmsw, num_tows) %>%
    dplyr::group_by(season, prey) %>%
    dplyr::filter(relmsw>0.01) %>%
    dplyr::filter(mean(relmsw)>10.0)
  
  compplot <- ggplot(diet, aes(year, relmsw, fill=prey)) + 
    ylab("Percent in Diet") +
    xlab("Year") +
    facet_wrap("season", nrow=3) +
    theme_bw() + 
    viridis::scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="bottom", 
          legend.text=element_text(size=5))
  compi <- compplot + geom_bar_interactive(stat = "identity", aes(tooltip = prey, data_id = prey)) 

  return(compi)
}  

ptm <- proc.time()

df_diet <- get_diet(103)
ggiraph(code = print(df_diet), height=14)

out <- data.frame(t = (proc.time() - ptm)[3],
                  sys = Sys.time())
out


# d <- read.csv("~/attempts.csv")
# d <- d %>% arrange(time)
# plot(d$sys, type = "l")
# points(rep(mean(d$sys),nrow(d)))
