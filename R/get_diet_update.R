library(dplyr)
library(tidyr)
# library(reshape2)
# library(data.table)
# library(ggplot2)
# library(ggiraph)
# library(profvis)

species_code <- 103
nstom = FALSE
# get_diet <- function(species_code){
  #profvis({
  
  sss <- read.csv("data/seasonal_stock_strata.csv",
                  stringsAsFactors = FALSE)
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
  
  #Get data, manipulate, set constants
  svspp_df <- data.frame(sp = sixcode,
                         svspp = svspp_list, 
                         stringsAsFactors = FALSE)
  
  seasonal_strata <- sss %>% 
    dplyr::mutate(season = toupper(season)) %>% 
    dplyr::left_join(svspp_df, by = "sp") %>% 
    dplyr::rename(stratum = strata) %>% 
    dplyr::filter(svspp %in% species_code)
  

  allfh <- allfhsg

  spring_strata <- seasonal_strata$stratum[seasonal_strata$season == 'SPRING']
  summer_strata <- seasonal_strata$stratum[seasonal_strata$season == 'SUMMER']
  fall_strata <-   seasonal_strata$stratum[seasonal_strata$season == 'FALL']
  winter_strata <- seasonal_strata$stratum[seasonal_strata$season == 'WINTER']
  
  ## Filter out only good stomachs for strata and species
  # allsum <- allfh %>%
  allsum_raw <- allfh %>%
    dplyr::filter(pynam != 'BLOWN', 
                  pynam != 'PRESERVED',
                  pynam != ' ',
                  purcode == 10, 
                  svspp %in% species_code,
                  (season == "SPRING" & stratum %in% spring_strata) |
                  (season == "SUMMER" & stratum %in% summer_strata) |
                  (season == "FALL"   & stratum %in% fall_strata)   |
                  (season == "WINTER" & stratum %in% winter_strata))

  ## Number of stomachs by length and weight
  if(nstom){
    ## Sum by length
        allsum_len <- allsum_raw %>% 
          dplyr::group_by(year,
                   season,
                   svspp, 
                   cruise, 
                   station, 
                   pdid, 
                   pdsex, 
                   pdlen) %>%
          dplyr::summarise(totwt = sum(pyamtw)) %>%
          na.omit() %>%
          dplyr::group_by(year,
                          season,
                          svspp) %>%
          dplyr::summarise(nstom    = n(),
                           meanstom = mean(totwt, na.rm = TRUE),
                           varstom  = var(totwt, na.rm = TRUE), 
                           meanlen  = mean(pdlen, na.rm = TRUE),
                           numlen   = n(), 
                           minstom  = min(totwt, na.rm = TRUE),
                           maxstom  = max(totwt, na.rm = TRUE),
                           minlen   = min(pdlen, na.rm = TRUE),
                           maxlen   = max(pdlen, na.rm = TRUE))
        
        ## Sum by weight
        allsum_wgt <- allsum_raw %>% 
          dplyr::group_by(year, 
                   season,
                   svspp, 
                   cruise, 
                   station, 
                   pdid, 
                   pdsex, 
                   pdwgt, 
                   pdlen) %>%
          dplyr::summarise(totwt = sum(pyamtw, na.rm = TRUE)) %>%
          na.omit() %>%
          dplyr::group_by(year, 
                          season,
                          svspp) %>%
            dplyr::summarise(meanwgt = mean(pdwgt, na.rm = TRUE),
                             numwgt  = n(),
                             minwgt  = min(pdwgt, na.rm = TRUE),
                             maxwgt  = max(pdwgt, na.rm = TRUE))
        
        #stomstats2
        nstom_df <- allsum_len %>% 
          dplyr::left_join(allsum_wgt, by = c("year", "season", "svspp")) %>%
          dplyr::mutate(std.error = sqrt(varstom/nstom))

        # nstom <- nstom_df %>% 
        #   dplyr::select(svspp, year, season, nstom)
  }
  #----------------------------------------------------------------------------------------------------------#
  
  #----------------------------------Start weighted diet proportion code-------------------------------------#
 
  ## Select and rename the appropriate columns
  allsum_strat <- allsum_raw %>%
  # allfh2 <- allsum %>%
    dplyr::select(cruise6, stratum, station, 
                  year,
                  season,
                  svspp, pdid, 
                  pdsex, pdlen, 
                  tax = collsci, 
                  pyamt = pyamtw, 
                  catnum, numlen, 
                  tot_catnum_stratum, tot_catwgt_stratum, tot_tows_spp_stratum, 
                  stratum_area)
  
  ## Group into seasonal length-class and remove NAs
  num_strat <- allsum_strat %>%
    # allfh <- allfh2 %>%
    group_by(cruise6,
             station,
             year,
             season,
             svspp,
             pdid,
             pdsex,
             pdlen) %>%
    dplyr::summarise(xxxx = n()) %>%
    group_by(cruise6,
             station, 
             year,
             season, 
             svspp, 
             pdsex, 
             pdlen) %>%
    dplyr::summarise(numlen2 = n()) %>%
    # left_join(allfh2,  by = c("cruise6", "station", "year",
    #                           "season", "svspp", "pdsex", "pdlen")) %>% 
    left_join(allsum_strat,  by = c("cruise6", "station", "year",
                              "season", "svspp", "pdsex", "pdlen")) %>% 
    mutate(numlen_fin = dplyr::case_when(numlen < numlen2 ~ numlen2,
                                         numlen >= numlen2 ~ numlen,
                                         TRUE ~ NA_integer_)) %>%
    dplyr::select(c(cruise6, stratum, station,
                    year,
                    season, 
                    svspp, pdid, pdsex, pdlen, tax, pyamt, 
                    catnum, tot_catnum_stratum, tot_catwgt_stratum,
                    tot_tows_spp_stratum, stratum_area, numlen, numlen_fin))

  # allfh2 <- allfh %>%
  sum_strat <- num_strat %>%
    group_by(cruise6,
             station, 
             year, 
             season,
             svspp, 
             pdsex,
             pdlen, 
             numlen_fin) %>%
    dplyr::summarise(dummy_var = sum(numlen, na.rm = TRUE)) %>% 
    na.omit() %>%
    group_by(cruise6,
             station, 
             year, 
             season, 
             svspp, 
             pdlen) %>%
    dplyr::summarise(rnumlen_fin = sum(numlen_fin, na.rm = TRUE)) %>%
    # left_join(allfh, by = c("cruise6", "station", "year", "season", "svspp", "pdlen")) %>% 
    left_join(num_strat, by = c("cruise6", "station", "year", "season", "svspp", "pdlen")) %>% 
    dplyr::mutate(rnumlen_fin = ifelse(svspp !=13 | svspp !=15, 
                                       numlen_fin, 
                                       rnumlen_fin)) %>%
    group_by(cruise6, 
             station,
             year,
             season,
             svspp, 
             pdsex, 
             catnum) %>%
    dplyr::summarise(dummy_var = sum(numlen)) %>% 
    na.omit()
  
  
  # allfh <- allfh2 %>%
  sum_catnum <- sum_strat %>%
    group_by(cruise6,
             station, 
             year,
             season,
             svspp) %>%
    dplyr::summarise(rcatnum = sum(catnum, na.rm = TRUE)) %>%
    na.omit() %>%
    # left_join(allfh,  by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    left_join(num_strat,  by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    mutate(rcatnum = ifelse(svspp != 13 | svspp != 15, 
                            catnum, 
                            rcatnum))
  
  # allfh_catstratum <- allfh %>%
  sum_catstratum <- sum_catnum %>%
    group_by(cruise6, stratum, 
             year,
             season, 
             svspp, 
             pdsex, 
             tot_tows_spp_stratum,
             tot_catnum_stratum,
             tot_catwgt_stratum) %>%
    dplyr::summarise(dum_var = sum(rcatnum, na.rm = TRUE)) %>%
    na.omit()
  
  ####################################################################################################################################
  
  # max_tot_tows_spp_stratum <- allfh_catstratum %>%
  max_tot_tows_spp_stratum <- sum_catstratum %>%
    group_by(cruise6, 
             stratum, 
             year,
             season,
             svspp) %>% 
    dplyr::summarise(tot_tows_spp_stratum = max(tot_tows_spp_stratum, na.rm = TRUE))

  # allfh_rcatstratum <- allfh_catstratum %>%
  sum_rcatstratum <- sum_catstratum %>%
    group_by(cruise6, 
             stratum, 
             year,
             season,
             svspp) %>%
    dplyr::summarise(rtot_catnum_stratum = sum(tot_catnum_stratum, na.rm = TRUE),
                     rtot_catwgt_stratum = sum(tot_catwgt_stratum, na.rm = TRUE)) %>% 
    na.omit() %>%
    left_join(max_tot_tows_spp_stratum, by = c("cruise6", "stratum", "year", "season", "svspp")) %>%
    dplyr::rename(rtot_tows_spp_stratum = tot_tows_spp_stratum)
  
   final_strat <- sum_rcatstratum %>%
      # allfh <- allfh_rcatstratum %>%
    # left_join(allfh, by = c("cruise6", "stratum", "year", "season", "svspp")) %>% 
     left_join(sum_catnum, by = c("cruise6", "stratum", "year", "season", "svspp")) %>% 
    mutate(rtot_catnum_stratum = ifelse(svspp != 13 | svspp != 15, 
                                        tot_catnum_stratum, 
                                        rtot_catnum_stratum),
           rtot_catwgt_stratum = ifelse(svspp != 13 | svspp != 15, 
                                        tot_catwgt_stratum, 
                                        rtot_catwgt_stratum),
           rtot_tows_spp_stratum = ifelse(svspp != 13 | svspp != 15, 
                                          tot_tows_spp_stratum,
                                          rtot_tows_spp_stratum))
  
  ####################################################################################################################################
  # fish <- allfh %>%
   py_raw <- final_strat %>%
     group_by(cruise6, 
              station, 
              year,
              season, 
              svspp, pdid, pdlen, tax) %>%
     dplyr::summarise(pysum = sum(pyamt, na.rm = TRUE)) %>% 
     na.omit() 
      
      # pred <- fish %>%
      #   # pred <- py_raw %>%
      #   ungroup() %>%
      #   dplyr::select(cruise6, station, pdid, pysum,
      #                 year,
      #                 season,
      #                 svspp) %>%
      #   dplyr::arrange(cruise6, station,
      #                  year,
      #                  season,
      #                  svspp,
      #                  pdid)
  
  # new <- pred %>%
  py_nostom <- py_raw %>%
    ungroup %>% 
      dplyr::arrange(cruise6, station,
                     year,
                     season,
                     svspp,
                     pdid) %>%
    group_by(cruise6,
             station, 
             year,
             season, 
             svspp, 
             pdid) %>% 
    top_n(-1, wt = pysum) %>%
    group_by(cruise6, 
             station, 
             year, 
             season, 
             svspp) %>%
    dplyr::summarise(nostom = n())
  
  py_list <- data.frame(tax = unique(py_raw$tax),
                       pycode = paste0('prey', 1:length(unique(py_raw$tax))))
  
  # fish <- new %>%
  py_all <- py_nostom %>%
    # left_join(fish, by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    left_join(py_raw, by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    left_join(py_list, by = "tax")
  
  
  # length2 <- allfh %>%
  pd_strat <- final_strat %>%
    group_by(cruise6, 
             station, 
             year,
             season,
             svspp, 
             pdid, 
             pdlen) %>% 
    top_n(-1, wt = stratum_area) 
  
  
  # missingvars <- length2 %>%
  pd_nas <- pd_strat %>%
    group_by(cruise6, 
             station, 
             year,
             season,
             svspp, 
             pdlen) %>%
    dplyr::summarise(rnumlen_fin = sum(numlen_fin, na.rm = TRUE)) %>%
    na.omit()
  
  # length2 <- length2 %>%
  pd_strat <- pd_strat %>%
    # left_join(missingvars, by = c("cruise6", "year", "season", "svspp", "station", "pdlen")) %>% 
    left_join(pd_nas, by = c("cruise6", "year", "season", "svspp", "station", "pdlen")) %>% 
    dplyr::select(cruise6, station, stratum, 
                  year,
                  season,
                  svspp, pdid, pdlen,
                  rcatnum, rtot_catnum_stratum,
                  tot_catwgt_stratum, rtot_catwgt_stratum,
                  rtot_tows_spp_stratum, stratum_area, rnumlen_fin)
    
  
  # length2 <- length3 %>% dplyr::select(c(cruise6, station, stratum, byvar1, byvar2,
  #                                        byvar3, byvar4, svspp,  pdid, pdlen,
  #                                        rcatnum, rtot_catnum_stratum,
  #                                        tot_catwgt_stratum, rtot_catwgt_stratum,
  #                                        rtot_tows_spp_stratum, stratum_area, rnumlen_fin))
  
  
  #Working with fish data.frame----------------------------------------------------------------------------------#
  # fish_ply <- fish %>%
  fish_ply <- py_all %>%
    dplyr::select(-tax) %>% 
    # left_join(length2, by = c("cruise6", "station", "year", "season", "svspp", "pdid", "pdlen"))
    left_join(pd_strat, by = c("cruise6", "station", "year", "season", "svspp", "pdid", "pdlen"))

  
  ####################################################################################################################################
  #transpose cast pycode data and merge table to fish
  library(data.table)
  fish = data.table(fish_ply)

  #THIS IS A STRIPPED DOWN VERSION
  tran1 = data.frame(dcast.data.table(fish,
                                      cruise6 + stratum + station +
                                      year + season + #byvar3 + byvar4 +
                                      svspp + rcatnum +
                                      rtot_catnum_stratum +
                                      rtot_catwgt_stratum +
                                      rtot_tows_spp_stratum +
                                      stratum_area + rnumlen_fin  ~ pycode,
                                      fun.aggregate = median, value.var='pysum'))   #add byvars before svspp

  ## Take the median value for each prey group
  # tran1_ply = fish_ply %>%
  trans_ply <- fish_ply %>%
    group_by(cruise6, stratum, station,
           year, season, 
           svspp, rcatnum,
           rtot_catnum_stratum, 
           rtot_catwgt_stratum,
           rtot_tows_spp_stratum,
           stratum_area, rnumlen_fin, pycode) %>%
    summarize(pysum = median(pysum, na.rm = FALSE)) %>% 
    tidyr::spread(pycode, pysum) %>%
    filter(!is.na(rnumlen_fin)) %>%
    replace(., is.na(.), 0)
  
  #set prey NA to zero in fish
  preycol <- as.character(py_list$pycode)
  tran1[,preycol][is.na(tran1[,preycol])]=0
  # 
  tran1=data.table(tran1)
  #Am I missing something, or does this do not do anything?
  # new1=tran2[,lapply(.SD,mean, na.rm=T),by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdlen, catnum, rcatnum, tot_catnum_stratum, rtot_catnum_stratum, tot_catwgt_stratum,  #add byvars before svspp
  #                                               rtot_catwgt_stratum, tot_tows_spp_stratum, rtot_tows_spp_stratum, stratum_area, numlen, rnumlen_fin, nostom)]
  # #see here -> all.equal(tran2, new1)  
  #NA for many rnumlen_fin records??
  
  #remove NAs here
  new1rm <- tran1[!is.na(tran1$rnumlen_fin)]

  ## Weighted means
  # new11 = new1rm[,lapply(.SD,  weighted.mean,
  #                        rnumlen_fin, na.rm=TRUE), by=list(cruise6,stratum, station, year,
  #                                                         season,
  #                                                         # byvar3, byvar4,
  #                                                         svspp, rcatnum,
  #                                                         rtot_catnum_stratum, rtot_tows_spp_stratum,
  #                                                         stratum_area)]
  
  ## Weighted mean and mean of pysum
  # new11_ply <- tran1_ply %>%
  pysum_wm <- trans_ply %>%
    tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>%
    group_by(cruise6, stratum, station, 
             year, season,
             svspp, rcatnum, 
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, pycode) %>% 
    dplyr::summarize(wmean = weighted.mean(val, w = rnumlen_fin))# %>%
    # dplyr::summarize(wmean = weighted.mean(pysum, w = rnumlen_fin),
    #                  musw = mean(wmean, na.rm = TRUE)) %>% 
    # dplyr::mutate(musw2 = wmean * rcatnum) %>% 
    # tidyr::spread(pycode, wmean)
  

  ### Mean
# 
#     new2=new11[,lapply(.SD,mean, na.rm=T), by=list(cruise6, stratum, station, year,
#                                                  season,
#                                                  # byvar3, byvar4,
#                                                  svspp, rcatnum,
#                                                  rtot_catnum_stratum, rtot_tows_spp_stratum,
#                                                  stratum_area)]

  ## Mean
  pysum_m <- pysum_wm %>%
  # new2_ply <- new11_ply %>%
    # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>%
    group_by(cruise6, stratum, station,
             year, season,
             svspp, rcatnum,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area,
             pycode) %>%
    # dplyr::summarize(mean = mean(val)) %>%
    dplyr::summarize(musw = mean(wmean, na.rm = TRUE),
                     musw2 = wmean * rcatnum) #%>%
    # tidyr::spread(pycode, musw)
    # tidyr::spread(pycode, mean)

  # 
  # # musw2_ply <- new11_ply %>%
  #   musw2_ply <- pysum_wm %>%
  #   # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>%
  #   group_by(cruise6, stratum, station,
  #            year, season,
  #            svspp, rcatnum,
  #            rtot_catnum_stratum,
  #            rtot_tows_spp_stratum,
  #            stratum_area,
  #            pycode) %>%
  #   summarize(musw2 = wmean * rcatnum) %>%
  #   tidyr::spread(pycode, musw2)
  
  # musw2 = new11[,lapply(.SD,function(x) x*rcatnum),by=list(cruise6, stratum, station, year,
  #                                                          season,
  #                                                          # byvar3, byvar4,
  #                                                          svspp, rcatnum,
  #                                                          rtot_catnum_stratum, rtot_tows_spp_stratum,
  #                                                          stratum_area)] #add byvars before svspp


  # new2s = musw2[,lapply(.SD,sum ), by=list(cruise6, stratum, year, season,
  #                                          # byvar3, byvar4,
  #                                          svspp,
  #                                        rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp

  ## by station
  # new2s_ply <- musw2_ply %>%
  pysum_wm_s <- pysum_m %>%
    # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>%
    group_by(cruise6, stratum, 
             year, season,
             svspp,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, 
             pycode) %>% 
    # summarize(tmsw_strat = sum(val, na.rm = TRUE)) %>% 
    summarize(tmsw_strat = sum(musw2, na.rm = TRUE)) #%>% 
    # dplyr::mutate(rat = tmsw_strat/rtot_tows_spp_stratum,
    #               munfish_strat = rtot_catnum_stratum / rtot_tows_spp_stratum) %>%
    # tidyr::spread(pycode, tmsw_strat)
    
    
  # musw_strat_ply <- new2s_ply %>% 
  musw_strat_ply <- pysum_wm_s %>% 
    # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>%
    group_by(cruise6, stratum, 
             year, season,
             svspp,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, 
             pycode) %>% 
    # dplyr::summarize(rat = val/rtot_tows_spp_stratum) %>%
    dplyr::summarize(musw_strat = tmsw_strat/rtot_tows_spp_stratum) %>%
    # dplyr::mutate(rat = val/rtot_tows_spp_stratum) %>% 
    # tidyr::spread(pycode, rat) %>% 
    # ungroup %>%
    dplyr::mutate(munfish_strat = rtot_catnum_stratum / rtot_tows_spp_stratum)
  

  munfish_stratdat_ply <- musw_strat_ply %>%
    ungroup %>%
    dplyr::mutate(munfish_strat = rtot_catnum_stratum / rtot_tows_spp_stratum) %>%
    select(cruise6, stratum,
             year, season,
             svspp, munfish_strat)
  
  # musw_strat=new2s[,lapply(.SD, function(x) x/rtot_tows_spp_stratum),by=list(cruise6, stratum, year, season,
  #                                                                            # byvar3, byvar4,
  #                                                                            svspp,rcatnum,
  #                                                                            rtot_catnum_stratum, rtot_tows_spp_stratum,
  #                                                                            stratum_area)] #add byvars before svspp


  
  # musw_strat$munfish_strat = musw_strat$rtot_catnum_stratum / musw_strat$rtot_tows_spp_stratum
  # 
  # #retain munfish_strat
  # munfish_stratdat=musw_strat[, c('cruise6', 'stratum',  'year', 'season',# 'byvar3', 'byvar4',
  #                                 'svspp', 'munfish_strat'), with=F] #add byvars before
  # 
  # musw_strat=musw_strat[order(musw_strat$svspp, musw_strat$year,
  #                             musw_strat$season #musw_strat$byvar3,
  #                             # musw_strat$byvar4
  #                             ),] #byvars AFTER svspp

  #weight by stratum area
  
  # new3s_ply <- musw_strat_ply %>% 
  pymean_strat <- musw_strat_ply %>% 
    # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>% 
    group_by(svspp, year, season,
             pycode) %>% 
    dplyr::summarize(msw_strat = weighted.mean(musw_strat, stratum_area, na.rm = TRUE),
                     # strat_weight = weighted.mean(val, stratum_area, na.rm = TRUE),
                     # munfish_strat = weighted.mean(munfish_strat, stratum_area, na.rm = TRUE),
                     m_nfish_strat = weighted.mean(munfish_strat, stratum_area, na.rm = TRUE),
                     num_stra = n())#%>% 
  # tidyr::spread(pycode, msw_strat)
  
    
    
  # #first remove NA stratum areas
  # musw_stratum=musw_strat[!is.na(musw_strat$stratum_area)]
  # 
  # new3s=musw_stratum[,lapply(.SD, weighted.mean, stratum_area, na.rm=T), by=list(svspp, year, season)]#, byvar3, byvar4)] #add byvars AFTER svspp   #msw_strat
  # 
  # new3s[,':='(cruise6=NULL, stratum=NULL ,station=NULL, rcatnum=NULL, rtot_catnum_stratum=NULL, rtot_tows_spp_stratum=NULL, stratum_area=NULL)]
  # 
  # # new3s$num_stra=NROW(musw_stratum) #fix for multiple svspp
  # num_stra=musw_stratum[, .N,by=list(svspp,year,season)]#,byvar3,byvar4)] #add byvars AFTER svspp
  # names(num_stra)=gsub('N','num_stra',names(num_stra))


    meansw_s_ply <- pymean_strat %>% 
      # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>% 
      group_by(svspp, year, season, pycode) %>%
      # dplyr::summarize(per_strat = val/munfish_strat) %>%
      dplyr::summarize(meansw_s = msw_strat/m_nfish_strat) #%>%
      # dplyr::summarize(meansw_s = msw_strat/munfish_strat) #%>%
      # tidyr::spread(pycode, per_strat)

    # meansw_ply <- new2_ply %>%
      meansw_ply <- pysum_m %>% 
      # tidyr::gather(key = "pycode", value = "val", starts_with("prey")) %>% 
      group_by(svspp, year, season, pycode) %>%
      dplyr::summarize(meansw = weighted.mean(musw, rcatnum, na.rm = TRUE),
                       # weight_catnum = weighted.mean(val, rcatnum, na.rm = TRUE),
                       num_tows = n()) #%>%
      # tidyr::spread(pycode, weight_catnum)
    
    
    
  # new3s=merge(new3s, num_stra, by=c('svspp', 'year', 'season'),
  #             # 'byvar3', 'byvar4'),
  #             all.x=T)  #add byvars AFTER svspp
  # 
  # meansw_s=new3s[,lapply(.SD,function(x) x/munfish_strat),by=list(svspp,year,season)]#,byvar3,byvar4)] #add byvars AFTER svspp
  # 
  # meansw_s[,':='(munfish_strat=NULL, num_stra=NULL)]

  #CHANGE labels to meansw_s by prey number

  # three=new2[order(new2$svspp,new2$year, new2$season)]#, new2$byvar3,new2$byvar4),] #add byvars AFTER svspp

  # meansw = three[,lapply(.SD,weighted.mean, rcatnum, na.rm=T), by=list(svspp,year, season)]#, byvar3,byvar4)] #add byvars AFTER svspp
  # meansw[,':='(cruise6=NULL, stratum=NULL ,station=NULL, rcatnum=NULL, rtot_catnum_stratum=NULL, rtot_tows_spp_stratum=NULL, stratum_area=NULL)]


  # #meansw$num_tows=NROW(three) fix for multiple svspp
  # num_tows=three[, .N,by=list(svspp,year,season)]#,byvar3,byvar4)] #add byvars AFTER svspp
  # names(num_tows)=gsub('N','num_tows',names(num_tows))
  # 
  # meansw=merge(meansw, num_tows, by=c('svspp',  'year', 'season'),
  #              # 'byvar3', 'byvar4'),
  #              all.x=T)  #add byvars AFTER svspp

  master_ply <- pysum_m %>% #musw and musw2
    left_join(pysum_wm_s) %>%  #tmsw_strat
    # left_join(pysum_m) %>% # musw2
    left_join(musw_strat_ply) %>% # musw_strat
    left_join(meansw_ply) %>% # meansw
    left_join(meansw_s_ply) %>% # meansw_s
    left_join(pymean_strat) %>%     # msw_strat
    select(-munfish_strat) %>% 
    mutate(prod = rcatnum^2 * ((musw - meansw))^2,
           prodf = (rcatnum - m_nfish_strat)^2,
           prodd = (musw2 - musw_strat)^2,
           prod_cov = (rcatnum - m_nfish_strat) * (musw2 - musw_strat))# %>% 
  
  # 
  # master_musw_ply <- new2_ply %>% 
  #   tidyr::gather(key = "pycode", value = "musw", starts_with("prey"))
  # 
  # master_tmsw_strat_ply <- new2s_ply %>% 
  #   tidyr::gather(key = "pycode", value = "tmsw_strat", starts_with("prey")) 
  # 
  # master_musw2_ply <- musw2_ply %>% 
  #   tidyr::gather(key = "pycode", value = "musw2", starts_with("prey"))
  # 
  # master_musw_strat_ply <- musw_strat_ply %>% 
  #   tidyr::gather(key = "pycode", value = "musw_strat", starts_with("prey"))  
  #   
  # master_meansw_ply <- meansw_ply %>% 
  #   tidyr::gather(key = "pycode", value = "meansw", starts_with("prey"))  
  # 
  # master_meansw_s_ply <-  meansw_s_ply %>% 
  #   tidyr::gather(key = "pycode", value = "meansw_s", starts_with("prey"))  
  # 
  # master_msw_strat_ply <-  new3s_ply %>% 
  #   tidyr::gather(key = "pycode", value = "msw_strat", starts_with("prey"))  %>% 
  #   select(-munfish_strat,
  #          -num_stra)
  

  #melt three to start prod
  # master_musw=melt(three, id.vars=c("cruise6", "stratum", "station", "svspp",  "year", "season",
  #                                   # ##"byvar3", "byvar4",
  #                                   "rcatnum", "rtot_catnum_stratum", "rtot_tows_spp_stratum", "stratum_area"),
  #                  measure.vars=preycol, variable.name='pycode', value.name='musw')
  # 
  # master_tmsw_strat=melt(new2s, id.vars=c("cruise6", "stratum", "svspp", "year", "season"), #"byvar3", "byvar4"),
  #                        measure.vars=preycol, variable.name='pycode', value.name='tmsw_strat')
  # 
  # master_musw2=melt(musw2, id.vars=c("cruise6", "stratum", "station", "svspp", "year", "season"), #"byvar3", "byvar4"),
  #                   measure.vars=preycol, variable.name='pycode', value.name='musw2')
  # 
  # master_musw_strat=melt(musw_strat, id.vars=c("cruise6", "stratum",  "svspp", "year", "season"), #"byvar3", "byvar4" ),
  #                        measure.vars=preycol, variable.name='pycode', value.name='musw_strat')
  # 
  # master_meansw=melt(meansw, id.vars=c("svspp", "year", "season"), #"byvar3", "byvar4"),     #add byvars AFTER svspp
  #                    measure.vars=preycol, variable.name='pycode', value.name='meansw')
  # 
  # master_meansw_s=melt(meansw_s, id.vars=c("svspp", "year", "season"), #"byvar3", "byvar4"),   #add byvars AFTER svspp
  #                      measure.vars=preycol, variable.name='pycode', value.name='meansw_s')
  # 
  # master_msw_strat=melt(new3s, id.vars=c("svspp", "year", "season"), #"byvar3", "byvar4"),   #add byvars AFTER svspp
  #                       measure.vars=preycol, variable.name='pycode', value.name='msw_strat')
  # 
  # keep7=c('cruise6', 'stratum',  'svspp',  'year', 'season')#, #'byvar3', 'byvar4', 'munfish_strat') #add byvars AFTER svspp
  # master_munfish_strat=musw_strat[,keep7, with=F]

  # master_munfish_strat_ply <- musw_strat_ply %>% 
  #   select(cruise6, stratum, svspp, year, season)
  # 
  #   
  # # num_stra_fish=new3s[,c('svspp',  'year', 'season', #'byvar3', 'byvar4',
  # #                        'munfish_strat', 'num_stra'),with=F] #add byvars AFTER svspp
  # # names(num_stra_fish)=gsub('munfish_strat','m_nfish_strat',names(num_stra_fish))
  # 
  # num_stra_fish_ply <- new3s_ply %>% 
  #   select(svspp, year, season, m_nfish_strat = munfish_strat, num_stra)
  # 
  # # master_num_tows=meansw[,c('svspp',  'year', 'season', #'byvar3', 'byvar4',
  # #                           'num_tows'), with=F] #add byvars AFTER svspp
  # 
  # master_num_tows_ply <- meansw_ply %>% 
  #   select(svspp, year, season, num_tows)
  # 
  #   #merge masters
  # #memory.size(4095)
  # merge1_ply <- master_musw_ply %>% 
  #   left_join(master_musw_strat_ply)
  # 
  # merge2_ply <- merge1_ply %>% 
  #   left_join(master_tmsw_strat_ply)
  # 
  # merge3_ply <- merge2_ply %>% 
  #   left_join(master_munfish_strat_ply)
  # 
  # merge4_ply <- merge3_ply %>% 
  #   left_join(master_musw2_ply)
  # 
  # merge5_ply <- merge4_ply %>% 
  #   left_join(master_meansw_ply)
  # 
  # merge6_ply <- merge5_ply %>% 
  #   left_join(master_num_tows_ply)
  # 
  # merge7_ply <- merge6_ply %>% 
  #   left_join(master_meansw_s_ply)
  # 
  # merge8_ply <- merge7_ply %>% 
  #   left_join(num_stra_fish_ply)
  # 
  # master_ply <- merge8_ply %>% 
  #   left_join(master_msw_strat_ply) %>% 
  #   select(-munfish_strat) %>% 
  #   mutate(prod = rcatnum^2 * ((musw - meansw))^2,
  #          prodf = (rcatnum - m_nfish_strat)^2,
  #          prodd = (musw2 - musw_strat)^2,
  #          prod_cov = (rcatnum - m_nfish_strat) * (musw2 - musw_strat))# %>% 
  # 
  # 
  
  # bind_rows(master_ply %>%
  #             purrr::keep(is.numeric) %>%
  #             purrr::map_df(range),
  #           as.data.frame(master) %>%
  #             purrr::keep(is.numeric) %>%
  #             purrr::map_df(range))
  

      # select(-munfish_strat, -tt)
# 
#   merge1=merge(master_musw, master_musw_strat, by=c('cruise6', 'stratum',  'svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                                     'pycode'), all.x=T)  #add byvars AFTER svspp
#   merge2=merge(merge1, master_tmsw_strat, by=c('cruise6', 'stratum',  'svspp',   'year', 'season', #'byvar3', 'byvar4',
#                                                'pycode'), all.x=T)   #add byvars AFTER svspp
#   merge3=merge(merge2, master_munfish_strat, by=c('cruise6', 'stratum',  'svspp', 'year', 'season'), #'byvar3', 'byvar4'),
#                all.x=T)      #add byvars AFTER svspp
#   merge4=merge(merge3, master_musw2, by=c('cruise6', 'stratum', 'station', 'svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                           'pycode'), all.x=T)   #add byvars AFTER svspp
#   merge5=merge(merge4, master_meansw, by=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                            'pycode'), all.x=T)   #add byvars AFTER svspp
#   merge6=merge(merge5, master_num_tows, by=c('svspp', 'year', 'season'), #'byvar3', 'byvar4'),
#                all.x=T)          #add byvars AFTER svspp
#   merge7=merge(merge6, master_meansw_s, by=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                              'pycode'), all.x=T)  #add byvars AFTER svspp
#   merge8=merge(merge7, num_stra_fish, by=c('svspp', 'year', 'season'), #'byvar3', 'byvar4'),
#                all.x=T)           #add byvars AFTER svspp
#   master=merge(merge8, master_msw_strat, by=c('svspp', 'year', 'season', #'byvar3', 'byvar4',
#                                               'pycode'), all.x=T)  #add byvars AFTER svspp

#   
  # #prod    
  # master$prod=master$rcatnum^2*((master$musw-master$meansw))^2
  # master$prodf= (master$rcatnum-master$m_nfish_strat)^2
  # master$prodd= (master$musw2-master$musw_strat)^2
  # master$prod_cov=((master$rcatnum-master$m_nfish_strat)*(master$musw2-master$musw_strat))
  # 
  # 
  # master=master[order(master$svspp),] #add byvars AFTER svspp ##causes crash?
  
  
  # #sprod   better way?  remove cols except svspp and pycode?
  # sprod=master[,lapply(.SD, sum, na.rm=T), by=list(svspp,year,season,#byvar3,byvar4,
  #                                                  pycode)]#, num_tows, meansw, meansw_s, msw_strat, m_nfish_strat, num_stra)] #add byvars AFTER svspp
  # 
  # keep8=c('svspp',  'year', 'season',# 'byvar3', 'byvar4',
  #         'pycode', 'prod') ##'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra', 'prod') #add byvars AFTER svspp
  # sprod2=sprod[,keep8, with=F]
  # names(sprod2)=gsub('prod','sprod',names(sprod2))
  # 
  # sprod2_ply <- master_ply %>%
  #   group_by(svspp, year, season, pycode) %>%
  #   dplyr::summarize(sprod = sum(prod, na.rm = TRUE))
  # 
  # #mprod and mnumfish
  # mprod_mnumfish = master[,lapply(.SD, mean, na.rm=T), by=list(svspp, year,season,#byvar3,byvar4, pycode,
  #                                                              pycode)]#, num_tows, meansw, meansw_s, msw_strat, m_nfish_strat, num_stra)] #add byvars AFTER svspp
  # keep9=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
  #         'pycode', 'prod', 'rcatnum')#'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra', 'prod', 'rcatnum')        #add byvars AFTER svspp
  # mprod_mnumfish2=mprod_mnumfish[,keep9,with=F]
  # names(mprod_mnumfish2)=gsub('prod', 'mprod', names(mprod_mnumfish2))
  # names(mprod_mnumfish2)=gsub('rcatnum', 'mnumfish', names(mprod_mnumfish2))
  # 
  mprod_mnumfish2_ply <- master_ply %>%
    group_by(svspp, year, season, pycode) %>%
    summarize(mprod = mean(prod, na.rm = TRUE),
              mnumfish = mean(rcatnum, na.rm = TRUE))

  ## This should replace sprod2 and mprod_mnumfish2
  new4_ply <- master_ply %>% 
    group_by(svspp, year, season, pycode) %>% 
    summarize(sprod = sum(prod, na.rm = TRUE),
              mprod = mean(prod, na.rm = TRUE),
              mnumfish = mean(rcatnum, na.rm = TRUE))
  
  # new4=merge(sprod2, mprod_mnumfish2, by=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
  #                                          'pycode'), all.x=T)#, 'num_tows', 'meansw', 'meansw_s', 'msw_strat', 'm_nfish_strat', 'num_stra'), all.x=T)
  # add byvars?
  
  # sprodf_d_cov=master[,lapply(.SD, sum, na.rm=T), by=list(svspp, year,season,#byvar3,byvar4,
  #                                                         pycode, cruise6, stratum, stratum_area, rtot_tows_spp_stratum, m_nfish_strat)]      #add byvars after svspp
  # keep10=c('svspp',  'year', 'season',# 'byvar3', 'byvar4',
  #          'pycode', 'cruise6', 'stratum', 'stratum_area', 'rtot_tows_spp_stratum', 'm_nfish_strat', 'prodf', 'prodd', 'prod_cov')  #add byvars after svspp
  # sprodf_d_cov2=sprodf_d_cov[,keep10, with=F]
  # names(sprodf_d_cov2)=gsub('prodf', 'sprodf', names(sprodf_d_cov2))
  # names(sprodf_d_cov2)=gsub('prodd', 'sprodd', names(sprodf_d_cov2))
  # names(sprodf_d_cov2)=gsub('prod_cov', 'sprod_cov', names(sprodf_d_cov2))

  new6_ply <- master_ply %>% 
    group_by(svspp, year, season,
             pycode, cruise6, stratum,
             stratum_area, rtot_tows_spp_stratum,
             m_nfish_strat) %>%
    summarize(sprodf = sum(prodf, na.rm = TRUE),
              sprodd = sum(prodd, na.rm = TRUE),
              sprod_cov = sum(prod_cov, na.rm = TRUE)) %>% 
    mutate(dfntows_strat = rtot_tows_spp_stratum - 1,
           varprodf = ifelse(rtot_tows_spp_stratum > 1,
                             (stratum_area^2) * ((sprodf/dfntows_strat)/rtot_tows_spp_stratum),
                             0),
           varprodd = ifelse(rtot_tows_spp_stratum > 1,
                             (stratum_area^2) * ((sprodd/dfntows_strat)/rtot_tows_spp_stratum), 
                             0),
           varprod_cov = ifelse(rtot_tows_spp_stratum > 1, 
                                (stratum_area^2) * ((sprod_cov/dfntows_strat)/rtot_tows_spp_stratum),
                                0)) %>% 
    arrange(svspp, year, season)
  
  # sprodf_d_cov2$dfntows_strat= sprodf_d_cov2$rtot_tows_spp_stratum-1
  # sprodf_d_cov2$varprodf=ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprodf/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)
  # sprodf_d_cov2$varprodd= ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprodd/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)
  # sprodf_d_cov2$varprod_cov=ifelse(sprodf_d_cov2$rtot_tows_spp_stratum>1, (sprodf_d_cov2$stratum_area^2)*((sprodf_d_cov2$sprod_cov/sprodf_d_cov2$dfntows_strat)/sprodf_d_cov2$rtot_tows_spp_stratum), 0)

  # new6=sprodf_d_cov2[order(sprodf_d_cov2$svspp,sprodf_d_cov2$year,sprodf_d_cov2$season),]#,sprodf_d_cov2$byvar3,sprodf_d_cov2$byvar4),] #add byvars AFTER svspp
  # 
  # #sumvarprod     some vars still zero?
  # sumvarprod1=new6[,lapply(.SD,sum, na.rm=T), by=list(svspp, year,season, #byvar3,byvar4,
  #                                                     pycode)] #add byvars AFTER svspp
  # keep11=c('svspp',  'year', 'season',# 'byvar3', 'byvar4',
  #          'pycode', 'varprodf', 'varprodd', 'varprod_cov', 'stratum_area')   #add byvars AFTER svspp
  # sumvarprod=sumvarprod1[, keep11, with=F]
  # names(sumvarprod)=gsub('varprodf', 'svarprodf', names(sumvarprod))
  # names(sumvarprod)=gsub('varprodd', 'svarprodd', names(sumvarprod))
  # names(sumvarprod)=gsub('varprod_cov', 'svarprod_cov', names(sumvarprod))
  # names(sumvarprod)=gsub('stratum_area', 'sstratum_area', names(sumvarprod))
  # 
  # sumvarprod$varf=sumvarprod$svarprodf/sumvarprod$sstratum_area^2
  # sumvarprod$vard=sumvarprod$svarprodd/sumvarprod$sstratum_area^2
  # sumvarprod$var_cov=sumvarprod$svarprod_cov/sumvarprod$sstratum_area^2

  sumvarprod_ply <- new6_ply %>% 
    group_by(svspp, year, season, pycode) %>%
    summarize(svarprodf = sum(varprodf, na.rm = TRUE),
              svarprodd = sum(varprodd, na.rm = TRUE),
              svarprod_cov = sum(varprod_cov, na.rm = TRUE),
              stratum_area = sum(stratum_area, na.rm = TRUE)) %>% 
    mutate(varf = svarprodf / stratum_area^2,
           vard = svarprodd/ stratum_area^2,
           var_cov = svarprod_cov/ stratum_area^2) %>% 
    select(-stratum_area) %>% 
    arrange(svspp, year, season)
  
  new4_strat_ply <- new4_ply %>% 
    left_join(sumvarprod_ply) 
  
  # #new4_strat
  # new4_strat=merge(new4, sumvarprod, by=c('svspp',  'year', 'season',# 'byvar3', 'byvar4',
  #                                         'pycode'), all.x=T) #add byvars AFTER svspp

  
  
  
  #merge with  select master columns
six_ply <- new4_strat_ply %>%
  left_join(master_ply) %>% 
  # left_join(master_meansw_ply) %>% 
  # left_join(master_meansw_s_ply) %>% 
  # left_join(master_msw_strat_ply) %>% 
  # left_join(master_num_tows_ply) %>% 
  # left_join(num_stra_fish_ply) %>% 
  mutate(variance = ifelse(num_tows > 1 & mnumfish != 0 & meansw!=0 & m_nfish_strat !=0 & meansw_s!=0,
                           1/(num_tows * (mnumfish)^2) * (sprod/(num_tows-1)),
                           0),
         cv = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                     ((variance)^0.5)/meansw,
                     0),
         var_s = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                        (meansw_s^2) * ((varf/(m_nfish_strat^2)) + (vard/(msw_strat^2)) - (2 * var_cov/m_nfish_strat/msw_strat)),
                        0),
         cv_s = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                       ((var_s)^0.5)/meansw_s,0)) %>% 
  # select(-vard, -varf, -var_cov, -stratum_area)
  select(pycode, svspp, year, season, meansw,
         meansw_s, num_tows, m_nfish_strat, num_stra, 
         variance, cv, var_s, cv_s, pycode) %>% 
  distinct(.keep_all = TRUE)
  

# six1=merge(new4_strat, master_meansw, by=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                            'pycode'), all.x=T)  #add byvars AFTER svspp
# six2= merge(six1, master_meansw_s, by=c('svspp', 'year', 'season', #'byvar3', 'byvar4',
#                                         'pycode'), all.x=T)      #add byvars AFTER svspp
# six3=merge(six2, master_msw_strat, by=c('svspp',  'year', 'season', #'byvar3', 'byvar4',
#                                         'pycode'), all.x=T)     #add byvars AFTER svspp
# six4=merge(six3, master_num_tows, by=c('svspp', 'year', 'season'), #'byvar3', 'byvar4'),
#            all.x=T) #add byvars AFTER svspp
# six=merge(six4, num_stra_fish, by=c('svspp', 'year', 'season'),#, 'byvar3', 'byvar4'),
#           all.x=T) #add byvars AFTER svspp


  # six$variance=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0, 1/(six$num_tows*(six$mnumfish)^2)*(six$sprod/(six$num_tows-1)),0)
  # six$cv=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0,((six$variance)^0.5)/six$meansw,0)
  # six$var_s=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0, (six$meansw_s^2)*((six$varf/(six$m_nfish_strat^2))+(six$vard/(six$msw_strat^2))-(2*six$var_cov/six$m_nfish_strat/six$msw_strat)),0)
  # six$cv_s=ifelse(six$num_tows>1&six$mnumfish!=0&six$meansw!=0&six$m_nfish_strat!=0&six$meansw_s!=0,((six$var_s)^0.5)/six$meansw_s,0)

# six <- as.data.table(six)
  # renamed meansw since used earlier
  # meanswf=six[,':='(sprod=NULL, mprod=NULL, mnumfish=NULL, msw_strat=NULL, svarprodf=NULL, svarprodd=NULL, svarprod_cov=NULL, sstratum_area=NULL, varf=NULL, vard=NULL, var_cov=NULL)]
  
 # merge tax names back in
  # output=merge(meanswf, py_list, by=c('pycode'), all.x=T)
  # names(output)=gsub('tax', 'prey', names(output))

  final_ply <- six_ply %>% 
    mutate(pycode = as.factor(pycode)) %>% 
    left_join(py_list) %>% 
    group_by(svspp, year, season) %>% 
    mutate(totwt = sum(meansw, na.rm = TRUE),
           totwt_s = sum(meansw_s, na.rm = TRUE),
           relmsw = 100*(meansw/totwt),
           relmsw_s = 100*(meansw_s/totwt_s),
           ci = sqrt(variance/num_tows)*2,
           relci = (ci/totwt)*100,
           ci_s = sqrt(var_s/num_tows)*2,
           relci_s = (ci_s/totwt_s)*100) %>% 
    select(svspp, year, season, meansw, num_tows,
           variance, cv, prey = tax, totwt, 
           relmsw, ci, relci)

  # 
  # td <-  bind_rows(final_ply %>%
  #                    purrr::keep(is.numeric) %>%
  #                    purrr::map_df(range, na.rm = TRUE),
  #                  as.data.frame(final) %>%
  #                    purrr::keep(is.numeric) %>%
  #                    purrr::map_df(range, na.rm = TRUE))
# 
  # totwt1=output[,lapply(.SD,sum, na.rm=T), by=list(svspp,year,season),#,byvar3,byvar4),
  #               .SDcols=c('meansw', 'meansw_s')] #add byvars AFTER svspp
  # keep12=c('svspp',  'year', 'season',# 'byvar3', 'byvar4',
  #          'meansw', 'meansw_s')  #add byvars AFTER svspp
  # totwt=totwt1[,keep12,with=F]
  # names(totwt)=gsub('meansw', 'totwt', names(totwt))
  # names(totwt)=gsub('meansw_s', 'totwt_s', names(totwt))
  # 
  # final=merge(output, totwt, by=c('svspp',  'year', 'season'),# 'byvar3', 'byvar4'),
  #             all.x=T)  #add byvars AFTER svspp
  # 
  # final=final[,':='(pycode=NULL, m_nfish_strat=NULL)]
  # 
  # final$relmsw=100*(final$meansw/final$totwt)
  # final$relmsw_s=100*(final$meansw_s/final$totwt_s)
  # final$ci=sqrt(final$variance/final$num_tows)*2
  # final$relci=(final$ci/final$totwt)*100
  # final$ci_s=sqrt(final$var_s/final$num_tows)*2
  # final$relci_s=(final$ci_s/final$totwt_s)*100
  # 
  # #remove strata-based metrics for now; only minor differences from original weighted metrics by tow
  # final[,':=' (meansw_s=NULL, num_stra=NULL, var_s=NULL, cv_s=NULL, totwt_s=NULL, relmsw_s=NULL, ci_s=NULL, relci_s=NULL )]
  # 

  # names(final)[names(final)=='byvar1']=name_byvar1
  # names(final)[names(final)=='byvar2']=name_byvar2
  # names(final)[names(final)=='byvar3']=name_byvar3
  # names(final)[names(final)=='byvar4']=name_byvar4
  #})
  
  # diet_ply <- final_ply %>%
  #   dplyr::select(year, season, prey, relmsw, num_tows) %>%
  #   dplyr::group_by(season, prey) %>%
  #   dplyr::filter(relmsw>0.01) %>%
  #   dplyr::filter(mean(relmsw)>10.0)
  # 
  # 
  # diet <- final %>%
  #   dplyr::select(year, season, prey, relmsw, num_tows) %>%
  #   dplyr::group_by(season, prey) %>%
  #   dplyr::filter(relmsw>0.01) %>%
  #   dplyr::filter(mean(relmsw)>10.0)
  # 
  # 
  # library(ggplot2)
  # compplot <- ggplot(diet, aes(year, relmsw, fill=prey)) +
  #   geom_bar(stat = "identity") +
  #   ylab("Percent in Diet") +
  #   xlab("Year") +
  #   facet_wrap("season", nrow=3) +
  #   theme_bw() +
  #   viridis::scale_fill_viridis(discrete = TRUE) +
  #   theme(legend.position="bottom",
  #         legend.text=element_text(size=5))
  # 
  # compplot_ply <- ggplot(diet_ply, aes(year, relmsw, fill=prey)) +
  #   geom_bar(stat = "identity") +
  #   ylab("Percent in Diet") +
  #   xlab("Year") +
  #   facet_wrap("season", nrow=3) +
  #   theme_bw() +
  #   viridis::scale_fill_viridis(discrete = TRUE) +
  #   theme(legend.position="bottom",
  #         legend.text=element_text(size=5))
  # 
  # library(patchwork)
  # 
  # compplot + compplot_ply
  # compi <- compplot + geom_bar_interactive(stat = "identity", aes(tooltip = prey, data_id = prey))

  # return(compi)
# }  

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
