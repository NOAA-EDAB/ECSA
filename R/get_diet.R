# Original file from Brian Smith, August 23 2018 was allwt_nstoms.R, renamed as get_diet.R

#calc descriptive stats for diet data (aka allsum.sas) and weighted diet compositions (aka allwt.sas)

# this part to go in the template, defining the species and strata

#read pred-seas-strata definitions and add cols
sss <- read.csv("data/seasonal_stock_strata.csv")
sss$season <- toupper(sss$season)
sss$stratum <- sss$strata

splookup <- read.csv("data/species_list.csv") #this does not have all species in it yet
spsvspp <- unique(splookup[,c("six_name", "svspp")])
#this fills in the summer flounder number now, will do all when species_list is complete
sss$svspp <- spsvspp$svspp[match(sss$sp, spsvspp$six_name)]

# we dont need all these for the demo
# sss$svspp <- ifelse(sss$sp=='acared', 155, 
#              ifelse(sss$sp=='alewif', 33, 
#              ifelse(sss$sp=='amepla', 102, 
#              ifelse(sss$sp=='atlcod', 73, 
#              ifelse(sss$sp=='atlhal', 101, 
#              ifelse(sss$sp=='atlher', 32, 
#              ifelse(sss$sp=='atlmac', 121, 
#              ifelse(sss$sp=='atlwol', 192, 
#              ifelse(sss$sp=='barska', 22,
#              ifelse(sss$sp=='blabas', 141, 
#              ifelse(sss$sp=='bluefi', 135, 
#              ifelse(sss$sp=='bluher', 34,
#              ifelse(sss$sp=='butter', 131,
#              ifelse(sss$sp=='cleska', 24, 
#              ifelse(sss$sp=='haddoc', 74, 
#              ifelse(sss$sp=='litska', 26, 
#              ifelse(sss$sp=='monkfh', 197, 
#              ifelse(sss$sp=='ocpout', 193,
#              ifelse(sss$sp=='offhak', 69, 
#              ifelse(sss$sp=='polloc', 75, 
#              ifelse(sss$sp=='redhak', 77, 
#              ifelse(sss$sp=='rosska', 25, 
#              ifelse(sss$sp=='scupzz', 143,
#              ifelse(sss$sp=='silhak', 72, 
#              ifelse(sss$sp=='smodog', 13, 
#              ifelse(sss$sp=='smoska', 27,
#              ifelse(sss$sp=='spidog', 15, 
#              ifelse(sss$sp=='sumflo', 103,
#              ifelse(sss$sp=='thoska', 28,
#              ifelse(sss$sp=='whihak', 76,
#              ifelse(sss$sp=='window', 108,
#              ifelse(sss$sp=='winflo', 106,
#              ifelse(sss$sp=='winska', 23, 
#              ifelse(sss$sp=='witflo', 107, 
#              ifelse(sss$sp=='yelflo', 105,'X')))))))))))))))))))))))))))))))))))

##sumflo example svspp==103
# this will come from the main template
sssin=subset(sss, sss$svspp==103)
SVSPP=unique(sssin$svspp)
#SEASON=unique(sssin$season)

#read in allfh
load("/Users/sgaichas/Documents/0_Data/ESR/SpeciesEcoreport/dietdata/allfhsg.RData")
#load("data/allfhsg.RData")

#allfh
allfh=allfhsg

#column names to lowercase 
names(allfh)=tolower(names(allfh))


#USER OPTIONS
#CHOOSE byvars and byvar names (e.g. geoarea, sizecat, season, year, etc.).  Order according to output and fill as necessary or leave 'x'. Allows for max of 4.
#1
allfh$byvar1=allfh$year
name_byvar1=c('year')

#2
#allfh$byvar2=allfh$season
#name_byvar2=c('season')
allfh$byvar2='x'
name_byvar2=c('x')

#3
allfh$byvar3='x'
name_byvar3=c('x')

#4
allfh$byvar4='x'
name_byvar4=c('x')




################BEGIN ALLSUM CODE#################################
# DON'T CHANGE. This allows for the appropriate number of columns when setting up data.
nbyvar=4

#ALLSUM  Limit data here and select predators (svspp). 
allsum2= subset(allfh, allfh$pynam != 'BLOWN' & allfh$pynam != 'PRESERVED' & allfh$pynam != ' ' & allfh$purcode == 10 ) 

#this selects the species
allsum1a=subset(allsum2, allsum2$svspp%in%SVSPP )  #select svspp 

#this is selecting the seasonal strata
allsum1=subset(allsum1a,(allsum1a$season=='FALL' &allsum1a$stratum%in%sssin$stratum[sssin$season=='FALL'])|(allsum1a$season=='SPRING' &allsum1a$stratum%in%sssin$stratum[sssin$season=='SPRING'])|(allsum1a$season=='WINTER' &allsum1a$stratum%in%sssin$stratum[sssin$season=='WINTER']))

#attach(allsum1)
#allsum=allsum1[order(+svspp, year, #byvar,year
#+cruise, +station, +pdsex, +pdid, +pdlen),]
#detach(allsum1)
#head(allsum,10)

allsum=allsum1[order(allsum1$svspp, allsum1$byvar1, allsum1$byvar2, allsum1$byvar3, allsum1$byvar4,
                     allsum1$cruise, allsum1$station, allsum1$pdsex, allsum1$pdid, allsum1$pdlen),]
head(allsum,10)

### this is the summer flounder raw data, not yet aggregated or weighted diets ###
save(allsum, file="/Users/sgaichas/Documents/0_Data/ESR/SpeciesEcoreport/dietdata/sumflodietraw.RData")

#attach(allsum)
#asum=allsum[order(+svspp, +year),] #+year, #byvar
#detach(allsum)

###### read in the summer flounder data only to start calcs here #######

load("data/sumflodietraw.RData")

asum=allsum[order(allsum$svspp, allsum$byvar1, allsum$byvar2, allsum$byvar3, allsum$byvar4),]

#aggregate, byvars first if necessary.
agg_allsum = aggregate(asum$pyamtw,  list(byvar1=asum$byvar1, byvar2=asum$byvar2, byvar3=asum$byvar3, byvar4=asum$byvar4,  svspp=asum$svspp, cruise=asum$cruise, station=asum$station, pdid=asum$pdid, pdsex=asum$pdsex, pdlen=asum$pdlen ), sum)
colnames(agg_allsum)[7+nbyvar]="totwt"
#removed pdswgt=allsum1$pdswgt
head(agg_allsum,10)

#nstoms
nstom=aggregate(agg_allsum$totwt, list('byvar1'=agg_allsum$byvar1, 'byvar2'=agg_allsum$byvar2, 'byvar3'=agg_allsum$byvar3, 'byvar4'=agg_allsum$byvar4, 'svspp'=agg_allsum$svspp), length)
colnames(nstom)[2+nbyvar]='nstom'
nstom  
#mean pdgutw
meanstom=aggregate(agg_allsum$totwt[!is.na(agg_allsum$totwt)], list('byvar1'=agg_allsum$byvar1[!is.na(agg_allsum$totwt)], 'byvar2'=agg_allsum$byvar2[!is.na(agg_allsum$totwt)], 'byvar3'=agg_allsum$byvar3[!is.na(agg_allsum$totwt)],'byvar4'=agg_allsum$byvar4[!is.na(agg_allsum$totwt)], 'svspp'=agg_allsum$svspp[!is.na(agg_allsum$totwt)]), mean)
colnames(meanstom)[2+nbyvar]='meanstom'
meanstom

#var pdgutw
varstom=aggregate(agg_allsum$totwt[!is.na(agg_allsum$totwt)], list('byvar1'=agg_allsum$byvar1[!is.na(agg_allsum$totwt)], 'byvar2'=agg_allsum$byvar2[!is.na(agg_allsum$totwt)], 'byvar3'=agg_allsum$byvar3[!is.na(agg_allsum$totwt)],'byvar4'=agg_allsum$byvar4[!is.na(agg_allsum$totwt)], 'svspp'=agg_allsum$svspp[!is.na(agg_allsum$totwt)]), var)
colnames(varstom)[2+nbyvar]='varstom'
varstom

#mean pdlen
meanlen=aggregate(agg_allsum$pdlen[!is.na(agg_allsum$pdlen)], list('byvar1'=agg_allsum$byvar1[!is.na(agg_allsum$pdlen)], 'byvar2'=agg_allsum$byvar2[!is.na(agg_allsum$pdlen)],'byvar3'=agg_allsum$byvar3[!is.na(agg_allsum$pdlen)],'byvar4'=agg_allsum$byvar4[!is.na(agg_allsum$pdlen)], 'svspp'=agg_allsum$svspp[!is.na(agg_allsum$pdlen)]), mean)
colnames(meanlen)[2+nbyvar]='meanlen'
meanlen

#mean pdwgt
#new aggregate dataset just for pdwgt
aggpdwgt= aggregate(asum$pyamtw,  list(byvar1=asum$byvar1, byvar2=asum$byvar2, byvar3=asum$byvar3, byvar4=asum$byvar4, svspp=asum$svspp,  cruise=asum$cruise, station=asum$station, pdid=asum$pdid, pdsex=asum$pdsex, pdwgt=asum$pdwgt, pdlen=asum$pdlen ), sum)
colnames(aggpdwgt)[8+nbyvar]="totwt"
head(aggpdwgt,10)
meanwgt=aggregate(aggpdwgt$pdwgt[!is.na(aggpdwgt$pdwgt)], list('byvar1'=aggpdwgt$byvar1[!is.na(aggpdwgt$pdwgt)], 'byvar2'=aggpdwgt$byvar2[!is.na(aggpdwgt$pdwgt)], 'byvar3'=aggpdwgt$byvar3[!is.na(aggpdwgt$pdwgt)],'byvar4'=aggpdwgt$byvar4[!is.na(aggpdwgt$pdwgt)], 'svspp'=aggpdwgt$svspp[!is.na(aggpdwgt$pdwgt)]), mean)
colnames(meanwgt)[2+nbyvar]='meanwgt'
meanwgt

#n pdlen
numlen=aggregate(agg_allsum$pdlen, list('byvar1'=agg_allsum$byvar1, 'byvar2'=agg_allsum$byvar2, 'byvar3'=agg_allsum$byvar3, 'byvar4'=agg_allsum$byvar4,'svspp'=agg_allsum$svspp), length)
colnames(numlen)[2+nbyvar]='numlen'
numlen

#n pdwgt
numwgt=aggregate(aggpdwgt$pdwgt, list('byvar1'=aggpdwgt$byvar1, 'byvar2'=aggpdwgt$byvar2, 'byvar3'=aggpdwgt$byvar3, 'byvar4'=aggpdwgt$byvar4,'svspp'=aggpdwgt$svspp), length)
colnames(numwgt)[2+nbyvar]='numwgt'
numwgt

#min stom amt
minstom=aggregate(agg_allsum$totwt[!is.na(agg_allsum$totwt)], list('byvar1'=agg_allsum$byvar1[!is.na(agg_allsum$totwt)], 'byvar2'=agg_allsum$byvar2[!is.na(agg_allsum$totwt)], 'byvar3'=agg_allsum$byvar3[!is.na(agg_allsum$totwt)], 'byvar4'=agg_allsum$byvar4[!is.na(agg_allsum$totwt)], 'svspp'=agg_allsum$svspp[!is.na(agg_allsum$totwt)]), min)
colnames(minstom)[2+nbyvar]='minstom'
minstom

#max stom amt
maxstom=aggregate(agg_allsum$totwt[!is.na(agg_allsum$totwt)], list('byvar1'=agg_allsum$byvar1[!is.na(agg_allsum$totwt)], 'byvar2'=agg_allsum$byvar2[!is.na(agg_allsum$totwt)], 'byvar3'=agg_allsum$byvar3[!is.na(agg_allsum$totwt)],'byvar4'=agg_allsum$byvar4[!is.na(agg_allsum$totwt)], 'svspp'=agg_allsum$svspp[!is.na(agg_allsum$totwt)]), max)
colnames(maxstom)[2+nbyvar]='maxstom'
maxstom

#min pdwgt
minwgt=aggregate(aggpdwgt$pdwgt, list('byvar1'=aggpdwgt$byvar1, 'byvar2'=aggpdwgt$byvar2, 'byvar3'=aggpdwgt$byvar3,'byvar4'=aggpdwgt$byvar4, 'svspp'=aggpdwgt$svspp), min)
colnames(minwgt)[2+nbyvar]='minwgt'
minwgt

#max pdwgt
maxwgt=aggregate(aggpdwgt$pdwgt, list('byvar1'=aggpdwgt$byvar1, 'byvar2'=aggpdwgt$byvar2, 'byvar3'=aggpdwgt$byvar3,'byvar4'=aggpdwgt$byvar4, 'svspp'=aggpdwgt$svspp), max)
colnames(maxwgt)[2+nbyvar]='maxwgt'
maxwgt

#min pdlen
minlen=aggregate(agg_allsum$pdlen, list('byvar1'=agg_allsum$byvar1, 'byvar2'=agg_allsum$byvar2, 'byvar3'=agg_allsum$byvar3, 'byvar4'=agg_allsum$byvar4, 'svspp'=agg_allsum$svspp), min)
colnames(minlen)[2+nbyvar]='minlen'
minlen

#max pdlen
maxlen=aggregate(agg_allsum$pdlen, list('byvar1'=agg_allsum$byvar1, 'byvar2'=agg_allsum$byvar2, 'byvar3'=agg_allsum$byvar3, 'byvar4'=agg_allsum$byvar4, 'svspp'=agg_allsum$svspp), max)
colnames(maxlen)[2+nbyvar]='maxlen'
maxlen


allsuma=cbind(meanstom, 'meanlen'=meanlen$meanlen, 'varstom'=varstom$varstom, 'nstom'=nstom$nstom, 'numlen'=numlen$numlen, 'minstom'=minstom$minstom, 'minlen'=minlen$minlen, 'maxstom'=maxstom$maxstom, 'maxlen'=maxlen$maxlen)
allsuma
allsumb=merge(allsuma, meanwgt, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)
allsumb
allsumc=merge(allsumb, numwgt, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)
allsumc
allsumd=merge(allsumc, minwgt, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)
allsumd
allsume=merge(allsumd, maxwgt, by=c('svspp', 'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)
allsume

allsume$stderror=sqrt(allsume$varstom/allsume$nstom)
#allsum=allsume[,c(1:4,15,5:14)]
c1=4+nbyvar
c2=15+nbyvar
c3=5+nbyvar
c4=14+nbyvar
allsumfin=allsume[,c(1:c1,c2,c3:c4)]

allsum=allsumfin

if(unique(allsum$byvar1=='x')){allsum$byvar1=NULL} 
if(unique(allsum$byvar2=='x')){allsum$byvar2=NULL}
if(unique(allsum$byvar3=='x')){allsum$byvar3=NULL}
if(unique(allsum$byvar4=='x')){allsum$byvar4=NULL}

names(allsum)[names(allsum)=='byvar1']=name_byvar1                                  
names(allsum)[names(allsum)=='byvar2']=name_byvar2
names(allsum)[names(allsum)=='byvar3']=name_byvar3                                  
names(allsum)[names(allsum)=='byvar4']=name_byvar4                                  

#allsum with nstom
nstom=allsum[,c(1,2,7)]

###################Begin allwt code###################################

#read in allfh again for weighted diet props
load("allfhsg.RData")

#allfh
allfh=allfhsg

names(allfh)=tolower(names(allfh))


#CHOOSE byvars and byvar names (e.g. geoarea, sizecat, season, year, etc.).  Order according to output and fill as necessary or leave 'x'. Allows for max of 4.
#1
allfh$byvar1=allfh$year
name_byvar1=c('year')

#2
allfh$byvar2='x'
name_byvar2=c('x')

#3
allfh$byvar3='x'
name_byvar3=c('x')

#4
allfh$byvar4='x'
name_byvar4=c('x')


#Define pyvar; pyamt covers all years; global var
#pyvar=('pyamt') #for now use pyamt throughout pr pyvar

########Define taxnam; analcat, collcat, gencat, or create your own with ifelse etc.

allfh$taxnam=allfh$collsci


########Define svspp;
pred=allfh$svspp%in%SVSPP  
pred

#Define byvars;
#byvars=c("year")#(season, geoarea, year...)
#byvars

# DON'T CHANGE. This allows for the appropriate number of columns when setting up data.
nbyvar=4

allfh1d=subset(allfh, pred & (allfh$pynam!='BLOWN'& allfh$pynam != 'PRESERVED' & allfh$pynam != ' ' & allfh$purcode == 10)) #add other data limitations here

#data limitations specific to sssin
allfh1=subset(allfh1d, (allfh1d$season=='FALL' &allfh1d$stratum%in%sssin$stratum[sssin$season=='FALL'])|(allfh1d$season=='SPRING' &allfh1d$stratum%in%sssin$stratum[sssin$season=='SPRING'])|(allfh1d$season=='WINTER' &allfh1d$stratum%in%sssin$stratum[sssin$season=='WINTER']))


allfh1$tax=allfh1$taxnam
allfh1$perpy=allfh1$perpyw
allfh1$pyamt=allfh1$pyamtw

#pyamt is the variable for prey weight

keep1=c('cruise6', 'stratum', 'station', 'byvar1','byvar2', 'byvar3', 'byvar4', 'svspp', 'pdid', 'pdsex', 'pdlen', 'tax', 'pyamt', 'catnum', 'numlen', 'tot_catnum_stratum', 'tot_catwgt_stratum', 'tot_tows_spp_stratum', 'stratum_area')  #add byvars before svspp
allfh2=allfh1[keep1]
#head(allfh2,10)

allwt1=allfh2[order(allfh2$cruise6, allfh2$station, allfh2$byvar1, allfh2$byvar2, allfh2$byvar3, allfh2$byvar4, allfh2$svspp, allfh2$pdid, allfh2$pdsex, allfh2$pdlen),] #add byvars before svspp
#allwt1

new_numlen1=aggregate(allwt1$numlen, list('cruise6'=allwt1$cruise6, 'station'=allwt1$station, 'byvar1'=allwt1$byvar1, 'byvar2'=allwt1$byvar2, 'byvar3'=allwt1$byvar3, 'byvar4'=allwt1$byvar4, 'svspp'=allwt1$svspp, 'pdid'=allwt1$pdid, 'pdsex'=allwt1$pdsex, 'pdlen'=allwt1$pdlen), length) #add byvars before svspp
#head(new_numlen1,10)
colnames(new_numlen1)[7+nbyvar]='xxxx' #change column number if byvars added

new_numlen1a=new_numlen1[order(new_numlen1$cruise6, new_numlen1$station, new_numlen1$byvar1, new_numlen1$byvar2, new_numlen1$byvar3, new_numlen1$byvar4, new_numlen1$svspp, new_numlen1$pdsex, new_numlen1$pdlen),] #add byvars before svspp
#new_numlen1a

#create numlen2 (ie count of true numbers at pdlen); eg in some cases (2) 40cm svspp individuals sampled per station
new_numlen2=aggregate(new_numlen1a$pdlen, list('cruise6'=new_numlen1a$cruise6, 'station'=new_numlen1a$station, 'byvar1'=new_numlen1a$byvar1, 'byvar2'=new_numlen1a$byvar2, 'byvar3'=new_numlen1a$byvar3, 'byvar4'=new_numlen1a$byvar4, 'svspp'=new_numlen1a$svspp, 'pdsex'=new_numlen1a$pdsex, 'pdlen'=new_numlen1a$pdlen), length) #add byvars before svspp
#head(new_numlen2,10)
colnames(new_numlen2)[6+nbyvar]='numlen2' #change column number if byvars added

#write.csv(new_numlen2,'new_numlen2.csv')

allfh_prep=merge(allwt1, new_numlen2, by=c('cruise6', 'station','byvar1', 'byvar2', 'byvar3', 'byvar4',  'svspp', 'pdsex', 'pdlen'), all.x=T) #add byvars before svspp
#head(allfh_prep,10)

allfh=allfh_prep
allfh$numlen_fin=ifelse(allfh$numlen< allfh$numlen2, allfh$numlen2, ifelse(is.na(allfh$numlen), allfh$numlen, allfh$numlen))
#head(allfh,10)



keep2=c('cruise6', 'stratum', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdid', 'pdsex', 'pdlen', 'tax', 'pyamt', 'catnum', 'tot_catnum_stratum', 'tot_catwgt_stratum', 'tot_tows_spp_stratum', 'stratum_area', 'numlen', 'numlen_fin')  #add byvars before svspp
#edit
allfh=allfh[keep2]

#numlen_fin created but only for non spiny dog and smoothdog; create rnumlen_fin: use instead of numlen; numlen_fin includes null numlen
#fix numlens for 15 and 13
allfh=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdsex, allfh$pdlen, allfh$numlen_fin),] #add byvars before svspp
#head(allfh,10)


allfh_numlen_fin=aggregate(allfh$numlen, list('cruise6'=allfh$cruise6, 'station'=allfh$station, 'byvar1'=allfh$byvar1, 'byvar2'=allfh$byvar2,'byvar3'=allfh$byvar3,'byvar4'=allfh$byvar4, 'svspp'=allfh$svspp, 'pdsex'=allfh$pdsex, 'pdlen'=allfh$pdlen, 'numlen_fin'=allfh$numlen_fin), sum, na.rm=T) #add byvars before svspp
#head(allfh_numlen_fin,10)
colnames(allfh_numlen_fin)[7+nbyvar]='dumby_var' #change column number if byvars added

allfh_numlen_fin=allfh_numlen_fin[order(allfh_numlen_fin$cruise6, allfh_numlen_fin$station, allfh_numlen_fin$byvar1, allfh_numlen_fin$byvar2, allfh_numlen_fin$byvar3, allfh_numlen_fin$byvar4, allfh_numlen_fin$svspp, allfh_numlen_fin$pdlen),] #add byvars before svspp
#head(allfh_numlen_fin,10)

allfh_rnumlen_fin=aggregate(allfh_numlen_fin$numlen_fin, list('cruise6'=allfh_numlen_fin$cruise6, 'station'=allfh_numlen_fin$station, 'byvar1'=allfh_numlen_fin$byvar1, 'byvar2'=allfh_numlen_fin$byvar2, 'byvar3'=allfh_numlen_fin$byvar3, 'byvar4'=allfh_numlen_fin$byvar4, 'svspp'=allfh_numlen_fin$svspp, 'pdlen'=allfh_numlen_fin$pdlen),sum, na.rm=T)  #add byvars before svspp
#head(allfh_rnumlen_fin,10)
colnames(allfh_rnumlen_fin)[5+nbyvar]='rnumlen_fin' #change column number if byvars added


allfh=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdlen),] #add byvars before svspp

allfh=merge(allfh, allfh_rnumlen_fin, by=c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdlen'), all.x=T) #add byvars before svspp
#allfh_rnumlen_fin missing 2014

allfh$rnumlen_fin=ifelse(allfh$svspp!=13 & allfh$svspp!=15, allfh$numlen_fin, allfh$rnumlen_fin)
#head(allfh,10)

#numlen_fin=NA??

#end fixes for numlen 13 and 15

#sort and sum stomach data (volume) over prey taxa - (e.g. over analcat)
allfh=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdsex, allfh$pdid, allfh$pdlen, allfh$tax),] #add byvars before svspp

allfh=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdsex, allfh$catnum),] #add byvars before svspp

allfh_catnum=aggregate(allfh$numlen, list('cruise6'=allfh$cruise6, 'station'=allfh$station, 'byvar1'=allfh$byvar1, 'byvar2'=allfh$byvar2, 'byvar3'=allfh$byvar3, 'byvar4'=allfh$byvar4, 'svspp'=allfh$svspp, 'pdsex'=allfh$pdsex, 'catnum'=allfh$catnum), sum, na.rm=T) #add byvars before svspp
#head(allfh_catnum,10)
colnames(allfh_catnum)[6+nbyvar]='dumby_var' #change column number if byvars added

allfh_catnum=allfh_catnum[order(allfh_catnum$cruise6, allfh_catnum$station, allfh_catnum$byvar1, allfh_catnum$byvar2, allfh_catnum$byvar3, allfh_catnum$byvar4, allfh_catnum$svspp),] #add byvars before svspp

allfh_rcatnum=aggregate(allfh_catnum$catnum, list('cruise6'=allfh_catnum$cruise6, 'station'=allfh_catnum$station, 'byvar1'=allfh_catnum$byvar1, 'byvar2'=allfh_catnum$byvar2, 'byvar3'=allfh_catnum$byvar3, 'byvar4'=allfh_catnum$byvar4, 'svspp'=allfh_catnum$svspp), sum,na.rm=T) #add byvars before svspp
#head(allfh_rcatnum,10)
colnames(allfh_rcatnum)[4+nbyvar]='rcatnum' #change column number if byvars added

allfh=merge(allfh, allfh_rcatnum, by=c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x=T) #add byvars before svspp
#head(allfh,10)

#fix rcatnum for svspp ne 015 and ne 013
allfh$rcatnum=ifelse(allfh$svspp!=13 & allfh$svspp!=15, allfh$catnum, allfh$rcatnum)

#015 013 tot_catnum_stratum tot_catwgt_stratum fix, catch and tow numbers by stratum separated by sex
allfh=allfh[order(allfh$cruise6, allfh$stratum, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdsex, allfh$tot_tows_spp_stratum, allfh$tot_catnum_stratum, allfh$tot_catwgt_stratum),]  #add byvars before svspp


allfh_catstratum=aggregate(allfh$rcatnum, list('cruise6'=allfh$cruise6, 'stratum'=allfh$stratum, 'byvar1'=allfh$byvar1, 'byvar2'=allfh$byvar2, 'byvar3'=allfh$byvar3, 'byvar4'=allfh$byvar4, 'svspp'=allfh$svspp, 'pdsex'=allfh$pdsex, 'tot_tows_spp_stratum'=allfh$tot_tows_spp_stratum, 'tot_catnum_stratum'=allfh$tot_catnum_stratum, 'tot_catwgt_stratum'=allfh$tot_catwgt_stratum),sum,na.rm=T) #add byvars before svspp
#head(allfh_catstratum,10)
colnames(allfh_catstratum)[8+nbyvar]='dum_var'

allfh_catstratum=allfh_catstratum[order(allfh_catstratum$cruise6, allfh_catstratum$stratum, allfh_catstratum$byvar1, allfh_catstratum$byvar2, allfh_catstratum$byvar3, allfh_catstratum$byvar4, allfh_catstratum$svspp),] #add byvars before svspp

#keeps tot_tows_spp_stratum ala id statement in proc means
max_tot_tows_spp_stratum=aggregate(allfh_catstratum$tot_tows_spp_stratum, list('cruise6'=allfh_catstratum$cruise6, 'stratum'=allfh_catstratum$stratum, 'byvar1'=allfh_catstratum$byvar1, 'byvar2'=allfh_catstratum$byvar2, 'byvar3'=allfh_catstratum$byvar3,  'byvar4'=allfh_catstratum$byvar4, 'svspp'=allfh_catstratum$svspp),max) #add byvars before svspp
#head(max_tot_tows_spp_stratum,10)
colnames(max_tot_tows_spp_stratum)[4+nbyvar]='tot_tows_spp_stratum'

allfh_rcatstratum=aggregate(cbind(allfh_catstratum$tot_catnum_stratum, allfh_catstratum$tot_catwgt_stratum), list('cruise6'=allfh_catstratum$cruise6, 'stratum'=allfh_catstratum$stratum, 'byvar1'=allfh_catstratum$byvar1, 'byvar2'=allfh_catstratum$byvar2, 'byvar3'=allfh_catstratum$byvar3, 'byvar4'=allfh_catstratum$byvar4, 'svspp'=allfh_catstratum$svspp),sum,na.rm=T) #add byvars before svspp
#head(allfh_rcatstratum,10)
colnames(allfh_rcatstratum)[4+nbyvar]='rtot_catnum_stratum' #change column numbers if adding byvars
colnames(allfh_rcatstratum)[5+nbyvar]='rtot_catwgt_stratum' #change column numbers if adding byvars


allfh_rcatstratum=merge(allfh_rcatstratum, max_tot_tows_spp_stratum, by=c('cruise6', 'stratum', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x=T) #add byvars before svspp

allfh_rcatstratum=allfh_rcatstratum[order(allfh_rcatstratum$cruise6, allfh_rcatstratum$stratum, allfh_rcatstratum$byvar1, allfh_rcatstratum$byvar2, allfh_rcatstratum$byvar3, allfh_rcatstratum$byvar4, allfh_rcatstratum$svspp),] #add byvars before svspp
allfh_rcatstratum$rtot_tows_spp_stratum=allfh_rcatstratum$tot_tows_spp_stratum


allfh_rcatstratum2=subset(allfh_rcatstratum, select=-c(tot_tows_spp_stratum))

#merge in final rcatstratum numbers
allfh=merge(allfh, allfh_rcatstratum2, by=c('cruise6', 'stratum', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp'), all.x=T) #add byvars before svspp

#fix rcatstratum for svspp ne 015 and ne 013, also created rtot_tows_spp_stratum need to keep until for diet calc
allfh$rtot_catnum_stratum=ifelse(allfh$svspp!=13&allfh$svspp!=15, allfh$tot_catnum_stratum, allfh$rtot_catnum_stratum)
allfh$rtot_catwgt_stratum=ifelse(allfh$svspp!=13&allfh$svspp!=15, allfh$tot_catwgt_stratum, allfh$rtot_catwgt_stratum)
allfh$rtot_tows_spp_stratum=ifelse(allfh$svspp!=13&allfh$svspp!=15, allfh$tot_tows_spp_stratum, allfh$rtot_tows_spp_stratum)

#end of fixes for 13 and 15 catnums and catwgts and numbers per stratum

allfh=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdid, allfh$pdlen, allfh$tax),]

fish=aggregate(allfh$pyamt, list('cruise6'=allfh$cruise6, 'station'=allfh$station, 'byvar1'=allfh$byvar1, 'byvar2'=allfh$byvar2, 'byvar3'=allfh$byvar3, 'byvar4'=allfh$byvar4, 'svspp'=allfh$svspp, 'pdid'=allfh$pdid, 'pdlen'=allfh$pdlen, 'tax'=allfh$tax),sum,na.rm=T) #add byvars before svspp
#head(fish,10)
colnames(fish)[7+nbyvar]='pysum'

#count the number of prey taxa

fish=fish[order(fish$tax),]

taxcount=data.frame('tax'=unique(fish$tax))
taxcount

numpy=data.frame(cbind(taxcount,'num_prey'=nrow(taxcount)))
numpy

keep3=c('cruise6', 'station', 'pdid', 'pysum', 'byvar1',  'byvar2', 'byvar3', 'byvar4', 'svspp') #add byvars before svspp
pred= fish[keep3]
pred=pred[order(pred$cruise6, pred$station, pred$byvar1, pred$byvar2, pred$byvar3, pred$byvar4, pred$svspp, pred$pdid),] # add byvars before svspp
pred
one=aggregate(pred$pysum, list('cruise6'=pred$cruise6, 'station'=pred$station, 'byvar1'=pred$byvar1, 'byvar2'=pred$byvar2, 'byvar3'=pred$byvar3, 'byvar4'=pred$byvar4, 'svspp'=pred$svspp, 'pdid'=pred$pdid),head,1) #add byvars before svspp
#head(one,10)
colnames(one)[5+nbyvar]='pysum' #change column number if byvars added

one=one[order(one$cruise6, one$station, one$byvar1, one$byvar2, one$byvar3, one$byvar4, one$svspp),] #add byvars before svspp
#head(one,15)


new=aggregate(one$pysum, list('cruise6'=one$cruise6, 'station'=one$station, 'byvar1'=one$byvar1, 'byvar2'=one$byvar2, 'byvar3'=one$byvar3, 'byvar4'=one$byvar4, 'svspp'=one$svspp),length)  #add byvars before svspp
#head(new,10)
colnames(new)[4+nbyvar]='nostom' #change column number if byvars added

new=new[order(new$cruise6, new$station, new$byvar1, new$byvar2, new$byvar3, new$byvar4, new$svspp),] # add byvars before svspp

fish=fish[order(fish$cruise6, fish$station, fish$byvar1, fish$byvar2, fish$byvar3, fish$byvar4, fish$svspp),] # add byvars before svspp

fish=merge(fish, new, by=c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp')) #add byvars before svspp


fish=fish[order(fish$cruise6, fish$station, fish$byvar1, fish$byvar2, fish$byvar3, fish$byvar4, fish$svspp),] # add byvars before svspp


#Dataset fish now has the foodhabits data, plus the number
#of stomachs per cruise6 station combination and the number of prey taxa
#in data set

#Create prey codes to set up for the macro loops

pylist=cbind(taxcount,'pycode'=paste('prey',1:nrow(taxcount),sep=''))
pylist


fish=merge(pylist, fish, by=c('tax'), all.x=T)
#head(fish,10)

fish=fish[order(fish$cruise6, fish$station, fish$byvar1, fish$byvar2, fish$byvar3, fish$byvar4, fish$svspp, fish$pdid, fish$pdlen),] #add byvars before svspp


#Merge in the catch at length data
length1=allfh[order(allfh$cruise6, allfh$station, allfh$byvar1, allfh$byvar2, allfh$byvar3, allfh$byvar4, allfh$svspp, allfh$pdid, allfh$pdlen),] #add byvars before svspp

length2=aggregate(length1$stratum_area, list('cruise6'=length1$cruise6, 'station'=length1$station, 'byvar1'=length1$byvar1, 'byvar2'=length1$byvar2, 'byvar3'=length1$byvar3, 'byvar4'=length1$byvar4, 'svspp'=length1$svspp, 'pdid'=length1$pdid, 'pdlen'=length1$pdlen),head,1) #add byvars before svspp
#head(length2,10)
colnames(length2)[6+nbyvar]='stratum_area' #change column number if adding byvars

length2=merge(length2, length1, by=c('cruise6', 'station', 'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdid', 'pdlen', 'stratum_area'), all.x=T) #byvars before svspp
#head(length2,10)

keep4=c('cruise6', 'station', 'stratum', 'byvar1', 'byvar2', 'byvar3', 'byvar4','svspp',  'pdid', 'pdlen', 'catnum', 'rcatnum', 'tot_catnum_stratum', 'rtot_catnum_stratum', 'tot_catwgt_stratum', 'rtot_catwgt_stratum', 'tot_tows_spp_stratum', 'rtot_tows_spp_stratum', 'stratum_area', 'numlen', 'numlen_fin', 'rnumlen_fin') #byvars before svspp
keep4

length2=length2[keep4]
#head(length2,10)

length2=length2[order(length2$cruise6, length2$station, length2$byvar1, length2$byvar2, length2$byvar3, length2$byvar4, length2$svspp, length2$pdid, length2$pdlen),] #add byvars before svspp

fish=subset(fish, select=-c(tax))

fish=merge(fish, length2, by=c('cruise6','station',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'pdid', 'pdlen'), all.x=T) #byvars before svspp
#head(fish,10)

fish=fish[order(fish$cruise6, fish$stratum, fish$station, fish$byvar1, fish$byvar2, fish$byvar3, fish$byvar4, fish$svspp, fish$pdid,    fish$pdlen, fish$catnum, fish$rcatnum, fish$tot_catnum_stratum, fish$rtot_catnum_stratum, fish$tot_catwgt_stratum, fish$rtot_catwgt_stratum, fish$tot_tows_spp_stratum, fish$rtot_tows_spp_stratum, fish$stratum_area, fish$numlen, fish$numlen_fin, fish$rnumlen_fin, fish$nostom),]
#head(fish,10)

#transpose cast pycode data and merge table to fish
library(reshape2)
library(data.table)

fish=data.table(fish)

tran1=data.frame(dcast.data.table(fish,cruise6+stratum+station+byvar1+byvar2+byvar3+byvar4+svspp+pdid+pdlen+catnum+rcatnum+tot_catnum_stratum+rtot_catnum_stratum+tot_catwgt_stratum+rtot_catwgt_stratum+tot_tows_spp_stratum+rtot_tows_spp_stratum+stratum_area+numlen+rnumlen_fin+nostom~pycode, fun.aggregate=median, value.var='pysum') )   #add byvars before svspp

#head(tran1,10)
#set prey NA to zero in fish

#preycol=dput(as.character(pylist$pycode))
preycol=dput(as.character(pylist$pycode))

tran1[,preycol][is.na(tran1[,preycol])]=0




tran2=data.table(tran1)

new1=tran2[,lapply(.SD,mean, na.rm=T),by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp, pdid, pdlen, catnum, rcatnum, tot_catnum_stratum, rtot_catnum_stratum, tot_catwgt_stratum,  #add byvars before svspp
                                              rtot_catwgt_stratum, tot_tows_spp_stratum, rtot_tows_spp_stratum, stratum_area, numlen, rnumlen_fin, nostom)]

#NA for many rnumlen_fin records??

#remove nas  here
new1rm=new1[!is.na(new1$rnumlen_fin)]   
new1rm
new11=new1rm[,lapply(.SD,  weighted.mean,rnumlen_fin,na.rm=TRUE), by=list(cruise6,stratum, station, byvar1, byvar2, byvar3, byvar4, svspp,rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)]  #add byvars before svspp

#need to remove unnecessary columns
new11[,':='(pdid=NULL, pdlen=NULL ,catnum=NULL, tot_catnum_stratum=NULL, tot_catwgt_stratum=NULL, rtot_catwgt_stratum=NULL, tot_tows_spp_stratum=NULL, numlen=NULL, rnumlen_fin=NULL, nostom=NULL)]


#new11 already sorted by cruise6, stratum, station byvars svspp
#and py amts already means by station so no need for proc means here

#addition of new weighted estimator

new2=new11[,lapply(.SD,mean, na.rm=T), by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp, rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)]


musw2=new11[,lapply(.SD,function(x) x*rcatnum),by=list(cruise6, stratum, station, byvar1, byvar2, byvar3, byvar4, svspp,rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp


new2s=musw2[,lapply(.SD,sum ), by=list(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp,rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp


musw_strat=new2s[,lapply(.SD, function(x) x/rtot_tows_spp_stratum),by=list(cruise6, stratum, byvar1, byvar2, byvar3, byvar4, svspp,rcatnum, rtot_catnum_stratum, rtot_tows_spp_stratum, stratum_area)] #add byvars before svspp
musw_strat$munfish_strat=musw_strat$rtot_catnum_stratum/musw_strat$rtot_tows_spp_stratum
#retain munfish_strat
munfish_stratdat=musw_strat[, c('cruise6', 'stratum',  'byvar1', 'byvar2', 'byvar3', 'byvar4', 'svspp', 'munfish_strat'), with=F] #add byvars before 


musw_strat=musw_strat[order(musw_strat$svspp, musw_strat$byvar1, musw_strat$byvar2,musw_strat$byvar3,musw_strat$byvar4),] #byvars AFTER svspp


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

total=merge(output, totwt, by=c('svspp',  'byvar1', 'byvar2', 'byvar3', 'byvar4'), all.x=T)  #add byvars AFTER svspp

final=total[,':='(pycode=NULL, m_nfish_strat=NULL)]

final$relmsw=100*(final$meansw/final$totwt)
final$relmsw_s=100*(final$meansw_s/final$totwt_s)
final$ci=sqrt(final$variance/final$num_tows)*2
final$relci=(final$ci/final$totwt)*100
final$ci_s=sqrt(final$var_s/final$num_tows)*2
final$relci_s=(final$ci_s/final$totwt_s)*100

#remove strata-based metrics for now; only minor differences from original weighted metrics by tow  
final[,':=' (meansw_s=NULL, num_stra=NULL, var_s=NULL, cv_s=NULL, totwt_s=NULL, relmsw_s=NULL, ci_s=NULL, relci_s=NULL )]


if(unique(final$byvar1=='x')){final$byvar1=NULL} 
if(unique(final$byvar2=='x')){final$byvar2=NULL}
if(unique(final$byvar3=='x')){final$byvar3=NULL}
if(unique(final$byvar4=='x')){final$byvar4=NULL}

names(final)[names(final)=='byvar1']=name_byvar1                                  
names(final)[names(final)=='byvar2']=name_byvar2
names(final)[names(final)=='byvar3']=name_byvar3                                  
names(final)[names(final)=='byvar4']=name_byvar4                                  


#final


#merge nstoms to final

final2=merge(final, nstom, by=c('svspp', 'year'), all.x=T )

#export csv
#write.csv(final2, paste('allwt_year',SVSPP,'.csv', sep=''))

library(tidyverse)

diet <- final2[,c("year","prey","relmsw")]

compplot <- ggplot(diet, aes(year, relmsw, fill=prey)) + 
  #geom_bar(stat = "identity", aes(fill=fillwhite)) 
  geom_bar(stat = "identity") 
compplot + theme(legend.position="none")

dietmost <- diet %>%
  group_by(prey) %>%
  filter(max(relmsw)>1.0)

dietmain <- diet %>%
  group_by(prey) %>%
  filter(mean(relmsw)>5.0)

  

compplot2 <- ggplot(dietmain, aes(year, relmsw)) + 
  geom_bar(stat="identity")

compplot2 + facet_wrap("prey", nrow=10) + theme(legend.position="none")

compplot <- ggplot(dietmain, aes(year, relmsw, fill=prey)) + 
  #geom_bar(stat = "identity", aes(fill=fillwhite)) 
  geom_bar(stat = "identity") 
compplot #+ theme(legend.position="none")
