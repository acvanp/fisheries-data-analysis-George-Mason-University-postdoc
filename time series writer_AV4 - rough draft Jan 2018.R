# NGOMEX EwE time series writer
# Jan 25, 2018
# Alex Van Plantinga

# This is meant to be the most concise and efficient way to merge and aggregate
# SEAMAP data in order to get yearly EwE group biomass
# Admittedly, I learned R while working on this so my methods
# evolved from having too many new variables and for loops to 
# doing more targetted merging and agreggating.

# This work relies on having a species list or reference table for all of the
# species and species codes that belong to each EwE group/type/stanza.
#

setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex\\public_seamap_csvs\\2017")

# make updated glfrec_lw with L-W regression a's, b's and wt.kg columns

#glfrec <- read.csv("GLFREC.csv")
glfrec.lw <- read.csv("glfrec_lw2_julyupdate.csv")

biocodes <- read.csv("NEWBIOCODESBIG.csv")
starec <- read.csv("STAREC.csv")
bgsrec <- read.csv("BGSREC.csv")
invrec <- read.csv("INVREC.csv")

#start by taking the geographic subset of STAREC by coordinates

STATIONID <- starec$STATIONID
# s is for starting lat and lon, and e is for ending lat and lon of the trawl
slat <- as.numeric(as.character(starec$S_LATD)) + as.numeric(as.character(starec$S_LATM))/60
slon <- as.numeric(as.character(starec$S_LOND)) + as.numeric(as.character(starec$S_LONM))/60
elat <- as.numeric(as.character(starec$E_LATD)) + as.numeric(as.character(starec$E_LATM))/60
elon <- as.numeric(as.character(starec$E_LOND)) + as.numeric(as.character(starec$E_LONM))/60

year <- as.numeric(format(as.Date(starec$MO_DAY_YR, format = "%Y-%m-%d"), "%Y"))

station.df <- data.frame(STATIONID, slat, slon, elat, elon, year)

area <- station.df[which(station.df$slat > 27.53 &
                           station.df$elat > 27.53 &
                           station.df$slon > 87.7 &
                           station.df$elon > 87.7 &
                           station.df$slon < 94.7 &
                           station.df$elon < 94.7 ),]
library(geosphere)

library("sp")

# make a data frame for the values that
# go into the distm function
pts <- data.frame(area$slon, area$slat, area$elon, area$elat)

#initial value for distance column
dist_stat <- 0

# use for-loop to go row-by-row and make an
# array of distances for each row in mydata1
# so distance values are the same as long as
# the station number is the same

for(i in 1:length(area$STATIONID)) {
  dist_stat[i] <- distm(c(pts[i,1], pts[i,2]), c(pts[i,3], pts[i,4]), fun=distHaversine)
}

distance.df <- data.frame(dist_stat, area)

# aggregate glfrec.lw weight column by species of fish per station
# using BGSID value, taking the mean weight which can be multiplied
# by extrapolated count
df = aggregate(glfrec.lw$wt.kg ~ glfrec$BGSID, FUN  = mean)
colnames(df) = c("BGSID", "wt")
df = merge(df, bgsrec, by = "BGSID")
data <- merge(distance.df, df, by = "STATIONID")
data = merge(data, invrec, by = "STATIONID")

# then merge that data frame with the fish weight data
# make a column of metric tons per square kilometer
data$tonnes.bgs.avg <- (data$wt/1000)
# divide meters by 1000 for km and multiply gear size by 0.3 and divide by 1000 
# to convert ft to km, multiply km of net by km of trawl distance for sqkm 
data$sq.km <- (data$dist_stat/1000) * ((data$GEAR_SIZE*0.3048)/1000)

# cannot divide by area in this data frame because it does not account for zero catch
# call tonnes of average fish per species per catch times the number of those fish
# the extrapolated tonnes per species per catch
data$tonnes.exp <- data$tonnes.bgs.avg*data$CNTEXP

setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex\\")

# the species.reference is the species reference table populated from various web sources including Fishbase
spec.reference <- read.csv("updated_master_data_fromFishBase_AV1.csv")
species.ref <- spec.reference
species <- unique(data$BIO_GLF)
type <- unique(spec.reference$Type)
data$CODE = data$BIO_BGS
data = merge(data, species.ref, by = "CODE") #merge data and species reference table to sort data by model Type or group
setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//Fisheries//Landings")
poolcodes <- read.csv("Jan2018BalancedEcopathLandings.csv")

# aggregate sum of tonnes
data = aggregate(data$tonnes.exp ~ data$Type + data$year, FUN = sum)


# area trawled per year put in table for quick reference
# and divide aggregate yearly catches by in the loop below
station.area <- aggregate(data$sq.km ~ data$STATIONID + data$year, FUN  = mean)
area.yearly = aggregate(station.area$`data$sq.km`~ station.area$`data$year`, FUN = sum)
# rename columns so they are manageable
colnames(area.yearly) = c("yr", "sqkm")

# the purpose of the loop below is to account for zero catch and put all
# the EwE group time series into a spreadsheet

yr <- seq(1950, 2016)

ts <- data.frame(yr)
ts <- rbind("pool code", ts)

c = 2
for(i in 2:nrow(poolcodes)){
 
  if(poolcodes$Group.and.Dominant.Stanza[i] == ""){next}
  r <- poolcodes[i,]
  p <- as.character(r$Group.and.Dominant.Stanza)
  # take all species from that pool code group or Type
  # type and group are the same thing here
  sp <- species.ref[which(species.ref$Type == p),]
  glf <- data[which(as.character(data$`data$Type`) == as.character(r$Group.and.Dominant.Stanza)),]
  
  glf$cpue = glf$`data$tonnes.exp`/
    area.yearly$sqkm[which(area.yearly$yr %in% 
                             glf[which(!is.na(glf$`data$tonnes.exp`)),2])]
  
  # DOUBLE the biomass to account for trawl undercatch or bias
  glf$cpue = 2*glf$cpue
  
  df.y <- data.frame(yr)
  df.y$cpue <- NA
  
  # taking biomass in mt from those years and putting it in the 
  # 1950-2016 data frame
  df.y$cpue[which(df.y$yr %in% glf$`data$year`)] = glf$cpue[which(as.numeric(glf$`data$year`) %in% as.numeric(df.y$yr))]

  
  df <- data.frame(df.y[,2], stringsAsFactors = FALSE)#need to use string as factors to prevent pool code from showing up as NA
  if(length(na.omit(as.numeric(df$df.y...2.))) == 0){next}
  df <- rbind(as.character(r$code), df) # arrange the data with the column name and a row for the EwE pool code
  ts <- cbind(ts, df)
  colnames(ts)[c] <- as.character(r$Group.and.Dominant.Stanza)
  c <- c + 1
  
}

ts.seamap = ts # set this biomass time series aside as ts.seamap  
# ts of raw non-lowess values is read
# now make a lowess smooth table

tsl <- data.frame(yr) # call the LOWESS time series "tsl"
tsl <- rbind("pool code", tsl)
c <- 2


for(i in 2:ncol(ts)){
  r <- poolcodes[which(poolcodes$code == ts[1,i]),]
  
  df = data.frame(ts$yr, as.numeric(ts[,i]))
  df = df[seq(2,nrow(df)),]
  
  df.b = data.frame()
  #get rid of blanks
  for(j in 1:nrow(df)){
    if(is.na(df[j,2])){next}
    df.b = rbind(df.b, df[j,])
  }
  
  #make lowess values
  df.l <- data.frame(df.b[,1], lowess(df.b$ts.yr, df.b$as.numeric.ts...i..)[2])
  df.a = data.frame(yr)
  df.a$cpuelowess = NA
  for(j in 1:nrow(df.a)){
    if(df.a$yr[j] %in% df.l$df.b...1.)
      {df.a$cpuelowess[j] = df.l[which(df.l$df.b...1. == df.a$yr[j]),][,2] }
    
  }
  
  df <- data.frame(df.a[,2], stringsAsFactors=FALSE)#need to use string as factors to prevent pool code from showing up as NA
  df <- rbind(r$code, df)
  tsl <- cbind(tsl, df)
  colnames(tsl)[c] <- as.character(r$Group.and.Dominant.Stanza)
  c <- c + 1
  
  
}



##########################################################################################

# make landings time series for species that are fished in the NGOMEX model
# https://www.st.nmfs.noaa.gov/pls/webpls/MF_MONTHLY_LANDINGS.RESULTS
# Alex Van Plantinga 8/2/17
# I just copied and pasted the NOAA table into Excel and saved as CSV

setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//Fisheries//Landings")

landings <- read.csv("NOAAlandings_1950-2015annual.csv") # this file is the output of querying the NOAA landings for Louisiana for 1950-2015 for all species on a yearly basis

targets <- read.csv("Jan2018BalancedEcopathLandings.csv")


setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex\\")

# we first need to make a column in landings that labels the fish species
# in a way that is common with either SEAMAP or EwE names or categories

spec.reference <- read.csv("updated_master_data_fromFishBase_AV1.csv")
species.ref <- spec.reference
type <- unique(spec.reference$Type)


# parse the common names from NOAA table
name2 <- gsub("([A-Za-z]+).*", "\\1", landings$Species)
name1 <- tolower(sub('^.* ([[:alnum:]]+)$', '\\1', landings$Species))
name1[which(name1 == "atlantic")] <- "Atlantic"
landings$second.word <- tolower(gsub("([A-Za-z]+).*", "\\1", landings$Species))
landings$common.name <- paste(name1, tolower(name2))

# for entries that have one-word common names, replace the clumsy redundant double word names with single word names
library(qdap)

for( i in 1:nrow(landings)){
  
  if( gsub("([A-Za-z]+).*", "\\1",landings$common.name[i]) == 
      sub('^.* ([[:alnum:]]+)$', '\\1', landings$common.name[i]) )
  {landings$common.name[i] <- gsub("([A-Za-z]+).*", "\\1",landings$common.name[i])}
  
}

landings$common.name[which(landings$common.name == "menhaden")] <- "Gulf menhaden"

landings$type <- NA

for( i in 1:nrow(landings)){
  fish <- NA
  fish <- species.ref[which(species.ref$common_name == landings$common.name[i]), ]
  if(nrow(fish) == 0){next}
  landings$type[i] <- as.character(fish$Type)
  
}
# manually enter in some identifiers
landings$type[which(landings$second.word %in% c("tuna", "tunas"))] = "tunas"
landings$type[which(landings$second.word %in% c("ray", "rays"))] = "rays"


landings$type[which(landings$common.name == "cero mackerel")] <- "mackerel"

landings$type[which(landings$common.name == "squids")] <- "squid"
landings$type[which(landings$common.name == "pink shrimp")] <- "pink shrimp"
landings$type[which(landings$common.name == "white shrimp")] <- "white shrimp"
landings$type[which(landings$common.name == "brown shrimp")] <- "brown shrimp"
landings$type[which(landings$common.name %in%
                      c("snappers", "yellowtail snapper", 
                        "vermillion snapper", "silk snapper", 
                        "queen snapper", "mullet snapper", 
                        "black snapper", "blackfin snapper", "cubera snapper",
                        "dog snapper"))] = "other snapper"
landings$type[which(landings$common.name == "red snapper")] <- "red snapper"
landings$type[which(landings$common.name == c("amberjack", "lesser amberjack"))] <- "Carangidae"
landings$type[which(landings$common.name %in% c("seabob shrimp", "red shrimp", "rock shrimp", "other shrimp"))] <- "other shrimp"
landings$type[which(landings$second.word == "shad")] = "clupeids"
landings$type[which(landings$second.word == "mullet")] = "mullet"
landings$type[which(landings$second.word %in% c("shark", "sharks"))] = "sharks"
landings$type[which(landings$second.word %in% c("turtle", "turtles"))] = "sea turtles"

landings$type[which(landings$common.name == "catfishes sea")] = "catfish"

yr <- seq(1950,2016)
ts <- data.frame(yr)
ts <- rbind("stanza", "pool code", "type", ts)
for ( i in 1:nrow(targets)){
  r = NA
  r <- targets[i,]
  g = NA
  g = targets[which(as.character(targets$multistanza) == as.character(r$Group)),]
  if(nrow(g) == 0){g = r}
  print(i)
  if(!r$Group %in% landings$type){next} # skip things with no landings data
  if(sum(g$Total) == 0){next} # skip things that are considered not caught in the model
  s <- NA
  t <- NA
  a <- NA
  lw <- NA
  year <- NA
  catch <- NA
  df.b <- data.frame(year, catch)
  
  ldata <- landings[which (landings$type == as.character(r$Group)),]
  
  
  for(ii in 1:length(yr)){
    #these little loops keep each time series the same dimensions for easier cutting/copying/pasting
    df.b[ii,1] <- yr[ii]
    aa = try(ldata[which(ldata$Year == yr[ii]),], silent = TRUE)
    df.b[ii,2] = sum(as.numeric(gsub(",", "",aa$Metric.Tons)))/118500 
    #use as.numeric because there are "factors" hidden in the aa$ values
    # ArcGIS indicates the model area is about 118500 square kilometers
    # convert metric tonnes to tonnes per square kilometer
  }
  df <- df.b[2]
  if(nrow(g) == 0){g = r}
  g$proportion = g$Total / sum(g$Total)
  df.stanza = NA
  df.group = NA
  for(j in 1:nrow(g)){
    if(g$Total[j] == 0){next}
    df.stanza = rbind(as.character(g$stanza[j]), as.character(g$code[j]), "6", df*g$proportion[j])
    df.group = cbind(df.group, df.stanza)
  }
  # get rid of NA values
  df.group = df.group[,2:ncol(df.group)]
  
  ts <- cbind(ts, df.group)
  
}

# set aside the landings ts as ts.landings
ts.landings = ts

# get rid of NA and zero 0
for(i in 1:ncol(ts.landings)){
  
  colnames(ts.landings[i]) = as.character(unlist(list(ts.landings[1,])[[1]][i][1,]))
  
}

##############################################################################

# Swap out 1984-2017 timescale for the 1950-2016 timescale
setwd("C:/Users/avanplan/Dropbox/NGOMEX Model components/Fisheries/Stock Assessments/data and r scripts")
#_______________________________________________________________
# use v2 for Kim's balanced Janyuary 2018 values and then use v3 for the more recent more accurate values
SEDARcpue = read.csv("compilation of stock assessment biomass - v2.csv")
#______________________________________________________
df = data.frame(seq(1:nrow(SEDARcpue)))
for(i in 1:ncol(SEDARcpue)){
  df = cbind(df,as.numeric(as.character(SEDARcpue[,i])))
}
df = df[2:nrow(df),2:ncol(df)]
df[,c(3,7)] = df[,c(3,7)]/118500 # use model area, not Gulf area of 700000 sqkm for sea trout and menhaden
df[,c(2,4,5,6)] = df[,c(2,4,5,6)]/700000 # use Gulf area of 700000 sqkm

colnames(df) = as.character(unlist(SEDARcpue[1,]))

for(i in 2:ncol(df)){
  ts[,which(colnames(ts) == colnames(df[3]))]
}

# add the absolute biomass code 1 to the ts.seamap data frame
ts.seamap = rbind(colnames(ts.seamap), ts.seamap[1,], 1, ts.seamap[2:nrow(ts.seamap),])

# replace stock assessment species with their stock assessment time series 
ts.biomass = ts.seamap # in a new data frame called ts.biomass
replacements = targets[which(targets$Group.and.Dominant.Stanza %in% colnames(df)[2:ncol(df)]),]
ts.biomass[,which(ts.biomass[2,] %in% replacements$code)][4:70,] = NA #first blank out the seamap numbers that are going to be replaced

for(i in 1:(ncol(df)-1)){
  print(i)
  replacement.code = as.numeric(targets[which(targets$Group.and.Dominant.Stanza == colnames(df[2:ncol(df)])[i]),][1])
  print(replacement.code)
  print(colnames(df[2:ncol(df)])[i])
  ts.biomass[,which(ts.biomass[2,] == replacement.code)][4:70] = df[,i+1][1:67]
  
}

######################################################################
#LOWESS ts reformatting
# same thing as above for the non lowess as now for the LOWESS
tsl = rbind(colnames(tsl), tsl[1,], 1, tsl[2:nrow(tsl),])

# replace stock assessment species with their stock assessment time series
tsl.biomass = tsl
replacements = targets[which(targets$Group.and.Dominant.Stanza %in% colnames(df)[2:ncol(df)]),]
tsl.biomass[,which(tsl.biomass[2,] %in% replacements$code)][4:70,] = NA

for(i in 1:(ncol(df)-1)){
  print(i)
  replacement.code = as.numeric(targets[which(targets$Group.and.Dominant.Stanza == colnames(df[2:ncol(df)])[i]),][1])
  print(replacement.code)
  print(colnames(df[2:ncol(df)])[i])
  tsl.biomass[,which(tsl.biomass[2,] == replacement.code)][4:70] = df[,i+1][1:67]
  
}

##############################################################################
# make a fishing mortality ts 1950-2016 to cbind to the biomass and landings tables
setwd("C:/Users/avanplan/Dropbox/NGOMEX Model components/Fisheries/Effort and Fishing Mortality")
library(lubridate)
rawF = read.csv("fishing mortality AV Dec 29.csv")

setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//Fisheries//Landings")

targets <- read.csv("Jan2018BalancedEcopathLandings.csv")

yr <- seq(1950,2016)
ts <- data.frame(yr)
ts <- rbind("stanza", "pool code", "type", ts)

# this for loop proportions out the F from the one-F-per-group rawF table
# to a proportions of F per stanza for the multistanza groups according to the Ecopath landings proportions
# of multistanza table.

for(i in 2:ncol(rawF)){
  
  if(sum(as.numeric(na.omit(rawF[,i]))) <= 0){next}
  
  r = NA
  r <- targets[which(as.character(targets$Group) == as.character(rawF[1,i])),]
  ldata <- data.frame(as.numeric(as.character(rawF[4:70,i])))
  if(nrow(na.omit(ldata)) == 0){next}
  g = NA
  g = targets[which(as.character(targets$multistanza) == as.character(r$Group)),]
  if(nrow(g) == 0){g = r}
  print(i)
  
  if(sum(g$Total) == 0){next} # skip things that are considered not caught in the model
  df <- data.frame(ldata)
  if(nrow(g) == 0){g = r}
  g$proportion = g$Total / sum(g$Total)
  df.stanza = NA
  df.group = NA
  for(j in 1:nrow(g)){
    if(g$Total[j] == 0){next}
    df.stanza = rbind(as.character(g$stanza[j]), g$code[j], "4", df*g$proportion[j])
    df.group = cbind(df.group, df.stanza)
  }
  
  # get rid of NA values
  df.group = data.frame(df.group[,2:ncol(df.group)])
  
  ts <- cbind(ts, df.group)
  
  
}


# replace menhaden data with stock assessment data
setwd("C:/Users/avanplan/Dropbox/NGOMEX Model components/Fisheries/Stock Assessments/data and r scripts")
x=dget('gm-020.rdata')

menhadenF = cbind(x$F.age[,1], x$F.age[,2], x$F.age[,3], x$F.age[,4])
ts[4:70, which(ts[2,] %in% targets[which(targets$multistanza == "Gulf menhaden"),][,1])] = NA
ts[31:69, which(ts[2,] %in% targets[which(targets$multistanza == "Gulf menhaden"),][,1])] = menhadenF

ts.F = ts #set aside a multistanza Fishing mortality table

setwd("C:/Users/avanplan/Dropbox/NGOMEX Model components/Fisheries/Effort and Fishing Mortality")
#write.csv(ts,"yearly fishing mortality with EwE format Jan 2018 av2.csv")

#################################################################################################
# cbind the biomass(SEAMAP and SEDAR), landings(NOAA), and fishing mortality data
# only use the date column from the first table, and 2:ncol(table) for the next tables cbind together
ts.combined = cbind(ts.landings, ts.biomass[,2:ncol(ts.biomass)], ts.F[,2:ncol(ts.F)])

tsl.combined = cbind(ts.landings, tsl.biomass[,2:ncol(tsl.biomass)], ts.F[,2:ncol(ts.F)])


# replace clupeids, small forage fish, benthic crabs, and benthic invertebrates 1 (absolute biomass) with 0 (relative biomass)
ts.combined[3 , which(ts.combined[1,] %in% c("clupeids", "small forage fish", "benthic crabs", "benthic invertebrates"))] = 0
tsl.combined[3 , which(tsl.combined[1,] %in% c("clupeids", "small forage fish", "benthic crabs", "benthic invertebrates"))] = 0


setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\EwE-Alex\\new inputs\\Ecosim")

#write.csv(ts.combined, "1950 ts combined non lowess.csv")

#write.csv(tsl.combined, "1950 ts combined LOWESS.csv")

# set aside tables of the ts and tsl - combined that are just for 2000-2016
ts.combined.2000 = rbind(ts.combined[c(1,2,3),], ts.combined[which(ts.combined[,1] %in% seq(2000,2016)),])

tsl.combined.2000 = rbind(tsl.combined[c(1,2,3),], tsl.combined[which(ts.combined[,1] %in% seq(2000,2016)),])


#write.csv(ts.combined.2000, "2000 ts combined non lowess.csv")

#write.csv(tsl.combined.2000, "2000 ts combined LOWESS.csv")

# get average 2000-2005 values

# this spreadsheet has been modified from the ts above to inlcude stock assessment data
# instead of SEAMAP for several species, including: steatrout, red snapper, 
#  Gulf menhaden, brown shrimp, white shrimp, pink shrimp

tsslice <- ts.biomass[which(ts.biomass[,1] %in% seq(2000,2005)),]
biomass = data.frame()

# loop through 
for (i in 1:(ncol(ts.biomass)-1)){
  biomass[i,1] = ts.biomass[1,(i+1)]
  biomass[i,2]  = mean(na.omit(as.numeric(tsslice[,(i+1)])))
  print(i)
  
}
biomass$category = "biomass"
setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\EwE-Alex\\new inputs\\Ecopath")
#write.csv(biomass, "Jan 2018 biomass.csv")

