# Alex Van Plantinga
# 2017
# This program takes internet data from Fishbase.org
# and populates a table about fish taxa life history information
# like length at maturity.
# R accesses the website and uses regex to take the right values from 
# the HTML text.


setwd("C:/Users/avanplan/Desktop/Alex SEAMAP")

#******************************************************
# **run species_reference_compiler_AV1.R first!**
# then run landings script second,
# then run this fishbase script
# this has been updated to need the biomas_NGOMEX scripts run beforehand
# to generate a centralized master dataset for what is relevant from SEAMAP
# and what data can be brought in from fishbase to populate the basic input
#****************************************************


#make a Type column in all.species.cpue
for (i in 1:nrow(all.species.cpue)){
  try(x <- species.ref[which(species.ref$BIO_GLF == all.species.cpue$CODE[i]),], silent = TRUE)
  if(nrow(x) < 1) next
  all.species.cpue$Type[i] <- as.character(x$Type[1])
}

# calculate Q/B numbers from fishbase
# aspect ratio, trophic level, and assumed
# temperature of 23C, and maxWt from SEAMAP len and a b data.

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
library("rvest")
library(rfishbase)

# make a taxonomic family column
all.species.cpue$family.code <- NA
all.species.cpue$subfamily <- NA
y <- data.frame() #initial value of y
for (i in (1):nrow(all.species.cpue)){
  genus <- gsub("([A-Za-z]+).*", "\\1",all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  
  fish <- paste(genus, species)
  try(x <- species(fish), FALSE)
  # include invertebrate information from sealifebase
  if(nrow(x) == 0) {y <- sealifebase[which(sealifebase$Genus == genus),]}
  if(nrow(x) > 0) {all.species.cpue$family.code[i] <- x$FamCode}
  if(nrow(x) > 0) {all.species.cpue$subfamily[i] <- x$Subfamily}
  if(nrow(y) == 0) next
  if(nrow(x) == 0) {all.species.cpue$family.code[i] <- y$FamCode}
  if(nrow(x) == 0) next

}

# Add a column for fishbase values of Q/B (should also list references), rather than values I calculate myself
all.species.cpue$QB.fishbase <- NA

for (i in 1:nrow(all.species.cpue)){
  genus <- gsub("([A-Za-z]+).*", "\\1",all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  fish <- paste(genus, species)
  url <- paste('http://www.fishbase.se/summary/', genus, "-", species, ".html", sep = "")
  session <- html_session(url)
  ecology <- NA #need to clear the ecology rule because old values will carry over and populate fields for fish other than what this iteration was intended for
  try(ecology <- session %>% follow_link("Food consumption"), silent = TRUE) #this is the part that breaks the loop. Use try()
  if(is.na(ecology)) next # if the try() failed then it doesn't break the loop but ecology is still NA so don't continue the loop if it's NA
  s <- read_html(ecology) %>% html_nodes("body") %>% html_text()
  s <- strsplit(s[[1]], split = "Ref")[[1]][2]# takes you to the part of the html that has trophic level if there is a trophic level given
  s <- strsplit(s, split = "[\\\\]|[^[:print:]]")[[1]][5]
  t <- as.numeric(s)
  all.species.cpue$QB.fishbase[i] <- t
}

# make an Lm column
all.species.cpue$Lm <- NA
# run this with strsplit "," and then with "&" then "[.]"
for (i in 1:nrow(all.species.cpue)){
  if(!is.na(all.species.cpue$Lm[i]))next# skip what is already there
  genus <- gsub("([A-Za-z]+).*", "\\1",all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  fish <- paste(genus, species)
  url <- paste('http://www.fishbase.se/summary/', genus, "-", species, ".html", sep = "")
  session <- NA
  session <- html_session(url)
  ecology <- NA #need to clear the ecology rule because old values will carry over and populate fields for fish other than what this iteration was intended for
  try(ecology <- session %>% follow_link("Maturity"), silent = TRUE)# navigates to the Maturity page
  try(s <- ecology %>% html_nodes("body") %>% html_text(), silent = TRUE) 
  try(s <- strsplit(s[[1]], split = "TL"), silent = TRUE)
  try(s <- strsplit(s[[1]], split = "\t"), silent = TRUE)# takes you to the part of the html that has trophic level if there is a trophic level given
  x <- NA
  y <- NA
  try(x <- as.numeric(s[[1]][length(s[[1]])]), silent = TRUE)
  try(y <- as.numeric(s[[2]][length(s[[2]])]), silent = TRUE)
  t <- min(x,y)
  if(is.na(t))next
  all.species.cpue$Lm[i] <- t
}

#try this with sealifebase?

#family aggregated Lm
all.species.cpue$Lm.family.ag <- NA
for(i in 1:nrow(all.species.cpue)){
  x <- all.species.cpue[which(all.species.cpue$family.code == all.species.cpue$family.code[i]),]
  if(length(na.omit(x$Lm)) < 1) next
  all.species.cpue$Lm.family.ag[i] <-mean(na.omit(x$Lm))
}

# make an aspect ratio "A" column
all.species.cpue$A <- NA
for (i in (1):nrow(all.species.cpue)){
  genus <- gsub("([A-Za-z]+).*", "\\1",all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  
  fish <- paste(genus, species)
  
  x <- morphometrics(fish)
  if(nrow(x) == 0)next #skip rows for which there are no avaialable fishbase
  all.species.cpue$A[i] <- mean(x$AspectRatio)
}

#Atlantic cutlassfish aspect ratio is zero, enter manually
all.species.cpue[which(all.species.cpue$common_name=="Atlantic cutlassfish"),]$A <- 0
 

# get max Wt from SEAMAP as weight calculated from maximum observed length, which often exceeds the Winf or Linf in fishbase
all.species.cpue$Wtinf.SEAMAP.grams <- NA
for (i in 1:nrow(all.species.cpue)){
  lw <- glfrec.lw[which(glfrec.lw$BIO_GLF == all.species.cpue$CODE[i]),]
  maxlen <- max(na.omit(lw$LEN_GLF))
  a <- lw$a[1]
  b <- lw$b[1]
  #use max length value, in mm, which converts to kg using the aL^b equation.
  x <- a*maxlen^b
  
  all.species.cpue$Wtinf.SEAMAP.grams[i] <- x*1000 #convert kg to grams
}


# make a trophic level column populated from rfishbase ecology() function
all.species.cpue$trophic.level <- NA
for (i in 1:nrow(all.species.cpue)){
  genus <- gsub("([A-Za-z]+).*", "\\1", all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  
  fish <- paste(genus, species)
  x <- ecology(fish)
  #detritivores troph < 2.2; herbivores troph < 2.8; carnivores troph > 2.8
  if (length(x) > 0) {all.species.cpue$trophic.level[i] <- x$FoodTroph}
  if (length(x) == 0) next
}  

# the only reason to use the loop directly below is to find the
# trophic levels of crabs and shrimps.

#************************************************************
# When trophic level is 2.2-2.8, Q/B is highest (herbivory),
# and Q/B is also high when TL < 2.8 in general (herbivory or detrivory)
# and Q/B is lowest when TL > 2.8 (carnivory)
# So if you see high crab TL (>2.8) you would expect low Q/B
# and if you see low Gulf menhaden TL (2.1) then expect higher Q/B 
# based on Palomares and Pauly (1998)
#***********************************************************
#the next loop needs to use the try(catch) to suppress errors when the link I am looking for isn't present
# populate trophic level values for crabs and shrimps from sealifebase
for (i in 1:nrow(all.species.cpue)){
  if(!is.na(all.species.cpue$trophic.level[i]))next
  genus <- gsub("([A-Za-z]+).*", "\\1",all.species.cpue$TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', all.species.cpue$TAXONOMIC[i])
  species <- tolower(species)
  fish <- paste(genus, species)
  url <- paste('http://www.sealifebase.org/summary/', genus, "-", species, ".html", sep = "")
  session <- html_session(url)
  ecology <- NA
  try(ecology <- session %>% follow_link("Ecology"), silent = TRUE) #this is the part that breaks the loop. Use try()
  if(is.na(ecology)) next
  s <- read_html(ecology) %>% html_nodes("body") %>% html_text()
  s <- strsplit(s[[1]], split = "From individual food items")[[1]][[2]]# takes you to the part of the html that has trophic level if there is a trophic level given
  s <- strsplit(s, split = " ")[[1]][1]
  t <- as.numeric(s)
  all.species.cpue$trophic.level[i] <- t
  
}

# make general rule about K in relation to TL
# start by making 
TL.df <- data.frame(all.species.cpue$TAXONOMIC, all.species.cpue$common_name, all.species.cpue$trophic.level)
TL.df$K <- NA
all.species.cpue$K <- NA
for (i in 1:nrow(TL.df)){
  genus <- gsub("([A-Za-z]+).*", "\\1",TL.df$all.species.cpue.TAXONOMIC[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', TL.df$all.species.cpue.TAXONOMIC[i])
  species <- tolower(species)
  fish <- paste(genus, species)
  url <- paste('http://www.fishbase.se/summary/', genus, "-", species, ".html", sep = "")
  session <- html_session(url)
  ecology <- NA #need to clear the ecology rule because old values will carry over and populate fields for fish other than what this iteration was intended for
  try(ecology <- session %>% follow_link("Growth"), silent = TRUE) #this is the part that breaks the loop. Use try()
  if(is.na(ecology)) next # if the try() failed then it doesn't break the loop but ecology is still NA so don't continue the loop if it's NA
  s <- read_html(ecology) %>% html_nodes("body") %>% html_text()
  s <- strsplit(s[[1]], split = "K = ")[[1]][[2]]# takes you to the part of the html that has trophic level if there is a trophic level given
  s <- strsplit(s, split = "[\\\\]|[^[:print:]]")[[1]][1]
  t <- as.numeric(s)
  TL.df$K[i] <- t
  all.species.cpue$K[i] <- t
}
plot(TL.df$K, TL.df$all.species.cpue.trophic.level)
summary(lm(TL.df$K~ TL.df$all.species.cpue.trophic.level))


# Take aggregate means by Type (EwE identifier) of the aspect ratio, trophic level,
# and Wtinf that go into the Q/B equation
library(data.table)
library(base)
A.ag <- aggregate(all.species.cpue$A~all.species.cpue$Type, FUN = mean)
A.ag$Type <- A.ag$`all.species.cpue$Type`
A.ag$A.agmean <- A.ag$`all.species.cpue$A`
Type <- all.species.cpue$Type
Type <- data.frame(Type) #make a separate data frame for Type. This way we don't have to fill up the all.species.cpue with garbage columns from the merge() function

Type$rownr <- as.numeric(rownames(all.species.cpue)) #use row number to keep data frame rows aligned correctly

xmerge <- merge(A.ag, Type, "Type", all = TRUE) #Take the xmerge column (making sure to have all=TRUE rows accounted for)
setorder(xmerge, 'rownr') #order the rows in the same order as in the all.species.cpue table, so rows are aligned correct species to correct species

all.species.cpue$A.agmean <- xmerge$A.agmean #add just the column we need A.agmean to the all.species.cpue data frame

# Same thing with Winf. 
# Use aggregate mean of Winf by type of fish (e.g., clupeids)
Ag.wt.seamap <- aggregate(all.species.cpue$Wtinf.SEAMAP.grams~all.species.cpue$Type, FUN = mean)
Ag.wt.seamap$Type <- Ag.wt.seamap$`all.species.cpue$Type`
Ag.wt.seamap$Wtinf.agmean.seamap <- Ag.wt.seamap$`all.species.cpue$Wtinf.SEAMAP.grams`
xmerge <- merge(Ag.wt.seamap, Type, "Type", all = TRUE)
setorder(xmerge, 'rownr') #order the rows in the same order as in the all.species.cpue table, so rows are aligned correct species to correct species

all.species.cpue$Wtinf.agmean.seamap <- xmerge$Wtinf.agmean.seamap


# Same thing with aggregate mean of trophic level
troph.ag <- aggregate(all.species.cpue$trophic.level ~ all.species.cpue$Type, FUN = mean)
troph.ag$Type <- troph.ag$`all.species.cpue$Type`
troph.ag$Trophic.level.agmean <-troph.ag$`all.species.cpue$trophic.level`
xmerge <- merge(troph.ag, Type, "Type", all = TRUE)  
setorder(xmerge, 'rownr') #order the rows in the same order as in the all.species.cpue table, so rows are aligned correct species to correct species

all.species.cpue$Trophic.level.agmean <- xmerge$Trophic.level.agmean


# Properly use the Palomares and Pauly 1998 equation by including trophic level -
# try doing this with a loop
all.species.cpue$QB.indiv <- NA
for (i in 1:nrow(all.species.cpue)){
  if (is.na(all.species.cpue$trophic.level[i])) {t <- 0
  }else if (all.species.cpue$trophic.level[i] > 2.8) {t <- 0
  }else if (all.species.cpue$trophic.level[i] < 2.8 & all.species.cpue$trophic.level[i] > 2.2) {t <- 0.532
  }else t <- 0.398
  if(is.na(all.species.cpue$A)) next
  all.species.cpue$QB.indiv[i] <- 10^(7.964 - 0.204*log10(all.species.cpue$Wtinf.SEAMAP.grams [i])-1.965 * 
                                        (1000/(23+273.15)) + 0.083*all.species.cpue$A[i] + t)
}

# Make Q/B using PP98 for Type aggregated values
for (i in 1:nrow(all.species.cpue)){
  if (is.na(all.species.cpue$Trophic.level.agmean[i])) {t <- 0
  }else if (all.species.cpue$Trophic.level.agmean[i] > 2.8) {t <- 0
  }else if (all.species.cpue$Trophic.level.agmean[i] < 2.8 & all.species.cpue$Trophic.level.agmean[i] > 2.2) {t <- 0.532
  }else t <- 0.398
  
  all.species.cpue$QB.agtype[i] <- 10^(7.964 - 0.204*log10(all.species.cpue$Wtinf.agmean.seamap [i])-1.965 * 
                                         (1000/(23+273.15)) + 0.083*all.species.cpue$A.agmean[i] + t)
}

#aggregate paramters by family, slightly more generalized than aggregate by type
library(data.table)
A.ag <- aggregate(all.species.cpue$A~all.species.cpue$family.code, FUN = mean) #aggregate
A.ag$family.code <- A.ag$`all.species.cpue$family.code` #rename columns
A.ag$A.agmean <- A.ag$`all.species.cpue$A`#rename columns
family.code <- all.species.cpue$family.code

family.code <- data.frame(family.code) #make a separate data frame for family.code. This way we don't have to fill up the all.species.cpue with garbage columns from the merge() function
family.code$rownr <- as.numeric(rownames(all.species.cpue))

xmerge <- merge(family.code, A.ag, "family.code", all = TRUE) #Take the xmerge column (making sure to have all=TRUE rows accounted for)
setorder(xmerge, 'rownr') #order the rows in the same order as in the all.species.cpue table, so rows are aligned correct species to correct species

all.species.cpue$A.fam.agmean <- xmerge$A.agmean #add just the column we need A.agmean to the all.species.cpue data frame

# Same thing with Winf. 
# Use aggregate mean of Winf by family.code of fish (e.g., clupeids)
Ag.wt.seamap <- aggregate(all.species.cpue$Wtinf.SEAMAP.grams~all.species.cpue$family.code, FUN = mean)
Ag.wt.seamap$family.code <- Ag.wt.seamap$`all.species.cpue$family.code`
Ag.wt.seamap$Wtinf.agmean.seamap <- Ag.wt.seamap$`all.species.cpue$Wtinf.SEAMAP.grams`

xmerge <- merge(Ag.wt.seamap, family.code, "family.code", all = TRUE)
setorder(xmerge, 'rownr') #needs this in every aggregation block of code to align rows correctly between data frames
all.species.cpue$Wtinf.fam.agmean.seamap <- xmerge$Wtinf.agmean.seamap


# Same thing with aggregate mean of trophic level
troph.ag <- aggregate(all.species.cpue$trophic.level ~ all.species.cpue$family.code, FUN = mean)
troph.ag$family.code <- troph.ag$`all.species.cpue$family.code`
troph.ag$Trophic.level.agmean <-troph.ag$`all.species.cpue$trophic.level`
xmerge <- merge(troph.ag, family.code, "family.code", all = TRUE)  
setorder(xmerge, 'rownr')#needs this in every aggregation block of code to align rows correctly between data frames
all.species.cpue$Trophic.level.fam.agmean <- xmerge$Trophic.level.agmean

# Make Q/B using PP98 for family-code aggregated values
for (i in 1:nrow(all.species.cpue)){
  if (is.na(all.species.cpue$Trophic.level.fam.agmean[i])) {t <- 0
  }else if (all.species.cpue$Trophic.level.fam.agmean[i] > 2.8) {t <- 0
  }else if (all.species.cpue$Trophic.level.fam.agmean[i] < 2.8 & all.species.cpue$Trophic.level.fam.agmean[i] > 2.2) {t <- 0.532
  }else t <- 0.398
  
  all.species.cpue$QB.agfam[i] <- 10^(7.964 - 0.204*log10(all.species.cpue$Wtinf.fam.agmean.seamap [i])-1.965 * 
                                        (1000/(23+273.15)) + 0.083*all.species.cpue$A.fam.agmean[i] + t)
}

# this completes the fishbase scraping and parameter calculation effort
# this shouldn't have to be done more than once, ideally.
# write the final version to a file
write.csv(all.species.cpue, "master_data_AV2.csv")

#STOP!


# try to get diet data more conviently than manually downlaoding
# from http://gomexsi.tamucc.edu
setwd("C:/Users/avanplan/Desktop/Alex SEAMAP/Model input data/Diets")
library(RCurl)
for (i in 1:nrow(basic.input.merge)){
  genus <- gsub("([A-Za-z]+).*", "\\1",basic.input.merge$model.species[i])
  genus <- tolower(genus)
  genus <- simpleCap(genus)
  species <- sub('^.* ([[:alnum:]]+)$', '\\1', basic.input.merge$model.species[i])
  species <- tolower(species)
  fish <- paste(genus, species)
  url <- paste('http://gomexsi.tamucc.edu/gomexsi/gomexsi-wp/data-query-raw.php?subjectName=', genus, "+", species, "&allInteractions=on&findPrey=on&filterPrey=&findPredators=on&filterPredators=&serviceType=rest&action=rhm_data_query", sep = "")
  x <- getURL(url)
  out <- read.csv(textConnection(x))
  y <- rbind(y, out)
  #filename <- paste(genus, "_", species, ".csv", sep = "")
  #download.file(url ,destfile= filename,method="libcurl")  
}
write.csv(y, "diets-all-species-aggregated.csv")
#https://stackoverflow.com/questions/23028760/download-a-file-from-https-using-download-file
x <- getURL(url)
out <- read.csv(textConnection(x))
download.file(url ,destfile= filename,method="libcurl")  
