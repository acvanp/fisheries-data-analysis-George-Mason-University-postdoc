# 2017
# Alex Van Plantinga
# This program takes fish species length and wieight data and 
# finds the a and b values for W = a*L^b equation.
# The nls() function is finnicky and the loop can break with 
# errors, so use try() and establish starting a and b values.

setwd("C:/Users/Kristy Lewis/Desktop/Alex SEAMAP/public_seamap_csvs")

glfrec <- read.csv("GLFREC.csv")
biocodes <- read.csv("BIOCODES.csv")

fish.unique <- glfrec[which(!duplicated(glfrec$BIO_GLF)),]
fish.list <- list(fish.unique$BIO_GLF)

#----Loop to get genus LW a and b coefficients
for (i in 1:length(fish.list[[1]])){

fishcode <- fish.list[[1]][[i]]

fish <- glfrec[which(glfrec$BIO_GLF == fishcode),]


try(x <- fish$LEN_GLF, silent = TRUE)
try(y <- fish$INDVL_WT, silent = TRUE)
try(df <- na.omit(data.frame(x,y)), silent = TRUE)
try(df <- df[which(df$y < Inf),], silent = TRUE)
try(df <- df[order(df$x),], silent = TRUE)
try(x <- df$x, silent = TRUE)
try(y <- df$y, silent = TRUE)
#plot(x,y)



# make the a and b starting values
# so the loop doesn't break
lmfit <- NA
try(lmfit <- lm(log(y)~log(x)), silent=TRUE )
try(astart <- lmfit[[1]][[1]], silent = TRUE)
try(bstart <- lmfit[[1]][[2]], silent = TRUE)
fit <- NA
try(fit <- (nls(y ~ a*x^b, data = df, start = list(a = exp(astart), b = bstart))), silent=TRUE )
try(summary(fit), silent = TRUE)
a <- NA
b <- NA
try(a <- summary(fit)[[10]][[1]], silent = TRUE)
try(b <- summary(fit)[[10]][[2]], silent = TRUE)

if(is.na(a)){
  
  # filter out the stray crazy values, the outliers
  lowx <- fish[which(fish$LEN_GLF <= 0.25 * median(fish$LEN_GLF)),]
  highx <- fish[which(fish$LEN_GLF >= 0.25 * median(fish$LEN_GLF)),]
  lowx.filt <- lowx[which(lowx$INDVL_WT < mean(na.omit(lowx$INDVL_WT))),]
  fish <- rbind(lowx.filt,highx)
  # this causes an exceptional error for brown shrimp 
  # but brown shrimp should have gone through the first loop and
  # and therefore shouldn't apply to this section of code.
  x <- fish$LEN_GLF
  y <- fish$INDVL_WT
  df <- na.omit(data.frame(x,y))
  df <- df[which(df$y < Inf),]
  df <- df[order(df$x),]
  x <- df$x
  y <- df$y
  #plot(x,y)
  # make the a and b starting values
  # so the loop doesn't break
  lmfit <- NA
  try(lmfit <- lm(log(y)~log(x)), silent=TRUE )
  try(astart <- lmfit[[1]][[1]], silent = TRUE)
  try(bstart <- lmfit[[1]][[2]], silent = TRUE)
  fit <- NA
  try(fit <- (nls(y ~ a*x^b, data = df, start = list(a = exp(astart), b = bstart))), silent=TRUE )
  try(summary(fit), silent = TRUE)
  a <- NA
  b <- NA
  try(a <- summary(fit)[[10]][[1]], silent = TRUE)
  try(b <- summary(fit)[[10]][[2]], silent = TRUE)
  
  
}

# if the above blocks of code don't help then go to the genus level
if(is.na(a)){
  
  fish <- glfrec[which(glfrec$GENUS_GLF == fish$GENUS_GLF),]
  x <- fish$LEN_GLF
  y <- fish$INDVL_WT
  df <- na.omit(data.frame(x,y))
  df <- df[which(df$y < Inf),]
  df <- df[order(df$x),]
  x <- df$x
  y <- df$y
  #plot(x,y)
  
  # make the a and b starting values
  # so the loop doesn't break
  lmfit <- NA
  try(lmfit <- lm(log(y)~log(x)), silent=TRUE )
  try(astart <- lmfit[[1]][[1]], silent = TRUE)
  try(bstart <- lmfit[[1]][[2]], silent = TRUE)
  fit <- NA
  try(fit <- (nls(y ~ a*x^b, data = df, start = list(a = exp(astart), b = bstart))), silent=TRUE )
  try(summary(fit), silent = TRUE)
  a <- NA
  b <- NA
  try(a <- summary(fit)[[10]][[1]], silent = TRUE)
  try(b <- summary(fit)[[10]][[2]], silent = TRUE)
  
}

glfrec$a[glfrec$BIO_GLF == fishcode] <- a
glfrec$b[glfrec$BIO_GLF == fishcode] <- b

}
# write this so it doesn't have to be rerun
#write.csv(glfrec, "glfrec_lw_partial.csv")
# what isn't there we take from fishbase
library(rvest)
library(rfishbase)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



# aggregate glfrec down to just uniqe
glf.ag <- subset(glfrec, !duplicated(glfrec$BIO_GLF))

# take what you can from FishBase
for (i in (1:nrow(glf.ag))){
  x <- glf.ag[i,]
  if (is.na(x$a)){
y <- biocodes[which(biocodes$CODE == x$BIO_GLF),]  
genus <- gsub("([A-Za-z]+).*", "\\1", y$TAXONOMIC) #use regex to parse text
genus <- tolower(genus)
try(genus <- simpleCap(genus), silent = TRUE)
species <- sub('^.* ([[:alnum:]]+)$', '\\1', y$TAXONOMIC)
species <- tolower(species)
fish <- paste(genus, species)

url <- paste('http://www.fishbase.se/summary/', genus, "-", species, ".html", sep = "")
try(session <- html_session(url), silent = TRUE)
s <- NA
t <- NA
try(s <- session %>% html_nodes("body") %>% html_text(), silent = TRUE)
a <- NA
try(t <- strsplit(s[[1]], split = "a=")[[1]][2], silent = TRUE)
try(a <- as.numeric(unlist(regmatches(t, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",t))))[[1]][1], silent = TRUE)
u <- NA
b <- NA
try(u <- strsplit(s[[1]], split = "b=")[[1]][2], silent = TRUE)
try(b <- as.numeric(unlist(regmatches(u, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",u))))[[1]][1], silent = TRUE)

glfrec$a[glfrec$BIO_GLF == y$CODE] <- a*1e-6 #fishbase converts mm to g so convert to SEAMAP units
glfrec$b[glfrec$BIO_GLF == y$CODE] <- b

}

}
# write this so it doesn't have to be rerun
#write.csv(glfrec, "glfrec_lw_partial2.csv")

# what you can't get from regression and from FishBase, take from like species
# take what you can from FishBase
# this might not 

animals <- sub('^.* ([[:alnum:]]+)$', '\\1', biocodes$common_name)

for (i in (1:nrow(glf.ag))){
  x <- glf.ag[i,]
  if (is.na(x$a)){
    y <- biocodes[which(biocodes$CODE == x$BIO_GLF),] 
    animal <- sub('^.* ([[:alnum:]]+)$', '\\1', y$common_name)
    z <- biocodes[which(animals %in% animal),]
    section <- glfrec[which(glfrec$BIO_GLF == z$CODE),]
    a <- NA
    b <- NA
    a <- mean(na.omit(section$a))
    b <- mean(na.omit(section$b))
    
    glfrec$a[glfrec$BIO_GLF == y$CODE] <- a
    glfrec$b[glfrec$BIO_GLF == y$CODE] <- b
    
  }
}


#glfrec$wt.kg <-NA
glfrec$wt.kg <- glfrec$a * glfrec$LEN_GLF ^ glfrec$b #make a weight in kilograms column based on a, b, and length in mm


#this loop takes hours so don't write over the old file
#write.csv(glfrec, "glfrec_lw1.csv")

setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex")
glfreclw1 <- read.csv("glfrec_lw1.csv")

basic.input <- read.csv("basic-input-merge-AV1.csv")

for (i in 1:nrow(basic.input)){
  x <- biocodes[which(biocodes$TAXONOMIC == as.character(basic.input$model.species[i])),]
  y <- glfreclw1[which(glfreclw1$BIO_GLF == x$CODE),]
  basic.input$meanwt.try[i] <- sum(na.omit(y$wt.kg))
}  
# ----
# problem originally  encountered for the following species below. 
# I fixed many of these problems with the try() function 
# and with filtering out the outliers that are abnormally heavy for shorter individuals
# however, this doesn't get the output to have all the fish weights. 
# There are problems then with crab and eel a and b estimates.
# so I just generalized eel and crab, etc. a and b values for anything that didn't have them.
# ----
# RHINOBA LENTIG (guitarfish, ray/skate/shark)
# GYMNURA (ray)
# BREGMAC ATLANT (codlet, Bregmaceros single genus of cod-like fish)
# RENILLA   MULLER (sea pansy, colonial cnidarian native)
# MACOMA   BREVIF (macoma brevifons, mollusk)
# RAJA   TEXANA (skate)
# OPHICHT   GOMESI (Ophichthus gomesii, shrimp eel)
# STOMOLO MELEAG (Stomolophus meleagris, jellyfish)
# COLLODE   ROBUST
# SYNGNAT   LOUISI (Louisiana pipefish)
# MYLIOBA   FREMIN (Myliobatis freminvillei, a manta ray)
# ALPHEUS FLORID Alpheus floridanus sand snapping shrimp
# POLYDAC OCTONE Polydactylus octonemus  (Girard, 1858) Atlantic threadfin
# Ficus Communis a snail
# GINGLYM CIRRAT nurse shark
# OPHIOTH ANGULA brittle star
# SPEOCAR LOBATU Speocarcinus a crustacean
# RHINOPT BONASU  Scientific name: Rhinoptera bonasus, crownose ray
# RHYNCHO FLAVUS eel, yellow conger
# SCYPHOZ jellyfish

