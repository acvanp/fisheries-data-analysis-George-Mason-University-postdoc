# Hi Alex,
# 
# 
# 
# Alright, I've got something to work with. I've put output of the 
# calibration in the presentation file of our shared folder, and the model (v5) in the shared folder. 
# The output is predicted versus observed, where observed is only 1 value per year, and should 
# correspond with what you have as observations (check just in case), these are going to be the 
# observations in the graphs with error bars. The predicted are going to be the line graphs to 
# be plotted with the observations. I am interested in a 3 by 2 panel of catch of the following 6 species:
#   
# 6-24 red snapper
# 
# Other snappers
# 
# 24-36 gulf menhaden
# 
# 36+ gulf menhaden
# 
# Brown shrimp
# 
# White shrimp
# 
# I am also interested in a 5 by 3 panel of biomass of the following species:
#   
#   Gulf menhaden
# 
# Atlantic croaker
# 
# Brown shrimp
# 
# White shrimp
# 
# Blue crab
# 
# Butterfish
# 
# Squid
# 
# Flounders
# 
# Red snapper
# 
# Serranidae
# 
# Sea trout
# 
# Red drum
# 
# Benthic crabs
# 
# Anchovy
# 
# Porgies
# 
# I will include images of these plots in the dropbox as well under calibration, this will show you the SS of each fit at the top of each graph, I would like you to include that in these graphs as well (plus the name of the species as you did before).
# Could you work on these plot panels?
# Thanks!
#   Kim


########## CATCHES ###########
setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//presentations//ts plots")

ts = read.csv("new Kim NGOMEX1_allfit_catches.csv")
cnames = ts[7,]

#make a date time series to match the left column
datevals = seq(as.Date("2000-01-01"), as.Date("2016-12-01"), by = "month")
ts = ts[8:nrow(ts),]
for(i in 1:ncol(ts)){
  colnames(ts)[i] = as.character(cnames[i][[1]])# weird non character format
}

ts$Date = datevals

catchnames.pred = c(
  "catch (predicted)6-24 red snapper",
  
  "catch (predicted)other snapper",
  
  "catch (predicted)Gulf menhaden 24-36",
  
  "catch (predicted)Gulf menhaden 36+",
  
  "catch (predicted)brown shrimp",
  
  "catch (predicted)white shrimp"
)

ts.pred = ts[,which(colnames(ts) %in% catchnames.pred)]

catchnames.obs = c(
  
  "catch (observed) 6-24 red snapper",
  
  "catch (observed) other snapper",
  
  "catch (observed) Gulf menhaden 24-36",
  
  "catch (observed) Gulf menhaden 36+",
  
  "catch (observed) brown shrimp",
  
  "catch (observed) white shrimp" 
)
ts.obs = ts[,which(colnames(ts) %in% catchnames.obs)]

ss.c = c("9.028",
         "4.251",
         "3.341",
         "3.478",
         "11.68",
         "4.818"
         )


# special error data
setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex\\")
err = read.csv("yearly error bars stdev for NOAA landings data.csv")
df = data.frame()
# had trouble reading the stanza names as character so iterate cell by cell to drop levels
# to coerce format of stanza names as character to match with list of Kim's requested stanzas
err = err[, which(colnames(err) %in% c("X6.24.red.snapper", 
                "other.snapper", "Gulf.menhaden.24.36", "Gulf.menhaden.36.", 
                "brown.shrimp", "white.shrimp"))]

err = err[2:18,]

# make sure columns are in the right order


setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//presentations//ts plots")
########################### figure

tiff("Catches six figure panel diagram Monthly Error Bars - AV2.tiff", width = 8, height = 10, units = 'in', res = 300, compression = 'none')

layout(matrix(c(1:6),3,2,byrow = TRUE))

###############
# par( mar = c(bottom, left, top, right))
par(mar=c(3, 4.5, 1.2, 1.8))

for(i in 1:6){

b = as.numeric(as.character(ts.obs[,i][which(ts.obs[,i] != "")]))

years = seq(as.Date("2000-06-01"), as.Date(paste(as.character(2000+length(b)-1),"-06-01", sep = "")), by = "year")

df = na.omit(data.frame(years, b))


yscale = 1.8*max(b)
plot(df, 
     pch = 19, ylim = c(0,yscale), ylab = "", xlab = "", cex.axis = 1.6)
x = df[,1]
avg = df[,2]
sdev = err[,i] # the error bar values
arrows(x, avg-sdev, x, avg+sdev, length= 0.05, angle= 90, code=3)

# make the title the column name after the parentheses
text(as.Date("2008-01-01"), (yscale-0.01*yscale), 
     paste(as.character(strsplit(catchnames.obs[i], ")")[[1]][2])," : ", ss.c[i], sep = "")
      , cex = 2)
# weird non character format
mtext(expression(paste("Catch (t" %.%  "km" ^ "-2", ")", sep = "")), side = 2, line = 2.4, outer = FALSE, cex = 1)

lines(datevals, as.numeric(as.character(ts.pred[,i])), lwd = 2)
if(i == 1){legend(as.Date("2010-01-01"), (yscale - 0.1 * yscale), legend = c("obsv", "pred"),
                 pch=c(19,NA), 
                 lty = c(NA,1) , lwd = c(NA,2), cex = 1.6)}

}

dev.off()



########## BIOMASS yearly station (geographic) error bars ###########
setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//presentations//ts plots")

ts = read.csv("new Kim NGOMEX1_allfit_biomass.csv")
cnames = ts[7,]

#make a date time series to match the left column
datevals = seq(as.Date("2000-01-01"), as.Date("2016-12-01"), by = "month")
ts = ts[8:nrow(ts),]
for(i in 1:ncol(ts)){
  colnames(ts)[i] = as.character(cnames[i][[1]]) # weird non character format
}

ts$Date = datevals

biomass = c(
"Gulf menhaden", "Atlantic croaker",

"brown shrimp",  "white shrimp",  "blue crab",

  "butterfish",  "squid",

  "flounders",  "red snapper",

  "Serranidae",  "sea trout",

  "red drum",  "benthic crabs",

  "anchovy",  "porgies"
)
# names of columns in the predicted ts
biomass.pred = list()
for(i in 1:length(biomass)){
  biomass.pred[i] = paste("biomass (predicted)", biomass[i])
}
ts.pred = ts[,which(colnames(ts) %in% biomass.pred)]

# names of columns in the observed ts
biomass.obs = list()
for(i in 1:length(biomass)){
  biomass.obs[i] = paste("biomass (observed)", biomass[i])
}
ts.obs = ts[,which(colnames(ts) %in% biomass.obs)]

# special error data
setwd("C:\\Users\\avanplan\\Dropbox\\NGOMEX Model components\\SEAMAP-Alex\\")
err = read.csv("yearly error bars doubled stdev for SEAMAP data.csv")
err = err[52:68,]

err = err[,which(colnames(err) %in% c("Gulf.menhaden", "Atlantic.croaker", "brown.shrimp", "white.shrimp", 
                                      "blue.crab", "butterfish", "squid", "flounders", 
                                      "red.snapper", "Serranidae", "sea.trout", "red.drum", 
                                      "benthic.crabs", "anchovy","porgies"))]

# fix NAs make 0s
err[is.na(err)] = 0

err[,which(colnames(err) %in% c("Gulf.menhaden", "brown.shrimp", "white.shrimp", 
                                      "red.snapper", "sea.trout"))] = 0
ss.b = c("15.17", "9.066", "3.601", "11.26",
       "5.762", "8.522", "8.764", "1.522",
       "3.441", "3.394", "2.848", "12.18",
       "2.894", "4.775", "2.266"
       )
# reorder columns to get them in the right order for the loop.
err = err[,c("Gulf.menhaden", "Atlantic.croaker", "brown.shrimp", "white.shrimp", 
                                      "blue.crab", "butterfish", "squid", "flounders", 
                                      "red.snapper", "Serranidae", "sea.trout", "red.drum", 
                                      "benthic.crabs", "anchovy","porgies")]
########################### figure
setwd("C://Users//avanplan//Dropbox//NGOMEX Model components//presentations//ts plots")
tiff("Biomass 15 figure panel diagram SEAMAP SD - AV2.tiff", width = 8, height = 10, units = 'in', res = 300, compression = 'none')

layout(matrix(c(1:15),5,3,byrow = TRUE))

###############
# par( mar = c(bottom, left, top, right))
par(mar=c(2.4, 4.5, 1, 1.8))

for(i in 1:15){
  
  b = as.numeric(as.character(ts.obs[,i][which(ts.obs[,i] != "")]))
  
  years = seq(as.Date("2000-06-01"), as.Date(paste(as.character(2000+length(b)-1),"-06-01", sep = "")), by = "year")
  
  df = na.omit(data.frame(years, b))
  
  
  yscale = 1.8*max(b)
  plot(df, 
       pch = 19, ylim = c(0,yscale), ylab = "", xlab = "", cex.axis = 1.3)
  x = df[,1]
  avg = df[,2]
  sdev = err[,i] # the error bar values
  arrows(x, avg-sdev, x, avg+sdev, length= 0.05, angle= 90, code=3)
  
  # make the title the column name after the parentheses
  text(as.Date("2007-06-01"), (yscale-0.05*yscale),
       paste(
       as.character(biomass[i]), ss.b[i]
        ), cex = 1.6)
  
  mtext(expression(paste("Biomass (t" %.%  "km" ^ "-2", ")", sep = "")), side = 2, line = 2.2, outer = FALSE, cex = 1)
  
  lines(datevals, as.numeric(as.character(ts.pred[,i])), lwd = 2)
  if(i == 8){legend(as.Date("2005-01-01"), 0.027, legend = c("obsv", "pred"),
                    pch=c(19,NA), 
                    lty = c(NA,1) , lwd = c(NA,2),cex = 1.2)}
  
}

dev.off()