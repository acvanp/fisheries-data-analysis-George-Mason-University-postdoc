Check out my first map made in R Shiny!
https://acvanp.shinyapps.io/myfirstmapappinrshiny/

The goal of this postdoctoral research fellowship, under a NOAA research grant with professor 
Kim de Mutsert at George Mason University, is to model Gulf of Mexico food webs using Ecopath with Ecosim
software. This requires using the SEAMAP database and SEDAR stock assessments to load accurate 
starting values for fish biomass.

Biomass
Before using the SEAMAP data, read the SEAMAP Data Structures PDF and the SEAMAP Operations Manual. Note that STATIONID can be used to merge the tables. Note that BGSID is a useful identifier that allows you to aggregate by species per station (station is the same thing as a specific catch or trawl).
Calculating biomass from SEAMAP data starts with calculating weight of the average fish per species per trawl station. That average weight of that fish of that species, would be based on length of the fish and the a and b values in the equation Weight = a*Length^b. You can scrape or load these regression coefficients from different resources (white papers, Fishbase). 
The SEAMAP data has subsets of fish measured for length and weight at every station. There is a count of the subset (CNT) and there is an extrapolated count of total fish per species estimated in the entire trawl (CNTEXP). Most of the fish in the subset were measured for length, but fewer were measured for weight, so it is hard to use the given weight to generalize about weight of fish per species per catch.
Because there were fewer weight measurements than lengths it was necessary to use the regression method to generate a and b values and weights for all individual fish with length measurements. With the SEAMAP data it is possible to generate unique and accurate a and b values. This was done using the R script “LWcalculator”, using the nls(y~a*x^b) equation format in R, where nls is nonlinear least squares. R is finicky about finding starting a and b values to begin searching for optimal a and b values in the nls() function – finicky in the sense that if R does not find better a and b values within 10 iterations of its nls() algorithm, then it gives up, throws an error and the loop breaks. Recommend starting values to the program using the results of lmfit = lm(log(y)~log(x)) (based on StackOverflow troubleshooting). Also, the function try() helps to prevent a loop from breaking from an error. The a and b vales would be taken from running regression for each species subsetted from the entire SEAMAP GLFREC data file (all years, for that particular species) in R in the R script file “LWcalculator_AV2”. The SEAMAP file called GLFREC had a record for every fish that was measured for length out of all the catches and the fish weights could be estimated from this and the a and b values. Using SEAMAP-generated a and b values was more accurate for predicting weight from length than using Fishbase values. 
SEAMAP biomass was first calculated in yearly time series and then the average yearly 2000-2005 values for each EwE model stanza were taken. The “time series writer_AV1” R script in the folder SEAMAP-Alex wrote the time series. The script starts with using the STAREC table and limiting the SEAMAP data area to the model area (north of 27.53°N) and from 87.7 to 94.7 °W using the “sp” and “geosphere” packages – whatever this clipped-down STAREC table merges with will be within the model area. This added up per year is the area trawled per year, so the R program sets aside a time series of yearly total trawled area within the study area. Aggregate mean fish weights in metric tonnes (average length-based mass of fish) per species per catch are multiplied by extrapolated count (CNTEXP column in BGSREC) of those fish per species per station to get estimated metric tonnes per species per station (station and catch are the same thing for all purposes here). In R this involves aggregating and merging the tables in a way that doesn’t slow down the computer too much:
1)	With the modified GLFREC (glfrec.lw, modified with regression-generated a and b and W=aL^b values) table make aggregate average weight of fish per species per station – the very helpful “set theory” identifier of fish species per station is the BGSID.
2)	Merge modified STAREC (just the model area), aggregated glfrec.lw, BGSREC (summary of fish counts per station), and INVREC (summary of gear used per station) tables.
3)	Using the INVREC table, area trawled is the width of the net (converted from feet to km) by the distance trawled (in the modified STAREC table).
4)	Merge this data with the species reference table that has the SEAMAP species codes (BIO_GLF) and the corresponding EwE group (e.g., sharks, Carangidae) – and this table or reference list can be made a number of different ways, then aggregate-sum the biomass of Types (or groups) by year.
5)	Have a separate aggregate sum of area trawled per year in time series of area and years.
6)	Now it is possible to loop through per group and per yearly area trawled and make yearly group “CPUE” of metric tonnes per square kilometer.
7)	Use cbind and rbind to assemble the time series with proper column names and corresponding pool codes into a spreadsheet that can be uploaded into EwE.
The biomass per EwE group per year divided by the area trawled per year becomes that entry in the SEAMAP biomass time series. We decided not to break down the biomass data into multiple stanzas per group.
Take the resulting time series and take an aggregate mean of 2000-2005 data that go into the Ecopath “basic input” page. 
Biomass for seatrout, red snapper, menhaden, white shrimp, brown shrimp, and pink shrimp came from stock assessments (PDFs in the Fisheries > Stock Assessments folder).
1.	Update Assessment of Spotted Seatrout Cynoscion nebulosus in Louisiana Waters 2014 Report
2.	Stock Assessment of Red Snapper in the Gulf of Mexico 1872-2013 With Provisional 2014 Landings
3.	SEDAR 32A Menhaden Stock Assesment in the R folder under stock assessment data and r scripts, “gm-020.r” Kim received from NOAA. The x$ssb values for stanzas 3+ years old are used for biomass (SSB or spawning stock biomass).
4.	D – 5(a) Brown Shrimp (Rick Hart)
5.	D – 5(b) White Shrimp (Rick Hart)
6.	D – 5 (c) Pink Shrimp (Rick Hart)

Consumption-Biomass (Q/B) Ratios
Information of calculating Q/B can be taken from Palomares and Pauly (1998), and their equation 12. This reference includes estimates for Gulf menhaden and red snapper. The R script “QB high and low values AV1” in the “Consumption to biomass ratio QB” folder carries out the task of taking the data scraped from Fishbase and put in a “species reference” table (found in the SEAMAP-Alex folder) and estimating the high and low possible Q/B ratios depending on if we assume the fish are juvenile detritivores/herbivores or adult omnivores.
Landings
Louisiana landings for all available species and years in metric tonnes are taken from the NOAA fisheries website. The species nomenclature is different so I used an R script “raw landings time series AV1” to parse the names and match them with the model groups. Then the data are put in time series. Landings data from Louisiana is divided by 118,000 square kilometers because that is the area that ArcMap says the model area in the Gulf of Mexico is (excluding any land area). The entire Gulf of Mexico that is considered US waters is 700,000 square kilometers according to ArcMap. 
Discards
Discard data for 2000-2005 comes from the National Bycatch Report – Southeast Region, Subtable 4.2.A.4, page 42 in the PDF and page 184 in the document’s numbering style (NOAA, 2011).
Diets
Diet data comes from GOMEXSI. I used the R script in “fishbase_query_AV2” (which includes numerous web scraping procedures for getting Fishbase data) to download the CSV data combining prey and predation per species. The data can be turned into histograms or percent of diet data. The data product that this resulted in was an Excel file in the Diets folder called “GOMEXSI diet workbook prey and predator interactions - AV1 December”. This file was made using the R script “main diet items input into a template_AV1” in the Diets > GOMEXSI folder.
Time Series
See above for the time series R script for landings and biomass. I have been taking out the NA’s in Excel using the “Replace” function.
Fishing Mortality
Kristy’s student compiled fishing mortality F values from SEDAR.
References
Hart, R. 2015. Stock Assessment Update for Brown Shrimp (Farfantepenaeus aztecus) in the U.S. Gulf of Mexico for 2014.  NOAA Fisheries, Southeast Fisheries Science Center, Galveston Laboratory, Galveston, TX 77551.

Hart, R. 2015. Stock Assessment Update for White Shrimp (Litopenaeus setiferus) in the U.S. Gulf of Mexico for 2014.  NOAA Fisheries, Southeast Fisheries Science Center, Galveston Laboratory, Galveston, TX 77551.

Hart, R. 2015. Stock Assessment Update for Pink Shrimp (Farfantepenaeus duorarum) in the U.S. Gulf of Mexico for 2014.  NOAA Fisheries, Southeast Fisheries Science Center, Galveston Laboratory, Galveston, TX 77551.

SEDAR 34 Stock Assessment Report. 2013. HMS Atlantic Sharpnose Shark. Stock assessment report. SEDAR. 4055 Faber Place Drive, Suite 201 North Charleston, SC 29405.

SEDAR 32A Stock Assessment Report. September 2013. Gulf of Mexico Menhaden. SEDAR. 4055 Faber Place Drive, Suite 201 North Charleston, SC 29405.

West, J., Melancon, A., Potts, S., Powers, J. 2014. Update Assessment of Spotted Seatrout Cynoscion nebulosus in Louisiana Waters 2014 Report.

VanderKooy, S (editor). 2013. June 2013. GDAR 01 Stock Assessment Report Gulf of Mexico Blue Crab. GSMFC Number 215. Gulf Data, Assessment, and Review, Gulf States Marine Fisheries Commission, 2404 Government Street, Ocean Springs, MS 39564.

Shannon L. Cass-Calay, Clay E. Porch, Daniel R. Goethel, Matthew W. Smith,
Vivian Matter, and Kevin J. McCarthy. 2015. Stock assessment of red snapper in the Gulf of Mexico 1872 – 2013 - with provisional 2014 landings. SEDAR Update Assessment, 09/07/2015. 

Palomares, M., Pauly, D. 1998. Predicted food consumption of fish populations as functions of mortality, food type, morphometrics, temperature and salinity. Marine and Freshwater Research, Volume 49, 447-53.

De Mutsert, K., Lewis, K., Milroy, S., Buszowski, J., Steenbeek, J. 2017. Using ecosystem modeling to evaluate trade-offs in coastal management: Effects of large-scale river diversions on fish and fisheries. Ecological Modelling 360 (2017) 14–26.
