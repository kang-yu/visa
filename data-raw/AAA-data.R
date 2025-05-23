############################################################
#
# prepare example data
############################################################

## devtools::use_data_raw()
# * Add data creation scripts in data-raw
# * Includes data-raw/ on .Rbuildignore

exampleData <- read.csv('data-raw/exampleData.csv', header=FALSE)[1:20,1:652]
# wavelength <- as.numeric(gsub("X","",names(exampleData)[(1+1):952]))

exampleData <- data.matrix(exampleData) # Convert a Data Frame to a Numeric Matrix
exampleData <- matrix(as.numeric(unlist(exampleData)),nrow=nrow(exampleData))

# devtools::use_data(exampleData, overwrite = TRUE)
# visa.spectra <- exampleData
# devtools::use_data(visa.spectra, overwrite = TRUE)
# rm(visa.spectra)

# put spec-matrix in 1 column


# load("data/spec_dataframe.rda")
y <- matrix(exampleData[-1, 1]) # Variable of interest, e.g., Chl, N, LAI
xS <- as.matrix(exampleData[-1, 2:ncol(exampleData)]) # Reflectance spectra
w <- exampleData[1, 2:ncol(exampleData)]

str(xS)
dimnames(xS) #
dimnames(xS) <- list(rownames(xS), paste(w, "nm")) # replace dim names

#####################################
# save the spectra as dataframe format
NSpec.DF <- data.frame("N" = y, "spectra" = I(xS))
str(NSpec.DF)

str(NSpec.DF$spectra)
class(NSpec.DF) # AsIs

devtools::use_data(NSpec.DF, overwrite = TRUE)

#####################################
## save the spectra as S4 class format

class(NSpec.DF$spectra) # !!! to assure the spectra class
if (is(NSpec.DF$spectra, "AsIs")) class(NSpec.DF$spectra) <- "matrix" # or NULL

smat <- as.matrix(NSpec.DF$spectra)
class(smat)

wavelength <- as.numeric(gsub("\\D", "", colnames(NSpec.DF$spectra)))
s.id <- rownames(NSpec.DF)
w.unit <- unique(gsub("\\d+\\s", "", colnames(NSpec.DF$spectra)))
N <- NSpec.DF["N"]

# test 'spectra'
speclass <- as.spectra(smat, wavelength, as.numeric(s.id), w.unit, data = N)
class(speclass)
class(speclass@spectra)
str(speclass)
rownames(speclass@spectra) # can be NULL or 's.id'

NSpec.Lib <- speclass
devtools::use_data(NSpec.Lib, overwrite = TRUE)






###############################################################
#
# Following to be revised
#
# Internal data
#--------------------------------------------------------------
# Sometimes functions need pre-computed data tables
# devtools::use_data(exampleData, internal = TRUE)

library(gdata)
spec150629 <- read.xls("O:/FIP/2015/WW007/SPM3M/20150629_rad.xlsx")[,1:652]
spec150629s <- spec150629[,c(1,seq(2,652,5))]
strP <- unlist(strsplit(as.character(spec150629s$Plot_ID),"20150629_FWW007"))
spec150629s$Plot_ID <- as.numeric(strP[seq(2,222,2)])
spec150629s <- rename.vars(spec150629s, "Plot_ID", "Plot")

lai <- read.csv("O:/FIP/2015/WW007/RefTraits/Wide/2015_WW007_LAI_Wide.csv")
lai <- lai[,c("Plot","LAI.2015.06.29")]
ph <- read.csv("O:/FIP/2015/WW007/RefTraits/Wide/2015_WW007_PH_Hand_cm_Wide.csv")
ph <- ph[,c("Plot","PH_Hand_cm.2015.06.25")]

Y <- merge(lai,ph)
visa.wheat.spec <- merge(Y, spec150629s)
names(visa.wheat.spec)
visa.wheat.spec <- rename.vars(visa.wheat.spec,
                               c("LAI.2015.06.29", "PH_Hand_cm.2015.06.25"),
                               c("LAI","PH"))

devtools::use_data(visa.wheat.spec, overwrite = TRUE)


# some remarks ------------------------------------------------------------

# Setting email address for a single repository might be needed

# $ git config --global user.email

# build addins to push

# ggbiplot add pca etc




# read asd ----------------------------------------------------------------

library(prospectr)
asdFiles <- list.files(path="data-raw/", "asd", full.names = T)
speciespec <- readASD(asdFiles,'binary',"list")

rad <- sapply(speciespec, "[[", "radiance")
ref <- sapply(speciespec, "[[", "reference")
rflt <- sapply(speciespec, "[[", "reflectance")
wvlt <- sapply(speciespec, "[[", "wavelength")
matplot(wvlt, rad, type = "l", ylim = c(0,50000))
matplot(wvlt, ref, type = "l", ylim = c(0,50000))
matplot(wvlt, rflt, type = "l", ylim = c(0,1))
matplot(wvlt, rad/ref, type = "l", ylim = c(0,1))

## read as matrix
speciespec <- readASD(asdFiles,'binary')
matplot(wvlt, t(speciespec), type = "l", ylim = c(0,1))

tempStr <- strsplit(row.names(speciespec),"00")
tempStr <- sapply(tempStr, function(x) x = unlist(x[1])[1])
Species <- tempStr

speciespec <- data.frame(Species,speciespec)
str(speciespec)

devtools::use_data(speciespec, overwrite = TRUE)

#  ------------------------------------------------------------------------
#  PCA and visualization
#  ------------------------------------------------------------------------

library(rgl)
library(ggord)
library(ggbiplot)

pca <- prcomp(speciespec[,2:1000])

# biplot 1
biplot(pca)

# biplot 2
p <- ggord(pca, speciespec$Species)
p

# biplot 3
ggbiplot(pca)


## biplot 3d
plot3d(pca$scores[,1:3])

