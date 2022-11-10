indir.rainfall = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/rainfall_CHIRPS/outputs/CHIRPS23_on_muni_boundaries_Amazon/"
fname = "rainfall_CHIRPS_muni_1981_2020_whole_amazon.csv"

setwd(indir.rainfall)
x = read.csv(fname)

xNA = rowSums(is.na(x))

xNAvals = x[xNA>0,]
xNAvals$CD_GEOCMU
