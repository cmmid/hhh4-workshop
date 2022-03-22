## ----import ECDC data---------------------------------------------------------
ecdc_long <- read.csv("data/ECDC_surveillance_data_IMD.csv.gz",
                      na.strings = "-") |>    # always important to know NA encoding!
  subset(!RegionName %in% c("EU/EEA", "EU"))  # exclude aggregate counts

## reshape from long to wide format of multivariate time series
ecdc <- reshape(ecdc_long[c("Time", "RegionCode", "NumValue")],
                direction = "wide", idvar = "Time", timevar = "RegionCode",
                varying = unique(ecdc_long$RegionCode))
row.names(ecdc) <- ecdc$Time; ecdc$Time <- NULL

head(ecdc)
tail(ecdc)

## exclude contries without data
ecdc <- ecdc[, colSums(!is.na(ecdc)) > 0]


## ----import population data---------------------------------------------------
popdata <- read.csv("data/Eurostat_population_2018.csv")
head(popdata, 3)
stopifnot(colnames(ecdc) %in% popdata$geo)
pop <- setNames(popdata$OBS_VALUE, popdata$geo)[colnames(ecdc)]
pop


## ----import map, results = "hide"---------------------------------------------
file_map <- "data/map.RData"
if (file.exists(file_map)) {
   load(file_map)
} else {
  library("sf")
  ## read NUTS1-level data from Eurostat/GISCO
  map1 <- st_read("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_60M_2021_4326_LEVL_1.geojson")
  ## omit French overseas regions for a more compact map
  map1 <- subset(map1, NUTS_ID != "FRY")
  ## union polygons by country
  map0 <- aggregate(map1[0], by = list(COUNTRY = map1$CNTR_CODE), FUN = sum)
  ## check that the map contains the country codes of colnames(ecdc)
  stopifnot(colnames(ecdc) %in% map0$COUNTRY)
  ## convert to "SpatialPolgons" for use with sts()
  row.names(map0) <- map0$COUNTRY  # to match with colnames(ecdc)
  library("sp")
  map <- geometry(as_Spatial(map0))
  save(map, file = file_map, compress = "xz")
}


## ----map_plot, out.width = "100%", fig.crop = TRUE----------------------------
library("sp")
par(mar = c(0,0,0,0))
plot(map)


## ----create an sts object-----------------------------------------------------
library("surveillance")

## start of the monthly time series
(start <- as.numeric(strsplit(rownames(ecdc)[1], split="-")[[1]]))

## IMD0 <- sts(....)  # see help("sts")






## ----subset of countries------------------------------------------------------
IMD <- IMD0[, grep("^[ADFNU]", colnames(IMD0))]


## ----plotting, include = FALSE------------------------------------------------
##
## ... use the plot() method or autoplot.sts() ...
##








## ----IMD1---------------------------------------------------------------------
IMD1 <- IMD[,"FR"]
plot(IMD1)

## plot yearly time series to investigate seasonality, here using "lattice"
library("lattice")
xyplot(observed ~ factor(epochInYear), data = tidy.sts(IMD1),
       groups = year, type = "l", xlab = "month")


## ----regression formula, eval = FALSE-----------------------------------------
## addSeason2formula(~ t, S = s, period = 12)


## ----convenient model updates, eval = FALSE-----------------------------------
## fit2 <- update(fit1, ar = list(f = ~ 1), S = list(ar = 2, end = 2))

