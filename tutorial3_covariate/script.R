data <- read.csv("data_2022-Mar-15.csv")
dates <- unique(data$date)
data <- t(with(data, tapply(newCasesBySpecimenDate, list(areaName, date),
                                 function(x){x})))
tail(data)

library(surveillance)
sts_covid <- sts(data,
                 start = c(2020, as.Date(rownames(head(data, 1))) -
                             as.Date("2019-12-31")),
                 frequency = 365)
sts_covid@freq
sts_covid@start

mod0 <- hhh4(sts_covid)
summary(mod0)

autoplot.sts(sts_covid)

mod1 <- hhh4(sts_covid,
             control = list(end = list(f = ~ fe(1, unitSpecific = TRUE) - 1)))
summary(mod1)

AIC(mod0, mod1)


# Here assuming only NI and Scotland connected
neigh_mat <- matrix(c(0, 0, 1, 1,
                      0, 0, 1, 0,
                      1, 1, 0, 2,
                      1, 0, 2, 0),
                    ncol = 4, byrow = TRUE) + 1
colnames(neigh_mat) <- rownames(neigh_mat) <-
  c("England", "Northern Ireland", "Scotland", "Wales")
mod2 <- hhh4(sts_covid,
             control = list(end = list(f = ~ fe(1, unitSpecific = TRUE) - 1),
                            ne = list(f = ~ fe(1, unitSpecific = TRUE) - 1,
                                      weights = neigh_mat)))
plot(mod2, units = 1 : 4)

#Population estimates for the UK, England and Wales,
#Scotland and Northern Ireland: mid-2020
uk_pop <- c(56550000, 1896000, 5466000, 3170000)
names(uk_pop) <- c("England", "Northern Ireland", "Scotland", "Wales")
# Implicit assumption of population not changing over time


# Update sts_covid to have this information
sts_covid <- sts(data,
                 start = c(2020, as.Date(rownames(head(data, 1))) -
                             as.Date("2019-12-31")),
                 frequency = 365,
                 population = uk_pop)
mod3 <- hhh4(sts_covid,
             control = list(end = list(f = ~ fe(1, unitSpecific = TRUE) - 1,
                                       offset = population(sts_covid)),
                            ne = list(f = ~ fe(1, unitSpecific = TRUE) - 1,
                                      weights = neigh_mat)))
summary(mod2)$fixef
summary(mod3)$fixef

pillar <- read.csv("pillar_2022-Mar-15.csv")
pillar <- pillar[pillar$date %in% dates, ] # Match to case data
pillar$prop <- pillar$capacityPillarOne / pillar$plannedCapacityByPublishDate
plot(as.Date(pillar$date), pillar$prop, xlab = "Date", ylab = "Pillar 1")

head(pillar[, c("areaName", "date", "plannedCapacityByPublishDate", "prop")])

# Keep what we are using
# Sanity check -- are dimensions the same
isTRUE(dim(data)[1] == dim(pillar)[1])

# Need to add zeros for the following dates
pillar <- rbind(pillar[, c("date", "prop")],
      cbind(date = dates[which(!(dates %in% pillar$date))],
            prop = rep(0, length(dates[which(!(dates %in% pillar$date))]))))

# Sanity check -- is covariate numeric?
isTRUE(is.numeric(pillar$prop))

test <- as.numeric(pillar$prop)

mod4 <- hhh4(sts_covid,
             control = list(end = list(f = ~ fe(1, unitSpecific = TRUE) - 1,
                                       offset = population(sts_covid)),
                            ne = list(f = ~ fe(1, unitSpecific = TRUE) - 1 +
                                        test,
                                      weights = neigh_mat)))

AIC(mod3, mod4)