yrbs_raw <- list()

yrbs_raw[[1]] <- read.csv("../dat/yrbs2007.csv")
yrbs_raw[[2]] <- read.csv("../dat/yrbs2009.csv")
yrbs_raw[[3]] <- read.csv("../dat/yrbs2011.csv")
yrbs_raw[[4]] <- read.csv("../dat/yrbs2013.csv")
yrbs_raw[[5]] <- read.csv("../dat/yrbs2015.csv")
yrbs_raw[[6]] <- read.csv("../dat/yrbs2017.csv")

#q1 = age
# 1. 12 years old or younger
# 2. 13 years old
# 3. 14 years old
# 4. 15 years old
# 5. 16 years old
# 6. 17 years old
# 7. 18 years old or older

#q2: sex
# 1. Female
# 2. Male

#raceth
# 1 AI/AN NH
# 2 Asian NH
# 3 Black NH
# 4 NH/PI NH
# 5 White NH
# 6 Hisp
# 7 Hisp
# 8 Mult NH

# n (total)
round(sapply(1:6, function(year) sum(yrbs_raw[[year]]$weight)), 0)

# Percent mising data
pct_missing <- rep(NA,6)
for (year in 1:6)
  pct_missing[year] <-
  weighted.mean(is.na(yrbs_raw[[year]]$q1) | is.na(yrbs_raw[[year]]$q2),
    yrbs_raw[[year]]$weight)
round(pct_missing,3)

# Percent 14-18
pct_agerange <- rep(NA,6)
for (year in 1:6)
  pct_agerange[year] <-
    sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q1 %in% 3:7]) /
    sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q1 %in% 1:7])
round(pct_agerange,4)

# Remove non 14-18
for (year in 1:6)
  yrbs_raw[[year]] <- yrbs_raw[[year]][(yrbs_raw[[year]]$q1 %in% 3:7),]

# Percent female
pct_female <- rep(NA,6)
for (year in 1:6)
  pct_female[year] <-
    sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q2 %in% 1]) /
      sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q2 %in% 1:2])
round(pct_female,3)

# Remove non-females
for (year in 1:6)
  yrbs_raw[[year]] <- yrbs_raw[[year]][(yrbs_raw[[year]]$q2 %in% 1),]

# n (sub-sample)
round(sapply(1:6, function(year) sum(yrbs_raw[[year]]$weight)), 0)

# Percent by age
pct_age <- matrix(NA,5,6)
for (year in 1:6)
  pct_age[,year] <-
    sapply(3:7, function(x) sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q1==x]) /
      sum(yrbs_raw[[year]]$weight))
round(pct_age,3)

# Percent by raceethn
pct_ethn <- matrix(NA,8,6)
for (year in 1:6)
  pct_ethn[,year] <-
    sapply(1:8, function(x) sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$raceeth%in%x]) /
           sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$raceeth%in%1:8]))
round(rbind(pct_ethn[3,], colSums(pct_ethn[6:7,]), pct_ethn[5,], colSums(pct_ethn[c(1,2,4,8),])), 3)

# Percent ever had SI
# q58 : 2007, 2009
# q59 : 2013, 2017
# q60 : 2011, 2015

pct_eversex <- rep(NA,6)
for (year in 1:2)    pct_eversex[year] <-
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q58 %in% 1]) /
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q58 %in% 1:2])
for (year in c(4,6)) pct_eversex[year] <-
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q59 %in% 1]) /
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q59 %in% 1:2])
for (year in c(3,5)) pct_eversex[year] <-
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q60 %in% 1]) /
                       sum(yrbs_raw[[year]]$weight[yrbs_raw[[year]]$q60 %in% 1:2])
round(pct_eversex,3)

# Mean # lifetime partners (among those with any)
# q60 : 2007, 2009
# q61 : 2013, 2017
# q62 : 2011, 2015

for (year in 1:2) {
  yrbs_raw[[year]]$npart <- yrbs_raw[[year]]$q60-1
  yrbs_raw[[year]]$npart[yrbs_raw[[year]]$npart==0] <- NA
}
for (year in c(4,6)) {
  yrbs_raw[[year]]$npart <- yrbs_raw[[year]]$q61-1
  yrbs_raw[[year]]$npart[yrbs_raw[[year]]$npart==0] <- NA
}
for (year in c(3,5)) {
  yrbs_raw[[year]]$npart <- yrbs_raw[[year]]$q62-1
  yrbs_raw[[year]]$npart[yrbs_raw[[year]]$npart==0] <- NA
}

mean_part <- rep(NA,6)
for (year in 1:6) mean_part[year] <- weighted.mean(yrbs_raw[[year]]$npart, yrbs_raw[[year]]$weight, na.rm=TRUE)
round(mean_part, 1)

pct_6p <- rep(NA,6)
for (year in 1:6) pct_6p[year] <- weighted.mean(yrbs_raw[[year]]$npart%in%6, yrbs_raw[[year]]$weight)
mean(pct_6p)


