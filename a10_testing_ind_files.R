
# This is to confirm that the individual-level files Elizabeth Rosenthal sent 
#   sum up to the same weights as the summary files she sent earlier, so as to
#    insure an apples-to-apples comparison for the bootstrap.

years <- seq(2007, 2017, 2)
races <- c("White", "Black", "Hispanic")
ages <- 13:18
firstages <- c('13','14','15','16','17','18')
firstages <- 11:17

poptots_array <- eversex_array <- array(dim=c(3,6,6))
matrix1 <- matrix2 <- array(dim=c(6,length(firstages),3,6))

for (year in 1:length(years)) {
  filename <- paste(datapath, "/data_ind_", years[year], ".csv", sep="")
  temp1 <- read.csv(filename)
  for (race in 1:length(races)) {
    for(age in 1:length(ages)) {
      temp2 <- filter(temp1, 
                        Current.age==ages[age], 
                        X4.level.race.variable==races[race])
      poptots_array[race, age, year] <- sum(temp2$Analysis.weight)
      temp3 <- filter(temp1, 
                          Ever.had.sexual.intercourse=="Yes", 
                          Current.age==ages[age], 
                          X4.level.race.variable==races[race])
      eversex_array[race, age, year] <- sum(temp3$Analysis.weight)
      for (firstage in 1:length(firstages)) {
        temp4 <- filter(temp1, 
                        Ever.had.sexual.intercourse=="Yes", 
                        Current.age==ages[age], 
                        X4.level.race.variable==races[race],
                        At.at.first.sex==firstages[firstage]
                        )
        matrix1[age, firstage, race, year] <- sum(temp4$Analysis.weight)
        matrix2[age, firstage, race, year] <- 
          weighted.mean(as.numeric(temp4$Lifetime.number.of.sexual.partners),
                        temp4$Analysis.weight, na.rm = T)
      }
    }
  }
}
