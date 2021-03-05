
pred_bctype_minLARC_dyn_boot_compiled <- list()

for(bctype in 1:6) {
  pred_bctype_minLARC_dyn_boot_compiled[[bctype]] <- array(dim=c(3,6,11,10))
  for(repnum in 1:10) {
    pred_bctype_minLARC_dyn_boot_compiled[[bctype]][,,,repnum] <- 
      pred_bctype_minLARC_dyn_boot[[repnum]][[bctype]][,,]
  }
}

pdf("pred_values.pdf")
for(bctype in 1:6) {
  for(race in 1:3) {
    for(age in 1:6) {
      boxplot(t(pred_bctype_minLARC_dyn_boot_compiled[[bctype]][race,age,,]), ylim=c(0,1), 
              main = paste("bctype ", bctype, ", race ", race, ", age ", age+12, sep=""))
      lines(pred_bctype_minLARC_dyn[[bctype]][race,age,], type='l', lwd=3) 
    }
  }
}
dev.off()



aaa <- (eversex_f/wts_f)[,,1]
bbb <- apply(eversex_boot_yes[,,1,] / (eversex_boot_yes[,,1,] + eversex_boot_no[,,1,]),
      1:2, mean)
#ccc <- eversex_boot_yes[,,1,1] / (eversex_boot_yes[,,1,1] + eversex_boot_no[,,1,1])

matplot(t(bbb/aaa), type='b')
#matplot(t(ccc/aaa), type='b')

sum((temp2 %>% filter(race=='white', age==14))$weight)
wts_f[3,2,1]


temp2 %>% filter(race=="hispanic" & age==13)


