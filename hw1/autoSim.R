# autoSim.R
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

nVals = seq(100, 500, by = 100)
distTypes = c("gaussian", "t1", "t5")
seed = 203
rep = 50
for (n in nVals) {
  for (i in distTypes) {
    oFile <- paste(shQuote(i), "_dist_n", n, ".txt", sep="")
    arg = paste("n=", n,
                " seed=", seed, " rep=", rep, " dist=", shQuote(shQuote(i)), sep="")
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile, sep="")
    system(sysCall, wait = FALSE)
    print(paste("sysCall=", sysCall, sep=""))
  }
}
