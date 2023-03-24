##################################################################################
### function to modify Age Structured Assessment Program (ASAP) .dat file, run executable, and produce results object
###
library(ASAPplots)
stockName <- "codGB"
cod <- readRDS("hydraData_cod.rdat")

get_ASAP <- function(stock){
  
  out <- within(stock, {
    
    # read in assessment .dat file and modify accordingly
    dat_file <- dat_file1<- ReadASAP3DatFile(paste(stockName, ".dat", sep = ''))
    
    ### modify for each simulation/year
    
    #start year;
    dat_file$dat$year1 <- 1
    styear <- fmyearIdx - ncaayear
    
    #Change start years below to use moving window
    #dat_file$dat$year1 <- y - ncaayear
    #styear <- y - ncaayear
    
    #end year
    if(mproc[m,'Lag'] == 'TRUE'){
      endyear <- y-2
    }
    else if(mproc[m,'Lag'] == 'FALSE'){
      endyear <- y-1
    }
    
    #number of years in assessment
    N_rows <- length(styear:endyear)
    dat_file$dat$n_years <- N_rows
    
    #natural mortality
    dat_file$dat$M <- matrix(get_dwindow(natM, styear, endyear), nrow = N_rows, ncol = page)
    if (M_mis==TRUE){#If there is a natural mortality missepcification, the stock assessment will assume that M is the value in M_mis_val
      dat_file$dat$M <- matrix(M_mis_val, nrow = N_rows, ncol = page)
    }
    
    #maturity-at-age
    dat_file$dat$maturity <- matrix(get_dwindow(mat, styear, endyear), nrow = N_rows)
    
    #WAA matrix
    dat_file$dat$WAA_mats <-matrix(get_dwindow(waa, styear, endyear), nrow = N_rows)
    
    if(waa_mis==TRUE){#If there is a weight-at-age misspecification, the stock assessment will assume that weight at age was constant overtime (the first vector in the matrix).
      dat_file$dat$WAA_mats<-t(replicate(N_rows,waa[1,]))
    }
    
    #selectivity block (single block setup)
    dat_file$dat$sel_block_assign <- matrix(1, nrow = N_rows, 1)
    
    #catch-at-age proportions and sum catch weight
    dat_file$dat$CAA_mats <- list(cbind(cbind(cod$paaCN$one, cod$paaCN$two,cod$paaCN$three, cod$paaCN$four, cod$paaCN$five, cod$paaCN$six, cod$paaCN$seven, cod$paaCN$eight, cod$paaCN$nine, cod$paaCN$ten), cod$sumCW$catch))
    
    #discards - need additional rows even if not using
    dat_file$dat$DAA_mats <- matrix(0, nrow = N_rows, ncol = page + 1)
    
    #release - also need additional rows if not using
    dat_file$dat$prop_rel_mats <- matrix(0, nrow = N_rows, ncol = page)
    
    #index data; sum index value, observation error, proportions-at-age, sample size
    dat_file$dat$IAA_mats <- list(cbind(cod$sumIN$year, cod$sumIN$biomass, rep(0.05, 37),cbind(cod$paaIN$one, cod$paaIN$two, cod$paaIN$three, cod$paaIN$four, cod$paaIN$five, cod$paaIN$six, cod$paaIN$seven, cod$paaIN$eight, cod$paaIN$nine, cod$paaIN$ten), rep(0.05, 37))) #year, value, CV, by-age, sample size
    
    #recruitment CV
    dat_file$dat$recruit_cv <- matrix(pe_RSA, nrow = N_rows, 1)
    
    #catch CV
    dat_file$dat$catch_cv <- matrix(0.05, nrow = N_rows, 1)
    
    #discard CV - need additional years even if not using
    dat_file$dat$discard_cv <- matrix(0, nrow = N_rows, 1)
    
    #catch effective sample size
    dat_file$dat$catch_Neff <- matrix(oe_paaCN, nrow = N_rows, 1)
    
    #discard ESS (even if not using)
    dat_file$dat$discard_Neff <- matrix(0, nrow = N_rows, 1)
    
    if(mproc[m,'Lag'] == 'TRUE'){
      dat_file$dat$nfinalyear <- y-1
    }
    else if(mproc[m,'Lag'] == 'FALSE'){
      dat_file$dat$nfinalyear <- y
    }
    
    dat_file$dat$proj_ini <- c((y), -1, 3, -99, 1)
    dat_file$dat$R_avg_start <- styear
    dat_file$dat$R_avg_end <- endyear - 10
    
    if (Sys.info()['sysname'] == "Windows") {
      
      # save copy of .dat file by stock name, nrep, and sim year
      WriteASAP3DatFile(fname = paste(stockName,'1.dat', sep = ''),
                        dat.object = dat_file,
                        header.text = paste(stockName, 'Simulation', sep = '_'))
      
      # write .dat file needs to have same name as exe file
      WriteASAP3DatFile(fname = paste('ASAP3.dat', sep = ''),
                        dat.object = dat_file,
                        header.text = paste(stockName, 'Simulation',sep = '_'))
      
      # Run the ASAP assessment model
      asapEst <- try(system('ASAP3.exe', show.output.on.console = TRUE))
      
      # Read in results
      res <- dget('ASAP3.rdat')
      
      # save .Rdata results from each run
      saveRDS(res, file = paste(stockName,'results.rdat', sep = ''))
      
      # save .par file
      #file.copy("asap3.par", paste('assessment/ASAP/', stockName, '_', r, '_', y,'.par', sep = ""), overwrite = TRUE)
      
    }
    
    if (Sys.info()['sysname'] == "Linux") {
      
      # save copy of .dat file by stock name, nrep, and sim year
      WriteASAP3DatFile(fname = paste(rundir, '/', stockName, '_', r, '_', y,'.dat', sep = ''),
                        dat.object = dat_file,
                        header.text = paste(stockName, 'Simulation', r, 'Year', y, sep = '_'))
      
      # write .dat file needs to have same name as exe file
      WriteASAP3DatFile(fname = paste(rundir, '/ASAP3.dat', sep = ''),
                        dat.object = dat_file,
                        header.text = paste(stockName, 'Simulation', r, 'Year', y, sep = '_'))
      
      tempwd <- getwd()
      setwd(rundir)
      asapEst<- try(system("singularity exec $WINEIMG wine ASAP3.EXE", wait = TRUE))
      
      while (!file.exists('asap3.rdat')) {
        Sys.sleep(1)
      }
      
      # Read in results
      res <- dget('asap3.rdat')
      
      # save .Rdata results from each run
      saveRDS(res, file = paste(rundir, '/', stockName, '_', r, '_', y,'.rdat', sep = ''))
      
      #save .par file
      #file.copy("asap3.par", paste(rundir, '/', stockName, '_', r, '_', y,'.par', sep = ""), overwrite = TRUE)
      
      setwd(tempwd)
      
    }
    
  })
  
  return(out)
  
}

ifelse((asapEst == 0), 1, 0)
parpop <- list(waa = tail(res$WAA.mats$WAA.catch.fleet1, 1),
               sel = tail(res$fleet.sel.mats$sel.m.fleet1, 1),
               M = tail(res$M.age, 1),
               mat = res$maturity[1,],
               R = res$SR.resids$recruits,
               SSBhat = res$SSB,
               J1N = tail(res$N.age,1),                 
               Fhat = tail(res$F.report, 1))
