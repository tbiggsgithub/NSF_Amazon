


rmd.dir = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R-master/Rmd_files/UTF8_resave/"
rmdlist = list.files(rmd.dir)
R.dir = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R-master/Rfiles_tb/"
Rnames = paste0(substr(rmdlist,1,nchar(rmdlist)-4),".R")
knitr::purl(paste0(rmd.dir,rmdlist[1]), paste0(R.dir,Rnames[1]) , documentation = 2)

for (i in 2:length(rmdlist)){
  knitr::purl(paste0(rmd.dir,rmdlist[i]), paste0(R.dir,Rnames[i]) , documentation = 2)
}


# Individual lines as needed
i=2
knitr::purl(paste0(rmd.dir,rmdlist[i]), paste0(R.dir,Rnames[i]) , documentation = 2)
