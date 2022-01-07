sxtTools::setwd_project()
setwd("other_files/demo_data/")

## Open a MS file and read the spectrum and header information
library(mzR)
mz <- openMSfile("QC_1.mzXML", backend = "pwiz")

## Get the peaks
pks <- mzR::peaks(mz)
## Get the header
hdr <- header(mz)

###if you want to modify pks, just modify here.
###if you want to modify rt, for example, add 10 second.
hdr$retentionTime = hdr$retentionTime + 100

## Write the data to a mzXML file.
mzR::writeMSData(object = pks, file = "test.mzXML", header = hdr, outformat = "mzxml")

library(MSnbase)

data = MSnbase::readMSData(c("QC_1.mzXML", "test.mzXML"), msLevel. = 1)

tof_tic <- MSnbase::chromatogram(data)
plot(tof_tic, col = c("red", "blue"))



