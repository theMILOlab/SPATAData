# The MILOLab database for spatially resolved transcriptomic data

!! As long as data is not published we share data in collaboration projects, please request a source file with the corresponding links (dieter.henrik.heiland@uniklinik-freiburg.de) !!

Here, we provide a database for spatially resolved transcriptomic data that was curated by the MILO Lab and partners. The data derives from different studies across various projects. If any data is used for publications, datasets need to be specifically cited in accordance to their publication. See
instructions on citing down below.

Downloaded spata objects can be used within the SPATA2 package. More info: https://themilolab.github.io/SPATA2/

## Install package

```
library(devtools)

devtools::install_github("kueckelj/confuns")
devtools::install_github("theMILOlab/SPATAData")

```

```

# load both packages!
library(SPATAData)
library(SPATA2)

```

## Overview

If you want to get an overview of all available datasets and metadata:

```
sourceDataFrame()

```

## Interactive downloads

All samples can be downloaded and visualized interactively with: 

```
launchSpataData()

```
This function provides access to a shiny application in which all available samples 
are listed and displayed - sorted by organ, pathology and additional meta data.


## Downloading

To obtain valid input options for samples names: 

```
validSampleNames()

```

S4 spata objects can be downloaded via: 

```
# Downloads single spata objects, saves them on your device and immediately
# returns them

object <- downloadSpataObject(sample_name = "275_T")

# Downloads multiple spata objects at the same time  

downloadSpataObjects(sample_names = c("275_T", "334_T"), folder = "objects")

```

## Citation

To obtain proper citation: 

```
# for the object you are working with
getCitation(object)

# by sample 
getCitationBySample(sample_names = "275_T")

```

If you have any questions, please do not hesitate and contact us. 






