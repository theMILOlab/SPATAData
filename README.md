# The MILOLab database for spatially resolved transcriptomic data

!! As long as data is not published we share data in collaboration projects, please request a source file with the corresponding links (dieter.henrik.heiland@uniklinik-freiburg.de) !!

Here, we provide a database for spatially resolved transcriptomic data that was curated by the MILO Lab and partners. The data derives from different studies across various projects. If any data is used for publications, datasets need to be specifically cited in accordance to their publication.See
instructions on citing down below.

Downloaded spata objects can be used within the SPATA2 package. More info: https://themilolab.github.io/SPATA2/

## Install package

```
library(devtools)

devtools::install_github("kueckel/confuns")
devtools::install_github("theMILOlab/SPATAData")

```

```

# load package
library(SPATAData)

```

## Overview

If you want to get an overview of all available datasets and metadata:

```
list.data()

```

If you want to obtain available sample names

```

validSampleNames()

```

## Downloading

Datasets can be downloaded via: 

```
# Downloads single spata objects, saves them on your device and immediately
# returns them

object <- downloadSpataObject(sample_name = "275_T")

# Downloads multiple spata objects at the same time  

downloadSpataObjects(sample_names = c("275_T", "334_T"))

```

For project-specific downloads, a customized source file has to be added. 

```
source.csv <- ".../your/path/to/csv/source.csv"

#update source file

SPATAData::addSourceFile(source.csv)

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






