# The MILOLab Database for spatially resolved Transcriptomic data


Here, we provide a database for spatially resolved transcriptomic data that were curated by the MILO Lab and patners. The data are from different studies across various projects. If any data were used for publications, datasets need to be specificly cited in accordance to their publication.


Data can be used within the SPATA2 package. More info: https://themilolab.github.io/SPATA2/

## Install package

```
library(devtools)
devtools::install_github("heilandd/SPATAData")
```

If you want to get an overview of all avaiable datasets and metadata:

```
SPATAData::list.data()
```

Datasets can be downloaded by: 

```
#Download a/or more samples
get.data <- SPATAData::getData(sample.name = "#UKF334_T")
#Load data 
object <- SPATAData::loadData(sample.name = "#UKF334_T", folder=get.data[[1]])

```

In case of any questions, please do not hesitat to cantact us!! The MILO Lab




