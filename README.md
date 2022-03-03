# The MILOLab Database for spatially resolved transcriptomic data

!! As long as data are not published we share data in collaboration projects, please request a source file with the corresponding links (dieter.henrik.heiland@uniklinik-freiburg.de) !!



Here, we provide a database for spatially resolved transcriptomic data that were curated by the MILO Lab and patners. The data are from different studies across various projects. If any data were used for publications, datasets need to be specificly cited in accordance to their publication.


Data can be used within the SPATA2 package. More info: https://themilolab.github.io/SPATA2/

## Install package

```
library(devtools)
devtools::install_github("theMILOlab/SPATAData")
```

If you want to get an overview of all avaiable datasets and metadata:

```
SPATAData::list.data()
```

Datasets can be downloaded by: 

```
#Download a/or more samples
get.data <- SPATAData::getData(sample.name = "275_T")
#Load data 
object <- SPATAData::loadData(sample.name = "275_T", folder=get.data[[1]])

```

For project-specific downloads, a customized source file has to be added. 

```
source.csv <- ".../your/path/to/csv/source.csv"
#update source file
SPATAData::AddSourceFile(source.csv)

```





In case of any questions, please do not hesitat to cantact us!! The MILO Lab




