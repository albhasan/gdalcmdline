# gdalcmdline
Yet another R wrapper for GDAL's utilities.

This package invokes GDAL's utilies using simple system calls and file paths. It 
adds suppors for `gdal_callc` (which is missing from similar packages such as 
`gdalUtils` and `gdalUtilities`), it is magrittr-friendly (by re-ordering 
functions arguments and using temporal as outputs) and to some extent, 
tidy-verse friendly.


## Installation.

```
devtools::install_github("albhasan/gdalcmdline")
```

