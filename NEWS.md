# nanoAFMr 2.1.8

* fix channel 2 `plot.AFMdata`
* add thicker lines for `AFM.lineProfile` with addLines option

# nanoAFMr 2.1.7

* fix Unicode issues
* list all AFM images in database with `AFM.readDB`
* allow an AFM image to be removed from the database with `AFM.writeDB` and `NULL` object

# nanoAFMr 2.1.6

* add Kurtosis

# nanoAFMr 2.1.5

* update documentation and HHCF return values in table, returns Hurst parameter and roughness correctly
* add a random seed to the HHCF, so that results are reproducible


# nanoAFMr 2.1.4

* fix units for Park images (showed um instead of nm)


# nanoAFMr 2.1.3

* update helper functions and see-also
* update to new ggplot2 3.3+ conventions with `after_stat`
* Support database export and import using `AFM.writeDB` and `AFM.readDB`

# nanoAFMr 2.1.2

* update the `AFM.hhcf` documentation
* allow HHCF to export fit information, using `AFM.hhcf(allResults=TRUE)`

# nanoAFMr 2.1.1

* add graphType=5 for `plot` to only show legend


# nanoAFMr 2.1

* fix `summary.AFMdata()` for noImage data types such as force curves
* data analysis of AFM data files
