# nanoAFMr 2.2.6

# nanoAFMr 2.2.5

* update ggplot `legend.position.inside`
* documentation to create AFM database sqlite

# nanoAFMr 2.2.4

* AFM rating can automatically save images to the SQL databse

# nanoAFMr 2.2.3

* return NULL object, if file name is not a recognized AFM format

# nanoAFMr 2.2.2

* fix plotting issue for non-square AFM images

# nanoAFMr 2.2.1

* fix the rating and particle size functions
* adding `AFM.add2DB` function that adds a particular image to the SQLite database

# nanoAFMr 2.2.0

* adding AFM rating
* add particle size measurement function `AFM.particleSize()`

# nanoAFMr 2.1.11

* remove functions from `raster` and move to `terra`
* fix minor bugs from `devtools::check()`
* add `AFM.setLine()` to set data for a specific line in the AFM image

# nanoAFMr 2.1.10

* improve method lineByLine in `AFM.flatten()`
* add method slope for `AFM.flatten()`, the slopes are generated with `AFM.flattenLine()`
* adding a zShift to the `AFM.flatten()` function

# nanoAFMr 2.1.9

* bug fix for setRange in `plot.AFMdata()`
* but fix for `AFM.linePlot()`, previously: the bottom left corner is (1,0), now it is (1,1)
* add methods to `AFM.flatten()`, including lineByLine method
* add method `AFM.partial()` that can identify partially recorded AFM images
* make `plot()` for `legend only` (gType==5) to have transparent background

# nanoAFMr 2.1.8

* fix channel 2 `plot.AFMdata()`
* add thicker lines for `AFM.lineProfile()` with addLines option
* fix bug in `AFM.readDB()`

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
