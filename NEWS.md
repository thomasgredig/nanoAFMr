# nanoAFMr 2.3.4

* fix reading SQL AFM image that is neither Park nor Cypher image

# nanoAFMr 2.3.3

* add new flatten method: AutoMask for flat surfaces with particles
* `AFM.getCSV` creates a CSV file for export
* fix setRange() argument in `plot.AFMdata`, so that pixels are not overwritten
* add more items to `AFMinfo()`: image size in um and scanning angle
* fix warning in loading NID header, when end of line is missing

# nanoAFMr 2.3.2

* cannot load certain NID files, issue with channels in NID.getHeaderSet()

# nanoAFMr 2.3.1

* reduce size and dependencies of code for submission
* AFM.add2DB is superseded by writeAFM2DB in dataProjectTemplate
* AFM images can be removed with AFM.writeDB() instead
* NID files have units converted from `m` to `nm` to be consistent with other formats
* import `.ibw` through Igor wave files. 
* fix raster and plot of NID file spectroscopy

# nanoAFMr 2.3.0

* clean up code to make available to CRAN

# nanoAFMr 2.2.7

* adding creation date to the AFMdata object (only supported with Park images so far)

# nanoAFMr 2.2.5

* update ggplot `legend.position.inside`
* documentation to create AFM database sqlite

# nanoAFMr 2.2.4

* AFM rating can automatically save images to the SQL database

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
