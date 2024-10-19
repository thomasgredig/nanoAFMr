[![DOI](https://zenodo.org/badge/580499036.svg)](https://zenodo.org/badge/latestdoi/580499036)

# nanoAFMr <img src="man/figures/logo.png" align="right" width="250"/>

Imports and analyzes Atomic Force Microsocpy (AFM) images; currently four types are supported, images from Nanosurf (.nid), Bruker / Veeco Multimode Nanoscope III (.000), Park AFM images (.tiff), and Asylum Research AFM Igor images (.ibw).

This package provides three main advantages:

-   Using this data class, a series of images can be analyzed uniformly and quickly within R;
-   Access to each data point allows us to integrate mathematical models directly to the original data;
-   Part of reproducible data science that processes raw data directly into publishable figures.

## Installation

Using `R`, install the latest release from the GitHub repository:

``` r
# install.packages("devtools")
devtools::install_github("thomasgredig/nanoAFMr")
```

## Data Classes

Several new data structures (S3 and S4) are introduced:

-   `AFMdata` - S4 class that contains all AFM images from a file
-   `AFMinfo` - S3 class that contains all parameters of the AFM images, such as vibration frequency, etc.
-   `AFMmath` - S3 class that contains computed parameters, such as roughness, etc. about a particular AFM image

## Usage

There is a complete description for nanoAFMr library functions; examples in the Article Vignettes are also available.

### AFM data

Use this package to generate `AFMdata` S4 data objects, which can be used to quickly display AFM data and also process AFM data. Here are some examples:

Creating an `AFMdata` data object, then output a graph and output a summary of characteristics:

``` r
fileAFM = AFM.getSampleImages('ibw')
d = AFM.import(fileAFM)
class(d)
plot(d)
print(d)
summary(d)
```

### AFM info

Use the `AFMinfo()` function to obtain information about the AFM image. The information depends on the file format, but some items are common for all images, see `names(h)`, such as `widthPixel`, `scanRate.Hz`, etc.; others can be obtained with `AFMinfo.item()`.

``` r
afmInfo = AFMinfo(AFM.getSampleImages(type='ibw'))
AFMinfo.item(afmInfo, 'ScanSpeed')
```

The AFM image information can also be exported to a file.

``` r
write.csv(afmInfo$data, file='AFMinfo.csv', row.names = FALSE)
```

### AFM math

Some properties about the image are included in the `AFMdata` object, but others - like the roughness - need to be computed. In order to obtain, the computed values, use the `AFMmath` object, which is generated from the `AFMdata` class.

``` r
filename = AFM.getSampleImages(type='ibw')
afmMath = AFM.math.params(AFM.import(filename))
summary(afmMath)
afmMath$Ra
```

## Image Analysis

Several functions are available for image analysis, including:

-   Flatten image
-   Histogram
-   Profile Liens
-   Roughness Analysis
-   Height-height Correlation Function

Example to extract image roughness for a series of images:

``` r
file.list = AFM.getSampleImages()
Ra = c()
for(filename in file.list) {
  Ra = c(Ra, AFM.math.params(AFM.import(filename))$Ra)
}
data.frame(
  name = basename(file.list),
  Ra
)
```

## AFM Image Rating

Finding AFM images based on quality is integrated with the AFM rating system. A user is shown the image and can assign a rating based on the quality of the image. The rating data is user specific and stored in the AFM SQlite database. Use `AFM.readRatings()` and `AFM.writeRatings()` to read / write ratings.

# Acknowledgments

This material is based upon work supported by the National Science Foundation under Grant Number 2018653. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author and do not necessarily reflect the views of the National Science Foundation.
