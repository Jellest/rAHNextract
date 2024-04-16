# rAHNextract v0.98
* Released on: 11-04-2024.

## Breaking changes
* AHN1-AHN3 is currently not supported to be used. Please change it to 'AHN' to always use the latest version made available by PDOK. Currently that is AHN4.
* The default extraction method to receive the AHN elevation is changed to 'simple' (nearest) instead of bilinear. Please see this [article](https://gisgeography.com/raster-resampling/) that explains the difference.
* In this release downloading point clouds is not supported.
* It is now possible to retrieve AHN elevation data at exact intersection points of 2 or more AHN sheets.

## Other changes
* AHN4 is the latest full AHN version made available by PDOK. This release now supports this AHN version.
* in `ahn_area()` introduced the ability to only download AHN sheets using the `sheets` parameter.
* Introduced the function `ahn_sheets_info()` to retrieve information about the AHN sheets and which URLs are used for download. 
* Removed the ability to use older AHN versions (AHN1-AHN3). It is in the planning to reintroduce this in a future release.
* Removed the ability to download point cloud data from the AHN. This will possibly be reintroduced in a future release.
* Adjusted URLs so that AHN datasets hosted by PDOK can be retrieved again.
* The code now looks at source URLs from data frames instead of hard coded references in the code.
* Efficient checks are built in to see if the AHN version called is currently supported.   
* Reorganised code and removed deprecated/unsupported functions.
* Removed all dependencies of the deprecated 'rgdal' package. 'Terra' package is now used instead of 'raster'.
* Updated all the dependent package to the the latest versions.
* improved R syntax throughout the whole code.

# rAHNextract v0.97
* Released on: 15-05-2020.
* default values fixes.
* remove re-download sheet functionality.
* typos and documentation improvements.


# rAHNextract v0.96
* Released on: 25-04-2020.
* small missing variables and dependency fixes.

# rAHN extract v0.95
* Released on 22-04-2020.
* pre-release version to be used for testing with last users.
* see README for features and instructions.
