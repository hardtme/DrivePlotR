# DrivePlotR 0.2.0

## Improvements

* Added the `save_plotmap` function to save plot maps as standalone HTML files.

* Updated the default tile provider for the maps from "OpenStreetMap" to
"Esri.WorldStreetMap".

* Added the `tileprovider` argument to the `driveplot` and `driveplot_map` 
functions to prevent problems with the map tiles not rendering properly.

* Added support for groups by adding the `group` argument to the 
`driveplot_companion`, `driveplot_companions`, `driveplot_map` and `driveplot`
function.

* Plot maps now show a subtle gray line beneath the points to better distinguish
groups.

# DrivePlotR 0.1.0

* Initial CRAN submission.
