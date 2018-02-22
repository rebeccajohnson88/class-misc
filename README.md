# class-misc
Miscellaneous code for classes / etc. This includes:

1. **Code for checking and changing projections in spatial polygon data**

As part of an activity for Professor Roberto’s Soc 413, "Spatial analysis in the social sciences," we worked with spatial polygon data. In order to plot these data on the same map, the data should have the same projections (a coordinate reference system). The function in the code takes in a list of spatial polygon objects, checks their projections, and uses the SpTransform function in the rgdal package to change the projection of spatial polygon(s) with discordant projection(s).