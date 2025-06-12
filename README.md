# Flicker-maps
In this code I build on the already analysed dataset to summarise and map the Images retrieved from Flicker. No original data could be provided because of anonymity and ethic reasons. However the code shows the procedure of creating maps, for both each Flicker post, and also aggregating them at a datazone level. 

This is a completely working code, in my View. There is no dataset though. A new development was to make human written post and the tags anonymous, by applying a function for that. I worked but not completely, so we did not used that in the end. 
While the first part of the code saves a leaflet .html file. An additional part of the code instead saves the files in a geojason and shapefiele (Which needs to be zipepd manually). The geojason seems to be working fine, but the upload on ArcGIS onlyne is not properly working with some numeric fields - like the sentiment one. 

I could not upload the original file of the SIMD, but please see the file here "https://github.com/Stephen137/Scottish-Index-of-Multiple-Deprivation". Anyway this is a common Shapefile of multideprivation for datazones.
