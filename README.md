## robubu
the rolling burning bus -  whatever, it runs fine for now 

====

robubu is a collection of functions, which have arisen from everyday scientific curiosity. Most of it is pretty hybrid integrating GRASS, SAGA, JS tools or other stuff in the R biotope. 

You will find both real tools like an optimized cost analysis for huge areas or the uav autonomous flight planning tool for DJI Phantom and 3DR Solo as well as nice minitols as leafDraw for digitizing in leaflet maps within Rstudio/Browser... 

It will never pass the cran check nevertheless maybe it is useful but at least it runs fine for now ...

```S
devtools::install_github("gisma/robubu", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/robubu", ref = "master", dependencies = TRUE, force = TRUE)
```

Have a look into the documentation and like always have Fun.
