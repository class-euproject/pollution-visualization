
# Pollution-visualization

This application is a dashboard showing the pollution in the [MASA](https://www.automotivesmartarea.it/?lang=en) sector in Modena.

## Folder scheme

* Data contains all the data that would be generated by PHEMLight called **result_*.csv** and **stream.csv** which is the file that the dashboard would use to display the data.
* Images contains a image generated by mapview.
* R contains r files, dashboard-Modena.r is the dashboard application, and lanzador only rewrites stream.csv data. 

## Requirements


**THIS PROCESS WAS DONE ON An UBUNTU 18.04.5 LTS**

To execute this application is required to diferents library and R packages:
* R version 3.6.3 or superior
* R package [mapview](https://r-spatial.github.io/mapview/) 2.9.0
* R package [shiny](https://shiny.rstudio.com/) 1.5.0

```console
sudo apt install -y libssl-dev \
                    libcurl4-openssl-dev \
                    libudunits2-dev \
                    gdal-bin \
                    libgdal-dev \
                    libcairo2-dev \
                    libfontconfig1-dev \
                    libproj-dev \
                    libgeos-dev
```

```R
install.packages("devtools")
devtools::install_github("hadley/devtools")
install.packages("leaftlet")
install.packages("dplyr")
install.packages("mapview")
install.packages("shiny")
```



## Usage

### SHELL
To execute view shell:

```shell 
cd dir/to/pollution-visualization/R
Rscript "dashboard-Modena.r"
```
 
 
As a result it should show where the dashboard is running like this:

```shell 
Listening on http://0.0.0.0:8888
```

### R
To run the application run the R dashboard-Modena.r script as a background job.

Once the dashboard is running it will show a window with the dashboard, it will show the data saved in stream.csv inside the Data folder.
In order to simulate that the dashboard receives new data by running *lanzador.r*, this script will rewrite stream.csv periodically, such changes will be detected by the dashboard which will be updated automatically.

## Create roads.csv

In the Data folder there are as an example ***modena_roads.csv** and **test_roads.csv**. The process is a bit complex.

To paint a map line you need at least 2 points, these points must be, first latitude and longitude coordinates.

The *road_attiraglio* in this case goes *44.653813, 10.934223* to *44.655202, 10.934499*.

```R
road_attiraglio <- st_multilinestring(list(rbind(c(10.934070, 44.654792),c(10.934499, 44.655202))))
road_montalcini <- st_multilinestring(list(rbind(c(10.931728,44.655004),c(10.934223,44.653813))))
```

The next line creates the column **geometry**. This column is used to paint correctly on the map.

```R
modena_roads <- st_sfc(road_attiraglio2,road_canaletto2, crs="EPSG:4326")
```

To associate the LinkID with the street it would be with the following command that would create a dataframe

```R
d = st_sf(data.frame(LinkID=c("20939","20940"), geom=modena_roads))
```

Y last step to create the **.csv** file:

```R
st_write(d, "/home/bscuser/Work/pollution-visualization/Data/test_roads2.csv", driver = "CSV", layer_options = "GEOMETRY=AS_WKT")
```
