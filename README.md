# SG Rental Map

An interactive R Shiny map of Singapore private residential rental contracts (URA data, Sep 2021 – Aug 2026, 412,595 contracts). It's styled after [viscover](https://github.com/XiaodanLyu/viscover).

## Features

- **Map:** one circle per building, coloured by median rent psf or median monthly rent and sized by number of contracts. Includes an optional heatmap layer and Light, OneMap and Satellite basemaps.
- **Click a building:** see its contract count, median rent and psf, bedroom breakdown and monthly trend.
- **Draw a shape:** use the polygon, rectangle or circle tool to summarise every contract in that area.
- **Filters:** lease month range, property type, bedrooms, postal district, geocode quality.
- **Export:** download the contracts in the current selection as CSV.

## Run it

```r
install.packages(c("shiny", "shinydashboard", "leaflet", "leaflet.extras",
                   "sf", "dplyr", "DT", "plotly", "readxl"))
shiny::runApp("sg_rental_map")
```

## Data

`sg_rental_map/data/rentals.rds` is a cleaned, fast-loading copy of
`URA_Union_Cleaned_with_Address.xlsx`. To refresh it:

1. Put the updated `URA_Union_Cleaned_with_Address.xlsx` in `sg_rental_map/data/`.
2. Delete `rentals.rds`.
3. Start the app. It rebuilds the `.rds` from the Excel file in about 30 seconds.

The Excel file itself is git-ignored because of its size (about 30 MB).

### Notes

- **Missing coordinates:** 1,647 contracts are not shown on the map.
- **Default geocode filter:** only *Exact* and *Good* matches are shown at first. *Street only* matches sit at the street rather than the building, and you can switch them on in the sidebar.
