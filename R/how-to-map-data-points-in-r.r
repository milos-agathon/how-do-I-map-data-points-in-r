################################################################################
#                 How do I map data points in R
#                 Milos Popovic
#                 2023/03/04
################################################################################
# libraries we need
libs <- c(
    "tidyverse", "stringr", "httr", "sf",
    "giscoR", "scales"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))


### 1) DATA WRANGLING
### -----------------
file_name <- "geonames-population-1000.csv"
### get Geonames table on places with population >= 1000
get_geonames_data <- function() {
    table_link <-
        "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/geonames-all-cities-with-a-population-1000/exports/csv?lang=en&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B&country_code=NL"
    res <- httr::GET(
        table_link,
        write_disk(file_name),
        progress()
    )
}

get_geonames_data()

### load the table
load_geonames_data <- function() {
    places_df <- read.csv(file_name, sep = ";")
    return(places_df)
}

places_df <- load_geonames_data()

head(places_df) # inspect the first few rows of the table
names(places_df) # inspect table names

places_modified_df <- places_df[, c(2, 7, 14, 20)] # select town name, country code, population and coordinates
names(places_modified_df) <-
    c("name", "country_code", "pop", "coords") # rename columns

head(places_modified_df)


places_modified_df[c("lat", "long")] <-
    stringr::str_split_fixed(
        places_modified_df$coords, ",", 2
    )

places_clean_df <- places_modified_df |>
    dplyr::select(-coords)

head(places_clean_df)

# define longlat projection
crsLONGLAT <-
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# transform data frame into sf object
places_sf <- places_clean_df |>
    sf::st_as_sf(
        coords = c("long", "lat"),
        crs = crsLONGLAT
    )

places_sf


### 2) BASIC MAPPING
### -----------------

# all towns
ggplot() +
    geom_sf(
        data = places_sf,
        color = "#7d1d53", fill = "#7d1d53"
    )

# filter UK towns
ggplot() +
    geom_sf(
        data = dplyr::filter(
            places_sf,
            country_code == "GB"
        ),
        color = "#7d1d53", fill = "#7d1d53"
    )

### ISO2 codes: https://www.iban.com/country-codes

### 3) POINTS WITHIN POLYGON
### -----------------------
# get UK national shapefile
uk <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "GBR"
) |>
    sf::st_transform(crsLONGLAT)

plot(uk)

# points within polygon with sf
uk_places <- sf::st_intersection(
    places_sf, uk)

plot(sf::st_geometry(uk_places))

# alpha value
ggplot() +
    geom_sf(
        data = uk_places,
        aes(size = pop),
        color = "#7d1d53", fill = "#7d1d53",
        alpha = .5
    ) +
    scale_size(
        range = c(1, 10),
        breaks = scales::pretty_breaks(n=6)
        )

### 4) LABELING
### ------------
# METHOD 1
uk_labeled_places <- places_clean_df |>
    dplyr::filter(
            country_code == "GB"
        ) |>
    dplyr::select(
        name, long, lat, pop) |>
    dplyr::arrange(desc(pop))

head(uk_labeled_places)


# METHOD 2
uk_labeled_places <- uk_places |>
    dplyr::mutate(
        long = unlist(map(geometry, 1)),
        lat = unlist(map(geometry, 2))
    ) |>
    dplyr::select(
        name, long, lat, pop) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::arrange(desc(pop))

head(uk_labeled_places)

# FINAL MAP
ggplot() +
    geom_sf(
        data = uk,
        color = "grey20", fill = "transparent"
    ) +
    geom_sf(
        data = uk_places,
        aes(size = pop),
        color = "#7d1d53", fill = "#7d1d53",
        alpha = .5
    ) +
    scale_size(
        range = c(1, 15),
        breaks = scales::pretty_breaks(n=6)
        ) +
    ggrepel::geom_text_repel(uk_labeled_places[1:10, ],
        mapping = aes(x = long, y = lat, label = name),
        colour = "grey20",
        fontface = "bold",
        size = 4
    ) +
    theme_minimal() +
        theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(
                c(t = 0, r = 0, b = 0, l = 0), "lines"
            ),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            panel.border = element_blank(),
        )
