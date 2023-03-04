################################################################################
#                 How do I map data points in R
#                 Milos Popovic
#                 2023/03/04
################################################################################

setwd("C:/users/milos/Downloads")
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




# DATA
#-----




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

head(places_df)






names(places_df)









places_modified_df <- places_df[, c(2, 7, 14, 20)]
names(places_modified_df) <-
    c("name", "country_code", "pop", "coords")

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

places_sf <- places_modified_df |>
    sf::st_as_sf(
        coords = c("long", "lat"),
        crs = crsLONGLAT
    )

places_sf



ggplot() +
    geom_sf(
        data = places_sf,
        color = "#7d1d53", fill = "#7d1d53"
    )





ggplot() +
    geom_sf(
        data = dplyr::filter(
            places_sf,
            country_code == "GB"
        ),
        color = "#7d1d53", fill = "#7d1d53"
    )

https://www.iban.com/country-codes


uk <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "GBR"
) |>
    sf::st_transform(crsLONGLAT)

plot(uk)



uk_places <- sf::st_intersection(
    places_sf, uk)

plot(sf::st_geometry(uk_places))


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
    






# ggplot2 theme
theme_for_the_win <- function() {
    theme_minimal() +
        theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            legend.position = c(.5, 1),
            legend.text = element_text(size = 60, color = "grey20"),
            legend.title = element_text(size = 70, color = "grey20"),
            legend.spacing.y = unit(0.25, "cm"),
            legend.spacing.x = unit(0.25, "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(
                c(t = 0, r = 0, b = 0, l = 0), "lines"
            ),
            plot.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.background = element_rect(fill = "#f5f5f2", color = NA),
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.border = element_blank(),
        )
}






# function for labelling regions
label_regions <- function() {
    ggrepel::geom_text_repel(deu_coords[1:5, ],
        mapping = aes(x = long, y = lat, label = NAME_LATN),
        colour = "grey20",
        family = "Montserrat",
        fontface = "bold",
        size = 20,
        segment.colour = "grey20",
        segment.alpha = .9,
        segment.linetype = 3,
        segment.size = .25,
        nudge_x = .95,
        nudge_y = .15,
        direction = "x"
    )
}

# CONSTANTS
#------------
# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"



# colors
cols <- rev(c(
    "#140e26", "#451a40",
    "#7d1d53", "#b32957",
    "#eb804e", "#ffdc58",
    "#eae2b7"
))

make_point_map <- function() {
    p2 <-
        ggplot() +
        geom_sf(
            data = deu_polygon,
            fill = "transparent",
            color = "grey20",
            size = .1
        ) +
        geom_sf(
            data = deu_points,
            mapping = aes(
                size = pop_1000s,
                geometry = geometry
            ), color = cols[5],
            alpha = .5
        ) +
        label_regions() +
        scale_size(
            breaks = breaks,
            range = c(1, 10),
            labels = round(breaks, 0),
            limits = c(vmin, vmax),
            name = ""
        ) +
        guides(
            color = "none",
            size = guide_legend(
                direction = "horizontal",
                title.position = "top",
                title.hjust = 0.5,
                label.hjust = 0,
                nrow = 1,
                byrow = F,
                reverse = F,
                label.position = "bottom"
            )
        ) +
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p2)
}

map2 <- make_point_map()
ggsave(
    filename = "germany_population_point.png",
    width = 6, height = 8.5, dpi = 600, device = "png",
    bg = "white", map2
)

