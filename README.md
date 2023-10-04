# YieldScapeUSA

YieldScapeUSA is an interactive visualization tool that showcases the historical yield of oilseed sunflower across different counties in the states of North Dakota (ND), South Dakota (SD), and Minnesota (MN) from 1976 to 2022. The app is designed to provide insights into yield variations at a county and state level, with the flexibility to expand to other crops in the future.

## Features

- **Interactive Map**: Navigate through an interactive map to explore yield data across different counties.
- **Dynamic Time Slider**: Adjust the time slider to view yield data for specific years.
- **State Selection**: Select a particular state to zoom in and explore the yield data at a more granular level.
- **Color-Coded Yield Values**: Yield values are color-coded to provide a visual indication of high and low yield areas.
- **County-Level Yield Information**: Hover over a county to view the specific yield value.

## Data

The data used in this app is sourced from the USDA Quick Stats database using the `tidyUSDA` R package. The current version of the app focuses on oilseed sunflower yield, with plans to incorporate other major crops in the near future.

The datasets are cleaned and processed to ensure accuracy and to remove entries with insufficient data.

## Technologies

- **R**: The app is built using the R programming language.
- **Shiny**: Shiny is used for building the interactive web application.
- **Leaflet**: Leaflet is used for rendering the interactive map.
- **tidyUSDA**: The `tidyUSDA` package is used to fetch data from the USDA Quick Stats database.

## Future Expansions

- **Additional Crops**: Integrate yield data for other major crops.
- **Enhanced Filtering**: Implement enhanced filtering options to allow users to view data based on different criteria.
- **Advanced Analytics**: Incorporate advanced analytics to provide deeper insights into yield trends over time.

## Deployment

YieldScapeUSA is deployed on ShinyApps.io and can be accessed [here](https://sammajumder.shinyapps.io/YieldScapeUSA/).

## Local Setup

1. Clone the repository to your local machine.
2. Open RStudio and set the working directory to the cloned repository folder.
3. Install the required R packages listed at the beginning of the app.R file.
4. Run the `app.R` file in RStudio to launch the app locally.

## Contributions

Feel free to fork the repository, create a feature branch, and submit a Pull Request if you have features or fixes you'd like to contribute.

## License

[MIT License](LICENSE)
