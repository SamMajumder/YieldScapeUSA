
# 🌾 YieldScapeUSA 🌾

YieldScapeUSA is an interactive visualization tool designed to illustrate agricultural yield data across the United States. Initially showcasing oilseed sunflower yield data across counties in North Dakota (ND), South Dakota (SD), and Minnesota (MN), the app now provides a platform for users to upload and visualize their own yield data across different crops and regions.

## 🌐 Deployment (check out the app)

YieldScapeUSA is deployed on ShinyApps.io and can be accessed [here](https://sammajumder.shinyapps.io/YieldScapeUSA/).

## 🚀 Features

- **Interactive Map 🗺️**: Engage with a dynamic map to explore yield data across various geographical regions.
- **Custom Data Upload 📤**: Upload your own RDS data file to visualize yield statistics for different crops and locations.
- **Dynamic Selections ⚙️**: Choose geometry, time, administrative, and value columns from your data to suit your analysis needs.
- **Dynamic Time Slider ⏳**: Adjust the time slider to view yield data for specific years, dynamically changing based on your data.
- **Color-Coded Yield Values 🎨**: Yield values are color-coded for visual distinction between high and low yield areas. (Choropleth maps)
- **Detailed Hover Information ℹ️**: Hover over a region to view detailed yield information, including state and county names alongside yield values.
- **Admin Level Analysis 📊**: Visualize average yield trends over time with line plots, faceted by the first level of administrative division.
- **Admin 2 Level Analysis 📈**: Explore the top 5 counties in terms of total yield with bar plots, faceted by the first level of administrative division.
- **Customizable Color Palette 🌈**: Choose different color palettes for the line and bar plots to suit your visual preferences.


## 📊 Data

The data visualized in the app is user-provided, allowing for a wide variety of agricultural yield data to be displayed. This feature enables the app to serve as a flexible tool for analyzing yield trends across different crops and regions over time.

### Input Expectations 📑
The app accepts RDS files containing spatial data. Here are the expectations for the input data to ensure accurate visualization:

1. **File Format**: The data file should be in RDS format.
2. **Spatial Data**: The data should contain spatial geometry information for accurate mapping.
3. **Columns**:
   - A `Geometry` column representing the spatial geometry of each data point.
   - A `Time` column representing the time dimension (e.g., year) of the data.
   - An `Admin` column representing the first level of administrative division (e.g., State).
   - An `Admin 2` column representing the second level of administrative division (e.g., County).
   - A `Value` column representing the yield values or any other values that you wish to visualize

   The actual column names in your data can be different, as you will direct the app to the appropriate columns through dropdown selections.

### Example Data Structure 📋
```plaintext
| Geometry | Time | Admin | Admin 2 | Value |
|----------|------|-------|---------|-------|
| ...      | 1976 | ND    | Stark   | 1234  |
| ...      | 1976 | ND    | Dunn    | 5678  |
| ...      | 1977 | SD    | Jones   | 9101  |
| ...      | 1977 | SD    | Lyman   | 1121  |

```

## 💻 Technologies

- **R**: The app is built using the R programming language.
- **Shiny**: Shiny is employed for creating the interactive web application.
- **Leaflet**: Leaflet facilitates rendering the interactive map.
- **sf**: The `sf` package is used for handling spatial data.

## 📈 Future Expansions

- **Additional Built-in Data Sets 🌽**: Integrate yield data for other major crops to provide built-in data sets for users.
- **Enhanced Filtering and Analytics 🔍**: Implement advanced filtering options and analytics to offer deeper insights into yield trends.


## 🛠️ Local Setup

1. Clone the repository to your local machine.
2. Open RStudio and set the working directory to the cloned repository folder.
3. Install the required R packages listed at the beginning of the `app.R` file.
4. Run the `app.R` file in RStudio to launch the app locally.

## 🤝 Contributions

Feel free to fork the repository, create feature branches, and send me your pull requests for enhancements!

## 📜 License

[MIT License](LICENSE)
