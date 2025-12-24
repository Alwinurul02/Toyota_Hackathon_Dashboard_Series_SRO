# Toyota Hackathon Dashboard Series SRO

![Dashboard Screenshot](https://via.placeholder.com/800x400?text=Dashboard+Preview) 

## Overview

This repository contains an interactive R Shiny dashboard built for the Toyota Hackathon, focused on analyzing race performance metrics from the 2025 TGRNA GR Cup North America series (Series SRO). The dashboard provides end-to-end data analysis, from raw CSV ingestion to insightful visualizations and AI-powered commentary, helping users uncover patterns in pace evolution, driver consistency, ultimate laps, standings, and more.

Key highlights:
- End-to-End Pipeline: Handles messy racing data (inconsistent formats, delimiters, time strings) with robust parsing, cleaning, feature engineering, statistical analysis, and visualization.
- AI Insights: A "Race Engineer Bot" delivers critical, comparative feedback based on statistical metrics like IQR for consistency and pace degradation deltas.
- Deployed Live: Explore the app at [https://nurulalawiyah.shinyapps.io/Toyota_Hackathon_Dashboard_Series_SRO/](https://nurulalawiyah.shinyapps.io/Toyota_Hackathon_Dashboard_Series_SRO/).

This project demonstrates advanced R skills in data science, inspired by a Harvard Statistics perspective rigorous, evidence-based, and actionable.

## Features

- Pace Evolution: Compare practice, qualifying, and race lap times with degradation analysis (e.g., mean deltas, field-adjusted comparisons).
- Biggest Movers: Track position gains/losses from qualifying to race finish.
- Race Story: Lap-by-lap position tracking with pit stop markers and weather context.
- Consistency Analysis: Box plots of lap time distributions (IQR-based scoring for precision).
- Ultimate Lap (Sectors): Theoretical best lap vs. actual, with sector breakdowns and execution scores.
- Standings: Driver and team championship visualizations with deviation from average.
- Dark Mode & Customization: Toggle for light/dark theme; driver highlighting with photos.
- Statistical Methods: Incorporates z-score outlier detection, median/IQR calculations, rank-based positioning, and hypothesis-inspired verdicts.

## Technologies Used

- R & Shiny: Core framework for the interactive app.
- Libraries: shiny, ggplot2, dplyr, readr, lubridate, tidyr, stringr, ggrepel.
- Data Handling: Custom parsers for semi-structured CSVs; time conversions with lubridate.
- Visualization: ggplot2 with custom themes and reactive plots.
- Deployment: Hosted on shinyapps.io.

## Installation

To run this dashboard locally:

1. Clone the repository:
  
   git clone https://github.com/yourusername/toyota-hackathon-dashboard.git
   cd toyota-hackathon-dashboard
   
2. Install required R packages:
  
   install.packages(c("shiny", "ggplot2", "dplyr", "readr", "lubridate", "tidyr", "stringr", "ggrepel"))
   
3. Prepare data:
   - Place your season data in a folder named 2025_season_TGRNA_GR_CUP_NORTH_AMERICA (or update SEASON_FOLDER_NAME in the code).
   - Expected file structure: Per circuit, include results CSVs like 03_Results GR Cup Race 1 Official.csv, lap analysis like 23_AnalysisEnduranceWithSections_Race 1.csv, etc.

4. Run the app:
  
   R -e "shiny::runApp('app.R')"
   
   Open http://127.0.0.1:port in your browser (port shown in console).

## Usage

1. Launch the app.
2. Select a circuit (e.g., "virginia-international-raceway") and race (e.g., "Race 1").
3. Click "Generate Race Report" to load and process data.
4. Highlight a driver for focused insights and photos (place driver images in /www/ named like DriverName.jpg).
5. Navigate tabs for different analyses; hover for tooltips, and check the AI box for stats-backed commentary.

Example workflow:
- Analyze pace: See quali-to-race deltas with arrows indicating improvement/degradation.
- Check consistency: Narrower boxes mean more reliable lap times (filtered to exclude pits/outliers via median * 1.2 threshold).

## Data Sources
- Hackathon-provided CSVs (results, laps, weather).
- Assumes files in season folder; adaptable to other motorsport data.

## Contributing

Feel free to fork and submit pull requests! Ideas for enhancements:
- Add more statistical tests (e.g., ANOVA for weather impacts).
- Integrate real-time API feeds.
- Expand to other series.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

Built as part of the Toyota Hackathon. Inspired by motorsport analytics and R's power for interactive data apps. Feedback welcome let's connect on LinkedIn!

[Explore the Live Dashboard](https://nurulalawiyah.shinyapps.io/Toyota_Hackathon_Dashboard_Series_SRO/) 
