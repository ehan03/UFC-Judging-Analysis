# UFC-Judging-Analysis

Final project for S&DS 625 (Statistical Case Studies), Fall 2024

This project was developed using R version 4.4.1.

## File Structure

- Folders
    - `data`: Contains all data used in the project. The top-level contains the final merged dataset. The subfolders `UFC Stats` and `MMA Decisions` hold both the raw and cleaned data for their corresponding website sources.
    - `deliverables`: Contains abstract and report PDFs.
    - `misc`: Contains bibliography BibTeX file and Jay's report template for reference.

- R Scripts
    - `00_install_packages.R`: Install all packages outside of base R used for this project.
    - `01_clean_ufcstats_data.R`: Clean the pre-collected UFC Stats data and persist to disk.
    - `02_scrape_mmadecisions.R`: Scrape MMA Decisions with some preliminary cleaning and formatting included and persist to disk.
    - `03_clean_mmadecisions_data.R`: Further clean the partially raw data from MMA Decisions, focusing on fighter information and deduplicating fighter IDs.
    - `04_merge_data.R`: Create one-to-one mappings between fighters, events, and bouts in the UFC Stats and MMA Decisions datasets, such that each row of the final dataset represents a single judge's score for a round with the corresponding round fight metrics from both fighters. Conduct final preprocessing of this final dataset and persist to disk.
    - `05_analyze_judge_scores.R`: Catch-all for data exploration, feature generation, and statistical modeling.
    - `06_create_report.R`: Create the knitted report in PDF format.

As implied by the numbered prefixes, the R scripts should be run in the above order to replicate results. I've added logic in `02_scrape_mmadecisions.R` that prevents the actual scraping from running if the output files are already present since this takes a very long time.