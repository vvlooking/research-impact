# Quick Summary

## Checklist

- [ ] Create figure: "Total Citations of Documents Published Each Year"
- [ ] Calculate sum of times cited
- [ ] Calculate the average number of citations per document
- [ ] List the top three most cited documents

## Create Figure: "Total Citations of Documents Published Each Year"
Create a stacked line graph to visualize the total number of citations and total number of documents published each year. In the script below, update the following lines with the correct file paths: `path_to_combined-deduplicated-standardized.xlsx` and `path_to_total-citations-of-documents-published-each-year.png`. Update the publication years for `PY >= START_YEAR, PY >= END_YEAR`, `breaks = START_YEAR:END_YEAR`, and `limits = c(START_YEAR, END_YEAR)`.

```r
# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)

# Import spreadsheet (Excel)
df <- read_excel("path_to_combined-deduplicated-standardized.xlsx") # adjust file path

# Summarize by year
year_summary <- df %>%
  mutate(
    PY = suppressWarnings(as.integer(PY)),
    TC_num = readr::parse_number(as.character(TC))
  ) %>%
  filter(!is.na(PY), PY >= START_YEAR, PY <= END_YEAR) %>% # adjust publication years
  group_by(PY) %>%
  summarise(
    Publications     = n(),
    Total_Citations  = sum(TC_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PY)

print(year_summary)

# Format for faceting
plot_data <- year_summary %>%
  pivot_longer(
    cols = c(Publications, Total_Citations),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("Publications", "Total_Citations"),
      labels = c("Publications", "Total Citations")
    )
  )

# Stacked line plot
p <- ggplot(plot_data, aes(x = PY, y = value, color = metric)) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Publications"    = "#73000a",
      "Total Citations" = "#1F414D"
    )
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR, # adjust publication years
    limits = c(START_YEAR, END_YEAR) # adjust publication years
  ) +
  labs(
    x = "Year",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",              
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

# Save graph
ggsave(
  filename = "path_to_total-citations-of-documents-published-each-year.png", # adjust file name
  plot     = p,
  width    = 7,
  height   = 7,
  dpi      = 300
)
```
