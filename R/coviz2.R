# Indonesia Amongst ASEAN

# # Load package
# library(tidyverse)
# library(scales)
# 
# # Import data world_covid.csv
# world_covid <- read.csv(file = "data-raw/world_covid.csv", header = TRUE, 
#                         stringsAsFactors = FALSE, fileEncoding = "latin1")

asean_list <- c("Philippines",
                "Indonesia",
                "Malaysia",
                "Singapore",
                "Thailand",
                "Brunei",
                "Vietnam",
                "Laos",
                "Myanmar",
                "Cambodia")

asean <- world_covid %>% 
  filter(continent == "Asia") %>% 
  arrange(total_cases) %>% 
  mutate(label_negara = case_when(country %in% asean_list ~ country,
                                  TRUE ~ ""))

# Scatter plot
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point() + 
  theme_minimal() 

# Point opacity
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(alpha = 0.5) +
  theme_minimal() 

# Bubble chart size
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), alpha = 0.5) +
  theme_minimal()

# Buble chart color & remove legend
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), alpha = 0.5, color = "coral") +
  theme_minimal() + 
  theme(legend.position = "none")

# Custom title & axis title
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), alpha = 0.5, color = "coral") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indonesia Amongst ASEAN",
       x = "Median Population Age",
       y = "Fatality Rate")

# Buble chart custom axis title
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), alpha = 0.5, color = "coral") +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "Indonesia Amongst ASEAN",
       x = "Median Population Age",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Buble chart add label
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), color = "coral", alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "Indonesia Amongst ASEAN",
       x = "Median Population Age",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = label_negara))

# Buble chart custom label position
ggplot(data = asean, mapping = aes(x = med_age, y = fatality_rate)) + 
  geom_point(aes(size = density_km2), alpha = 0.5, color = "coral") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(title = "Indonesia Amongst ASEAN",
       x = "Median Population Age",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.06)) +
  geom_text(aes(label = label_negara), vjust = -0.5)
