# Install package yang dibutuhkan
# install.packages(c("tidyverse", "scales"))

library(tidyverse)
library(scales)

# Import data world_covid.csv
world_covid <- read.csv(file = "data-raw/world_covid.csv", header = TRUE, 
                        stringsAsFactors = FALSE, fileEncoding = "latin1")

# Lihat struktur data
str(world_covid)

# Tampilkan n baris pertama data
head(world_covid, n = 10)

View(world_covid)

# Pilih variable yang akan digunakan
data1 <- world_covid %>% 
  select(country, fatality_rate)
data1

ggplot(data = data1, mapping = aes(x = country, y = fatality_rate)) + 
  geom_bar(stat = "identity")

# rotate x-axis label 90 deg
ggplot(data = data1, mapping = aes(x = country, y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90))

# sorted barchart
ggplot(data = data1, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90))

topn <- data1 %>% 
  arrange(desc(fatality_rate)) %>%
  head(n = 20)
topn

# sorted barchart topn
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90))

# Custom title
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate")

# Custom title - axis title
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Flip barchart
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip()

# Remove text angle 
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  # theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip()

# Custom theme 
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_minimal()

# Change color & opacity
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_flip() + 
  theme_minimal() 

# Change gridline
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_flip() + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

# Add label & y-limit 
ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) + 
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  geom_text(aes(label = paste0(fatality_rate*100, "%")), size = 3, hjust = -0.1)

ggplot(data = topn, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) + 
  labs(title = "Top Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.30)) +
  coord_flip() +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  geom_text(aes(label = paste0(fatality_rate*100, "%")), size = 3, hjust = -0.1)

