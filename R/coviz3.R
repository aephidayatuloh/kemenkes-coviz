# Indonesia Daily Cases Trend

dailynational <- read.csv("data-raw/dailynational.csv", header = TRUE, 
                          stringsAsFactors = FALSE)
dailynational$dates <- as.Date(dailynational$dates)
str(dailynational)

head(dailynational, n = 10)

dailynational %>% 
  select(dates, daily_cases, daily_recovered, daily_deaths) %>% 
  head(n = 10)

ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New")) + 
  geom_line(aes(y = daily_recovered, color = "Recovered")) + 
  geom_line(aes(y = daily_deaths, color = "Deaths")) +
  theme_minimal()

# Change color & line size
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) +
  theme_minimal() + 
  scale_color_manual(name = "Cases", # judul legend
                     values = c("New" = "yellow", 
                                "Recovered" = "green", 
                                "Deaths" = "red"))

# Add title and axis title
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) +
  theme_minimal() + 
  scale_color_manual(name = "Cases", # judul legend
                     values = c("New" = "yellow", 
                                "Recovered" = "green", 
                                "Deaths" = "red")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases")

# Change x-axis format
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) +
  theme_minimal() + 
  scale_color_manual(name = "Cases", # judul legend
                     values = c("New" = "yellow", 
                                "Recovered" = "green", 
                                "Deaths" = "red")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m")

# Add trend line
ks <- ksmooth(x = dailynational$dates, 
              y = dailynational$daily_cases, 
              kernel = "normal", 
              bandwidth = 10, 
              x.points = dailynational$dates)
dailynational$daily_trend <- ks$y

ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) +
  theme_minimal() + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", 
                                "Recovered" = "green", 
                                "Deaths" = "red", 
                                "Trend" = "grey")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") + 
  geom_line(aes(y = daily_trend, color = "Trend"), size = 0.8)

# Custom legend & position
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  theme_minimal() + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", 
                                "Recovered" = "green", 
                                "Deaths" = "red", 
                                "Trend" = "grey")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
  geom_line(aes(y = daily_trend, color = "Trend"), size = 0.8) + 
  theme(legend.position = "top")


