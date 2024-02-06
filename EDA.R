library(tidyverse)
library(lubridate)

coeffs = data.frame(
  Event = c("200m", "800m", "60mH", "100mH", "HJ", "LJ", "SP", "Jav"),
  A = c(4.99087, 0.11193, 20.0479, 9.23076, 1.84523, 0.188807, 56.0211, 15.9803),
  B = c(42.5, 254, 17, 26.7, 75, 210, 1.5, 3.8),
  C = c(1.81, 1.88, 1.835, 1.835, 1.348, 1.41, 1.05, 1.04)
)

fieldEvents = c("HJ", "LJ", "SP", "Jav")
runningEvents = c("200m", "800m", "60mH", "100mH")
combinedEvents = c("Hep", "Pent")

marks = readODS::read_ods("Multi Event Progression by Age.ods", sheet = "Data") %>%
  left_join(coeffs, by = "Event") %>%
  mutate(Performance = ifelse(Event == "800m", period_to_seconds(ms(Performance)), Performance)) %>%
  mutate(Performance = as.numeric(Performance)) %>%
  mutate(Score = ifelse(Event %in% c("200m", "800m", "60mH", "100mH"), A*(B-Performance)**C,
         ifelse(Event %in% c("HJ", "LJ"), A*(Performance*100 - B)**C, 
         A*(Performance-B)**C))) %>%
  mutate(Score = round(Score, 0)) %>%
  mutate(Score = ifelse(is.na(Score), Performance, Score))


marks %>%
  filter(Event %in% fieldEvents) %>%
  ggplot(aes(x = Age, y = Score, group = Name, shape = Name, color = Name)) +
  geom_point() +
  geom_line() +
  ggtitle("Field Events") +
  facet_wrap(~Event, scales = "free_y", ncol = 2) +
  theme_bw()

marks %>%
  filter(Event %in% runningEvents) %>%
  ggplot(aes(x = Age, y = Score, group = Name, shape = Name, color = Name)) +
  geom_point() +
  geom_line() +
  ggtitle("Running Events") +
  facet_wrap(~Event, scales = "free_y", ncol = 2) +
  theme_bw()

marks %>%
  filter(Event %in% combinedEvents) %>%
  ggplot(aes(x = Age, y = Score, group = Name, shape = Name, color = Name)) +
  geom_point() +
  geom_line() +
  ggtitle("Combined Events") +
  facet_wrap(~Event, scales = "free_y", ncol = 2) +
  theme_bw()
