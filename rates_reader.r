library(ggplot2)
library(lubridate)
library(grid)
library(dplyr)
library(tidyr)
library(ggrepel)
library(stringr)
library(directlabels)

rates <- read.csv("https://www.banqueducanada.ca/valet/observations/group/FX_RATES_MONTHLY/csv",
                  header = TRUE, skip = 39)
rates_modif <- rates

names(rates_modif) <- str_replace(names(rates_modif), "FXM", "")
names(rates_modif) <- str_replace(names(rates_modif), "CAD", "")

rates_modif_2 <- rates_modif %>%
  gather(currency, month_rate, ZAR:VND, na.rm = TRUE) %>%
  group_by(currency)


currencies <- as.vector(unique(rates_modif_2$currency))
rates_modif_2$date <- as.Date(rates_modif_2$date)

# selection of currencies and last 12 months - to be used later, in Shiny, via input checkboxes
rates_selected_currency <- rates_modif_2 %>%
  filter(currency %in% c("USD", "EUR"))  %>%
  filter(date > as.Date(max(date)) - 365) %>%
  arrange(currency, date)


# average for each currency under selected period
avg_currency <- rates_selected_currency %>%
  group_by(currency) %>%
  summarize(avg = mean(month_rate))

# dollar US ~ CA
ggplot(data = rates_selected_currency,
       aes(x = date, y = month_rate, col = currency, group = currency)) +

  geom_line(data = rates_selected_currency,
            aes(x = date, y = month_rate)) +
  geom_smooth(method = "lm", se = FALSE, linetype = 3, alpha = 0.2) +
  geom_point()  +
  annotation_custom(grid.text(paste("Average Rate",
                                    avg_currency[1,]$currency, " = ",
                                    round(avg_currency[1,]$avg, 4), "CAD"),
                                    x=0.7,
                                    y=0.9,
                                    gp=gpar(col="BLACK", fontsize=10))) +
  annotation_custom(grid.text(paste("Average Rate",
                                    avg_currency[2,]$currency, " = ",
                                    round(avg_currency[2,]$avg, 4), "CAD"),
                                    x=0.7,
                                    y=0.05,
                                    gp=gpar(col="black", fontsize=10))) +
 labs(x = "Month", y = "$CA",
      title = "Monthly Average Exchange Rate of Foreign Currencies in CAD",
      caption = "Source: Bank of Canada", subtitle = today()) +
geom_dl(aes(label = currency), method = "smart.grid") +
  theme_minimal() +
  geom_label_repel(data = filter(rates_selected_currency, month_rate == min(month_rate) | month_rate == max(month_rate)), aes(label = month_rate,
                       fill = currency),
                   show.legend = FALSE, color = 'white',
                   size = 3.5)
