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
  gather(Currency, month_rate, ZAR:VND, na.rm = TRUE) %>%
  group_by(Currency)


currencies <- as.vector(unique(rates_modif_2$Currency))
rates_modif_2$date <- as.Date(rates_modif_2$date)

# selection of currencies and last 12 months - to be used later, in Shiny, via input checkboxes
rates_selected_Currency <- rates_modif_2 %>%
  filter(Currency %in% c("USD", "EUR"))  %>%
  filter(date > as.Date(max(date)) - 365) %>%
  arrange(Currency, date)


# average for each currency under selected period
avg_Currency <- rates_selected_Currency %>%
  group_by(currency) %>%
  summarize(avg = mean(month_rate))

# dollar US ~ CA
ggplot(data = rates_selected_Currency,
       aes(x = date, y = month_rate, col = Currency, group = Currency)) +

  geom_line(data = rates_selected_Currency,
            aes(x = date, y = month_rate)) +
  geom_smooth(method = "lm", se = FALSE, linetype = 3, alpha = 0.2) +
  geom_point()  +
   annotation_custom(grid.text(paste("Average Rates for Last 12 Months:", 
                                    "\nCA$1.00 = ", 
                                    round(avg_Currency[1,]$avg, 4), avg_Currency[1,]$Currency,
                                    "\nCA$1.00 = ", 
                                    round(avg_Currency[2,]$avg, 4), avg_Currency[2,]$Currency), 
                                    x=0.7,  
                                    y=0.95, 
                                    gp=gpar(col="BLACK", fontsize=10))) +
labs(x = "Month", y = "Foreign Currency", 
      title = "Monthly Average Exchange Rate of Foreign Currencies (CA$1.00 =)", 
      caption = "Source: Bank of Canada", subtitle = today()) +
  theme_minimal() +
  geom_label_repel(data = filter(rates_selected_Currency, month_rate == min(month_rate) | month_rate == max(month_rate)), aes(label = month_rate, 
                       fill = Currency), 
                   show.legend = FALSE, color = 'white',
                   size = 3.5) 
