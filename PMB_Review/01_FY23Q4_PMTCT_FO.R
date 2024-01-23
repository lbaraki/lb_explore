# PROJECT: PMB FY24Q1 Data Review
# PURPOSE: Munge and Analysis of PMTCT_FO indicator
# AUTHOR: Lemlem Baraki | SI
# REF ID:   d5b30bf7
# LICENSE: MIT
# DATE: 2024-01-22
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)


# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,pattern = "OU_IM_FY21-24.")
hist_path <- return_latest(folderpath = merdata,pattern = "OU_IM_FY15-20.")

# Grab metadata
get_metadata(file_path)

# REF ID for plots
ref_id <- "d5b30bf7"

# Functions  


# LOAD DATA ============================================================================  

df_msd <- read_psd(file_path) %>% filter(indicator %in% c("PMTCT_FO"))
names(df_msd)

hist_msd <- read_psd(hist_path) %>% filter(indicator %in% c("PMTCT_FO"))

total_msd <- bind_rows(df_msd, hist_msd)
glimpse(total_msd)

# MUNGE ============================================================================

#numerator
  #unknown + died from otherdisaggregate 
num_pmtct <- total_msd %>%  
  filter(
    indicator %in% c("PMTCT_FO"),
    funding_agency == "USAID",
    standardizeddisaggregate %in% c(
      "Outcome"),
    otherdisaggregate %in% c(
      "HIV-final status unknown",
      "Other Outcomes: Died"),
    fiscal_year %in% c(2017:2023)) %>% 
  group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate, operatingunit) %>%  #include operatingunit
  summarise(across(c("cumulative"), sum, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(
    names_from = otherdisaggregate,
    names_glue = "{tolower(otherdisaggregate)}",
    values_from = cumulative
  ) %>%
  mutate_all(~replace_na(., 0)) %>% #replace NA's with zero
  rowwise() %>% 
  mutate(
    num_sum = `hiv-final status unknown` + `other outcomes: died`) %>% 
  filter(operatingunit %ni% c("Nigeria", "Tanzania"))

  #only "died" from otherdisaggregate 
num2_pmtct <- total_msd %>%  
  filter(
    indicator %in% c("PMTCT_FO"),
    funding_agency == "USAID",
    standardizeddisaggregate %in% c(
      "Outcome"),
    otherdisaggregate %in% c(
      #"HIV-final status unknown",
      "Other Outcomes: Died"),
    fiscal_year %in% c(2017:2023)) %>% 
  group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate, operatingunit) %>%  #include operatingunit
  summarise(across(c("cumulative"), sum, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(
    names_from = otherdisaggregate,
    names_glue = "{tolower(otherdisaggregate)}",
    values_from = cumulative
  ) %>%
  rowwise() %>% 
  mutate(
    num_sum = `other outcomes: died`) %>% 
  filter(operatingunit %ni% c("Nigeria", "Tanzania"))



#denominator
denom_pmtct <- total_msd %>%  
  filter(
    indicator %in% c("PMTCT_FO"),
    funding_agency == "USAID",
    standardizeddisaggregate %in% c(
      "Total Numerator"),
    fiscal_year %in% c(2017:2023)) %>% 
  group_by(fiscal_year, standardizeddisaggregate, operatingunit) %>% #include operating unit
  summarise(across(c("cumulative"), sum, na.rm = TRUE), 
            .groups = "drop") %>% 
  filter(operatingunit %ni% c("Nigeria", "Tanzania"))

#join 1 (Uknown + Died Otherdisaggs as Numerator)
merged_df <- left_join(num_pmtct, denom_pmtct %>% select(fiscal_year, operatingunit, cumulative),
                       by = c("fiscal_year", "operatingunit")) %>% 
  select(-standardizeddisaggregate)


#join 2 (Died Otherdisaggs as Numerator)
merged_df2 <- left_join(num2_pmtct, denom_pmtct %>% select(fiscal_year, operatingunit, cumulative),
                        by = c("fiscal_year", "operatingunit"))


#check
setequal(num_pmtct$operatingunit, denom_pmtct$operatingunit) #TRUE match
setequal(num2_pmtct$operatingunit, denom_pmtct$operatingunit) #FALSE
intersect(num_pmtct$operatingunit, denom_pmtct$operatingunit) #15 OU's similar
intersect(num2_pmtct$operatingunit, denom_pmtct$operatingunit) #14 OU's similar
setdiff(num_pmtct$operatingunit, denom_pmtct$operatingunit) #0 OU's diffent
setdiff(num2_pmtct$operatingunit, denom_pmtct$operatingunit)


# VIZ ============================================================================

#Viz 1 - PMTCT_FO across fiscal years (uknown + died vs numerator)
#include operatingunit for small multiples 
#x-axis = period/fiscal_year, y-axis = sum/cumulative

#bar charts 
viz0 <- merged_df %>% 
  ggplot(aes(x = fiscal_year))+
  geom_col(aes(y = cumulative), fill = "#287c6f", width = 0.75, alpha = 0.75)+ #Numerator
  geom_col(aes(y = num_sum), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 0.1))+ #Sum
  geom_text(aes(y = num_sum,
                label = percent(num_sum/cumulative, 1)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            position = position_nudge(x = 0.1),
            vjust = -0.5)+
  si_style_ygrid() + 
  facet_wrap(~operatingunit, scales = "free_y")+
  scale_y_continuous(labels = comma, expand = c(0,0))+
  coord_cartesian(expand = T) + 
  theme(plot.subtitle = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       subtitle = glue::glue("<span style = 'color:#7ecfc0'>FO Status: Uknown and/or Died</span> | 
                          <span style = 'color:#287c6f'>Total Numerator</span>"),
       caption = "Source: FY23Q4c MSD |
           Tanzania and Nigeria excluded due to FY23 data withheld from MSD in Pano",
       title = "USAID PMTCT_FO TRENDS FY17 - FY23")

si_save("Images/PMTCT_FO_V1_barchart_trends.png")

#line graph
viz1 <- merged_df %>% 
  mutate(percent_fo = num_sum/cumulative) %>% 
  ggplot(aes(x = fiscal_year, 
             y = percent_fo,
             group = operatingunit
  )) +
  geom_line(color= "#287c6f",linewidth = 0.5) +
  geom_point(aes(y = percent_fo), color = "#287c6f", size = 2.5) +
  si_style_ygrid()+
  geom_text(aes(label = percent(percent_fo, 1)),
            size = 9/.pt,
            vjust = -0.25,
            hjust = -.25
  ) + 
  facet_wrap(~operatingunit,
             scales = "free_y") +
  scale_y_continuous(labels = scales::percent, expand = c (0, 0.15)) +
  scale_x_continuous(expand = c(0,0.5),
                     breaks = c(2017, 2019, 2021, 2023),  
                     label = c("FY17", "FY19", "FY21", "FY23")) +
  #coord_cartesian(expand = TRUE) +
  theme(plot.subtitle = ggtext::element_markdown(),
        plot.margin = )+
  labs(x = NULL, y = NULL, 
       subtitle = glue::glue("<span style = 'color:#287c6f'>Percent Infants with reported Final Outcome who have an Unknown Status and/or Died </span>"),
       caption = "Source: FY23Q4c MSD |
           Tanzania and Nigeria excluded due to FY23 data withheld from MSD in Pano",
       title = "USAID PMTCT_FO TRENDS FY17 - FY23")

si_save("Images/PMTCT_FO_V1_linechart_trends.png")


#Viz 2- PMTCT_FO across fiscal years (died vs numerator)
#include operatingunit for small multiples

#bar charts
viz00 <- merged_df2 %>% 
  ggplot(aes(x = fiscal_year))+
  geom_col(aes(y = cumulative), fill = "#287c6f", width = 0.75, alpha = 0.75)+ #Numerator
  geom_col(aes(y = num_sum), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 0.1))+ #Sum
  geom_text(aes(y = num_sum,
                label = percent(num_sum/cumulative, 1)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            position = position_nudge(x = 0.1),
            vjust = -0.5)+
  si_style_ygrid() + 
  facet_wrap(~operatingunit, scales = "free_y")+
  scale_y_continuous(labels = comma, expand = c(0,0))+
  coord_cartesian(expand = T) + 
  theme(plot.subtitle = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       subtitle = glue::glue("<span style = 'color:#7ecfc0'>FO Status: Died</span> | 
                          <span style = 'color:#287c6f'>Total Numerator</span>"),
       caption = "Source: FY23Q4c MSD |
           Tanzania and Nigeria excluded due to FY23 data withheld from from MSD in Pano",
       title = "USAID PMTCT_FO TRENDS FY17 - FY23")

si_save("Images/PMTCT_FO_V2_barchart_trends.png")


viz01 <- merged_df2 %>% 
  mutate(percent_fo = num_sum/cumulative) %>% 
  ggplot(aes(x = fiscal_year, 
             y = percent_fo,
             group = operatingunit)) +
  geom_line(color= "#287c6f", linewidth = 0.5) +
  geom_point(aes(y = percent_fo), color = "#287c6f", size = 2.5) +
  si_style_ygrid()+
  geom_text(aes(label = percent(percent_fo, 1)),
            size = 9/.pt,
            vjust = -0.25,
            hjust = -.25) + 
  facet_wrap(~operatingunit, scales = "free_y") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0.1)) +
  scale_x_continuous(expand = c(0,0.5),
                     breaks = c(2017, 2019, 2021, 2023),  
                     label = c("FY17", "FY19", "FY21", "FY23")) +
  #coord_cartesian(expand = TRUE) +
  theme(plot.subtitle = ggtext::element_markdown())+
  labs(x = NULL, y = NULL, 
       subtitle = glue::glue("<span style = 'color:#287c6f'>Percent Infants with reported Final Outcome who have Died </span>"),
       caption = "Source: FY23Q4c MSD |
           Tanzania and Nigeria excluded due to FY23 data withheld from from MSD in Pano",
       title = "USAID PMTCT_FO TRENDS FY17 - FY23")

si_save("Images/PMTCT_FO_V2_linechart_trends.png")

# SPINDOWN ============================================================================


