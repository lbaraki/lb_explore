# PROJECT: Chart Types Code
# PURPOSE: Munge and Analysis of the MSD
# AUTHOR: Lemlem Baraki | SI
# REF ID:   4861e6cc
# LICENSE: MIT
# DATE: 2023-11-08
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
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    
    
  # SI specific paths/functions  
      load_secrets()
      merdata <- file.path(glamr::si_path("path_msd"))
      msd_path <- return_latest(merdata, "_PSNU_IM_FY21-24.*Zambia") #plug in any OU
      
      
  # Grab metadata
   msd_source <- source_info(msd_path)
   curr_pd <- source_info(msd_path, return = "period")
   pd <- source_info(msd_path, return = "period")
   fy <- source_info(msd_path, return = "fiscal_year")
   qtr <- source_info(msd_path, return = "quarter")  
  
  # REF ID for plots
    ref_id <- "4861e6cc"
    
  # Functions  
    # Limit labels - use this on the x-axis to create some space
    # returns a tick on every nth space given (2 - will skip 1 tick)
    every_nth = function(n) {
      return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
    }  
    

# LOAD DATA ============================================================================  

    #PSNU/district level data
    df_msd <- read_psd(msd_path) %>% 
      filter(funding_agency =="USAID")
    
    names(df_msd) #column names
    glimpse(df_msd) #preview column, column types, and values 
    
# MUNGE ============================================================================
  
  #Magnitude 
    #collapse data to SNU1 level and summarize TST_POS results
    df_bar <- df_msd %>% 
      filter(indicator %in% c("HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2022) %>% 
      summarise(HTS_TST_POS = sum(cumulative, na.rm = T), .by = "snu1")
    
  #Change over time
    df_time <- df_msd %>% 
      filter(
        indicator %in% c("HTS_TST_POS"),
        standardizeddisaggregate == "Total Numerator") %>% 
      summarise(across(c(contains("qtr")),
                       \(x) sum(x, na.rm = T)),
                .by = "fiscal_year") %>% 
      reshape_msd() %>% 
      arrange(period)
    
  #Ranking 
      #collapse data to sub-national level to show testing trends
    df_time_snu1 <- df_msd %>% 
      filter(indicator %in% c("HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator") %>% 
      summarise(across(c(contains("qtr")),
                       \(x) sum(x, na.rm = T)),
                .by = c("snu1","fiscal_year")) %>% 
      reshape_msd() %>% 
      filter(period == min(period) | period == max(period))
    
  #Part-to-whole
      #how much does index testing contribute to overall positives?
    df_index <- df_msd %>% 
      filter(indicator %in% c("HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             fiscal_year == min(fiscal_year)) %>% 
      group_by(indicator, modality) %>% 
      summarise(
        results = sum(cumulative, na.rm = T),
        .groups = "drop") %>% 
      mutate(share = results/sum(results),
             modality = fct_reorder(modality, share, .desc = T)) %>% 
      arrange(modality)
    
  #Small Multiples
    #stacked bar graph showing proportions (plot top 5)
    df_index_2 <- df_index%>% 
      slice_max(share, n = 5) %>% 
      mutate(benchmark = 1)
    
  #Distribution
    #create a population pyramid of test positive to show missing populations
    df_pyramid <- df_msd %>% 
      filter(
        indicator %in% c("HTS_TST_POS"),
        standardizeddisaggregate == "Modality/Age/Sex/Result",
        fiscal_year == 2022,
        ageasentered!= "Unknown Age") %>% 
      summarise(cumulative = sum(cumulative, na.rm = T),
                 .by = c(indicator, sex, ageasentered)) %>% 
      mutate(
        cumulative = ifelse(sex == "Female", -cumulative, cumulative),
        fill_color = ifelse(sex == "Female", moody_blue, genoa))
      
  
# VIZ ============================================================================

    #Magnitude
      #create a sorted bar graph
    df_bar %>% 
      mutate(snu1 = fct_reorder(snu1, HTS_TST_POS)) %>% 
      ggplot(aes(x = HTS_TST_POS, y = snu1))+ 
      geom_col(fill = glitr::old_rose, color = grey40k)+
      labs(title = "{snu1} leads in positive cases in {fiscal year}",
           caption = glue("{msd_source}"),
           x = NULL, y = NULL)+
      scale_x_continuous(labels = comma) + 
      si_style_xgrid()
    
    #Change over time
      #create time-series graph shaded below
    
    df_time %>%
      ggplot(aes(x = period, y = value, group = "a"))+
      geom_area(fill = grey20k, alpha = 0.75)+
      geom_line()+
      scale_x_discrete(breaks = every_nth(2)) +
      labs(title = "Positive tests have {trend} over time",
           caption = glue("{msd_source}")) + 
      scale_y_continuous(labels = comma) + 
      si_style()
      
    
    #Ranking 
      #create a slop graph
    df_time_snu1 %>% 
      mutate(value_label = case_when(
        period == max(period) ~ paste(comma(value),snu1),
        TRUE ~ comma(value))) %>% 
      ggplot(aes(x = period, y = value, group = snu1)) + 
      geom_line() + 
      geom_point() + 
      ggrepel::geom_text_repel(aes(label = value_label),
                               hjust = 1, force = 4) + 
      si_style_xline() + 
      theme(axis.text.y = element_blank()) + 
      labs(y = NULL, x = NULL,
           title = "Testing has {trend} in {snu1}",
           caption = glue("{msd_source}"))
    
    #Part-to-whole
      #need to get a proportion back to pass to waffle
    
    prop_fill <- df_index %>% 
      filter(modality == "Other PITC") %>% 
      transmute(fill_number = round(share * 100, 0)) %>% 
      pull() #gives you the # for modality of choice
    
    waffle_input <- c(100 - prop_fill, prop_fill) #gives you the 2 percentages needed
    
    waffle_input %>% 
      waffle::waffle(
        color = c(grey20k, scooter_med), #select colors of interest
        flip = T, #makes it run horizontal 
        reverse = T, #makes run along bottom
        size = 0.5) +
      geom_text(aes(x = 2.5, y = 2, #font location
                    label = percent(prop_fill/100)), #make percentage
                size = 36/.pt)+
      theme(legend.position = "none")+ #remove legend
    labs(title = str_to_upper(glue("{prop_fill}% of a all positive tests are \nfrom [modality] testing")),
           caption = glue("{msd_source}"))+
      si_style_void()#adjusts axis/grid lines 
    
    
    
    #Small Multiples
    ggplot(df_index_2, 
           (aes(x = indicator,y = share))) + 
             geom_col(aes(y = benchmark), fill = grey20k, alpha = 0.5)+
             geom_col(aes(fill = modality))+
      geom_hline(yintercept = 1, color = grey40k, linetype = "dotted")+
      geom_text(aes(label = percent(share, 1)),
                vjust = -0.25)+
      facet_wrap(~modality, ncol = 5) + 
      scale_fill_si(palette = "old_rose", discrete = T)+
      si_style_ygrid(facet_space = 0.5)+
      scale_y_continuous(labels = percent)+
      theme(axis.text.x = element_blank(),
            legend.position = "none")+
      labs(x = NULL, y = NULL, 
           title = str_to_upper("{modality} is an effective testing modality"),
           caption = "Source: Zambia MSD 2023")
    
    #Distribution
      #plot a pyramid to compare age/sex distributions of positives
    df_pyramid %>% 
      ggplot(aes(cumulative, ageasentered, 
                 group = "sex", fill = fill_color))+
      geom_col(alpha = 0.85)+
      geom_col(
        data = . %>% filter(ageasentered %in% c("15-19", "20-24","25-29"),
                            sex == "Male"),
        fill = "#004137")+
      geom_blank(aes(-cumulative))+
      geom_vline(xintercept = 0, color = "white")+
      scale_x_continuous(labels = abs)+
      scale_fill_identity() +
      labs(x = NULL, y = NULL,
        title = "{OU} is missing finding 15-29 yr olds <span style = 'color:#287c6f;'>MEN</span>",
        caption = glue("{msd_source}"))+
      si_style_xgrid() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            plot.title = element_markdown())

# SPINDOWN ============================================================================
