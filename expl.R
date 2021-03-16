#git push --set-upstream origin master

# data from https://www.kaggle.com/fedesoriano/air-quality-data-set

library(pacman)
pacman::p_load(bookdown, tidyverse, janitor, here, readxl, lubridate, hms, patchwork, GGally,
               ggthemes, qdapRegex, inspectdf, R.utils, RColorBrewer, tidytext, ggrepel, tibbletime,
               viridis, DT, plotly, gapminder, echarts4r, cowplot, magick, tidymodels, skimr)


EN <- read_excel(here("./DATA/AirQualityUCI.xlsx"), .name_repair = make_clean_names)

#skim(EN)

EN_Clean =
  EN %>% 
  dplyr::mutate(date_time = as.POSIXct(paste(date, format(time, format = "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S")) %>%
  # dplyr::mutate(year = format(date, "%Y"),
  #               month = format(date, "%m"),
  #               day = format(date, "%d"),
  #               time = format(time, format = "%H:%M:%S")) %>% 
  relocate(date_time)

#skim(EN_Clean)

EN_long =
  EN_Clean %>%
  select(-date,-time) %>% 
  pivot_longer(
    cols = co_gt:ah,
    names_to = c("measurements"),
    values_to = "values") %>% 
  filter(values > 0)

EN_long_tt = as_tbl_time(EN_long,index = date_time) 


EN_long_tt %>% 
  collapse_by("monthly") %>%
  group_by(date_time,measurements) %>% 
  summarise(mean_val = values) %>% 
  ggplot(aes(x=date_time, y=mean_val, z=factor(measurements), color=measurements)) +
  geom_line() + 
  xlab("") +
  facet_wrap(.~measurements, scales = "free_y")
  
mean_500 <- rollify(mean, window = 500)
EN_roll =
  EN_long_tt %>% 
  group_by(measurements) %>% 
  mutate(roll_mean = mean_500(values))


ggplot(EN_roll, aes(x=date_time, y=roll_mean, z=factor(measurements), color=measurements)) +
  geom_line() + 
  xlab("") +
  facet_wrap(.~measurements, scales = "free_y")


colnames(EN)

# Most basic bubble plot
ggplot(EN_Clean, aes(x=date, y=pt08_s2_nmhc)) +
  geom_line( color="steelblue") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))# +
  ylim(0,15)
  



ggplot(EN_long, aes(x=date, y=values, z=factor(measurements), color=measurements)) +
  geom_line() + 
  xlab("") +
  facet_wrap(.~measurements, scales = "free_y")
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylim(0,15)
  
  
CTM_Clean =
  CTM %>% 
  mutate_at(vars(analysis_time), dmy_hms) %>% 
  mutate_at(vars(analysis_time), funs("date" = date(.))) %>% 
  relocate(date) %>% 
  #rename(grassi = "grassi_tq", proteine = "proteine_tq" ) %>%
  select(-analysis_time, -sample_comment, -cup_id, -cup_type, -product_name,
         -instrument_serial_number, -instrument_name, -product_code, -sample_type,
         -saccarosio, -ceneri, -fibra, -carboidrati, -proteine) %>%
  # mutate(sample_number = tolower(sample_number),
  #       varieta = case_when(str_detect(sample_number,".*robusta") ~ "robusta", 
  #                           str_detect(sample_number,".*arabica*|.*arabuca*") ~ "arabica",
  #                           TRUE ~ "NA"),
  #sample_number = str_remove(sample_number, "robusta|arabica")) %>% 
  mutate(n_acc = paste(parse_number(sample_number), str_sub(year(date), -2), sep = "/")) %>%
  # mutate(origine = str_replace(sample_number, "_", " "),
  #        origine = rm_nchar_words(origine, "1,3", trim = T, clean = TRUE, extract=F),
  #        origine = rm_non_words(origine),
  #        origine = na_if(origine, ""), 
  #        #origine = (coalesce(origine, sample_number)),
  #        origine = case_when(str_detect(origine,".*camer") ~ "camerun", 
  #                            TRUE ~ origine)) %>%  
  #dplyr::filter(sample_number !="test") %>% # remove TEST
  #dplyr::filter(!str_detect(sample_number, "DECAFFEINATO" )) %>% # remove DECAFFEINATO
  #dplyr::filter(date > "2019-12-31") %>% # filter 2020 samples
  drop_na("umidita", "grassi")


mergetest =
  left_join(GBDB, IDENTIF, by = c("sample_number" = "n_acc")) %>% 
  group_by(varieta, caffeina) %>% 
  mutate(varieta = case_when(varieta == "NA" & caffeina<=1 ~ "arabica",
                             varieta == "NA" & caffeina>1 ~ "robusta", TRUE ~ varieta)) %>% 
  ungroup()



CTM_long =
  CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y"),
                #month = format(date, "%m"),
                day = format(date, "%d")) %>% # get year from date
  group_by(date, factor(day)) %>% 
  filter(year == "2020") %>% 
  pivot_longer(
    cols = umidita:caffeina,
    names_to = c("measurements"),
    values_to = "values")





CTM_long %>% 
  #filter(varieta %in% "arabica") %>% 
  group_by(sample_number, measurements) %>%
  summarise(sd = sd(values, na.rm = TRUE), len = mean(values)) %>%   
  filter(!str_detect(measurements,"fibra|ceneri")) %>% 
  ungroup() %>% 
  ggplot(aes(measurements, len, fill = measurements)) +
  geom_bar(stat = "Identity", position = "dodge", width = 0.9) +
  geom_linerange(aes(ymin = len-sd, ymax = len+sd), color="grey20") +  
  labs(fill="Misurazioni") +
  scale_y_continuous(breaks = seq(from = 0, to = 12.5, by = 2.5)) +
  coord_flip() +
  theme_wsj(title_family = "", base_family="", base_size = 8) +
  #scale_fill_brewer(palette = "Set2", direction=-1) +
  scale_fill_viridis(discrete = T,option = "B", begin = 0.3, end = 0.8, direction = 1) +
  theme(legend.position = "none",
        strip.text.x = element_text(
          size = 12, face = "bold")) +
  facet_wrap(.~sample_number, scales = "free") 





CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y"),
                #month = format(date, "%m"),
                day = format(date, "%d")) %>% # get year from date
  #filter(year != "2016") %>% 
  select(-date, -day, -n_acc, -sample_number) %>% 
  mutate(is_outlier = case_when((abs(caffeina - median(caffeina)) > 2*sd(caffeina)) ~ "OUT", TRUE ~ "OK")) %>% 
  #filter(!(abs(caffeina - median(caffeina)) > 2*sd(caffeina))) %>%
  #ggpairs(aes(size=caffeina, colour=is_outlier), cardinality_threshold = 70) + 
  ggpairs(aes(size=caffeina, colour=year), cardinality_threshold = 70) + 
  scale_fill_viridis(discrete = T,option = "B", begin = 0.1, end = 0.8, direction = 1, alpha=0.6) +
  scale_color_viridis(discrete = T,option = "B", begin = 0.1, end = 0.8, direction = 1, alpha=0.6)



CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y"),
                #month = format(date, "%m"),
                day = format(date, "%d")) %>% # get year from date
  group_by(year, caffeina) %>% 
  arrange(desc(umidita))

CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y"),
                month = format(date, "%m"),
                day = format(date, "%d")) %>% # get year from date
  mutate(is_outlier = case_when((abs(caffeina - median(caffeina)) > 1.6*sd(caffeina)) ~ "OUT", TRUE ~ "OK")) %>% 
  group_by(year, caffeina, is_outlier) %>% 
  filter(is_outlier == "OUT") %>% 
  arrange(desc(caffeina)) 

%>% 
  view()


library(ggstatsplot)

CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y"),
                #month = format(date, "%m"),
                day = format(date, "%d")) %>% # get year from date
  #filter(year != "2016") %>% 
  select(-date, -day, -n_acc) %>% 
  ggstatsplot::ggbetweenstats(
    x = year,
    y = caffeina,
    mean.ci = TRUE, # whether to display confidence interval for means
    k = 3, # number of decimal places for statistical results
    outlier.tagging = TRUE, # whether outliers need to be tagged
    outlier.label = sample_number, # variable to be used for tagging outliers
    xlab = "Anno analisi", # label for the x-axis variable
    ylab = "RH%", # label for the y-axis variable
    title = "Caffeina in tostato macinato vs. anno di saggio", # title text for the plot
    ggtheme = ggthemes::theme_fivethirtyeight(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    #package = "wesanderson", # package from which color palette is to be taken
    #palette = "Darjeeling1" # choosing a different color palette
  )

set.seed(234)

mergetest %>% 
  filter(date > "2019-12-31") %>% 
  add_count(origine) %>%
  filter(n > 2) %>% 
  select(origine, caffeina, sample_number, varieta, umidita, grassi, proteine) %>% 
  drop_na() %>% 
  ggstatsplot::ggbetweenstats(
    x = origine,
    y = umidita,
    mean.ci = F, # whether to display confidence interval for means
    k = 1, # number of decimal places for statistical results
    outlier.tagging = TRUE, # whether outliers need to be tagged
    outlier.label = sample_number, # variable to be used for tagging outliers
    xlab = "Anno analisi", # label for the x-axis variable
    ylab = "RH%", # label for the y-axis variable
    title = "Caffeina in tostato macinato vs. anno di saggio", # title text for the plot
    ggtheme = ggthemes::theme_fivethirtyeight(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    #package = "wesanderson", # package from which color palette is to be taken
    #palette = "Darjeeling1" # choosing a different color palette
  )



### PCA

CTM_PCA_tidy =
  CTM_Clean %>% 
  dplyr::mutate(year = format(date, "%Y")) %>% # get year from date
  mutate(year = as.character(year)) %>% 
  select(-date) %>% 
  # mutate(origine = tolower(coalesce(origine, sample_number))) %>%  #works
  na.omit()

%>% 
  type.convert()

#merge_PCA_tidy %>% select(!where(is.numeric))


CTM_pca_rec = 
  recipe(~., CTM_PCA_tidy) %>% 
  update_role(all_nominal(), new_role="id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), id = "pca") %>% 
  prep()


CTM_tidied_pca = tidy(CTM_pca_rec, id = "pca")


CTM_plot_exp_var =
  CTM_pca_rec %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(aes(fill=(component)), show.legend = F) + 
  labs(y="% variabilità spiegata", x="componenti principali", fill="PC") +
  scale_fill_viridis(option="D", begin = 0.25, end=0.85, direction =-1) +
  xlim(0.5, 3.5) + ylim(0, 80) +
  # theme_wsj(title_family = "", base_family="", base_size = 8) + 
  theme_minimal() + 
  theme(axis.title=element_text(size=12))
#coord_flip() 
#plot_exp_var

CTM_plot_PC_loadings =
  CTM_tidied_pca %>% 
  filter(component %in% paste0("PC", 1:3)) %>% 
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = F) + labs(y=NULL, x=NULL) +
  scale_fill_viridis(discrete=T,option="B",begin = 0.25, end=0.85) +
  facet_wrap(~component, nrow = 3) +
  labs(y="", x="correlazione tra variabili e PC") +
  theme_wsj(title_family = "", base_family="", base_size = 8) +
  theme(axis.title=element_text(size=12)) 

expl_var_plots = (CTM_plot_exp_var|CTM_plot_PC_loadings)

expl_var_plots +
  plot_annotation(tag_levels = 'A')



# define arrow style for plotting
arrow_style <- arrow(angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))

# get pca loadings into wider format
CTM_pca_wider <- CTM_tidied_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)

set.seed(234)

CTM_pca_plot =
  juice(CTM_pca_rec) %>% 
  ggplot(aes(PC1, PC3, label = sample_number)) +
  geom_point(aes(color = year), alpha = .7, size = 10) +
  #geom_label_repel(aes(fill=n_acc), force=0.2, size=4, show.legend = FALSE, segment.alpha = .5) +
  geom_text(check_overlap = T) +
  scale_color_viridis(discrete=T,option="B", begin = 0.25, end=0.95) +
  #scale_fill_viridis(discrete=T,option="B", begin = 0.25, end=0.95, alpha=0.7, direction=-1) +
  labs(color=NULL) +
  #  theme_dark() +
  theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))
CTM_pca_plot

CTM_p2= 
  ggplot(CTM_pca_wider, aes(PC1, PC3)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(aes(label=terms), force=0.1, size=4, 
                  show.legend = FALSE, direction="y", segment.alpha = 0.6) +
  xlim(-1.5, .9) + ylim(-1, 1) +  
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal() + 
  background_grid()

patchwork <- CTM_pca_plot|(CTM_plot_exp_var/CTM_p2) #& theme(legend.position = "none")
#patchwork

patchwork +
  plot_layout(widths = c(3, 1), heights = (c(1, 2))) + #guides = "collect"
  plot_annotation(tag_levels = 'A')




PCA3D =
  juice(CTM_pca_rec)

plot_ly(PCA3D, x = ~PC1, y = ~PC2, z = ~PC3, 
        color = ~year, alpha=0.7,
        colors = viridis_pal(option = "B", begin=.25, end=.95)(6),
        marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(10, 25),
        type   = 'scatter3d', 
        mode   = 'markers', scene='scene1') %>%
  layout(title = "Tostato macinato")



