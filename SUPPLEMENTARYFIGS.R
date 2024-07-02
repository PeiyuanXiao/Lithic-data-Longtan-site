library("readxl")
library(tidyverse)
library(ggpmisc)
library(forcats)
library(patchwork)

# Cores-------------------------------------------------------------------------------
 
cores <- 
  read_excel("Longtan site lithic data-Cores.xlsx", skip = 1) %>%
  filter(...1 %in% c('Quina core', 'Discoid', 'C-O-F', 'Surface core')) %>%
  rename("Coretype" = ...1,
         "Length (mm)" = `长（高）（mm）`,
         "Width (mm)" = `宽（mm）`,
         "Thickness (mm)" = `厚（mm）`,
         "Weight (g)" = `重量`,
         "Platform angle" = `平均台面角...41`) 
  
cores_clean <-   
  cores %>%
  select(
    Coretype,
    `Length (mm)` ,
         `Width (mm)`,
         `Thickness (mm)`,
         `Weight (g)`,
  ) %>%
  mutate(Coretype = case_when(
    Coretype ==  "Quina core" ~ "Quina cores", 
    Coretype ==  "Surface core" ~ "Surface cores",   
    Coretype ==  "Discoid" ~ "Discoidal cores",
    Coretype ==  "C-O-F" ~ "C-O-Fs",
  )) %>%
  drop_na() %>%
  pivot_longer(-Coretype) %>%
  mutate(Coretype = factor(Coretype, 
                           levels = c("Quina cores", 
                                      "Discoidal cores", 
                                      "C-O-Fs", 
                                      "Surface cores"))) %>%
  mutate(name = factor(name, levels = c("Length (mm)", 
                                        "Width (mm)", 
                                        "Thickness (mm)", 
                                        "Weight (g)")))

# Overall size of cores  
  
ggplot(cores_clean, 
      aes(x = Coretype, y = value, fill = Coretype)) +
  geom_violin(
    fill = "lightgrey",
    alpha = 1,
    linewidth = 0,
    color = "white",
    adjust = 2
  ) +
  stat_boxplot(geom = "errorbar", width = 0.1, linewidth = 0.5) +
  geom_boxplot(
    aes(fill = Coretype),
    alpha = 1,
    linewidth = 0.5,
    color = "black",
    width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  scale_color_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  facet_wrap(~ name, scales = "free_y") +   
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ylab("")

ggsave(filename = "Core_size.png", width = 8, height = 6, dpi = 800, bg = "white")

# Platform angle of cores  

cores_angle <- cores %>%
  mutate(Coretype = case_when(
    Coretype == "Quina core" ~ "Quina cores",               
    Coretype == "Surface core" ~ "Surface cores",   
    Coretype == "Discoid" ~ "Discoidal cores",
    Coretype == "C-O-F" ~ "C-O-Fs"
  ) ) %>%
  mutate(Coretype = factor(Coretype, levels = c("Quina cores",       
                                                "Discoidal cores",
                                                "C-O-Fs",
                                                "Surface cores")))

ggplot(cores_angle, aes(x = Coretype, y = `Platform angle`, fill = Coretype)) +
  geom_violin(
    fill = "lightgrey",
    alpha = 1,
    linewidth = 0,
    color = "white",
    adjust = 2
  ) +
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(
    aes(fill = Coretype),
    alpha = 1,
    linewidth = 0.5,
    color = "black",
    width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  scale_color_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(filename = "Core_PA.png", width = 8, height = 6, dpi = 800, bg = "white")

# Flakes------------------------------------------------------------------------

flakes <- read_excel("Longtan site lithic data-Flakes.xlsx", skip = 0)
resharpening <- read_excel("Longtan lithic data-Reaffutage.xlsx", skip = 1)

resharpening_clean <- 
  resharpening %>%
  select(`Length (mm)` = ...2,
         `Width (mm)` = `...3`,
         `Thickness (mm)` = `...4`,
         `Weight (g)` = `...5`,
         `Interior platform angle` = `IPA`,
         `Platform length (mm)` = `Width`,
         `Platform width (mm)` = `Depth`) %>%
  mutate(Typology = "Resharpening flakes") %>%
  drop_na()

flakes_clean <- 
  flakes %>%
  select(typology = ...1,
         `Length (mm)` = `长（mm）`,
         `Width (mm)` = `宽（mm）`,
         `Thickness (mm)` = `厚（mm）`,
         `Weight (g)` = `重量（g）`,
         `Interior platform angle` = `IPA`,
         `Platform length (mm)` = `台面长（mm)`,
         `Platform width (mm)` = `台面厚（mm）`) %>%
  mutate(Typology = case_when(
    typology ==  "Kombewa" ~ "Kombewa flakes", 
    typology ==  "Surface" ~ "Surface flakes",   
    typology ==  "Discoid" ~ "Discoidal flakes",
    typology ==  "Quina flake" ~ "Quina flakes",
  )) %>%
  drop_na() 

all_flakes_clean <- 
  bind_rows(resharpening_clean,
            flakes_clean)

all_flakes_clean_long <- 
  all_flakes_clean %>%
  select(c("Typology",
           "Length (mm)", 
           "Width (mm)", 
           "Thickness (mm)", 
           "Weight (g)", 
           "Interior platform angle", 
           "Platform length (mm)", 
           "Platform width (mm)")) %>%
  pivot_longer(-Typology) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina flakes", 
                                      "Discoidal flakes", 
                                      "Kombewa flakes", 
                                      "Surface flakes", 
                                      "Resharpening flakes"))) 

# Overall size in flakes with statistics test   

ggplot(all_flakes_clean_long %>%
           filter(name != "Interior platform angle")) +
  aes(Typology, value) +
  geom_violin(fill = "lightgrey", 
              alpha = 1,
              linewidth = 0, 
              color = "white", 
              adjust = 2) + 
  stat_boxplot(geom = "errorbar", 
               width = 0.1, 
               size = 0.5) +
  geom_boxplot(aes(fill = Typology), 
               alpha = 1, 
               linewidth = 0.5, 
               color = "black", 
               width = 0.4) + 
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2,
             color = "black", 
             show.legend = FALSE) +
  facet_wrap( ~ name, scales = "free_y") +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                "Surface flakes" = "#CDEBB3", 
                                "Quina flakes" = "#21318C", 
                                "Discoidal flakes" = "#1E80B8",
                                "Resharpening flakes" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                 "Surface flakes" = "#CDEBB3", 
                                 "Quina flakes" = "#21318C", 
                                 "Discoidal flakes" = "#1E80B8",
                                 "Resharpening flakes" = "#EEF8B4")) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  stat_multcomp(geom = "text_pairwise",
                size = 2, 
                small.p = F,
                contrasts = "Dunnet") +
  labs(fill = "Typology") +
  theme(legend.position = "none", axis.title.x = element_blank(),axis.text.x = element_text(size = 8))

ggsave(filename = "Flake_size.png", width = 13, height = 9, dpi = 800, bg = "white")

# IPA in flakes with statistics test 
  
ggplot(all_flakes_clean_long %>%
           filter(name == "Interior platform angle")) +
    aes(Typology, value) +
    geom_violin(fill = "lightgrey", 
                alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
    stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
    geom_boxplot(aes(fill = Typology), 
                 alpha = 1, 
                 linewidth = 0.5, 
                 color = "black", 
                 width = 0.4) + 
    geom_point(stat = "summary", 
               fun = "mean", 
               shape = 19, 
               size = 2, 
               color = "black", 
               show.legend = FALSE) +
    ylab("Interior platform angle") +
    xlab("") +
    scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                  "Surface flakes" = "#CDEBB3", 
                                  "Quina flakes" = "#21318C", 
                                  "Discoidal flakes" = "#1E80B8",
                                  "Resharpening flakes" = "#EEF8B4" )) +
    scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                   "Surface flakes" = "#CDEBB3", 
                                   "Quina flakes" = "#21318C", 
                                   "Discoidal flakes" = "#1E80B8",
                                   "Resharpening flakes" = "#EEF8B4")) +
    theme_classic() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    stat_multcomp(geom = "text_pairwise",
                  size = 2, 
                  small.p = F,
                  contrasts = rbind(c(0, 0, 0, -1, 1),
                                    c(0, 0, -1, 1, 0),
                                    c(0, -1, 0, 1, 0),
                                    c(-1, 0, 0, 1, 0))) +
    labs(fill = "Typology") +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(filename = "Flake_IPA.png", width = 8, height = 6, dpi = 800, bg = "white")
  
# Staked chart for platform types in flakes  
  
flake_platform_type <- 
  bind_rows(.id = "id",
    flakes = flakes %>%
      select(`Platform type` = `台面类型`,
             `Flake type` = `...1`),
     resharpening = resharpening %>% 
      select(`Platform type` = `Type`) %>%
      mutate(`Flake type` = "Resharpening")
  ) %>%
  mutate(`Flake type` = case_when(
    `Flake type` ==  "Kombewa" ~ "Kombewa flakes", 
    `Flake type` ==  "Surface" ~ "Surface flakes",   
    `Flake type` ==  "Discoid" ~ "Discoidal flakes",
    `Flake type` ==  "Quina flake" ~ "Quina flakes",
    `Flake type` ==  "Resharpening" ~ "Resharpening flakes",
  ) ) %>%
  mutate(`Platform type`= case_when(
    `Platform type` == "素"~ "Plain",
    `Platform type` == "自然"~ "Natural",
    `Platform type` == "线"~ "Linear",
    `Platform type` == "单棱脊"~ "Dihedral",
    `Platform type` == "双棱脊"~ "Faceted",
    .default = `Platform type`
  ) ) %>% 
  drop_na(`Platform type`)

ppt <- ggplot(data = flake_platform_type %>%
         mutate(`Flake type` = fct_infreq(`Flake type`),
                `Platform type` = factor(`Platform type`, 
                                         levels = c("Plain", 
                                                    "Dihedral", 
                                                    "Faceted", 
                                                    "Linear", 
                                                    "Natural"))) %>%
         group_by(`Platform type`) %>%
         count(`Flake type`), 
       aes(`Flake type`,
           n,
           group = `Platform type`,
           fill = `Platform type`)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Faceted" = "#30A5C2",
      "Dihedral" = "#1E80B8",
      "Natural" = "#EEF8B4",
      "Linear" = "#CDEBB3",
      "Plain" = "#21318C")) +
  theme_classic() +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Platform types")) +
  xlab("") + ylab("Count") 

# Staked chart for dorsal scar pattern in flakes

flake_dsp_type <- 
  bind_rows(.id = "id",
            flakes = flakes %>%
              select(`Dorsal scar pattern` = `片疤方向`,
                     `Flake type` = `...1`),
            resharpening = resharpening %>% 
              select(`Dorsal scar pattern` = `...15`) %>%
              mutate(`Flake type` = "Resharpening")
  ) %>%
  mutate(`Flake type` = case_when(
    `Flake type` ==  "Kombewa" ~ "Kombewa flakes", 
    `Flake type` ==  "Surface" ~ "Surface flakes",   
    `Flake type` ==  "Discoid" ~ "Discoidal flakes",
    `Flake type` ==  "Quina flake" ~ "Quina flakes",
    `Flake type` ==  "Resharpening" ~ "Resharpening flakes",
  ) ) %>%
  mutate(`Dorsal scar pattern`= case_when(
    `Dorsal scar pattern` == "从近端到远端"~ "Proximal only",
    `Dorsal scar pattern` == "从远端到近端"~ "Distal only",
    `Dorsal scar pattern` == "向心剥片"~ "Centripetal",
    `Dorsal scar pattern` == "向心"~ "Centripetal",
    `Dorsal scar pattern` == "多向"~ "Multi direction",
    `Dorsal scar pattern` == "从左侧到右侧"~ "Lateral",
    `Dorsal scar pattern` == "从右侧到左侧"~ "Lateral",
    `Dorsal scar pattern` == "无法识别"~ "Unidentified",
    `Dorsal scar pattern` == "NA"~ "Unidentified",
    .default = `Dorsal scar pattern`
  ) ) %>% 
  drop_na(`Dorsal scar pattern`)

pdsp <- ggplot(data = flake_dsp_type %>%
         mutate(`Flake type` = fct_infreq(`Flake type`),
                `Dorsal scar pattern` = factor(`Dorsal scar pattern`, 
                                         levels = c("Proximal only", 
                                                    "Distal only", 
                                                    "Lateral", 
                                                    "Multi direction", 
                                                    "Centripetal",
                                                    "Unidentified"))) %>%
         group_by(`Dorsal scar pattern`) %>%
         count(`Flake type`), 
       aes(`Flake type`,
           n,
           group = `Dorsal scar pattern`,
           fill = `Dorsal scar pattern`)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Multi direction" = "#CDEBB3",
      "Distal only" = "#1E80B8",
      "Lateral" = "#30A5C2",
      "Unidentified" = "#FFD082",
      "Centripetal" = "#EEF8B4",
      "Proximal only" = "#21318C")) +
  theme_classic() +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Dorsal scar pattern")) +
  xlab("") + ylab("Count") 

ppt / pdsp

ggsave(filename = "Stacked_chart_flakes.png", width = 8, height = 6, dpi = 800, bg = "white")

# Tools-------------------------------------------------------------------------

qn_scrapers <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1)
scraper <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Scrapers")
notch <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Notches")
denticulate <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Denticulates")
Misc <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 0, sheet = "Miscellaneous tools")

 
 Tools_clean <- 
   bind_rows(
    .id = "id",
    list(
      `Quina scraper` =
        qn_scrapers %>%
        select(Length, Breadth, Thickness, Weight),
      Scraper =
        scraper %>%
        select(Length, Breadth, Thickness, Weight),
      Notch =
        notch %>%
        select(Length, Breadth, Thickness, Weight),
      Denticulate  =
        denticulate %>%
        select(Length, Breadth, Thickness, Weight),
      Misc = Misc %>%
        select(Length, Breadth, Thickness, Weight) %>% 
        mutate(Weight = parse_number(Weight))
    )) %>%
  drop_na() %>%
  pivot_longer(-id) %>%
    mutate(id = case_when(
      id == "Quina scraper" ~ "Quina scrapers",
      id == "Scraper" ~ "Scrapers",
      id == "Notch" ~ "Notches",
      id == "Denticulate" ~ "Denticulates",
      id == "Misc" ~ "Misc"
      )) %>%
  mutate(name = case_when(
    name == "Length" ~ "Length (mm)",
    name == "Breadth" ~ "Width (mm)",
    name == "Thickness" ~ "Thickness (mm)",
    name == "Weight" ~ "Weight (g)"
  )) %>%
  mutate(id = factor(id, levels = c("Quina scrapers",
                                    "Scrapers",
                                    "Notches",
                                    "Denticulates",
                                    "Misc"))) %>%
  mutate(name = factor(name, levels = c("Length (mm)", 
                                        "Width (mm)", 
                                        "Thickness (mm)", 
                                        "Weight (g)")))
    
# Overall size of Tools  

ggplot(Tools_clean) +
  aes(id, value)+
  geom_violin(
    fill = "lightgrey",
    alpha = 1,
    linewidth = 0,
    color = "white",
    adjust = 2
  ) +
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(
    aes(fill = id),
    alpha = 1,
    linewidth = 0.5,
    color = "black",
    width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Notches" = "#30A5C2",
      "Denticulates" = "#CDEBB3",
      "Quina scrapers" = "#21318C",
      "Scrapers" = "#1E80B8",
      "Misc" = "#EEF8B4"
    )) +
  scale_color_manual(
    values = c(
      "Notches" = "#30A5C2",
      "Denticulates" = "#CDEBB3",
      "Quina scrapers" = "#21318C",
      "Scrapers" = "#1E80B8",
      "Misc" = "#EEF8B4"
    )) +
  facet_wrap(~name, scales = "free_y") +
  theme_classic() +
  ylab("") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 

ggsave(filename = "Tool_size.png", width = 8, height = 6, dpi = 800, bg = "white")
  
# Sites to river distance-------------------------------------------------------------------------

Site_river_distance <- read_excel("Site_river_distance.xlsx", skip = 0) %>%
  mutate(Site = factor(Site, levels = c("Guanshan", 
                                        "Longtan", 
                                        "Tianhuadong",
                                        "Dazhuang",
                                        "Songping")))

ggplot(Site_river_distance) +
  aes(Site, Distance)+
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13), 
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  labs(x = "",
       y = "Distance (m)")

ggsave(filename = "Site_river_distance.png", width = 3, height = 5, dpi = 400, bg = "white")


































