library("readxl")
library(tidyverse)
library(ggpmisc)

#-------------------------------------------------------------------------------
# Cores
# import the data 
cores <- 
  read_excel("Longtan site lithic data-Cores.xlsx", skip = 1) %>%
  filter(...1 %in% c('Quina core', 'Discoid', 'C-O-F', 'Surface core')) %>%
  rename("Coretype" = ...1,
         "Length (mm)" = `长（高）（mm）`,
         "Width (mm)" = `宽（mm）`,
         "Thickness (mm)" = `厚（mm）`,
         "Weight (g)" = `重量`,
         "Platform angle" = `平均台面角...41`) 
  
  
cores_clean <-   #create a new dataset
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
                                      "Surface cores")))

p1 <- ggplot(cores_clean, aes(x = Coretype, y = value, fill = Coretype)) +
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
  facet_wrap(~ name, scales = "free_y") +   #auto adjust ylim
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ylab("")

cores_angle <- cores %>%
  mutate(Coretype = case_when(
    Coretype == "Quina core" ~ "Quina cores",               #重命名
    Coretype == "Surface core" ~ "Surface cores",   
    Coretype == "Discoid" ~ "Discoidal cores",
    Coretype == "C-O-F" ~ "C-O-Fs"
  ) ) %>%
  mutate(Coretype = factor(Coretype, levels = c("Quina cores",       #重排顺序
                                                "Discoidal cores",
                                                "C-O-Fs",
                                                "Surface cores")))

p2 <-  ggplot(cores_angle, aes(x = Coretype, y = `Platform angle`, fill = Coretype)) +
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

#-------------------------------------------------------------------------------
# Flakes 

flakes <- read_excel("Longtan site lithic data-Flakes.xlsx", skip = 0)
resharpening <- read_excel("Longtan lithic data-Reaffutage.xlsx", skip = 1)

resharpening_clean <- 
  resharpening %>%
  select(`Length (mm)` = ...2,
         `Width (mm)` = `...3`,
         `Thickness (mm)` = `...4`,
         `Weight (mm)` = `...5`,
         `Interioir platform angle` = `IPA`) %>%
  mutate(typology = "Resharpening flakes") %>%
  drop_na()

flakes_clean <- 
flakes %>%
  select(typology = ...1,
         `Length (mm)` = `长（mm）`,
         `Width (mm)` = `宽（mm）`,
         `Thickness (mm)` = `厚（mm）`,
         `Weight (mm)` = `重量（g）`,
         `Interioir platform angle` = `IPA`) %>%
  mutate(typology = case_when(
    typology ==  "Kombewa" ~ "Kombewa flakes", 
    typology ==  "Surface" ~ "Surface flakes",   
    typology ==  "Discoid" ~ "Discoidal flakes",
    typology ==  "Quina flake" ~ "Quina flakes",
  )) %>%
  drop_na() 

all_flakes_clean <- 
bind_rows(resharpening_clean,
          flakes_clean) %>%
  pivot_longer(-typology) %>%
  mutate(typology = factor(typology, 
                           levels = c("Quina flakes", 
                                      "Discoidal flakes", 
                                      "Kombewa flakes", 
                                      "Surface flakes", 
                                      "Resharpening flakes"))) 
  ggplot(all_flakes_clean %>%
           filter(name != "Interioir platform angle")) +
  aes(typology, value) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = typology), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
  facet_wrap( ~ name, scales = "free_y") +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4")) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  stat_multcomp(geom = "text_pairwise",
                size = 2, 
                small.p = TRUE,
                contrasts = "Dunnet")
  
  
  ggplot(all_flakes_clean %>%
           filter(name == "Interioir platform angle")) +
    aes(typology, value) +
    geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
    stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
    geom_boxplot(aes(fill = typology), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
    geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
    ylab("Interior platform angle") +
    xlab("") +
    scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4" )) +
    scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4")) +
    theme_classic() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    stat_multcomp(geom = "text_pairwise",
                  size = 2, 
                  small.p = TRUE,
                  contrasts = rbind(c(0, 0, 0, -1, 1),
                                    c(0, 0, -1, 1, 0),
                                    c(0, -1, 0, 1, 0),
                                    c(-1, 0, 0, 1, 0)))
  

  
flake_platform_type <- 
  bind_rows(.id = "id",
    flakes = flakes %>%
      select(`Platform type` = `台面类型`,
             `Flake type` = `...1`),
     resharpening = resharpening %>% 
      select(`Platform type` = `Type`) %>%
      mutate(`Flake type` = "Resharpening")
  ) %>%
  mutate(`Platform type`= case_when(
    `Platform type` == "素"~ "Plain",
    `Platform type` == "自然"~ "Natural",
    `Platform type` == "线"~ "Linear",
    `Platform type` == "单棱脊"~ "Dihedral",
    `Platform type` == "双棱脊"~ "Faceted",
    .default = `Platform type`
  ) ) %>% 
  drop_na(`Platform type`)



ggplot(data = flake_platform_type, 
       aes(`Flake type`, 
           group = `Platform type`,
           fill = `Platform type`)) +
  geom_bar() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Faceted" = "#30A5C2",
      "Dihedral" = "#1E80B8",
      "Natural" = "#EEF8B4",
      "Linear" = "#CDEBB3",
      "Plain" = "#21318C")) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Dorsal scar pattern")) +
  xlab("") + ylab("Percentage") 


#-------------------------------------------------------------------------------
# Tools

qn_scrapers <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1)
scraper <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Scrapers")
notch <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Notches")
denticulate <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Denticulates")
Misc <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 0, sheet = "Miscellaneous tools")




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
    )
  ) %>%
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
    name == "Weight" ~ "Weight (mm)"
  )) %>%
  mutate(id = factor(id, levels = c("Quina scrapers",
                                    "Scrapers",
                                    "Notches",
                                    "Denticulates",
                                    "Misc"))) %>%
  ggplot() +
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
    )
  ) +
  scale_color_manual(
    values = c(
      "Notches" = "#30A5C2",
      "Denticulates" = "#CDEBB3",
      "Quina scrapers" = "#21318C",
      "Scrapers" = "#1E80B8",
      "Misc" = "#EEF8B4"
    )
  ) +
  facet_wrap(~name, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
#-------------------------------------------------------------------


#工具长度盒须图
LENGTHTOOL$TYPO <- factor(LENGTHTOOL$TYPO, levels = c('Quina scrapers', 'Ordinary scrapers', 'Notches', 'Denticulates', 'Misc'))
p1 <- ggplot(LENGTHTOOL, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p1)
ggsave("LENGTHTOOL.png", plot = p, width = 6, height = 8, units = "in", dpi = 800)
#工具宽度盒须图
WIDTHTOOL$TYPO <- factor(WIDTHTOOL$TYPO, levels = c('Quina scrapers', 'Ordinary scrapers', 'Notches', 'Denticulates', 'Misc'))
p2 <- ggplot(WIDTHTOOL, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p2)
ggsave("WIDTHTOOL.png", plot = p, width = 6, height = 8, units = "in", dpi = 800)
#工具厚度盒须图
THICKTOOL$TYPO <- factor(THICKTOOL$TYPO, levels = c('Quina scrapers', 'Ordinary scrapers', 'Notches', 'Denticulates', 'Misc'))
p3 <- ggplot(THICKTOOL, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p3)
ggsave("THICKTOOL.png", plot = p, width = 6, height = 8, units = "in", dpi = 800)
#工具质量盒须图
MASSTOOL$TYPO <- factor(MASSTOOL$TYPO, levels = c('Quina scrapers', 'Ordinary scrapers', 'Notches', 'Denticulates', 'Misc'))
p4 <- ggplot(MASSTOOL, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 0.5, color = "black", width = 0.4) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 2, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Notches" = "#30A5C2", "Denticulates" = "#CDEBB3", "Quina scrapers" = "#21318C", "Ordinary scrapers" = "#1E80B8","Misc" = "#EEF8B4" )) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p4)
ggsave("MASSTOOL.png", plot = p, width = 6, height = 8, units = "in", dpi = 800)

combined_plot <- (p1 | p2) / (p3 | p4)
print(combined_plot)
ggsave("TOOLSIZE.png", combined_plot, width = 8, height = 8, units = "in", dpi = 800)

#石片IPA盒须图
IPAFLAKE$TYPO <- factor(IPAFLAKE$TYPO, levels = c('Quina flakes', 'Discoidal flakes', 'Kombewa flakes', 'Surface flakes', 'Resharpening flakes'))
p1 <- ggplot(IPAFLAKE, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 1, color = "black", width = 0.6) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 4, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", "Surface flakes" = "#CDEBB3", "Quina flakes" = "#21318C", "Discoidal flakes" = "#1E80B8","Resharpening flakes" = "#EEF8B4")) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p1)
ggsave("IPAFLAKE.png", plot = p1, width = 10, height = 8, units = "in", dpi = 800)

#工具刃角密度图
p <- ggplot(EASCRAPER, aes(x = DATA, fill = TYPO)) +       
  geom_density(alpha = 0.7, color = "transparent", adjust = 0.5, kernel = "optcosine") +  
  labs(x = "Values", y = "Frequency") +  
  scale_fill_manual(values = c("Quina scrapers" = "#21318C", "Ordinary scrapers" = "#30A5C2")) + 
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p)
ggsave("grouped_density.png", plot = p, width = 6, height = 4, units = "in", dpi = 800)


#石核台面角盒须图
PACORE$TYPO <- factor(PACORE$TYPO, levels = c('Quina cores', 'Discoidal cores', 'COFs', 'Surface cores'))
p1 <- ggplot(PACORE, aes(x = TYPO, y = DATA, fill = TYPO)) +
  geom_violin(fill = "lightgrey", alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = TYPO), alpha = 1, linewidth = 1, color = "black", width = 0.5) + 
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 4, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c( "COFs" = "#30A5C2", "Surface cores" = "#CDEBB3", "Quina cores" = "#21318C", "Discoidal cores" = "#1E80B8")) +
  scale_color_manual(values = c( "COFs" = "#30A5C2", "Surface cores" = "#CDEBB3", "Quina cores" = "#21318C", "Discoidal cores" = "#1E80B8")) +
  theme_classic() +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank())
print(p1)
ggsave("PACORE.png", plot = p1, width = 8, height = 6, units = "in", dpi = 800)

#石片台面类型堆叠图
PLATFORMTYPE$TYPE <- factor(PLATFORMTYPE$TYPE, levels = c('Natural', 'Linear', 'Facetted', 'Dihederal', 'Plain'))
PLATFORMTYPE$TYPO <- factor(PLATFORMTYPE$TYPO, levels = c('Quina flakes', 'Discoidal flakes', 'Kombewa flakes', 'Surface flakes', 'Resharpening flakes', 'Ordinary flakes'))
p <- ggplot(data = PLATFORMTYPE, aes(TYPO, PERCENT, GROUP = TYPE)) +
  geom_col(aes(fill = factor(TYPE, levels = rev(levels(factor(TYPE)))))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Facetted" = "#30A5C2", "Dihederal" = "#1E80B8", "Natural" = "#EEF8B4", "Linear" = "#CDEBB3", "Plain" = "#21318C")) +
  theme_minimal() +
  theme(legend.position = "right")
print(p)
ggsave("PLATFORMTYPE.png", plot = p, width = 6, height = 4, units = "in", dpi = 800)


#石片背疤模式堆叠图
DSPFLAKE$TYPE <- factor(DSPFLAKE$TYPE, levels = c( 'Unidentified', 'Centripetal','Multi direction', 'Lateral', 'Opposite direction', 'Distal only', 'Proximal only'))
DSPFLAKE$TYPO <- factor(DSPFLAKE$TYPO, levels = c('Quina flakes', 'Discoidal flakes', 'Kombewa flakes', 'Surface flakes', 'Resharpening flakes', 'Ordinary flakes'))
p <- ggplot(data = DSPFLAKE, aes(TYPO, PERCENT, GROUP = TYPE)) +
  geom_col(aes(fill = factor(TYPE, levels = rev(levels(factor(TYPE)))))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Opposite direction" = "#30A5C2", "Distal only" = "#1E80B8", "Multi direction" = "#EEF8B4", "Lateral" = "#CDEBB3", "Centripetal" = "#FADA6C", "Proximal only" = "#21318C", "Unidentified" = "#F58358")) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Dorsal scar pattern")) +
  xlab("") + ylab("Percentage")
print(p)
ggsave("DSPFLAKE.png", plot = p, width = 6, height = 4, units = "in", dpi = 800)












































