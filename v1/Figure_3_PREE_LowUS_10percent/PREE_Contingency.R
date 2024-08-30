library(tidyverse)
library(afex)
library(emmeans)
library(pwr2)

# df ---------------------------------------------------

list.files(pattern=".csv$", recursive = TRUE)
list.filenames <- list.files(".", pattern = ".csv$", 
                             full.names=TRUE, recursive = TRUE)
# delete the metadata from the list.filenames
list.filenames <- list.filenames[!(str_detect(list.filenames, "metadata"))]
list.filenames
metadata <- read.csv("metadata.csv")
list.data <- list()

for (i in 1:length(list.filenames)) {
  list.data[[i]] <- read.csv(list.filenames[[i]], skip=
                               ifelse(str_detect(list.filenames[[i]], "FC\\w*10.csv$"), 42,
                                      ifelse(str_detect(list.filenames[[i]], "FC\\w*100.csv$"), 15,
                                             ifelse(str_detect(list.filenames[[i]], "FC1.csv$"), 18,
                                                    ifelse(str_detect(list.filenames[[i]], "FC2.csv$"), 18,
                                                          ifelse(str_detect(list.filenames[[i]], "ER"), 16,
                                                                 ifelse(str_detect(list.filenames[[i]], "SR"), 16,
                                                                        24))))))
  )
  list.data[[i]]$ExpDetails <- str_sub(list.filenames[[i]], 22, -5)
  #remove the first and last characters 
  list.data[[i]] <- dplyr::select(list.data[[i]], -c(Animal, Group))
}

list.data <- list.data %>% 
  reduce(full_join) %>% #collapse separate items in list into one
  separate(col=ExpDetails, into = c("Exp_Date", "Exp_Type"), sep = "_") 

# Conver list into a dataframe
df<-as.data.frame(list.data)
# Join df and metadata
df<- full_join(df, metadata,  by= c("Exp_Type", "Exp_Date", "Box", "Trial"))


df<- df%>%
  mutate(id= paste(Group, Cage, Animal, sep="_")) %>% 
  mutate(Trial=as.factor(Trial)) %>% 
  mutate(CS_Time=as.factor(CS_Time)) %>% 
  mutate(Contingency=ifelse(
    str_detect(Group, "10$"), "10%",
    ifelse(str_detect(Group, "100$"),"100%",
           ifelse(str_detect(Group, "50$"),"50%", NA
                  ))))

rm(list=setdiff(ls(), "df"))
df<- df %>% 
  drop_na(Trial, Strain, Experiment)  

df$Contingency <- factor(df$Contingency, levels = c("100%", "50%", "10%"))


# Fig 3. (10% vs. 100%) Fear Learning/Conditioning --------------------------------------------------------------------
## B. Fear Acquisition --------------------------------------------------
count_fig1 <- df %>% 
  filter(str_detect(Exp_Type, "FC"),
         str_detect(Contingency, "50%")) %>% 
  group_by(Contingency, Sex, Exp_Type, id) %>% 
  summarize(n=n())

summary(count_fig1)



FC_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "FC")) %>% 
  mutate(Condition = ifelse(
    str_detect(Exp_Type, "FC"), "FC", NA)) %>% 
  group_by(id, Group, Contingency, Condition) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

FC_ave_1 <- FC_id_1 %>% 
  group_by(Group, Contingency) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

#write_csv(FC_ave, "220803_PREE_FC_ave.csv")

ggplot(FC_ave_1,
       aes(x=Contingency,
           y=group_mean_freezing, 
           group=Contingency,
           color=Contingency,
           fill=Contingency,
           shape=Contingency)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width=0.8,
    alpha=0.4) + 
  geom_errorbar(
    aes(
      ymin = group_mean_freezing-se,
      ymax = group_mean_freezing+se,
      width = .2),
    position=position_dodge(width=0.8)
  ) +
  geom_point(
    data=FC_id_1,
    aes(x=Contingency, y=mean_freezing), 
    position = position_jitterdodge(jitter.height=0, jitter.width=0.5,
                                    dodge.width = 0.8),
    size=3
  ) +
  ylim(0,100) +
  theme_bw() + 
  theme(legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "top",
        panel.border= element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 18,face="bold.italic", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        strip.text = element_text(size = 18)) + 
  labs(
    x = "Contingency",
    y = "% Time Freezing")

m1b <- FC_id_1 %>%
  aov_ez(id = "id",
         dv = "mean_freezing",
         between = "Contingency",
         within = "Condition",
         data = .)
m1b

n1b <- emmeans(m1b, "Contingency", by = "Condition")
pairs(n1b)



## C. Fear Retrieval ----------------------------------------------------------
E1_Session1_id_1 <- df %>% 
  filter(str_detect(Experiment, "E1"),
         str_detect(Experiment, "ER", negate = TRUE),
         str_detect(Component.Name,  "tone 01|tone 02|tone 03|tone 04")) %>% 
  group_by(id, Group, CS_Time, Contingency, Exp_Type) %>%
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

E1_Session1_ave_1 <- E1_Session1_id_1 %>% 
  group_by(CS_Time, Group, Contingency, Exp_Type) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

FC2_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "FC2")) %>% 
  group_by(id, Group, CS_Time, Contingency, Exp_Type) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

FC2_ave_1 <- FC2_id_1 %>% 
  group_by(CS_Time, Group, Contingency, Exp_Type) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))


E1_FC2_ave_1 <-rbind(E1_Session1_ave_1, FC2_ave_1)
E1_FC2_ave_1 <- E1_FC2_ave_1 %>% 
  mutate(Exp_Type = if_else(Exp_Type == "E1", "Session 1", Exp_Type))

E1_FC2_id_1 <-rbind(E1_Session1_id_1, FC2_id_1)
E1_FC2_id_1 <- E1_FC2_id_1 %>% 
  mutate(Exp_Type = if_else(Exp_Type == "E1", "Session 1", Exp_Type))

ggplot(
  E1_FC2_ave_1,
  aes(x=Exp_Type,
      y=group_mean_freezing,
      group = Contingency,
      color = Contingency,
      fill = Contingency,
      shape = Contingency)) +
  geom_point(
    size = 3
  ) +
  geom_line(
  ) + 
  facet_wrap(
    vars(CS_Time)
  ) +
  ylim(0,100) +
  geom_ribbon(
    aes(
      ymin = group_mean_freezing-se,
      ymax = group_mean_freezing+se),
    alpha = .2,
    color = NA
  ) +
  theme_bw() +
  theme(legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "top",
        panel.border= element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 18,face="bold.italic", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        strip.text = element_text(size = 18)) + 
  labs(
    x = "Day",
    y = "% Time Freezing")

m2b <- E1_FC2_id_1 %>%
  aov_ez(id = "id",
         dv = "mean_freezing",
         between = "Contingency",
         within = "Exp_Type",
         data = .)
m2b

n2b <- emmeans(m2b, "Exp_Type", by = "Contingency")
pairs(n2b)

## D. Extinction Acquisition -----------------------------------------------
bin1 =c("tone 01", "tone 02")
bin2 =c("tone 03", "tone 04")
bin3 =c("tone 05", "tone 06")
bin4 =c("tone 07", "tone 08")
bin5 =c("tone 09", "tone 10")
bin6 =c("tone 11", "tone 12")

EXT_bin_id_1 <- df %>% 
  filter(str_detect(Experiment, "E"),
         str_detect(Experiment, "ER", negate = TRUE)) %>% 
  mutate(Bin = ifelse(Component.Name %in% "pre-tone", "Pre",
                      ifelse(Component.Name %in% bin1, "1",
                             ifelse(Component.Name %in% bin2, "2",
                                    ifelse(Component.Name %in% bin3, "3",
                                           ifelse(Component.Name %in% bin4, "4",
                                                  ifelse(Component.Name %in% bin5, "5",
                                                         ifelse(Component.Name %in% bin6, "6", "Post")))))))) %>% 
  group_by(Exp_Type, id, Group, CS_Time, Contingency, Bin) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

EXT_bin_ave_1 <- EXT_bin_id_1 %>% 
  group_by(Exp_Type, Bin, Group, CS_Time, Contingency) %>%
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

EXT_bin_ave_1$Bin <- factor(EXT_bin_ave_1$Bin, levels = c("Pre", "1", "2", "3", "4", "5", "6", "Post"))

ggplot(
  data = subset(EXT_bin_ave_1, !is.na(Bin)),
  aes(x=Bin,
      y=group_mean_freezing,
      group = Contingency,
      color = Contingency,
      fill = Contingency,
      shape=Contingency)) +
  facet_wrap(
    vars(CS_Time, Exp_Type)
  ) +
  geom_point(
    size = 3
  ) +
  geom_line(
  ) + 
  ylim(0,100) +
  geom_ribbon(
    aes(
      ymin = group_mean_freezing-se,
      ymax = group_mean_freezing+se),
    alpha = .2,
    color = NA
  ) +
  theme_bw() +
  theme(legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "top",
        panel.border= element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 18,face="bold.italic", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, color="black", angle = 45),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        strip.text = element_text(size = 18)) + 
  labs(
    x = "Bin",
    y = "% Time Freezing")

m2a <- EXT_bin_id_1 %>% 
  aov_ez(id = "id",
         dv = "mean_freezing",
         between = "Contingency",
         within = c("Exp_Type", "Bin"),
         data = .)

m2a

n2a <- emmeans(m2a, "Contingency", by = c("Exp_Type", "Bin"))
pairs(n2a)



## E. Extinction Consolidation ---------------------------------------------
EXT_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "E"),
         str_detect(Exp_Type, "ER", negate = TRUE)) %>% 
  group_by(Exp_Type, id, Group, CS_Time, Contingency) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

EXT_ave_1 <- EXT_id_1 %>% 
  group_by(Exp_Type, CS_Time, Group, Contingency) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

ggplot(
  EXT_ave_1,
  aes(x=Exp_Type,
      y=group_mean_freezing,
      group = Contingency,
      color = Contingency,
      fill = Contingency,
      shape = Contingency)) +
  geom_point(
    size = 3
  ) +
  geom_line(
  ) + 
  facet_wrap(
    vars(CS_Time)
  ) +
  ylim(0,100) +
  geom_ribbon(
    aes(
      ymin = group_mean_freezing-se,
      ymax = group_mean_freezing+se),
    alpha = .2,
    color = NA
  ) +
  theme_bw() +
  theme(legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "top",
        panel.border= element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 18,face="bold.italic", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        strip.text = element_text(size = 18)) + 
  labs(
    x = "Day",
    y = "% Time Freezing")

m2b <- EXT_id_1 %>%
  aov_ez(id = "id",
         dv = "mean_freezing",
         between = "Contingency",
         within = "Exp_Type",
         data = .)
m2b

n2b <- emmeans(m2b, "Exp_Type", by = "Contingency")
pairs(n2b)


## F. Extinction Recall -------------------------------------------------------------
ER_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "ER")) %>% 
  group_by(id, Group, CS_Time, Contingency, Exp_Type) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

ER_ave_1 <- ER_id_1 %>% 
  group_by(CS_Time, Group, Contingency, Exp_Type) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

E3_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "E3")) %>% 
  group_by(id, Group, CS_Time, Contingency, Exp_Type) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

E3_ave_1 <- E3_id_1 %>% 
  group_by(CS_Time, Group, Contingency, Exp_Type) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

SR_id_1 <- df %>% 
  filter(str_detect(Component.Name, "pre-tone|post-tone", negate = TRUE),
         str_detect(Exp_Type, "SR")) %>% 
  group_by(id, Group, CS_Time, Contingency, Exp_Type) %>% 
  summarize(mean_freezing=mean(Pct.Component.Time.Freezing), n=n(),
            se=sd(Pct.Component.Time.Freezing/sqrt(n())))

SR_ave_1 <- SR_id_1 %>% 
  group_by(CS_Time, Group, Contingency, Exp_Type) %>% 
  summarize(group_mean_freezing=mean(mean_freezing), n=n(),
            se=sd(mean_freezing/sqrt(n())))

E3_ER_SR_ave_1 <-rbind(E3_ave_1, ER_ave_1, SR_ave_1)
E3_ER_SR_id_1 <-rbind(E3_id_1, ER_id_1, SR_id_1)

ggplot(
  E3_ER_SR_ave_1,
  aes(x=Exp_Type,
      y=group_mean_freezing,
      group = Contingency,
      color = Contingency,
      fill = Contingency,
      shape = Contingency)) +
  geom_point(
    size = 3
  ) +
  geom_line(
  ) + 
  facet_wrap(
    vars(CS_Time)
  ) +
  ylim(0,100) +
  geom_ribbon(
    aes(
      ymin = group_mean_freezing-se,
      ymax = group_mean_freezing+se),
    alpha = .2,
    color = NA
  ) +
  theme_bw() +
  theme(legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "top",
        panel.border= element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 18,face="bold.italic", hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, color="black"),
        axis.text.y = element_text(size = 18, color="black"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        strip.text = element_text(size = 18)) + 
  labs(
    x = "Day",
    y = "% Time Freezing")

m2b <- E3_ER_SR_id_1 %>%
  aov_ez(id = "id",
         dv = "mean_freezing",
         between = "Contingency",
         within = "Exp_Type",
         data = .)
m2b

n2b <- emmeans(m2b, "Exp_Type", by = "Contingency")
pairs(n2b)

