## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

#Set working dir location location
(WD <- dirname(rstudioapi::getSourceEditorContext()$path))
if (!is.null(WD)) setwd(WD)

#Load counts
Viral_Reads <- read.table("./Viral_Reads.txt", header = T)#Load viral read table

Viral_Reads$Temperature <- gsub("33", "33°C", Viral_Reads$Temperature)
Viral_Reads$Temperature <- gsub("37", "37°C", Viral_Reads$Temperature)

## Mean fraction of total viral counts

#SARS-CoV-2
p1 <- Viral_Reads %>% filter(Genome == "SARS.CoV.2") %>% 
  ggplot(., aes(x=Condition, y=Frac_Total_Viral_Counts, fill=Condition)) +
  geom_boxplot(coef=1e30) +
  geom_jitter(aes(colour = Donor)) +
  #geom_col(position=position_dodge(1), width = 0.5) + 
  scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02"), labels = c("Mock", "SARS-CoV", "SARS-CoV-2")) +
  facet_grid(Temperature ~ Time) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.text = element_text(size = 14, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"), strip.text = element_text(size = 14), 
        axis.title = element_text(size = 16, family = "sans"),
        legend.title = element_text(size = 18, family = "sans")) +
  scale_x_discrete(breaks = c("Mock", "SARS.CoV", "SARS.CoV.2"),
                   labels = c("Mock", "SARS-CoV", "SARS-CoV-2")) +
  labs(y= "Fraction of Viral Counts", x = "", fill = "") 

# Modify colors facet rectangle to match condition palette
e <- ggplot_gtable(ggplot_build(p1))
strip_t <- which(grepl('strip-t', e$layout$name))
time <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', e$grobs[[i]]$grobs[[1]]$childrenOrder))
  e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-time[k]
  k <- k+1
}
strip_r1 <- which(grepl('strip-r', e$layout$name))
temp <- c("#66a61e", "#e7298a")
k <- 1
for (i in strip_r1) {
  j <- which(grepl('rect', e$grobs[[i]]$grobs[[1]]$childrenOrder))
  e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- t_col(temp[k])
  k <- k+1  
}

ggsave(filename = "SARS.CoV.2_Viral_Reads.pdf", grid::grid.draw(e), width = 30, height = 20, units = "cm")

#SARS-CoV
p2 <- Viral_Reads %>% filter(Genome == "SARS.CoV") %>% 
  ggplot(., aes(x=Condition, y=Frac_Total_Viral_Counts, fill=Condition)) +
  geom_boxplot(coef=1e30) +
  scale_fill_manual(values = c("#1b9e77","#7570b3","#d95f02"), labels = c("Mock", "SARS-CoV", "SARS-CoV-2")) +
  geom_jitter(aes(colour = Donor)) +
  #geom_col(position=position_dodge(1), width = 0.5) + 
  facet_grid(Temperature ~ Time) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.text = element_text(size = 14, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"), strip.text = element_text(size = 14), 
        axis.title = element_text(size = 16, family = "sans"),
        legend.title = element_text(size = 18, family = "sans"),
        ) +
  scale_x_discrete(breaks = c("Mock", "SARS.CoV", "SARS.CoV.2"),
                   labels = c("Mock", "SARS-CoV", "SARS-CoV-2")) +
  labs(y= "Fraction of Viral Counts", x = "", fill = "Condition") 

# Modify colors facet rectangle to match condition palette
e <- ggplot_gtable(ggplot_build(p2))
strip_t <- which(grepl('strip-t', e$layout$name))
time <- c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")
k <- 1
for (i in strip_t) {
  j <- which(grepl('rect', e$grobs[[i]]$grobs[[1]]$childrenOrder))
  e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-time[k]
  k <- k+1
}
strip_r1 <- which(grepl('strip-r', e$layout$name))
temp <- c("#66a61e", "#e7298a")
k <- 1
for (i in strip_r1) {
  j <- which(grepl('rect', e$grobs[[i]]$grobs[[1]]$childrenOrder))
  e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- t_col(temp[k])
  k <- k+1  
}

ggsave(filename = "SARS.CoV_Viral_Reads.pdf", grid::grid.draw(e), width = 30, height = 20, units = "cm")

