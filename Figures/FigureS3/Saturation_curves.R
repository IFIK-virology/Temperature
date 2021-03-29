library(tidyverse)
#nreads is the number of downsampled reads, from the total number of reads in the library (from reads mapped to exons)
#medianreads is the median number of reads for each multiplexed sample within the library
#mediangenes is the median number of detected genes (>0 reads) across multiplexed samples within the library

Batch01 <- read.delim("./data.results.Run_1.txt")
Batch01$ID <- "Batch 1 - 72 samples"
Batch02 <- read.delim("./data.results.Run_2.txt")
Batch02$ID <- "Batch 2 - 96 samples"

Seq_depth <- rbind(Batch01, Batch02)
Seq_depth <- Seq_depth %>%
  mutate(totalreads = nreads/10e6)

#Plot meadian detect
p1 <- ggplot(Seq_depth, aes(x = meanreads, y = mediangenes)) +
  geom_point() + geom_path() + theme_bw() +
  facet_grid(.~ID) +
  labs(x="Mean reads per sample", y="Median detected genes") +
  theme(axis.text = element_text(size = 14, family = "sans"),
        axis.title = element_text(size = 16, family = "sans"),
        strip.text = element_text(size = 12, family = "sans"))

p2 <- ggplot(Seq_depth, aes(x = nreads/1e6, y = mediangenes)) +
  geom_point() + geom_path() + theme_bw() +
  facet_grid(.~ID) +
  labs(x="Mean reads per sample", y="Total sequencing depth (Mio reads)") +
  theme(axis.text = element_text(size = 14, family = "sans"),
        axis.title = element_text(size = 16, family = "sans"),
        strip.text = element_text(size = 12, family = "sans"))

saturation <- cowplot::plot_grid(p1, p2, align = "v", labels = "auto",
                   ncol = 1, nrow = 2)
ggpubr::ggexport(saturation, width = 10, height = 15, filename = "saturation_plots.pdf")

