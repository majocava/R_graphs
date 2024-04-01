library (ggplot2)
library(ggbreak)
library(readr)

kegg <- read_csv("~/Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/Cpelliculosa_update/New_annotations_masurca/KEGG/KEG.csv")
kegg$Paths <- factor(kegg$Paths, levels = unique(kegg$Paths))

#Make the column graph, and also set the background to white, including the breaks in the X-axis 
ggbase_filtered <- ggplot(data = kegg, aes(y = `Paths`, x = `Number`, fill = `Class` )) + guides(fill = guide_legend(ncol = 1, title = NULL))

ggsave("Kegg_2.png", plot = ggbase_filtered + geom_bar(stat="identity", colour="black", linewidth=0.01) + xlab("Number of genes") + ylab(NULL) + scale_fill_discrete() + scale_x_break(c(750, 1000), scales = 0.2) + theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill=NA, colour="black"), axis.text.y = element_text(size = 6), axis.text.x = element_text(size = 6), text = element_text(size = 7)) + scale_x_continuous(breaks = seq(0, 1400, 200) ), width = 6, height=5.8 ,dpi=300)

legend_plot <- ggplot(data = kegg, aes(x = 1, y = 1, fill = Class)) +
  +     geom_bar(stat = "identity", show.legend = TRUE) +
  +     guides(fill = guide_legend(ncol = 1, title = NULL)) +
  +     theme_void() 

ggsave("kegg_legend.png", legend_plot, width = 6, height=5.8 ,dpi=300)

ggsave("Kegg_update.png", plot = ggbase_filtered + geom_bar(stat="identity", colour="black", linewidth=0.01) + xlab("NUMBER OF GENES") + ylab(NULL) + scale_fill_discrete() + scale_x_break(c(750, 1000), scales = 0.2) + theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill=NA, colour="black"), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 9), text = element_text(size = 10), legend.text = element_text(size = 9)) + scale_x_continuous(breaks = seq(0, 1400, 200)), width = 12, height=8, dpi=300)