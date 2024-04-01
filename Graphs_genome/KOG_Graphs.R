library (ggplot2)
library(ggbreak)
library(readr)

kog <- read_csv("~/Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/Cpelliculosa_update/New_annotations_masurca/KOG_ANNOTATION_R.csv")

#Removing value of 0 in the number of genes (it removed 2 KOGs: R and X)
kog_filtered <- kog[kog$`NUMBER OF GENES` != 0, ]

#Reverse the order so that the graph starts with A and finish at Z. 
kog_filtered$`KOG CLASSIFICATION` <- factor(kog_filtered$`KOG CLASSIFICATION`)

kog_filtered$`KOG CLASSIFICATION` <- factor(kog_filtered$`KOG CLASSIFICATION`, levels = rev(levels(kog_filtered$`KOG CLASSIFICATION`)))

#Make the column graph, and also set the background to white, including the breaks in the X-axis 
ggbase_filtered_1 <- ggplot(data = kog_filtered_1, aes(y = `KOG CLASSIFICATION`, x = `NUMBER OF GENES`, fill = `NAME OF KOGS` )) + guides(fill = guide_legend(ncol = 1, title = NULL))

ggsave("KOG_update.png", ggbase_filtered_1 + geom_col() + ylab(NULL) + scale_fill_discrete() + scale_x_break(c(400, 800)) + theme(panel.background = element_rect(fill = "white", colour = "black")), dpi = 300)

ggsave("KOG_update.png", ggbase_filtered_1 + geom_col() + ylab(NULL) + scale_fill_discrete() + scale_x_break(c(400, 800)) + theme(panel.background = element_rect(fill = "white", colour = "black")), width = 12, height = 8, dpi = 300)