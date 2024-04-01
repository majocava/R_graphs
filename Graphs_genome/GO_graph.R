library (ggplot2)
library(ggbreak)
library(readr)

Data_count_GO_update <- read_csv("~/Documents/Documents - Maria’s MacBook Air - 1/18. UEES/10. UESS/Cpelliculosa_update/New_annotations_masurca/GO_Annotation/Data_count_GO_update.csv")
         
Data_count_GO_update$Meaning <- factor(Data_count_GO_update$Meaning)
Data_count_GO_update$Meaning <- factor(Data_count_GO_update$Meaning, levels = unique(Data_count_GO_update$Meaning))


Data_count_GO_update$Meaning <- factor(Data_count_GO_update$Meaning, levels = rev(levels(Data_count_GO_update$`Meaning`)))

Data_count_GO_update$`Process` <- factor(Data_count_GO_update$`Process`, levels = rev(levels(Data_count_GO_update$`Process`)))


gobase_filtered <- ggplot(data = Data_count_GO_update, aes(y = `Meaning`, x = `Number of Genes`, fill = `Process` )) + guides(fill = guide_legend(ncol = 1, title = NULL))

gobase_filtered + geom_col() + ylab(NULL) + scale_x_break(c(2000, 3000), scales = 0.2) + scale_fill_discrete() + theme(panel.background = element_rect(fill = "white", colour = "black"))                                 

ggsave("GO_update.png", gobase_filtered + geom_col() + ylab(NULL) + xlab("NUMBER OF GENES") + scale_x_break(c(2000, 3000), scales = 0.2) + scale_fill_discrete() + theme(panel.background = element_rect(fill = "white", colour = "black")), width = 12, height = 8, dpi = 300)                                 
