# Manhattan plot of results
setwd('~/Desktop/ImperialCollage/analysis/PEM_14_2_18/miRNA/')
PM25p_resM2_pvalue<- read.table('PM25p_resM2_pvalue.txt')
PM25p_resM2_pvalue$fdr2_pm25_p<-p.adjust(PM25p_resM2_pvalue$log_pm25_adj_p, method = "fdr")

library(ggplot2)
PM25p_resM2_pvalue$mirna <- rownames(PM25p_resM2_pvalue)
PM25p_resM2_pvalue$mirna.name <- ifelse(-log(PM25p_resM2_pvalue$fdr2_pm25_p) > 0.5, 
               PM25p_resM2_pvalue$mirna, NA)

ggplot(PM25p_resM2_pvalue, aes(x = mirna, y = -log(fdr2_pm25_p))) + 
  geom_point() + 
  theme_bw() +
  #scale_shape_manual(values = )
  #scale_x_continuous() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank()) +
  geom_hline(yintercept = c(-log(0.05)), linetype = "dashed") +
  geom_text(aes(label = mirna.name), hjust = -0.1, vjust = 0, size = 3) +
  xlab("miRNA") + ylab("FDR adjusted log p-value")
