library(ggplot2)
library(scales)
library(svglite)
library(reshape)
gpc = read.csv("GPC_trace.csv") 
summary(gpc) 
colnames(gpc) = c("Retention_Volume", "0.5hr", "1.5hr", "1hr", "2.5hr", "5hr", "PPS")
new_gpc = subset(gpc, gpc$Retention_Volume > 15 & gpc$Retention_Volume <22.5)
summary(new_gpc)
new_gpc$`1hr`= new_gpc$`1hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`1hr`)
new_gpc$`1.5hr`= new_gpc$`1.5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`1.5hr`)
new_gpc$`2.5hr`= new_gpc$`2.5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`2.5hr`)
new_gpc$`5hr`= new_gpc$`5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`5hr`)
new_gpc$`PPS`= new_gpc$`PPS`*max(new_gpc$`0.5hr`)/max(new_gpc$`PPS`)

mdata = melt(new_gpc, id = "Retention_Volume")
ggplot(mdata, aes(Retention_Volume, value)) +
  geom_line(aes(colour = variable)) +
  scale_color_manual(name = "variable",
                     limits = c("PPS", "0.5hr", "1hr", "1.5hr", "2.5hr", "5hr"),
                     values = c("#222222", "#6EB43F", "#FF00FF", "#B31B1B", "#D47500", "#006699")) +
  scale_x_continuous(limits = c(16, 22.3), n.breaks = 4) +
  scale_y_continuous(limits = c(0, 300), n.breaks = 5) +
  labs(y = "RI", x = "Retention Volume (mL)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=8, color = "black", face = "bold"),
    axis.text.y = element_text(size=8, color = "black", face = "bold"),
    axis.title.x = element_text(size=8, color = "black", face = "bold"),
    axis.title.y = element_text(size=8, color = "black", face = "bold"),
    legend.text = element_text(size=8, color = "black", face = "bold"),
    legend.title = element_blank())
ggsave("cgpc.eps", device="eps", width = 83, height = 50, units = "mm")
