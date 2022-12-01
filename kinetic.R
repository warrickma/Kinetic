library(ggplot2)
library(scales)
library(svglite)
library(reshape)
install.packages("reshape")
data = read.csv("kinetic.csv") #load csv file into a dataframe
data$Conversion = as.numeric(data$Conversion); #Convert all to numeric
data$Mn = data$Mn/1000
model = lm(Conversion~Time, data=data)
summary(data)

#time vs conversion graph
ggplot(data, aes(x = Time)) +
  geom_point(aes(y = Conversion)) +
  geom_smooth(aes(y = Conversion), method='lm', se=FALSE, color = "black") +
  scale_y_continuous(limits = c(0,1.00), n.breaks = 5, labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Conversion", x = "Time (min)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=8, color = "black", face = "bold"),
    axis.text.y = element_text(size=8, color = "black", face = "bold"),
    axis.title.x = element_text(size=8, color = "black", face = "bold"),
    axis.title.y = element_text(size=8, color = "black", face = "bold"))
ggsave("time_v_conversion.svg", width = 83, height = 50, units = "mm")
coeff = max(data$Mn/data$Dispersity)

#Plot nunber averaged molecular weight and dispersity against conversion
ggplot(data) +
  #geom_point(aes(x = Conversion, y = Mn)) +
  geom_point(aes(x = Conversion, y = Dispersity), color = "#DF1E12", shape = 24, fill = "#DF1E12") +
  geom_point(aes(x = Conversion, y = Mn / coeff + 0.802)) +
  geom_smooth(aes(x = Conversion, y = Mn / coeff + 0.802), method ='lm', se=FALSE, color = "black") +
  scale_y_continuous(name = "Dispersity",
                     sec.axis = sec_axis(~.*coeff-0.802*coeff, name = "Mn (kDa)")) +
  scale_x_continuous(limits = c(0,1.00), n.breaks = 5, labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Conversion") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=8, color = "black", face = "bold"),
    axis.text.y = element_text(size=8, color = "black", face = "bold"),
    axis.title.x = element_text(size=8, color = "black", face = "bold"),
    axis.title.y = element_text(size=8, color = "black", face = "bold"))
ggsave("conversion_v_Mn.svg", width = 83, height = 50, units = "mm")

gpc = read.csv("GPC_trace.csv") #load the gpc raw file. Raw file must be in csv format.
summary(gpc) 
colnames(gpc) = c("Retention_Volume", "0.5hr", "1.5hr", "1hr", "2.5hr", "5hr", "PPS") #Change column names to whatever you want
new_gpc = subset(gpc, gpc$Retention_Volume > 15 & gpc$Retention_Volume <22.5) #Define the plotting area
summary(new_gpc)
#Nomralize RI signal intensity to the talled peak
new_gpc$`1hr`= new_gpc$`1hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`1hr`)
new_gpc$`1.5hr`= new_gpc$`1.5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`1.5hr`)
new_gpc$`2.5hr`= new_gpc$`2.5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`2.5hr`)
new_gpc$`5hr`= new_gpc$`5hr`*max(new_gpc$`0.5hr`)/max(new_gpc$`5hr`)
new_gpc$`PPS`= new_gpc$`PPS`*max(new_gpc$`0.5hr`)/max(new_gpc$`PPS`)

#Plot GPC Data
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
    axis.text.y = element_blank(),
    axis.title.x = element_text(size=8, color = "black", face = "bold"),
    axis.title.y = element_blank(),
    legend.text = element_text(size=8, color = "black", face = "bold"),
    legend.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank())
ggsave("cgpc.svg", width = 83, height = 50, units = "mm")
