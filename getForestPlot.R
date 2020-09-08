
library(ggplot2)
#
# function for forest plot
#
getForestPlot <- function(input_file, feature_name, figure_name) {
  
  # read CSV file 
  df <- read.csv(input_file)
  print(df)
  label       <- df$trial
  y           <- df$y
  lower       <- df$lower
  upper       <- df$upper
  pt_type     <- df$pt_type
  line_type   <- df$line_type
  
  # calculate the dashed vertical line by taking th average of all the mean/median
  #sum(y, na.rm = TRUE)
  #length(y[!is.na(y)]-1)
  #yint <- sum(y[-1], na.rm = TRUE)/length(y[!is.na(y)])
  #df$label <- factor(df$label, levels=rev(df$label))
  yint <- y[1]
    
  # forest plot
  fp <- ggplot(data=df, aes(x=label, y=y, ymin=lower, ymax=upper)) +
                geom_pointrange(aes(shape=pt_type)) +
                geom_linerange(aes(color=line_type)) +
                geom_hline(yintercept=yint, lty=2) +
                coord_flip() + # flip coordinates (puts labels on y axis)
                xlab("Trial + INTERMACS") + 
                ylab(feature_name) +
                theme_bw() # use a white background 
  print(fp)
  # update the directory by the command--cd clinical_data/
  ggsave(fp, filename = figure_name, path ="clinical_data")
}


#
# call function 
#
input = c("clinical_data/data/Age.csv", "clinical_data/data/BSA.csv", "clinical_data/data/CI.csv", "clinical_data/data/LVEF.csv",
          "clinical_data/data/ACEinh.csv", "clinical_data/data/IABP.csv", "clinical_data/data/Ventilation.csv", 
          "clinical_data/data/Ischemic.csv", "clinical_data/data/NYHA_IV.csv")
feature = c("Age (year)", "BSA (m^2)", "CI (l/min/m^2)", "LVEF (%)", 
            "ACE inhibitor", "Intra aortic balloon pump", "Ventilation", 
            "Ischemic cardiomyopathy", "NYHA Level IV")
figure = c("figure/Age.png", "figure/BSA.png", "figure/CI.png", "figure/LVEF.png", 
           "figure/ACEinh.png", "figure/IABP.png", "figure/Ventilation.png", 
           "figure/Ischemic.png", "figure/NYHA_IV.png")
size = length(input)
for (i in 1:size) {
  input_file = input[i]
  feature_name = feature[i]
  figure_name = figure[i]
  getForestPlot(input_file, feature_name, figure_name)
}



