EJCorrPlotFigure <- function(x){
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  plot <- corrplot::corrplot(step3, method="color",
                     order = 'original',
                     type="upper",
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col="black", tl.srt=45, #Text label color and rotation
                     diag=FALSE, # hide correlation coefficient on the principal diagonal,
                     tl.cex=1.25
  )
  return(plot)
}
