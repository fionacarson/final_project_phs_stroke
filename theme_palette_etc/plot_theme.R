theme_cc_project <- function(){
  
  theme_minimal() %+replace%   
    
    theme(
      axis.line = element_line(colour = "grey50"),
      axis.ticks = element_line(colour = "grey50"),
      plot.title = element_text(size = 22, 
                                face = "bold", 
                                colour = "grey50",
                                margin = margin(b = 12),
                                hjust = 0),
      plot.subtitle = element_text(size = 18,  
                                   colour = "grey50",
                                   margin = margin(b = 12),
                                   hjust = 0),
      axis.title = element_text(size = 16, 
                                face = "bold", 
                                colour = "grey50"),
      axis.title.x = element_text(margin = margin(t = 12)),
      # changing the margin on y-axis causes it to flip to horizontal   
      # axis.title.y = element_text(margin = margin(b = 12)),
      axis.text = element_text(size = 14, 
                               colour = "grey50"),
      legend.title = element_blank(),
      legend.text = element_text(size = 14, 
                                 colour = "grey50")
    )
}

