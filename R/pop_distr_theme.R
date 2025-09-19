#' Custom theme for ggplot overall density distributions
#' @description
#' This is not a function but is appended to ggplot objects by ggplot()+pop_distr_theme()
pop_distr_theme=theme(plot.margin=unit(c(0.1,0.5,0.5,0.1),"cm"),
                      legend.direction = 'horizontal',
                      legend.text =element_text(color='black',size=12))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.title = element_text(color='black',size=15,hjust=0.5))+
  theme(axis.line=element_line(colour="black"),
        axis.text.x=element_text(color="black",size=12,vjust=-2),
        axis.text.y=element_text(color="black",size=12))+
  theme(legend.margin=margin(0,0,0,0,"cm"),legend.position='top',
        legend.key=element_rect("white"),
        legend.title=element_text(color='black',size=14))+
  theme(axis.title.y=element_text(size=15),
        axis.title.x = element_text(size=15,vjust=-1))
