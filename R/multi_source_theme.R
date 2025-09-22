#' Custom theme for ggplot multi-source density distributions
#' @description
#' This is not a function but is appended to ggplot objects by ggplot()+multi_source_theme()
#'
multi_source_theme=ggplot2::theme(plot.margin=grid::unit(c(0.1,0.5,0.5,0.1),"cm"),
                         legend.direction = 'horizontal',
                         legend.text =ggplot2::element_text(color='black',size=10))+
  ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
        panel.background=ggplot2::element_blank(),
        plot.title = ggplot2::element_text(color='black',size=15,hjust=0.5))+
  ggplot2::theme(axis.line=ggplot2::element_line(colour="black"),
        axis.text.x=ggplot2::element_text(color="black",size=12,vjust=-2),
        axis.text.y=ggplot2::element_text(color="black",size=12))+
  ggplot2::theme(legend.margin=ggplot2::margin(0,0,0,0,"cm"),legend.position='top',
        legend.key=ggplot2::element_rect("white"),
        legend.title=ggplot2::element_blank())+
  ggplot2::theme(axis.title.y=ggplot2::element_text(size=15),
        axis.title.x = ggplot2::element_text(size=15,vjust=-1))
