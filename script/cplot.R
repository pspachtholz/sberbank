cplot <- function(data, x, y ){
  require(grid)
  summ <- paste0('mean(', y, ',na.rm=T)')  # construct summary method, e.g. mean(mpg)
  summ2 <- 'n()'
  summ_names <- c(paste0('m'),paste0('n'))  # construct summary variable name, e.g. mean_mpg
  
  meandat <- data %>% group_by_(.dots = x) %>% summarize_(.dots = setNames(list(summ,summ2),summ_names))
  meandat[[x]] <- as.factor(meandat[[x]])
  meandat$num <- as.numeric(as.factor(meandat[[x]]))
  meandat$num <- meandat$num - 0.3
  
  plotlims <- quantile(data[[y]],c(0.025,0.975))
  
  p1 <- ggplot(data,aes_string(x = x, y = y,fill=x,color=x))+
    geom_violin(size=1,alpha=0.1, width=1.1)+
    geom_pointrange(stat="summary",fun.data="mean_se", size=1)+
    geom_text(mapping=aes_string(x="num",y=plotlims[2],label="n"),data=meandat, hjust="center")+
    scale_y_continuous(limits=plotlims)+
    theme_bw()+
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=14),
          axis.ticks.x = element_blank())
  
  p2 <- ggplot(data,aes_string(x = x, y = y,fill=x,color=x))+
    geom_errorbar(stat="summary",fun.data="mean_se", size=1, width=0.7)+
    geom_point(stat="summary",fun.data="mean_se", size=4)+
    geom_abline(data=meandat,mapping=aes_string(color=x,intercept=meandat$m,slope=0),linetype=2, alpha=0.5)+
    theme_bw()+
    theme(legend.position="none",
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank())
  
  gA=ggplot_gtable(ggplot_build(p1))
  gB=ggplot_gtable(ggplot_build(p2))
  maxWidth = grid::unit.pmax(gA$widths, gB$widths)
  gA$widths <- as.list(maxWidth)
  gB$widths <- as.list(maxWidth)
  grid.newpage()
  
  grid.arrange(
    arrangeGrob(gA,gB,nrow=2,heights=c(.6,.5))
  )
}
