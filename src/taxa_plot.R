pm = -0.8
taxa_plot <- function(mot) {
  # make a color palette
  n.colors <- length(unique(mot[,1]))
  my.colors <- colorRampPalette(brewer.pal(9, "Set1"))(n.colors)
  
  # make the base plot
  p <- ggplot(mot, aes(x=factor(variable), y=value)) +
    geom_bar(stat='identity', aes(fill=mot[,1])) +
    scale_fill_manual(values=my.colors)
  
  # beautify the plot with a stripped-down theme and nicer limits & labels
  p +
    theme_bw() +
    ylim(0, 1.01) +
    xlab("sample") +
    #ylab("relative abundance") +
    theme(legend.key.size=unit(0.1, 'inches'),
          legend.text=element_text(size=rel(0.5)),
          #axis.text.x = element_blank(),
          #axis.text.x = element_text(angle = 0, hjust = 0.5),
          #axis.text.y = element_blank(),
          #axis.text.y = element_text(size = rel(0.7)),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(mot$variable))) +
    guides(fill=guide_legend(title = names(mot)[1]))+
    theme(plot.title = element_text(size=rel(0.8))) +
    theme(plot.title = element_text(hjust = 0,vjust = 1, margin = margin(b=0, unit = "pt"))) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = rel(0.7)),
          axis.text.y = element_text(size = rel(0.7)))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = rel(0.7)),
          #axis.text.y = element_blank(),
          plot.margin = margin(l=-0.8, unit="cm"),
          axis.title=element_text(size=rel(0.8)))#+
    #rremove("legend")
}