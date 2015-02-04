## Plot results
img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
theme_set(theme_gray(base_size = 15))
market_impact <- 
  ggplot(sfret,aes(x=k,colour=class))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(plot.background = element_rect(fill = "#C0C0C0", colour = "#111111"),
        axis.text = element_text(colour="black"), 
        legend.background = element_rect(fill = "#A0A0A0", colour = "#333333"),
        panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 180, by=30),linetype="dotted",colour="#00FFFF")+
  geom_hline(yintercept=seq(-6*10^-5, 2*10^-4, by=5e-05),linetype="dotted",colour="#0088D8")+
  geom_line(aes(y=avg),size=0.8)+
  xlab("Time (in sec)") +
  ylab("Relative price change (Pt%P0)") +
  ggtitle("Price impact for CL contracts in 2014")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=class), alpha=0.3, colour=NA)+
  #geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
  #                ymin=ci.d(avg,sigma/sqrt(N),0.99),
  #                fill=class), alpha=0.3, colour=NA)
  
  scale_fill_manual(values=c("#FFFF00","#FF3333","#00FF00","#0055FF"))+
  scale_color_manual(values=c("#FFFF00","#FF3333","#00FF00","#0055FF"))

market_impact
ggsave(market_impact,file="MIplots/algo+size.jpg",height=10,width=15)
