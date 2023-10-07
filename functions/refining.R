transform_rci <- function(rcis){
  return((rcis+1)/2)
}

topdown_fa <- function(df, nfactors = 1) {
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  FAplot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                   aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.28, size=5) +
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\n Coefficients de saturation \n", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                       expand = c(0,0)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=15,
                                     margin = margin(r = 5, l = 3)), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 15),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  print(FAplot)
  print("What we want:")
  print(paste0("Alpha de Cronbach > 0.6 -> ",cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 -> ",factor1stEigen))
  print(paste0("Tous les coefficients de saturation > 0.3"))
}

finverser <- function(vec_col){
  unique_col <- unique(vec_col)
  unique_col <- unique_col[!is.na(unique_col)]
  n <- length(unique_col)
  max <- max(vec_col, na.rm = T)
  ord <- sort(as.vector(unique_col))
  rev <- rev(ord)
  for (i in 1:n){
    vec_col[vec_col == ord[i]] <- max + rev[i] 
  }
  vec_col <- vec_col - max
  return(vec_col)
}
