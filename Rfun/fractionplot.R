subraction.plot <- function(frac1 = c(2,3),
                                      frac2 = c(6, 7),
                                      fill1 = "purple",
                                      fill2 = "purple",
                                      xlab = "",
                                      size = 0.5,
                                      line.color = "white",
                                      ylab = ""){
  
  mat <- data.frame(matrix(0, ncol = frac2[2], nrow = frac1[2]))
  if (frac1[1] > 0){
    for (i in 1:frac1[1]){
      for (j in 1:frac2[1]){
        mat[i,j] <- 1
      }
    }
  }
  mat           <- sapply(mat, as.numeric)
  colnames(mat) <- c(1:frac2[2])
  rownames(mat) <- c(1:frac1[2])
  mat           <- melt(mat)
  
  ggplot()+
    geom_tile(aes(Var2, Var1, fill = as.character(value)),
              color = line.color, 
              size = size, data = mat) +
    scale_fill_manual(values = alpha(c("0" = "purple", "1" = "purple"), 
                                     c(0.3, 1))) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.line.y  = element_blank(), 
          axis.line.x  = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x  = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.text.y  = element_blank()) +
    xlab(xlab) + ylab(ylab)
  
}


plot.simple.fraction <- function(numerator = 3, denominator = 5,
                                 xlab = paste0(numerator,"/",denominator),
                                 ylab = "", ncol = denominator,
                                 nrow = 1, fill = "#18bc9c",
                                 alpha = 1,
                                 line.color = "white", size = 0.5){

    if (numerator > denominator){
        warning("Invalid for not fractions where numerator > denominator")
    }  
  
    if (nrow > 1){
      mat           <- data.frame(matrix(1, ncol = ncol, nrow = nrow))
      mat           <- sapply(mat, as.numeric)
      colnames(mat) <- c(1:ncol)
      rownames(mat) <- c(1:nrow)
      mat           <- melt(mat)
    } else {
      mat           <- data.frame(matrix(1, ncol = ncol, nrow = nrow))
      colnames(mat) <- c(1:ncol)
      rownames(mat) <- c(1:nrow)
      mat           <- melt(mat)
      mat$Var2      <- 1
      colnames(mat) <- c("Var1","Var2", "value")
    }
    
    ggplot()+
      geom_tile(aes(Var2, Var1),
                fill = fill,
                alpha = 0.25,
                color = line.color, 
                size = size, data = mat) + 
      geom_tile(aes(Var2, Var1),
                fill = fill,
                alpha = alpha,
                color = line.color, 
                size = size, 
                data = mat %>% filter(as.numeric(Var1) <= !!numerator)) +
      theme_classic() +
      theme(legend.position = "none", 
            axis.line.y  = element_blank(), 
            axis.line.x  = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.x  = element_blank(),
            axis.ticks.y = element_blank(), 
            axis.text.y  = element_blank()) +
      xlab(xlab) + ylab(ylab)
}


plot.fraction <- function(numerator = 13, denominator = 5,
                          xlab = paste0(numerator,"/",denominator),
                          ylab = "", ncol = denominator,
                          nrow = 1, fill = "#18bc9c",
                          alpha = 1,
                          line.color = "white", size = 0.5){
  
  plot.list <- list()
  
  if (numerator > denominator){
    
    while (numerator > denominator){
    
      frac.plot <- plot.simple.fraction(numerator = denominator,
                                        denominator = denominator,
                                        xlab = paste0(denominator,"/",denominator), 
                                        ylab = ylab,
                                        ncol = ncol, nrow = nrow, 
                                        fill = fill,
                                        alpha = alpha,
                                        line.color = line.color, 
                                        size = size)
      
      #Reduce numerator
      numerator <- numerator - denominator
    
      #Add plot to list
      plot.list <- list.append(plot.list, frac.plot)
    }
    
  } 
  
    #Last plot
    frac.plot  <- 
      plot.simple.fraction(numerator = numerator,
                           denominator = denominator,
                           xlab = paste0(numerator,"/",denominator), 
                           ylab = ylab, alpha = alpha,
                           ncol = ncol, nrow = nrow, fill = fill,
                           line.color = line.color, size = size)
    
    #Add plot to list
    plot.list <- list.append(plot.list, frac.plot)
  
    
    return(plot.list)
  
}

fraction.plot <- function(numerator = 13, denominator = 5,
                      xlab = paste0(numerator,"/",denominator),
                      ylab = "", ncol = denominator,
                      nrow = 1, fill = "#18bc9c",
                      nrowplot = 1, alpha = 1,
                      ncolplot = ceiling(numerator/denominator),
                      line.color = "white", size = 0.5) {
  
  plot.list <- plot.fraction(numerator = numerator,
                             denominator = denominator,
                             xlab = paste0(numerator,"/",denominator), 
                             ylab = ylab,
                             ncol = ncol, nrow = nrow, 
                             fill = fill,
                             alpha = alpha,
                             line.color = line.color, 
                             size = size)
  
  plot_grid(plotlist = plot.list, nrow = nrowplot, 
            ncol = ncolplot)
}

frac.operation <- function(frac1 = c(2,4), 
                           op1   = "+",
                           frac2 = c(1,4),
                           op2   = NULL,
                           frac3 = NULL,
                           color = rep("deepskyblue3",3),
                           widths = "auto"){
  
  if (is.null(frac3)){
    
    if (widths == "auto"){
      widths <- c(ceiling(frac1[1]/frac1[2]), 1, ceiling(frac2[1]/frac2[2]))
    }
    
    plot_grid(fraction.plot(frac1[1],frac1[2], fill = color[1]),
              ggplot() + 
                annotate("text", label = op1, 
                         x = 0, y = 0, 
                         size = 10) + theme_void(),
              fraction.plot(frac2[1],frac2[2], fill = color[2]),
              ncol = 3,
              rel_widths = widths)
    
  } else {
  
    if (widths == "auto"){
      widths <- c(ceiling(frac1[1]/frac1[2]), 1, ceiling(frac2[1]/frac2[2]),
                  1, ceiling(frac3[1]/frac3[2]))
    }
    
    plot_grid(fraction.plot(frac1[1],frac1[2], fill = color[1]),
              ggplot() + 
                annotate("text", label = op1, 
                         x = 0, y = 0, 
                         size = 10) + theme_void(),
              fraction.plot(frac2[1],frac2[2], fill = color[2]),
              ggplot() + 
                annotate("text", label = op2, x = 0, y = 0, 
                         size = 10) + theme_void(),
              fraction.plot(frac3[1],frac3[2], fill = color[3]), ncol = 5,
              rel_widths = widths)
  }
  
}

