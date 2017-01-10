    
    ###Europe
    xmin <- min(fishSubset$x)
    xmax <- max(fishSubset$x)
    ymin <- min(fishSubset$y)
    ymax <- max(fishSubset$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    fishSubset1 <- subset(fishSubset, fishSubset$Element==input$element1)
    fishSubset2 <- subset(fishSubset, fishSubset$Element==input$element2)
    fishSubset3 <- subset(fishSubset, fishSubset$Element==input$element3)


    fish.int.1 <- with(fishSubset1, interp(x=x, y=y, z=counts, duplicate="user", dupfun="min", nx=input$resolution, ny=input$resolution*y.ratio))
    fish.int.melt.1 <- melt(fish.int.1$z, na.rm=TRUE)
    colnames(fish.int.melt.1) <- c("x", "y", "z")
    
    fish.int.melt.1$x <- fish.int.1$x[fish.int.melt.1$x]
    fish.int.melt.1$y <- fish.int.1$y[fish.int.melt.1$y]
    
    fish.int.melt.1[is.na(fish.int.melt.1)] <- 0
    
    
    
    fish.int.2 <- with(fishSubset2, interp(x=x, y=y, z=counts, duplicate="user", dupfun="min", nx=input$resolution, ny=input$resolution*y.ratio))
    fish.int.melt.2 <- melt(fish.int.2$z, na.rm=TRUE)
    colnames(fish.int.melt.2) <- c("x", "y", "z")
    
    fish.int.melt.2$x <- fish.int.2$x[fish.int.melt.2$x]
    fish.int.melt.2$y <- fish.int.2$y[fish.int.melt.2$y]
    
    fish.int.melt.2[is.na(fish.int.melt.2)] <- 0
    
    
    
    fish.int.3 <- with(fishSubset3, interp(x=x, y=y, z=counts, duplicate="user", dupfun="min", nx=input$resolution2, ny=input$resolution2*y.ratio))
    fish.int.melt.3 <- melt(fish.int.3$z, na.rm=TRUE)
    colnames(fish.int.melt.3) <- c("x", "y", "z")
    
    fish.int.melt.3$x <- fish.int.3$x[fish.int.melt.3$x]
    fish.int.melt.3$y <- fish.int.3$y[fish.int.melt.3$y]
    
    fish.int.melt.3[is.na(fish.int.melt.3)] <- 0
    
    melt.x <- c(fish.int.melt.1$x, fish.int.melt.2$x, fish.int.melt.3$x)
    melt.y <- c(fish.int.melt.1$y, fish.int.melt.2$y, fish.int.melt.3$y)
    melt.z <- range01(c(fish.int.melt.1$z*input$element1alpha, fish.int.melt.2$z*input$element2alpha, fish.int.melt.3$z*input$element3alpha))
    melt.names <- c(rep(input$element1, length(melt.x)), rep(input$element2, length(melt.y)), rep(input$element3, length(melt.z)))
    
    melt.frame <- data.frame(melt.x, melt.y, melt.z, melt.names)
    colnames(melt.frame) <- c("x", "y", "z", "Element")


    spectral.mult.int.map <- ggplot(melt.frame) +
    geom_point(aes(jitter(x),jitter(y), colour=Element, fill=Element, alpha=z/sum(z))) +
    scale_colour_manual(values=c(input$element1color, input$element2color, input$element3color)) +
    scale_alpha_continuous(limits=c(0.2, 1)) +
    coord_equal() +
    theme_classic()