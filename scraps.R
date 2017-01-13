    
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






####For Line


###UI
,

tabPanel("Line",
titlePanel("Multi-element lines"),
sidebarLayout(
sidebarPanel(
fileInput('file2', 'Choose file to upload',
accept = c('.xlsx')
),



uiOutput('inElements'),

tags$hr(),


selectInput(
"colorramp", "Color Ramp",
c("Terrain" = "terrain.colors(",
"Rainbow" = "rainbow(",
"Heat" = "heat.colors(",
"Topo" = "topo.colors(",
"CM" = "cm.colors("),

selected="Terrain"),

sliderInput("colorrampvalues", label = "Steps", value=10, min=2, max=30),

tags$hr(),



checkboxInput('interpolate', "Interpolation"),

sliderInput("resolution", label = "Interpolation Resolution", value=100, min=10, max=1000),


tags$hr()


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("simpleMap", height = 600,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
))


####Server












plotInput3a <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    
    
    unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
    null <- rep(1, length(unique.spec))
    
    
    
    spectra.line.table.norm <- data.frame(spectra.line.table, null)
    colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
    spectra.line.table.norm
    
    interval <- unique.spec*as.numeric(input$intervalmm)
    
    spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
    colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
    
    
    trendy <-  as.vector((if(input$elementnorm=="None") {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
    } else {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
    }))
    
    
    
    
    
    
    
    
    
    
    
    
    
    black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(colour = "black", lwd=input$linesize) +
    theme_light()
    
    smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
    theme_light() +
    stat_smooth()
    
    ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Selected), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
    theme_classic() +
    geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
    scale_x_continuous("Length (mm)") +
    scale_y_continuous(trendy)
    
    
    cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Cluster), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    
    
    if (input$timecolour == "Black") {
        black.time.series
    } else if (input$timecolour == "Smooth") {
        smooth.time.series
    } else if (input$timecolour == "Selected") {
        ramp.time.series
    } else if (input$timecolour == "Cluster") {
        cluster.time.series
    } else if (input$timecolour == "Area") {
        area.time.series
    }
    
    
    
    
})


observeEvent(input$timeseriesact1, {
    
})

output$timeseriesplot1 <- renderPlot({
    input$timeseriesact1
    isolate(print(plotInput3a()))
    
})


output$downloadPlot3a <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput3a(), width=10, height=7)
}
)



plotInput3b <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    
    
    unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
    null <- rep(1, length(unique.spec))
    
    
    
    spectra.line.table.norm <- data.frame(spectra.line.table, null)
    colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
    spectra.line.table.norm
    
    interval <- unique.spec*as.numeric(input$intervalmm)
    
    spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
    colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
    
    
    trendy <-  as.vector((if(input$elementnorm=="None") {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
    } else {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
    }))
    
    
    
    
    
    
    
    
    
    
    
    
    
    black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(colour = "black", lwd=input$linesize) +
    theme_light()
    
    smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
    theme_light() +
    stat_smooth()
    
    ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Selected), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
    theme_classic() +
    geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
    scale_x_continuous("Length (mm)") +
    scale_y_continuous(trendy)
    
    
    cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Cluster), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    
    
    if (input$timecolour == "Black") {
        black.time.series
    } else if (input$timecolour == "Smooth") {
        smooth.time.series
    } else if (input$timecolour == "Selected") {
        ramp.time.series
    } else if (input$timecolour == "Cluster") {
        cluster.time.series
    } else if (input$timecolour == "Area") {
        area.time.series
    }
    
    
    
    
})


observeEvent(input$timeseriesact2, {
    
    
})
output$timeseriesplot2 <- renderPlot({
    input$timeseriesact2
    isolate(print(plotInput3b()))
    
})


output$downloadPlot3b <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput3b(), width=10, height=7)
}
)



plotInput3c <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    
    
    unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
    null <- rep(1, length(unique.spec))
    
    
    
    spectra.line.table.norm <- data.frame(spectra.line.table, null)
    colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
    spectra.line.table.norm
    
    interval <- unique.spec*as.numeric(input$intervalmm)
    
    spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
    colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
    
    
    trendy <-  as.vector((if(input$elementnorm=="None") {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
    } else {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
    }))
    
    
    
    
    
    
    
    
    
    
    
    
    
    black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(colour = "black", lwd=input$linesize) +
    theme_light()
    
    smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
    theme_light() +
    stat_smooth()
    
    ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Selected), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
    theme_classic() +
    geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
    scale_x_continuous("Length (mm)") +
    scale_y_continuous(trendy)
    
    
    cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Cluster), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    
    
    if (input$timecolour == "Black") {
        black.time.series
    } else if (input$timecolour == "Smooth") {
        smooth.time.series
    } else if (input$timecolour == "Selected") {
        ramp.time.series
    } else if (input$timecolour == "Cluster") {
        cluster.time.series
    } else if (input$timecolour == "Area") {
        area.time.series
    }
    
    
    
    
})

observeEvent(input$timeseriesact3, {
})

output$timeseriesplot3 <- renderPlot({
    input$timeseriesact3
    
    isolate(print(plotInput3c()))
    
    
})

output$downloadPlot3c <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput3c(), width=10, height=7)
}
)


plotInput3d <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    
    
    unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
    null <- rep(1, length(unique.spec))
    
    
    
    spectra.line.table.norm <- data.frame(spectra.line.table, null)
    colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
    spectra.line.table.norm
    
    interval <- unique.spec*as.numeric(input$intervalmm)
    
    spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
    colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
    
    
    trendy <-  as.vector((if(input$elementnorm=="None") {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
    } else {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
    }))
    
    
    
    
    
    
    
    
    
    
    
    
    
    black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(colour = "black", lwd=input$linesize) +
    theme_light()
    
    smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
    theme_light() +
    stat_smooth()
    
    ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Selected), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
    theme_classic() +
    geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
    scale_x_continuous("Length (mm)") +
    scale_y_continuous(trendy)
    
    
    cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Cluster), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    
    
    if (input$timecolour == "Black") {
        black.time.series
    } else if (input$timecolour == "Smooth") {
        smooth.time.series
    } else if (input$timecolour == "Selected") {
        ramp.time.series
    } else if (input$timecolour == "Cluster") {
        cluster.time.series
    } else if (input$timecolour == "Area") {
        area.time.series
    }
    
    
    
    
})

observeEvent(input$timeseriesact4, {
    
})



output$timeseriesplot4 <- renderPlot({
    input$timeseriesact4
    isolate(print(plotInput3d()))
    
})

output$downloadPlot3d <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput3d(), width=10, height=7)
}
)





plotInput3e <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    
    
    unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
    null <- rep(1, length(unique.spec))
    
    
    
    spectra.line.table.norm <- data.frame(spectra.line.table, null)
    colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
    spectra.line.table.norm
    
    interval <- unique.spec*as.numeric(input$intervalmm)
    
    spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
    colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
    
    
    trendy <-  as.vector((if(input$elementnorm=="None") {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
    } else {
        paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
    }))
    
    
    
    
    
    
    
    
    
    
    
    
    
    black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(colour = "black", lwd=input$linesize) +
    theme_light()
    
    smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
    theme_light() +
    stat_smooth()
    
    ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Selected), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
    theme_classic() +
    geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
    scale_x_continuous("Length (mm)") +
    scale_y_continuous(trendy)
    
    
    cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
    geom_line(aes(colour = Cluster), lwd=input$linesize) +
    theme_light() +
    scale_colour_gradientn(colours=rainbow(7))
    
    
    
    if (input$timecolour == "Black") {
        black.time.series
    } else if (input$timecolour == "Smooth") {
        smooth.time.series
    } else if (input$timecolour == "Selected") {
        ramp.time.series
    } else if (input$timecolour == "Cluster") {
        cluster.time.series
    } else if (input$timecolour == "Area") {
        area.time.series
    }
    
    
    
    
})

observeEvent(input$timeseriesact5, {
    
    
})

output$timeseriesplot5 <- renderPlot({
    input$timeseriesact5
    
    
    
    isolate(print(plotInput3e()))
    
})


output$downloadPlot3e <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput3e(), width=10, height=7)
}
)

