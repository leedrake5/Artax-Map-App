library(shiny)
library(ggplot2)
library(reshape2)
library(pbapply)
library(TTR)
library(DT)
library(pvclust)
library(data.table)
library(pbapply)
library(R.utils)
library(R.oo)
library(Biobase)
library(plyr)

library(ggplot2)
library(xlsx)
library(reshape2)
library(pbapply)
library(akima)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    


fishList <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    #if (is.null(inFile)) {return(NULL)}
    
    
    
    proto.fish <- loadWorkbook(file=inFile$datapath)
    fish.sheets <- getSheets(proto.fish)
    fish.names <- as.vector(names(fish.sheets))
    
    fishes <- pblapply(fish.names, function(x) read.xlsx(file=inFile$datapath, sheetIndex=x))
    names(fishes) <- fish.names
    parameters <- fishes[[1]]
    fishes[[1]] <- NULL
    
    fish.melt <- melt(fishes, id="NA.")
    
    fish.melt$variable <- as.numeric(substring(fish.melt$variable, 2))
    colnames(fish.melt) <- c("y", "x", "counts", "Element")

    
    fish.melt
    

})

#data.m <- metadataTableRe()

#if (is.null(data.m)){ data.m <- ceramics}




outElements <- reactive({
    metadata.dat <- fishList()
    
    element.names <- unique(metadata.dat$Element)
    
    element.names
    
    
})



output$inElements <- renderUI({
    selectInput(inputId = "elements", label = h4("Choose Element Line"), choices =  outElements())
})


output$inElement1 <- renderUI({
    selectInput(inputId = "element1", label = h4("Choose First Element Line"), choices =  outElements())
})



output$inElement2 <- renderUI({
    selectInput(inputId = "element2", label = h4("Choose Second Element Line"), choices =  outElements())
})



output$inElement3 <- renderUI({
    selectInput(inputId = "element3", label = h4("Choose Third Element Line"), choices =  outElements())
})




plotInput <- reactive({
    
    colvals = as.character(paste(input$colorramp, input$colorrampvalues, ")", sep="", collapse=""))
    
    fishImport <- fishList()
    
    fishSubset <- subset(fishImport, fishImport$Element==input$elements)
    
    spectral.map <- ggplot(fishSubset) +
    geom_tile(aes(x, y, colour=counts, fill=counts)) +
    scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    coord_equal() +
    theme_classic()
    
    
    
    ###Europe
    xmin <- min(fishSubset$x)
    xmax <- max(fishSubset$x)
    ymin <- min(fishSubset$y)
    ymax <- max(fishSubset$y)
    
    x.range <- xmax-xmin
    y.range <- ymax-ymin
    
    y.ratio <- y.range/x.range
    
    fish.int <- with(fishSubset, interp(x=x, y=y, z=counts, duplicate="user", dupfun="min", nx=input$resolution, ny=input$resolution*y.ratio))
    fish.int.melt <- melt(fish.int$z, na.rm=TRUE)
    colnames(fish.int.melt) <- c("x", "y", "z")
    
    fish.int.melt$x <- fish.int$x[fish.int.melt$x]
    fish.int.melt$y <- fish.int$y[fish.int.melt$y]
    
    fish.int.melt[is.na(fish.int.melt)] <- 0
    
    
    spectral.int.map <- ggplot(fish.int.melt) +
    geom_tile(aes(x, y, colour=z, fill=z)) +
    scale_colour_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    scale_fill_gradientn("Net Counts", colours=eval(parse(text=paste(colvals)))) +
    coord_equal() +
    theme_classic()
    
    
    
    if (input$interpolate == FALSE) {
        spectral.map
    } else if (input$interpolate == TRUE) {
        spectral.int.map
    }






})


output$simpleMap <- renderPlot({
    
    print(plotInput())
    
    








 })





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








})






