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


output$downloadmap <- downloadHandler(
filename = function() { paste(input$dataset, '.tiff', sep='') },
content = function(file) {
    ggsave(file,plotInput(), width=7, height=7, dpi=300, device="tiff")
}


)




})






