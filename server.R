shinyServer(function(input, output) {

  calculus <- reactive({
    who <<- covidExp(dfcovv, input$m, input$cutttL, input$cutttU, datee, 100)
    
  })
    
  
  ##WHO
    output$plot1who <- renderPlot({
     calculus()
     
     modelpaper <- covidExp(dfcovv, 15, "2020-03-03", "2020-03-22", datee, 100)
     
     dat <- who$dataCut[ ,c('report', 'all', 'd')]
     dfF <- who$dfFitted[ ,c('report', 'allFitted', 'd')]
     names(dfF)[2] <- 'all'
     dfcovPred <- rbind(dfF, who$dfPred)

     dat1 <- dplyr::bind_rows(dfcovPred, dat,  .id = 'Legend')
     dat1$size <- dat1$Legend
     
     
     p22 <- ggplot(data=dat1, aes(x = report, y= all, col=Legend)) +
        geom_line(aes(col = Legend)) +
        geom_point(aes(col = Legend, shape = Legend), alpha = 0.8) +
        #geom_hline(yintercept = 1000000,  col = 'red', alpha = 0.5) +
        geom_vline(xintercept = tail(dat$report, 1),  col = 'blue', alpha = 0.5) +
        geom_vline(xintercept = head(dat$report, 1),  col = 'blue', alpha = 0.5) +
        theme_minimal() + theme(legend.position="none") +
        scale_y_continuous(name="confirmed cases outside China", labels = scales::comma) +
        xlab('WHO situation report day') +
        theme(axis.text.x=element_text(size=rel(1.3)),
              axis.text.y=element_text(size=rel(1.3)),
              axis.title.x=element_text(size=rel(1.3)),
              axis.title.y=element_text(size=rel(1.3))) +
        geom_label(x = tail(dat$report, 1) - 1.5, y = max(dat1$all),
                   label = tail(dat$d, 1), col = 'blue') +
        geom_label(x = head(dat$report, 1) + 1.5, y = max(dat1$all),
                   label = head(dat$d, 1), col = 'blue') +
       scale_color_grey(start = 0.8, end = 0.2) 
     
     
     p23 <- p22 + geom_hline(yintercept = 1000000,  col = 'red', alpha = 0.5)
       
   
     if (input$redline == TRUE) p23 else p22  
      
    
    })
    
    
    output$diffplotwho <- renderPlot({
     calculus()
      tab <- data.frame(who$dfFitted, diff = who$error)
      tab$norm <- (tab$diff - mean(tab$diff))/sd(tab$diff)
      tab$stand <- (tab$diff - mean(tab$diff))/sd(tab$diff - mean(tab$diff))
      
      p3 <- ggplot(tab, aes(x = report, y = stand)) +
        geom_point(col = 'black') +
        geom_line() +
        geom_vline(xintercept =  head(tab$report, 1),  col = 'blue', alpha = 0.5) +
        geom_vline(xintercept =  tail(tab$report, 1), col = 'blue', alpha = 0.5) +
        geom_area(fill = scales::muted('lightgray'), alpha = 0.2)+
        ylab(' standardized residuals') +
        xlab('WHO situation report day') + theme_minimal() +
        theme(axis.text.x=element_text(size=rel(1.3)), 
              axis.text.y=element_text(size=rel(1.3)),
              axis.title.x=element_text(size=rel(1.3)), 
              axis.title.y=element_text(size=rel(1.3)))+
        geom_label(x = tail(tab$d, 1) - 1, y = max(tab$stand),
                   label = tail(tab$d, 1), col = 'blue') +
        geom_label(x = head(who$dfFitted$d, 1) + 1, y = max(tab$stand),
                   label = head(tab$d, 1), col = 'blue')


      p31 <- ggplot(tab, aes(x = report, y = stand)) +
        geom_point(col = 'black', size = 1.6) +
        #geom_line() +
        geom_hline(yintercept =  0) +
        geom_vline(xintercept =  head(tab$report, 1),  col = 'blue', alpha = 0.5) +
        geom_vline(xintercept =  tail(tab$report, 1), col = 'blue', alpha = 0.5) +
        #geom_area(fill = scales::muted('lightgray'), alpha = 0.2)+
        ylab(' standardized residuals') +
        xlab('WHO situation report day') + theme_minimal() +
        theme(axis.text.x=element_text(size=rel(1.3)), 
              axis.text.y=element_text(size=rel(1.3)),
              axis.title.x=element_text(size=rel(1.3)), 
              axis.title.y=element_text(size=rel(1.3)))+
        geom_label(x = tail(tab$d, 1) - 1, y = max(tab$stand),
                   label = tail(tab$d, 1), col = 'blue') +
        geom_label(x = head(who$dfFitted$d, 1) + 1, y = max(tab$stand),
                   label = head(tab$d, 1), col = 'blue')
      
      
       if (input$resid == TRUE) p3 else p31 
      
        
    })
    
    ###tables
    output$model <- renderPrint({
      calculus()
      summary(who$modE)
    })
    
    
    output$dfP <- renderDT({
      calculus()
      tab <- who$dfPred[ ,c('report', 'd', 'all')]
      tab$all <- format(tab$all, big.mark=',', scientific=FALSE)
       datatable(tab,
                  style = 'bootstrap',
                  rownames = F,
                  class = 'cell-border stripe',
                  #extensions = c('Responsive'),
                  options = list(pageLength = nrow(tab),
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = '_all')),
                    autoWidth = T,
                    ordering = F))
    })
    
    output$dfF <- renderDT({
      calculus()
      tab <- who$dfFitted[ ,c('report', 'd', 'all', 'allFitted')] %>% arrange(desc(report))
      tab$all <- format(tab$all, big.mark=',', scientific=FALSE)
      tab$allFitted <- format(tab$allFitted, big.mark=',', scientific=FALSE)
      
      datatable(tab,
                style = 'bootstrap',
                rownames = F,
                class = 'cell-border stripe',
                #extensions = c('Responsive'),
                options = list(pageLength = nrow(tab),
                  dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  autoWidth = T,
                  ordering = T))
    })
    
    output$dfcov <- renderDT({
      
      dfcovv$all <- format(dfcovv$all, big.mark=',', scientific=FALSE) 
      
      datatable(dfcovv %>% arrange(desc(report)),
                # #style = 'bootstrap',
                rownames = F,
                # #class = 'cell-border stripe',
                # #extensions = c('Responsive'),
                options = list(
                  pageLength = 25,
                              #dom = 't',
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))
                               #autoWidth = T
                               #ordering = T
                               ))
    })
    
    output$dat <- renderDT({
      
      tab <- data.frame(ReportDay = datee$report, Date = as.Date(datee$d)) %>% arrange(desc(ReportDay))
      
      datatable(tab, 
                rownames = F,
                # #class = 'cell-border stripe',
                # #extensions = c('Responsive'),
                options = list(
                  #pageLength = 25,
                  #dom = 't',
                  columnDefs = list(list(className = 'dt-center', targets = '_all'))
                  #autoWidth = T
                  #ordering = T
                ))
      
    })
    
    
})
