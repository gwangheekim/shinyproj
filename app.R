

# plist = c("shiny","plotly","lsirm12pl","data.table",
#           "DT","spatstat","Rcpp","bslib","MCMCpack","networkD3",
#           "igraph","shinyWidgets","shinyalert","reshape2")
# 
# for(i in 1:length(plist)){
#   if(!(plist[i] %in% installed.packages())){
#     install.packages(plist[i],repos='http://cran.us.r-project.org')
#   }else{
#     require(plist[i],character.only = TRUE)
#   }
# }

require(reshape2)
require(shiny)
require(plotly)
require(lsirm12pl)
require(data.table)
require(DT)
require(spatstat)
require(Rcpp)
require(bslib)
require(MCMCpack)
require(networkD3)
require(igraph)
require(shinyWidgets)
require(shinyalert)



options(shiny.maxRequestSize=30*1024^2)


ui <- shinyUI(
  navbarPage(
    titlePanel(title=div(img(src="logo.png", height="70px", width="70px"),
                         "LSIRM R Shiny App")),
    theme = bs_theme(version = 4, bootswatch = "minty"),
    
    tabPanel(title=HTML("Introduction"),
             includeHTML("./html/introduction.html")
    ),
    tabPanel(title=HTML(paste("STEP1", "Data upload & Visualization", sep="<br/>")),
             sidebarLayout(
               sidebarPanel(
                 includeHTML("./html/step1.html"),
                 fileInput("data", "Upload Data", accept = ".txt"),
                 selectInput("sample", "Sample Data", c("sample1" = "samp1",
                                                        "sample2" = "samp2",
                                                        "sample3" = "samp3"))
               ),
               mainPanel(
                 h3('Data Visualization : '),
                 forceNetworkOutput("force"),
                 actionButton("vizreset", "Reset"),
                 # visNetworkOutput("image1"),
                 h3('Network Statistic : '),
                 DT::dataTableOutput("table3")
               )
             )
    ),
    tabPanel(title=HTML(paste("STEP2", "LSIRM fitting", sep="<br/>")),
             sidebarLayout(
               sidebarPanel(
                 includeHTML("./html/step2.html"),
                 # h5("Num of Iteration:"),
                 # numericInput("iter",NULL, min = 1, max = Inf, value = 15000),
                 h5("Variable Selection:"),
                 checkboxInput("ss", "Spike and Slab", F),
                 h5("Missing Mechanism:"),
                 radioButtons("mis", NULL,
                              choices = c(MAR = "mar",
                                          MCAR = "mcar",
                                          Default = "none"),
                              selected = ","),
                 actionButton("do","Start"),
                 h4("Sample Result: "),
                 actionButton("samplsirm","Sample",style="background-color: skyblue; border-color: skyblue")
               ),
               mainPanel(
                 h3('Model Summary : '),
                 verbatimTextOutput("sum"),
                 h3('Interaction map : '),
                 plotlyOutput("fig2", height = 500L, width = 600L)
               )
             ))
    ,
    tabPanel(title=HTML(paste("STEP3", "Model Diagnostic", sep="<br/>")),
             sidebarLayout(
               sidebarPanel(
                 includeHTML("./html/step3.html"),
                 uiOutput("blankmessage"),
                 uiOutput("betanum2")
               ),
               mainPanel(
                 h3('Beta Diagnostic : '),
                 plotOutput("beta", height = 300, width = 800L),
                 h3('Gamma Diagnostic : '),
                 plotOutput("gamma", height = 300, width = 800L),
                 h3('ROC curve : '),
                 plotOutput("figroc", height = 300L, width = 300L)
               )
             )),
    tabPanel(title=HTML(paste("STEP4", "Clustering", sep="<br/>")),
             sidebarLayout(
               sidebarPanel(
                 includeHTML("./html/step4.html"),
                 h4("Number of BIC Iteration: "),
                 numericInput("bicnum",NULL,value=10, min=10, max=1000),
                 actionButton("doclust","Start"),
                 uiOutput("num"),
                 h4('Optimal number of Cluster: '),
                 DT::dataTableOutput("table2"),
                 h4("Sample Result: "),
                 actionButton("sampclust","Sample",style="background-color: skyblue; border-color: skyblue")
               ),
               
               mainPanel(
                 h3('Clustering Visualization : '),
                 plotlyOutput("fig3", height = 500L, width = 500L),
                 h3('Clustering Result Table: '),
                 DTOutput("table1")
               )
             )
    ),
    tabPanel(title=HTML(paste("About", "Us", sep="<br/>")),
             # includeHTML("about.html")
             htmlOutput("info")
    )
    
  )
)


server <- function(input, output, session){
  isolate({load("./test.RData")})
  output$info <- renderUI(
    tryCatch(
      {
        includeHTML("./html/about.html")  
      },
      error = function(e){
        return()
      },
      warning = function(w){
        return()
      }
    )
  )
  getwd()
  ldata <- reactive({
    
    if(is.null(input$data)){
      df = read.table(paste0("data/",input$sample,".txt"))
    }else{
      req(input$data)
      tryCatch(
        {
          df <- read.table(input$data$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
    return(df)
  })
  
  
  draw_graph = function(df){
    G1 = data.frame(name=unique(row.names(df)))
    G1$size = 1
    G2 = data.frame(name=unique(colnames(df)))
    G2$size = 10
    G1$group = "respondent"
    G2$group = "item"
    node = rbind(G1,G2)
    node$pos = seq(0,nrow(node)-1)
    
    E = reshape2::melt(as.matrix(df))
    edge = data.frame(source = E$Var1,
                      target = E$Var2)
    
    link = data.frame(source = merge(edge, node, by.x="source",
                                     by.y="name",all.x=T)$pos,
                      target = merge(edge, node, by.x="target",
                                     by.y="name",all.x=T)$pos)
    link = link[!duplicated(link),]
    
    MyClickScript <- 'alert("You clicked " + d.name);'
    forceNetwork(Links = link,
                 Nodes = node, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group',
                 zoom=T,
                 height = "1200px",
                 linkDistance = 50,
                 linkWidth = 1,
                 Nodesize = "size",
                 opacity = 0.8,
                 opacityNoHover = 0.7, 
                 fontSize = 17,
                 clickAction = MyClickScript,
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 legend=T,
                 bounded = F # restrict draw range
    )
  }
  # force.alpha(1); force.restart(); 
  
  output$force <- renderForceNetwork({
    df = ldata()
    return(draw_graph(df))
  })
  
  # network draw reset
  observeEvent(input$vizreset,{
    output$force <- renderForceNetwork({
      df = ldata()
      return(draw_graph(df))
    })
  })
  
  output$table3 <- DT::renderDataTable({
    
    file = ldata()
    nsamp <- nrow(file)
    nitem <- ncol(file)
    
    datatable(data.frame("Number of Respondent" = nsamp, "Number of Item" = nitem),
              options = list(dom = 't'))
    
    
  })
  
  storage <- reactiveValues()
  
  observeEvent(input$samplsirm,{
    # model summary
    output$sum <- renderPrint({
      print(summary(lsirm_result))
    })
    
    # beta index slider
    output$betanum2 = renderUI({
      sliderInput("betanum",
                  "Beta Index:",
                  min = 1,
                  max = ncol(lsirm_result$data),
                  value = 1,
                  animate = T,
                  step=1)
    })
    
    # diagnostic plot
    output$beta = renderPlot({
      par(mfrow=c(1,3))
      # lsirmr <- lsirm_result
      beta = lsirm_result$beta
      #### ACF
      draw.item.beta <- input$betanum
      ACF = acf(beta[,draw.item.beta], plot=F)
      acf.val = ACF
      acf.print = setNames(drop(ACF$acf), format(drop(ACF$lag), digits = 3L))
      ## Draw plot
      mcmc.beta <- as.mcmc(beta)
      mcmc.sample.beta = mcmc.beta[,draw.item.beta]
      ts.plot(mcmc.sample.beta, ylab="Beta", main = "Trace Plot")
      plot(acf.val, main = "")
      title("ACF", line = 1.6)
      plot(mcmc.sample.beta, density = T, trace = F,
           main = "Density", auto.layout = F)
      mtext(paste0("Beta ", draw.item.beta),line=0.5,
            side=3,outer=TRUE, cex = 1.5) # add title
      par(ask = dev.interactive())
    })
    
    output$gamma = renderPlot({
      par(mfrow=c(1,3))
      gamma = lsirm_result$gamma
      #### ACF
      draw.item.beta <- input$betanum
      acf.val = acf(gamma,plot=F)
      acf.print = setNames(drop(acf.val$acf), format(drop(acf.val$lag),
                                                     digits = 3L))
      ## Draw plot
      mcmc.gamma <- as.mcmc(gamma)
      ts.plot(mcmc.gamma, ylab="Gamma", main = "Trace Plot")
      plot(acf.val, main = "")
      title("ACF", line = 1.6)
      plot(mcmc.gamma, density = T, trace = F,
           main = "Density", auto.layout = F)
      mtext("Gamma",line=0.5,side=3,outer=TRUE, cex = 1.5)
      par(ask = dev.interactive())
    })
    
    
    output$figroc = renderPlot({
      lsirmr <- lsirm_result
      ### ROC
      roc_1pl(lsirmr)
    })
    
    
    # interaction plot
    output$fig2 <- renderPlotly({
      z = lsirm_result$z_estimate
      w = lsirm_result$w_estimate
      
      notation = c('w','z')
      df1=as.data.frame(z)
      df2=as.data.frame(w)
      colnames(df1)=c('axis1','axis2')
      colnames(df2)=c('axis1','axis2')
      df = rbind(df1,df2)
      max_coordinate = sapply(df[,c(1,2)], max, na.rm = TRUE)
      min_coordinate = sapply(df[,c(1,2)], min, na.rm = TRUE)
      axis_value = max(abs(c(max_coordinate,min_coordinate)))
      axis_range = c(-axis_value,axis_value)*1.1
      
      plot_ly(x = df1$axis1,y = df1$axis2) %>%
        add_markers(type = 'scatter',
                    mode = 'markers',
                    text = paste("respondent ", 1:nrow(df1), sep = ""),
                    name = "Respondent") %>%
        add_text(x = df2$axis1, y = df2$axis2 + 0.1, type = 'scatter',
                 name = "Item",
                 text = 1:nrow(df2),
                 textfont = list(family="Arial Black",size=20,weight="bold"))
      
    })
    
  })
  
  
  observeEvent(input$do,{
    
    if(is.null(input$mis)){
      storage$res_lsirm = lsirm1pl(data=ldata(), spikenslab = input$ss)
    }else{
      if(input$mis == "none"){
        storage$res_lsirm = lsirm1pl(data=ldata(), spikenslab = input$ss,
                                     missing_data = NA)
      }else{
        storage$res_lsirm = lsirm1pl(data=ldata(), spikenslab = input$ss,
                                     missing_data = input$mis)  
      }
    }
    
    # model summary
    output$sum <- renderPrint({
      print(summary(storage$res_lsirm))
    })
    
    # beta index slider
    output$betanum2 = renderUI({
      sliderInput("betanum",
                  "Beta Index:",
                  min = 1,
                  max = ncol(ldata()),
                  value = 1,
                  animate = T,
                  step=1)
    })
    
    # diagnostic plot
    output$beta = renderPlot({
      par(mfrow=c(1,3))
      # lsirmr <- storage$res_lsirm
      beta = storage$res_lsirm$beta
      #### ACF
      draw.item.beta <- input$betanum
      ACF = acf(beta[,draw.item.beta], plot=F)
      acf.val = ACF
      acf.print = setNames(drop(ACF$acf), format(drop(ACF$lag), digits = 3L))
      ## Draw plot
      mcmc.beta <- as.mcmc(beta)
      mcmc.sample.beta = mcmc.beta[,draw.item.beta]
      ts.plot(mcmc.sample.beta, ylab="Beta", main = "Trace Plot")
      plot(acf.val, main = "")
      title("ACF", line = 1.6)
      plot(mcmc.sample.beta, density = T, trace = F,
           main = "Density", auto.layout = F)
      mtext(paste0("Beta ", draw.item.beta),line=0.5,
            side=3,outer=TRUE, cex = 1.5) # add title
      par(ask = dev.interactive())
    })
    
    output$gamma = renderPlot({
      par(mfrow=c(1,3))
      gamma = storage$res_lsirm$gamma
      #### ACF
      draw.item.beta <- input$betanum
      acf.val = acf(gamma,plot=F)
      acf.print = setNames(drop(acf.val$acf), format(drop(acf.val$lag),
                                                     digits = 3L))
      ## Draw plot
      mcmc.gamma <- as.mcmc(gamma)
      ts.plot(mcmc.gamma, ylab="Gamma", main = "Trace Plot")
      plot(acf.val, main = "")
      title("ACF", line = 1.6)
      plot(mcmc.gamma, density = T, trace = F,
           main = "Density", auto.layout = F)
      mtext("Gamma",line=0.5,side=3,outer=TRUE, cex = 1.5)
      par(ask = dev.interactive())
    })
    
    
    output$figroc = renderPlot({
      lsirmr <- storage$res_lsirm
      ### ROC
      roc_1pl(lsirmr)
    })
    
    
    # interaction plot
    output$fig2 <- renderPlotly({
      z = storage$res_lsirm$z_estimate
      w = storage$res_lsirm$w_estimate
      
      notation = c('w','z')
      df1=as.data.frame(z)
      df2=as.data.frame(w)
      colnames(df1)=c('axis1','axis2')
      colnames(df2)=c('axis1','axis2')
      df = rbind(df1,df2)
      max_coordinate = sapply(df[,c(1,2)], max, na.rm = TRUE)
      min_coordinate = sapply(df[,c(1,2)], min, na.rm = TRUE)
      axis_value = max(abs(c(max_coordinate,min_coordinate)))
      axis_range = c(-axis_value,axis_value)*1.1
      
      plot_ly(x = df1$axis1,y = df1$axis2) %>%
        add_markers(type = 'scatter',
                    mode = 'markers',
                    text = paste("respondent ", 1:nrow(df1), sep = ""),
                    name = "Respondent") %>%
        add_text(x = df2$axis1, y = df2$axis2 + 0.1, type = 'scatter',
                 name = "Item",
                 text = 1:nrow(df2),
                 textfont = list(family="Arial Black",size=20,weight="bold"))
      
    })
    
    
  })
  

  ###################################
  # Clustering
  ##################################3
  
  storage2 <- reactiveValues()
  
  # make clustering result
  observeEvent(input$doclust,{
    storage2$Niter = input$bicnum # num of BIC iteration
    if(is.null(storage$res_lsirm)){
      # set popup message
      shinyalert("Model Not found",
                 "Fitting LSIRM Model first",
                 type="error")
    }else{
      
      if(!is.null(ldata())){
        z.samps1 = storage$res_lsirm$z_estimate
        w.samps1 = storage$res_lsirm$w_estimate
      }
      
      W <- owin(xrange=c(0,1), yrange=c(0,1))
      
      # Normalizing the w
      x <- (w.samps1[,1] - min(w.samps1[,1])) / (max(w.samps1[,1]) - min(w.samps1[,1]))
      y <- (w.samps1[,2] - min(w.samps1[,2])) / (max(w.samps1[,2]) - min(w.samps1[,2]))
      w.post1 <- data.frame(cbind(x, y))
      colnames(w.post1) <- c('x', 'y')
      
      # Normalizing the z
      zx <- (z.samps1[,1] - min(z.samps1[,1])) / (max(z.samps1[,1]) - min(z.samps1[,1]))
      zy <- (z.samps1[,2] - min(z.samps1[,2])) / (max(z.samps1[,2]) - min(z.samps1[,2]))
      z.post1 <- data.frame(cbind(zx, zy))
      colnames(z.post1) <- c('x', 'y')
      
      xlim <- W$xrange; ylim <- W$yrange
      AreaW <- 1 # |S| area of the interaction map domain
      
      U <- w.post1
      x <- U[,1]
      y <- U[,2]
      X <- t(rbind(x, y))
      
      # alpha: an expected number of items for each group center ci
      # omega: controls the range of item groups in the interaction map
      
      salpha <- 0.1; somega <- 0.015
      sd_alpha <- 0.1; sd_omega <- 0.015
      
      parent <- list()
      parentnum <- c()
      accept <- c()
      logllh <- c()
      alphalist <- list()
      kappalist <- list()
      omegalist <- list()
      
      withProgress(message = "Clustering...", value=0,{
        for (i in 1:storage2$Niter){
          # Update the alpha, omega, CC (Thomas process fitting procedures)
          Thomas<-MCMCestThomas(X, xlim, ylim, NStep=25000, DiscardStep=5000, Jump=5)
          
          # Summarize the result of Thomas process fitting process
          CC <- Thomas$CC
          omega <- Thomas$Omegahat
          alpha <- Thomas$Alphahat
          integral <- Kumulavsech(CC, omega, xlim, ylim)
          logP <- logpXCbeta(X, CC, alpha, omega, AreaW, integral)
          
          # Save the result for each repeat
          parentnum <- c(parentnum, dim(Thomas$CC)[1])
          parent[[i]] <- CC
          accept <- c(accept, Thomas$Accept)
          logllh <- c(logllh, logP)
          alphalist[[i]] <- Thomas$Postalpha
          kappalist[[i]] <- Thomas$Postkappa
          omegalist[[i]] <- Thomas$Postomega
          incProgress(1/storage2$Niter, detail = paste(i, 'finished'))
        }
      })
      
      
      # BIC
      bic.total <- bic(X, parentnum, logllh)
      df <- data.frame(parentnum, bic.total)
      ind <- as.numeric(names(which.max(table(parentnum))))
      tablec <- data.frame(table(parentnum))
      colnames(tablec) <- c("Cluster Size", "Freq")
      
      storage2$biclist <- list(
        "bic_df" = df,
        "parent" = parent,
        "bic.total" = bic.total,
        "z.post1" = z.post1,
        "w.post1" = w.post1,
        "ind" = ind,
        "tablec" = tablec
      )
      
    }
  })
  
  observeEvent(input$sampclust,{
    
    # clustering result table
    output$table1 <- renderDT({
      
      tryCatch(
        {
          df  <- biclist$bic_df
          bic.total <- biclist$bic.total
          parent <- biclist$parent
          
          z.post1 <- biclist$z.post1
          w.post1 <- biclist$w.post1
          
          parentnum <- df$parentnum
          
          ## BIC of the most frequent parent number
          ind <- input$num
          
          ## The index number which parent number is "ind (most frequent parent number)"
          ind5 <- c()
          for(i in 1:100){
            if(parentnum[i]==ind){
              ind5 <- c(ind5, i)
            }
          }
          bic5 <- bic.total[ind5]
          
          
          # The index number having smallest BIC among number of parent is "ind"
          ind2 <- as.numeric(rownames(df[((df["parentnum"]==ind) & df["bic.total"]==bic5[which.min(bic5)] ),]))
          
          # location of "in2" (which is best interation whith having smallest BIC)
          par5 <- parent[[ind2]]
          par5 <- data.frame(par5)
          colnames(par5) <- c('x', 'y')
          
          g_item <- clustering(w.post1, par5)
          
          
          # raanking based on distance from center
          # (The closer the center is, the greater the alpha is.)
          g_alpha <- c()
          
          for(l in 1:ind){
            temp <- g_item[which(g_item$group==l),]
            temp <- ranking(temp)
            g_alpha <- rbind(g_alpha, temp)
          }
          
          # ordering by order of item
          g_alpha <- g_alpha[order(as.numeric(rownames(g_alpha))),]
          g_fin <- cbind(w.post1[,1:2], g_alpha)
          
          # parent density using the 1000 estimated parent location
          cc <- parent[[1]]
          for(i in 2:100){
            cc<-rbind(cc, parent[[i]])
          }
          cc <- data.frame(cc)
          colnames(cc) <- c('x','y')
          
          if(ind > length(LETTERS)){
            addlabel = LETTERS
            for(U in 1:length(LETTERS)){
              for(l in 1:length(letters)){
                addlabel = c(addlabel, paste0(LETTERS[U],letters[l]))
              }
            }
            alphabet = addlabel[1:ind]
          } else{
            alphabet = LETTERS[1:ind]
          }
          
          g_alpha2 <- cbind(g_alpha, item = rownames(g_alpha))
          clust <- data.frame(
            g_alpha2 %>%
              group_by(group) %>%
              summarise(items = paste(item, collapse = ", ")))
          
          data.frame(Group = alphabet[clust$group], Item = clust[,2])
          
        },
        error = function(e){
          return()
        },
        warning = function(w){
          return()
        }
        
      )
      
      
    })
    # cluster slider
    output$num = renderUI({
      tryCatch(
        {
          shinyWidgets::sliderTextInput(inputId = "num",
                                        label = "Number of Cluster:",
                                        choices = biclist$tablec[,1],
                                        selected = biclist$tablec[which.max(biclist$tablec[,2]),1])    
        },
        error = function(e){
          return()
        },
        warning = function(w){
          return()
        }
      )
    })
    
    # cluster table
    output$table2 <- DT::renderDataTable({
      datatable(biclist$tablec, options = list(dom = 't'))
    })
    
    # clustering visualization
    output$fig3 <- renderPlotly({
      tryCatch(
        {
          df <- biclist$bic_df
          bic.total <- biclist$bic.total
          parent <- biclist$parent
          
          z.post1 <- biclist$z.post1
          w.post1 <- biclist$w.post1
          
          
          parentnum <- df$parentnum
          
          ## BIC of the most frequent parent number
          ind <- input$num
          
          ## The index number which parent number is "ind (most frequent parent number)"
          ind5 <- c()
          for(i in 1:100){
            if(parentnum[i]==ind){
              ind5 <- c(ind5, i)
            }
          }
          bic5 <- bic.total[ind5]
          
          # The index number having smallest BIC among number of parent is "ind"
          ind2 <- as.numeric(rownames(df[((df["parentnum"]==ind) & df["bic.total"]==bic5[which.min(bic5)] ),]))
          
          # location of "in2" (which is best interation whith having smallest BIC)
          par5 <- parent[[ind2]]
          par5 <- data.frame(par5)
          colnames(par5) <- c('x', 'y')
          
          g_item <- clustering(w.post1, par5)
          
          
          # raanking based on distance from center
          # (The closer the center is, the greater the alpha is.)
          g_alpha <- c()
          
          for(l in 1:ind){
            temp <- g_item[which(g_item$group==l),]
            temp <- ranking(temp)
            g_alpha <- rbind(g_alpha, temp)
          }
          
          # ordering by order of item
          g_alpha <- g_alpha[order(as.numeric(rownames(g_alpha))),]
          g_fin <- cbind(w.post1[,1:2], g_alpha)
          
          # parent density using the 1000 estimated parent location
          cc <- parent[[1]]
          for(i in 2:100){
            cc<-rbind(cc, parent[[i]])
          }
          cc <- data.frame(cc)
          colnames(cc) <- c('x','y')
          
          if(ind > length(LETTERS)){
            addlabel = LETTERS
            for(U in 1:length(LETTERS)){
              for(l in 1:length(letters)){
                addlabel = c(addlabel, paste0(LETTERS[U],letters[l]))
              }
            }
            alphabet = addlabel[1:ind]
          } else{
            alphabet = LETTERS[1:ind]
          }
          
          # Set the color and cluster name
          ggcolor = rainbow(ind, s=.6, v=.9)[sample(1:ind, ind)]
          par5["cluster"] <- alphabet
          par5["color"] <- ggcolor
          
          # Draw plot using ggplot
          temp <- (ggplot(w.post1, aes(x, y)) +
                     geom_point(data=z.post1, aes(x, y), col="grey", cex=1.0) +
                     stat_density_2d(data=cc, aes(x, y), color="gray80") + #density
                     geom_text(data=g_fin, aes(x, y), label=rownames(g_fin), color=ggcolor[g_fin$group], cex=4, fontface="bold") + # number of item
                     geom_text(data=par5, aes(x, y), label=alphabet[1:ind], col="gray30", cex=5.5, fontface="bold") + #alphabet
                     theme_bw() +
                     theme(plot.margin = unit(c(1,1,1,1), "cm"),
                           axis.text=element_text(size=16),
                           axis.title=element_text(size=14,face="bold"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           legend.title=element_blank(),
                           legend.position = c(0.9,0.9),
                           legend.text = element_text(size=16),
                           plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
                     ggtitle("Interaction Map"))
          
          ggplotly(temp) %>%
            add_markers(x = z.post1$x, y = z.post1$y,
                        type = 'scatter',
                        mode = 'markers',
                        text = paste("respondent", 1:nrow(z.post1), sep = ""),
                        name = "Respondent",
                        marker = list(color = "lightgrey")) %>%
            add_text(x = g_fin$x, y = g_fin$y, type = 'scatter',
                     name = "Item",
                     text = 1:nrow(w.post1),
                     textfont = list(family="Arial Black",size=16, weight="bold", color = ggcolor[g_fin$group])) %>%
            add_text(x = par5$x, y = par5$y, type = 'scatter',
                     name = "Cluster",
                     text = alphabet[1:ind],
                     textfont = list(size=19, color = "black"))
        },
        error = function(e){
          return()
        },
        warning = function(w){
          return()
        }
      )
      
      
      
    })
    
  })
  
  # clustering visualization
  output$fig3 <- renderPlotly({
    tryCatch(
      {
        df <- storage2$biclist$bic_df
        bic.total <- storage2$biclist$bic.total
        parent <- storage2$biclist$parent
        
        z.post1 <- storage2$biclist$z.post1
        w.post1 <- storage2$biclist$w.post1
        
        
        parentnum <- df$parentnum
        
        ## BIC of the most frequent parent number
        ind <- input$num
        
        ## The index number which parent number is "ind (most frequent parent number)"
        ind5 <- c()
        for(i in 1:storage2$Niter){
          if(parentnum[i]==ind){
            ind5 <- c(ind5, i)
          }
        }
        bic5 <- bic.total[ind5]
        
        # The index number having smallest BIC among number of parent is "ind"
        ind2 <- as.numeric(rownames(df[((df["parentnum"]==ind) & df["bic.total"]==bic5[which.min(bic5)] ),]))
        
        # location of "in2" (which is best interation whith having smallest BIC)
        par5 <- parent[[ind2]]
        par5 <- data.frame(par5)
        colnames(par5) <- c('x', 'y')
        
        g_item <- clustering(w.post1, par5)
        
        
        # raanking based on distance from center
        # (The closer the center is, the greater the alpha is.)
        g_alpha <- c()
        
        for(l in 1:ind){
          temp <- g_item[which(g_item$group==l),]
          temp <- ranking(temp)
          g_alpha <- rbind(g_alpha, temp)
        }
        
        # ordering by order of item
        g_alpha <- g_alpha[order(as.numeric(rownames(g_alpha))),]
        g_fin <- cbind(w.post1[,1:2], g_alpha)
        
        # parent density using the 1000 estimated parent location
        cc <- parent[[1]]
        for(i in 2:storage2$Niter){
          cc<-rbind(cc, parent[[i]])
        }
        cc <- data.frame(cc)
        colnames(cc) <- c('x','y')
        
        if(ind > length(LETTERS)){
          addlabel = LETTERS
          for(U in 1:length(LETTERS)){
            for(l in 1:length(letters)){
              addlabel = c(addlabel, paste0(LETTERS[U],letters[l]))
            }
          }
          alphabet = addlabel[1:ind]
        } else{
          alphabet = LETTERS[1:ind]
        }
        
        # Set the color and cluster name
        ggcolor = rainbow(ind, s=.6, v=.9)[sample(1:ind, ind)]
        par5["cluster"] <- alphabet
        par5["color"] <- ggcolor
        
        # Draw plot using ggplot
        temp <- (ggplot(w.post1, aes(x, y)) +
                   geom_point(data=z.post1, aes(x, y), col="grey", cex=1.0) +
                   stat_density_2d(data=cc, aes(x, y), color="gray80") + #density
                   geom_text(data=g_fin, aes(x, y), label=rownames(g_fin), color=ggcolor[g_fin$group], cex=4, fontface="bold") + # number of item
                   geom_text(data=par5, aes(x, y), label=alphabet[1:ind], col="gray30", cex=5.5, fontface="bold") + #alphabet
                   theme_bw() +
                   theme(plot.margin = unit(c(1,1,1,1), "cm"),
                         axis.text=element_text(size=16),
                         axis.title=element_text(size=14,face="bold"),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.title=element_blank(),
                         legend.position = c(0.9,0.9),
                         legend.text = element_text(size=16),
                         plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
                   ggtitle("Interaction Map"))
        
        ggplotly(temp) %>%
          add_markers(x = z.post1$x, y = z.post1$y,
                      type = 'scatter',
                      mode = 'markers',
                      text = paste("respondent", 1:nrow(z.post1), sep = ""),
                      name = "Respondent",
                      marker = list(color = "lightgrey")) %>%
          add_text(x = g_fin$x, y = g_fin$y, type = 'scatter',
                   name = "Item",
                   text = 1:nrow(w.post1),
                   textfont = list(family="Arial Black",size=16, weight="bold", color = ggcolor[g_fin$group])) %>%
          add_text(x = par5$x, y = par5$y, type = 'scatter',
                   name = "Cluster",
                   text = alphabet[1:ind],
                   textfont = list(size=19, color = "black"))
      },
      error = function(e){
        return()
      },
      warning = function(w){
        return()
      }
    )
    
    
    
  })
  
  # call function for clustering
  isolate({source("./utilities.R",local=T)})
  
  # clustering result table
  output$table1 <- renderDT({
    
    tryCatch(
      {
        df  <- storage2$biclist$bic_df
        bic.total <- storage2$biclist$bic.total
        parent <- storage2$biclist$parent
        
        z.post1 <- storage2$biclist$z.post1
        w.post1 <- storage2$biclist$w.post1
        
        parentnum <- df$parentnum
        
        ## BIC of the most frequent parent number
        ind <- input$num
        
        ## The index number which parent number is "ind (most frequent parent number)"
        ind5 <- c()
        for(i in 1:storage2$Niter){
          if(parentnum[i]==ind){
            ind5 <- c(ind5, i)
          }
        }
        bic5 <- bic.total[ind5]
        
        
        # The index number having smallest BIC among number of parent is "ind"
        ind2 <- as.numeric(rownames(df[((df["parentnum"]==ind) & df["bic.total"]==bic5[which.min(bic5)] ),]))
        
        # location of "in2" (which is best interation whith having smallest BIC)
        par5 <- parent[[ind2]]
        par5 <- data.frame(par5)
        colnames(par5) <- c('x', 'y')
        
        g_item <- clustering(w.post1, par5)
        
        
        # raanking based on distance from center
        # (The closer the center is, the greater the alpha is.)
        g_alpha <- c()
        
        for(l in 1:ind){
          temp <- g_item[which(g_item$group==l),]
          temp <- ranking(temp)
          g_alpha <- rbind(g_alpha, temp)
        }
        
        # ordering by order of item
        g_alpha <- g_alpha[order(as.numeric(rownames(g_alpha))),]
        g_fin <- cbind(w.post1[,1:2], g_alpha)
        
        # parent density using the 1000 estimated parent location
        cc <- parent[[1]]
        for(i in 2:storage2$Niter){
          cc<-rbind(cc, parent[[i]])
        }
        cc <- data.frame(cc)
        colnames(cc) <- c('x','y')
        
        if(ind > length(LETTERS)){
          addlabel = LETTERS
          for(U in 1:length(LETTERS)){
            for(l in 1:length(letters)){
              addlabel = c(addlabel, paste0(LETTERS[U],letters[l]))
            }
          }
          alphabet = addlabel[1:ind]
        } else{
          alphabet = LETTERS[1:ind]
        }
        
        g_alpha2 <- cbind(g_alpha, item = rownames(g_alpha))
        clust <- data.frame(
          g_alpha2 %>%
            group_by(group) %>%
            summarise(items = paste(item, collapse = ", ")))
        
        data.frame(Group = alphabet[clust$group], Item = clust[,2])
        
      },
      error = function(e){
        return()
      },
      warning = function(w){
        return()
      }
      
    )
    
    
  })
  
  
  # num of cluster slider
  output$num = renderUI({
    tryCatch(
      {
        shinyWidgets::sliderTextInput(inputId = "num",
                                      label = "Number of Cluster:",
                                      choices = storage2$biclist$tablec[,1],
                                      selected = storage2$biclist$tablec[which.max(storage2$biclist$tablec[,2]),1])    
      },
      error = function(e){
        return()
      },
      warning = function(w){
        return()
      }
    )
  })
  
  # optimal num of cluster
  output$table2 <- DT::renderDataTable({
    datatable(storage2$biclist$tablec, options = list(dom = 't'))
  })
  
  
}

shinyApp(ui = ui, server = server)

