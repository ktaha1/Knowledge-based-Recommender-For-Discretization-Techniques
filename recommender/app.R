library(shiny)
library(shinythemes)
# Define UI for data upload app ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("flatly"),
  #tags$br(),
  # App title ----
  titlePanel("Knowledge-based Recommender For Discretization Techniques"),
  #titlePanel(title=div(img(src="bus.gif", height = 70), "Beat the Trend - The Domino Bus Group"), windowTitle = "Beat the Trend - The Domino Bus Group"),  
  
  
  tags$hr(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      #tags$hr(),
      #hr(),
      helpText("The file must include a header and the output attribute must be named 'class'"),

      # Input: Checkbox if file has header ----
      #checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data set",
                           tags$br(),
                           #h4("Data set table"),
                           tableOutput("contents")),
                  
                  
                  tabPanel("Characteristics",
                           tags$br(),
                           h4("Table of measurement"),
                           tableOutput("char"),
                           downloadButton('downloadData', 'Download', class="btn-xs btn-info", style = "font-size:100%")),
                  
                  
                   tabPanel("Recommendation", 
                            fluidRow(
                              
                              column(6, 
                                     tags$br(),
                                     h4("Characteristics partitioned"), 
                                     tableOutput("table")),
                              
                              column(6, 
                                     tags$br(),
                                     h4("Technique(s) recommended based on Decision Tree accuracy"), 
                                     verbatimTextOutput("txtout"),
                                     tags$br(),
                                     h4("Technique(s) recommended based on k-Narest Neighbours accuracy"), 
                                     verbatimTextOutput("txtout2"))
                             
                              )
                            
                   ),
                  
                  
                  tabPanel("About", includeHTML("about.html") )
                  

                  
                  
                  
      )
      
    )
    
  )
)





# Define server logic to read selected file ----
server <- function(input, output) {
  
  global <- reactiveVal(0)
  global2 <- reactiveVal(0)
  
    #values <- reactiveValues(a = NULL, b = NULL)
  
  
  # ************************************* Tab 1 ****************************
  # ************************************************************************
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       #header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # ************************************* Tab 2 ****************************
  # ************************************************************************
  
  
  output$char <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    data <- read.csv(input$file1$datapath,
                   #header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    # N, d
    N=nrow(data)-0
    class_indice=ncol(data)-0
    d=class_indice-1
    data[,class_indice]=as.factor(data[,class_indice])
    
    # ************** Script *****************
    
    Characteristic<- c("Number of Instances (N)","Number of features (d)",
                         "Imbalance Ratio (IR)","Data Sparsity (DS)",
                         "Correlation of features (P)", "Multivariate normality (MVN)", 
                         "Homogeneity of class covariances (SDR)", "Intrinsic dimensionality ratio (IDR)", 
                         "Feature noise (ID2)", "Output noise (N2)")
    
    
    

    
    
    
    # IR : Imbalance Data
    library(imbalance)
    IR=round(imbalanceRatio(data, classAttr = colnames(data)[class_indice]),2)
    #final_table[[3]] <- imbalanceRatio(data);
    
    # DS : Data sparsity
    DS=round(N^(1/d),2)
    #final_table[[4]] <- N^(1/d)
    
    # P : Correlation
    P=round(mean(abs(as.dist(round(cor(data[,-class_indice], method = "pearson"),3)))),2)
    #final_table[[5]] <-
    
    # MVN
    library(mvShapiroTest)
    library(mvnTest)
    if(N<5000) mvn=mvShapiro.Test(as.matrix(data[,-class_indice])) else AD.test(data[,-class_indice])

    if (mvn$p.value >=0.05 ) {
      MVN <- 'Yes'
    }
    else {
      MVN <- 'No'
    }
    
    library(heplots)
    
    
    # SDR 
    library(mvShapiroTest)
    SDR=boxM(data[,-class_indice], data[,class_indice])
    if (SDR$p.value >=0.05 ) {
      SDR <- 'Yes'
    }
    else {
      SDR <- 'No'
    }
    
    # IDR
    # Mutual Information (based on Jackknife approach)
    library(mpmi)
    mi=mminjk(as.matrix(data[,-class_indice]), as.matrix(data[,class_indice]))
    #Sorting MI
    mis=sort(mi, decreasing = TRUE)
    # % cum of mu
    arr=(cumsum(mis)/sum(mis))*100
    acc <- 0
    i=0
    while (acc < 90) {
      i = i+1
      acc=arr[i]
    }
    ID=i
    IDR=round(ID/(d),2)
    #final_table[[8]] <- IDR
    
    
    
    # ID2 
    ID2=1-IDR
    #final_table[[9]] <- ID2
    
    
    # N2 : Output Noise
    library(DMwR)
    idxs <- sample(1:N,as.integer(0.7*N))
    train <- data[idxs,]
    test <- data[-idxs,]
    nn1 <- kNN(class ~ .,train,test,norm=FALSE,k=1)
    tab=table(test[,'class'],nn1)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    N2=round(100-accuracy(tab),2)
    #final_table[[10]]=100-accuracy(tab)
    
    
    
    Measure <- c(N,d,IR,DS,P,MVN,SDR,IDR,ID2,N2)
    df <- data.frame(Characteristic, Measure)
    #values$a=df
    global(df)
    return(df)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_characteristics', 'csv', sep = ".")
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(global(), file,
                row.names = FALSE)
    }
  )
  
  # ************************************* Tab 3 ****************************
  # ************************************************************************
  
  
  output$table <- renderTable({
      req(input$file1)
      req(global())
      df=global()
      
      if(as.numeric(as.character(df$Measure[1])) < 400) n="Small" else n="Large"
      
      if(as.numeric(as.character(df$Measure[2])) < 20) d="Small"  else d="Large"
      
      if(as.numeric(as.character(df$Measure[3])) < 0.60) ir="Yes" else ir="No"
      
      if(as.numeric(as.character(df$Measure[4])) < 1.5) ds="Large" else ds="Small"
      
      if(as.numeric(as.character(df$Measure[5])) < 0.13) p="No" else p="Yes" 
      
      if(as.numeric(as.character(df$Measure[8])) < 0.74) idr="Small" else idr="Large"
      
      if(as.numeric(as.character(df$Measure[9])) < 0.28) idr2="Small" else idr2="Large"
      
      if(as.numeric(as.character(df$Measure[10])) < 30) n2="Small" else n2="Large"
      
      Characteristic<- c("Number of Instances (N)","Number of features (d)",
                         "Imbalance Ratio (IR)","Data Sparsity (DS)",
                         "Correlation of features (P)", "Multivariate normality (MVN)", 
                         "Homogeneity of class covariances (SDR)", "Intrinsic dimensionality ratio (IDR)", 
                         "Feature noise (ID2)", "Output noise (N2)")
      State <- c(n,d,ir,ds,p,as.character(df$Measure[6]),as.character(df$Measure[7]),idr,idr2,n2)
      out <- data.frame(Characteristic,State)
      
      #return(df$Measure[1])
     
      global2(out)
      return(out)
   
  })
  
  
  output$txtout <- renderText({
    req(input$file1)
    req(global2())
    df=global2()
    
    if (as.character(df$State[1])=="Small") {
      if (as.character(df$State[6])=="No") {
        if (as.character(df$State[7])=="Yes") {
          if (as.character(df$State[10])=="Small") {
            if (as.character(df$State[3])=="Yes") 
              paste("Zeta") 
            else 
              paste("FUSINTER, \nCAIM.")
          }else {
              paste("ChiMerge.")
          }
        }else{
          if (as.character(df$State[10])=="Small") {
            if (as.character(df$State[1])=="Small") {
              if (as.character(df$State[8])=="Large") {
                paste("Distance")
              }else{
                if (as.character(df$State[3])=="Yes") {
                  paste("FUSINTER, \nCAIM, \nDistance, \nChiMerge")
                }else {
                  paste("EqualWidth")
                }
              }
            }else {
              paste("Distance, \nFUSINTER, \nEqualWidth")
            }
          }else{
            if (as.character(df$State[3])=="Yes") {
              if (as.character(df$State[8])=="Large") {
                paste("Zeta, \nDistance, \nEqualWidth")
              }else {
                paste("ChiMerge")
              }
            }else {
              paste("EqualWidth")
            }
          }
        }
      }else{
        if (as.character(df$State[3])=="Yes") {
          paste("Distance, \nChiMerge, \nEqualWidth")
        }else {
          paste("UCPD")
        }
      }
      
    }else {
      if (as.character(df$State[5])=="Yes") {
        if (as.character(df$State[10])=="Small") {
          paste("ChiMerge, \nDistance")
        }else {
          if (as.character(df$State[7])=="Yes") {
            paste("FUSINTER")
          }else {
            paste("EqualWidth")
          }
        }
      }else {
        if (as.character(df$State[1])=="Small") {
          paste("EqualWidth")
        }else {
          paste("Distance, \nEqualWidth")
        }
      }
    }
    
    
    
    
    
    #paste(df$disc, sep="\n")
  })
  
  
  
  
  output$txtout2 <- renderText({
    
    req(input$file1)
    req(global2())
    df=global2()
    if (as.character(df$State[3])=="Yes") {
      if (as.character(df$State[5])=="Yes") {
        if (as.character(df$State[10])=="Small") {
          paste("FUSINTER, \nChiMerge.")
          
        } else {  # else of N2
          if (as.character(df$State[1])=="Small") {
            paste("Distance.")
          }else {  # else of N
            paste("ChiMerge, \nMVD.")
          }
        }
        
      }else { #else of P
        if (as.character(df$State[7])=="Yes") {
          if (as.character(df$State[4])=="Small") {
            paste("Zeta.")
          }else {   # else of SD
            paste("FUSINTER, \nDistance, \nCaim, \nZeta.")
          }
        }else {  # else of SDR
          if (as.character(df$State[8])=="Large") {
            paste("FUSINTER.")
          }else { # else of IDR
            if (as.character(df$State[1])=="Small") {
              if (as.character(df$State[6])=="No") {
                paste("Caim.")
              }else {
                paste("EqualWidth.")
              }
            }else { # else of N
              paste("EqualWidth.")
            }
            
          }
        }
      }
    }else {  # else of IR
      if (as.character(df$State[4])=="Small") {
        if (as.character(df$State[5])=="Yes") {
          if (as.character(df$State[1])=="Small") {
            paste("FUSINTER.")
          }else {
            paste("EqualWidth, \nMVD.")
          }
        }else { # else of P
          paste("EqualWidth.")
        }
      } else {   # else of DS
        if (as.character(df$State[2])=="Small") {
          paste("Caim, \nZeta.")
        }else {  # else of D
          if (as.character(df$State[8])=="Large") {
            if (as.character(df$State[10])=="Small") {
              paste("Caim, \nDistance.")
            }else {  # else of N2
              if (as.character(df$State[7])=="Yes") {
                paste("Caim, \nUCPD.")
              }else {
                paste("ChiMerge.")
              }
              
            }
          }else { # else of IDR
            if (as.character(df$State[6])=="No") {
              paste("EqualWidth, \nFUSINTER.")
            }else {  # else of MVN
              if (as.character(df$State[7])=="Yes") {
                paste("ChiMerge.")
              } else {
                paste("Caim, \nZeta.")
              }
            }
          }
          
        }
      }
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)