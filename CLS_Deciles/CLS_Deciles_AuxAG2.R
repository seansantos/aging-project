library(shiny)
library(ggplot2)

X1 <- read.csv("19_1118_SL_auxAG_Plates1-3.csv",stringsAsFactors = F)
X2 <- read.csv("19_1118_SL_auxAG_Plates4-5.csv",stringsAsFactors = F)
X3 <- read.csv("19_1118_SL_auxAG_Plates6-10.csv",stringsAsFactors = F)
X4 <- read.csv("19_1118_SL_auxAG_PooledPlates.csv",stringsAsFactors = F)


ui <- fluidPage(
  selectInput(inputId = "Plates",
              label =  "Choose Plate Set",
              choices = c("Plates1-3","Plates4-5","Plates6-10","PooledAll"),
              selected = "Plates1-3"),
  selectInput(inputId = "Day",
              label =  "Choose Day",
              choices = unique(X4$Day),
              selected = 4),
  selectInput(inputId = "Include",
              label =  "Choose auxotrophy to include",
              choices = unique(X1$LeuLysMet),multiple=T),
  selectInput(inputId = "CPP",
              label =  "Choose L or K",
              choices = c("Rank_L","Rank_K")),
  plotOutput("hist")
)

server <- function(input,output){
  output$hist <- renderPlot({
    #select the dataset
    if(input$Plates == "Plates1-3"){
      df_sel <- X1
    }
    else if(input$Plates == "Plates4-5"){
      df_sel <- X2
    }
    else if(input$Plates == "Plates6-10"){
      df_sel <- X3
    }
    else if(input$Plates == "PooledAll"){
      df_sel <- X4
    }
    #select the day
    x_byDay <- df_sel[df_sel$Day == input$Day,]
    #get total num of observations
    tot_num <- length(x_byDay$l)
    #rank L and K
    x_byDay$Rank_L <- rank(x_byDay$l)
    x_byDay$Rank_K <- rank(x_byDay$K)
    #subset based on input "Include"
    x_byDay <- subset(x_byDay,LeuLysMet %in% input$Include)

    #print either L or K plots depending on selection
    if(input$CPP == "Rank_L"){
      ggplot(x_byDay,aes(Rank_L,color=LeuLysMet,fill=LeuLysMet)) +
        geom_density(aes(y= stat(density)),alpha=0.4,size=.8) +
        geom_vline(xintercept = seq(from=tot_num/10,to=tot_num,by=tot_num/10),
                   linetype=2,alpha=0.25) +
        xlab("Rank L") +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(size=16),axis.title.x = element_text(size=18),
              axis.text.y = element_text(size=16),axis.title.y = element_text(size=18)) +
        #ggtitle(paste("Day",input$Day)) +
        scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1")
      
    }
    else if(input$CPP == "Rank_K"){
      ggplot(x_byDay,aes(Rank_K,color=LeuLysMet,fill=LeuLysMet)) +
        geom_density(aes(y= stat(density)),alpha=0.4,size=.8) +
        geom_vline(xintercept = seq(from=tot_num/10,to=tot_num,by=tot_num/10),
                   linetype=2,alpha=0.25) +
        xlab("Rank L") +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(size=16),axis.title.x = element_text(size=18),
              axis.text.y = element_text(size=16),axis.title.y = element_text(size=18)) +
        #ggtitle(paste("Day",input$Day)) +
        scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1")
      
      
    }

  })
}

shinyApp(ui=ui,server=server)  
