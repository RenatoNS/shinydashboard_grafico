library(shiny)
library(shinydashboard)
library(xlsx)


# Funcoes
buscar_rz_social <- function(cnpj_input){
  user_cnpj <- which(df$CNPJ == cnpj_input)
  
  rz_social <- paste("Razao Social:", df$RazaoSocial[user_cnpj], sep=" ")
  
  return(rz_social)
}


buscar_cnpj <- function(cnpj_input){
  user_cnpj <- which(df$CNPJ == cnpj_input)
  
  cnpj <- paste("CNPJ:", df$CNPJ[user_cnpj], sep=" ")  
  
  return(cnpj)
}


buscar_dt_cad <- function(cnpj_input){
  user_cnpj <- which(df$CNPJ == cnpj_input)
  
  data_cad <- as.Date(df$DataSituacaoCadastral[user_cnpj], "%Y%m%d")
  data_cad <- format(data_cad, "%d-%m-%Y")
  
  dt_cad <- paste("Data de Cadastro:", data_cad, sep=" ")  
  
  return(dt_cad)
}


buscar_endereco <- function(cnpj_input){
  user_cnpj <- which(df$CNPJ == cnpj_input)
  
  endereco <- paste("Endereco:", df$TipoLogradouro[user_cnpj], df$Logradouro[user_cnpj],"," , df$Numero[user_cnpj],"," ,
                    df$Complemento[user_cnpj],"," , df$Bairro[user_cnpj],"," , df$Municipio[user_cnpj],"," , df$UF[user_cnpj],
                    "," ,"CEP:" , df$CEP[user_cnpj], sep=" ")
  
  return(endereco)
}


cria_grafico <- function(cnpj_input){
  user_cnpj <- which(df$CNPJ == cnpj_input)
  empresa_nome <- as.character(df$RazaoSocial[user_cnpj])
  df_nome <- df[df$RazaoSocial==empresa_nome,]
  out_grafic <- table(df_nome$UF[df_nome$RazaoSocial==empresa_nome])
  return(out_grafic)
}



# Define diretorio de trabalho
setwd("C:/workspace/shiny_grafic")


# Carrega arquivo .xlsx para o dataframe
df <- read.xlsx("Plan_Empresas_Estagiarios.xlsx", sheetIndex = 1)



ui <- dashboardPage(
  
  dashboardHeader(
    title = "PAINEL TCE"
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "aba1", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems( 
      tabItem(tabName = "aba1", 
              fluidRow(
                box(title = "Faça sua pesquisa", status = "primary", solidHeader = TRUE, width = 5,
                    textInput(inputId = "pesquisa", label = "Digite o CNPJ"),
                    actionButton(inputId = "busca", label = "Buscar"),
                ),
                
                box(title = "Resultado", status = "primary",solidHeader = TRUE, width = 7, 
                    textOutput("dado"),
                    textOutput("dado2"),
                    textOutput("dado3"),
                    textOutput("dado4"),
                ),
                box(title = "Grafico Unidades da empresa pesquisada por Estado", status = "primary",solidHeader = TRUE, width = 12,
                    plotOutput(outputId = "nomexregiao")
                    )
                
              )
              
             
      )
    )
  ) 
)
  

server <- function(input,output){
  dado_pesquisa <- eventReactive(input$busca, {input$pesquisa})
  output$dado <- renderText({buscar_rz_social(dado_pesquisa())})
  output$dado2 <- renderText({buscar_cnpj(dado_pesquisa())})
  output$dado3 <- renderText({buscar_endereco(dado_pesquisa())})
  output$dado4 <- renderText({buscar_dt_cad(dado_pesquisa())})
  output$nomexregiao <- renderPlot({barplot(cria_grafico(dado_pesquisa()),
                                            ylab="Unidades", xlab="Estado", main="Unidades da empresa por estado", col="lightblue")
})
  
 }

shinyApp(ui, server)
