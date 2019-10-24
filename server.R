library(shiny)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)
library(DT)
var.list <<- c("Month", "Age", "Sex", "deno", "disease", "cases","est", "lci", "uci")
genRCI <- function(dataset, scale = 1000) {
    res <- dataset
    res$est <- res$cases/res$deno
    res$lci <- res$est/exp(1.96/sqrt(res$cases))
    res$uci <- res$est*exp(1.96/sqrt(res$cases))
    if(sum(res$cases==0)>0|sum(res$deno==0)>0) {
      res[res$cases==0 | res$deno==0,c("est", "lci", "uci")] <- 0
    }
    res$est <- round(res$est *scale,2)
    res$lci <- round(res$lci *scale,2)
    res$uci <- round(res$uci *scale,2)
    return(res)
  }
reArrange <- function(dataset) {
    res <- gather(dataset, AURI, ALRI, ABD, AOD, AWD, AJS, AFI, Malaria, key = "disease", value = "cases")
    return(res)
}
deno.plot.age <- function(disease) {
  renderPlot({
    ggplot(data = deno.dataset.age[deno.dataset.age$disease==disease,],
           aes(colour = Sex, x = Month, y = est, group = Sex)) +
      geom_point(position=position_dodge2(width = 0.25))+
      geom_line(position=position_dodge2(width = 0.25)) +
      scale_x_continuous(breaks = 1:12) +
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
      theme_bw()+
      labs(y = "Incidence rate (per 1000)", title = paste(disease, " by sex over time", sep = ""))
  })
}
deno.plot.sex <- function(disease) {
  renderPlot({
    ggplot(data = deno.dataset.sex[deno.dataset.sex$disease==disease,],
           aes(colour = Age, x = Month, y = est, group = Age)) +
      geom_point(position=position_dodge2(width = 0.25))+
      geom_line(position=position_dodge2(width = 0.25)) +
      scale_x_continuous(breaks = 1:12) +
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
      theme_bw()+
      labs(y = "Incidence rate (per 1000)", title = paste(disease, " by age over time", sep = ""))
  })
}
deno.plot.month <- function(disease) {
  renderPlot({
    ggplot(data = deno.dataset.month[deno.dataset.month$disease==disease,],
           aes(fill = Sex, x = Age, y = est, group = Sex)) +
      geom_bar(position=position_dodge(),stat="identity")+
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
      theme_bw()+
      labs(y = "Cumulative incidence rate (per 1000)", title = paste(disease, " by age and sex", sep = ""))
  })
}
deno.table <- function(disease) {
  renderDT(
    deno.dataset.combined[deno.dataset.combined$disease==disease,], server = TRUE,rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = deno$months * 5
    )
  )
}
deno.ui <- function(disease) {
  renderUI({
    fluidPage(
      h5(strong("Results for ",disease), ": view results below or select another disease in the menu above."),
      tabsetPanel(
        tabPanel("Figures", 
                 verticalLayout(
                   plotOutput(paste(disease, ".age",sep = ""), width = deno$months*80),
                   plotOutput(paste(disease, ".sex",sep = ""), width = deno$months*150),
                   plotOutput(paste(disease, ".month",sep = ""), width = 500)
                 )),
        tabPanel("Table",
                 DTOutput(paste(disease, ".table", sep = ""))
                 )
      )
      
    )
  })
}
visit.plot.age <- function(disease) {
  renderPlot({
    ggplot(data = visit.dataset.age[visit.dataset.age$disease==disease,],
           aes(colour = Sex, x = Month, y = est, group = Sex)) +
      geom_point(position=position_dodge2(width = 0.25))+
      geom_line(position=position_dodge2(width = 0.25)) +
      scale_x_continuous(breaks = 1:12) +
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
      theme_bw()+
      labs(y = "Proportionate contribution (%))", title = paste(disease, " by sex over time", sep = ""))
  })
}
visit.plot.sex <- function(disease) {
  renderPlot({
    ggplot(data = visit.dataset.sex[visit.dataset.sex$disease==disease,],
           aes(colour = Age, x = Month, y = est, group = Age)) +
      geom_point(position=position_dodge2(width = 0.25))+
      geom_line(position=position_dodge2(width = 0.25)) +
      scale_x_continuous(breaks = 1:12) +
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
      theme_bw()+
      labs(y = "Proportionate contribution (%))", title = paste(disease, " by age over time", sep = ""))
  })
}
visit.plot.month <- function(disease) {
  renderPlot({
    ggplot(data = visit.dataset.month[visit.dataset.month$disease==disease,],
           aes(fill = Sex, x = Age, y = est, group = Sex)) +
      geom_bar(position=position_dodge(),stat="identity")+
      geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
      theme_bw()+
      labs(y = "Proportionate contribution (%))", title = paste(disease, " by age and sex", sep = ""))
  })
}
visit.table <- function(disease) {
  renderDT(
    visit.dataset.combined[visit.dataset.combined$disease==disease,], server = TRUE,rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = visit$months * 5
    )
  )
}
visit.ui <- function(disease) {
  renderUI({
    fluidPage(
      h5(strong("Results for ",disease), ": view results below or select another disease in the menu above."),
      tabsetPanel(
        tabPanel("Figures", 
                 verticalLayout(
                   plotOutput(paste(disease, ".age",sep = ""), width = visit$months*80),
                   plotOutput(paste(disease, ".sex",sep = ""), width = visit$months*150),
                   plotOutput(paste(disease, ".month",sep = ""), width = 500)
                 )),
        tabPanel("Table",
                 DTOutput(paste(disease, ".table", sep = ""))
        )
      )
      
    )
  })
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # UI-------------
  output$uifile <- renderUI({
    popify(
      fileInput("file", label = NULL,
                accept = c("xls", "xlsx")),
      title = NULL, content = "Please upload the Excel file (xlsx/xls)", placement = "right", trigger = "focus", options = list(container = "body")
    )
  })
  # Server---------
  observeEvent(input$file,{
    path <- input$file$datapath
    dtset <- lapply(excel_sheets(path), read_excel, path = path, col_names = FALSE)
    # Record key Values
    # Deno
    if(dtset[[5]][[9,3]]=="Yes") {
      deno <- list(
        sheet = as.numeric(substr(dtset[[5]][10,3],2,2)),
        monthStart = NA,
        months = as.numeric(dtset[[5]][[81,10]]),
        monthEnd = NA,
        missing = NA,
        dat = NA
      )
      if(deno$sheet==2){
        deno$dat <- dtset[[3]][15:26, 3:12]
      }else{
        deno$dat <- dtset[[4]][46:57, 3:12]
      }
      for(i in 1:12){
        if(sum(!is.na(deno$dat[i,]))>0) {
          deno$monthStart <- i
          break
        }
      }
      deno$monthEnd <- deno$monthStart + deno$months - 1
      deno$missing <- sum(is.na(deno$dat[deno$monthStart:deno$monthEnd, 1:10]))>0
    }else{
      deno <- list(monthStart = NA)
    }
    # Visits
    if(dtset[[11]][[9,3]]=="Yes") {
      visit <- list(
        monthStart = NA,
        months = as.numeric(dtset[[11]][[81,10]]),
        monthEnd = NA,
        missing = NA,
        dat = dtset[[10]][12:23, 3:12]
      )
      for(i in 1:12){
        if(sum(!is.na(visit$dat[i,]))>0) {
          visit$monthStart <- i
          break
        }
      }
      visit$monthEnd <- visit$monthStart + visit$months - 1
      visit$missing <- sum(is.na(visit$dat[visit$monthStart:visit$monthEnd, 1:10]))>0
    }else{
      visit <- list(monthStart = NA)
    }
    # Determing UI #
    if(is.na(deno$monthStart)&is.na(visit$monthStart)){ #No data available
      output$uioption <- renderUI({p("ERROR: Denominator missing, please complete W2, W3 or W8 first!", style = "color:red")
      })
    }else{
      if(is.na(visit$monthStart)){ #Only deno available
        if(deno$missing){
          output$uioption <- renderUI({p(paste("ERROR: Denominator data incomplete, please check W", deno$sheet, " for missingness!", sep = ""),
                                         style = "color:red")})
        }else{
          output$uioption <- renderUI({h4("Step 3: click button(s) below to generate results")})
        }
      }
      if(is.na(deno$monthStart)){ #Only visit available
        if(visit$missing){
          output$uioption <- renderUI({p("ERROR: Outpatient visits data incomplete, please check W8 for missingness!",
                                         style = "color:red")})
        }else{
          output$uioption <- renderUI({h4("Step 3: click button(s) below to generate results")})
        }
      }
      if(is.na(deno$monthStart) + is.na(visit$monthStart)==0){ #Both available
        if(deno$missing){
          output$uioption <- renderUI({p(paste("ERROR: Denominator data incomplete, please check W", deno$sheet, " for missingness!", sep = ""),
                                         style = "color:red")})
        }
        if(visit$missing){
          output$uioption <- renderUI({p("ERROR: Outpatient visits data incomplete, please check W8 for missingness!",
                                         style = "color:red")})
        }
        if(deno$missing + visit$missing==0){
          output$uioption <- renderUI({h4("Step 3: click button(s) below to generate results")})
        }
      }
    }
    if(!is.na(deno$monthStart)) {
      if(!deno$missing)
      output$uioption2 <- renderUI({
        actionButton(inputId = "deno.action", label = "Incidence Rate")
      })
    }
    if(!is.na(visit$monthStart)) {
      if(!visit$missing)
      output$uioption3 <- renderUI({
        actionButton(inputId = "visit.action", label = "Proportionate Contribution")
      })
    }
    
    # End of determing UI #
  #Deno dataset processing-----
    observeEvent(input$deno.action, {
      deno.dataset <- expand.grid(
        Month = 1:12,
        Age = c("0-<1", "1-<5", "5-14", "15-49", "50-"),
        Sex = c("Male", "Female")
      )
      deno.dataset$deno <- as.numeric(unlist(deno$dat))
      deno.dataset$AURI <- as.numeric(unlist(dtset[[5]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      deno.dataset$ALRI <- as.numeric(unlist(dtset[[5]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      deno.dataset$ABD <- as.numeric(unlist(dtset[[6]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      deno.dataset$AOD <- as.numeric(unlist(dtset[[6]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      deno.dataset$AWD <- as.numeric(unlist(dtset[[6]][c(15:26,28:39,41:52,54:65,67:78),9:10]))
      deno.dataset$AJS <- as.numeric(unlist(dtset[[7]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      deno.dataset$AFI <- as.numeric(unlist(dtset[[8]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      deno.dataset$Malaria <- as.numeric(unlist(dtset[[8]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      deno.dataset <- deno.dataset[deno.dataset$Month>= deno$monthStart & deno.dataset$Month <= deno$monthEnd,]
      deno.dataset[is.na(deno.dataset)] <- 0
      deno.dataset.sex <- deno.dataset %>% group_by(Age, Month) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.sex$Sex <- "Total"
      deno.dataset.month <- deno.dataset %>% group_by(Age, Sex) %>%
        dplyr::summarise(deno = mean(deno), # Cumulative burden
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.month$Month <- "Total"
      deno.dataset.age <- deno.dataset %>% group_by(Month, Sex) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.age$Age <- "Total"
      deno.dataset.by.age <- deno.dataset.month %>% group_by(Age) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.by.age$Sex <- "Total"
      deno.dataset.by.age$Month <- "Total"
      deno.dataset.by.sex <- deno.dataset.month %>% group_by(Sex) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.by.sex$Age <- "Total"
      deno.dataset.by.sex$Month <- "Total"
      deno.dataset.by.month <- deno.dataset.age %>% group_by(Month) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.by.month$Age <- "Total"
      deno.dataset.by.month$Sex <- "Total"
      deno.dataset.all <- deno.dataset.by.age %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      deno.dataset.all$Age <- "Total"
      deno.dataset.all$Sex <- "Total"
      deno.dataset.all$Month <- "Total"
#######Now eight dattasets ready #################
      deno.dataset <- reArrange(deno.dataset)
      deno.dataset.age <- reArrange(deno.dataset.age)
      deno.dataset.month <- reArrange(deno.dataset.month)
      deno.dataset.sex <- reArrange(deno.dataset.sex)
      deno.dataset.by.age <- reArrange(deno.dataset.by.age)
      deno.dataset.by.month <- reArrange(deno.dataset.by.month)
      deno.dataset.by.sex <- reArrange(deno.dataset.by.sex)
      deno.dataset.all <- reArrange(deno.dataset.all)
      # Calc Rate + CI
      deno.dataset <- genRCI(deno.dataset)[var.list]
      deno.dataset.age <- genRCI(deno.dataset.age)[var.list]
      deno.dataset.month <- genRCI(deno.dataset.month)[var.list]
      deno.dataset.sex <- genRCI(deno.dataset.sex)[var.list]
      deno.dataset.by.age <- genRCI(deno.dataset.by.age)[var.list]
      deno.dataset.by.month <- genRCI(deno.dataset.by.month)[var.list]
      deno.dataset.by.sex <- genRCI(deno.dataset.by.sex)[var.list]
      deno.dataset.all <- genRCI(deno.dataset.all)[var.list]
      deno.dataset <<- deno.dataset
      deno.dataset.age <<- deno.dataset.age
      deno.dataset.month <<- deno.dataset.month
      deno.dataset.sex <<- deno.dataset.sex
      deno.dataset.by.age <<- deno.dataset.by.age
      deno.dataset.by.month <<- deno.dataset.by.month
      deno.dataset.by.sex <<- deno.dataset.by.sex
      deno.dataset.all <<- deno.dataset.all
      deno.dataset.combined <<- list.stack(list(
        deno.dataset, deno.dataset.age, deno.dataset.month, deno.dataset.sex,
        deno.dataset.by.age, deno.dataset.by.month, deno.dataset.by.sex,
        deno.dataset.all
      ))
      # Figures----
      output$AURI.age <- deno.plot.age("AURI")
      output$ALRI.age <- deno.plot.age("ALRI")
      output$ABD.age <- deno.plot.age("ABD")
      output$AOD.age <- deno.plot.age("AOD")
      output$AWD.age <- deno.plot.age("AWD")
      output$AJS.age <- deno.plot.age("AJS")
      output$AFI.age <- deno.plot.age("AFI")
      output$Malaria.age <- deno.plot.age("Malaria")
      output$AURI.sex <- deno.plot.sex("AURI")
      output$ALRI.sex <- deno.plot.sex("ALRI")
      output$ABD.sex <- deno.plot.sex("ABD")
      output$AOD.sex <- deno.plot.sex("AOD")
      output$AWD.sex <- deno.plot.sex("AWD")
      output$AJS.sex <- deno.plot.sex("AJS")
      output$AFI.sex <- deno.plot.sex("AFI")
      output$Malaria.sex <- deno.plot.sex("Malaria")
      output$AURI.month <- deno.plot.month("AURI")
      output$ALRI.month <- deno.plot.month("ALRI")
      output$ABD.month <- deno.plot.month("ABD")
      output$AOD.month <- deno.plot.month("AOD")
      output$AWD.month <- deno.plot.month("AWD")
      output$AJS.month <- deno.plot.month("AJS")
      output$AFI.month <- deno.plot.month("AFI")
      output$Malaria.month <- deno.plot.month("Malaria")
      output$AURI.table <- deno.table("AURI")
      output$ALRI.table <- deno.table("ALRI")
      output$ABD.table <- deno.table("ABD")
      output$AOD.table <- deno.table("AOD")
      output$AWD.table <- deno.table("AWD")
      output$AJS.table <- deno.table("AJS")
      output$AFI.table <- deno.table("AFI")
      output$Malaria.table <- deno.table("Malaria")
      # Create each UI results-----
      output$uiAURI <- deno.ui("AURI")
      output$uiALRI <- deno.ui("ALRI")
      output$uiABD <- deno.ui("ABD")
      output$uiAOD <- deno.ui("AOD")
      output$uiAWD <- deno.ui("AWD")
      output$uiAJS <- deno.ui("AJS")
      output$uiAFI <- deno.ui("AFI")
      output$uiMalaria <- deno.ui("Malaria")
      observeEvent(input$action.diseases,{
        if(is.null(input$diseases)){
          output$uiCompare <- renderUI({
            p("Please select at least one disease first!", style = "color:red")
          })
        }else{
          output$deno.plot.by.month <- renderPlot({
            ggplot(data = deno.dataset.by.month[deno.dataset.by.month$disease%in%input$diseases,],
                   aes(colour = disease, x = Month, y = est, group = disease)) +
              geom_point(position=position_dodge2(width = 0.25))+
              geom_line(position=position_dodge2(width = 0.25)) +
              scale_x_continuous(breaks = 1:12) +
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
              theme_bw()+
              labs(y = "Incidence rate (per 1000)", title = "Incidence rates of selected disease(s) over time")
          })
          output$deno.plot.by.sex <- renderPlot({
            ggplot(data = deno.dataset.by.sex[deno.dataset.by.sex$disease%in%input$diseases,],
                   aes(fill = disease, x = Sex, y = est, group = disease)) +
              geom_bar(position=position_dodge(),stat="identity")+
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
              theme_bw()+
              labs(y = "Cumulative incidence rate (per 1000)", 
                   title = "Cumulative incidence rates of selected disease(s) by sex")
          })
          output$deno.plot.by.age <- renderPlot({
            ggplot(data = deno.dataset.by.age[deno.dataset.by.age$disease%in%input$diseases,],
                   aes(fill = disease, x = Age, y = est, group = disease)) +
              geom_bar(position=position_dodge(),stat="identity")+
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
              theme_bw()+
              labs(y = "Cumulative incidence rate (per 1000)", 
                   title = "Cumulative incidence rates of selected disease(s) by age")
          })
          output$diseases.table <- renderDT(
            deno.dataset.combined[deno.dataset.combined$disease%in%input$diseases,], server = TRUE,rownames = FALSE,
            extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              pageLength = deno$months * 5)
          )
          output$uiCompare <- renderUI({
            tabsetPanel(
              tabPanel("Figures",
                       verticalLayout(
                         plotOutput("deno.plot.by.month", width = deno$months * 50 * length(input$diseases)),
                         plotOutput("deno.plot.by.sex", width = 180*length(input$diseases)),
                         plotOutput("deno.plot.by.age", width = 300*length(input$diseases))
                       )),
              tabPanel("Table", DTOutput("diseases.table"))
            )
          })
        }
      })

      output$uiresults <- renderUI(
        {
          navbarPage("Results: Incidence rate (per 1000)",
                     navbarMenu("ARI",tabPanel("AURI", uiOutput("uiAURI")),
                                tabPanel("ALRI", uiOutput("uiALRI"))),
                     navbarMenu("ADD",tabPanel("ABD", uiOutput("uiABD")), 
                                tabPanel("AOD", uiOutput("uiAOD")), 
                                tabPanel("AWD", uiOutput("uiAWD"))),
                     tabPanel("AJS", uiOutput("uiAJS")),
                     navbarMenu("AFI", tabPanel("AFI", uiOutput("uiAFI")), 
                                tabPanel("Malaria", uiOutput("uiMalaria"))),
                     tabPanel("Comparisons between diseases", verticalLayout(
                       selectInput('diseases', 'Select diseases from below',
                                   c("AURI", "ALRI",
                                     "ABD", "AOD", "AWD",
                                     "AJS", "AFI", "Malaria"), multiple=TRUE, selectize=TRUE),
                       actionButton("action.diseases", "Confirm selection"),
                       uiOutput("uiCompare")
                     )))
        }
      )
    })
    # Visit dataset processing-----
    observeEvent(input$visit.action, {
      visit.dataset <- expand.grid(
        Month = 1:12,
        Age = c("0-<1", "1-<5", "5-14", "15-49", "50-"),
        Sex = c("Male", "Female")
      )
      visit.dataset$deno <- as.numeric(unlist(visit$dat))
      visit.dataset$AURI <- as.numeric(unlist(dtset[[11]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      visit.dataset$ALRI <- as.numeric(unlist(dtset[[11]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      visit.dataset$ABD <- as.numeric(unlist(dtset[[12]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      visit.dataset$AOD <- as.numeric(unlist(dtset[[12]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      visit.dataset$AWD <- as.numeric(unlist(dtset[[12]][c(15:26,28:39,41:52,54:65,67:78),9:10]))
      visit.dataset$AJS <- as.numeric(unlist(dtset[[13]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      visit.dataset$AFI <- as.numeric(unlist(dtset[[14]][c(15:26,28:39,41:52,54:65,67:78),5:6]))
      visit.dataset$Malaria <- as.numeric(unlist(dtset[[14]][c(15:26,28:39,41:52,54:65,67:78),7:8]))
      visit.dataset <- visit.dataset[visit.dataset$Month>= visit$monthStart & visit.dataset$Month <= visit$monthEnd,]
      visit.dataset[is.na(visit.dataset)] <- 0
      visit.dataset.sex <- visit.dataset %>% group_by(Age, Month) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.sex$Sex <- "Total"
      visit.dataset.month <- visit.dataset %>% group_by(Age, Sex) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.month$Month <- "Total"
      visit.dataset.age <- visit.dataset %>% group_by(Month, Sex) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.age$Age <- "Total"
      visit.dataset.by.age <- visit.dataset.month %>% group_by(Age) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.by.age$Sex <- "Total"
      visit.dataset.by.age$Month <- "Total"
      visit.dataset.by.sex <- visit.dataset.month %>% group_by(Sex) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.by.sex$Age <- "Total"
      visit.dataset.by.sex$Month <- "Total"
      visit.dataset.by.month <- visit.dataset.age %>% group_by(Month) %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.by.month$Sex <- "Total"
      visit.dataset.by.month$Age <- "Total"
      visit.dataset.all <- visit.dataset.by.age %>%
        dplyr::summarise(deno = sum(deno),
                         AURI = sum(AURI),
                         ALRI = sum(ALRI),
                         ABD = sum(ABD),
                         AOD = sum(AOD),
                         AWD = sum(AWD),
                         AJS = sum(AJS),
                         AFI = sum(AFI),
                         Malaria = sum(Malaria))
      visit.dataset.all$Age <- "Total"
      visit.dataset.all$Sex <- "Total"
      visit.dataset.all$Month <- "Total"
      ############### Now eight dattasets ready #################
      visit.dataset <- reArrange(visit.dataset)
      visit.dataset.age <- reArrange(visit.dataset.age)
      visit.dataset.month <- reArrange(visit.dataset.month)
      visit.dataset.sex <- reArrange(visit.dataset.sex)
      visit.dataset.by.age <- reArrange(visit.dataset.by.age)
      visit.dataset.by.month <- reArrange(visit.dataset.by.month)
      visit.dataset.by.sex <- reArrange(visit.dataset.by.sex)
      visit.dataset.all <- reArrange(visit.dataset.all)
      # Calc Rate + CI
      visit.dataset <- genRCI(visit.dataset,scale = 100)[var.list]
      visit.dataset.age <- genRCI(visit.dataset.age,scale = 100)[var.list]
      visit.dataset.month <- genRCI(visit.dataset.month,scale = 100)[var.list]
      visit.dataset.sex <- genRCI(visit.dataset.sex,scale = 100)[var.list]
      visit.dataset.by.age <- genRCI(visit.dataset.by.age,scale = 100)[var.list]
      visit.dataset.by.month <- genRCI(visit.dataset.by.month,scale = 100)[var.list]
      visit.dataset.by.sex <- genRCI(visit.dataset.by.sex,scale = 100)[var.list]
      visit.dataset.all <- genRCI(visit.dataset.all,scale = 100)[var.list]
      visit.dataset <<- visit.dataset
      visit.dataset.age <<- visit.dataset.age
      visit.dataset.month <<- visit.dataset.month
      visit.dataset.sex <<- visit.dataset.sex
      visit.dataset.by.age <<- visit.dataset.by.age
      visit.dataset.by.month <<- visit.dataset.by.month
      visit.dataset.by.sex <<- visit.dataset.by.sex
      visit.dataset.all <<- visit.dataset.all
      visit.dataset.combined <<- list.stack(list(
        visit.dataset, visit.dataset.age, visit.dataset.month, visit.dataset.sex,
        visit.dataset.by.age, visit.dataset.by.month, visit.dataset.by.sex,
        visit.dataset.all
      ))
      # Figures----
      output$AURI.age <- visit.plot.age("AURI")
      output$ALRI.age <- visit.plot.age("ALRI")
      output$ABD.age <- visit.plot.age("ABD")
      output$AOD.age <- visit.plot.age("AOD")
      output$AWD.age <- visit.plot.age("AWD")
      output$AJS.age <- visit.plot.age("AJS")
      output$AFI.age <- visit.plot.age("AFI")
      output$Malaria.age <- visit.plot.age("Malaria")
      output$AURI.sex <- visit.plot.sex("AURI")
      output$ALRI.sex <- visit.plot.sex("ALRI")
      output$ABD.sex <- visit.plot.sex("ABD")
      output$AOD.sex <- visit.plot.sex("AOD")
      output$AWD.sex <- visit.plot.sex("AWD")
      output$AJS.sex <- visit.plot.sex("AJS")
      output$AFI.sex <- visit.plot.sex("AFI")
      output$Malaria.sex <- visit.plot.sex("Malaria")
      output$AURI.month <- visit.plot.month("AURI")
      output$ALRI.month <- visit.plot.month("ALRI")
      output$ABD.month <- visit.plot.month("ABD")
      output$AOD.month <- visit.plot.month("AOD")
      output$AWD.month <- visit.plot.month("AWD")
      output$AJS.month <- visit.plot.month("AJS")
      output$AFI.month <- visit.plot.month("AFI")
      output$Malaria.month <- visit.plot.month("Malaria")
      output$AURI.table <- visit.table("AURI")
      output$ALRI.table <- visit.table("ALRI")
      output$ABD.table <- visit.table("ABD")
      output$AOD.table <- visit.table("AOD")
      output$AWD.table <- visit.table("AWD")
      output$AJS.table <- visit.table("AJS")
      output$AFI.table <- visit.table("AFI")
      output$Malaria.table <- visit.table("Malaria")
      # Create each UI results-----
      output$uiAURI <- visit.ui("AURI")
      output$uiALRI <- visit.ui("ALRI")
      output$uiABD <- visit.ui("ABD")
      output$uiAOD <- visit.ui("AOD")
      output$uiAWD <- visit.ui("AWD")
      output$uiAJS <- visit.ui("AJS")
      output$uiAFI <- visit.ui("AFI")
      output$uiMalaria <- visit.ui("Malaria")
      observeEvent(input$action.diseases,{
        if(is.null(input$diseases)){
          output$uiCompare <- renderUI({
            p("Please select at least one disease first!", style = "color:red")
          })
        }else{
          output$visit.plot.by.month <- renderPlot({
            ggplot(data = visit.dataset.by.month[visit.dataset.by.month$disease%in%input$diseases,],
                   aes(colour = disease, x = Month, y = est, group = disease)) +
              geom_point(position=position_dodge2(width = 0.25))+
              geom_line(position=position_dodge2(width = 0.25)) +
              scale_x_continuous(breaks = 1:12) +
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge2(width = 1))+
              theme_bw()+
              labs(y = "Proportionate contribution (%)", title = "Proportionate contribution of selected disease(s) over time")
          })
          output$visit.plot.by.sex <- renderPlot({
            ggplot(data = visit.dataset.by.sex[visit.dataset.by.sex$disease%in%input$diseases,],
                   aes(fill = disease, x = Sex, y = est, group = disease)) +
              geom_bar(position=position_dodge(),stat="identity")+
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
              theme_bw()+
              labs(y = "Proportionate contribution (%)", 
                   title = "Proportionate contribution of selected disease(s) by sex")
          })
          output$visit.plot.by.age <- renderPlot({
            ggplot(data = visit.dataset.by.age[visit.dataset.by.age$disease%in%input$diseases,],
                   aes(fill = disease, x = Age, y = est, group = disease)) +
              geom_bar(position=position_dodge(),stat="identity")+
              geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, size = 0.7, position=position_dodge(width = 0.9))+
              theme_bw()+
              labs(y = "Proportionate contribution (%)", 
                   title = "Proportionate contribution of selected disease(s) by age")
          })
          output$diseases.table <- renderDT(
            visit.dataset.combined[visit.dataset.combined$disease%in%input$diseases,], server = TRUE,rownames = FALSE,
            extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              pageLength = visit$months * 5)
          )
          output$uiCompare <- renderUI({
            tabsetPanel(
              tabPanel("Figures",
                       verticalLayout(
                         plotOutput("visit.plot.by.month", width = visit$months * 50 * length(input$diseases)),
                         plotOutput("visit.plot.by.sex", width = 180*length(input$diseases)),
                         plotOutput("visit.plot.by.age", width = 300*length(input$diseases))
                       )),
              tabPanel("Table", DTOutput("diseases.table"))
            )
          })
        }
      })
      output$uiresults <- renderUI(
        {
          navbarPage("Results: Proportionate contribution (%)",
                     navbarMenu("ARI",tabPanel("AURI", uiOutput("uiAURI")),
                                tabPanel("ALRI", uiOutput("uiALRI"))),
                     navbarMenu("ADD",tabPanel("ABD", uiOutput("uiABD")), 
                                tabPanel("AOD", uiOutput("uiAOD")), 
                                tabPanel("AWD", uiOutput("uiAWD"))),
                     tabPanel("AJS", uiOutput("uiAJS")),
                     navbarMenu("AFI", tabPanel("AFI", uiOutput("uiAFI")), 
                                tabPanel("Malaria", uiOutput("uiMalaria"))),
                     tabPanel("Comparisons between diseases", verticalLayout(
                       selectInput('diseases', 'Select diseases from below',
                                   c("AURI", "ALRI",
                                     "ABD", "AOD", "AWD",
                                     "AJS", "AFI", "Malaria"), multiple=TRUE, selectize=TRUE),
                       actionButton("action.diseases", "Confirm selection"),
                       uiOutput("uiCompare")
                     )))
        }
      )
    })
    deno <<- deno
    visit <<- visit

    
  

  })
})
