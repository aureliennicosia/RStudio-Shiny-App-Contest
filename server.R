server <- function(input, output, session) {
  
### Data ----------------------
  
  app.waiting<-read.csv("03-Rapport/waiting_apps.csv")
  
  
  for(i in seq(ncol(app.waiting))){
    
    app.waiting[, i] <- as.character(app.waiting[, i])
    
  }
  
  communication<-read.csv("03-Rapport/communication.csv")
  
  
  for(i in seq(ncol(communication))){
    
    communication[, i] <- as.character(communication[, i])
    
  }

  
  ### App -----------------------------
  
  #### Template to ask a question to the management ----
  
  observeEvent(input$question,{
    shinyalert(
      html=T,
      text=tagList(
        textAreaInput('q_email','Your email adress : '),
        textAreaInput('q_name','Your name : '),
        textAreaInput('q_question','Your message : '),
        actionButton('q_send', label = "Send", icon = icon("external-link-alt"))),
      type="info",
      showConfirmButton = FALSE,
      closeOnClickOutside = TRUE
    )
  })
  
  #### Ask a question to management -----
  
  observeEvent(input$q_send,{
    # x<-reactiveVal(tibble(
    #   Email=input$q_email,
    #   Name=input$q_name,
    #   Message=input$q_question
    # ))
    # y<-rbind(communication,x())
    #write.csv(y,'03-Rapport/communication.csv',row.names=F)
    shinyalert("Your message was sent !",type='success')
    
  })
  
  #### Apply filter to the apps------
  
 app.infos<- reactive({
    if(input$sort=="Most Recent" && input$category=="All Categories"){
      app.infos1 %>% arrange(Date)
    }else if(input$sort=="Most Popular" && input$category=="All Categories"){
      app.infos1 %>% arrange(desc(as.numeric(like)))
    }else if(input$sort=="Alphabetical Order" && input$category=="All Categories"){
      app.infos1 %>% arrange(app_name)
    }else if(input$sort=="Most Recent" && input$category!="All Categories"){
      app.infos1 %>% arrange(Date)%>%filter(category==input$category)
    }else if(input$sort=="Most Popular" && input$category!="All Categories"){
      app.infos1 %>% arrange(desc(as.numeric(like)))%>%filter(category==input$category)
    }else if(input$sort=="Alphabetical Order" && input$category!="All Categories"){
      app.infos1 %>% arrange(app_name)%>%filter(category==input$category)
    }
  })
  

 reactive({ 
 for(i in seq(ncol(app.infos()))){
    
    app.infos()[, i] <- as.character(app.infos()[, i])
    
  }
   })
  
 
 app.search <- reactive({
   
   return(app.infos()[grepl(toupper(input$searchText), toupper(app.infos()$app_name)) | grepl(toupper(input$searchText), toupper(app.infos()$category)), ])
   
 })
 
  output$box <- renderUI({
    
    if (nchar(input$searchText) == 0){
      
      lapply(seq(nrow(app.infos())), function(x){
        wellPanel(
          column(2, offset=10,
            h3(renderText(app.infos()$Date[x]))),
            div(tags$img(src = paste0(app.infos()$app_name[x], "_1.png"),width='100%',height=200),style='display: inline-block;'),
          br(),
          br(),
            h6(renderText(app.infos()$app_name[x])), 
            h5(renderText(app.infos()$category[x])),
            actionBttn(paste0("info", x), "More",color="warning", icon = icon("info-circle"),style="jelly"),
            
            br(),
            br(),
           actionBttn(paste0("like",x), "",color="success",style="jelly",icon=icon('thumbs-up')),
           p(id=paste0('nb_like',x),app.infos()$like[x]),
           actionBttn(paste0("dislike",x), "",color="danger",style="jelly",icon=icon('thumbs-down')),
           p(id=paste0('nb_dislike',x),app.infos()$dislike[x]),
           onclick(paste0("like",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_like',x),print(as.numeric(app.infos()$like[x])+1))),
           onclick(paste0("dislike",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_dislike',x),print(as.numeric(app.infos()$dislike[x])+1))),
          br(),
          br(),
          tags$a(href=app.infos()$url[x],"Click here to open the shiny",target="_blank"),
    style="width:617px;height:550px; display: inline-block;vertical-align: text-top; 
    margin-left: auto;
    margin-right: auto;
    text-align:center;")
        
        
        
      })
      
    }
    else {
      
      if(nrow(app.search() > 0)){
        
        
        lapply(seq(nrow(app.search())), function(x){
          
          wellPanel(
            column(2, offset=10,
                   h3(renderText(app.search()$Date[x]))),
            div(tags$img(src = paste0(app.search()$app_name[x], "_1.png"),width='100%',height=200),style='display: inline-block;'),
            br(),
            br(),
            h6(renderText(app.search()$app_name[x])), 
            h5(renderText(app.search()$category[x])),
            actionBttn(paste0("info_search", x), "More",color="warning", icon = icon("info-circle"),style="jelly"),
            
            br(),
            br(),
            actionBttn(paste0("like",x), "",color="success",style="jelly",icon=icon('thumbs-up')),
            p(id=paste0('nb_like',x),app.search()$like[x]),
            actionBttn(paste0("dislike",x), "",color="danger",style="jelly",icon=icon('thumbs-down')),
            p(id=paste0('nb_dislike',x),app.search()$dislike[x]),
            onclick(paste0("like",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_like',x),print(as.numeric(app.search()$like[x])+1))),
            onclick(paste0("dislike",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_dislike',x),print(as.numeric(app.search()$dislike[x])+1))),
            br(),
            br(),
            tags$a(href=app.search()$url[x],"Click here to open the shiny",target="_blank"),
            style="width:617px;height:550px; display: inline-block;vertical-align: text-top; 
    margin-left: auto;
    margin-right: auto;
    text-align:center;")
          
        })
        
      }
      else {print('No Result')}
    }
    
  })
  
  

  

  lapply(seq(nrow(isolate(app.infos()))), function(x){
   observeEvent(input[[paste0("info",x)]],{
     imgs <- c(paste0("www/",app.infos()$app_name[x],"_1.png"),
               paste0("www/",app.infos()$app_name[x],"_2.png"),
               paste0("www/",app.infos()$app_name[x],"_3.png"))
 
     sendSweetAlert(
       session = session,
       title = app.infos()$app_name[x],
       btn_labels = c("Close"),
       text = tagList(
         slickR::slickR(imgs,
                        width = "700px",
                        height = "400px"),
         HTML(
       "<br><b> Category : </b>",app.infos()$category[x],"<br><br>",
       "<b> Description : </b>", app.infos()$description[x],"<br><br>",
       "<b> Creator : </b>",app.infos()$creator[x],"(",app.infos()$desc_c[x],")","<br><br>"),
       tags$a(href=app.infos()$url_info[x],"Click here to see more info on the shiny",target="_blank")),
       type = "info", 
       html = TRUE,
       closeOnClickOutside = FALSE,
       width="900px")
     })
 })
  
  lapply(seq(nrow(isolate(app.search()))), function(x){
    observeEvent(input[[paste0("info_search",x)]],{
      imgs <- c(paste0("www/",app.search()$app_name[x],"_1.png"),
                paste0("www/",app.search()$app_name[x],"_2.png"),
                paste0("www/",app.search()$app_name[x],"_3.png"))
      
      sendSweetAlert(
        session = session,
        title = app.search()$app_name[x],
        btn_labels = c("Close"),
        text = tagList(
          slickR::slickR(imgs,
                         width = "700px",
                         height = "400px"),
          HTML(
            "<br><b> Category : </b>",app.search()$category[x],"<br><br>",
            "<b> Description : </b>", app.search()$description[x],"<br><br>",
            "<b> Creator : </b>",app.search()$creator[x],"(",app.search()$desc_c[x],")","<br><br>"),
          tags$a(href=app.search()$url_info[x],"Click here to see more info on the shiny",target="_blank")),
        type = "info", 
        html = TRUE,
        closeOnClickOutside = FALSE,
        width="900px")
    })
  })
  
  

  

  
  ### Developper ----------------------------------
  

  
  #### Submit an app to the management -----
  
  observeEvent(input$mailButton,{
    if (nrow(app.infos()[toupper(app.infos()$url) == toupper(input$link), ]) > 0){
      shinyalert("Sorry!", "This app is already available in the Shiny App Store", type = "error")
      
    }else{
      
      if (nrow(app.infos()[toupper(app.infos()$app_name) == toupper(input$appname), ]) > 0){
        shinyalert("Sorry!", "This name is already used by another app", type = "warning")
        
      }else{
        
        if (input$name !='' && input$email !='' && input$appname !='' && input$description !='' && input$link!='' &&
            is.null(input$screenshot_1)==F && is.null(input$screenshot_2)==F && is.null(input$screenshot_3)==F && is.null(input$category)==F ){
          shinyalert("Thank you!", "Your shiny will soon be added into the app", type = "success")

          {
            req(input$appname)
            req(input$screenshot)
            file_1 <- input$screenshot_1
            file_2 <- input$screenshot_2
            file_3 <- input$screenshot_3
            #file.copy(file_1$datapath, paste0("www/", as.character(input$appname), "_1.png"))
            #file.copy(file_1$datapath, paste0("www/", as.character(input$appname), "_2.png"))
            #file.copy(file_1$datapath, paste0("www/", as.character(input$appname), "_3.png"))
            
          }

          waiting_app <- app.waiting
          x <- data.frame(app_name = input$appname,
                          category = input$category,
                          description = input$appdescription,
                          url = input$link,
                          creator = input$name,
                          desc_c=input$description,
                          like=0,
                          dislike=0,
                          email=input$email,
                          Date=Sys.Date(),
                          url_info=input$link)
          y <- rbind(waiting_app,x)
          
          updateTextAreaInput(session,'appname',value='')
          updateTextAreaInput(session,'appdescription',value='')
          updateTextAreaInput(session,'link',value='')
          updateTextAreaInput(session,'name',value='')
          updateTextAreaInput(session,'description',value='')
          updateTextAreaInput(session,'email',value='')
          updateSelectizeInput(session, "category", choices = cat,options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          ))
          
          #write.csv(y,'03-Rapport/waiting_apps.csv',row.names=F)
        }else{
          
          shinyalert("Oops...", "Please, answer all the questions if you want to submit your application to the store", type = "warning")
          
        }}}
})
  
  
  ### Management ---------------------------------
  
  #### Waiting Room --------
  output$box_w <- renderUI({
      
      lapply(seq(nrow(app.waiting)), function(x){
        wellPanel(
          column(2, offset=10,
                 h3(renderText(app.waiting$Date[x]))),
          div(img(src = paste0(app.waiting$app_name[x], "_1.png"),width='100%',height=200),style='display: inline-block;'),
          br(),
          br(),
          h6(renderText(app.waiting$app_name[x])), 
          h5(renderText(app.waiting$category[x])),
          actionBttn(paste0("info_w", x), "More",color="warning", icon = icon("info-circle"),style="jelly"),
          
          br(),
          br(),
          actionBttn(paste0("like",x), "",color="success",style="jelly",icon=icon('thumbs-up')),
          p(id=paste0('nb_like',x),app.waiting$like[x]),
          actionBttn(paste0("dislike",x), "",color="danger",style="jelly",icon=icon('thumbs-down')),
          p(id=paste0('nb_dislike',x),app.waiting$dislike[x]),
          onclick(paste0("like",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_like',x),print(as.numeric(app.waiting$like[x])+1))),
          onclick(paste0("dislike",x),shinyjs::disable(paste0("like",x)) & shinyjs::disable(paste0("dislike",x)) & html(paste0('nb_dislike',x),print(as.numeric(app.waiting$dislike[x])+1))),
          br(),
          br(),
          tags$a(href=app.waiting$url[x],"Click here to open the shiny",target="_blank"),
          style="width:617px;height:550px; display: inline-block;vertical-align: text-top; 
    margin-left: auto;
    margin-right: auto;
    text-align:center;")
      })
 })
  
  lapply(seq(nrow(app.waiting)), function(x){
    observeEvent(input[[paste0("info_w",x)]],{
      
      
      imgs <- c(paste0("www/",app.waiting$app_name[x],"_1.png"),
                paste0("www/",app.waiting$app_name[x],"_2.png"),
                paste0("www/",app.waiting$app_name[x],"_3.png"))
      #shinyalert(title='Hey',text='test')
      sendSweetAlert(
        session = session,
        title = app.waiting$app_name[x],
        btn_labels = c("Close"),
        text = tagList(
          slickR::slickR(imgs,
                         width = "700px",
                         height = "400px"),
          HTML(
            "<br><b> Category : </b>",app.waiting$category[x],"<br><br>",
            "<b> Description : </b>", app.waiting$description[x],"<br><br>",
            "<b> Creator : </b>",app.waiting$creator[x],"(",app.waiting$desc_c[x],")","<br><br>"),
          tags$a(href=app.waiting$url_info[x],"Click here to see more info on the shiny",target="_blank")),
        type = "info", 
        html = TRUE,
        closeOnClickOutside = FALSE,
        width="900px"
      )
    })
  })
  
  #### Log-in ------------------
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  #### Ui of the manager page ------
  
  output$body <- renderUI({
    if (USER$login == TRUE ){
      tabBox(width=12,
        tabPanel(h6('Waiting Room'),
                 uiOutput('box_w')),
        tabPanel(h6('Approval'),
                 HTML('<p style="color:red; font-size:20px;"><u>Since the shiny is a proof of concept, 
                      the apps are not really added to the Apps page (when you click on approve) or removed 
                      from the Waiting Apps (when you click on decline).</p></u>'),
                br(),
                br(),
                 DTOutput('data'),
                 htmlOutput('myText'),
                 htmlOutput('myText2')),
        tabPanel(h6('Communication'),
                 HTML('<p style="color:red; font-size:20px;"><u>Since the shiny is a proof of concept, 
                      the comments are not really removed (when you click on remove).</p></u>'),
                 br(),
                 br(),
                 DTOutput('communication_data'),
                 htmlOutput('myText3')))
    }else if (USER$login == F){
    
      div(id = "loginpage", style = "width: 800px; max-width: 100%; margin: 0 auto; padding: 20px;",
          
          wellPanel(
            tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
            textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
            passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
              shinyjs::hidden(
                div(id = "nomatch",
                    tags$p("Oops! Incorrect username or password!",
                           style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                           class = "text-center"))),
              br(),
              br(),
              tags$code("Username: manager  Password: manager"),
              br(),
              br(),
              HTML('<p style="color:red; font-size:20px;"><u>Since the shiny is a proof of concept, 
                      anyone can have access to this section. In reality, only the people that are maintaining the
               Shiny App Store would have access to it.</p></u>')
              
            ))
      )
    }
  })
  
  #### Approval of applications -----
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
app.waiting_df<-reactiveVal(
  tibble(
    AppName=app.waiting$app_name,
    Approve = shinyInput(
      FUN = actionButton,
      len=nrow(app.waiting),
      id = 'button_',
      label = "Approve",
      onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
    ),
    Decline = shinyInput(
      FUN = actionButton,
      len=nrow(app.waiting),
      id = 'button_',
      label = "Decline",
      onclick = 'Shiny.setInputValue(\"select_button_decline\", this.id, {priority: \"event\"})'
    )
  )
)

  output$data <- DT::renderDT({
    
    app.waiting_df()
    
  },  escape = FALSE, selection = 'none')
  
  myValue <- reactiveValues(approve = '',
                            decline='')
  
  observeEvent(input$select_button, {
    # take the value of input$select_button, e.g. "button_1"
    # get the button number (1) and assign to selectedRow
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    #y<-rbind(app.infos2,app.waiting[selectedRow,])
    #z <- app.waiting[-selectedRow,]
    #app.waiting<-z
    #write.csv(y,'03-Rapport/data_new_app.csv',row.names = F)
    #write.csv(z,'03-Rapport/waiting_apps.csv',row.names = F)
    
    # get the value of the "Name" column in the data.frame for that row
    myValue$approve <- paste('<i class="fas fa-check-circle" style = "color:MediumSeaGreen;"></i><font size=5px><b>',
                             app.waiting[selectedRow,"app_name"], 
                             "was added to the Market Place",'</b></font>')
  })
  
  observeEvent(input$select_button_decline, {
    # take the value of input$select_button, e.g. "button_1"
    # get the button number (1) and assign to selectedRow
    selectedRow <- as.numeric(strsplit(input$select_button_decline, "_")[[1]][2])
    
    #x <- app.waiting[-selectedRow,]
    #app.waiting<-x
    #write.csv(x,'03-Rapport/waiting_apps.csv',row.names = F)

   
    # get the value of the "Name" column in the data.frame for that row
    myValue$decline <- paste('<i class="fas fa-exclamation-triangle" style = "color:Tomato;"></i><font size=5px><b>',
                             app.waiting[selectedRow,"app_name"], 
                             "was declined",'</b></font>')
  })
  
  # Show the name of the employee that has been clicked on
  output$myText <- renderText({
    
    myValue$approve
    
  })
  
  output$myText2 <- renderText({

    myValue$decline
    
  })
  
  #### Communication from the users ------
  
  myValue_communication <- reactiveValues(remove='')
  
  communication_df<-reactiveVal(
    tibble(
      Email=communication$Email,
      Name=communication$Name,
      Message=communication$Message,
      Remove = shinyInput(
        FUN = actionButton,
        len = nrow(communication),
        id = 'button2_',
        label = "Remove",
        onclick = 'Shiny.setInputValue(\"select_remove\", this.id, {priority: \"event\"})'
      )
    )
  )
  output$communication_data<-renderDT({
    communication_df()
  },escape = FALSE, selection = 'none')
  
  observeEvent(input$select_remove, {
    selectedRow <- as.numeric(strsplit(input$select_remove, "_")[[1]][2])

    #c <- communication[-selectedRow,]
    #communication<-c
    #write.csv(c,'03-Rapport/communication.csv',row.names = F)
    
    myValue_communication$remove<-paste('<i class="fas fa-exclamation-triangle" style = "color:Tomato;"></i><font size=5px><b>',
                                        "You will not see ",communication[selectedRow,"Name"], 
                                        "comment the next time you connect.",'</b></font>')
  
    
  })
  

  output$myText3 <- renderText({
    
    myValue_communication$remove
    
  })
  

  
  
  

  

  
  
}
  
      