



ui<-tagList(
  shinyjs::useShinyjs(),
  useShinydashboardPlus(),
  useShinydashboard(),
  tags$style(HTML(
    '.content-wrapper {float:top; margin-top:70px}
                        p { display: inline }

                        * { font-family: Arial; }
                        h1 {
                              color:  #008ACC;
                              font-size: 15px;
                            }
                        h2 {
                              color:  #008ACC;
                              font-size: 15px;
                              text-transform: uppercase;
                        }
                        h3 {
                              color:  #686868;
                              font-size: 12px;
                            }
                        h5 {
                              color:  #686868;
                              font-size: 15px;
                              font-style: italic
                        }
                        h6 {
                              color:  #000000;
                              font-size: 18px;
                              font-weight: bold;
                            }
                        /* body */.content-wrapper, .right-side {background-color: 	#F0F0F0;}
                        body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 95%; /* Webkit browsers */
}
                       '
  )),
  shinyalert::useShinyalert(),
  navbarPage(  title='Shiny App Store',
               #shinythemes::themeSelector(),
                 theme = shinytheme("flatly"),
                 windowTitle = "Shiny App Store",
                 fluid = TRUE, 
                 collapsible = TRUE,
                 tabPanel('Apps',
                          wellPanel(column(3,
                                      selectInput('sort','Sort',c('Most Recent','Most Popular','Alphabetical Order'))),
                                    column(3,
                                      selectInput('category','Category',c('All Categories',cat))),
                                    column(4,  
                                           sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "..."),
                                           style='padding : 12px;'),
                                    column(2,
                                      actionBttn('question','Ask a question to management',style="jelly",color="warning",
                                                 icon=icon("question"),size="md"),
                                      style='padding : 15px;'),
                                    style='height : 120px;text-align: center;vertical-align: middle;'),
                          uiOutput('box'),
                          uiOutput('jus')),
                 tabPanel('Developper',
                          wellPanel(h6('Submit your app'),
                                    HTML('<p style="color:red; font-size:20px;"><u>
                                         Since the Shiny is a proof of concept, you cannot really add an application to the app. 
                                         You can fill the form, but nothing new will appear in the Waiting Room of the Management tab.</p></u>'),
                                    br(),
                                    br(),
                                    column(6,
                                    textAreaInput('name','Your name',placeholder = '...'),
                                    textAreaInput('email','Your email',placeholder = '...'),
                                    textAreaInput('description','Small description of yourself',placeholder = '...'),
                                    textAreaInput('appname','Name of the app',placeholder = '...'),
                                    textAreaInput('appdescription','Description of the app',placeholder = '...')),
                                    column(6,
                                    textAreaInput('link','Link to the app',placeholder = '...'),
                                    selectizeInput('category', 'Category', choices = cat,
                                      options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                                    HTML('<b><u> Please, upload 3 different screenshots of your app that are showing what it can do </u></b>'),
                                    
                                    br(),
                                    br(),
                                    fileInput("screenshot_1", ("First screenshot of your app"),
                                              buttonLabel = ("Browse"), placeholder = "...", accept = 'image/png'),
                                    fileInput("screenshot_2", ("Second screenshot of your app"),
                                              buttonLabel = ("Browse"), placeholder = "...", accept = 'image/png'),
                                    fileInput("screenshot_3", ("Third screenshot of your app"),
                                              buttonLabel = ("Browse"), placeholder = "...", accept = 'image/png'),
                                    ),
                                    br(),
                                    br(),
                                    column(6,offset=3,
                                    actionBttn("mailButton",label = ("Ask to put your shiny on the app"),color='warning',
                                               style = "jelly",icon = icon("share-square"))),
                                    style="width:800px;height:1000px; display: inline-block;vertical-align: text-top;"),
                          #mainPanel(
                          wellPanel(h6('Documentation'),
                                    HTML('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam volutpat erat sit amet ante volutpat, sed tempus sapien placerat. Nulla diam felis, finibus vitae scelerisque et, congue ac diam. Suspendisse vel tempus sapien. Vivamus quis diam vel sem tempus vehicula. Sed vulputate diam turpis, sit amet faucibus velit ultricies ut. Aliquam in viverra tortor, ut sollicitudin ipsum. Mauris convallis, neque eget finibus aliquet, ante tortor mollis mauris, quis sodales tellus quam ac magna. Nam mauris nibh, interdum nec condimentum ut, malesuada eget justo. Sed augue leo, mollis id dictum vel, interdum in nunc. Sed aliquet est a dui mollis laoreet.'),
                                    style="width:350px;height:1000px; display: inline-block;vertical-align: text-top;"),
                          wellPanel(h6('Guideline'),
                                    HTML('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam volutpat erat sit amet ante volutpat, sed tempus sapien placerat. Nulla diam felis, finibus vitae scelerisque et, congue ac diam. Suspendisse vel tempus sapien. Vivamus quis diam vel sem tempus vehicula. Sed vulputate diam turpis, sit amet faucibus velit ultricies ut. Aliquam in viverra tortor, ut sollicitudin ipsum. Mauris convallis, neque eget finibus aliquet, ante tortor mollis mauris, quis sodales tellus quam ac magna. Nam mauris nibh, interdum nec condimentum ut, malesuada eget justo. Sed augue leo, mollis id dictum vel, interdum in nunc. Sed aliquet est a dui mollis laoreet.'),
                                    style="width:350px;height:1000px; display: inline-block;vertical-align: text-top;"),
                          wellPanel(h6('Rules'),
                                    HTML('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam volutpat erat sit amet ante volutpat, sed tempus sapien placerat. Nulla diam felis, finibus vitae scelerisque et, congue ac diam. Suspendisse vel tempus sapien. Vivamus quis diam vel sem tempus vehicula. Sed vulputate diam turpis, sit amet faucibus velit ultricies ut. Aliquam in viverra tortor, ut sollicitudin ipsum. Mauris convallis, neque eget finibus aliquet, ante tortor mollis mauris, quis sodales tellus quam ac magna. Nam mauris nibh, interdum nec condimentum ut, malesuada eget justo. Sed augue leo, mollis id dictum vel, interdum in nunc. Sed aliquet est a dui mollis laoreet.'),
                                    style="width:350px;height:1000px; display: inline-block;vertical-align: text-top;")),
                                    #style="width:400px;height:350px; display: inline-block;vertical-align: text-top;"),
                 tabPanel('Management',   
                          uiOutput('body')
                          )

    ))



#shinyApp(ui,server)
