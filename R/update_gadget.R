#'work on files is a function to load the historic file from the file path
#'@importFrom utils read.csv
#'@noRd
work_on_file <- function(.path){
  read.csv(.path, header = TRUE, row.names = NULL, stringsAsFactors = FALSE)
}

#'Shoud I use custom historic data?
#'@noRd
is_historic <- function(.path, keepwide){
  #lecture du data.frame
  data <-  work_on_file(.path)
  #par defaut je suppose que c'est long
  hdta <- historic_long_data
  #on test s'il faut garder le wide ou pas
  if(keepwide) hdta <- historic_wide_data
  #ici  je m'assure que j'ai les bons noms de colonne
  all(colnames(data)== colnames(hdta))
}

#' update using a file or a folder
#'@noRd
update_function <- function(.path, keepwide, up, folder){
  if(folder) return(folder_update(.path, keepwide = keepwide,
                                  update = up))
  file_update(.path, keepwide = keepwide, update = up)
}

#' Updating the borno data using a friendly user interface
#'@import miniUI
#'@import shiny
#'@importFrom DT datatable
#'@importFrom DT renderDT
#'@importFrom DT DTOutput
#'@importFrom magrittr %>%
#'@importFrom dplyr bind_rows
#'@importFrom dplyr distinct
#'@importFrom utils write.csv
#'@export
update_addin <- function(){
  #User interface in french
  ui <- miniPage(
    gadgetTitleBar("UPDATE BORNO DATA",   right = NULL),
    miniTabstripPanel(
      miniTabPanel("Parametres", icon = icon("sliders"),
                   miniContentPanel(
                     fillRow(
                       flex = c(1, 0.25, 1),
                       fillCol(
                         miniContentPanel(
                           h2("Aide d'utilisation"),
                           p(" Effectuez les choix necessaires pour faire un update des donnees et cliquez ensuite
                          sur le bouton Terminer. Les choix sont les suivants:"),
                           hr(),
                           h4("1- Update ou traitement"),
                           p("Par defaut, on suppose que vous voulez effectuer un update des donnees historiques disponibles avec
                         les nouvelles donnees que vous aurez saisies. Si vous ne voulez traiter que les nouvelles donnees
                         que vous avez en faisant fi des anciennes donnees disponibles, selectionnez l'option 'Nouvelles donnees uniquement'."),
                           hr(),
                           h4("2- Un seul fichier pdf ou un Dossier pdf?"),
                           p("Vous avez la possiblite de travailler pour faire un update d'un seul dossier de pdfs ou d'un fichier pdf.
                         S'il s'agit d'un dossier de pdfs (qui respectent tous le format de borno), choisissez l'option dossier pdf.
                         Dans le cas contraire, choisissez l'option fichier pdf."),
                           hr(),
                           h4("3- Selectionnez le format de table"),
                           p("Decidez si vous voulez obtenir un format de table wide (ou les noms des maladies sont en colonnes avec les cas et les deces),
                         ou un format long (ou les maladies constituent une variable)"),
                           hr(),
                           h4("4- Nouvelles donnees historiques"),
                           p("Si vous avez des donnees historiques recentes que vous voulez utiliser a la place des donnees historiques dans le package,
                         vous pouvez le preciser en cochant cette case, il vous sera ensuite demander de preciser le chemin vers les nouvelles
                         donnees historiques. Il devra s'agir d'un fichier csv avec les noms de colonnes comme le tableau en sorti.")
                         )
                       ),
                       fillCol(),
                       fillCol(
                         wellPanel(
                           selectInput("format_final",
                                       label = "1- Voulez-vous faire un update ou traiter juste de nouvelles donnees?",
                                       choices = c("Update avec donnees historiques", "Nouvelles donnees uniquement")
                           ),
                           selectInput("format_chemin",
                                       label = "2- Un seul fichier pdf ou un Dossier de PDFs?",
                                       choices = c("Un Dossier", "Un seul fichier")
                           ),
                           textInput("chemin", "chemin vers le fichier ou le dossier"),
                           selectInput("format_table",
                                       label = "3- Selectionnez le format de table",
                                       choices = c("format long", "format wide")
                           ),
                           checkboxInput("format_historique",
                                         "4- Nouvelles donnees historiques (Dechochez sinon)",
                                         value = FALSE),
                           uiOutput("historic_data"),
                           textOutput("loaded_historic_data"),
                           miniButtonBlock(
                             miniTitleBarButton("done", "Effectuer le traitement", primary = TRUE)
                           )
                         )
                       )
                     )
                   )
      ),
      miniTabPanel("Tableau", icon = icon("table"),
                   miniContentPanel(
                     DTOutput("tableau_final"),
                     downloadButton("download", "Telecharger la base de donnees")
                   )


      )
    )
  )

  #server side
  server <- function(input, output){

    #loading the historic data
    output$historic_data <- renderUI({
      if(input$format_historique){
        fileInput("chemin_data_historique",
                  label = "Entrez le chemin vers la base historique (csv)",
                  accept = "csv")
      }
    })

    #Definition des variables
    #faire un update
    must_update <- reactive({
      input$format_final == "Update avec donnees historiques" | !is.null(loaded_data())
    })

    #folder or file
    folder_format <- reactive({
     input$format_chemin == "Un Dossier"
    })

    #table_format
    keepwide_format <- reactive({
      input$format_table == "format long"
    })

    loaded_data <- reactive({
      if(input$format_historique){
        infile <- input$chemin_data_historique
        if(is.null(infile)) return(NULL)
        if(is_historic(infile$datapath, keepwide_format())) return(work_on_file(infile$datapath))
        return(NULL)
      }
    })

    #On effectue une sortie pour s'assurer que l'utilisateur corrige.
    output$loaded_historic_data <- renderText({
      validate(
        need(try(input$format_historique), "cochez le 4. si vous voulez ajouter vos propres donnees historiques"),
        need(try(is.data.frame(loaded_data())),
             "Choisissez un fichier .csv avec les bons noms de colonnes (regardez un output sans donnees historiques)")
      )
      "Les donnees ont ete bien chargees"
    })



    #output_data
    output_data <- reactive({
      bind_rows(loaded_data(),
                update_function(input$chemin,
                                keepwide = keepwide_format(),
                                up = must_update(),
                                folder = folder_format()
                )
      ) %>%
        distinct()
    })

    #Now I will render the datatable
    observeEvent(input$done,
                 output$tableau_final <- renderDT(output_data(), extensions = "Buttons",
                                                  filter = "top", options = list(
                                                                    dom = "Bfrtip",
                                                                    buttons = c("csv", "excel"),
                                                                    pageLength = 20
                                                                  )
                                                  )
    )

    output$download <- downloadHandler(
      filename = paste("borno_data", Sys.Date(), ".csv", sep = ""),
      content = function(file) write.csv(output_data(), file, row.names = FALSE)
    )

  }

  #viewer
  viewer <- dialogViewer("Update Borno data", width = 900, height = 1200)

  #running
  runGadget(ui, server, viewer = viewer)
}

