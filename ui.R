pacman::p_load(
  shiny,
  DT,
  shinyjs
)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .large-input input {
        height: 60px !important;
      }
      .table-actions { display: flex; gap: 8px; justify-content: flex-end; margin-bottom: 8px; }
      #annuler_modif {
        background-color: #a8bacc;
        color: white;
        border-color: #a8bacc;
      }
      table.dataTable tbody td { vertical-align: top; }
      .cell-fixed {
        max-height: 78px;
        overflow-y: auto;
        display: block;
        white-space: normal;
      }
      .info-icon{
        display:inline-block; margin-left:6px; width:18px; height:18px;
        line-height:18px; text-align:center; border-radius:50%;
        border:1px solid #17a2b8; color:#17a2b8; font-weight:700;
        font-family: Arial, sans-serif; cursor:help; font-size:12px;
      }
      .info-icon:hover{ background:#e8f7fb; }

      /* --- Table zoom mode --- */
      .zoom-dt #sidebar_panel { display: none !important; }
      .zoom-dt #main_panel { width: 100% !important; }
      .zoom-dt .dataTables_scrollBody {
        max-height: calc(100vh - 260px) !important;
      }
      .zoom-dt .col-sm-4 { display: none !important; }
      .zoom-dt .col-sm-8 { width: 100% !important; }

      /* --- Button: clear form (gray) --- */
      #effacer_masque {
        background-color: #6c757d !important;
        border-color: #6c757d !important;
        color: #ffffff !important;
      }

      /* --- Button: discard changes (secondary) --- */
      #discard_changes {
        background-color: #adb5bd !important;
        border-color: #adb5bd !important;
        color: #212529 !important;
      }

      /* --- Orange frame: export events --- */
      .export-box {
        border: 2px solid #ff8c00;
        padding: 10px;
        border-radius: 0px;
      }
    "))
  ),
  titlePanel("Event Reporting Tool"),
  tags$script(HTML('Shiny.addCustomMessageHandler("show_export_count", function(message) {
    alert("Number of selected events: " + message);
  });')),
  sidebarLayout(
    tags$div(
      id = "sidebar_panel",
      sidebarPanel(
        div(style = "display: flex; gap: 10px; margin-top: 20px;",
            actionButton("editer", label = "Load for editing", icon = icon("edit"), class = "btn btn-warning"),
            actionButton("supprimer", label = "Delete", icon = icon("trash"), class = "btn btn-danger")
        ),
        
        tags$div(style="margin-top:10px;",
                 actionButton("delete_for_good", label = "Delete for good (Deleted tab)", icon = icon("skull-crossbones"),
                              class = "btn btn-outline-danger")
        ),
        
        # Entry form
        div(class = "large-input", textInput("titre", "Event title", width = "100%")),
        div(class = "large-input", textInput("regions", "Regions (countries / regions mentioned)", width = "100%")),
        selectInput("categorie", "Alert category",
                    choices = c(
                      "",
                      "International and national alerts",
                      "National alerts",
                      "International alerts"
                    ),
                    selected = ""),
        selectInput("type_evenement", "Event type",
                    choices = c(
                      "",
                      "Human",
                      "Animal",
                      "Environment",
                      "Human and animal",
                      "Other"
                    ),
                    selected = ""),
        div(
          class = "large-input",
          textAreaInput(
            inputId = "sources",
            label = HTML(
              'Sources <span class="info-icon" title="Enter one source per line using: Name (link). Example: WHO (https://www.who.int)
ECDC (https://www.ecdc.europa.eu)">i</span>'
            ),
            width = "100%",
            rows = 4,
            placeholder = "One source per line:\nWHO (https://www.who.int)\nECDC (https://www.ecdc.europa.eu)"
          )
        ),
        
        dateInput("date", "Date of the latest data status",
                  value = Sys.Date(), format = "yyyy-mm-dd", language = "en"),
        textAreaInput("situation", "Epidemiological situation", rows = 10),
        textAreaInput("evaluation_risque", "Risk assessment", rows = 10),
        textAreaInput("mesures", "Measures taken", rows = 10),
        selectInput("pertinence_sante", "Public health relevance",
                    choices = c(
                      "",
                      "Very low",
                      "Low",
                      "Moderate",
                      "High"
                    ),
                    selected = ""),
        selectInput("pertinence_voyageurs", "Traveler relevance",
                    choices = c(
                      "",
                      "Very low",
                      "Low",
                      "Moderate",
                      "High"
                    ),
                    selected = ""),
        div(class = "large-input", textInput("commentaires", "Comments", width = "100%")),
        tags$div(style = "background-color: #ffffcc; padding: 10px; border-radius: 5px;",
                 textAreaInput("resume", "Disease summary", rows = 10)),
        div(class = "large-input", textInput("autre", "Other", width = "100%")),
        selectInput("statut", "Event status", choices = c("active", "closed", "deleted")),
        
        ####  BUTTONS ####
        
        # --- Import EIOS (CSV) ---
        tags$hr(),
        h4("EIOS"),
        fileInput(
          inputId = "upload_eios",
          label   = "Upload_EIOS (CSV)",
          accept  = c(".csv"),
          buttonLabel = "Choose a CSV file",
          placeholder = "No file selected"
        ),
        actionButton(
          inputId = "copier_vers_actifs",
          label   = "Copy selection to \"Active\"",
          icon    = icon("arrow-right"),
          class   = "btn btn-outline-warning"
        ),
        
        # --- Upload charts/images ---
        h4("Upload images"),
        tags$div(style = "margin-bottom: 10px;",
                 tags$label(HTML(
                   'Upload charts (linked to the selected event)
     <span class="info-icon" title="⚠️ Tip: Please select or load an event in the table before uploading images. Uploaded files will be linked to that event and will appear in the Word report.">i</span>'
                 ), style = "font-weight: bold;"),
                 fileInput(
                   inputId = "televerser_graphs",
                   label   = NULL,
                   multiple = TRUE,
                   accept   = c("image/png","image/jpeg","image/jpg","image/svg+xml"),
                   buttonLabel = "Choose files"
                 ),
                 tags$style(HTML("
    #televerser_graphs button, #televerser_graphs .btn {
      background-color: #ff8c00 !important;
      border-color:     #ff8c00 !important;
      color: #ffffff !important;
    }
  "))
        ),
        
        div(style = "margin-top: 20px;",
            h4("Add a new event"),
            div(style="display:flex; gap:10px; align-items:center;",
                actionButton("effacer_masque", label = "Clear form", icon = icon("eraser"), class = "btn btn-default"),
                actionButton("discard_changes", label = "Discard changes", icon = icon("undo"), class = "btn btn-default")
            ),
            tags$div(style="margin-top:10px;",
                     actionButton("enregistrer", label = "Add this event", icon = icon("plus"), class = "btn btn-success")
            )
        ),
        
        # --- Update + save as new event ---
        div(style = "margin-top: 20px;",
            actionButton("mettre_a_jour", label = "Save changes to the event", icon = icon("refresh"), class = "btn btn-primary"),
            br(), br(),
            actionButton("enregistrer_nouveau", label = "Save as a new event (update of the previous event)", icon = icon("copy"), class = "btn btn-info")
        ),
        
        # --- Export ---
        tags$div(
          class = "export-box",
          style = "margin-top: 20px;",
          h4("Export events"),
          actionButton("exporter_excel", label = "Export to Excel", icon = icon("file-excel"), class = "btn btn-outline-success",
                       onclick = 'Shiny.setInputValue("count_selected_excel", Math.random())'),
          actionButton("exporter_word", label = "Create a Word report", icon = icon("file-word"), class = "btn btn-outline-primary",
                       onclick = 'Shiny.setInputValue("count_selected_word", Math.random())')
        )
      )
    ),
    
    tags$div(
      id = "main_panel",
      mainPanel(
        dateRangeInput("filtre_date", "Filter by date", start = Sys.Date() - 30, end = Sys.Date()),
        actionButton("reset_filtre_date", "Reset date filter", icon = icon("undo")),
        
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "champ_filtre",
              label   = "Search field",
              choices = c(
                "All fields"      = "tous",
                "Title"           = "Title",
                "Sources"         = "Sources",
                "Situation"       = "Situation",
                "Measures"        = "Measures",
                "Summary"         = "Summary",
                "Risk assessment" = "Risk_Assessment",
                "Comments"        = "Comments",
                "Other"           = "Other",
                "Regions"         = "Regions",
                "EIOS_id"         = "EIOS_id"
              ),
              selected = "tous"
            )
          ),
          column(
            width = 4,
            textInput(
              inputId = "texte_filtre",
              label   = "Keyword",
              value   = ""
            )
          )
        ),
        
        div(class = "table-actions",
            actionButton("select_all", "Select all events", icon = icon("check-double"),
                         class = "btn btn-outline-secondary btn-sm"),
            actionButton("clear_all",  "Clear selection", icon = icon("ban"),
                         class = "btn btn-outline-secondary btn-sm"),
            actionButton("zoom_table", "Zoom table", icon = icon("expand"),
                         class = "btn btn-outline-secondary btn-sm")
        ),
        
        tabsetPanel(id = "onglets",
                    tabPanel("Active",
                             DTOutput("tableau_evenements_actifs")
                    ),
                    tabPanel("Closed",
                             DTOutput("tableau_evenements_termines")
                    ),
                    tabPanel("EIOS",
                             DTOutput("tableau_eios")
                    ),
                    tabPanel("Deleted",
                             DTOutput("tableau_evenements_supprimes")
                    )
        )
      )
    )
  )
)