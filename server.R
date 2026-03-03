pacman::p_load(
  shiny,
  shinyjs,
  DT,
  officer,
  flextable,
  readr,
  dplyr,
  writexl,
  magick,
  lubridate,
  stringr,
  renv
)

# ---- Link to store data ----
fichier_donnees <- "events_data.csv"

server <- function(input, output, session) {
  useShinyjs()
  
  # =========================================================
  # Helpers: clear form + fill form from a row
  # =========================================================
  clear_form <- function() {
    updateTextInput(session, "titre", value = "")
    updateSelectInput(session, "type_evenement", selected = "")
    updateSelectInput(session, "categorie", selected = "")
    updateTextInput(session, "regions", value = "")
    updateTextInput(session, "sources", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateDateRangeInput(session, "filtre_date", start = Sys.Date() - 30, end = Sys.Date())
    updateTextAreaInput(session, "situation", value = "")
    updateTextAreaInput(session, "evaluation_risque", value = "")
    updateTextAreaInput(session, "mesures", value = "")
    updateSelectInput(session, "pertinence_sante", selected = "")
    updateSelectInput(session, "pertinence_voyageurs", selected = "")
    updateTextInput(session, "commentaires", value = "")
    updateTextAreaInput(session, "resume", value = "")
    updateTextInput(session, "autre", value = "")
    updateSelectInput(session, "statut", selected = "active")
    
    # reset editing markers
    session$userData$indice_edition <- NULL
    session$userData$event_loaded_original <- NULL
  }
  
  fill_form_from_event <- function(evenement) {
    updateTextInput(session, "titre", value = evenement$Title)
    updateSelectInput(session, "type_evenement",
                      selected = ifelse(is.na(evenement$Event_Type) || evenement$Event_Type == "", "", evenement$Event_Type))
    updateSelectInput(session, "categorie",
                      selected = ifelse(is.na(evenement$Alert_Category) || evenement$Alert_Category == "", "", evenement$Alert_Category))
    updateTextInput(session, "regions", value = ifelse(is.na(evenement$Regions), "", evenement$Regions))
    updateTextInput(session, "sources", value = ifelse(is.na(evenement$Sources), "", evenement$Sources))
    updateSelectInput(session, "statut", selected = ifelse(is.na(evenement$Status) || evenement$Status == "", "active", evenement$Status))
    updateDateInput(session, "date", value = as.Date(evenement$Date))
    updateTextAreaInput(session, "situation",          value = ifelse(is.na(evenement$Situation), "", evenement$Situation))
    updateTextAreaInput(session, "evaluation_risque",  value = ifelse(is.na(evenement$Risk_Assessment), "", evenement$Risk_Assessment))
    updateTextAreaInput(session, "mesures",            value = ifelse(is.na(evenement$Measures), "", evenement$Measures))
    updateSelectInput(session, "pertinence_sante",
                      selected = ifelse(is.na(evenement$Public_Health_Relevance) || evenement$Public_Health_Relevance == "", "", evenement$Public_Health_Relevance))
    updateSelectInput(session, "pertinence_voyageurs",
                      selected = ifelse(is.na(evenement$Traveler_Relevance) || evenement$Traveler_Relevance == "", "", evenement$Traveler_Relevance))
    updateTextInput(session, "commentaires", value = ifelse(is.na(evenement$Comments), "", evenement$Comments))
    updateTextAreaInput(session, "resume",    value = ifelse(is.na(evenement$Summary), "", evenement$Summary))
    updateTextInput(session, "autre",         value = ifelse(is.na(evenement$Other), "", evenement$Other))
  }
  
  # =========================================================
  # Reactive file reader: when events_data.csv changes, all clients see the update.
  # =========================================================
  donnees <- reactivePoll(
    intervalMillis = 2000L,
    session,
    checkFunc = function() {
      fi <- file.info(fichier_donnees)
      if (is.na(fi$mtime)) return(0)
      paste(fi$mtime, fi$size, sep = "_")
    },
    valueFunc = function() {
      tryCatch({
        df_lu <- readr::read_csv(
          fichier_donnees, show_col_types = FALSE,
          col_types = cols(
            Version = col_double(),
            Date = col_date(),
            Last_Modified = col_character()
          )
        )
        if ("Last_Modified" %in% names(df_lu)) {
          df_lu$Last_Modified <- lubridate::ymd_hms(df_lu$Last_Modified, tz = "Africa/Johannesburg")
        }
        if (!"Images" %in% names(df_lu)) df_lu$Images <- NA_character_
        df_lu
      }, error = function(e) {
        data.frame()
      })
    }
  )
  
  # --- Button: table zoom (show/hide the sidebar) ---
  observeEvent(input$zoom_table, {
    shinyjs::toggleClass(selector = "body", class = "zoom-dt")
    shinyjs::runjs("setTimeout(function(){ $(window).trigger('resize'); }, 150);")
  }, ignoreInit = TRUE)
  
  # --- EIOS data in memory (EIOS tab) ---
  eios_donnees <- reactiveVal(
    data.frame(
      Sources   = character(),
      Title     = character(),
      Situation = character(),
      Other     = character(),
      Regions   = character(),
      EIOS_id   = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # --- Folders & utilities for uploaded images ---
  dir.create("Graphs_uploaded", showWarnings = FALSE)
  
  # Function: get current Event_ID (editing takes priority, otherwise selection in the active tab)
  id_evenement_courant <- function() {
    df_full <- donnees()
    
    # 1) if an event is loaded for editing, use that row
    if (!is.null(session$userData$indice_edition)) {
      idx <- session$userData$indice_edition
      if (is.finite(idx) && idx <= nrow(df_full)) {
        return(df_full$Event_ID[idx])
      }
    }
    
    # 2) otherwise, use the selection from the active tab table
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      sel <- input$tableau_evenements_actifs_rows_selected
      if (length(sel) == 1) return(donnees_actifs_filtrees()$Event_ID[sel])
    } else if (identical(onglet, "Closed")) {
      sel <- input$tableau_evenements_termines_rows_selected
      if (length(sel) == 1) return(donnees_termines_filtrees()$Event_ID[sel])
    } else if (identical(onglet, "Deleted")) {
      sel <- input$tableau_evenements_supprimes_rows_selected
      if (length(sel) == 1) return(donnees_supprimes_filtrees()$Event_ID[sel])
    }
    
    return(NA_character_)
  }
  
  # Function: next image_N index for a given Event_ID
  prochain_index_image <- function(id_evt) {
    dir_evt <- file.path("Graphs_uploaded", id_evt)
    if (!dir.exists(dir_evt)) return(1L)
    exist <- list.files(dir_evt, pattern = "^image_[0-9]+\\.", full.names = FALSE)
    if (length(exist) == 0) return(1L)
    nums <- suppressWarnings(as.integer(gsub("^image_([0-9]+)\\..*$", "\\1", exist)))
    max(nums, na.rm = TRUE) + 1L
  }
  
  # Function: compute height to keep aspect ratio (target width in inches)
  taille_img <- function(path, largeur_cible = 6) {
    info <- tryCatch(magick::image_read(path), error = function(e) NULL)
    if (is.null(info)) return(list(w = largeur_cible, h = 4.5))
    dim <- magick::image_info(info)
    h <- (dim$height / dim$width) * largeur_cible
    if (!is.finite(h) || h <= 0) h <- 4.5
    list(w = largeur_cible, h = h)
  }
  
  # =========================================================
  # Filters
  # =========================================================
  filtrer_par_texte <- function(df) {
    terme <- input$texte_filtre
    champ <- input$champ_filtre
    
    if (is.null(terme) || !nzchar(trimws(terme)) || nrow(df) == 0) {
      return(df)
    }
    
    terme <- trimws(terme)
    cols_tous <- c("Event_ID","Version","Event_Type","Alert_Category","Title","Sources","Situation","Other","Regions","EIOS_id")
    
    if (identical(champ, "tous")) {
      cols_existe <- intersect(cols_tous, colnames(df))
      if (length(cols_existe) == 0) return(df)
      idx <- apply(
        as.data.frame(df[, cols_existe, drop = FALSE], stringsAsFactors = FALSE),
        1,
        function(ligne) any(grepl(terme, ligne, ignore.case = TRUE))
      )
      df[idx, , drop = FALSE]
    } else {
      if (!champ %in% colnames(df)) return(df)
      df[grepl(terme, df[[champ]], ignore.case = TRUE), , drop = FALSE]
    }
  }
  
  # --- Active: filter + sort from newest to oldest ---
  donnees_actifs_filtrees <- reactive({
    df <- donnees()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "active")
    if (!is.null(input$filtre_date[1]) && !is.null(input$filtre_date[2])) {
      df <- dplyr::filter(df, Date >= input$filtre_date[1], Date <= input$filtre_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filtrer_par_texte(df)
  })
  
  # --- Closed: filter + sort from newest to oldest ---
  donnees_termines_filtrees <- reactive({
    df <- donnees()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "closed")
    if (!is.null(input$filtre_date[1]) && !is.null(input$filtre_date[2])) {
      df <- dplyr::filter(df, Date >= input$filtre_date[1], Date <= input$filtre_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filtrer_par_texte(df)
  })
  
  # --- Deleted: filter + sort from newest to oldest ---
  donnees_supprimes_filtrees <- reactive({
    df <- donnees()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "deleted")
    if (!is.null(input$filtre_date[1]) && !is.null(input$filtre_date[2])) {
      df <- dplyr::filter(df, Date >= input$filtre_date[1], Date <= input$filtre_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filtrer_par_texte(df)
  })
  
  # =========================================================
  # Editing markers
  # =========================================================
  session$userData$indice_edition <- NULL
  session$userData$event_loaded_original <- NULL
  
  # =========================================================
  # Add new event (auto-clear mask after save)
  # =========================================================
  observeEvent(input$enregistrer, {
    nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                           sum(grepl(format(Sys.Date(), "%Y%m%d"), donnees()$Event_ID)) + 1)
    
    nouvel_evenement <- data.frame(
      Event_ID = nouvelle_id,
      Update = "New event",
      Version = 1,
      Date = input$date,
      Last_Modified = lubridate::now(tzone = "Africa/Johannesburg"),
      Title = input$titre,
      Event_Type = input$type_evenement,
      Alert_Category = input$categorie,
      Regions = input$regions,
      Sources = input$sources,
      Situation = input$situation,
      Risk_Assessment = input$evaluation_risque,
      Measures = input$mesures,
      Public_Health_Relevance = input$pertinence_sante,
      Traveler_Relevance = input$pertinence_voyageurs,
      Comments = input$commentaires,
      Summary = input$resume,
      Other = input$autre,
      Status = input$statut,
      EIOS_id   = NA_character_,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      readr::write_csv(dplyr::bind_rows(donnees(), nouvel_evenement), fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving: ", e$message))
      return()
    })
    
    showModal(modalDialog(
      title = "Success",
      "The event was added successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    clear_form()
  })
  
  # =========================================================
  # Clear form button
  # =========================================================
  observeEvent(input$effacer_masque, {
    clear_form()
  })
  
  # =========================================================
  # Discard changes (revert to last loaded event without saving)
  # =========================================================
  observeEvent(input$discard_changes, {
    if (!is.null(session$userData$event_loaded_original)) {
      evenement <- session$userData$event_loaded_original
      fill_form_from_event(evenement)
    } else {
      clear_form()
    }
  })
  
  # =========================================================
  # Save as new event (update of previous) (auto-clear after save)
  # =========================================================
  observeEvent(input$enregistrer_nouveau, {
    indice <- session$userData$indice_edition
    
    if (is.null(indice)) {
      showModal(modalDialog("Please load an event to edit first using the 'Load for editing' button."))
      return()
    }
    
    donnees_actuelles <- donnees()
    evenement_original <- donnees_actuelles[indice, ]
    
    nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                           sum(grepl(format(Sys.Date(), "%Y%m%d"), donnees_actuelles$Event_ID)) + 1)
    
    original_id <- evenement_original$Event_ID[1]
    
    nouvel_evenement <- evenement_original %>%
      dplyr::mutate(
        Event_ID = nouvelle_id,
        Update = paste0("Update of event: ", original_id),
        Version = evenement_original$Version + 1,
        Last_Modified = lubridate::with_tz(Sys.time(), "Africa/Johannesburg"),
        Title = input$titre,
        Event_Type = input$type_evenement,
        Alert_Category = input$categorie,
        Regions = input$regions,
        Sources = input$sources,
        Date = input$date,
        Situation = input$situation,
        Risk_Assessment = input$evaluation_risque,
        Measures = input$mesures,
        Public_Health_Relevance = input$pertinence_sante,
        Traveler_Relevance = input$pertinence_voyageurs,
        Comments = input$commentaires,
        Summary = input$resume,
        Other = input$autre,
        Status = input$statut
      ) %>%
      dplyr::select(Event_ID, Update, dplyr::everything())
    
    tryCatch({
      readr::write_csv(dplyr::bind_rows(donnees_actuelles, nouvel_evenement), fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving: ", e$message))
      return()
    })
    
    showModal(modalDialog(
      title = "Success",
      "The new event was saved successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    clear_form()
  })
  
  # =========================================================
  # Save changes to existing row (auto-clear after save)
  # =========================================================
  observeEvent(input$mettre_a_jour, {
    indice <- session$userData$indice_edition
    if (is.null(indice)) return()
    
    donnees_actuelles <- donnees()
    
    if (indice > nrow(donnees_actuelles)) {
      showModal(modalDialog("Error: the item to update no longer exists."))
      return()
    }
    
    donnees_actuelles[indice, ] <- donnees_actuelles[indice, ] %>%
      dplyr::mutate(
        Title = input$titre,
        Event_Type = input$type_evenement,
        Alert_Category = input$categorie,
        Regions = input$regions,
        Sources = input$sources,
        Date = input$date,
        Situation = input$situation,
        Risk_Assessment = input$evaluation_risque,
        Measures = input$mesures,
        Public_Health_Relevance = input$pertinence_sante,
        Traveler_Relevance = input$pertinence_voyageurs,
        Comments = input$commentaires,
        Summary = input$resume,
        Other = input$autre,
        Status = input$statut,
        Last_Modified = lubridate::now(tzone = "Africa/Johannesburg")
      )
    
    readr::write_csv(donnees_actuelles, fichier_donnees)
    clear_form()
  })
  
  # =========================================================
  # Load for editing (also store original snapshot for discard)
  # =========================================================
  observeEvent(input$editer, {
    onglet <- input$onglets
    df_full <- donnees()
    
    if (identical(onglet, "Active")) {
      selection <- input$tableau_evenements_actifs_rows_selected
      df_view   <- donnees_actifs_filtrees()
    } else if (identical(onglet, "Closed")) {
      selection <- input$tableau_evenements_termines_rows_selected
      df_view   <- donnees_termines_filtrees()
    } else if (identical(onglet, "Deleted")) {
      selection <- input$tableau_evenements_supprimes_rows_selected
      df_view   <- donnees_supprimes_filtrees()
    } else {
      showModal(modalDialog("No editable tab detected."))
      return()
    }
    
    if (length(selection) != 1) {
      showModal(modalDialog("Please select exactly one (1) event in the current tab."))
      return()
    }
    
    evenement <- df_view[selection, ]
    
    idx <- which(df_full$Event_ID == evenement$Event_ID)
    if (length(idx) != 1) {
      showModal(modalDialog("Unable to find the selected event in the full dataset."))
      return()
    }
    
    fill_form_from_event(evenement)
    
    session$userData$indice_edition <- idx
    session$userData$event_loaded_original <- evenement
  })
  
  # =========================================================
  # Export count popups
  # =========================================================
  observeEvent(input$count_selected_excel, {
    selected <- input$tableau_evenements_actifs_rows_selected
    session$sendCustomMessage("show_export_count", length(selected))
  })
  
  observeEvent(input$count_selected_word, {
    selected <- input$tableau_evenements_actifs_rows_selected
    session$sendCustomMessage("show_export_count", length(selected))
  })
  
  # =========================================================
  # Soft delete: move selected events to Deleted tab
  # =========================================================
  observeEvent(input$supprimer, {
    
    onglet <- input$onglets
    ids_to_delete <- character(0)
    
    if (identical(onglet, "Active")) {
      sel <- input$tableau_evenements_actifs_rows_selected
      if (length(sel) > 0) ids_to_delete <- donnees_actifs_filtrees()$Event_ID[sel]
    } else if (identical(onglet, "Closed")) {
      sel <- input$tableau_evenements_termines_rows_selected
      if (length(sel) > 0) ids_to_delete <- donnees_termines_filtrees()$Event_ID[sel]
    } else if (identical(onglet, "Deleted")) {
      sel <- input$tableau_evenements_supprimes_rows_selected
      if (length(sel) > 0) ids_to_delete <- donnees_supprimes_filtrees()$Event_ID[sel]
    }
    
    if (length(ids_to_delete) == 0 && !is.null(session$userData$indice_edition)) {
      df_full <- donnees()
      idx <- session$userData$indice_edition
      if (is.finite(idx) && idx >= 1 && idx <= nrow(df_full)) {
        ids_to_delete <- df_full$Event_ID[idx]
      }
    }
    
    ids_to_delete <- unique(ids_to_delete)
    ids_to_delete <- ids_to_delete[!is.na(ids_to_delete) & nzchar(ids_to_delete)]
    
    if (length(ids_to_delete) == 0) {
      showModal(modalDialog(
        "Please select one or more events in the table (or load one for editing) before deleting.",
        easyClose = TRUE
      ))
      return()
    }
    
    session$userData$delete_event_ids <- ids_to_delete
    
    showModal(modalDialog(
      title = "Confirmation",
      paste0(
        "Move the following ", length(ids_to_delete), " event(s) to the Deleted tab?\n\n",
        paste(ids_to_delete, collapse = ", ")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_soft_delete", "Confirm", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_soft_delete, {
    removeModal()
    
    ids <- session$userData$delete_event_ids
    if (is.null(ids) || length(ids) == 0) return()
    
    df <- donnees()
    if (nrow(df) == 0) return()
    
    df$Status[df$Event_ID %in% ids] <- "deleted"
    
    tryCatch({
      readr::write_csv(df, fichier_donnees)
      
      if (!is.null(session$userData$indice_edition)) {
        idx <- session$userData$indice_edition
        if (is.finite(idx) && idx >= 1 && idx <= nrow(df) && df$Event_ID[idx] %in% ids) {
          session$userData$indice_edition <- NULL
          session$userData$event_loaded_original <- NULL
        }
      }
      
      session$userData$delete_event_ids <- NULL
      
      showModal(modalDialog(
        title = "Success",
        paste("Moved", length(ids), "event(s) to Deleted."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while deleting: ", e$message))
    })
  })
  
  # =========================================================
  # Hard delete: only from Deleted tab, delete permanently
  # =========================================================
  observeEvent(input$delete_for_good, {
    
    if (!identical(input$onglets, "Deleted")) {
      showModal(modalDialog(
        "Please go to the Deleted tab to permanently delete events.",
        easyClose = TRUE
      ))
      return()
    }
    
    sel <- input$tableau_evenements_supprimes_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog("Please select one or more events in the Deleted tab.", easyClose = TRUE))
      return()
    }
    
    ids <- donnees_supprimes_filtrees()$Event_ID[sel]
    ids <- unique(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) == 0) return()
    
    session$userData$hard_delete_ids <- ids
    
    showModal(modalDialog(
      title = "Permanent deletion",
      paste0(
        "This will permanently delete ", length(ids), " event(s) and their uploaded images.\n\n",
        paste(ids, collapse = ", ")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_hard_delete", "Delete for good", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_hard_delete, {
    removeModal()
    
    ids <- session$userData$hard_delete_ids
    if (is.null(ids) || length(ids) == 0) return()
    
    df <- donnees()
    df_new <- df[!df$Event_ID %in% ids, , drop = FALSE]
    
    tryCatch({
      readr::write_csv(df_new, fichier_donnees)
      
      for (id_evt in ids) {
        dir_evt <- file.path("Graphs_uploaded", id_evt)
        if (dir.exists(dir_evt)) unlink(dir_evt, recursive = TRUE, force = TRUE)
      }
      
      session$userData$hard_delete_ids <- NULL
      
      showModal(modalDialog(
        title = "Deleted",
        paste("Permanently deleted", length(ids), "event(s)."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error during permanent deletion: ", e$message))
    })
  })
  
  # =========================================================
  # Tables
  # =========================================================
  
  output$tableau_evenements_actifs <- DT::renderDT({
    df <- donnees_actifs_filtrees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))
    
    cols <- colnames(df)
    idx  <- function(nm) which(cols == nm) - 1L
    
    DT::datatable(
      transform(df, Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S %Z")),
      selection = "multiple",
      editable  = TRUE,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        scrollX    = TRUE,
        autoWidth  = TRUE,
        pageLength = 15,
        columnDefs = list(
          list(targets = idx("Sources"),         width = "250px"),
          list(targets = idx("Situation"),       width = "500px"),
          list(targets = idx("Risk_Assessment"), width = "500px"),
          list(targets = idx("Measures"),        width = "500px")
        )
      )
    )
  })
  
  output$tableau_evenements_termines <- DT::renderDT({
    df <- donnees_termines_filtrees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))
    
    cols <- colnames(df)
    idx  <- function(nm) which(cols == nm) - 1L
    
    DT::datatable(
      df,
      selection = "multiple",
      editable  = TRUE,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        scrollX    = TRUE,
        autoWidth  = TRUE,
        pageLength = 15,
        columnDefs = list(
          list(targets = idx("Sources"),         width = "250px"),
          list(targets = idx("Situation"),       width = "500px"),
          list(targets = idx("Risk_Assessment"), width = "500px"),
          list(targets = idx("Measures"),        width = "500px")
        )
      )
    )
  })
  
  output$tableau_evenements_supprimes <- DT::renderDT({
    df <- donnees_supprimes_filtrees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))
    
    DT::datatable(
      transform(df, Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S %Z")),
      selection = "multiple",
      editable  = FALSE,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        scrollX    = TRUE,
        autoWidth  = TRUE,
        pageLength = 15
      )
    )
  })
  
  output$tableau_eios <- DT::renderDT({
    df <- eios_donnees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))
    
    DT::datatable(
      df,
      selection = "multiple",
      editable  = FALSE,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        scrollX    = TRUE,
        autoWidth  = TRUE,
        pageLength = 15
      )
    )
  })
  
  proxy_actifs     <- DT::dataTableProxy("tableau_evenements_actifs")
  proxy_termines   <- DT::dataTableProxy("tableau_evenements_termines")
  proxy_supprimes  <- DT::dataTableProxy("tableau_evenements_supprimes")
  proxy_eios       <- DT::dataTableProxy("tableau_eios")
  
  .select_all <- function(proxy, nrows) {
    if (is.finite(nrows) && nrows > 0) DT::selectRows(proxy, 1:nrows) else DT::selectRows(proxy, NULL)
  }
  .clear_all <- function(proxy) DT::selectRows(proxy, NULL)
  
  observeEvent(input$select_all, {
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      .select_all(proxy_actifs, nrow(donnees_actifs_filtrees()))
    } else if (identical(onglet, "Closed")) {
      .select_all(proxy_termines, nrow(donnees_termines_filtrees()))
    } else if (identical(onglet, "Deleted")) {
      .select_all(proxy_supprimes, nrow(donnees_supprimes_filtrees()))
    } else if (identical(onglet, "EIOS")) {
      .select_all(proxy_eios, nrow(eios_donnees()))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_all, {
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      .clear_all(proxy_actifs)
    } else if (identical(onglet, "Closed")) {
      .clear_all(proxy_termines)
    } else if (identical(onglet, "Deleted")) {
      .clear_all(proxy_supprimes)
    } else if (identical(onglet, "EIOS")) {
      .clear_all(proxy_eios)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_filtre_date, {
    df <- donnees()
    if (!is.null(df) && nrow(df) > 0 && "Date" %in% names(df)) {
      min_date <- suppressWarnings(min(df$Date, na.rm = TRUE))
      if (is.infinite(suppressWarnings(as.numeric(min_date))) || is.na(min_date)) {
        min_date <- Sys.Date() - 30
      }
    } else {
      min_date <- Sys.Date() - 30
    }
    updateDateRangeInput(session, "filtre_date", start = as.Date(min_date), end = Sys.Date())
  })
  
  # =========================================================
  # Excel export
  # =========================================================
  dir.create("excel_outputs", showWarnings = FALSE)
  
  observeEvent(input$exporter_excel, {
    dir.create("excel_outputs", showWarnings = FALSE)
    
    data_view <- donnees_actifs_filtrees()
    sel <- input$tableau_evenements_actifs_rows_selected
    export_data <- if (!is.null(sel) && length(sel) > 0) data_view[sel, ] else data_view
    
    if (nrow(export_data) == 0) {
      showModal(modalDialog("No selected or active events."))
      return()
    }
    
    export_data <- export_data %>%
      dplyr::mutate(Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S"))
    
    date_today <- format(Sys.Date(), "%Y-%m-%d")
    existing_files <- list.files("excel_outputs", pattern = paste0("^evenements_", date_today, "_\\d+\\.xlsx$"))
    existing_nums <- as.integer(gsub(paste0("evenements_", date_today, "_|\\.xlsx"), "", existing_files))
    next_num <- ifelse(length(existing_nums) == 0, 1, max(existing_nums, na.rm = TRUE) + 1)
    filename <- sprintf("excel_outputs/evenements_%s_%d.xlsx", date_today, next_num)
    
    tryCatch({
      writexl::write_xlsx(export_data, filename)
      showModal(modalDialog(
        title = "Success",
        paste("The Excel file was created successfully:", filename),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error during export: ", e$message))
    })
  })
  
  # =========================================================
  # EIOS import + copy
  # =========================================================
  observeEvent(input$upload_eios, {
    if (is.null(input$upload_eios$datapath)) return()
    path <- input$upload_eios$datapath
    
    df_raw <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) { showModal(modalDialog("Error reading EIOS CSV: ", e$message)); return(NULL) }
    )
    if (is.null(df_raw)) return()
    
    req_cols <- c("externalLink","title","summary","categories","mentionedCountries","id")
    if (!all(req_cols %in% names(df_raw))) {
      manq <- paste(setdiff(req_cols, names(df_raw)), collapse = ", ")
      showModal(modalDialog(paste("Missing columns in the EIOS CSV:", manq)))
      return()
    }
    
    df_eios <- df_raw %>%
      dplyr::transmute(
        Sources   = as.character(.data$externalLink),
        Title     = as.character(.data$originalTitle),
        Situation = as.character(.data$translatedDescription),
        Other     = as.character(.data$categories),
        Regions   = as.character(.data$mentionedCountries),
        EIOS_id   = as.character(.data$id)
      )
    
    eios_donnees(df_eios)
  })
  
  observeEvent(input$copier_vers_actifs, {
    df_eios <- eios_donnees()
    sel <- input$tableau_eios_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    
    selection <- df_eios[sel, , drop = FALSE]
    if (nrow(selection) == 0) return()
    
    df_all <- donnees()
    
    to_add <- lapply(seq_len(nrow(selection)), function(i) {
      nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                             sum(grepl(format(Sys.Date(), "%Y%m%d"), df_all$Event_ID)) + i)
      
      data.frame(
        Event_ID                = nouvelle_id,
        Update                  = "new event from EIOS",
        Version                 = 1,
        Date                    = Sys.Date(),
        Last_Modified           = lubridate::now(tzone = "Africa/Johannesburg"),
        Title                   = selection$Title[i],
        Event_Type              = NA_character_,
        Alert_Category          = NA_character_,
        Regions                 = selection$Regions[i],
        Sources                 = selection$Sources[i],
        Situation               = selection$Situation[i],
        Risk_Assessment         = NA_character_,
        Measures                = NA_character_,
        Public_Health_Relevance = NA_character_,
        Traveler_Relevance      = NA_character_,
        Comments                = NA_character_,
        Summary                 = NA_character_,
        Other                   = selection$Other[i],
        Status                  = "active",
        EIOS_id                 = selection$EIOS_id[i],
        Images                  = if ("Images" %in% names(df_all)) NA_character_ else NULL,
        stringsAsFactors        = FALSE
      )
    })
    
    df_new <- dplyr::bind_rows(df_all, dplyr::bind_rows(to_add))
    tryCatch({
      write_csv(df_new, fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving CSV: ", e$message))
    })
  })
  
  # =========================================================
  # Word helpers for robust date replacement (run-splitting safe)
  # =========================================================
  .compute_report_date_value <- function() {
    month_en <- c("January","February","March","April","May","June",
                  "July","August","September","October","November","December")
    today <- Sys.Date()
    month_name <- month_en[as.integer(format(today, "%m"))]
    paste0(format(today, "%d"), " ", month_name, " ", format(today, "%Y"))
  }
  
  .xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
  
  .replace_token_in_docx_file <- function(docx_path, token = "[[BM_DATE]]", value) {
    tmp_dir <- tempfile("docx_unzip_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    
    utils::unzip(docx_path, exdir = tmp_dir)
    
    xml_files <- list.files(
      file.path(tmp_dir, "word"),
      pattern = "^(document|header[0-9]*|footer[0-9]*)\\.xml$",
      full.names = TRUE
    )
    
    token_regex <- paste0(
      "\\[\\[(?:<[^>]+>)*B(?:<[^>]+>)*M(?:<[^>]+>)*_(?:<[^>]+>)*D(?:<[^>]+>)*A(?:<[^>]+>)*T(?:<[^>]+>)*E(?:<[^>]+>)*\\]\\]"
    )
    
    repl <- .xml_escape(value)
    
    for (f in xml_files) {
      txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
      txt2 <- gsub(token_regex, repl, txt, perl = TRUE)
      txt2 <- gsub(token, repl, txt2, fixed = TRUE)
      
      if (!identical(txt, txt2)) {
        writeLines(txt2, f, useBytes = TRUE)
      }
    }
    
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(tmp_dir)
    
    new_zip <- tempfile(fileext = ".docx")
    files_to_zip <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    utils::zip(zipfile = new_zip, files = files_to_zip, flags = "-r9Xq")
    
    file.copy(new_zip, docx_path, overwrite = TRUE)
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  }
  
  # =========================================================
  # Word report generation (ONLY CHANGED PARTS: summary anchor + date token)
  # =========================================================
  observeEvent(input$exporter_word, {
    dir.create("report_word_outputs", showWarnings = FALSE)
    
    data_view <- donnees_actifs_filtrees()
    sel <- input$tableau_evenements_actifs_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "No event selected",
        "Please select at least one event in the Active table before creating a Word report.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    export_data <- data_view[sel, , drop = FALSE]
    
    if (nrow(export_data) == 0) {
      showModal(modalDialog("No selected or active events."))
      return()
    }
    
    rapport_filename <- generer_nom_fichier()
    
    doc <- officer::read_docx("template_report/template_word.docx")
    
    # (1) Insert summary on page 1 at bookmark BM_CONTENTS
    doc <- tryCatch(
      officer::cursor_bookmark(doc, "BM_CONTENTS"),
      error = function(e) {
        showModal(modalDialog(
          title = "Bookmark BM_CONTENTS not found",
          "Please ensure your Word template contains a bookmark named BM_CONTENTS right after the word 'Contents' on page 1.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
    )
    if (is.null(doc)) return()
    
    doc <- ajouter_sommaire(doc, export_data, NULL)
    
    # Events start on page 2
    doc <- officer::cursor_end(doc)
    doc <- officer::body_add_break(doc)
    doc <- ajouter_evenements(doc, export_data, NULL)
    
    date_value <- .compute_report_date_value()
    doc <- officer::body_replace_text_at_bkm(doc, bookmark = "BM_DATE", value = date_value)
    
    tryCatch({
      print(doc, target = rapport_filename)
      
      # (2) Replace [[BM_DATE]] robustly 
      date_value <- .compute_report_date_value()
      .replace_token_in_docx_file(rapport_filename, token = "[[BM_DATE]]", value = date_value)
      
      
      showModal(modalDialog(
        title = "Success",
        paste("The Word report was generated successfully:", rapport_filename),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while generating the Word report: ", e$message))
    })
  })
  
  # --- Upload charts/images and attach them to an event ---
  observeEvent(input$televerser_graphs, {
    fichiers <- input$televerser_graphs
    if (is.null(fichiers) || nrow(fichiers) == 0) {
      showModal(modalDialog("Please choose one or more image files."))
      return()
    }
    
    id_evt <- id_evenement_courant()
    if (is.na(id_evt) || is.null(id_evt)) {
      showModal(modalDialog("Please select an event (in the table) or load it for editing first."))
      return()
    }
    
    dir_evt <- file.path("Graphs_uploaded", id_evt)
    dir.create(dir_evt, showWarnings = FALSE, recursive = TRUE)
    
    idx <- prochain_index_image(id_evt)
    chemins_relatifs <- character(0)
    
    for (i in seq_len(nrow(fichiers))) {
      src  <- fichiers$datapath[i]
      ext  <- tools::file_ext(fichiers$name[i])
      if (identical(ext, "")) ext <- "png"
      dest_name <- sprintf("image_%d.%s", idx, tolower(ext))
      dest_path <- file.path(dir_evt, dest_name)
      ok <- file.copy(from = src, to = dest_path, overwrite = FALSE)
      if (isTRUE(ok)) {
        chemins_relatifs <- c(chemins_relatifs, dest_path)
        idx <- idx + 1L
      }
    }
    
    if (length(chemins_relatifs) == 0) {
      showModal(modalDialog("No file was copied."))
      return()
    }
    
    df <- donnees()
    pos <- which(df$Event_ID == id_evt)
    if (length(pos) == 1) {
      exist <- df$Images[pos]
      if (is.null(exist) || is.na(exist)) exist <- ""
      ajout <- paste(chemins_relatifs, collapse = ";")
      df$Images[pos] <- if (nzchar(exist)) paste(exist, ajout, sep = ";") else ajout
      
      tryCatch(readr::write_csv(df, fichier_donnees),
               error = function(e) showModal(modalDialog("Error while saving image paths: ", e$message)))
    }
    
    showModal(modalDialog(
      title = "Success",
      paste0(length(chemins_relatifs), " file(s) linked to event ", id_evt, "."),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  # File naming
  generer_nom_fichier <- function() {
    date_today <- format(Sys.Date(), "%Y-%m-%d")
    existing_files <- list.files("report_word_outputs", pattern = paste0("^report_", date_today, "_\\d+\\.docx$"))
    existing_nums <- as.integer(gsub(paste0("report_", date_today, "_|\\.docx"), "", existing_files))
    next_num <- ifelse(length(existing_nums) == 0, 1, max(existing_nums, na.rm = TRUE) + 1)
    sprintf("report_word_outputs/report_%s_%d.docx", date_today, next_num)
  }
  
  # =========================================================
  # Summary list (unchanged formatting, but WITHOUT adding an extra "EVENT LIST" title)
  # =========================================================
  ajouter_sommaire <- function(doc, data, styles) {
    cat_map <- list(
      "International and national alerts" = "International and National Public Health Alerts and Emergencies",
      "National alerts"                   = "National Public Health Alerts and Emergencies",
      "International alerts"              = "International Public Health Alerts and Emergencies"
    )
    categories <- names(cat_map)
    
    compteur <- 1
    
    for (cat in categories) {
      cat_data <- data %>% dplyr::filter(Alert_Category == cat)
      if (nrow(cat_data) == 0) next
      
      doc <- doc %>% officer::body_add_par(cat_map[[cat]], style = "t_heading_2")
      
      for (i in seq_len(nrow(cat_data))) {
        titre <- as.character(cat_data$Title[i])
        if (!is.na(titre) && nzchar(trimws(titre))) {
          doc <- doc %>% officer::body_add_par(
            paste0(compteur, ". ", titre),
            style = "t_heading_4"
          )
          compteur <- compteur + 1
        }
      }
    }
    
    other_data <- data %>%
      dplyr::filter(is.na(Alert_Category) | Alert_Category == "" | !(Alert_Category %in% categories))
    
    if (nrow(other_data) > 0) {
      doc <- doc %>% officer::body_add_par("Uncategorized", style = "t_heading_2")
      
      for (i in seq_len(nrow(other_data))) {
        titre <- as.character(other_data$Title[i])
        if (!is.na(titre) && nzchar(trimws(titre))) {
          doc <- doc %>% officer::body_add_par(
            paste0(compteur, ". ", titre),
            style = "t_heading_4"
          )
          compteur <- compteur + 1
        }
      }
    }
    
    doc
  }
  
  # =========================================================
  # Event printing (unchanged)
  # =========================================================
  ajouter_evenements <- function(doc, data, styles) {
    cat_map <- list(
      "International and national alerts" = "International and National Public Health Alerts and Emergencies",
      "National alerts"                   = "National Public Health Alerts and Emergencies",
      "International alerts"              = "International Public Health Alerts and Emergencies"
    )
    categories <- names(cat_map)
    
    compteur <- 1
    
    p_std   <- officer::fp_par(line_spacing = 0.7)
    run_std <- officer::fp_text()
    run_b   <- officer::fp_text(bold = TRUE)
    
    colonnes_exclues <- c("Event_ID", "Version", "Last_Modified", "EIOS_id", "Images")
    
    .print_event <- function(doc, row, compteur) {
      titre <- if ("Title" %in% names(row)) as.character(row$Title) else ""
      if (is.na(titre) || !nzchar(trimws(titre))) titre <- "(No title)"
      
      doc <- doc %>% officer::body_add_par(paste0(compteur, ". ", titre), style = "t_heading_1")
      
      for (champ in names(row)) {
        if (champ %in% colonnes_exclues) next
        
        val <- row[[champ]]
        val_txt <- as.character(val)
        
        if (!is.null(val) && !is.na(val) && nzchar(trimws(val_txt))) {
          bloc <- officer::fpar(
            officer::ftext(paste0(champ, " : "), run_b),
            officer::ftext(val_txt, run_std),
            fp_p = p_std
          )
          doc <- officer::body_add_fpar(doc, value = bloc, style = "t_standard")
        }
      }
      
      if ("Images" %in% names(row)) {
        imgs_raw <- as.character(row$Images)
        if (!is.na(imgs_raw) && nzchar(trimws(imgs_raw))) {
          chemins <- strsplit(imgs_raw, ";")[[1]]
          chemins <- trimws(chemins)
          chemins <- chemins[chemins != ""]
          for (che in chemins) {
            if (file.exists(che)) {
              s <- taille_img(che, 3)
              doc <- officer::body_add_img(doc, src = che, width = s$w, height = s$h)
              doc <- officer::body_add_par(doc, "", style = "t_standard")
            }
          }
        }
      }
      
      list(doc = doc, compteur = compteur + 1)
    }
    
    for (cat in categories) {
      cat_data <- data %>% dplyr::filter(Alert_Category == cat)
      if (nrow(cat_data) == 0) next
      
      doc <- doc %>% officer::body_add_par(cat_map[[cat]], style = "t_heading_2")
      
      for (i in seq_len(nrow(cat_data))) {
        res <- .print_event(doc, cat_data[i, , drop = FALSE], compteur)
        doc <- res$doc
        compteur <- res$compteur
      }
    }
    
    other_data <- data %>%
      dplyr::filter(is.na(Alert_Category) | Alert_Category == "" | !(Alert_Category %in% categories))
    
    if (nrow(other_data) > 0) {
      doc <- doc %>% officer::body_add_par("Uncategorized", style = "t_heading_2")
      
      for (i in seq_len(nrow(other_data))) {
        res <- .print_event(doc, other_data[i, , drop = FALSE], compteur)
        doc <- res$doc
        compteur <- res$compteur
      }
    }
    doc
  }
}