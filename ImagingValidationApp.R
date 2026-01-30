rm(list = setdiff(ls(), "dataset_id"))
source("config/config.R", local = TRUE)
stopifnot(
  file.exists(TAXONOMY_FILE),
  dir.exists(DATA_SORTED_DIR)
)


packages <- c("shiny", "magick", "base64enc")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}
lapply(packages, library, character.only = TRUE)



# Folder paths
taxonomy <- read.csv(TAXONOMY_FILE, stringsAsFactors = FALSE)


# Define colors by type
type_colors <- c(
  "detritus" = "#add8e6",            
  "biologicall" = "#90ee90",            
  "phyto" = "#ff9999",          
  "echino" = "grey",
  "copepod" = "yellow",
  "jelly" = "grey50"
  
)

######## CHOOSE ONE OF THE DATASETS
base_name <- "20250709-1140"
dataset_id <- "20250709-1140"

base_dir <- file.path(DATA_SORTED_DIR, base_name)

label_file <- file.path(base_dir, "labels.csv")
labels <- taxonomy$taxon

# Modified version to use caching
read_tif_as_base64_cached <- local({
  cache <- new.env(parent = emptyenv())
  
  function(file_path) {
    if (!exists(file_path, envir = cache)) {
      img <- tryCatch(image_read(file_path), error = function(e) NULL)
      if (!is.null(img)) {
        tmpfile <- tempfile(fileext = ".png")
        image_write(img, tmpfile, format = "png")
        encoded <- dataURI(file = tmpfile, mime = "image/png")
        assign(file_path, encoded, envir = cache)
      } else {
        assign(file_path, NULL, envir = cache)
      }
    }
    get(file_path, envir = cache)
  }
})


# Load existing labels
load_labeled_ids <- function() {
  if (file.exists(label_file)) {
    read.csv(label_file, stringsAsFactors = FALSE)$image
  } else {
    character()
  }
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .progress-container {
        width: 100%;
        background-color: #e0e0e0;
        border-radius: 5px;
        height: 20px;
        margin-top: 5px;
        margin-bottom: 10px;
      }
      .progress-bar {
        height: 100%;
        width: 0%;
        background-color: green;
        border-radius: 5px;
        transition: width 0.3s;
      }
      .left-panel {
        background-color: #eaeaea;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      .right-panel {
        background-color: #f5f5f5;
        padding: 15px;
        border-radius: 8px;
        height: 80vh;
        overflow-y: auto;
      }
       .copepod {
      position: absolute;
      top: -5px;
      height: 40px;
      transition: left 0.3s;
      z-index: 2;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProgressBar', function(percent) {
        document.getElementById('progress_bar').style.width = percent + '%';
      });
      Shiny.addCustomMessageHandler('updateSubfolderProgressBar', function(percent) {
        document.getElementById('progress_bar_subfolder').style.width = percent + '%';
      });

    
    
      Shiny.addCustomMessageHandler('updateProgressBar', function(percent) {
        document.getElementById('progress_bar').style.width = percent + '%';
      
        let copepod = document.getElementById('copepod_icon');
        let bar = copepod.parentElement;
        let barWidth = bar.clientWidth;
        let copepodWidth = copepod.clientWidth;
      
        // Shift left by 5px earlier (playful lead effect)
        let leftPos = (percent / 100) * (barWidth - copepodWidth) - 10;
        copepod.style.left = leftPos + 'px';
      });



      
       Shiny.addCustomMessageHandler('updateSubfolderProgressBar', function(percent) {
        document.getElementById('progress_bar_subfolder').style.width = percent + '%';

        let copepod = document.getElementById('copepod_icon_subfolder');
        let bar = copepod.parentElement;
        let barWidth = bar.clientWidth;
        let copepodWidth = copepod.clientWidth;

        let leftPos = (percent / 100) * (barWidth - copepodWidth) - 10;
        copepod.style.left = leftPos + 'px';
      });
      
      
    "))
  ),
  
  titlePanel("Image Labeling Tool"),
  
  fluidRow(
    column(3,
           
           # --- Panel 1: Controls ---
           div(class = "left-panel",
               h4("Labeling Controls"),
               selectInput("subfolder", "Choose subfolder:",
                           choices = list.dirs(base_dir, full.names = FALSE, recursive = FALSE)),
               
               textInput("user_name", "Enter your name:", value = ""),
               
               
               selectInput("images_per_page", "Images per page:",
                           choices = c(50, 100, 200, 500, 1000, 2000),
                           selected = 200),
               
               # turned down for the Game
               #numericInput("page", "Page:", value = 1, min = 1, step = 1),
               # actionButton("prev_btn", "Previous"),
               # actionButton("next_btn", "Next"),
               
               checkboxInput("select_all", "Select all visible", value = FALSE),
               
               # div(
               #   # Row 1: detritus and bubbles (light blue)
               #   div(style = "margin-bottom: 10px;",
               #       actionButton("label_btn_detritus", "detritus", style = "background-color: #add8e6; margin-right: 8px;"),
               #       actionButton("label_btn_bubbles", "bubbles", style = "background-color: #add8e6;")
               #   ),
               #   
               #   # Row 2: the rest with light green
               #   div(
               #     actionButton("label_btn_phaeocystis", "phaeocystis", style = "background-color: #90ee90; margin-right: 8px;"),
               #     actionButton("label_btn_calanoid", "calanoid", style = "background-color: #90ee90; margin-right: 8px;"),
               #     actionButton("label_btn_biologicalparticle", "biologicalparticle", style = "background-color: #90ee90; margin-right: 8px;"),
               #     actionButton("label_btn_noctiluca", "noctiluca", style = "background-color: #90ee90;")
               #   ),
               #   
               #   # Row 3: red buttons for phyto_Long and phyto_ShortSturdy
               #   div(style = "margin-bottom: 10px;",
               #       actionButton("label_btn_phyto", "phyto_OtherShape", style = "background-color: #ff9999; color: white; margin-right: 8px;"),
               #       actionButton("label_btn_phyto_Long", "phyto_Long", style = "background-color: #ff9999; color: white; margin-right: 8px;"),
               #       actionButton("label_btn_phyto_ShortSturdy", "phyto_ShortSturdy", style = "background-color: #ff9999; color: white;")
               #   )
               # )
               
               
               
               # Create button UI
               button_ui <- div(
                 style = "display: flex; flex-wrap: wrap; gap: 10px;",
                 lapply(seq_len(nrow(taxonomy)), function(i) {
                   taxon <- taxonomy$taxon[i]
                   type <- taxonomy$type[i]
                   color <- type_colors[type]
                   if (is.na(color)) color <- "#cccccc"  # fallback color if type not mapped
                   actionButton(
                     inputId = paste0("label_btn_", gsub("[^a-zA-Z0-9]", "_", taxon)),
                     label = taxon,
                     style = paste0(
                       "background-color: ", color, "; ",
                       "margin: 4px; ",
                       "font-size: 13px; ",        # smaller text
                       "padding: 1px 5px; ",        # smaller padding
                       "height: 25px; "             # smaller height
                     )
                   )
                 })
               )
           ),
           
           
           # --- Panel 2: Overall Progress ---
           div(class = "left-panel",
               h4("Overall Progress"),
               
               verbatimTextOutput("overall_progress_text"),
               div(class = "progress-container", style = "position: relative;",
                   div(id = "progress_bar", class = "progress-bar"),
                   tags$img(id = "copepod_icon",
                            src = "https://symbols.getvecta.com/stencil_288/0_acartia-spp-copepod.c88c8e27ad.svg",
                            class = "copepod",
                            style = "
                                position: absolute;
                                top: -15px;
                                left: 0px;
                                height: 60px;
                                transition: left 0.25s;
                                z-index: 2;
                              ")
               )
           ),
           
           #--- Panel 3: Subfolder Progress ---
           div(class = "left-panel",
               h4("Current Folder Progress"),
               verbatimTextOutput("subfolder_progress_text"),
               #div(class = "progress-container", div(id = "progress_bar_subfolder", class = "progress-bar"))
               
               
               div(class = "progress-container", style = "position: relative;",
                   div(id = "progress_bar_subfolder", class = "progress-bar"),
                   tags$img(id = "copepod_icon_subfolder",
                            src = "https://symbols.getvecta.com/stencil_288/22_copepod-1.5ebe1abe07.svg",
                            class = "copepod",
                            style = "
                                position: absolute;
                                top: -15px;
                                left: 0px;
                                height: 50px;
                                transition: left 0.3s;
                                z-index: 2;
                              ")
               )
           ),
           
           # --- Panel 4: Example Images ---
           # div(class = "left-panel",
           #     h4("Example Images"),
           #     uiOutput("all_examples_ui")
           # ),
           
           

               
               
           # --- Panel 5: Validate ALL ---
           div(class = "left-panel",
               h4("Validate All"),
               uiOutput("validateall")
           ),
           
           # --- Panel 5: Validate ALL ---
           div(class = "left-panel",
               h4("Click if you are done"),
               actionButton("save_validated_btn", "Save Validated CSV when 100%", icon = icon("save"), style = "margin-top: 10px; background-color: #ffd700;")
           )
           
           
           
           
           
           
    ),
    
    # --- Right Panel: Image Gallery ---
    column(9,
           div(class = "right-panel",
               uiOutput("image_gallery")
           )
           
           
           
           
           
    )
  )
)


server <- function(input, output, session) {
  labeling_trigger <- reactiveVal(0)
  
  current_page <- reactiveVal(1)
  labeled_ids <- reactiveVal(load_labeled_ids())
  
  observeEvent(input$prev_btn, {
    if (current_page() > 1) current_page(current_page() - 1)
  })
  
  observeEvent(input$next_btn, {
    current_page(current_page() + 1)
  })
  
  observeEvent(input$page, {
    current_page(input$page)
  })
  
  observeEvent(input$save_validated_btn, {
    if (file.exists(label_file)) {
      output_path <- file.path(PROCESSED_OUTPUT_DIR, paste0(base_name, "_validated.csv"))
      file.copy(label_file, output_path, overwrite = TRUE)
      showNotification(paste("Saved to", output_path), type = "message")
    } else {
      showNotification("Label file does not exist.", type = "error")
    }
  })
  
  
  image_paths <- reactive({
    req(input$subfolder)
    folder_path <- file.path(base_dir, input$subfolder)
    all_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
    setdiff(all_files, labeled_ids())
  })
  
  visible_images <- reactive({
    files <- image_paths()
    per_page <- as.numeric(input$images_per_page)
    from <- (current_page() - 1) * per_page + 1
    to <- min(current_page() * per_page, length(files))
    files[from:to]
  })
  
  output$image_gallery <- renderUI({
    files <- visible_images()
    if (length(files) == 0) return(h4("No unlabeled images left on this page."))
    
    image_cards <- lapply(seq_along(files), function(i) {
      f <- files[i]
      id <- basename(f)
      #src <- read_tif_as_base64(f)
      src <- read_tif_as_base64_cached(f)
      
      if (!is.null(src)) {
        div(
          style = "display: inline-block; width: 9%; margin: 0.5%; text-align: center; vertical-align: top;",
          tags$label(
            style = "cursor: pointer; display: block;",
            #size of pictures
            tags$img(src = src, style = "height: 120px; display: block; margin: 0 auto 5px auto; border: 1px solid #ccc;"),
            # Checkbox input, bigger with margin and block display for easier clicking
            tags$input(
              id = paste0("img_", i),
              type = "checkbox",
              checked = if (isTRUE(input$select_all)) "checked" else NULL,
              style = "transform: scale(1.8); margin: 0 auto; display: block; cursor: pointer;"
            )
          ),
          tags$div(style = "font-size: 10px; word-break: break-word; margin-top: 4px;", id)
        )
        
      }
    })
    
    div(style = "width: 100%;", do.call(tagList, image_cards))
  })
  
  observe({
    lapply(labels, function(lbl) {
      observeEvent(input[[paste0("label_btn_", gsub("[^a-zA-Z0-9]", "_", lbl))]], {
        selected <- sapply(seq_along(visible_images()), function(i) {
          input[[paste0("img_", i)]]
        })
        
        files <- visible_images()
        selected_files <- files[which(selected)]
        
        if (length(selected_files) > 0) {
          new_labels <- data.frame(
            image = selected_files,
            label = lbl,
            user = input$user_name,
            #timestamp = Sys.time(),
            stringsAsFactors = FALSE
          )
          
          # Append to existing file or create it
          if (file.exists(label_file)) {
            existing <- read.csv(label_file, stringsAsFactors = FALSE)
            
            # Ensure same columns in same order
            common_cols <- intersect(names(existing), names(new_labels))
            new_labels <- new_labels[, common_cols, drop = FALSE]
            existing <- existing[, common_cols, drop = FALSE]
            
            updated <- rbind(existing, new_labels)
          } else {
            updated <- new_labels
          }
          
          
          write.csv(updated, label_file, row.names = FALSE)
          labeled_ids(updated$image)  # update the reactive labeled list
          labeling_trigger(labeling_trigger() + 1)  # trigger UI update
          
          showNotification(paste(length(selected_files), "images labeled as", lbl), type = "message")
        } else {
          showNotification("No images selected", type = "warning")
        }
        
        
        # Deselect all checkboxes after labeling
        lapply(seq_along(visible_images()), function(i) {
          updateCheckboxInput(session, paste0("img_", i), value = FALSE)
        })
        
        # Also uncheck 'select all visible'
        updateCheckboxInput(session, "select_all", value = FALSE)
        # Update progress immediately
        stats <- labeling_stats()
        percent <- if (stats$total == 0) 0 else round(100 * stats$labeled / stats$total)
        session$sendCustomMessage("updateProgressBar", percent)
        
        # Force progress bar update
        labeling_trigger(labeling_trigger() + 1)
      })
    })
  })
  
  # Labeling stats for progress
  labeling_stats <- reactive({
    labeling_trigger()  # depend on trigger to re-run
    all_subfolders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
    all_images <- unlist(lapply(all_subfolders, function(folder_path) {
      list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
    }))
    total <- length(all_images)
    
    if (file.exists(label_file)) {
      labeled <- read.csv(label_file, stringsAsFactors = FALSE)
      labeled_count <- sum(labeled$image %in% all_images)
    } else {
      labeled_count <- 0
    }
    
    list(total = total, labeled = labeled_count)
  })
  
  subfolder_stats <- reactive({
    labeling_trigger()  # ✅ force re-evaluation when labeling happens
    req(input$subfolder)
    
    folder_path <- file.path(base_dir, input$subfolder)
    all_images <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
    
    if (file.exists(label_file)) {
      labeled <- read.csv(label_file, stringsAsFactors = FALSE)
      labeled_count <- sum(labeled$image %in% all_images)
    } else {
      labeled_count <- 0
    }
    
    list(total = length(all_images), labeled = labeled_count)
  })
  
  
  
  # Overall progress text
  output$overall_progress_text <- renderText({
    stats <- labeling_stats()
    percent <- if (stats$total == 0) 0 else round(100 * stats$labeled / stats$total)
    session$sendCustomMessage("updateProgressBar", percent)
    paste0("Labeled: ", stats$labeled, " / ", stats$total, " (", percent, "%) across all folders")
  })
  
  # Subfolder progress text
  output$subfolder_progress_text <- renderText({
    sub_stats <- subfolder_stats()
    sub_percent <- if (sub_stats$total == 0) 0 else round(100 * sub_stats$labeled / sub_stats$total)
    session$sendCustomMessage("updateSubfolderProgressBar", sub_percent)
    paste0("Current folder: ", sub_stats$labeled, " / ", sub_stats$total, " (", sub_percent, "%)")
  })
  
  observe({
    stats <- labeling_stats()
    percent <- if (stats$total == 0) 0 else round(100 * stats$labeled / stats$total)
    session$sendCustomMessage("updateProgressBar", percent)
  })
  
  
  output$all_examples_ui <- renderUI({
    example_dir <- EXAMPLE_LABEL_DIR
    image_files <- list.files(example_dir, pattern = "\\.tif$", full.names = TRUE)
    
    if (length(image_files) == 0) return(NULL)
    
    image_tags <- lapply(image_files, function(path) {
      img_label <- tools::file_path_sans_ext(basename(path))
      
      # Convert TIF to PNG using magick
      img <- tryCatch(magick::image_read(path), error = function(e) NULL)
      if (is.null(img)) return(NULL)
      
      tmpfile <- tempfile(fileext = ".png")
      magick::image_write(img, tmpfile, format = "png")
      encoded <- base64enc::dataURI(file = tmpfile, mime = "image/png")
      
      tags$div(
        style = "width: 48%; display: inline-block; vertical-align: top; margin: 1%;",
        tags$p(style = "margin: 0 0 5px 0; font-size: 12px;", img_label),
        tags$img(src = encoded, style = "height: 80px; border: 1px solid #ccc; display: block; margin-bottom: 10px;")
      )
    })
    
    # Wrap in a container to clear floats or inline-blocks if needed
    tags$div(style = "width: 100%;", do.call(tagList, image_tags))
  })
  
  
  ####Validate ALL button here

  # UI rendering for the Validate All button
  output$validateall <- renderUI({
    req(input$subfolder)
    actionButton(
      "validate_all_btn",
      "Validate All Images in This Folder",
      icon = icon("check"),
      style = "background-color: #90ee90;"
    )
  })

  observeEvent(input$validate_all_btn, {
    req(input$subfolder)   # ✅ only check that a subfolder exists
    folder_path <- file.path(base_dir, input$subfolder)
    tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
    
    if (length(tif_files) == 0) {
      showNotification("No .tif images found in this folder.", type = "warning")
      return()
    }
    
    # Use the subfolder name as label (fallback)
    label_name <- input$subfolder
    
    # Create label entries
    new_labels <- data.frame(
      image = tif_files,
      label = label_name,
      user = input$user_name,
      stringsAsFactors = FALSE
    )
    
    # Load existing labels
    if (file.exists(label_file)) {
      existing <- read.csv(label_file, stringsAsFactors = FALSE)
      new_labels <- new_labels[!new_labels$image %in% existing$image, ]
      updated <- rbind(existing, new_labels)
    } else {
      updated <- new_labels
    }
    
    write.csv(updated, label_file, row.names = FALSE)
    labeled_ids(updated$image)  
    labeling_trigger(labeling_trigger() + 1)
    
    showNotification(
      paste("Validated", nrow(new_labels), "images in", input$subfolder),
      type = "message"
    )
  })
  
  
  
  
  
  observe({
    # Wait for label updates or subfolder changes
    labeling_trigger()
    req(input$subfolder)
    
    sub_stats <- subfolder_stats()
    
    # If all images in current subfolder are labeled
    if (sub_stats$total > 0 && sub_stats$labeled >= sub_stats$total) {
      all_subfolders <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
      current_index <- match(input$subfolder, all_subfolders)
      
      # If not last subfolder, move to next
      if (!is.na(current_index) && current_index < length(all_subfolders)) {
        new_subfolder <- all_subfolders[current_index + 1]
        updateSelectInput(session, "subfolder", selected = new_subfolder)
        
        # Reset page to 1 when switching folder
        updateNumericInput(session, "page", value = 1)
      }
    }
  })
  
  
  
}

shinyApp(ui, server)
