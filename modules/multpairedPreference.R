multpairedPreferenceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Multiple Paired Preference"),
        wellPanel(
          h4(tagList(icon("upload"), "Unggah Data")),
          radioButtons(
            inputId = ns("sumber_data"),
            label = "Pilih data:",
            choices = c(
              "Gunakan contoh data" = "contoh",
              "Unggah data" = "unggah"
            )
          ),
          conditionalPanel(
            condition = "input.sumber_data == 'contoh'",
            ns = ns,
            helpText(tags$small("Contoh menggunakan data Multiple Paired Preference untuk sampel maskara. Pengaturan parameter adalah sebagai berikut: Panelis = 'Judge', Sampel preferensi= 'Preferred', Sampel bukan preferensi = 'Other'."))
          ),
          conditionalPanel(
            condition = "input.sumber_data == 'unggah'",
            ns = ns,
            tipify(
              fileInput(
                inputId = ns("data"),
                label = "",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                ),
                buttonLabel = "Pilih",
                placeholder = "Tidak ada dokumen"
              ),
              title = "format file *.csv",
              placement = "top"
            )
          ),
          actionButton(
            inputId = ns("unggah"),
            label = "Gunakan data"
          ),
          conditionalPanel(
            condition = "input.unggah",
            ns = ns,
            br(),
            h4(tagList(icon("sliders"), "Pengaturan Parameter")),
            pickerInput(
              inputId = ns("panelis"),
              label = "Panelis",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("sampel_pref"),
              label = "Sampel preferensi/disukai",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("sampel_nonpref"),
              label = "Sampel bukan preferensi/tidak disukai",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            actionButton(
              inputId = ns("terapkan"),
              label = "Terapkan"
            )
          )
        )
      ),
      column(
        8,
        panel(
          heading = h4(tagList(icon("database"), "Data")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Tabel",
              icon = icon("table"),
              br(),
              withSpinner(DT::dataTableOutput(ns("tabel_data")))
            ),
            tabPanel(
              "Ringkasan",
              icon = icon("briefcase"),
              br(),
              withSpinner(tableOutput(ns("ringkasan")))
            )
          )
        ),
        panel(
          heading = h4(tagList(icon("document"), "Bradley-Terry Model")),
          status = "primary",
          h4("Nilai Abilities"),
          withSpinner(DT::dataTableOutput(ns("tabel_abilities"))),
          hr(),
          h4("Preferensi"),
          withSpinner(plotOutput(ns("grafik_preferensi")))
        )
      )
    )
  )
}

multpairedPreference <- function(input, output, session) {
  ns <- session$ns
  
  # Data ----
  observe({
    if (input$sumber_data == "contoh") {
      enable("unggah")
    } else if (input$sumber_data == "unggah" & !is.null(input$data)) {
      enable("unggah")
    } else {
      disable("unggah")
    }
  })
  
  ## Tabel ----
  mentah <- eventReactive(input$unggah, {
    if (input$sumber_data == "contoh") {
      read_csv("data/mascaras.csv")
    } else if (input$sumber_data == "unggah") {
      read_csv(input$data$datapath)
    }
  })
  
  output$tabel_data <- DT::renderDataTable({
    mentah() %>% 
      datatable(
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy",
                text = "Salin"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Unduh Tabel"
              )
            )
          ,
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )
  
  ## Ringkasan ----
  observeEvent(input$unggah, {
    updatePickerInput(
      session = session,
      inputId = "panelis",
      choices = colnames(mentah())
    )
    
    updatePickerInput(
      session = session,
      inputId = "sampel_pref",
      choices = colnames(mentah())
    )
    
    updatePickerInput(
      session = session,
      inputId = "sampel_nonpref",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel_pref)  & !is.null(input$sampel_nonpref)
    )
  )
  
  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Hasil analisa data disajikan pada tab 'Bradley-Terry Model' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
                  title = strong("Parameter berhasil diterapkan!"),
                  footer = modalButton("Oke"),
                  easyClose = FALSE
      )
    )
  })
  
  ringkasan <- eventReactive(input$terapkan, {
    mentah() %>%
    {
      tibble(
        "Metode" = "Multiple Paired Preference with Bradley-Terry Model",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = c(.[, input$sampel_pref] %>% pull(), .[, input$sampel_nonpref] %>% pull()) %>% unique() %>% sort() %>% paste(collapse = ", "),
        "Jumlah Sampel" = c(.[, input$sampel_pref] %>% pull(), .[, input$sampel_nonpref] %>% pull()) %>% unique() %>% length()
      )
    } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })
  
  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)
  
  ## Bradley-Terry Model
  
  abilities <- eventReactive(input$terapkan, {
    dat <- mentah() %>% 
      mutate_all(funs(as.factor)) %>% 
      ntbt_xtabs(as.formula(paste("~", input$sampel_pref, "+", input$sampel_nonpref))) %>% 
      countsToBinomial() %>% 
      `colnames<-`(c("ProductA", "ProductB", "WinA", "WinB"))
    model <- BTm(outcome = cbind(WinA, WinB), player1 = ProductA, player2 = ProductB, data = dat)
    abilities <- BTabilities(model)
    res <- data_frame(Sampel = rownames(abilities),
                      Ability = abilities[,1],
                      `Standard Error` = abilities[,2])
    return(res)
  })
  
  output$tabel_abilities <- DT::renderDataTable({
    abilities() %>% 
      mutate_if(is.numeric, funs(round(.,3))) %>% 
      datatable(
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy",
                text = "Salin"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Unduh Tabel"
              )
            )
          ,
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )
  
  output$grafik_preferensi <- renderPlot({
    abilities() %>% 
      ggplot(aes(x = Ability, y = 0)) +
      geom_line() +
      geom_point() +
      geom_label_repel(aes(label = Sampel)) +
      labs(x = "Ability",
           y = "",
           title = "Preference Line") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
}
