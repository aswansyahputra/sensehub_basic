metodeSortingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        helper(
          h1("Sorting Task"),
          type = "markdown",
          content = "metodeSorting",
          size = "l"
        ),
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
            helpText(tags$small("Contoh menggunakan data Sorting Task untuk produk smoothies. Pengaturan parameter adalah sebagai berikut: Panelis = 'Panelist', Sampel = 'Smoothies', Komentar panelis = 'Comments', karakter pemisah = ';' (tanpa tanda kutip)."))
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
              inputId = ns("sampel"),
              label = "Sampel",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("atribut"),
              label = "Komentar panelis",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            tipify(
              textInput(
                inputId = ns("pemisah"),
                label = "Karakter pemisah kata dalam kolom",
                value = ";"
              ),
              title = "Contoh: ';'. Tuliskan tanpa tanda petik!", 
              placement = "top"
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
          heading = h4(tagList(icon("globe"), "Analisis Global")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Dimensi",
              icon = icon("plus"),
              h4("Analisis Korenspondensi"),
              fluidRow(
                column(5,
                       align = "center",
                       withSpinner(plotOutput(ns("global_screeplot")))
                ),
                column(7,
                       align = "center",
                       withSpinner(tableOutput(ns("global_ringkasan")))
                )
              )
            ),
            tabPanel(
              "Deskripsi Dimensi",
              icon = icon("file"),
              withSpinner(verbatimTextOutput(ns("global_dimensi")))
            ),
            tabPanel(
              "Frekuensi Atribut",
              icon = icon("bar-chart"),
              h4("Frekuensi Per Sampel"),
              pickerInput(
                inputId = ns("opsi_sampel"),
                label = "Sampel untuk ditampilkan:",
                choices = NULL,
                options = list(
                  title = "Pilih sampel",
                  size = 8
                )
              ),
              withSpinner(DT::dataTableOutput(ns("global_frekuensi"))),
              hr(),
              h4("Konsensus"),
              withSpinner(DT::dataTableOutput(ns("global_konsensus1"))),
              br(),
              withSpinner(DT::dataTableOutput(ns("global_konsensus2")))
            ),
            tabPanel(
              "Peta Persepsi",
              icon = icon("image"),
              fluidRow(
                column(
                  1,
                  dropdownButton(
                    circle = TRUE,
                    status = "primary",
                    icon = icon("gear"),
                    width = "300px",
                    tooltip = tooltipOptions(title = "Pengaturan"),
                    selectInput(
                      inputId = ns("grafikGlobal_opsi"),
                      label = "Pilihan Grafik",
                      choices = c("Sampel", "Atribut", "Konsensus", "Panelis"),
                      selected = "Sampel"
                    ),
                    numericInput(
                      inputId = ns("grafikGlobal_sumbu_x"),
                      label = "Dimensi Sumbu X",
                      min = 1,
                      max = 4,
                      value = 1
                    ),
                    numericInput(
                      inputId = ns("grafikGlobal_sumbu_y"),
                      label = "Dimensi Sumbu Y",
                      min = 2,
                      max = 5,
                      value = 2
                    ),
                    textInput(
                      inputId = ns("grafikGlobal_judul"),
                      label = "Judul Grafik",
                      value = ""
                    ),
                    sliderInput(
                      inputId = ns("grafikGlobal_cex"),
                      label = "Ukuran Label",
                      min = 2,
                      max = 8,
                      value = 4,
                      step = 1
                    )
                  )
                ),
                column(11,
                       align = "center",
                       withSpinner(plotOutput(ns("grafikGlobal"))),
                       uiOutput(ns("opsi_unduh_grafikGlobal"))
                )
              )
            )
          )
        )
        
      )
    )
  )
}

metodeSorting <- function(input, output, session) {
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
      read_csv("data/smoothies_sorting.csv")
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
      inputId = "sampel",
      choices = colnames(mentah())
    )
    updatePickerInput(
      session = session,
      inputId = "atribut",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel)  & !is.null(input$atribut) & !is.null(input$pemisah)
    )
  )
  
  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Hasil analisa data disajikan pada tab 'Analisis Global' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
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
        "Metode" = "Sorting Task",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% paste(collapse = ", "),
        "Jumlah Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% length()
      )
    } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })
  
  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)
  
  # Analisis Global ----
  global <- eventReactive(input$terapkan, {
    res <- mentah() %>% 
      select(input$panelis,
             input$sampel,
             input$atribut) %>% 
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Atribut = !!input$atribut) %>% 
      spread(key = "Panelis", value = "Atribut") %>% 
      as.data.frame() %>% 
      `row.names<-`(.[,"Sampel"]) %>% 
      select(-Sampel) %>% 
      fast_modified(sep.words = input$pemisah, graph = FALSE, proba = 1)
    return(res)
  })
  
  ## Ringkasan Dimensi ----
  output$global_screeplot <- renderPlot({
    plot_eigen(global(), threshold = FALSE)
  })
  
  output$global_ringkasan <- renderTable({
    req(global())
    global()$acm$eig %>%
      signif(3) %>%
      as_tibble(rownames = "Dimensi") %>%
      rename(
        Eigenvalue = eigenvalue,
        "Persentase Varian" = `percentage of variance`,
        "Persentase Kumulatif Varian" = `cumulative percentage of variance`
      ) %>%
      mutate(Dimensi = str_replace_all(Dimensi, "dim", "Dimensi")) %>%
      as.data.frame()
  })
  
  ## Deskripsi Dimensi ----
  global_dimensi <- reactive({
    list(
      Sampel = list(
        Koordinat = global()$acm$ind$coord %>%
          signif(2) %>%
          `colnames<-`(paste("Dim", 1:ncol(.)))
      ),
      Atribut = list(
        Koordinat = global()$acm$var$coord %>%
          signif(2) %>%
          `colnames<-`(paste("Dim", 1:ncol(.)))
      )
    )
  })
  
  output$global_dimensi <- renderPrint({
    global_dimensi()
  })
  
  ## Frekuensi Atribut
  observeEvent(input$terapkan, {
    updatePickerInput(
      session = session,
      inputId = "opsi_sampel",
      choices = pull(unique(mentah()[, input$sampel]))
    )
  })
  
  global_frekuensi <- eventReactive(input$opsi_sampel, {
    global()$textual %>%
      map(`colnames<-`, c("% Internal", "% Global", "Frekuensi Internal", "Frekuensi Global", "P value", "V test")) %>% 
      map(signif, 2) %>% 
      map(as_tibble, rownames = "Atribut") %>% 
      `[[`(input$opsi_sampel)
  })
  
  output$global_frekuensi <- DT::renderDataTable({
    global_frekuensi() %>% 
      datatable(
        rownames = FALSE,
        caption = "Ringkasan analisa frekuensi atribut per sampel. Nilai V test positif menunjukan atribut tersebut sering digunakan untuk mendeskripsikan sampel, sedangkan negatif menunjukan atribut tersebut jarang digunakan untuk mendeskripsikan sampel.",
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
  
  global_konsensus1 <- reactive({
    global() %>% 
      `[[`("descriptor") %>% 
      as_tibble(rownames = "Atribut") %>%
      rename(Frekuensi = Occurrence) %>% 
      arrange(-Frekuensi)
  })
  
  output$global_konsensus1 <- DT::renderDataTable({
    
    global_konsensus1() %>% 
      formattable(list(Frekuensi = color_bar_custom(bg.color = "#34495e", text.col = "white"))) %>% 
      as.datatable(
        rownames = FALSE,
        caption = "Frekuensi atribut dari seluruh panelis.",
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
  
  global_konsensus2 <- reactive({
    ConsensualWords(global(), graph = FALSE)
  })
  
  output$global_konsensus2 <- DT::renderDataTable({
    global_konsensus2() %>% 
      `[[`("Results.Bootstrap") %>% 
      as_tibble(rownames = "Atribut") %>% 
      rename(Frekuensi = nb.times, 
             "Within Inertia" = within.inertia,
             "P value" = prob) %>% 
      datatable(
        rownames = FALSE,
        caption = "Ringkasan analisa Bootstraap untuk metode Sorting Task dengan kriteria frekuensi > 2.",
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
  
  ## Peta Persepsi ----
  grafikGlobal <- reactive({
    req(global())
    if (input$grafikGlobal_opsi == "Sampel") {
      plot_sample(global()$acm, 
                  axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                  main = input$grafikGlobal_judul,
                  lab.size = input$grafikGlobal_cex)
    } else if (input$grafikGlobal_opsi == "Atribut") {
      plot_attribute(res = global()$acm, 
                     axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                     main = input$grafikGlobal_judul,
                     lab.size = input$grafikGlobal_cex)
    } else if (input$grafikGlobal_opsi == "Konsensus") {
      plot_consensus(res = global(), 
                     axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y), 
                     main = input$grafikGlobal_judul,
                     lab.size = input$grafikGlobal_cex)
    } else if (input$grafikGlobal_opsi == "Panelis") {
      plot_group(
        res = global(),
        axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y),
        main = input$grafikGlobal_judul,
        lab.size = input$grafikGlobal_cex
      )
    } 
  })
  
  output$grafikGlobal <- renderPlot({
    plot(grafikGlobal())
  })
  
  observeEvent(input$terapkan, {
    output$opsi_unduh_grafikGlobal <- renderUI({
      downloadButton(ns("unduh_grafikGlobal"), label = "Simpan Grafik")
    })
  })
  
  output$unduh_grafikGlobal <- downloadHandler(
    filename = "Grafik - SenseHub.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 72, units = "in"
        )
      }
      ggsave(file, plot = grafikGlobal(), device = device)
    }
  )
}
