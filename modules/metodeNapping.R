metodeNappingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        helper(
          h1("Napping"),
          type = "markdown",
          content = "metodeNapping",
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
            helpText(tags$small("Contoh menggunakan data Napping untuk produk smoothies. Pengaturan parameter adalah sebagai berikut: Panelis = 'Panelist', Sampel = 'Smoothies', Koordinat sumbu x = 'X', Koordinat sumbu y = 'Y'."))
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
              inputId = ns("x"),
              label = "Koordinat sumbu x",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("y"),
              label = "Koordinat sumbu y",
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
          heading = h4(tagList(icon("globe"), "Analisis Global")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Dimensi",
              icon = icon("plus"),
              h4("Analisis Faktor Berganda"),
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
                      choices = c("Sampel", "Panelis"),
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

metodeNapping <- function(input, output, session) {
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
      read_csv("data/smoothies_napping.csv")
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
      inputId = "x",
      choices = colnames(mentah())
    )
    updatePickerInput(
      session = session,
      inputId = "y",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel)  & !is.null(input$x) & !is.null(input$y)
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
        "Metode" = "Napping",
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
    n_panelis <- mentah() %>% 
      .[, input$panelis] %>% pull() %>% unique() %>% length()
    panelis <- mentah() %>% 
      .[, input$panelis] %>% pull() %>% unique()
    res <- mentah() %>% 
      select(input$panelis,
             input$sampel,
             input$x,
             input$y) %>% 
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             X = !!input$x,
             Y = !!input$y) %>% 
      gather(key = "Sumbu", value = "Koordinat", -c("Panelis", "Sampel")) %>% 
      unite("Kombinasi", c("Sumbu", "Panelis"), remove = FALSE) %>%
      mutate(Panelis = factor(Panelis, levels = unique(Panelis))) %>% 
      arrange(Sampel, Panelis, Sumbu) %>% 
      mutate(Kombinasi= factor(Kombinasi, unique(Kombinasi))) %>% 
      select(-c("Panelis", "Sumbu")) %>% 
      spread(key = "Kombinasi", value = "Koordinat") %>% 
      as.data.frame() %>% 
      `rownames<-`(.[, "Sampel"]) %>% 
      select_if(is.numeric) %>% 
      MFA(group = rep(2, n_panelis), 
          type = rep("c", n_panelis),
          name.group = panelis, 
          graph = FALSE)
    return(res)
  })
  
  ## Ringkasan Dimensi ----
  output$global_screeplot <- renderPlot({
    plot_eigen(global())
  })
  
  output$global_ringkasan <- renderTable({
    req(global())
    global()$eig %>%
      signif(3) %>%
      as_tibble(rownames = "Dimensi") %>%
      rename(
        Eigenvalue = eigenvalue,
        "Persentase Varian" = `percentage of variance`,
        "Persentase Kumulatif Varian" = `cumulative percentage of variance`
      ) %>%
      mutate(Dimensi = str_replace_all(Dimensi, "comp", "Dimensi")) %>%
      as.data.frame()
  })
  
  ## Deskripsi Dimensi ----
  global_dimensi <- reactive({
    list(
      Sampel = list(
        Koordinat = global()$ind$coord %>%
          signif(2) %>%
          `colnames<-`(paste("Dim", 1:ncol(.))),
        "Koordinat Parsial" = global()$ind$coord.partiel %>%
          signif(2) %>%
          `colnames<-`(paste("Dim", 1:ncol(.)))
      )
    )
  })
  
  output$global_dimensi <- renderPrint({
    global_dimensi()
  })
  
  ## Peta Persepsi ----
  grafikGlobal <- reactive({
    req(global())
    if (input$grafikGlobal_opsi == "Sampel") {
      plot_sample(
        res = global()$global.pca,
        axes = c(input$grafikGlobal_sumbu_x, input$grafikGlobal_sumbu_y),
        main = input$grafikGlobal_judul,
        lab.size = input$grafikGlobal_cex
      )
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
