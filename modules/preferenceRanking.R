preferenceRankingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Preference Ranking"),
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
            helpText(tags$small("Contoh menggunakan data preferensi untuk produk Keju. Pengaturan parameter adalah sebagai berikut: Panelis = 'Panelist', Sampel = 'Cheese', Skor Ranking = 'Preference Rank'"))
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
              inputId = ns("ranking"),
              label = "Skor Ranking",
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
            heading = h4(tagList(icon("line-chart"), "Analisis Lokal")),
            status = "primary",
            tabsetPanel(
              tabPanel(
                "Ringkasan Statistik",
                icon = icon("bookmark"),
                h4("Atribut Kesukaan"),
                withSpinner(DT::dataTableOutput(ns("statistik_atributUtama")))
              ),
              tabPanel(
                "Tabel",
                icon = icon("table"),
                h4("Preferensi"),
                withSpinner(DT::dataTableOutput(ns("nilai_atributUtama")))
              ),
              tabPanel(
                "Grafik",
                icon = icon("image"),
                hr(),
                h4("Preferensi"),
                withSpinner(plotOutput(ns("grafik_atributUtama")))
              )
            )
          )
      )
    )
  )
}

preferenceRanking <- function(input, output, session) {
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
      read_csv("data/cheese_preference.csv")
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
      inputId = "ranking",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel) & !is.null(input$ranking)
    )
  )
  
  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Hasil analisa data disajikan pada tab 'Analisis Lokal' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
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
        "Metode" = "Preference Ranking",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = .[, input$ranking] %>% pull() %>% unique() %>% sort() %>% paste(collapse = ", "),
        "Jumlah Sampel" = .[, input$ranking] %>% pull() %>% unique() %>% length()
      )
    } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })
  
  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)
  
  # Analisis Lokal ----
  
  ## Atribut Utama ----
  lokal_atributUtama <- eventReactive(input$terapkan, {
    req(mentah())
    mentah() %>%
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Ranking = !!input$ranking) %>%
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      ntbt_friedman.test(Ranking ~ Sampel | Panelis) %>% 
      tidy() %>% {
        data_frame(Atribut = "Preferensi",
                   Chi_stat = .$statistic,
                   P_value = .$p.value)
      }
  })
  
  ### Ringkasan Statistik ----
  statistik_atributUtama <- reactive({
    req(lokal_atributUtama())
    lokal_atributUtama() %>%
      select(Atribut, Chi_stat, P_value) %>%
      mutate(
        Chi_stat = round(Chi_stat, 3),
        P_value = signif(P_value, 3),
        " " = if_else(P_value < 0.01, "**",
                      if_else(P_value > 0.05, "t.s.", "*")
        )
      ) %>%
      arrange(P_value) %>% 
      rename("Chi-squared statistik" = Chi_stat, "P value" = P_value)
  })
  
  output$statistik_atributUtama <- DT::renderDataTable({
    statistik_atributUtama() %>%
      datatable(
        rownames = FALSE,
        caption = "Ringkasan parameter statistik dari Friedman Rank Sum Test. Tanda (**) menunjukan signifikansi pada tingkat kepercayaan 99%, tanda (*) pada tingkat kepercayaan 95%, sedangkan t.s. menunjukan tidak beda signifikan.",
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brt",
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
        )
      )
  }, server = FALSE
  )
  
  ### Tabel Nilai ----
  nilai_atributUtama <- reactive({
    req(lokal_atributUtama())
    mentah() %>%
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Ranking = !!input$ranking) %>%
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      group_by(Panelis) %>% 
      mutate(Rank = rank(Ranking)) %>% 
      group_by(Sampel) %>% 
      summarise("JumlahRanking" = sum(Rank))
  })
  
  output$nilai_atributUtama <- DT::renderDataTable({
    nilai_atributUtama() %>% 
      datatable(
        rownames = FALSE,
        caption = "Tabel Nilai Jumlah Ranking",
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
  
  
  ### Grafik ----
  
  grafik_atributUtama <- reactive({
    req(nilai_atributUtama())
    nilai_atributUtama() %>% 
      ggplot(aes_string(x = "Sampel", y = "JumlahRanking")) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Jumlah Ranking", title = input$opsi_grafik_atributUtama) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid = element_blank(),
        panel.background = element_blank()
      )
  })
  
  output$grafik_atributUtama <- renderPlot({
    grafik_atributUtama()
  })
  
}
