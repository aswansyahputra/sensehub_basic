metodeJarUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        helper(
          h1("Just-about-Right"),
          type = "markdown",
          content = "metodeJar",
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
            helpText(tags$small("Contoh menggunakan data JAR untuk produk parfum. Pengaturan parameter adalah sebagai berikut: Panelis = 'consumer', Sampel = 'product', Atribut sensoris = 'intensity'-'green', Kesukaan = 'liking', Nilai skala JAR = 0."))
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
              inputId = ns("atribut"),
              label = "Atribut Sensoris",
              choices = NULL,
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE,
                title = "Pilih kolom",
                "deselect-all-text" = "Hapus semua",
                "select-all-text" = "Pilih semua",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("kesukaan"),
              label = "Kesukaan",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            numericInput(
              inputId = ns("skalaJAR"),
              label = "Nilai tengah skala JAR",
              value = 0
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
          heading = h4(tagList(icon("globe"), "Analisis Penalti")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Nilai Penalti",
              icon = icon("plus"),
              pickerInput(
                inputId = ns("opsi_sampel"),
                label = "Pilih sampel untuk ditampilkan",
                choices = NULL,
                options = list(
                  title = "Pilih sampel",
                  size = 8
                )
              ),
              withSpinner(DT::dataTableOutput(ns("nilai_penalti")))
            ),
            tabPanel(
              "Grafik Penalti",
              icon = icon("image"),
              withSpinner(plotOutput(ns("grafik_penalti"))),
              uiOutput(ns("opsi_unduh_grafik"))
            )
          )
        )
      )
    )
  )
}

metodeJar <- function(input, output, session) {
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
      read_csv("data/perfumes_jar.csv")
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
    updatePickerInput(
      session = session,
      inputId = "kesukaan",
      choices = colnames(mentah())
    )
  }, ignoreInit = TRUE)
  
  observe(
    toggleState(
      id = "terapkan",
      condition = !is.null(input$panelis) & !is.null(input$sampel)  & !is.null(input$atribut) & !is.null(input$kesukaan) & !is.null(input$skalaJAR)
    )
  )
  
  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Hasil analisa data disajikan pada tab 'Analisis Penalti' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
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
        "Metode" = "Just-about-Right",
        "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
        "Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% paste(collapse = ", "),
        "Jumlah Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% length(),
        "Atribut" = paste(input$atribut, collapse = ", "),
        "Jumlah Atribut" = length(input$atribut),
        "Kesukaan" = input$kesukaan
      )
    } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })
  
  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)
  
  observeEvent(input$terapkan, {
    updatePickerInput(
      session = session,
      inputId = "opsi_sampel",
      choices = unique(pull(mentah()[, isolate(input$sampel)]))
    )
  }, ignoreInit = TRUE)
  
  # Analisis Penalti ----
  penalti <- eventReactive(input$terapkan, {
    df <- mentah() %>% 
      select(input$panelis, 
             input$sampel, 
             input$atribut,
             input$kesukaan) %>% 
      rename(Panelis = !!input$panelis,
             Sampel = !!input$sampel,
             Kesukaan = !!input$kesukaan) %>% 
      mutate(Panelis = as.factor(Panelis),
             Sampel = as.factor(Sampel)) %>% 
      mutate_at(vars(input$atribut), .funs = function(x) {if_else(x < input$skalaJAR, "Low", if_else(x > input$skalaJAR, "High", "JAR"))})
    
    sampel <- levels(df$Sampel)
    
    output1 <- map(sampel, ~filter(.data = df, Sampel == .x) %>% 
          select(-Sampel) %>% 
          gather(key = "Atribut", value = "Label", -c(Panelis, Kesukaan)) %>% 
          mutate(Label = factor(Label, levels = c("JAR", "High", "Low"))) %>% 
          group_by(Atribut) %>% 
          nest(.key = "Data") %>% 
          mutate(Model = map(Data, ~ aov(Kesukaan ~ Label, data = .)),
                 ANOVA = map(Model, summary.lm),
                 Tidy_ANOVA = map(ANOVA, tidy)) %>% 
          select(Atribut, Tidy_ANOVA) %>% 
          unnest() %>% 
          select(Atribut, term, estimate, p.value) %>% 
          filter(term != '(Intercept)') %>%
          mutate(term = str_remove(term, "Label")) %>% 
          rename(Quality = term,
                 Penalty = estimate,
                 "P value" = p.value)) %>% 
      `names<-`(sampel)
    
    output2 <- map(sampel, ~map(input$atribut, ~table(unlist(df[,"Sampel"]), unlist(df[,.x]))) %>% 
          `names<-`(input$atribut) %>% 
          map(., ~ as_tibble(100*prop.table(.x, margin = 1))) %>% 
          bind_rows(.id = "Atribut") %>% 
          spread(key = "Var2", value = "n") %>% 
          rename(Sampel = Var1) %>% 
          filter(Sampel == .x) %>% 
          select(-c(Sampel, JAR)) %>% 
          gather(key = "Quality", value = "Frequency", -Atribut)) %>% 
      `names<-`(sampel)
    
    res <- map2(output1, output2, ~left_join(.x, .y, by = c("Atribut", "Quality")) %>% 
           mutate(Atribut = if_else(`P value` <= 0.05, paste0(Atribut, "*"), Atribut),
                  Penalty = abs(Penalty)) %>% 
           select(Atribut, Quality, Frequency, everything()))
    return(res)
  })
  
  ## Nilai Penalti
  nilai_penalti <- eventReactive(input$opsi_sampel, {
    penalti()[[input$opsi_sampel]]
  })
  
  output$nilai_penalti <- DT::renderDataTable({
    nilai_penalti() %>% 
      mutate(Frequency = round(Frequency, 3),
             Penalty = round(Penalty, 3),
             `P value` = signif(`P value`, 3)) %>% 
      datatable(
        rownames = FALSE,
        caption = "Tabel ringkasan analisa penalti",
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
  
  ## Grafik Penalti
  grafik_penalti <- eventReactive(input$opsi_sampel,{
    nilai_penalti() %>% 
      ggplot(aes(x = Frequency, y = Penalty, col = Quality)) +
      geom_point() +
      geom_text_repel(aes(label = Atribut), show.legend = FALSE) +
      scale_color_manual(values = c("blue", "red")) +
      geom_vline(xintercept = 20, lty = 2) +
      labs(col = "", title = paste("Penalty analysis:", isolate(input$opsi_sampel))) +
      theme_ipsum() +
      theme(legend.position = c(1,1))
  })
  
  output$grafik_penalti <- renderPlot({
    grafik_penalti()
  })
  
  observeEvent(input$terapkan, {
    output$opsi_unduh_grafik <- renderUI({
      downloadButton(ns("unduh_grafik"), label = "Simpan Grafik")
    })
  })
  
  output$unduh_grafik <- downloadHandler(
    filename = "Grafik - SenseHub.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 72, units = "in"
        )
      }
      ggsave(file, plot = grafik_penalti(), device = device)
    }
  )
}
