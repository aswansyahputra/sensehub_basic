performaPanelisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h1("Performa Panelis"),
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
            helpText(tags$small("Contoh menggunakan data QDA dari panelis terlatih. Pengaturan parameter adalah sebagai berikut: Panelis = 'Panelist', Sampel = 'Product', Sesi = 'Session', Urutan = 'Rank', Atribut = 'Spicy' - 'Wrapping'."))
          ),
          conditionalPanel(
            condition = "input.sumber_data == 'unggah'",
            ns = ns,
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
              inputId = ns("sesi"),
              label = "Sesi",
              choices = NULL,
              options = list(
                title = "Pilih kolom",
                size = 8
              )
            ),
            pickerInput(
              inputId = ns("urutan"),
              label = "Urutan Penyajian Sampel",
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
          heading = h4(tagList(icon("dashboard"), "Performa")),
          status = "primary",
          tabsetPanel(
            tabPanel(
              "Performa Panel",
              icon = icon("users"),
              br(),
              withSpinner(DT::dataTableOutput(ns("panel"))),
              hr(),
              textOutput(ns("penjelasan_panel"))
            ),
            tabPanel(
              "Performa Panelis",
              icon = icon("user"),
              h4("Kemampuan Diskriminasi"),
              withSpinner(DT::dataTableOutput(ns("panelis_diskriminasi"))),
              hr(),
              h4("Konsensus Terhadap Panel"),
              withSpinner(DT::dataTableOutput(ns("panelis_konsensus")), type = 7, color = "#3c8dbc"),
              hr(),
              h4("Konsistensi"),
              withSpinner(DT::dataTableOutput(ns("panelis_konsistensi")))
            )
          )
        )
      )
    )
  )
}

performaPanelis <- function(input, output, session) {
  ns <- session$ns

  observe({
    if (input$sumber_data == "contoh") {
      enable("unggah")
    } else if (input$sumber_data == "unggah" & !is.null(input$data)) {
      enable("unggah")
    } else {
      disable("unggah")
    }
  })

  mentah <- eventReactive(input$unggah, {
    if (input$sumber_data == "contoh") {
      read_csv("data/perfumes_qda_experts.csv")
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
      inputId = "sesi",
      choices = colnames(mentah())
    )

    updatePickerInput(
      session = session,
      inputId = "urutan",
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
      condition = !is.null(input$panelis) & !is.null(input$sampel) & !is.null(input$sesi) & !is.null(input$urutan) & !is.null(input$atribut)
    )
  )

  observeEvent(input$terapkan, {
    showModal(
      modalDialog("Silakan periksa tab 'Ringkasan' untuk melihat ringkasan parameter penelitian. Analisis performa panel dan panelis disajikan pada tab 'Performa' (diperlukan waktu beberapa detik untuk proses kalkulasi).",
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
          "Jumlah Panelis" = .[, input$panelis] %>% pull() %>% unique() %>% length(),
          "Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% paste(collapse = ", "),
          "Jumlah Sampel" = .[, input$sampel] %>% pull() %>% unique() %>% length(),
          "Jumlah Sesi" = .[, input$sesi] %>% pull() %>% unique() %>% length(),
          "Atribut" = paste(input$atribut, collapse = ", "),
          "Jumlah Atribut" = length(input$atribut)
        )
      } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })

  output$ringkasan <- renderTable({
    ringkasan()
  }, colnames = FALSE)

  res <- eventReactive(input$terapkan, {
    req(mentah())
    if (is.null(input$sesi) & !is.null(input$urutan)) {
      mentah() %>%
        rename(
          Panelis = !!input$panelis,
          Sampel = !!input$sampel,
          Urutan = !!input$urutan
        ) %>%
        mutate(
          Panelis = as.factor(Panelis),
          Sampel = as.factor(Sampel),
          Urutan = as.factor(Urutan)
        ) %>%
        select(Panelis, Sampel, Urutan, one_of(input$atribut)) %>%
        as.data.frame() %>%
        paneliperf(.,
          formul = "~Sampel+Panelis+Urutan+Sampel:Panelis",
          formul.j = "~Sampel", col.j = which(names(.) == "Panelis"),
          firstvar = min(which(names(.) %in% input$atribut)),
          lastvar = max(which(names(.) %in% input$atribut)),
          synthesis = TRUE
        )
    } else if (!is.null(input$sesi) & is.null(input$urutan)) {
      mentah() %>%
        rename(
          Panelis = !!input$panelis,
          Sampel = !!input$sampel,
          Sesi = !!input$sesi
        ) %>%
        mutate(
          Panelis = as.factor(Panelis),
          Sampel = as.factor(Sampel),
          Sesi = as.factor(Sesi)
        ) %>%
        select(Panelis, Sampel, Sesi, one_of(input$atribut)) %>%
        as.data.frame() %>%
        paneliperf(.,
          formul = "~Sampel+Panelis+Sesi+Sampel:Panelis+Sampel:Sesi+Panelis:Sesi",
          formul.j = "~Sampel+Sesi", col.j = which(names(.) == "Panelis"),
          firstvar = min(which(names(.) %in% input$atribut)),
          lastvar = max(which(names(.) %in% input$atribut)),
          synthesis = TRUE
        )
    } else if (is.null(input$sesi) & is.null(input$urutan)) {
      mentah() %>%
        rename(
          Panelis = !!input$panelis,
          Sampel = !!input$sampel
        ) %>%
        mutate(
          Panelis = as.factor(Panelis),
          Sampel = as.factor(Sampel)
        ) %>%
        select(Panelis, Sampel, one_of(input$atribut)) %>%
        as.data.frame() %>%
        paneliperf(.,
          formul = "~Sampel+Panelis+Sampel:Panelis",
          formul.j = "~Sampel", col.j = which(names(.) == "Panelis"),
          firstvar = min(which(names(.) %in% input$atribut)),
          lastvar = max(which(names(.) %in% input$atribut)),
          synthesis = TRUE
        )
    } else {
      mentah() %>%
        rename(
          Panelis = !!input$panelis,
          Sampel = !!input$sampel,
          Sesi = !!input$sesi,
          Urutan = !!input$urutan
        ) %>%
        mutate(
          Panelis = as.factor(Panelis),
          Sampel = as.factor(Sampel),
          Sesi = as.factor(Sesi),
          Urutan = as.factor(Urutan)
        ) %>%
        select(Panelis, Sampel, Sesi, Urutan, one_of(input$atribut)) %>%
        as.data.frame() %>%
        paneliperf(.,
          formul = "~Sampel+Panelis+Sesi+Urutan+Sampel:Panelis+Sampel:Sesi+Panelis:Sesi",
          formul.j = "~Sampel+Sesi", col.j = which(names(.) == "Panelis"),
          firstvar = min(which(names(.) %in% input$atribut)),
          lastvar = max(which(names(.) %in% input$atribut)),
          synthesis = TRUE
        )
    }
  })

  output$panel <- DT::renderDataTable({
    preferable <- style(
      "background-color" = "#34495e",
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    not_preferable <- style(
      "background-color" = csscolor("whitesmoke"),
      color = "black",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    cond_formatter <- formatter("span", style = x ~ ifelse(x < 0.05, preferable, not_preferable))

    res()$p.value %>%
      as_tibble(rownames = "Atribut") %>%
      setNames(str_remove_all(names(.), " ")) %>%
      arrange(Sampel) %>%
      mutate_if(is.numeric, signif, 2) %>%
      formattable(
        list(area(col = 2:ncol(.)) ~ cond_formatter)
      ) %>%
      as.datatable(
        caption = "Tabel ringkasan P-value dari analisa varian model I 'Atribut ~ Sampel + Panelis + Sesi + Urutan + Sampel:Panelis + Sampel:Sesi + Panelis:Sesi'. Sel berwarna biru menunjukan signifikansi pada level kepercayaan 95%.",
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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
            ),
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )

  observeEvent(input$terapkan, {
    output$penjelasan_panel <- renderText({
      paste("Kemampuan diskriminasi panel (discrimination ability) dinyatakan dalam komponen 'Sampel', konsensus panel (agreement) dinyatakan dalam komponen 'Sampel:Panelis', sedangkan konsistensi panel (repeatibility) dinyatakan dalam komponen 'Sampel:Sesi'. Komponen 'Urutan' menyatakan apakah urutan penyajian sampel berpengaruh terhadap penilaian sampel.")
    })
  })

  output$panelis_diskriminasi <- DT::renderDataTable({
    preferable <- style(
      "background-color" = "#34495e",
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    not_preferable <- style(
      "background-color" = csscolor("whitesmoke"),
      color = "black",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    cond_formatter <- formatter("span", style = x ~ ifelse(x < 0.05, preferable, not_preferable))

    res()$prob.ind %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "Panelis") %>%
      select(-median) %>%
      filter(Panelis != "median") %>%
      mutate_if(is.numeric, signif, 2) %>%
      formattable(
        list(area(col = 2:ncol(.)) ~ cond_formatter)
      ) %>%
      as.datatable(
        caption = "Tabel ringkasan P-value dari analisa varian model II 'Atribut ~ Sampel + Sesi' untuk tiap panelis. Sel berwarna biru menunjukan signifikansi pada level kepercayaan 95%.",
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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
            ),
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )

  output$panelis_konsensus <- DT::renderDataTable({
    preferable <- style(
      "background-color" = "#34495e",
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    not_preferable <- style(
      "background-color" = csscolor("whitesmoke"),
      color = "black",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    cond_formatter <- formatter("span", style = x ~ ifelse(x >= 0.5, preferable, not_preferable))

    res()$agree.ind %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "Panelis") %>%
      select(-median) %>%
      select(Panelis, rev(everything())) %>%
      filter(Panelis != "median") %>%
      .[nrow(.):1, ] %>%
      mutate_if(is.numeric, signif, 2) %>%
      formattable(
        list(area(col = 2:ncol(.)) ~ cond_formatter)
      ) %>%
      as.datatable(
        caption = "Tabel ringkasan nilai korelasi antara panelis dan panel berdasarkan perbandingan koefisien 'Sampel' dari analisa varian model I dan model II. Sel berwarna biru menunjukan nilai korelasi diatas 0.5.",
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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
            ),
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )

  output$panelis_konsistensi <- DT::renderDataTable({
    preferable <- style(
      "background-color" = "#34495e",
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    not_preferable <- style(
      "background-color" = csscolor("whitesmoke"),
      color = "black",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )

    cond_formatter <- formatter("span", style = x ~ ifelse(x <= 1.96, preferable, not_preferable))

    res()$res.ind %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "Panelis") %>%
      select(-median) %>%
      filter(Panelis != "median") %>%
      mutate_if(is.numeric, round, 2) %>%
      formattable(
        list(area(col = 2:ncol(.)) ~ cond_formatter)
      ) %>%
      as.datatable(
        caption = "Tabel ringkasan nilai standar deviasi galat dari analisa varian model I dan model II. Sel berwarna biru menunjukan standar deviasi dibawah 1.96.",
        rownames = FALSE,
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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
            ),
          language = list(
            info = "Menampilkan data ke-_START_ hingga ke-_END_ dari total _TOTAL_ data"
          )
        )
      )
  },
  server = FALSE
  )
}
