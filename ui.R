ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo.png"),
    includeScript("google-analytics.js"),
    tags$style(type = "text/css", "body {padding-top: 70px;}")
  ),
  navbarPage(
    title = "SenseHub",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("flatly"),
    windowTitle = "SenseHub: Aplikasi Web Terintegrasi untuk Analisis Sensoris",
    tabPanel(
      "Rancangan Percobaan",
      icon = icon("cogs"),
      rancanganPercobaanUI("rancangan")
    ),
    tabPanel(
      "Performa Panelis",
      icon = icon("address-card"),
      performaPanelisUI("performa")
    ),
    tabPanel(
      "Uji Diskriminatif",
      icon = icon("search-plus"),
      ujiDiskriminasiUI("diskriminasi")
    ),
    navbarMenu(
      "Uji Deskriptif",
      icon = icon("leanpub"),
      "Kuantitatif",
      tabPanel(
        "Quantitative Descriptive Analysis (QDA)",
        metodeQdaUI("qda")
      ),
      tabPanel(
        "Flash Profiling (FP)",
        metodeFpUI("fp")
      ),
      tabPanel(
        "Free Choice Profiling (FCP)",
        metodeFcpUI("fcp")
      ),
      tabPanel(
        "Rate-all-that-Apply (RATA)",
        metodeRataUI("rata")
      ),
      "----",
      "Kualitatif",
      tabPanel(
        "Check-all-that-Apply (CATA)",
        metodeCataUI("cata")
      ),
      tabPanel(
        "Rate-all-that-Apply (RATA as CATA)",
        metodeRata2UI("rata_as_cata")
      ),
      "----",
      "Sorting",
      tabPanel(
        "Sorting Task",
        metodeSortingUI("sorting")
      ),
      tabPanel(
        "Hierarchical Sorting Task",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      ),
      "----",
      "Projective Mapping",
      tabPanel(
        "Napping",
        metodeNappingUI("napping")
      ),
      tabPanel(
        "Sorted Napping",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    navbarMenu(
      "Uji Afektif",
      icon = icon("heart"),
      "Penerimaan",
      tabPanel(
        "Acceptance Test",
        acceptanceTestUI("acceptance")
      ),
      "----",
      "Preferensi",
      tabPanel(
        "Paired Preference",
        pairedPreferenceUI("pairedpref")
      ),
      tabPanel(
        "Multiple Paired Preference",
        multpairedPreferenceUI("multpref")
      ),
      tabPanel(
        "Preference Ranking",
        preferenceRankingUI("prefrank")
      ),
      "----",
      "Hedonik",
      tabPanel(
        "Hedonic Rating",
        hedonicRatingUI("hedonic")
      ),
      "----",
      "Drivers of liking",
      tabPanel(
        "Internal Preference Mapping (MDPref)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      ),
      tabPanel(
        "External Preference Mapping (PrefMap)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    navbarMenu(
      "Optimasi",
      icon = icon("arrow-up"),
      tabPanel(
        "Just-about-Right (JAR)",
        metodeJarUI("jar")
      ),
      tabPanel(
        "Ideal Profile Method (IPM)",
        h1("Maaf, fitur sedang dalam tahap pengembangan.")
      )
    ),
    tabPanel(
      "Ihwal",
      icon = icon("support"),
      wellPanel(
        includeMarkdown("README.md")
      )
    ),
    tabPanel(
      "Keluar",
      icon = icon("sign-out"),
      fluidRow(
        column(4),
        column(
          4,
          wellPanel(
            h4("Apakah Anda yakin ingin keluar?"),
            actionButton(
              "kembali",
              "Kembali",
              onclick = "location.href='https://sensehub.sensolution.id/';"
            ),
            actionButton(
              "keluar",
              "Keluar",
              onclick = "location.href='https://sensehub.sensolution.id/logout';"
            )
          )
        ),
        column(4)
      )
    )
  )
)