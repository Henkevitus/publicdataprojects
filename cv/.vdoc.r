#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: cvevents
cvevents <- function(tbl, when, what, where, descr) {

    command_start <- "\\cvevent"
    tbl[[where]] <- gsub("\n", " \\\\newline ", tbl[[where]])
    res <- paste0(
        command_start, 
        "{", tbl[[when]], "}", 
        "{", tbl[[what]], "}",
        "{", tbl[[where]], "}",
        "{", tbl[[descr]], "}"
        )

    cat(res, sep = "\n\n\n") 
}

cvproj <- function(tbl,what,role,items){
    command_start <- "\\cvproj"
    res <- paste0(
        command_start, 
        "{", tbl[[what]], "}",
        "{", tbl[[role]], "}",
        "{", sapply(tbl[[items]], function(x)paste0(x,collapse = ", ")), "}"
    )

    cat(res, sep = "\n\n\n")
}
#
#
#
#
#
#
#
#
#
# Define vectors for each column
roles <- c(
  "Ph.D student", 
  "Data Manager", 
  "Scientific Assistant"
)

institutions <- c(
  "Aalborg University, Steno Diabetes Center North Denmark", 
  "Steno Diabetes Center North Denmark", 
  "Danish Center for Healthcare Improvements"
)

dates <- c(
  "04/2020 - Present", 
  "01/2019 - 03/2020", 
  "11/2017 - 08/2018"
)

details <- c(
  "My Ph.D. combines systematically reviewing cost-effectiveness analyses of medications for treating Type 2 Diabetes, with testing assumptions in these decision-analytical models against real-world data, further developing my expertise in epidemiological study design. I have taken courses in biostatistics, advanced epidemiology, time to event analysis, and decision-analytical modelling using R.",
  "In this position I gained expertise in epidemiological study design, data wrangling, analysis, and presentation. In dialogue with medical doctors, we defined the cohorts necessary to answer research questions regarding treatment and outcomes for patients with Diabetes Mellitus or Osteoporosis.",
  "I finished my first systematic review and investigated methodological challenges regarding cost-analysis of extremely expensive haematological medications."
)
#
#
#
#| label: experience
#| output: asis
tibble::tribble(
    #"Position X", "Where", "When", "Details"    
    ~role, ~institution, ~dates, ~details,
    roles[1], institutions[1], dates[1], details[1],
    roles[2], institutions[2], dates[2], details[2],
    roles[3], institutions[3], dates[3], details[3],

) |>
    cvevents(when = "dates", what = "role", where = "institution", descr = "details")
#
#
#
#
#
#
#
#
#
#
whats <- c(
  "Master: Medical Market Access",
  "Bachelor: Medicine With Industrial Specialisation",
  "Review - PharmacoEconomics Journal - 2023",
  "Survival Analysis - Current Drug Safety - 2020",
  "Cohort study - Bone - 2020",
  #"Cohort study - Diabetic Medicine - 2020" - erstattet med den hvor jeg lavede dataset på 3 dage
)

roles <- c(
  "Aalborg University 2014 - 2017" ,
  "Aalborg University 2011 - 2014" ,
  "DOI: 10.1007/s40273-023-01268-5",
  "DOI: 10.2174/1574886315666200819114629",
  "DOI: 10.1016/j.bone.2019.115083",
  #"DOI: 10.1111/dme.14251" - prioriterer at vise at jeg har lavet datasæt på kort tid med min bone artikel med abrahamsen
)  
  
items <- c(
  "Health conomics, medical market access, quality improvement, big picture and detail-level pragmatic thinking.",
  "A solid foundation in medicine to understand and utilise health data.",
  "Gathering large amounts of data and information, and synthesising methodological critiques of state of the art decision-analytical models.",
  "Setting up a nationwide cohort study using registry data to perform, survival analysis, create baseline tables, kaplan-meier plots and cox proportional hazards models. The study highlights connection between SGLT2s and diabetic ketoacidosis.",
  "Creating a dataset for the purpose of examining hip fractures and quality checking it for publication-ready analyses in less than a week."
  # "Setting up a case-control study using registry data. Learned a lot of basic data cleaning, wrangling, and statistical programming making for this paper."
)
#
#
#
#| label: education
#| output: asis
    # Education name and dates, education place, education description
    # Journal - year, DOI, publication title and description
tibble::tribble(
    ~what, ~role, ~items,
    whats[1], roles[1], items[1],
    whats[2], roles[2], items[2],
    whats[3], roles[3], items[3],
    whats[4], roles[4], items[4],
    whats[5], roles[5], items[5],
) |>
    cvproj(what = "what", role = "role", items = "items")
#
#
#
#
#
#| label: other
#| output: asis
tibble::tribble(
    ~what, ~role, ~items,
    "Quarto blog", "Data analysis practice", c("Recently started a Quarto blog where i delve into publicly available data in Denmark to comment on and extract insights","Find the URL under the *Hobbies* section."),
    "Østerport Ejerforening", "Board member", c("Organisation of events and tasks via Trello","coordinating events","communication with apartment owners about their wants and needs","long-term planning regarding apartment complex condition."),
    "Aalborg Klatreklub", "Climbing instructor", c("Organising events", "teaching climbing safety, technique and equipment use."),
) |>
    cvproj(what = "what", role = "role", items = "items")
#
#
#
#
#
# Define vectors for each column
roles <- c(
  "Ph.D student", 
  "Data Manager", 
  "Scientific Assistant"
)

institutions <- c(
  "Aalborg University and Steno Diabetes Center North Denmark", 
  "Steno Diabetes Center North Denmark", 
  "Danish Center for Healthcare Improvements"
)

dates <- c(
  "04/2020 - Present", 
  "01/2019 - 03/2020", 
  "11/2017 - 08/2018"
)

details <- c(
  "My Ph.D. combines systematically reviewing cost-effectiveness analyses of medications for treating Type 2 Diabetes, with testing assumptions in these decision-analytical models against real-world data, further developing my expertise in epidemiological study design. I have taken courses in biostatistics, advanced epidemiology, time to event analysis, and decision-analytical modelling using R.",
  "In this position I gained expertise in epidemiological study design, data wrangling, analysis, and presentation. In dialogue with medical doctors, we defined the cohorts necessary to answer research questions regarding treatment and outcomes for patients with Diabetes Mellitus or Osteoporosis.",
  "I finished my first systematic review and investigated methodological challenges regarding cost-analysis of extremely expensive haematological medications."
)

# Create the tribble using the vectors
experience <- tibble::tribble(
  ~role, ~institution, ~dates, ~details,
  roles[1], institutions[1], dates[1], details[1],
  roles[2], institutions[2], dates[2], details[2],
  roles[3], institutions[3], dates[3], details[3]
)

# Use the cvevents function
experience |> 
  cvevents(when = "dates", what = "role", where = "institution", descr = "details")
#
#
#
#
#
#
#
#
#
#
#
#
