#' Convert Metabolite Identifiers Between Databases
#'
#' Convert metabolite identifiers between supported database formats using one
#' of the package's online backends.
#'
#' @param query Character. Identifier to convert, for example `"C00001"`.
#' @param from Character. Source identifier system.
#' @param to Character. Target identifier system.
#' @param top Integer. Maximum number of matches to return.
#' @param server Character. Conversion backend. One of `"cts.fiehnlab"`,
#'   `"chemspider"`, or `"openai"`.
#' @param chemspider_apikey Character. ChemSpider API key. Required when
#'   `server = "chemspider"`.
#'   **How to Obtain a ChemSpider API Key:**
#'   1. Go to the **Royal Society of Chemistry (RSC) Developer Portal**:
#'      [https://developer.rsc.org/](https://developer.rsc.org/)
#'   2. Sign in or create an account.
#'   3. Navigate to **My Account** > **API Keys**.
#'   4. Apply for access to the **ChemSpider API**.
#'   5. Once approved, generate your API key and copy it for use.
#'   6. Use this key as the `chemspider_apikey` argument in the function.
#' @param openai_apikey Character. OpenAI API key. Required when
#'   `server = "openai"`.
#'
#' @return A data frame with the input identifier in the first column and the
#'   converted identifier(s) in the second column.
#' @export
#'
#' @examples
#' \dontrun{
#' convert_metabolite_id(query = "C00001", from = "KEGG",
#' to = "Chemical Name", server = "cts.fiehnlab")
#' convert_metabolite_id(query = "BQJCRHHNABKAKU-KBQPJGBKSA-N",
#' from = "InChIKey", to = "InChI", server = "chemspider", chemspider_apikey = "your_key")
#' convert_metabolite_id(query = "C00001", from = "KEGG",
#' to = "Chemical Name", server = "openai", openai_apikey = "your_key")
#' }


convert_metabolite_id <-
  function(query = "C00001",
           from = "KEGG",
           to = "Chemical Name",
           top = 1,
           server = c("cts.fiehnlab", "chemspider", "openai"),
           chemspider_apikey = "",
           openai_apikey = "") {
    server <- match.arg(server)
    top <- as.numeric(top)
    if (is.na(top)) {
      top <- 1
    }

    ####Oliver lab
    if (server == "cts.fiehnlab") {
      result <-
        convert_metabolite_id_oliver(
          query = query,
          from = from,
          to = to,
          top = top
        )
      return(result)
    }

    ##ChemSpider
    if (server == "chemspider") {
      if (chemspider_apikey == "") {
        stop("Please provide the ChemSpider API key.")
      }
      result <-
        convert_metabolite_id_chemspider(
          query = query,
          from = from,
          to = to,
          top = top,
          chemspider_apikey = chemspider_apikey
        )
      return(result)
    }

    ####openai
    if (server == "openai") {
      if (openai_apikey == "") {
        stop("Please provide the OpenAI API key.")
      }
      result <-
        convert_metabolite_id_openai(
          query = query,
          from = from,
          to = to,
          top = top,
          openai_apikey = openai_apikey
        )
      return(result)
    }

  }



#' Convert Metabolite Identifiers with CTS FiehnLab
#'
#' Convert metabolite identifiers using the CTS FiehnLab web service.
#'
#' @inheritParams convert_metabolite_id
#' @return A two-column data frame containing the input identifier and the
#'   converted identifier(s).
#' @export
#' @examples
#' \dontrun{
#' # Convert KEGG ID to Chemical Name using FiehnLab CTS
#' convert_metabolite_id_oliver(
#'   query = "C00001",
#'   from = "KEGG",
#'   to = "Chemical Name"
#' )
#'
#' # Convert InChIKey to PubChem CID
#' convert_metabolite_id_oliver(
#'   query = "BQJCRHHNABKAKU-KBQPJGBKSA-N",
#'   from = "InChIKey",
#'   to = "PubChem CID"
#' )
#' }


convert_metabolite_id_oliver <-
  function(query = "C00001",
           from = "KEGG",
           to = "Chemical Name",
           top = 1) {
    from <-
      match.arg(from, choices = cts_fiehnlab_chemical_sources)

    to <-
      match.arg(to, choices = cts_fiehnlab_chemical_sources)

    server <- "https://cts.fiehnlab.ucdavis.edu/rest/convert"
    top <- as.numeric(top)

    if (is.na(top)) {
      top <- 1
    }

    url <- paste(server, from, to, query, sep = "/")
    url <- stringr::str_replace_all(url, " ", "%20")

    result <-
      try(expr = xml2::read_html(url, encoding = "UTF-8"),
          silent = TRUE)
    if (inherits(result, "try-error")) {
      warning(
        "Please check you query, 'from' and 'to' again.
        You can use list_metabolite_id_systems() function to
        check the databases this package support."
      )
      result <- data.frame(query, NA, stringsAsFactors = FALSE)
      colnames(result) <- c(from, to)
      return(result)
    } else {
      result <-
        try(result %>%
              rvest::html_nodes("p") %>%
              rvest::html_text(trim = TRUE) %>%
              stringr::str_split("results") %>%
              `[[`(1) %>%
              `[[`(2) %>%
              stringr::str_trim("both") %>%
              stringr::str_split('\"') %>%
              `[[`(1) %>%
              unlist() %>%
              unname() %>%
              data.frame(name = ., stringsAsFactors = FALSE) %>%
              dplyr::filter(!name %in% c("[", "]", "{", "}", "result:", ",", "]}]", ":[", "", ":[]}]")) %>%
              dplyr::filter(!stringr::str_detect(name, "fromIdentifier|searchTerm|toIdentifier")) %>%
              dplyr::pull(name))

      if (inherits(result, "try-error")) {
        result <- data.frame(query, NA, stringsAsFactors = FALSE)
        colnames(result) <- c(from, to)
        return(result)
      }

      if (length(result) == 0) {
        result <- data.frame(query, NA, stringsAsFactors = FALSE)
        colnames(result) <- c(from, to)
        return(result)
      }

      if (length(result) == 1) {
        if (result == "") {
          result <- data.frame(query, NA, stringsAsFactors = FALSE)
          colnames(result) <- c(from, to)
          return(result)
        }
      }
    }

    if (top > length(result)) {
      top <- length(result)
    }
    result <- result[seq_len(top)]
    result <-
      data.frame(query, result, stringsAsFactors = FALSE)
    colnames(result) <- c(from, to)
    return(result)
  }




#' Convert Metabolite Identifiers with ChemSpider
#'
#' Convert metabolite identifiers using the ChemSpider API.
#'
#' @inheritParams convert_metabolite_id
#' @return A two-column data frame containing the input identifier and the
#'   converted identifier(s).
#' @export
#' @examples
#' \donttest{
#' # Example 1: Convert from InChIKey to InChI using ChemSpider (API key required)
#' # Replace 'your_chemspider_api_key' with your actual API key
#' convert_metabolite_id_chemspider(
#'   query = "BQJCRHHNABKAKU-KBQPJGBKSA-N",
#'   from = "InChIKey",
#'   to = "InChI",
#'   chemspider_apikey = "your_chemspider_api_key"
#' )
#'
#' # Example 2: Convert from a metabolite ID in "csid" format
#' # Replace '12345' with a valid ChemSpider ID
#' convert_metabolite_id_chemspider(
#'   query = "12345", 
#'   from = "ChemSpider", 
#'   to = "InChI",
#'   chemspider_apikey = "your_chemspider_api_key"
#' )
#' }

convert_metabolite_id_chemspider <-
  function(query = "BQJCRHHNABKAKU-KBQPJGBKSA-N",
           from = "InChIKey",
           to = "InChI",
           top = 1,
           chemspider_apikey = "") {
    code_name_table <- chemspider_code_name_table()
    if (is.na(top)) {
      top <- 1
    }

    if (chemspider_apikey == "") {
      stop(
        "Please provide the ChemSpider API key. See the guidenace here: https://developer.rsc.org/"
      )
    }
    from <- match.arg(from, choices = code_name_table$name)
    to <- match.arg(to, choices = code_name_table$name)

    from2 <-
      code_name_table %>%
      dplyr::filter(name == from) %>%
      dplyr::pull(code)

    to2 <-
      code_name_table %>%
      dplyr::filter(name == to) %>%
      dplyr::pull(code)

    if (from2 == to2) {
      return(query)
    }

    ####if from2 is csid, then we need to use another function
    if (from2 == "csid") {
      result <- retrieve_chemspider_metabolite(
        chemspider_id = query,
        chemspider_apikey = chemspider_apikey
      )
      if (is.na(result)) {
        result <-
          data.frame(query, NA, stringsAsFactors = FALSE)
        colnames(result) <- c(from, to)
        return(result)
      } else{
        result <-
          result %>%
          dplyr::select(c("id", to2))
        colnames(result) <- c(from, to)
        return(result)
      }
    }

    headers <- c(`Content-Type` = "", `apikey` = chemspider_apikey)
    body <- list(
      "input" = query,
      "inputFormat" = from2,
      "outputFormat" = to2
    )
    body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    qurl <- "https://api.rsc.org/compounds/v1/tools/convert"
    postres <- try(httr::RETRY(
      "POST",
      url = qurl,
      httr::add_headers(.headers = headers),
      body = body,
      terminate_on = 404,
      quiet = TRUE
    ),
    silent = TRUE)

    if (inherits(postres, "try-error")) {
      result <-
        data.frame(query, NA, stringsAsFactors = FALSE)
      colnames(result) <- c(from, to)
      return(result)
    }

    if (postres$status_code == 200) {
      result <- jsonlite::fromJSON(rawToChar(postres$content))$output
      result <-
        data.frame(query, result, stringsAsFactors = FALSE)
      colnames(result) <-
        c(from, to)
      return(result)
    } else {
      result <-
        data.frame(query, NA, stringsAsFactors = FALSE)
      colnames(result) <- c(from, to)
      return(result)
    }
  }


#' Convert Metabolite Identifiers with OpenAI
#'
#' Convert metabolite identifiers using the OpenAI backend.
#'
#' @inheritParams convert_metabolite_id
#' @return A data frame with the original query and converted metabolite ID(s).
#' @export

convert_metabolite_id_openai <-
  function(query = "C00001",
           from = "KEGG",
           to = "Chemical Name",
           top = 1,
           openai_apikey = "") {
    if (openai_apikey == "") {
      stop("Please provide the OpenAI API key.")
    }

    if (is.na(top)) {
      top <- 1
    }

    from <- match.arg(from, choices = openai_chemical_sources)
    to <- match.arg(to, choices = openai_chemical_sources)

    # Define the prompt
    prompt_text <- paste0(
      "Convert the ",
      from,
      " identifier ",
      query,
      " into its/their corresponding ",
      to,
      ". ",
      "The ID to convert is: ",
      query,
      ". ",
      "Return only the top ",
      top,
      " most relevant ",
      to,
      " in a vector format, seperated by '{}' .",
      "The output should contain no any extra explanation. ",
      "The example output format for three results: 'result1{}result1{}result1'."
    )

    # OpenAI API request
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", openai_apikey)),
      httr::content_type_json(),
      body = jsonlite::toJSON(
        list(
          model = "gpt-4o-mini",
          messages = list(
            list(role = "system", content = "You are a chemical database expert."),
            list(role = "user", content = prompt_text)
          ),
          temperature = 0
        ),
        auto_unbox = TRUE
      )
    )

    # Parse the response
    response_content <- httr::content(response, as = "text", encoding = "UTF-8")
    response_json <- jsonlite::fromJSON(response_content)

    # Extract and return the converted metabolite ID(s)
    if (!is.null(response_json$choices)) {
      result <- response_json$choices$message$content %>%
        stringr::str_split("\\{\\}") %>%
        `[[`(1)
      result <-
        data.frame(query, result, stringsAsFactors = FALSE)
      colnames(result) <- c(from, to)
      return(result[seq_len(top), , drop = FALSE])
    } else {
      result <-
        data.frame(query, NA, stringsAsFactors = FALSE)
      colnames(result) <- c(from, to)
      return(result)
    }

  }

# Internal lookup table for translating ChemSpider identifier codes to
# display names used by the package.
chemspider_code_name_table <- function() {
  data.frame(
    "code" = c("csid", "inchikey", "inchi", "smiles"),
    "name" = c("ChemSpider", "InChIKey", "InChI", "SMILES"),
    stringsAsFactors = FALSE
  )
}


cts_fiehnlab_chemical_sources <- c(
  "AAA Chemistry",
  "ABBLIS Chemicals",
  "Abbott Labs",
  "ABI Chem",
  "AbMole Bioscience",
  "Acesobio",
  "Achemica",
  "Acorn PharmaTech",
  "Active Biopharma",
  "Adooq BioScience",
  "AK Scientific",
  "AKos Consulting & Solutions",
  "Alagar Yadav",
  "ALDRICH",
  "Alinda Chemical",
  "Alsachim",
  "Amadis Chemical",
  "Amatye",
  "Ambinter",
  "Ambit Biosciences",
  "AmicBase - Antimicrobial Activities",
  "Angene Chemical",
  "Angene International",
  "Anitha",
  "Annamalai Universtiy",
  "Annker Organics",
  "Anward",
  "Apeiron Synthesis",
  "ApexBio Technology",
  "Apexmol",
  "Ark Pharm",
  "Aromsyn catalogue",
  "Aronis",
  "ASINEX",
  "Aurora Fine Chemicals LLC",
  "Aurum Pharmatech LLC",
  "Avanti Polar Lipids",
  "Beijing Advanced Technology Co",
  "Bertin Pharma",
  "Bharathiar University",
  "Bharathidasan University",
  "Bhaskar Lab",
  "BIDD",
  "BIND",
  "BindingDB",
  "BioChemPartner",
  "BioCyc",
  "Biological Magnetic Resonance Data Bank (BMRB)",
  "Bioprocess Technology Lab",
  "Biosynth",
  "Broad Institute",
  "BroadPharm",
  "Burnham Center for Chemical Genomics",
  "Calbiochem",
  "Calicut University",
  "Cambridge Crystallographic Data Centre",
  "CAPOT",
  "CAS",
  "Cayman Chemical",
  "CC_PMLSC",
  "Center for Chemical Genomics",
  "ChEBI",
  "ChemBank",
  "Chembase.cn",
  "ChEMBL",
  "ChemBlock",
  "Chembo",
  "ChemBridge",
  "ChemDB",
  "ChemExper Chemical Directory",
  "ChemFrog",
  "Chemical Biology Department",
  "Chemical Name",
  "chemicalize.org by ChemAxon",
  "ChemIDplus",
  "ChemMol",
  "ChemScene",
  "ChemSpider",
  "ChemSynthesis",
  "ChemTik",
  "Chiralblock Biosciences",
  "Circadian Research",
  "CLRI (CSIR)",
  "CMLD-BU",
  "Columbia University Molecular Screening Center",
  "Comparative Toxicogenomics Database",
  "Creasyn Finechem",
  "Department of Bioinformatics",
  "Department of Biotechnology",
  "Department of Environmental Biotechnology",
  "Department of Microbiology",
  "Department of Pharmacy",
  "Department of Zoology",
  "DiscoveryGate",
  "DrugBank",
  "DTP/NCI",
  "EDASA Scientific Compounds June 2013",
  "EMD Biosciences",
  "Emory University Molecular Libraries Screening Center",
  "Enamine",
  "Ennopharm",
  "EPA DSSTox",
  "Excenen Pharmatech",
  "Exchemistry",
  "Faculty of Marine Sciences",
  "FINETECH",
  "Finley and King Labs",
  "FLUKA",
  "ForeChem",
  "Fragmenta",
  "Georganics",
  "GlaxoSmithKline (GSK)",
  "GLIDA",
  "GNF / Scripps Winzeler lab",
  "Golm Metabolome Database (GMD)",
  "GPCR-Ligand Database",
  "Hangzhou APIChem Technology",
  "Hangzhou Trylead Chemical Technology",
  "Harvard Medical School",
  "HDH Pharma",
  "Human Metabolome Database",
  "HUMGENEX",
  "IBCH RAS",
  "IBM",
  "ICCB-Longwood/NSRB Screening Facility",
  "IIT Guwahati",
  "Immunology Lab",
  "Inc.",
  "Inc. (AKSCI)",
  "InChIKey",
  "InFarmatik",
  "Inhibitor 2",
  "Insect Molecular Biology Lab",
  "Ion Channels and Transporters",
  "IS Chemical Technology",
  "Isoprenoids",
  "iThemba Pharmaceuticals",
  "IUPHAR-DB",
  "Jamson Pharmachem Technology",
  "Japan Chemical Substance Dictionary (Nikkaji)",
  "Johns Hopkins Ion Channel Center",
  "Karpagam University",
  "Kay Laboratory",
  "KEGG",
  "Kingston Chemistry",
  "KUMGM",
  "LeadScope",
  "Leiden University Medical Center",
  "LipidMAPS",
  "LLC",
  "LMSD",
  "LMU",
  "Ltd",
  "M.Jeyam and G.Shalini. Biochematics Division",
  "Marine Medicinal Plant Biotechnology Laboratory",
  "Max Planck Institute of Molecular Physiology",
  "Max Planck Institute of Molecular Plant Physiology",
  "MedChemexpress MCE",
  "MIC Scientific",
  "MICAD",
  "Milwaukee Institute for Drug Discovery",
  "MLSMR",
  "MMDB",
  "Molecular Libraries Program",
  "MOLI",
  "MolPort",
  "MP Biomedicals",
  "MTDP",
  "Nanjing Pharmaceutical Factory",
  "Nantong Baihua Bio-Pharmaceutical Co.",
  "National Cancer Institute (NCI)",
  "Nature Chemical Biology",
  "Nature Chemistry",
  "Nature Communications",
  "NCGC",
  "NIAID",
  "NIH Clinical Collection",
  "NINDS Approved Drug Screening Program",
  "NIST",
  "NIST Chemistry WebBook",
  "Nitric Oxide Research",
  "NMMLSC",
  "NMRShiftDB",
  "NovoSeek",
  "Oakwood Products",
  "ORST SMALL MOLECULE SCREENING CENTER",
  "P.Ravikumar",
  "P3 BioSystems",
  "PANACHE",
  "Paul Baures",
  "PCMD",
  "PDSP",
  "PENN-ABS",
  "PennChem-GAM",
  "PFC",
  "Phytomatics Laboratory",
  "priyadharshini sabarathinam angayarkanni murugesh palaniswamy",
  "Prous Science Drugs of the Future",
  "PubChem CID",
  "Pubchem SID",
  "Quorum sensing and Peptidomimetics Laboratory",
  "R.Sathishkumar",
  "R&D Chemicals",
  "Rangan Lab",
  "RSChem",
  "S.GURUDEEBAN",
  "SASTRA University",
  "SCRIPDB",
  "Selleck Chemicals",
  "Selleckbio",
  "SGCOxCompounds",
  "SGCStoCompounds",
  "Shanghai Institute of Organic Chemistry",
  "Shanghai Sinofluoro Scientific Company",
  "SIGMA",
  "Sigma-Aldrich",
  "SLING Consortium",
  "SMID",
  "SMILES",
  "Southern Research Institute",
  "Southern Research Specialized Biocontainment Screening Center",
  "Specialized Chemistry Center",
  "Specs",
  "Sri Venkateswara University",
  "SRMLSC",
  "Structural Genomics Consortium",
  "SureChem",
  "SYNCHEM OHG",
  "Syntechem",
  "T.RAMANATHAN & K.SATYAVANI",
  "TCI (Tokyo Chemical Industry)",
  "ten Dijke Lab",
  "Tetrahedron Scientific Inc",
  "The Scripps Research Institute Molecular Screening Center",
  "Therapeutic Targets Database",
  "Thomson Pharma",
  "TimTec",
  "Total TOSLab Building-Blocks",
  "Tox21",
  "True PharmaChem",
  "Tyger Scientific",
  "UCLA Molecular Screening Shared Resource",
  "UM-BBD",
  "UniCarbKB",
  "University of California at San Diego (UCSD)",
  "University of Kansas",
  "University of Michigan",
  "University of Pittsburgh Molecular Library Screening Center",
  "UPCMLD",
  "Vanderbilt Screening Center for GPCRs",
  "Vanderbilt Specialized Chemistry Center",
  "Vanderbilt University Medical Center",
  "VIT University",
  "Vitas-M Laboratory",
  "Watec Laboratories",
  "Watson International Ltd",
  "Web of Science",
  "xPharm",
  "Zancheng Functional Chemicals",
  "zealing chemical",
  "ZINC"
)


openai_chemical_sources <- c(
  "BioCyc",
  "CAS",
  "ChEBI",
  "ChEMBL",
  "Chemical Name",
  "DrugBank",
  "Human Metabolome Database",
  "InChIKey",
  "KEGG",
  "LipidMAPS",
  "LMSD",
  "NIST",
  "PubChem CID",
  "Pubchem SID",
  "SMILES"
)
