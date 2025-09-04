#' Convert Metabolite Identifiers Between Different Databases
#'
#' This function converts metabolite identifiers from one database format to another using different
#' online services, including CTS FiehnLab, ChemSpider, and OpenAI.
#'
#' @param query Character. The metabolite ID to convert (e.g., "C00001").
#' @param from Character. The source database (e.g., "KEGG", "InChIKey").
#' @param to Character. The target database or chemical name format (e.g., "Chemical Name", "InChI").
#' @param top Integer. The number of top matches to return.
#' @param server Character. The server to use for conversion. Options: `"cts.fiehnlab"`, `"chemspider"`, `"openai"`.
#' @param chemspider_apikey Character. API key for ChemSpider (required if using `"chemspider"` server).
#'   **How to Obtain a ChemSpider API Key:**
#'   1. Go to the **Royal Society of Chemistry (RSC) Developer Portal**:
#'      [https://developer.rsc.org/](https://developer.rsc.org/)
#'   2. Sign in or create an account.
#'   3. Navigate to **My Account** > **API Keys**.
#'   4. Apply for access to the **ChemSpider API**.
#'   5. Once approved, generate your API key and copy it for use.
#'   6. Use this key as the `chemspider_apikey` argument in the function.
#' @param openai_apikey Character. API key for OpenAI (required if using `"openai"` server).
#'
#' @return A data frame with the original query and converted metabolite ID(s).
#' @export
#'
#' @examples
#' convert_metabolite_id(query = "C00001", from = "KEGG",
#' to = "Chemical Name", server = "cts.fiehnlab")
#' convert_metabolite_id(query = "BQJCRHHNABKAKU-KBQPJGBKSA-N",
#' from = "InChIKey", to = "InChI", server = "chemspider", chemspider_apikey = "your_key")
#' convert_metabolite_id(query = "C00001", from = "KEGG",
#' to = "Chemical Name", server = "openai", openai_apikey = "your_key")


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



#' Convert Metabolite Identifiers Using CTS FiehnLab
#'
#' Converts metabolite IDs from one database to another using the CTS FiehnLab online service.
#'
#' @inheritParams convert_metabolite_id
#' @return A data frame with the original query and converted metabolite ID(s).
#' @export
#' @examples
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
    if (is(result, class2 = "try-error")) {
      warning(
        "Please check you query, 'from' and 'to' again.
        You can use request_metabolite_id_systems() function to
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

      if (is(result, class2 = "try-error")) {
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




#' Convert Metabolite Identifiers Using ChemSpider
#'
#' Converts metabolite IDs from one format to another using the ChemSpider API.
#'
#' @inheritParams convert_metabolite_id
#' @return A data frame with the original query and converted metabolite ID(s).
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
#' # Assuming '12345' is a valid ChemSpider ID
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
    if (is.na(top)) {
      top <- 1
    }

    if (chemspider_apikey == "") {
      stop(
        "Please provide the ChemSpider API key. See the guidenace here: https://developer.rsc.org/"
      )
    }
    from <- match.arg(from, choices = chemspider_code_name_table$name)
    to <- match.arg(to, choices = chemspider_code_name_table$name)

    from2 <-
      chemspider_code_name_table %>%
      dplyr::filter(name == from) %>%
      dplyr::pull(code)

    to2 <-
      chemspider_code_name_table %>%
      dplyr::filter(name == to) %>%
      dplyr::pull(code)

    if (from2 == to2) {
      return(query)
    }

    ####if from2 is csid, then we need to use another function
    if (from2 == "csid") {
      result <- request_chemspider_metabolite(chemspider_id = query, chemspider_apikey = chemspider_apikey)
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

    if (is(postres, class2 = "try-error")) {
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


#' Convert Metabolite Identifiers Using OpenAI API
#'
#' Converts metabolite IDs from one database format to another using OpenAI's language models.
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


#' Retrieve Metabolite Information from ChemSpider
#'
#' This function queries the **ChemSpider API** to retrieve detailed metabolite information
#' for a given ChemSpider ID, including molecular formula, mass, InChI, InChIKey, and other properties.
#'
#' @param chemspider_id Numeric or character. The ChemSpider compound ID(s) to query.
#' @param chemspider_apikey Character. The API key required to access the ChemSpider database.
#'   **How to Obtain a ChemSpider API Key:**
#'   1. Visit the **Royal Society of Chemistry (RSC) Developer Portal**:
#'      [https://developer.rsc.org/](https://developer.rsc.org/)
#'   2. Sign in or create an account.
#'   3. Navigate to **My Account** > **API Keys**.
#'   4. Apply for access to the **ChemSpider API**.
#'   5. Once approved, generate your API key and copy it for use.
#'   6. Use this key as the `chemspider_apikey` argument in this function.
#'
#' @return A data frame containing metabolite properties retrieved from ChemSpider.
#'   If the request fails, the function returns `NA`.
#'
#'   The returned data frame includes the following columns:
#'   - **id**: ChemSpider compound ID.
#'   - **SMILES**: Simplified molecular-input line-entry system (SMILES) notation.
#'   - **Formula**: Molecular formula.
#'   - **InChI**: IUPAC International Chemical Identifier.
#'   - **InChIKey**: Standard InChIKey.
#'   - **StdInChI**: Standardized InChI identifier.
#'   - **StdInChIKey**: Standardized InChIKey.
#'   - **AverageMass**: Average molecular mass.
#'   - **MolecularWeight**: Exact molecular weight.
#'   - **MonoisotopicMass**: Monoisotopic mass.
#'   - **NominalMass**: Nominal mass.
#'   - **CommonName**: Commonly used name.
#'   - **ReferenceCount**: Number of references in ChemSpider.
#'   - **DataSourceCount**: Number of linked data sources.
#'   - **PubMedCount**: Number of associated PubMed articles.
#'   - **RSCCount**: Number of references in the Royal Society of Chemistry database.
#'
#' @export
#'
#' @examples
#' # Retrieve metabolite information for a specific ChemSpider ID
#' request_chemspider_metabolite(chemspider_id = 12345, chemspider_apikey = "your_key")
#'
#' # Retrieve information for multiple IDs
#' request_chemspider_metabolite(chemspider_id = c(12345, 67890), chemspider_apikey = "your_key")


request_chemspider_metabolite <-
  function(chemspider_id, chemspider_apikey = "") {
    if (chemspider_apikey == "") {
      stop("Please provide the ChemSpider API key.")
    }

    fields <- c(
      "SMILES",
      "Formula",
      "InChI",
      "InChIKey",
      "StdInChI",
      "StdInChIKey",
      "AverageMass",
      "MolecularWeight",
      "MonoisotopicMass",
      "NominalMass",
      "CommonName",
      "ReferenceCount",
      "DataSourceCount",
      "PubMedCount",
      "RSCCount"
    )
    chemspider_id <- as.numeric(chemspider_id)
    headers <- c("Content-Type" = "", "apikey" = chemspider_apikey)
    body <- list("recordIds" = chemspider_id[!is.na(chemspider_id)], "fields" = fields)
    body <- jsonlite::toJSON(body)

    qurl <- "https://api.rsc.org/compounds/v1/records/batch"

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
      return(NA)
    }

    if (postres$status_code == 200) {
      res <- jsonlite::fromJSON(rawToChar(postres$content))$records
      if (length(res) == 0) {
        return(NA)
      }
      out <- data.frame(id = chemspider_id)
      out <- dplyr::left_join(out, res, by = "id")
      out$id <- as.character(out$id)
      return(out)
    } else {
      return(NA)
    }
  }


#' Retrieve Supported Metabolite Identifier Conversion Systems
#'
#' This function returns the available metabolite identifier conversion systems supported by
#' different online services, including **CTS FiehnLab**, **ChemSpider**, and **OpenAI**.
#' It provides a list of identifier formats that can be used for conversion.
#'
#' @param server Character. The server to retrieve supported identifier formats from.
#'   Available options:
#'   - `"cts.fiehnlab"`: The **Chemical Translation Service (CTS) by FiehnLab** at UC Davis.
#'   - `"chemspider"`: The **ChemSpider API** provided by the Royal Society of Chemistry.
#'   - `"openai"`: Uses OpenAI's language models for metabolite ID conversions.
#' @param source_format Character. Specifies whether to use a **local** or **online** source
#'   for fetching the available databases. Options:
#'   - `"local"`: Uses locally stored information about supported databases.
#'   - `"online"`: Fetches real-time data from the respective API endpoints.
#'
#' @return A data frame or vector containing the supported **From-To** identifier conversions
#'   for the selected `server`.
#'   - If `server = "cts.fiehnlab"`, returns a vector of available **from** and **to** databases.
#'   - If `server = "chemspider"`, returns a data frame listing valid identifier conversions.
#'   - If `server = "openai"`, returns a predefined set of supported sources.
#'
#' @details
#' - **CTS FiehnLab**: The function queries `http://cts.fiehnlab.ucdavis.edu/rest/fromValues`
#'   to retrieve the list of supported database formats.
#' - **ChemSpider**: Retrieves a predefined set of identifier conversions.
#' - **OpenAI**: Returns a list of supported metabolite ID formats available through OpenAI models.
#'
#' **ChemSpider API Key Requirement**:
#' If using the `"chemspider"` server, an API key is required for querying ChemSpider’s services.
#'
#' **How to Obtain a ChemSpider API Key:**
#' 1. Visit the **Royal Society of Chemistry (RSC) Developer Portal**:
#'    [https://developer.rsc.org/](https://developer.rsc.org/)
#' 2. Sign in or create an account.
#' 3. Navigate to **My Account** > **API Keys**.
#' 4. Apply for access to the **ChemSpider API**.
#' 5. Once approved, generate your API key and copy it for use.
#'
#' @export
#'
#' @examples
#' # Get supported conversion systems for CTS FiehnLab (local)
#' request_metabolite_id_systems(server = "cts.fiehnlab", source_format = "local")


request_metabolite_id_systems <-
  function(server = c("cts.fiehnlab", "chemspider", "openai"),
           source_format = c("local", "online")) {
    server <- match.arg(server)
    source_format <- match.arg(source_format)
    if (server == "cts.fiehnlab") {
      server <- "https://cts.fiehnlab.ucdavis.edu/service/convert"
    }

    if (server == "chemspider") {
      server <- "https://api.rsc.org/compounds/v1/tools/convert"
    }

    if (server == "https://cts.fiehnlab.ucdavis.edu/service/convert") {
      if (source_format == "local") {
        return(cts_fiehnlab_chemical_sources)
      }
      chemical_database_from <-
        xml2::read_html("http://cts.fiehnlab.ucdavis.edu/rest/fromValues")
      chemical_database_to <-
        xml2::read_html("http://cts.fiehnlab.ucdavis.edu/rest/fromValues")

      chemical_database_from <-
        chemical_database_from %>%
        rvest::html_nodes("p") %>%
        rvest::html_text(TRUE) %>%
        stringr::str_split(pattern = ",") %>%
        `[[`(1) %>%
        lapply(function(x) {
          stringr::str_trim(x, "both") %>%
            stringr::str_replace_all(pattern = '\"', replacement = "") %>%
            stringr::str_replace_all(pattern = ",|\\[", replacement = "")
        }) %>%
        unlist() %>%
        unname()

      chemical_database_to <-
        chemical_database_to %>%
        rvest::html_nodes("p") %>%
        rvest::html_text(TRUE) %>%
        stringr::str_split(pattern = ",") %>%
        `[[`(1) %>%
        lapply(function(x) {
          stringr::str_trim(x, "both") %>%
            stringr::str_replace_all(pattern = '\"', replacement = "") %>%
            stringr::str_replace_all(pattern = ",|\\[", replacement = "")
        }) %>%
        unlist() %>%
        unname()

      chemical_database_from <- sort(chemical_database_from)
      chemical_database_to <- sort(chemical_database_to)

      chemical_database_from
    }

    if (server == "https://api.rsc.org/compounds/v1/tools/convert") {
      result <- c(
        "inchikey_csid",
        "inchikey_inchi",
        "inchi_csid",
        "inchi_inchikey",
        "inchi_smiles",
        "smiles_inchi"
      )
      result <-
        stringr::str_split(result, "_") %>%
        dplyr::bind_cols() %>%
        t() %>%
        tibble::as_tibble(.name_repair = "minimal")
      colnames(result) <- c("From", "To")
      message(
        crayon::green(
          nrow(result),
          "are supported in server https://api.rsc.org/compounds/v1/tools/convert."
        )
      )

      result <-
        result %>%
        dplyr::left_join(chemspider_code_name_table, by = c("From" = "code")) %>%
        dplyr::left_join(chemspider_code_name_table, by = c("To" = "code")) %>%
        dplyr::select(-c("From", "To")) %>%
        dplyr::rename("From" = "name.x", "To" = "name.y")

      return(result)
    }

    if (server %in% c("openai")) {
      return(openai_chemical_sources)
    }

  }


chemspider_code_name_table <-
  data.frame(
    "code" = c("csid", "inchikey", "inchi", "smiles"),
    "name" = c("ChemSpider", "InChIKey", "InChI", "SMILES"),
    stringsAsFactors = FALSE
  )


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
