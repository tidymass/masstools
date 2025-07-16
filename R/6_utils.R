# msg <- function(..., startup = FALSE) {
#   if (startup) {
#     if (!isTRUE(getOption("masstools.quiet"))) {
#       packageStartupMessage(text_col(...))
#     }
#   } else {
#     message(text_col(...))
#   }
# }

# text_col <- function(x) {
#   # If RStudio not available, messages already printed in black
#   if (!rstudioapi::isAvailable()) {
#     return(x)
#   }
#
#   if (!rstudioapi::hasFun("getThemeInfo")) {
#     return(x)
#   }
#
#   theme <- rstudioapi::getThemeInfo()
#
#   if (isTRUE(theme$dark)) {
#     crayon::white(x)
#   } else {
#     crayon::black(x)
#   }
# }

#' List all packages in the masstools
#'
#' @param include_self Include masstools in the list?
#' @export
#' @return masstools packages
#' @examples
#' masstools_packages()
masstools_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("masstools")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "masstools")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}


























# # NOTE: keep in sync with install-patRoon version
# findPWizPath <- function() {
#   # try to find ProteoWizard
#   # order: options --> win registry --> PATH
#   # the PATH is searched last because
#    OpenMS might have added its own old version.
#
#   path <- getOption("patRoon.path.pwiz")
#   if (!is.null(path) & nzchar(path))
#     return(path)
#
#   if (Sys.info()[["sysname"]] == "Windows")
#   {
#     # Inspired by scan_registry_for_rtools() from pkgload
#     key <- "Directory\\shell\\Open with SeeMS\\command"
#     reg <-
#       tryCatch(
#         utils::readRegistry(key, "HCR"),
#         error = function(e)
#           NULL
#       )
#
#     # not sure if this might occur
#     if (is.null(reg))
#       reg <-
#       tryCatch(
#         utils::readRegistry(key, "HLM"),
#         error = function(e)
#           NULL
#       )
#
#     if (!is.null(reg))
#     {
#       path <-
#         tryCatch(
#           dirname(sub("\"([^\"]*)\".*", "\\1", reg[[1]])),
#           error = function(e)
#             NULL
#         )
#       if (!is.null(path) &
#           file.exists(file.path(path, "msconvert.exe")))
#         # extra check: see if msconvert is there
#         return(path)
#     }
#   }
#
#   # check PATH
#   msc <-
#     if (Sys.info()[["sysname"]] == "Windows")
#       "msconvert.exe"
#   else
#     "msconvert"
#   path <- dirname(Sys.which(msc))
#   if (nzchar(path))
#     return(path)
#
#   return(NULL)
# }



#' @title show_progresser
#' @description show_progresser
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param index index for loop
#' @param progresser progresser
#' @return A data.frame
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when everything select filter
#' @importFrom purrr map map2 walk
#' @importFrom crayon green
#' @export
#' @examples
#' show_progresser()

show_progresser <-
  function(index = seq_len(1000),
           progresser = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    idx <-
      seq(
        from = 1,
        to = max(index),
        length.out = length(progresser)
      ) %>%
      round()

    data.frame(idx = idx, progresser = paste0(progresser, "%"))
  }



#' @title install_fastgit
#' @description install packages from fastgit. Credit to Shixiang Wang
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param pkg pkg name from github, gitlab or gitee, "name/repo" format
#' @param from gitlab, github or gitee.
#' @param ... Other parameters for install_git
#' @return `NULL`. This function is called for its side effect of installing packages from specified Git repository mirrors.
#' @export
#' @examples
#' \dontrun{
#' # Install a package from GitHub using the fastgit mirror
#' install_fastgit(pkg = "tidyverse/ggplot2", from = "github")
#'
#' # Install a package from Gitee
#' install_fastgit(pkg = "yourusername/yourpkg", from = "gitee")
#' }


install_fastgit <-
  function(pkg,
           from = c("gitee", "gitlab", "github"),
           ...) {
    from <- match.arg(from)

    if (from == "gitee") {
      if (!grepl("/", pkg)) {
        stop("Expected 'name/repo' format for 'pkg'.")
      }
      remotes::install_git(paste0("https://gitee.com/", pkg), ...)
    } else {
      if (any(grepl(":", pkg))) {
        remotes::install_git(pkg, ...)
      } else {
        if (any(grepl("/", pkg))) {
          tryCatch(
            remotes::install_git(paste0("https://hub.fastgit.org/", pkg)),
            error = function(e) {
              message("Installation failed using the GitHub mirror; falling back to the official GitHub.")
              remotes::install_github(pkg)
            }
          )
        }
      }
    }
  }





# Generate a data frame of chemical elements
elements <- c(
  "H",
  "He",
  "Li",
  "Be",
  "B",
  "C",
  "N",
  "O",
  "F",
  "Ne",
  "Na",
  "Mg",
  "Al",
  "Si",
  "P",
  "S",
  "Cl",
  "Ar",
  "K",
  "Ca",
  "Sc",
  "Ti",
  "V",
  "Cr",
  "Mn",
  "Fe",
  "Co",
  "Ni",
  "Cu",
  "Zn",
  "Ga",
  "Ge",
  "As",
  "Se",
  "Br",
  "Kr",
  "Rb",
  "Sr",
  "Y",
  "Zr",
  "Nb",
  "Mo",
  "Tc",
  "Ru",
  "Rh",
  "Pd",
  "Ag",
  "Cd",
  "In",
  "Sn",
  "Sb",
  "Te",
  "I",
  "Xe",
  "Cs",
  "Ba",
  "La",
  "Ce",
  "Pr",
  "Nd",
  "Pm",
  "Sm",
  "Eu",
  "Gd",
  "Tb",
  "Dy",
  "Ho",
  "Er",
  "Tm",
  "Yb",
  "Lu",
  "Hf",
  "Ta",
  "W",
  "Re",
  "Os",
  "Ir",
  "Pt",
  "Au",
  "Hg",
  "Tl",
  "Pb",
  "Bi",
  "Th",
  "Pa",
  "U",
  "Np",
  "Pu",
  "Am",
  "Cm",
  "Bk",
  "Cf",
  "Es",
  "Fm",
  "Md",
  "No",
  "Lr",
  "Rf",
  "Db",
  "Sg",
  "Bh",
  "Hs",
  "Mt",
  "Ds",
  "Rg",
  "Cn",
  "Nh",
  "Fl",
  "Mc",
  "Lv",
  "Ts",
  "Og"
)

names <- c(
  "Hydrogen",
  "Helium",
  "Lithium",
  "Beryllium",
  "Boron",
  "Carbon",
  "Nitrogen",
  "Oxygen",
  "Fluorine",
  "Neon",
  "Sodium",
  "Magnesium",
  "Aluminum",
  "Silicon",
  "Phosphorus",
  "Sulfur",
  "Chlorine",
  "Argon",
  "Potassium",
  "Calcium",
  "Scandium",
  "Titanium",
  "Vanadium",
  "Chromium",
  "Manganese",
  "Iron",
  "Cobalt",
  "Nickel",
  "Copper",
  "Zinc",
  "Gallium",
  "Germanium",
  "Arsenic",
  "Selenium",
  "Bromine",
  "Krypton",
  "Rubidium",
  "Strontium",
  "Yttrium",
  "Zirconium",
  "Niobium",
  "Molybdenum",
  "Technetium",
  "Ruthenium",
  "Rhodium",
  "Palladium",
  "Silver",
  "Cadmium",
  "Indium",
  "Tin",
  "Antimony",
  "Tellurium",
  "Iodine",
  "Xenon",
  "Cesium",
  "Barium",
  "Lanthanum",
  "Cerium",
  "Praseodymium",
  "Neodymium",
  "Promethium",
  "Samarium",
  "Europium",
  "Gadolinium",
  "Terbium",
  "Dysprosium",
  "Holmium",
  "Erbium",
  "Thulium",
  "Ytterbium",
  "Lutetium",
  "Hafnium",
  "Tantalum",
  "Tungsten",
  "Rhenium",
  "Osmium",
  "Iridium",
  "Platinum",
  "Gold",
  "Mercury",
  "Thallium",
  "Lead",
  "Bismuth",
  "Thorium",
  "Protactinium",
  "Uranium",
  "Neptunium",
  "Plutonium",
  "Americium",
  "Curium",
  "Berkelium",
  "Californium",
  "Einsteinium",
  "Fermium",
  "Mendelevium",
  "Nobelium",
  "Lawrencium",
  "Rutherfordium",
  "Dubnium",
  "Seaborgium",
  "Bohrium",
  "Hassium",
  "Meitnerium",
  "Darmstadtium",
  "Roentgenium",
  "Copernicium",
  "Nihonium",
  "Flerovium",
  "Moscovium",
  "Livermorium",
  "Tennessine",
  "Oganesson"
)

accurate_mass <- c(
  1.007825,
  4.002603,
  7.016004,
  9.012182,
  11.009305,
  12.000000,
  14.003074,
  15.994915,
  18.998403,
  20.1797,
  22.989769,
  24.305,
  26.981538,
  28.0855,
  30.973762,
  32.065,
  35.453,
  39.948,
  39.0983,
  40.078,
  44.955912,
  47.867,
  50.9415,
  51.9961,
  54.938045,
  55.845,
  58.933195,
  58.6934,
  63.546,
  65.38,
  69.723,
  72.63,
  74.9216,
  78.96,
  79.904,
  83.798,
  85.4678,
  87.62,
  88.90585,
  91.224,
  92.90638,
  95.95,
  98,
  101.07,
  102.9055,
  106.42,
  107.8682,
  112.411,
  114.818,
  118.71,
  121.76,
  127.6,
  126.90447,
  131.293,
  132.9054519,
  137.327,
  138.90547,
  140.116,
  140.90766,
  144.242,
  145,
  150.36,
  151.964,
  157.25,
  158.92535,
  162.5,
  164.93033,
  167.259,
  168.93422,
  173.04,
  174.9668,
  178.49,
  180.94788,
  183.84,
  186.207,
  190.23,
  192.217,
  195.084,
  196.966569,
  200.592,
  204.3833,
  207.2,
  208.9804,
  232.0377,
  231.03588,
  238.02891,
  237.0482,
  244.0642,
  243.0614,
  247.0703,
  247.0703,
  251.0796,
  252.083,
  257.0951,
  258.0984,
  259.101,
  262.110,
  267.1218,
  270.1335,
  271.1343,
  270.1353,
  277.1519,
  276.1555,
  281.1621,
  282.1664,
  285.1741,
  286.1793,
  289.1904,
  290.1981,
  293.2045,
  294.214,
  294.214
)

average_mass <- c(
  1.008,
  4.003,
  6.941,
  9.012,
  10.81,
  12.01,
  14.01,
  16.00,
  19.00,
  20.18,
  22.99,
  24.31,
  26.98,
  28.09,
  30.97,
  32.07,
  35.45,
  39.95,
  39.10,
  40.08,
  44.96,
  47.87,
  50.94,
  52.00,
  54.94,
  55.85,
  58.93,
  58.69,
  63.55,
  65.38,
  69.72,
  72.63,
  74.92,
  78.96,
  79.90,
  83.80,
  85.47,
  87.62,
  88.91,
  91.22,
  92.91,
  95.95,
  98,
  101.07,
  102.91,
  106.42,
  107.87,
  112.41,
  114.82,
  118.71,
  121.76,
  127.60,
  126.90,
  131.29,
  132.91,
  137.33,
  138.91,
  140.12,
  140.91,
  144.24,
  145,
  150.36,
  151.96,
  157.25,
  158.93,
  162.50,
  164.93,
  167.26,
  168.93,
  173.04,
  174.97,
  178.49,
  180.95,
  183.84,
  186.21,
  190.23,
  192.22,
  195.08,
  196.97,
  200.59,
  204.38,
  207.2,
  208.98,
  232.04,
  231.04,
  238.03,
  237.05,
  244.06,
  243.06,
  247.07,
  247.07,
  251.08,
  252.08,
  257.10,
  258.10,
  259.10,
  262.11,
  267.12,
  270.13,
  271.13,
  270.14,
  277.15,
  276.16,
  281.16,
  282.17,
  285.17,
  286.18,
  289.19,
  290.20,
  293.20,
  294.21,
  294.21
)

chemical_elements_information <-
  data.frame(
    element = elements,
    name = names,
    accurate_mass = accurate_mass,
    average_mass = average_mass
  )



# Generate a data frame of possible adducts
adducts <- c(
  "(M+H)+",
  "(M+H-H2O)+",
  "(M+H-2H2O)+",
  "(M+NH4)+",
  "(M+Na)+",
  "(M-H+2Na)+",
  "(M-2H+3Na)+",
  "(M+K)+",
  "(M-H+2K)+",
  "(M-2H+3K)+",
  "(M+CH3CN+H)+",
  "(M+CH3CN+Na)+",
  "(2M+H)+",
  "(2M+NH4)+",
  "(2M+Na)+",
  "(2M+K)+",
  "(M+HCOO+2H)+",
  "(M-H)-",
  "(M-H2O-H)-",
  "(M+Na-2H)-",
  "(M+K-2H)-",
  "(M+NH4-2H)-",
  "(2M-H)-",
  "(M+F)-",
  "(M+CH3COO+2H)+",
  "(M+CH3COO)-"
)

mz_values <- c(
  1.0073,-18.0106,-36.0212,
  18.0338,
  22.9898,
  22.9898,
  33.0155,
  38.9637,
  38.9637,
  55.9393,
  41.0265,
  64.0158,
  1.0073,
  18.0338,
  22.9898,
  38.9637,
  45.0174,-1.0073,-18.0106,-21.9819,-37.9559,-17.0265,-1.0073,-18.9984,
  59.0139,-59.0139
)

adduct_table <- data.frame(adduct = adducts, mz = mz_values)
