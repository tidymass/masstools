# get_accurate_mass <- function(formula) {
#   atomic_masses <-
#     list(
#       H = 1.00794,
#       He = 4.00260,
#       Li = 6.941,
#       Be = 9.01218,
#       B = 10.81,
#       C = 12.01,
#       N = 14.01,
#       O = 16.00,
#       F = 19.00,
#       Ne = 20.18,
#       Na = 22.99,
#       Mg = 24.31,
#       Al = 26.98,
#       Si = 28.09,
#       P = 30.97,
#       S = 32.07,
#       Cl = 35.45,
#       K = 39.10,
#       Ca = 40.08,
#       Sc = 44.96,
#       Ti = 47.87,
#       V = 50.94,
#       Cr = 52.00,
#       Mn = 54.94,
#       Fe = 55.85,
#       Ni = 58.69,
#       Co = 58.93,
#       Cu = 63.55,
#       Zn = 65.38,
#       Ga = 69.72,
#       Ge = 72.63,
#       As = 74.92,
#       Se = 78.96,
#       Br = 79.90,
#       Kr = 83.80,
#       Rb = 85.47,
#       Sr = 87.62,
#       Y = 88.91,
#       Zr = 91.22,
#       Nb = 92.91,
#       Mo = 95.94,
#       Tc = 98.00,
#       Ru = 101.07,
#       Rh = 102.91,
#       Pd = 106.42,
#       Ag = 107.87,
#       Cd = 112.41,
#       In = 114.82,
#       Sn = 118.71,
#       Sb = 121.76,
#       Te = 127.60,
#       I = 126.90,
#       Xe = 131.29,
#       Cs = 132.91,
#       Ba = 137.33,
#       La = 138.91,
#       Ce = 140.12,
#       Pr = 140.91,
#       Nd = 144.24,
#       Pm = 145.00,
#       Sm = 150.36,
#       Eu = 151.96,
#       Gd = 157.25,
#       Tb = 158.93,
#       Dy = 162.50,
#       Ho = 164.93,
#       Er = 167.26,
#       Tm = 168.93,
#       Yb = 173.05,
#       Lu = 174.97,
#       Hf = 178.49,
#       Ta = 180.95,
#       W = 183.84,
#       Re = 186.21,
#       Os = 190.23,
#       Ir = 192.22,
#       Pt = 195.08,
#       Au = 196.97,
#       Hg = 200.59,
#       Tl = 204.38,
#       Pb = 207.2,
#       Bi = 208.98,
#       Th = 232.04,
#       Pa = 231.04,
#       U = 238.03
#     )
#   # Split the formula into individual elements
#   elements <-
#     split_formula(formula = formula)
#   
#   not_elements <-
#     elements %>% 
#     dplyr::filter(!element.name %in% names(atomic_masses))
#   
#   if (nrow(not_elements) > 0){
#     stop(paste(not_elements$element.name, collapse = ", "), 
#          'are not not found in alphabet!')
#   }
#     
#     elements <- strsplit(formula, "(?=[A-Z])", perl = TRUE)[[1]]
#   # Create a dictionary of atomic masses
#   # Calculate the accurate mass of the formula
#   mass <- 0
#   for (element in elements) {
#     if (grepl("[A-Z][a-z]*\\d*", element)) {
#       # Check if the element is followed by a number
#       # Extract the element symbol and number of atoms
#       match <- regexpr("\\d+", element)
#       symbol <- substr(element, 1, match - 1)
#       count <- as.numeric(substr(element, match, nchar(element)))
#       # Add the contribution to the mass
#       mass <- mass + count * atomic_masses[[symbol]]
#     } else {
#       # If the element is not followed by a number, assume one atom
#       mass <- mass + atomic_masses
#     }
#   }
# }
