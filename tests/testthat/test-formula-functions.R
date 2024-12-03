
# Associative vector of element symbols and their exact masses
# based on their most abundant isotope
element_accurate_masses <- c(
  'H' = 1.007825,
  'He' = 4.002603,
  'Li' = 7.016004,
  'Be' = 9.012182,
  'B' = 11.009305,
  'C' = 12.000000,
  'N' = 14.003074,
  'O' = 15.994915,
  'F' = 18.998403,
  'Ne' = 19.992440,
  'Na' = 22.989769,
  'Mg' = 23.985042,
  'Al' = 26.981538,
  'Si' = 27.976927,
  'P' = 30.973762,
  'S' = 31.972071,
  'Cl' = 34.968853,
  'Ar' = 39.962383,
  'K' = 38.963707,
  'Ca' = 39.962591,
  'Sc' = 44.955912,
  'Ti' = 47.947947,
  'V' = 50.943962,
  'Cr' = 51.940510,
  'Mn' = 54.938045,
  'Fe' = 55.934942,
  'Co' = 58.933198,
  'Ni' = 57.935348,
  'Cu' = 62.929599,
  'Zn' = 63.929147,
  'Ga' = 68.925581,
  'Ge' = 73.921178,
  'As' = 74.921596,
  'Se' = 79.916521,
  'Br' = 78.918338,
  'Kr' = 83.911507,
  'Rb' = 84.911800,
  'Sr' = 87.905614,
  'Y' = 88.905848,
  'Zr' = 89.904703,
  'Nb' = 92.906377,
  'Mo' = 97.905407,
  'Tc' = 98.000000,
  # Technetium has no stable isotopes; value is a placeholder
  'Ru' = 101.904349,
  'Rh' = 102.905504,
  'Pd' = 105.903483,
  'Ag' = 106.905093,
  'Cd' = 113.903358,
  'In' = 114.903878,
  'Sn' = 119.902197,
  'Sb' = 120.903818,
  'Te' = 129.906223,
  'I' = 126.904468,
  'Xe' = 131.904154,
  'Cs' = 132.905451,
  'Ba' = 137.905247,
  'La' = 138.906348,
  'Ce' = 139.905443,
  'Pr' = 140.907648,
  'Nd' = 141.907719,
  'Pm' = 145.000000,
  # Promethium has no stable isotopes; value is a placeholder
  'Sm' = 151.919729,
  'Eu' = 152.921227,
  'Gd' = 157.924101,
  'Tb' = 158.925343,
  'Dy' = 163.929174,
  'Ho' = 164.930319,
  'Er' = 165.930290,
  'Tm' = 168.934211,
  'Yb' = 173.938859,
  'Lu' = 174.940770,
  'Hf' = 179.946549,
  'Ta' = 180.947994,
  'W' = 183.950930,
  'Re' = 186.955750,
  'Os' = 191.961479,
  'Ir' = 192.962921,
  'Pt' = 194.964774,
  'Au' = 196.966552,
  'Hg' = 201.970626,
  'Tl' = 204.974412,
  'Pb' = 207.976636,
  'Bi' = 208.980383,
  'Th' = 232.038050,
  'Pa' = 231.035880,
  'U' = 238.050783,
  'Np' = 237.000000,
  # Neptunium has no long-lived stable isotopes; value is a placeholder
  'Pu' = 244.000000,
  # Plutonium has no long-lived stable isotopes; value is a placeholder
  'Am' = 243.000000,
  # Americium has no long-lived stable isotopes; value is a placeholder
  'Cm' = 247.000000,
  # Curium has no long-lived stable isotopes; value is a placeholder
  'Bk' = 247.000000,
  # Berkelium has no long-lived stable isotopes; value is a placeholder
  'Cf' = 251.000000,
  # Californium has no long-lived stable isotopes; value is a placeholder
  'Es' = 252.000000,
  # Einsteinium has no long-lived stable isotopes; value is a placeholder
  'Fm' = 257.000000,
  # Fermium has no long-lived stable isotopes; value is a placeholder
  'Md' = 258.000000,
  # Mendelevium has no long-lived stable isotopes; value is a placeholder
  'No' = 259.000000,
  # Nobelium has no long-lived stable isotopes; value is a placeholder
  'Lr' = 262.000000,
  # Lawrencium has no long-lived stable isotopes; value is a placeholder
  'Rf' = 267.000000,
  # Rutherfordium has no long-lived stable isotopes; value is a placeholder
  'Db' = 270.000000,
  # Dubnium has no long-lived stable isotopes; value is a placeholder
  'Sg' = 271.000000,
  # Seaborgium has no long-lived stable isotopes; value is a placeholder
  'Bh' = 270.000000,
  # Bohrium has no long-lived stable isotopes; value is a placeholder
  'Hs' = 277.000000,
  # Hassium has no long-lived stable isotopes; value is a placeholder
  'Mt' = 276.000000,
  # Meitnerium has no long-lived stable isotopes; value is a placeholder
  'Ds' = 281.000000,
  # Darmstadtium has no long-lived stable isotopes; value is a placeholder
  'Rg' = 280.000000,
  # Roentgenium has no long-lived stable isotopes; value is a placeholder
  'Cn' = 285.000000,
  # Copernicium has no long-lived stable isotopes; value is a placeholder
  'Nh' = 284.000000,
  # Nihonium has no long-lived stable isotopes; value is a placeholder
  'Fl' = 289.000000,
  # Flerovium has no long-lived stable isotopes; value is a placeholder
  'Mc' = 288.000000,
  # Moscovium has no long-lived stable isotopes; value is a placeholder
  'Lv' = 293.000000,
  # Livermorium has no long-lived stable isotopes; value is a placeholder
  'Ts' = 294.000000,
  # Tennessine has no long-lived stable isotopes; value is a placeholder
  'Og' = 294.000000   # Oganesson has no long-lived stable isotopes; value is a placeholder
)


# Associative vector of element symbols and their average atomic masses
# Values from IUPAC as of my last training data in 2022
element_average_masses <- c(
  'H' = 1.008,
  'He' = 4.0026,
  'Li' = 6.94,
  'Be' = 9.0122,
  'B' = 10.81,
  'C' = 12.011,
  'N' = 14.007,
  'O' = 15.999,
  'F' = 19.000,
  'Ne' = 20.180,
  'Na' = 22.990,
  'Mg' = 24.305,
  'Al' = 26.982,
  'Si' = 28.086,
  'P' = 30.974,
  'S' = 32.06,
  'Cl' = 35.45,
  'Ar' = 39.948,
  'K' = 39.098,
  'Ca' = 40.078,
  'Sc' = 44.956,
  'Ti' = 47.867,
  'V' = 50.942,
  'Cr' = 52.00,
  'Mn' = 54.938,
  'Fe' = 55.845,
  'Co' = 58.933,
  'Ni' = 58.693,
  'Cu' = 63.546,
  'Zn' = 65.38,
  'Ga' = 69.723,
  'Ge' = 72.630,
  'As' = 74.922,
  'Se' = 78.971,
  'Br' = 79.904,
  'Kr' = 83.798,
  'Rb' = 85.468,
  'Sr' = 87.62,
  'Y' = 88.906,
  'Zr' = 91.224,
  'Nb' = 92.906,
  'Mo' = 95.95,
  'Tc' = 98,
  # Technetium has no stable isotopes; value is an average
  'Ru' = 101.07,
  'Rh' = 102.91,
  'Pd' = 106.42,
  'Ag' = 107.87,
  'Cd' = 112.41,
  'In' = 114.82,
  'Sn' = 118.71,
  'Sb' = 121.76,
  'Te' = 127.60,
  'I' = 126.90,
  'Xe' = 131.29,
  'Cs' = 132.91,
  'Ba' = 137.33,
  'La' = 138.91,
  'Ce' = 140.12,
  'Pr' = 140.91,
  'Nd' = 144.24,
  'Pm' = 145,
  # Promethium has no stable isotopes; value is an average
  'Sm' = 150.36,
  'Eu' = 151.98,
  'Gd' = 157.25,
  'Tb' = 158.93,
  'Dy' = 162.50,
  'Ho' = 164.93,
  'Er' = 167.26,
  'Tm' = 168.93,
  'Yb' = 173.05,
  'Lu' = 174.97,
  'Hf' = 178.49,
  'Ta' = 180.95,
  'W' = 183.84,
  'Re' = 186.21,
  'Os' = 190.23,
  'Ir' = 192.22,
  'Pt' = 195.08,
  'Au' = 196.97,
  'Hg' = 200.59,
  'Tl' = 204.38,
  'Pb' = 207.2,
  'Bi' = 208.98,
  'Th' = 232.04,
  'Pa' = 231.04,
  'U' = 238.03,
  # The following are transuranic elements, and their average masses can vary depending on the isotope and source of information.
  'Np' = 237,
  'Pu' = 244,
  'Am' = 243,
  'Cm' = 247,
  'Bk' = 247,
  'Cf' = 251,
  'Es' = 252,
  'Fm' = 257,
  'Md' = 258,
  'No' = 259,
  'Lr' = 262,
  'Rf' = 267,
  'Db' = 270,
  'Sg' = 271,
  'Bh' = 270,
  'Hs' = 277,
  'Mt' = 276,
  'Ds' = 281,
  'Rg' = 280,
  'Cn' = 285,
  'Nh' = 284,
  'Fl' = 289,
  'Mc' = 288,
  'Lv' = 293,
  'Ts' = 294,
  'Og' = 294
)

test_that("check_chemical_formula works as expected", {
  expect_true(check_chemical_formula("H2O"))
  expect_true(check_chemical_formula("C6H12O6"))
  expect_false(check_chemical_formula("H2ZO4"))
  expect_false(check_chemical_formula("ZzZz"))
})

test_that("parse_chemical_formula works as expected", {
  result <- parse_chemical_formula("H2O")
  expect_equal(result$Element, c("H", "O"))
  expect_equal(result$Count, c(2, 1))
  
  result <- parse_chemical_formula("C6H12O6")
  expect_equal(result$Element, c("C", "H", "O"))
  expect_equal(result$Count, c(6, 12, 6))
})

test_that("calculate_mass works correctly for exact and average masses", {
  # Placeholder values - you'd replace these with the correct exact/average masses
  # for the given formulas using your defined element_accurate_masses and element_average_masses vectors
  exact_mass_H2O <- (element_accurate_masses["H"] * 2) + element_accurate_masses["O"]
  average_mass_H2O <- (element_average_masses["H"] * 2) + element_average_masses["O"]
  exact_mass_H2O <-
    unname(exact_mass_H2O)
  average_mass_H2O <-
    unname(average_mass_H2O)
  expect_equal(unname(calculate_mass("H2O", which = "exact_mass")), exact_mass_H2O)
  # expect_equal(unname(calculate_mass("H2O", which = "average_mass")), average_mass_H2O)
})

