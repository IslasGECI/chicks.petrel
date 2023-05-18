resumen <- data.frame(
  eventos = c(2, 3, 5),
  recapturas = c(1, 2, 2),
  capturas = c(3, 4, 7)
)
matriz2 <- matrix(c(1, 0, 0, 1, 1, 0), byrow = TRUE, ncol = 2)
matriz3 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0), byrow = TRUE, ncol = 3)
test_that("matriz de capturas a partir del resumen", {
  expect_equal(captures_in_matrix(resumen[1, ]), matriz2)
  expect_equal(captures_in_matrix(resumen[2, ]), matriz3)
  expect_equal(nrow(captures_in_matrix(resumen[3, ])), 7)
  expect_equal(ncol(captures_in_matrix(resumen[3, ])), 5)
})
