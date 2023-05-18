describe("Construct m-array from matrix history", {
  summary <- data.frame(
    eventos = c(4, 5),
    recapturas = c(5, 10),
    capturas = c(20, 50)
  )
  matrix_history <- matrix(
    c(
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1,
      1, 1, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      1, 0, 0, 1,
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 1,
      0, 0, 0, 1
    ),
    ncol = 4, byrow = T
  )
  matrix_history_8X6 <- matrix(
    c(
      1, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 1,
      1, 1, 0, 0, 0, 1, 0, 1,
      0, 0, 1, 0, 1, 0, 0, 1,
      1, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 1, 1, 0, 0, 0, 1
    ),
    ncol = 8, byrow = T
  )
  m_values <- c(1, 0, 1, 2, 0, 0, 0, 4, 0, 0, 1, 2)
  m_array <- matrix(
    m_values,
    ncol = 4, byrow = T
  )
  expected_values <- as.vector(m_array)
  it("build m array", {
    obtained_m_array <- get_m_array(matrix_history)
    obtained_m_array_values <- as.vector(obtained_m_array)
    expect_equal(obtained_m_array_values, expected_values)
    obtained_col_names <- colnames(obtained_m_array)
    expected_col_names <- c("Y2", "Y3", "Y4", "never")
    expect_equal(obtained_col_names, expected_col_names)
    expected_row_names <- c("Y1", "Y2", "Y3")
    obtained_row_names <- rownames(obtained_m_array)
    expect_equal(obtained_row_names, expected_row_names)
  })
  it("get N_i from matrix history", {
    obtained_Ns <- get_Ns_from_matrix_history(matrix_history)
    Ms <- c((4 + 1) * 0 / (2 + 1) + 0, (4 + 1) * 1 / (0 + 1) + 1, (3 + 1) * 1 / (1 + 1) + 0, 2)
    alphas <- c((0 + 1) / (4 + 1), (1 + 1) / (4 + 1), (0 + 1) / (3 + 1), (2 + 1) / (4 + 1))
    expected_Ns <- Ms / alphas
    expect_equal(obtained_Ns, expected_Ns)
  })
  it("get M_i from matrix history", {
    obtained_Ms <- get_Ms_from_matrix_history(matrix_history)
    expected_Ms <- c((4 + 1) * 0 / (2 + 1) + 0, (4 + 1) * 1 / (0 + 1) + 1, (3 + 1) * 1 / (1 + 1) + 0, 2)
    expect_equal(obtained_Ms, expected_Ms)
  })
  it("get alpha_i from matrix history", {
    obtained_alphas <- get_alphas_from_matrix_history(matrix_history)
    expected_alphas <- c((0 + 1) / (4 + 1), (1 + 1) / (4 + 1), (0 + 1) / (3 + 1), (2 + 1) / (4 + 1))
    expect_equal(obtained_alphas, expected_alphas)
  })
  it("get n_i from matrix history", {
    obtained_u <- get_ns_from_matrix_history(matrix_history)
    expected_u <- c(4, 4, 3, 4)
    expect_equal(obtained_u, expected_u)
    obtained_u <- get_ns_from_matrix_history(matrix_history_8X6)
    expected_u <- c(3, 1, 3, 1, 1, 3, 0, 4)
    expect_equal(obtained_u, expected_u)
  })

  it("get m_i from matrix history", {
    obtained_m <- get_ms_from_matrix_history(matrix_history)
    expected_m4 <- 2
    expect_equal(obtained_m[[4]], expected_m4)
    expected_m <- c(0, 1, 0, 2)
    expect_equal(obtained_m, expected_m)
  })
  it("get Z_i from m array", {
    index <- 2
    obtained_z2 <- get_z_i_from_m_array(m_array, index)
    expected_z2 <- 1
    expect_equal(obtained_z2, expected_z2)
  })
  it("get Z_i from matrix history", {
    index <- 2
    obtained_z2 <- get_z_i_from_matrix_history(matrix_history, index)
    expected_z2 <- 1
    expect_equal(obtained_z2, expected_z2)
    matrix_history_2 <- matrix(
      c(
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
        1, 1, 0, 0,
        0, 1, 0, 1,
        0, 0, 1, 0,
        1, 0, 0, 1,
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 1,
        0, 0, 0, 1
      ),
      ncol = 4, byrow = T
    )
    index <- 3
    obtained_z3 <- get_z_i_from_matrix_history(matrix_history_2, index)
    expected_z3 <- 2
    expect_equal(obtained_z3, expected_z3)
    index <- 3
    obtained_z3 <- get_z_i_from_matrix_history(matrix_history_8X6, index)
    expected_z3 <- 3

    expect_equal(obtained_z3, expected_z3)
    obtained_z <- get_z_from_matrix_history(matrix_history_2)
    expected_z <- c(0, 1, 2, 0)
    expect_equal(obtained_z, expected_z)

    obtained_z <- get_z_from_matrix_history(matrix_history)
    expected_z <- c(0, 1, 1, 0)
    expect_equal(obtained_z, expected_z)

    obtained_z <- get_z_from_matrix_history(matrix_history_8X6)
    expected_z <- c(0, 2, 3, 5, 5, 3, 4, 0)
    expect_equal(obtained_z, expected_z)
  })
  it("get R_i from matrix history", {
    obtained_rs <- get_r_i_from_matrix_history(matrix_history_8X6)
    expected_r3 <- 3
    expect_equal(obtained_rs[3], expected_r3)
    obtained_rs <- get_r_i_from_matrix_history(matrix_history)
    expected_rs <- c(2, 0, 1, 0)
    expect_equal(obtained_rs, expected_rs)
  })
})
