test_that("continuous color is handled in ggally_statistic", {

  # cas de base : doit marcher sans erreur
  expect_no_error(
    ggally_statistic(
      iris,
      mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length),
      text_fn = function(x, y) round(cor(x, y), 2),
      title = "Corr"
    )
  )

  # class_number : doit marcher avec différentes valeurs
  expect_no_error(
    ggally_statistic(
      iris,
      mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length),
      text_fn = function(x, y) round(cor(x, y), 2),
      title = "Corr",
      class_number = 3
    )
  )

  # discretisation_method equal
  expect_no_error(
    ggally_statistic(
      iris,
      mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length),
      text_fn = function(x, y) round(cor(x, y), 2),
      title = "Corr",
      class_number = 4,
      discretisation_method = "equal"
    )
  )

  # cas catégoriel : doit toujours marcher
  expect_no_error(
    ggally_statistic(
      iris,
      mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species),
      text_fn = function(x, y) round(cor(x, y), 2),
      title = "Corr"
    )
  )
})


test_that("continuous color is handled in deprecated ggally_cor_v1_5", {

  # cas de base
  expect_no_error(
    ggally_cor_v1_5(iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length))
  )

  # cas catégoriel
  expect_no_error(
    ggally_cor_v1_5(iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species))
  )
})
