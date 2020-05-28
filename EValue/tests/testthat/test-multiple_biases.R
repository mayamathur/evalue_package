#### errors and messages ####

test_that(
  "biases that are well specified don't generate messages or errors", {
    expect_silent(multi_bias(confounding()))
    expect_silent(multi_bias(selection("general")))
    expect_silent(multi_bias(selection("selected")))
    expect_silent(multi_bias(selection("general", "increased risk")))
    expect_silent(multi_bias(selection("general", "increased risk", "S = U")))
    expect_silent(multi_bias(misclassification("outcome")))
    expect_silent(multi_bias(misclassification("exposure", rare_outcome = TRUE)))
    expect_silent(multi_bias(misclassification("exposure", rare_exposure = TRUE, rare_outcome = TRUE)))
  }
)

test_that(
  "biases that are correct but under- or over-specified generate message", {
    expect_message(multi_bias(confounding("blah")))
    expect_message(multi_bias(selection()))
    expect_message(multi_bias(selection("increased risk")))
    expect_message(multi_bias(misclassification("outcome", rare_outcome = TRUE)))
    expect_message(multi_bias(misclassification("outcome", rare_exposure = TRUE)))
  }
)

test_that(
  "biases that are incorrect generate errors", {
    expect_error(selection("general", "selected"))
    expect_error(selection("general", "increased risk", "decreased risk"))
    expect_error(selection("general", "increased risk", "decreased risk", "S = U") )
    expect_error(selection("selected", "increased risk", "S = U"))
    expect_error(misclassification())
    expect_error(misclassification("exposure"))
    expect_error(misclassification("outcome", "exposure"))
    expect_error(misclassification("exposure", rare_exposure = TRUE))
  }
)


#### attributes ####

mb01 <- multi_bias(confounding())
mb02 <- multi_bias(selection("general"))
mb03 <- multi_bias(selection("selected"))
mb04 <- multi_bias(selection("general", "increased risk"))
mb05 <- multi_bias(selection("general", "decreased risk", "S = U"))
mb06 <- multi_bias(selection("general", "S = U"))
mb07 <- multi_bias(misclassification("outcome")) 
mb08 <- multi_bias(misclassification("exposure", rare_outcome = TRUE)) 
mb09 <- multi_bias(misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE))
mb10 <- multi_bias(confounding(), selection("general"))
mb11 <- multi_bias(confounding(), selection("general"), misclassification("outcome")) 
mb12 <- multi_bias(confounding(), misclassification("outcome"), selection("general")) 
mb13 <- multi_bias(confounding(), selection("general", "increased risk"))
mb14 <- multi_bias(confounding(), selection("general", "increased risk", "S = U"))
mb15 <- multi_bias(confounding(), selection("general", "increased risk"), misclassification("outcome")) 
mb16 <- multi_bias(confounding(), misclassification("outcome"), selection("general", "increased risk"))
mb17 <- multi_bias(confounding(), selection("general", "increased risk", "S = U"), misclassification("exposure", rare_outcome = TRUE))
mb18 <- multi_bias(confounding(), selection("selected"), misclassification("exposure", rare_outcome = TRUE))
mb19 <- multi_bias(confounding(), misclassification("exposure", rare_outcome = TRUE), selection("selected"))
mb20 <- multi_bias(selection("selected"), misclassification("exposure", rare_outcome = TRUE))
mb21 <- multi_bias(misclassification("exposure", rare_outcome = TRUE), selection("selected"))
mb22 <- multi_bias(confounding(), selection("selected"), misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE)) 
mb23 <- multi_bias(confounding(), misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE), selection("selected"))
mb24 <- multi_bias(selection("selected"), misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE))
mb25 <- multi_bias(misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE), selection("selected"))
mb26 <- multi_bias(confounding(), selection("selected"))
mb27 <- multi_bias(confounding(), selection("general", "increased risk", "S = U"), misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE))
mb28 <- multi_bias(misclassification("outcome"), confounding())

test_that("number of parameters is correct", {
  expect_equal(nrow(attr(mb01, "parameters")), 2)
  expect_equal(nrow(attr(mb02, "parameters")), 4)
  expect_equal(nrow(attr(mb03, "parameters")), 2)
  expect_equal(nrow(attr(mb04, "parameters")), 2)
  expect_equal(nrow(attr(mb05, "parameters")), 1)
  expect_equal(nrow(attr(mb06, "parameters")), 2)
  expect_equal(nrow(attr(mb07, "parameters")), 1)
  expect_equal(nrow(attr(mb08, "parameters")), 1)
  expect_equal(nrow(attr(mb09, "parameters")), 1)
  expect_equal(nrow(attr(mb10, "parameters")), 6)
  expect_equal(nrow(attr(mb11, "parameters")), 7)
  expect_equal(nrow(attr(mb12, "parameters")), 7)
  expect_equal(nrow(attr(mb13, "parameters")), 4)
  expect_equal(nrow(attr(mb14, "parameters")), 3)
  expect_equal(nrow(attr(mb15, "parameters")), 5)
  expect_equal(nrow(attr(mb16, "parameters")), 5)
  expect_equal(nrow(attr(mb17, "parameters")), 4)
  expect_equal(nrow(attr(mb18, "parameters")), 3)
  expect_equal(nrow(attr(mb19, "parameters")), 3)
  expect_equal(nrow(attr(mb20, "parameters")), 3)
  expect_equal(nrow(attr(mb21, "parameters")), 3)
  expect_equal(nrow(attr(mb22, "parameters")), 3)
  expect_equal(nrow(attr(mb23, "parameters")), 3)
  expect_equal(nrow(attr(mb24, "parameters")), 3)
  expect_equal(nrow(attr(mb25, "parameters")), 3)
  expect_equal(nrow(attr(mb26, "parameters")), 2)
  expect_equal(nrow(attr(mb27, "parameters")), 4)
  expect_equal(nrow(attr(mb28, "parameters")), 3)
})


test_that("number of degrees in the numerator is correct", {
  expect_equal(attr(mb01, "n"), 2)
  expect_equal(attr(mb02, "n"), 4)
  expect_equal(attr(mb03, "n"), 2)
  expect_equal(attr(mb04, "n"), 2)
  expect_equal(attr(mb05, "n"), 1)
  expect_equal(attr(mb06, "n"), 2)
  expect_equal(attr(mb07, "n"), 1)
  expect_equal(attr(mb08, "n"), 2)
  expect_equal(attr(mb09, "n"), 1)
  expect_equal(attr(mb10, "n"), 6)
  expect_equal(attr(mb11, "n"), 7)
  expect_equal(attr(mb12, "n"), 7)
  expect_equal(attr(mb13, "n"), 4)
  expect_equal(attr(mb14, "n"), 3)
  expect_equal(attr(mb15, "n"), 5)
  expect_equal(attr(mb16, "n"), 5)
  expect_equal(attr(mb17, "n"), 5)
  expect_equal(attr(mb18, "n"), 4)
  expect_equal(attr(mb19, "n"), 4)
  expect_equal(attr(mb20, "n"), 4)
  expect_equal(attr(mb21, "n"), 4)
  expect_equal(attr(mb22, "n"), 3)
  expect_equal(attr(mb23, "n"), 3)
  expect_equal(attr(mb24, "n"), 3)
  expect_equal(attr(mb25, "n"), 3)
  expect_equal(attr(mb26, "n"), 2)
  expect_equal(attr(mb27, "n"), 4)
  expect_equal(attr(mb28, "n"), 3)
})

test_that("number of degrees in the denominator is correct", {
  expect_equal(attr(mb01, "d"), 1)
  expect_equal(attr(mb02, "d"), 2)
  expect_equal(attr(mb03, "d"), 1)
  expect_equal(attr(mb04, "d"), 1)
  expect_equal(attr(mb05, "d"), 0)
  expect_equal(attr(mb06, "d"), 0)
  expect_equal(attr(mb07, "d"), 0)
  expect_equal(attr(mb08, "d"), 0)
  expect_equal(attr(mb09, "d"), 0)
  expect_equal(attr(mb10, "d"), 3)
  expect_equal(attr(mb11, "d"), 3)
  expect_equal(attr(mb12, "d"), 3)
  expect_equal(attr(mb13, "d"), 2)
  expect_equal(attr(mb14, "d"), 1)
  expect_equal(attr(mb15, "d"), 2)
  expect_equal(attr(mb16, "d"), 2)
  expect_equal(attr(mb17, "d"), 1)
  expect_equal(attr(mb18, "d"), 1)
  expect_equal(attr(mb19, "d"), 1)
  expect_equal(attr(mb20, "d"), 1)
  expect_equal(attr(mb21, "d"), 1)
  expect_equal(attr(mb22, "d"), 1)
  expect_equal(attr(mb23, "d"), 1)
  expect_equal(attr(mb24, "d"), 1)
  expect_equal(attr(mb25, "d"), 1)
  expect_equal(attr(mb26, "d"), 1)
  expect_equal(attr(mb27, "d"), 1)
  expect_equal(attr(mb28, "d"), 1)
})

##### bounds ####

suppressWarnings({
mb01_bound <- multi_bound(mb01, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb02_bound <- multi_bound(mb02, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb03_bound <- multi_bound(mb03, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb04_bound <- multi_bound(mb04, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb05_bound <- multi_bound(mb05, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb06_bound <- multi_bound(mb06, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb07_bound <- multi_bound(mb07, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb08_bound <- multi_bound(mb08, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb09_bound <- multi_bound(mb09, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb10_bound <- multi_bound(mb10, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb11_bound <- multi_bound(mb11, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb12_bound <- multi_bound(mb12, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb13_bound <- multi_bound(mb13, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb14_bound <- multi_bound(mb14, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb15_bound <- multi_bound(mb15, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb16_bound <- multi_bound(mb16, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb17_bound <- multi_bound(mb17, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb18_bound <- multi_bound(mb18, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb19_bound <- multi_bound(mb19, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb20_bound <- multi_bound(mb20, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb21_bound <- multi_bound(mb21, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb22_bound <- multi_bound(mb22, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb23_bound <- multi_bound(mb23, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb24_bound <- multi_bound(mb24, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb25_bound <- multi_bound(mb25, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb26_bound <- multi_bound(mb26, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb27_bound <- multi_bound(mb27, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
mb28_bound <- multi_bound(mb28, RRAUc = 2, RRUcY = 2, RRUsYA1 = 3, RRSUsA1 = 3, RRUsYA0 = 4, RRSUsA0 = 4, RRAUsS = 5, RRUsYS = 5, RRAUscS = 6, RRUscYS = 6, RRAYy = 7, RRAYyS = 7, ORYAa = 8, ORYAaS = 8, RRYAa = 9, RRYAaS = 9)
})


test_that("the multi-bias bound is correct", {
  expect_equal(mb01_bound, 1.33333, tolerance = 1e-5)
  expect_equal(mb02_bound, 1.8 * 2.28571, tolerance = 1e-5)
  expect_equal(mb03_bound, 2.77778, tolerance = 1e-5)
  expect_equal(mb04_bound, 1.8, tolerance = 1e-5)
  expect_equal(mb05_bound, 4, tolerance = 1e-5)
  expect_equal(mb06_bound, 3 * 4, tolerance = 1e-5)
  expect_equal(mb07_bound, 7, tolerance = 1e-5)
  expect_equal(mb08_bound, 8, tolerance = 1e-5)
  expect_equal(mb09_bound, 9, tolerance = 1e-5)
  expect_equal(mb10_bound, 1.33333 * 1.8 * 2.28571, tolerance = 1e-5)
  expect_equal(mb11_bound, 1.33333 * 1.8 * 2.28571 * 7, tolerance = 1e-5)
  expect_equal(mb12_bound, 1.33333 * 1.8 * 2.28571 * 7, tolerance = 1e-5)
  expect_equal(mb13_bound, 1.33333 * 1.8, tolerance = 1e-5)
  expect_equal(mb14_bound, 1.33333 * 3, tolerance = 1e-5)
  expect_equal(mb15_bound, 1.33333 * 1.8 * 7, tolerance = 1e-5)
  expect_equal(mb16_bound, 1.33333 * 1.8 * 7, tolerance = 1e-5)
  expect_equal(mb17_bound, 1.33333 * 3 * 8, tolerance = 1e-5)
  expect_equal(mb18_bound, 3.27273 * 8, tolerance = 1e-5)
  expect_equal(mb19_bound, 3.27273 * 8, tolerance = 1e-5)
  expect_equal(mb20_bound, 2.77778 * 8, tolerance = 1e-5)
  expect_equal(mb21_bound, 2.77778 * 8, tolerance = 1e-5)
  expect_equal(mb22_bound, 3.27273 * 9 , tolerance = 1e-5)
  expect_equal(mb23_bound, 3.27273 * 9, tolerance = 1e-5)
  expect_equal(mb24_bound, 2.77778 * 9, tolerance = 1e-5)
  expect_equal(mb25_bound, 2.77778 * 9, tolerance = 1e-5)
  expect_equal(mb26_bound, 3.27273, tolerance = 1e-5)
  expect_equal(mb27_bound, 1.33333 * 3 * 9, tolerance = 1e-5)
  expect_equal(mb28_bound, 1.33333 * 7, tolerance = 1e-5)
  })


##### equal single bias functions ####
mb01 <- multi_bias(confounding())
mb02 <- multi_bias(selection("general"))
mb03 <- multi_bias(selection("selected"))
mb04 <- multi_bias(selection("general", "increased risk"))
mb05 <- multi_bias(selection("general", "decreased risk", "S = U"))
mb06 <- multi_bias(selection("general", "S = U"))

test_that("the multi-bias evalue equals the single-bias evalue", {
  # RR
  expect_equal(summary(multi_evalue(mb01, est = RR(3.5))), 
               summary(evalue(RR(3.5))), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = RR(.5))), 
               summary(selection_evalue(RR(.5))), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = RR(4.7))), 
               summary(selection_evalue(RR(4.7), sel_pop = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = RR(.8))), 
               summary(selection_evalue(RR(.8), risk_inc = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb05, est = RR(1.8))), 
               summary(selection_evalue(RR(1.8), risk_dec = TRUE, S_eq_U = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb06, est = RR(5.3))), 
               summary(selection_evalue(RR(5.3), S_eq_U = TRUE)), 
               tolerance = 1e-5)
  # HR, rare
  expect_equal(summary(multi_evalue(mb01, est = HR(.6, rare = TRUE))), 
               summary(evalue(HR(.6, rare = TRUE))), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = HR(2.6, rare = TRUE))), 
               summary(selection_evalue(HR(2.6, rare = TRUE))), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = HR(4.7, rare = TRUE))), 
               summary(selection_evalue(HR(4.7, rare = TRUE), sel_pop = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = HR(6.2, rare = TRUE))), 
               summary(selection_evalue(HR(6.2, rare = TRUE), risk_inc = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb05, est = HR(.3, rare = TRUE))), 
               summary(selection_evalue(HR(.3, rare = TRUE), risk_dec = TRUE, S_eq_U = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb06, est = HR(5.3, rare = TRUE))), 
               summary(selection_evalue(HR(5.3, rare = TRUE), S_eq_U = TRUE)), 
               tolerance = 1e-5)
  
  # HR, non-rare
  expect_equal(summary(multi_evalue(mb01, est = HR(3.5, rare = FALSE))), 
               summary(evalue(HR(3.5, rare = FALSE))), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = HR(2.6, rare = FALSE))), 
               summary(selection_evalue(HR(2.6, rare = FALSE))), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = HR(.1, rare = FALSE))), 
               summary(selection_evalue(HR(.1, rare = FALSE), sel_pop = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = HR(.9, rare = FALSE))), 
               summary(selection_evalue(HR(.9, rare = FALSE), risk_inc = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb05, est = HR(1.8, rare = FALSE))), 
               summary(selection_evalue(HR(1.8, rare = FALSE), risk_dec = TRUE, S_eq_U = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb06, est = HR(.8, rare = FALSE))), 
               summary(selection_evalue(HR(.8, rare = FALSE), S_eq_U = TRUE)), 
               tolerance = 1e-5)
  
  # OR, rare
  expect_equal(summary(multi_evalue(mb01, est = OR(3.5, rare = TRUE))), 
               summary(evalue(OR(3.5, rare = TRUE))), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = OR(2.6, rare = TRUE))), 
               summary(selection_evalue(OR(2.6, rare = TRUE))), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = OR(4.7, rare = TRUE))), 
               summary(selection_evalue(OR(4.7, rare = TRUE), sel_pop = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = OR(6.2, rare = TRUE))), 
               summary(selection_evalue(OR(6.2, rare = TRUE), risk_inc = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb05, est = OR(1.8, rare = TRUE))), 
               summary(selection_evalue(OR(1.8, rare = TRUE), risk_dec = TRUE, S_eq_U = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb06, est = OR(5.3, rare = TRUE))), 
               summary(selection_evalue(OR(5.3, rare = TRUE), S_eq_U = TRUE)), 
               tolerance = 1e-5)
  
  # OR, non-rare
  expect_equal(summary(multi_evalue(mb01, est = HR(3.5, rare = FALSE))), 
               summary(evalue(HR(3.5, rare = FALSE))), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = HR(2.6, rare = FALSE))), 
               summary(selection_evalue(HR(2.6, rare = FALSE))), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = HR(4.7, rare = FALSE))), 
               summary(selection_evalue(HR(4.7, rare = FALSE), sel_pop = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = HR(6.2, rare = FALSE))), 
               summary(selection_evalue(HR(6.2, rare = FALSE), risk_inc = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb05, est = HR(1.8, rare = FALSE))), 
               summary(selection_evalue(HR(1.8, rare = FALSE), risk_dec = TRUE, S_eq_U = TRUE)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb06, est = HR(5.3, rare = FALSE))), 
               summary(selection_evalue(HR(5.3, rare = FALSE), S_eq_U = TRUE)), 
               tolerance = 1e-5)
  
  # with different true
  # RR
  expect_equal(summary(multi_evalue(mb01, est = RR(3.5), true = 2)), 
               summary(evalue(RR(3.5), true = 2)), tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb02, est = RR(.5), true = 2)), 
               summary(selection_evalue(RR(.5), true = 2)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb03, est = RR(4.7), true = .5)), 
               summary(selection_evalue(RR(4.7), sel_pop = TRUE, true = .5)), 
               tolerance = 1e-5)
  expect_equal(summary(multi_evalue(mb04, est = RR(.8), true = .9)), 
               summary(selection_evalue(RR(.8), risk_inc = TRUE, true = .9)), 
               tolerance = 1e-5)

})

