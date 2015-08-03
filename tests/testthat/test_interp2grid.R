library(climates)

context("Test interp2grid function")

test_that("Example comes back as expected.", {
  load('data/interp2grid_example.xz')

  bilinear1_check = interp2grid(tmat,tx,ty,type=1)
  bicubic2_check = interp2grid(tmat,tx,ty,type=2)
  bicubic3_check = interp2grid(tmat,tx,ty,type=3)


  expect_equal(bilinear1[which(!is.na(bilinear1))],bilinear1_check[which(!is.na(bilinear1_check))])
  expect_equal(bicubic2[which(!is.na(bicubic2))],bicubic2_check[which(!is.na(bicubic2_check))])
  expect_equal(bicubic3[which(!is.na(bicubic3))],bicubic3_check[which(!is.na(bicubic3_check))])
})