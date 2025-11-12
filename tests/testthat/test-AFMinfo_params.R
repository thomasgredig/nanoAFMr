test_that("AFMinfo image sizes", {
  s_list = AFM.getSampleImages()
  size_um = c(4,3.25,1,10,NA,2.5)
  for (i in c(1:6)) {
    q <- AFMinfo(s_list[i])
    # cat(s_list[i],"  ",q$imageSize.um,"\n")
    expect_equal(q$imageSize.um, size_um[i])
  }
})

test_that("AFMinfo scan angles", {
  s_list = AFM.getSampleImages()
  angle = c(0,0,0,0,NA,0)
  for (i in c(1:6)) {
    q <- AFMinfo(s_list[i])
    # cat(s_list[i],"  ",q$scanAngle,"\n")
    expect_equal(q$scanAngle, angle[i])
  }
})
