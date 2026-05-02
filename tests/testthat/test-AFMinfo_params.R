test_that("AFMinfo image sizes", {
  s_list = sort(AFM.getSampleImages())
  size_um = c(
    AR_20211011.ibw = 4,
    ForceCurve_009.tiff = 3.25,
    kpg20080408.007 = 1,
    NanoSurf_20160301.nid = 10,
    NanoSurf_resonancePeak.nid = NA_real_,
    Park_20210916_034.tiff = 2.5
  )
  for (f in s_list) {
    q <- AFMinfo(f)
    b <- basename(f)
    expect_equal(q$imageSize.um, size_um[[b]])
  }
})

test_that("AFMinfo scan angles", {
  s_list = sort(AFM.getSampleImages())
  angle = c(
    AR_20211011.ibw = 0,
    ForceCurve_009.tiff = 0,
    kpg20080408.007 = 0,
    NanoSurf_20160301.nid = 0,
    Park_20210916_034.tiff = 0
  )
  for (f in s_list) {
    q <- AFMinfo(f)
    b <- basename(f)
    if (b == "NanoSurf_resonancePeak.nid") {
      expect_true(is.na(q$scanAngle) || isTRUE(all.equal(q$scanAngle, 0)))
    } else {
      expect_equal(q$scanAngle, angle[[b]])
    }
  }
})
