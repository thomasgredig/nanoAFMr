# test script

devtools::load_all(".")
fname = "/Users/gredigcsulb/CSULB/Gredig Molecular Thin Film Lab - General/AytanaSanchez/RAW/20220127_highresAFM_AS_AFM_TS100921Si1FePc200nm220C0000.ibw"
# fname = AFM.getSampleImages('ibw')
file.exists(fname)
a = AFM.import(fname)
plot(a)
summary(a)
print(a)
