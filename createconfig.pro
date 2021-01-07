pro createConfig

PETstrings=CREATE_STRUCT($
  'English',['Scan Date:','Partial Setup','Full setup','Time Alignment','Calibration','Block Noise','Block Efficiency','Randoms','Scanner Efficiency','Scatter Ratio','Image Plane','Block Timing','Phantom position','true'])
CTstrings=CREATE_STRUCT($
  'English',['Quality Daily','Quality Constancy','Slice','Homogeneity','Noise','Tolerance','Test: Typical head','Test: Typical body','Number of images','Test: Sharpest mode','Tester name','Product Name','Serial Number','Tube Asse','Description','Value','Result','Test Result'],$
  'Norsk',['Kvalitet daglig','Kvalitetskonstans','Snitt','Homogenitet','St?y','Toleranse','Test: Typisk hode','Test: Typisk kropp','Antall bilder','Test: Skarpeste modus','Kontroll?rnavn','Produktnavn','Serienummer','R?renhet','Beskrivelse','Verdi','Resultat','Test Resultat'])
months=['January','February','March','April','May','June','July','August','September','October','November','December']

config=CREATE_STRUCT('PET',PETstrings,'CT',CTstrings,'months',months)
;stop
thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('getSiemensQC'))+'\' 
SAVE, config, FILENAME=thisPath+'defConfig.dat'
stop
;update existing with appendtemp
defConfig=config
RESTORE, thisPath+'config.dat'
newConfig=CREATE_STRUCT(defConfig,'APPENDTEMPPET',config.APPENDTEMPPET,'APPENDTEMPCT',config.APPENDTEMPCT)
config=newConfig
SAVE, config, FILENAME=thisPath+'config.dat'
end