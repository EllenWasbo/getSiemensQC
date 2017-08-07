pro MakeExeGetSiemensQC
  exePath='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\'
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('getSiemensQC'))+'\'
  MAKE_RT, 'getSiemensQC', exePath, /OVERWRITE, SAVEFILE=thisPath+'getSiemensQC.sav', /VM, /WIN32

  ;----------------------
  ;RenameDICOM.ini (change show to false)
  ;[DIALOG]
  ;Show=FALSE
  ;BackColor=&H6B1F29
  ;Caption=IDL Virtual Machine Application
  ;Picture=.\splash.bmp
  ;DefaultAction=.\IDL71\bin\bin.x86\idlrt.exe -vm=getSiemensQC.sav
  ;
  ;[BUTTON1]
  ;Show=True
  ;Caption=TestObjGraphics
  ;Action=.\IDL71\bin\bin.x86\idlrt.exe -vm=getSiemensQC.sav
  ;
  ;[BUTTON2]
  ;Show=True
  ;Caption=Exit
  ;Action=Exit
  ;
  ;[BUTTON3]
  ;Show=False
  ;Caption=
  ;Action=
  ;
  ;[BUTTON4]
  ;Show=False
  ;Caption=
  ;Action=
  ;-----------------------------
  ;autorun.inf
  ;[autorun]
  ;open = getSiemensQC.exe
  ;icon= getSiemensQC.ico

end