;getSiemensQC - extraction of figures to table from Siemens QC reports (CT constancy/daily and PET daily QC)
;Copyright (C) 2017  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See thef
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
function readPETdailyQC, clipres, config
errMsg=''
  shortres=STRARR(N_ELEMENTS(clipres))
  FOR i=0, N_ELEMENTS(clipres)-1 DO shortres(i)=STRMID(clipres(i), 0, 10)
  resVect=FLTARR(9)

  ;date
  rowno=WHERE(shortres EQ 'Scan Date:')
  date=STRSPLIT(clipres(rowno),',',/EXTRACT)
  dateMD=STRSPLIT(date(0),' ',/EXTRACT)
  IF N_ELEMENTS(dateMD) GE 4 THEN BEGIN
    day=STRING(LONG(STRTRIM(dateMD(3),2)),FORMAT='(i02)')
    month=STRTRIM(dateMD(2),2)
    monthNmb=WHERE(month EQ config.months)
    IF monthNmb(0) NE -1 THEN month=STRING(LONg(monthNmb(0))+1, FORMAT='(i02)')
    year=STRTRIM(date(1),2)
    date=day+'.'+month+'.'+year
  ENDIF ELSE date=''

  rowno=WHERE(shortres EQ 'Partial se')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    IF clipSplit(-1) EQ 'true' THEN part='X' ELSE part=''
  ENDIF ELSE part=''
  rowno=WHERE(shortres EQ 'Full setup')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    IF clipSplit(-1) EQ 'true' THEN full='X' ELSE full=''
  ENDIF ELSE full=''
  rowno=WHERE(shortres EQ 'Time Align')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    IF clipSplit(-1) EQ 'true' THEN timA='X' ELSE timA=''
  ENDIF ELSE timA=''
  rowno=WHERE(shortres EQ 'Calibratio')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    calib=FLOAT(clipSplit(-1))
  ENDIF ELSE calib=-1


  ;Block Noise 3 [crystal] 0 [crystal] 0 Blocks
  rowno=WHERE(shortres EQ 'Block Nois')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,'Block noise out of range.']
  ENDIF

  ;Block Efficiency 120 [%] 80 [%] 0 Blocks
  rowno=WHERE(shortres EQ 'Block Effi')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,'Block efficiency out of range.']
  ENDIF

  ;Randoms 115 [%] 85 [%] 103.8 [%] Passed
  rowno=WHERE(shortres EQ 'Randoms 11')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    measuredRandoms=FLOAT(clipSplit(-3))
  ENDIF ELSE measuredRandoms=-1
  resVect(0)=measuredRandoms

  ;Scanner Efficiency 47.32 [cps/Bq/cc] 25.48 [cps/Bq/cc] 37.7 [cps/Bq/cc] Passed
  rowno=WHERE(shortres EQ 'Scanner Ef')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    scannerEff=FLOAT(clipSplit(-3))
  ENDIF ELSE scannerEff=-1
  resVect(1)=scannerEff

  ;Scatter Ratio 35.2 [%] 28.8 [%] 30.7 [%] Passed
  rowno=WHERE(shortres EQ 'Scatter Ra')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)),' ',/EXTRACT)
    scatterRat=FLOAT(clipSplit(-3))
  ENDIF ELSE scatterRat=-1
  resVect(2)=scatterRat

  ;      Scanner efficiency
  ;      correction factor
  ;      (ECF)
  ;      4e+007 [Bq*s/
  ;      ECAT counts]
  ;      2e+007 [Bq*s/
  ;      ECAT counts]
  ;      3.147e+007
  ;      [Bq*s/ECAT
  ;      counts]
  ;      Passed
  rowno=WHERE(shortres EQ '(ECF)')
  IF rowno(0) NE -1 THEN ECF=FLOAT(clipres(rowno(0)+5)) ELSE ECF=-1
  resVect(3)=ECF

  ;      Image Plane
  ;      Efficiency 5 [%] -5 [%] 0 Planes
  ;      out of range Passed
  rowno=WHERE(shortres EQ 'Image Plan')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)+1),' ',/EXTRACT)
    IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,'Image plane efficiency out of range.']
  ENDIF

  ;      Block Timing
  ;      Offset 0.5 [bin] 0 [bin] 0 Blocks
  ;      out of range Passed
  rowno=WHERE(shortres EQ 'Block Timi')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)+1),' ',/EXTRACT)
    IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,'Block timing offset out of range.']
    IF N_ELEMENTS(rowno) EQ 2 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
      IF clipSplit(-2) NE '0' THEN errMsg=[errMsg,'Block timing width out of range.']
    ENDIF
  ENDIF

  ;      Time Alignment
  ;      Residual 3 [mm] 0 [mm] 1.17 [mm] Passed
  ;      Time Alignment Fit
  ;      (x / y) 2 [mm] 0 [mm] 0.38 [mm] /
  ;      0.41 [mm] Passed
  rowno=WHERE(shortres EQ 'Time Align')
  IF N_ELEMENTS(rowno) GE 3 THEN BEGIN
    IF rowno(1) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(1)+1),' ',/EXTRACT)
      timeAlignResid=FLOAT(clipSplit(-3))
    ENDIF ELSE timeAlignResid=-1
    resVect(4)=timeAlignResid
    IF rowno(2) NE -1 THEN BEGIN
      clipSplit=STRSPLIT(clipres(rowno(2)+1),' ',/EXTRACT)
      timeAlignFitx=FLOAT(clipSplit(-3))
      clipSplit=STRSPLIT(clipres(rowno(2)+2),' ',/EXTRACT)
      timeAlignFity=FLOAT(clipSplit(0))
      timeAlignFit=[timeAlignFitx,timeAlignFity]
    ENDIF ELSE timeAlignFit=[-1,-1]
    resVect[5:6]=timeAlignFit
  ENDIF

  ;      Phantom position:
  ;      Axis Value [mm]
  ;      X -0.2
  ;      Y -1.9
  rowno=WHERE(shortres EQ 'Phantom po')
  IF rowno(0) NE -1 THEN BEGIN
    clipSplit=STRSPLIT(clipres(rowno(0)+2),' ',/EXTRACT)
    phantomPosx=FLOAT(clipSplit(1))
    clipSplit=STRSPLIT(clipres(rowno(0)+3),' ',/EXTRACT)
    phantomPosy=FLOAT(clipSplit(1))
    phantomPos=[phantomPosx,phantomPosy]
  ENDIF ELSE phantomPos=[-1,-1]
  resVect[7:8]=phantomPos

  resArr=[date, part, full, timA, STRING(calib),STRING(resVect)]
  
   
  res=CREATE_STRUCT('strArrRes',resArr, 'errMsg', errMsg)
  
  return, res
end