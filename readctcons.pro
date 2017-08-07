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
function readCTcons, clipres, config
  
  ;tolerance
  ;HU water +/-4 from ref, but never above +/-4
  ;diff +/-2 from ref
  ;noise +/-10% from ref
  ;slice +/- 1mm from ref, but never more than +/-1mm from nominal
  ;MTF B30f 3,02-3,70 / 5,17-6,31
  ;MTF H30s 2,84-3,47 / 5,13-6,27
  ;MTF H70h 10,24-12,52 / 12,60-15,4
  
  strArrRes=STRARR(22)
  structRes=CREATE_STRUCT('empty',0)
  IF N_ELEMENTS(clipres) GT 1 THEN BEGIN
    nShortRes=15
    shortres=STRARR(N_ELEMENTS(clipres))
    shortres5=shortres
    FOR i=0, N_ELEMENTS(clipres)-1 DO BEGIN
      shortres(i)=STRMID(clipres(i), 0, nShortRes)
      shortres5(i)=STRMID(clipres(i), 0, 5)
    ENDFOR
    resVectST=FLTARR(4)-1000.; slice thickness min/max head + body
    resVectWA=FLTARR(4)-1000.; HU water min/max head + body
    resVectHO=FLTARR(2)-1000.; max diff center head + body
    resVectNO=FLTARR(2)-1000.; max noise head + body
    resVectMTF=FLTARR(6)-1000.; 50% 10% typical head + mean typical body + sharpest 

    ;detect language and type of report
    nLangu=N_TAGS(config.CT)
    daily=0
    const=0
    typeRep=-1
    FOR i=0, nLangu-1 DO BEGIN
      daily=WHERE(STRMATCH(clipres, '*'+config.CT.(i)(0)+'*', /FOLD_CASE) EQ 1)
      IF daily(0) NE -1 THEN BEGIN
        langu=i
        typeRep=0
        BREAK
      ENDIF
      const=WHERE(STRMATCH(clipres, '*'+config.CT.(i)(1)+'*', /FOLD_CASE) EQ 1)
      IF const(0) NE -1 THEN BEGIN
        langu=i
        typeRep=1
        BREAK
      ENDIF
    ENDFOR

    IF typeRep EQ -1 THEN sv=DIALOG_MESSAGE('Found no language match or no results in file.') ELSE BEGIN

      ;date
      date=STRSPLIT(clipres(2),' ',/EXTRACT)
      date=date(0)
      strArrRes(0)=date
      
      ;tester
      rowno=WHERE(STRMATCH(clipres, config.CT.(langu)(10)+'*', /FOLD_CASE) EQ 1)
      strArrRes(1)=STRMID(clipres(rowno(0)),STRLEN(config.CT.(langu)(10)))
      ;product Name
      rowno=WHERE(STRMATCH(clipres, config.CT.(langu)(11)+'*', /FOLD_CASE) EQ 1)
      strArrRes(2)=STRMID(clipres(rowno(0)),STRLEN(config.CT.(langu)(11)))     
      ;serial Number
      rowno=WHERE(STRMATCH(clipres, config.CT.(langu)(12)+'*', /FOLD_CASE) EQ 1)
      strArrRes(3)=STRMID(clipres(rowno(0)),STRLEN(config.CT.(langu)(12)))

      rownoST=WHERE(STRMATCH(clipres, '*'+config.CT.(langu)(typeRep)+'*'+config.CT.(langu)(2)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Slice'
      rownoHO=WHERE(STRMATCH(clipres, '*'+config.CT.(langu)(typeRep)+'*'+config.CT.(langu)(3)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Homogeneity'
      rownoNO=WHERE(STRMATCH(clipres, '*'+config.CT.(langu)(typeRep)+'*'+config.CT.(langu)(4)+'*', /FOLD_CASE) EQ 1) ;'Quality *','Noise'
      rownoMTF=WHERE(STRMATCH(clipres, '*'+config.CT.(langu)(typeRep)+'*MTF*', /FOLD_CASE) EQ 1) ;'Quality Constancy','MTF'

      sliceStr=STRMID(config.CT.(langu)(2), 0, 5)
      tolStr=STRMID(config.CT.(langu)(5), 0, 5)

      strTest=[config.CT.(langu)(6),config.CT.(langu)(7), config.CT.(langu)(9)]; Test: typical head / body / sharpest

      ;slice thickness

      IF N_ELEMENTS(rownoST) EQ 2 THEN BEGIN

        FOR a=0, 1 DO BEGIN
          rowno=WHERE(shortres[rownoST(1):-1] EQ STRMID(strTest(a), 0, nShortRes))
          IF rowno(0) NE -1 THEN BEGIN
            rowno=rowno+rownoST(1)

            ;number of Images in test
            rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], config.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
            strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
            nImg=LONG(strNimg(-1))

            addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
            addSlice=addSlice[1:-1];first one is Nominal slice thickness
            rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
            rownoTest=WHERE(shortres5[rowno(0):-1] EQ STRMID(strTest(0), 0, 5))
            IF addSlice(0) NE -1 THEN BEGIN
              IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                actLines=WHERE(addslice LT rownoTol(0),nImg)
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(-2))
                ENDFOR
              ENDIF ELSE BEGIN
                IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                  actLines=WHERE(addslice LT rownoTest(1))
                  actLines=addSlice(actLines)+2
                ENDIF ELSE actLines=addSlice+2
                nImg=N_ELEMENTS(actLines)
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(0))
                ENDFOR
              ENDELSE

              resVectST[0+a*2:1+a*2]=[MIN(res),MAX(res)]
            ENDIF
          ENDIF
        ENDFOR
        structRes=CREATE_STRUCT(structRes, 'ST', resVectST)
      ENDIF

      ;homogenity / water
      IF N_ELEMENTS(rownoHO) EQ 2 THEN BEGIN

        ;min/max HU water
        FOR a=0, 1 DO BEGIN
          rowno=WHERE(shortres[rownoHO(1):-1] EQ STRMID(strTest(a), 0, nShortRes))
          IF rowno(0) NE -1 THEN BEGIN
            rowno=rowno+rownoHO(1)

            ;number of Images in test
            rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], config.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
            strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
            nImg=LONG(strNimg(-1))

            addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
            addSlice=addSlice[1:-1];first one is Nominal slice thickness
            rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
            IF addSlice(0) NE -1 THEN BEGIN
              ;min/max HU water
              tolEnd=0
              IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                tolEnd=1
                actLines=WHERE(addslice LT rownoTol(0),nImg)
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(-2))
                ENDFOR
              ENDIF ELSE BEGIN
                
                diff=addSlice- SHIFT(addSlice,-1)
                tt=WHERE(diff[0:nImg] EQ diff(0), nEQ)
                nImg=nEQ+1
                actLines=addSlice[0:nImg-1]+2
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(0))
                ENDFOR
              ENDELSE
              resVectWA[0+a*2:1+a*2]=[MIN(res),MAX(res)]

              ;max inhomogeneity
              actLines=addslice[nImg:2*nImg-1]
              res=FLTARR(5,nImg)
              IF tolEnd THEN BEGIN
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                  FOR d=0, 4 DO BEGIN
                    res(d,i)=FLOAT(clipSplit(-2*(5-d)))
                  ENDFOR
                ENDFOR
              ENDIF ELSE BEGIN
                FOR i=0, nImg-1 DO BEGIN
                  FOR d=1, 5 DO BEGIN
                    clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)+2*d),' ',/EXTRACT)
                    res(d-1,i)=FLOAT(clipSplit(0))
                  ENDFOR
                ENDFOR
              ENDELSE
              resVectHO(a)=MAX(ABS(res[1:4,*]))
            ENDIF

          ENDIF
        ENDFOR
        structRes=CREATE_STRUCT(structRes, 'WA', resVectWA, 'HO', resVectHO)

      ENDIF

      ;noise
      IF N_ELEMENTS(rownoNO) EQ 2 THEN BEGIN
        FOR a=0, 1 DO BEGIN
          rowno=WHERE(shortres[rownoNO(1):-1] EQ STRMID(strTest(a), 0, nShortRes))
          IF rowno(0) NE -1 THEN BEGIN
            rowno=rowno+rownoNO(1)

            ;number of Images in test
            rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], config.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
            strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
            nImg=LONG(strNimg(-1))

            addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
            addSlice=addSlice[1:-1];first one is Nominal slice thickness
            rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
            rownoTest=WHERE(shortres5[rowno(0):-1] EQ STRMID(strTest(0), 0, 5))
            IF addSlice(0) NE -1 THEN BEGIN
              IF addslice(1) EQ addslice(0)+1 THEN BEGIN ; tolerance at end
                actLines=WHERE(addslice LT rownoTol(0),nImg)
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(-2))
                ENDFOR
              ENDIF ELSE BEGIN
                IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                  actLines=WHERE(addslice LT rownoTest(1))
                  actLines=addSlice(actLines)+2
                ENDIF ELSE actLines=addSlice+2
                nImg=N_ELEMENTS(actLines)
                res=FLTARR(nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                  res(i)=FLOAT(clipSplit(0))
                ENDFOR
              ENDELSE
              resVectNO(a)=MAX(res)
            ENDIF

          ENDIF
        ENDFOR
        structRes=CREATE_STRUCT(structRes, 'NO', resVectNO)
      ENDIF

      ;MTF
      IF N_ELEMENTS(rownoMTF) EQ 2 THEN BEGIN
        
        strTestResort=[strTest(1),strTest(0),strTest(2)]
        FOR a=0, 2 DO BEGIN
          rowno=WHERE(shortres[rownoMTF(1):-1] EQ STRMID(strTestResort(a), 0, nShortRes))
          IF rowno(0) NE -1 THEN BEGIN
            rowno=rowno+rownoMTF(1)
            
            ;number of Images in test
            rownoNimg=WHERE(STRMATCH(clipres[rowno(0):-1], config.CT.(langu)(8)+'*',/FOLD_CASE) EQ 1)
            strNimg=STRSPLIT(clipres(rownoNimg(0)+rowno(0)),' ',/EXTRACT)
            nImg=LONG(strNimg(-1))

            addSlice=WHERE(shortres5[rowno(0):-1] EQ sliceStr)
            addSlice=addSlice[1:-1];first one is Nominal slice thickness
            rownoTol=WHERE(shortres5[rowno(0):-1] EQ tolStr)
            rownoTest=WHERE(shortres5[rowno(0):-1] EQ STRMID(strTest(0), 0, 5))
            IF addSlice(0) NE -1 THEN BEGIN
              IF N_ELEMENTS(STRSPLIT(clipres(rowno(0)+addslice(0)),' ',/EXTRACT)) GT 2 THEN BEGIN; tolerance at end
                actLines=WHERE(addslice LT rownoTol(0))
                nImg=N_ELEMENTS(actLines)
                res=FLTARR(2,nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+addSlice(actLines(i))),' ',/EXTRACT)
                  res(0,i)=FLOAT(clipSplit(-4))
                  res(1,i)=FLOAT(clipSplit(-2))
                ENDFOR
              ENDIF ELSE BEGIN
                IF rownoTest(0) NE -1 AND N_ELEMENTS(rownoTest) GT 1 THEN BEGIN
                  actLines=WHERE(addslice LT rownoTest(1))
                  actLines=addSlice(actLines)+2
                ENDIF ELSE actLines=addSlice+2
                nImg=N_ELEMENTS(actLines)
                res=FLTARR(2,nImg)
                FOR i=0, nImg-1 DO BEGIN
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)),' ',/EXTRACT)
                  res(0,i)=FLOAT(clipSplit(0))
                  clipSplit=STRSPLIT(clipres(rowno(0)+actLines(i)+2),' ',/EXTRACT)
                  res(1,i)=FLOAT(clipSplit(0))
                ENDFOR
              ENDELSE
              resVectMTF[0+a*2:1+a*2]=[MEAN(res[0,*]),MEAN(res[1,*])]
            ENDIF

          ENDIF
        ENDFOR
        structRes=CREATE_STRUCT(structRes, 'MTF', resVectMTF)

      ENDIF

    ENDELSE;no language match
  ENDIF; N_ELEMENTS(clipres)>1
 
  IF MIN(resVectWA) NE -1000. THEN strArrRes[4:4+3]=STRING(resVectWA, FORMAT='(f0.2)')
  IF MIN(resVectHO) NE -1000. THEN strArrRes[8:8+1]=STRING(resVectHO, FORMAT='(f0.2)')
  IF MIN(resVectNO) NE -1000. THEN strArrRes[10:10+1]=STRING(resVectNO, FORMAT='(f0.2)')
  IF MIN(resVectST) NE -1000. THEN strArrRes[12:12+3]=STRING(resVectST, FORMAT='(f0.2)')
  IF MIN(resVectMTF) NE -1000. THEN strArrRes[16:16+5]=STRING(resVectMTF, FORMAT='(f0.2)')

  return, strArrRes

end