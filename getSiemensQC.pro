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

pro getSiemensQC,  GROUP_LEADER=bMain

  COMMON VAR, config, langu, tblRes, headers, colWids, cwType, curType, curCTtype, cw_deciMark, btnTrans, btnHeaders, newline, xoffset, yoffset

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('getSiemensQC'))+'\'
  RESTORE, thisPath+'config.dat'

  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

  months=config.months;['January','February','March','April','May','June','July','August','September','October','November','December']

  langu=0;english default

  xsz=700
  xoffset=50
  yoffset=50

  bMain = WIDGET_BASE(TITLE='getSiemensQC v1.4', MBAR=bar, /COLUMN, XSIZE=xsz, YSIZE=900, XOFFSET=xoffset, YOFFSET=yoffset,/TLB_KILL_REQUEST_EVENTS, /TLB_MOVE_EVENTS)

  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  btn_appPET=WIDGET_BUTTON(file_menu, VALUE='Configure automatic append to txt-file PET', UVALUE='addAppendTempPET')
  btn_appCT=WIDGET_BUTTON(file_menu, VALUE='Configure automatic append to txt-file CT', UVALUE='addAppendTempCT')
  btn_clear=WIDGET_BUTTON(file_menu, VALUE='Clear table', UVALUE='clear')
  btn_exit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='Exit', ACCELERATOR='Ctrl+X', /SEPARATOR)

  ml0=WIDGET_LABEL(bMain, VALUE='', YSIZE=10)

  bTop=WIDGET_BASE(bMain, /ROW)
  bRead=WIDGET_BASE(bTop, /COLUMN, XSIZE=xsz-10)
  lblRead=WIDGET_LABEL(bRead, VALUE='Read report', FONT='Arial*Bold*18')
  bRead2=WIDGET_BASE(bRead, /ROW, FRAME=1, YSIZE=115)
  mlR=WIDGET_LABEL(bRead2, VALUE='', XSIZE=100)
  cwType=CW_BGROUP(bRead2,['PET','CT'], /EXCLUSIVE, LABEL_TOP='Report type...', SET_VALUE=1,FRAME=1, UVALUE='types')
  curType=1
  curCTtype=1; 1 default, 2 = symbia T2
  mlA=WIDGET_LABEL(bRead2, VALUE='', XSIZE=30)
  btnReadClip=WIDGET_BUTTON(bRead2, VALUE='Read from clipboard', UVALUE='readClip', XSIZE=200)
  mlA1=WIDGET_LABEL(bRead2, VALUE='', XSIZE=30)
  bInstructions=WIDGET_BASE(bRead2, /COLUMN, /ALIGN_LEFT)
  inst0=WIDGET_LABEL(bInstructions, VALUE='Instructions', XSIZE=180, FONT='Arial*Bold*12')
  inst1=WIDGET_LABEL(bInstructions, VALUE='* Open a report (pdf or txt)', XSIZE=180)
  inst2=WIDGET_LABEL(bInstructions, VALUE='* Select all text (Ctrl+A)', XSIZE=180)
  inst3=WIDGET_LABEL(bInstructions, VALUE='* Copy to clipboard (Ctrl+C)', XSIZE=180)
  inst4=WIDGET_LABEL(bInstructions, VALUE='* Press "Read from clipboard"', XSIZE=180)
  inst5=WIDGET_LABEL(bInstructions, VALUE='* Repeat for all reports', XSIZE=180)
  mlT=WIDGET_LABEL(bRead, VALUE='', YSIZE=10)
  btnReadExcel=WIDGET_BUTTON(bRead, VALUE='Read from txt files (automated)', UVALUE='readTxtFiles', XSIZE=200)

  ml1=WIDGET_LABEL(bMain, VALUE='', YSIZE=15)

  headersPET=['Date','ICS Name','Partial','Full',$
    'Time Align','Calib Factor','Measured Randoms','Scanner Efficiency',$
    'Scatter Ratio','ECF','Time Alignment Residual', 'Time Alignment fit x','Time Alignment fit y',$
    'Phantom Pos x', 'Phantom Pos y']
  headersCT=['Date','Tester name','Product Name','Serial Number','Serial Tube A', 'Serial Tube B',$
    'HUwater head min','HUwater head max','HUwater body min','HUwater body max',$
    'Diff head max(abs)','Diff body max(abs)',$
    'Noise head max','Noise body max',$
    'Slice head min','Slice head max','Slice body min','Slice body max',$
    'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 H sharp','MTF10 H sharp','MTF50 UHR','MTF10 UHR',$
    'HUwater dblA min','HUwater dblA max','HUwater dblB min','HUwater dblB max',$
    'Diff dblA max(abs)','Diff dblB max(abs)',$
    'Noise dblA max','Noise dblB max',$
    'Slice dblA min','Slice dblA max','Slice dblB min','Slice dblB max',$
    'MTF50 dblA smooth','MTF10 dblA smooth','MTF50 dblB smooth','MTF10 dblB smooth']
  headersCT_T2=['Date','Tester name','Product Name','Serial Number','Tube ID',$
    'HUwater 110kV min','HUwater 110kV max','HUwater 130kV min','HUwater 130kV max',$
    'Diff 110kV max(abs)','Diff 130kV max(abs)',$
    'Noise 80kV','Noise 110kV','Noise 130kV',$
    'Slice 1mm','Slice 1.5mm','Slice 2.5mm','Slice 4mm','Slice 5mm',$
    'MTF50 B31s','MTF10 B31s','MTF50 H41s','MTF10 H41s','MTF50 U90s','MTF10 U90s']
  headersCT_Intevo=['Date','Tester name','Product Name','Serial Number','Tube ID',$
    'HUwater head min','HUwater head max','HUwater body min','HUwater body max',$
    'Diff head max(abs)','Diff body max(abs)',$
    'Noise head max','Noise body max',$
    'Slice head min','Slice head max','Slice body min','Slice body max',$
    'MTF50 B smooth','MTF10 B smooth','MTF50 H smooth','MTF10 H smooth','MTF50 UHR','MTF10 UHR']
  headers=CREATE_STRUCT('PET',headersPET,'CT', headersCT,'CT_T2', headersCT_T2)
  tblRes=WIDGET_TABLE(bMain, ALIGNMENT=1, SCR_XSIZE=xsz-10, XSIZE=200, YSIZE=N_ELEMENTS(headers.(1)), SCR_YSIZE=520, row_labels=headers.(1), COLUMN_WIDTHS=80)

  bCopy=WIDGET_BASE(bMain, /COLUMN, XSIZE=xsz-10)
  lblCopy=WIDGET_LABEL(bCopy, VALUE='Copy table to clipboard / append to result txt-file', FONT='Arial*Bold*18')
  bCopy2=WIDGET_BASE(bCopy, /ROW, FRAME=1, YSIZE=100)
  bBtnBtm=WIDGET_BASE(bCopy2, /ROW)
  mlC0=WIDGET_LABEL(bBtnBtm, VALUE='', xSIZE=10)
  cw_deciMark=CW_BGROUP(bBtnBtm, ['. (point)',', (comma)'], /EXCLUSIVE, LABEL_TOP='Decimal mark...', SET_VALUE=1, FRAME=1, UVALUE='deci')
  mlC=WIDGET_LABEL(bBtnBtm, VALUE='', xSIZE=20)
  bCopy3=WIDGET_BASE(bBtnBtm, /NONEXCLUSIVE, /COLUMN)
  btnTrans=WIDGET_BUTTON(bCopy3, VALUE='Transpose table when copied')
  btnHeaders=WIDGET_BUTTON(bCopy3, VALUE='Include headers')
  ;btnSort=WIDGET_BUTTON(bCopy3, VALUE='Sort by date when copied')
  WIDGET_CONTROL, btnTrans, /SET_BUTTON
  ;WIDGET_CONTROL, btnSort, /SET_BUTTON
  btnCopyTbl=WIDGET_BUTTON(bBtnBtm, VALUE='Copy table to clipboard', UVALUE='copyTbl', XSIZE=130)
  btnAppTbl=WIDGET_BUTTON(bBtnBtm, VALUE='Append txt-file...', UVALUE='appendTbl', XSIZE=100)
  blblApp=WIDGET_BASE(bBtnBtm, /COLUMN, /ALIGN_LEFT)
  lblApp=WIDGET_LABEL(blblApp, VALUE='Configure auto-append')
  lblApp2=WIDGET_LABEL(blblApp, VALUE='from the File-menu')

  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'getSiemensQC', bMain

end

pro getSiemensQC_event, event

  COMMON var

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval
  evTop=event.Top
  IF N_ELEMENTS(uval) GT 0 THEN BEGIN

    CASE uval OF

      'Exit': WIDGET_CONTROL, event.top, /DESTROY
      'addAppendTempPET': editAppTemp, GROUP_LEADER=event.top, 0
      'addAppendTempCT': editAppTemp, GROUP_LEADER=event.top, 1
      'clear': BEGIN
        WIDGET_CONTROL, tblRes, GET_VALUE=curTbl
        sz=SIZE(curTbl, /DIMENSIONS)
        WIDGET_CONTROL, tblRes, SET_VALUE=STRARR(sz(0),sz(1))
        WIDGET_CONTROL, tblRes, SET_TABLE_VIEW=[0,0]
      END
      'types':BEGIN
        WIDGET_CONTROL, cwType, GET_VALUE=type
        IF type NE curType THEN BEGIN
          WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
          empt=WHERE(curTbl[0,*] NE '')
          progress=0
          IF empt(0) NE -1 THEN BEGIN
            sv=DIALOG_MESSAGE('Change mode and loose current results?', /QUESTION, DIALOG_PARENT=evTop)
            IF sv EQ 'Yes' THEN progress=1 ELSE WIDGET_CONTROL, cwType, SET_VALUE=curType
          ENDIF ELSE progress=1
          IF progress THEN BEGIN
            lab=STRARR(n_elements(headers.(1)))
            lab[0:n_elements(headers.(type))-1]=headers.(type)
            WIDGET_CONTROL, tblRes, ROW_LABELS=lab
            WIDGET_CONTROL, tblRes, GET_VALUE=curTbl
            sz=SIZE(curTbl, /DIMENSIONS)
            WIDGET_CONTROL, tblRes, SET_VALUE=STRARR(sz(0),sz(1))
            curType=type
          ENDIF
        ENDIF
      END
      'deci':
      'readClip': BEGIN
        clipb=CLIPBOARD.GET()
        WIDGET_CONTROL, cwType, GET_VALUE=type
        CASE type OF
          0:BEGIN;PET
            resu=readPETdailyQC(clipb, config)
            strArrRes=resu.strArrRes
            IF strArrRes(0) EQ '' THEN sv=DIALOG_MESSAGE('Unexpected file content. Is this really a PET daily QC report?', DIALOG_PARENT=evTop)
            IF resu.errMsg NE '' THEN sv=DIALOG_MESSAGE(STRJOIN(resu.errMsg, newline)+newline+'Not reported in table.', DIALOG_PARENT=evTop)
            WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
            empt=WHERE(curTbl[*,0] EQ '')
            curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
            IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
            WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
          END
          1:BEGIN; CT constancy
            resu=readCTcons(clipb, config, '')
            strArrRes=resu.strArrRes
            IF resu.errMsg NE '' THEN sv=DIALOG_MESSAGE(resu.errMsg, DIALOG_PARENT=evTop)
            ;IF N_TAGS(structRes) EQ 1 THEN sv=DIALOG_MESSAGE('Found no results for the tests in this text.') ELSE BEGIN
            WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
            empt=WHERE(curTbl[*,0] EQ '')

            ;check size and empty?
            proceed=1
            IF N_ELEMENTS(strArrRes) NE N_ELEMENTS(headers.(curCTtype)) THEN BEGIN
              IF empt(0) EQ 0 THEN BEGIN
                IF curCTtype EQ 1 THEN curCTtype=2 ELSE curCTtype=1
                lab=STRARR(n_elements(headers.(1)))
                lab[0:n_elements(headers.(curCTtype))-1]=headers.(curCTtype)
                WIDGET_CONTROL, tblRes, ROW_LABELS=lab
              ENDIF ELSE BEGIN
                sv=DIALOG_MESSAGE('There are two result-types for CT. The type of this file differ from those already read. Empty table before reading result-files of a different type.', DIALOG_PARENT=evTop)
                proceed=0
              ENDELSE
            ENDIF

            IF proceed THEN BEGIN
              curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
              IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
              WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
            ENDIF
          END
        ENDCASE

      END

      'readTxtFiles':BEGIN
        adr=dialog_pickfile(/READ, /Multiple, Filter='*.txt', /FIX_FILTER, TITLE='Select report-file(s)', DIALOG_PARENT=evTop)
        IF adr(0) NE '' THEN BEGIN

          errArr=STRARR(N_ELEMENTS(adr))

          FOR i=0, N_ELEMENTS(adr)-1 DO BEGIN
            OPENR, lun, adr(i), /GET_LUN
            array=''
            line=''
            WHILE NOT EOF(lun) DO BEGIN
              READF, lun, line
              array=[array,line]
            ENDWHILE
            process=1
            IF N_ELEMENTS(array) GT 1 THEN array=array[1:N_ELEMENTS(array)-1] ELSE process=0
            FREE_LUN,lun

            IF process THEN BEGIN
              WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
              empt=WHERE(curTbl[*,0] EQ '')
  
              CASE curType OF
                0:BEGIN; PET
                  resu=readPETdailyQC(array, config)
                  strArrRes=resu.strArrRes
  
                  errArr(i)=STRJOIN(resu.errMsg,newline)
                  IF errArr(i) NE '' THEN errArr(i)=errArr(i)+newline+'Not reported in table.'
                  IF strArrRes(0) EQ '' THEN errArr(i)='Unexpected file content. Is this really a PET daily QC report?'
                  curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
                END
                1:BEGIN; CT constancy
                  resu=readCTcons(array, config, FILE_BASENAME(adr(i)))
                  strArrRes=resu.strArrRes
                  errArr(i)=resu.errMsg
  
                  ;check size and empty?
                  proceed=1
                  IF N_ELEMENTS(strArrRes) NE N_ELEMENTS(headers.(curCTtype)) THEN BEGIN
                    IF empt(0) EQ 0 THEN BEGIN
                      IF curCTtype EQ 1 THEN curCTtype=2 ELSE curCTtype=1
                      lab=STRARR(n_elements(headers.(1)))
                      lab[0:n_elements(headers.(curCTtype))-1]=headers.(curCTtype)
                      WIDGET_CONTROL, tblRes, ROW_LABELS=lab
                    ENDIF ELSE BEGIN
                      errArr(i)='File type differ from those already read. Empty table before reading result-files of a different type.'
                      proceed=0
                    ENDELSE
                  ENDIF
  
                  IF proceed THEN curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
                END
              ENDCASE
              IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
              WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
            ENDIF
          ENDFOR
          errs=WHERE(errArr NE '')
          IF errs(0) NE -1 THEN BEGIN

            errLogg=''
            FOR u=0, N_ELEMENTS(errs)-1 DO errLogg=errLogg+FILE_BASENAME(adr(errs(u)))+':'+newline + errArr(errs(u))+newline
            sv=DIALOG_MESSAGE(errLogg, DIALOG_PARENT=evTop)
          ENDIF
        ENDIF
        
      END

      'copyTbl': BEGIN

        WIDGET_CONTROL,tblRes, GET_VALUE=curTbl

        empt=WHERE(curTbl[*,0] EQ '')
        nparam=N_ELEMENTS(headers.(curType))
        curTbl=curTbl[0:empt(0)-1,0:nparam-1]

        szT=SIZE(curTbl, /DIMENSIONS)

        ;decimal mark
        WIDGET_CONTROL, cw_deciMark, GET_VALUE=deci
        IF deci EQ 1 THEN BEGIN
          FOR i=0, szT(0)-1 DO BEGIN
            FOR j=1, szT(1)-1 DO BEGIN
              curTbl[i,j]=STRJOIN(STRSPLIT(curTbl[i,j], '.',/EXTRACT),',')
            ENDFOR
          ENDFOR
        ENDIF

        ;headers?
        newTable=STRARR(szT(0)+1,szT(1))
        newTable[1:szT(0),*]=curTbl
        WIDGET_CONTROL, cwType, GET_VALUE=type
        IF type EQ 1 THEN tabno=curCTtype ELSE tabno=0
        newTable[0,0:N_ELEMENTS(headers.(tabno))-1]=TRANSPOSE(headers.(tabno))
        IF ~WIDGET_INFO(btnHeaders, /BUTTON_SET) THEN newTable=newTable[1:szT(0),*]

        ;transpose
        IF WIDGET_INFO(btnTrans, /BUTTON_SET) THEN newTable=TRANSPOSE(newTable)

        CLIPBOARD.set, STRJOIN(newTable, STRING(9B))
      END

      'appendTbl': BEGIN
        WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
        empt=WHERE(curTbl[*,0] EQ '')
        nparam=N_ELEMENTS(headers.(curType))
        curTbl=curTbl[0:empt(0)-1,0:nparam-1]

        ;find template corresponding to serial number (CT) or ICS Name (PET)
        appendTemp=!Null
        txtSerial=''
        resSerial=''
        structNames=TAG_NAMES(config)
        CASE curType OF
          0:BEGIN; PET
            IF structNames.HasValue('APPENDTEMPPET') THEN appendTemp=config.APPENDTEMPPET
            txtSerial='ICS Name'
            resSerial=curTbl(0,1)
            IF empt(0) GT 1 THEN BEGIN
              IF N_ELEMENTS(UNIQ(curTbl[*,1])) GT 1 THEN BEGIN
                sv=DIALOG_MESSAGE('Table contain results from more than one '+txtSerial+'. Automatic append cannot be performed.', DIALOG_PARENT=evTop)
                appendTemp=-1
              ENDIF
            ENDIF
          END
          1:BEGIN;CT
            IF structNames.HasValue('APPENDTEMPCT') THEN appendTemp=config.APPENDTEMPCT
            txtSerial='Serial Number'
            resSerial=curTbl(0,3)
            IF empt(0) GT 1 THEN BEGIN
              IF N_ELEMENTS(UNIQ(curTbl[*,3])) GT 1 THEN BEGIN
                sv=DIALOG_MESSAGE('Table contain results from more than one '+txtSerial+'. Automatic append cannot be performed.', DIALOG_PARENT=evTop)
                appendTemp=-1
              ENDIF
            ENDIF
          END
          ELSE:
        ENDCASE

        adrApp=''
        IF N_ELEMENTS(appendTemp) LE 1 THEN BEGIN
          IF appendTemp EQ !Null THEN sv=DIALOG_MESSAGE('Go to file menu to configure automatic append. ', DIALOG_PARENT=evTop)
        ENDIF ELSE BEGIN
          tempNmb=WHERE(appendTemp[1,*] EQ resSerial)
          IF tempNmb(0) NE -1 THEN adrApp=appendTemp[2,tempNmb] ELSE sv=DIALOG_MESSAGE('Found no templates corresponding to current '+txtSerial+' found in pdf-file. Go to file menu to configure automatic append.', DIALOG_PARENT=evTop)

          IF adrApp NE '' THEN BEGIN

            tempnames=TRANSPOSE(appendTemp[0,*])

            szT=SIZE(curTbl, /DIMENSIONS)

            ;decimal mark
            WIDGET_CONTROL, cw_deciMark, GET_VALUE=deci
            IF deci EQ 1 THEN BEGIN
              FOR i=0, szT(0)-1 DO BEGIN
                FOR j=1, szT(1)-1 DO BEGIN
                  curTbl[i,j]=STRJOIN(STRSPLIT(curTbl[i,j], '.',/EXTRACT),',')
                ENDFOR
              ENDFOR
            ENDIF

            ;headers?
            newTable=STRARR(szT(0)+1,szT(1))
            newTable[1:szT(0),*]=curTbl
            WIDGET_CONTROL, cwType, GET_VALUE=type
            IF type EQ 1 THEN tabno=curCTtype ELSE tabno=0
            newTable[0,0:N_ELEMENTS(headers.(tabno))-1]=TRANSPOSE(headers.(tabno))
            IF ~WIDGET_INFO(btnHeaders, /BUTTON_SET) THEN newTable=newTable[1:szT(0),*]

            ;always transpose
            newTable=TRANSPOSE(newTable)

            CLIPBOARD.set, STRJOIN(newTable, STRING(9B))

            OPENW, resfile, adrApp, /APPEND, /GET_LUN
            PRINTF, resfile, CLIPBOARD.GET()
            CLOSE, resfile & FREE_LUN, resfile
            
            sv=DIALOG_MESSAGE('Results appended to file. Table will be cleared.', /INFORMATION)
            WIDGET_CONTROL, tblRes, GET_VALUE=curTbl
            sz=SIZE(curTbl, /DIMENSIONS)
            WIDGET_CONTROL, tblRes, SET_VALUE=STRARR(sz(0),sz(1))
            WIDGET_CONTROL, tblRes, SET_TABLE_VIEW=[0,0]

          ENDIF
        ENDELSE
      END

      ELSE:
    ENDCASE
  ENDIF

  ;******************* Exit program ***********************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
  ENDIF

  ;******************* Move on screen ***************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TLB_MOVE' THEN BEGIN
    xoffset=event.x
    yoffset=event.y
  ENDIF
end