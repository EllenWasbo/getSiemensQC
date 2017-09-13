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

  COMMON VAR, config, langu, tblRes, headers, colWids, cwType, curType, cw_deciMark, btnTrans, btnSort, btnHeaders, newline

  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('getSiemensQC'))+'\'
  RESTORE, thisPath+'config.dat'
  
  if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

  months=config.months;['Januray','February','March','April','May','June','July','August','September','October','November','December']

  langu=0;english default

  xsz=700

  bMain = WIDGET_BASE(TITLE='Extracting info from Siemens QC reports PET and CT', MBAR=bar, /COLUMN, XSIZE=xsz, YSIZE=800, XOFFSET=50, YOFFSET=50,/TLB_KILL_REQUEST_EVENTS)

  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  btn_clear=WIDGET_BUTTON(file_menu, VALUE='Clear table', UVALUE='clear')
  btn_exit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='Exit', ACCELERATOR='Ctrl+X', /SEPARATOR)

  ml0=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)

  bTop=WIDGET_BASE(bMain, /ROW)
  bRead=WIDGET_BASE(bTop, /COLUMN, XSIZE=xsz-10)
  lblRead=WIDGET_LABEL(bRead, VALUE='Read report', FONT='Arial*Bold*18')
  bRead2=WIDGET_BASE(bRead, /ROW, FRAME=1, YSIZE=100)
  mlR=WIDGET_LABEL(bRead2, VALUE='', XSIZE=120)
  cwType=CW_BGROUP(bRead2,['PET','CT'], /EXCLUSIVE, LABEL_TOP='Report type...', SET_VALUE=1,FRAME=1, UVALUE='types')
  curType=1
  mlA=WIDGET_LABEL(bRead2, VALUE='', XSIZE=30)
  btnReadClip=WIDGET_BUTTON(bRead2, VALUE='Read from clipboard', UVALUE='readClip', XSIZE=200)
  mlA1=WIDGET_LABEL(bRead2, VALUE='', XSIZE=30)
  bInstructions=WIDGET_BASE(bRead2, /COLUMN, /ALIGN_RIGHT)
  inst0=WIDGET_LABEL(bInstructions, VALUE='Instructions', XSIZE=150, FONT='Arial*Bold*12')
  inst1=WIDGET_LABEL(bInstructions, VALUE='* Open a report (pdf or txt)', XSIZE=150)
  inst2=WIDGET_LABEL(bInstructions, VALUE='* Select all text (Ctrl+A)', XSIZE=150)
  inst3=WIDGET_LABEL(bInstructions, VALUE='* Copy to clipboard (Ctrl+C)', XSIZE=150)
  inst4=WIDGET_LABEL(bInstructions, VALUE='* Press "Read from clipboard"', XSIZE=150)
  inst5=WIDGET_LABEL(bInstructions, VALUE='* Repeat for all reports', XSIZE=150)
  mlT=WIDGET_LABEL(bRead, VALUE='', YSIZE=10)
  btnReadExcel=WIDGET_BUTTON(bRead, VALUE='Read from txt files (automated)', UVALUE='readTxtFiles', XSIZE=200)

  ml1=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)

  headersPET=['Date','Partial','Full',$
    'Time Align','Calib Factor','Measured Randoms','Scanner Efficiency',$
    'Scatter Ratio','ECF','Time Alignment Residual', 'Time Alignment fit x','Time Alignment fit y',$
    'Phantom Pos x', 'Phantom Pos y']
  headersCT=['Date','Tester name','Product Name','Serial Number',$
    'HUwater head min','HUwater head max','HUwater body min','HUwater body max',$
    'Diff head max(abs)','Diff body max(abs)',$
    'Noise head max','Noise body max',$
    'Slice head min','Slice head max','Slice body min','Slice body max',$
    'MTF50 B30f','MTF10 B30f','MTF50 H30s','MTF10 H30s','MTF50 H70h','MTF10 H70h']
  headers=CREATE_STRUCT('PET',headersPET,'CT', headersCT)
  tblRes=WIDGET_TABLE(bMain, ALIGNMENT=1, SCR_XSIZE=xsz-10, XSIZE=200, YSIZE=N_ELEMENTS(headers.(1)), SCR_YSIZE=450, row_labels=headers.(1), COLUMN_WIDTHS=80)

  bCopy=WIDGET_BASE(bMain, /COLUMN, XSIZE=xsz-10)
  lblCopy=WIDGET_LABEL(bCopy, VALUE='Copy table to clipboard', FONT='Arial*Bold*18')
  bCopy2=WIDGET_BASE(bCopy, /ROW, FRAME=1, YSIZE=100)
  bBtnBtm=WIDGET_BASE(bCopy2, /ROW)
  mlC0=WIDGET_LABEL(bBtnBtm, VALUE='', xSIZE=100)
  cw_deciMark=CW_BGROUP(bBtnBtm, ['. (point)',', (comma)'], /EXCLUSIVE, LABEL_TOP='Decimal mark...', SET_VALUE=1, FRAME=1, UVALUE='deci')
  mlC=WIDGET_LABEL(bBtnBtm, VALUE='', xSIZE=20)
  bCopy3=WIDGET_BASE(bBtnBtm, /NONEXCLUSIVE, /COLUMN)
  btnTrans=WIDGET_BUTTON(bCopy3, VALUE='Transpose table when copied')
  btnHeaders=WIDGET_BUTTON(bCopy3, VALUE='Include headers')
  btnSort=WIDGET_BUTTON(bCopy3, VALUE='Sort by date when copied')
  WIDGET_CONTROL, btnTrans, /SET_BUTTON
  WIDGET_CONTROL, btnSort, /SET_BUTTON
  btnCopyTbl=WIDGET_BUTTON(bBtnBtm, VALUE='Copy table to clipboard', UVALUE='copyTbl', XSIZE=200)

  WIDGET_CONTROL, bMain, /REALIZE
  XMANAGER, 'getSiemensQC', bMain

end

pro getSiemensQC_event, event

  COMMON var

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN

    CASE uval OF

      'Exit': WIDGET_CONTROL, event.top, /DESTROY
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
            sv=DIALOG_MESSAGE('Change mode and loose current results?', /QUESTION)
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
            IF strArrRes(0) EQ '' THEN sv=DIALOG_MESSAGE('Unexpected file content. Is this really a PET daily QC report?') 
            IF resu.errMsg NE '' THEN sv=DIALOG_MESSAGE(STRJOIN(resu.errMsg, newline)+newline+'Not reported in table.')
            WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
            empt=WHERE(curTbl[*,0] EQ '')
            curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
            IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
            WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
          END
          1:BEGIN; CT constancy
            resu=readCTcons(clipb, config)
            strArrRes=resu.strArrRes
            IF resu.errMsg NE '' THEN sv=DIALOG_MESSAGE(resu.errMsg)
            ;IF N_TAGS(structRes) EQ 1 THEN sv=DIALOG_MESSAGE('Found no results for the tests in this text.') ELSE BEGIN
            WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
            empt=WHERE(curTbl[*,0] EQ '')
            curTbl[empt(0),*]=TRANSPOSE(strArrRes)
            IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
            WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
          END
        ENDCASE

      END

      'readTxtFiles':BEGIN
        adr=dialog_pickfile(/READ, /Multiple, Filter='*.txt', /FIX_FILTER, TITLE='Select report-file(s)')
        IF adr(0) NE '' THEN BEGIN

          errArr=STRARR(N_ELEMENTS(adr))
          WIDGET_CONTROL, cwType, GET_VALUE=type

          FOR i=0, N_ELEMENTS(adr)-1 DO BEGIN
            OPENR, lun, adr(i), /GET_LUN
            array=''
            line=''
            WHILE NOT EOF(lun) DO BEGIN
              READF, lun, line
              array=[array,line]
            ENDWHILE
            array=array[1:N_ELEMENTS(array)-1]
            FREE_LUN,lun
            
            WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
            empt=WHERE(curTbl[*,0] EQ '')

            CASE type OF
              0:BEGIN; PET
                resu=readPETdailyQC(array, config)
                strArrRes=resu.strArrRes
                
                errArr(i)=STRJOIN(resu.errMsg,newline)
                IF errArr(i) NE '' THEN errArr(i)=errArr(i)+newline+'Not reported in table.'
                IF strArrRes(0) EQ '' THEN errArr(i)='Unexpected file content. Is this really a PET daily QC report?'
                curTbl[empt(0),0:N_ELEMENTS(strArrRes)-1]=TRANSPOSE(strArrRes)
              END
              1:BEGIN; CT constancy
                resu=readCTcons(array, config)
                strArrRes=resu.strArrRes
                errArr(i)=resu.errMsg
                curTbl[empt(0),*]=TRANSPOSE(strArrRes)
              END
            ENDCASE
            IF empt(0) GT 6 THEN tabView=[empt(0)-6,0] ELSE tabView=[0,0]
            WIDGET_CONTROL, tblRes, SET_VALUE=curTbl, SET_TABLE_VIEW=tabView
            
          ENDFOR
          errs=WHERE(errArr NE '')
          IF errs(0) NE -1 THEN BEGIN
            
            errLogg=''
            FOR u=0, N_ELEMENTS(errs)-1 DO errLogg=errLogg+FILE_BASENAME(adr(errs(u)))+':'+newline + errArr(errs(u))+newline
            sv=DIALOG_MESSAGE(errLogg)
          ENDIF
        ENDIF
      END

      'copyTbl': BEGIN

        WIDGET_CONTROL,tblRes, GET_VALUE=curTbl
        
        empt=WHERE(curTbl[*,0] EQ '')
        curTbl=curTbl[0:empt(0)-1,*]
        szT=SIZE(curTbl, /DIMENSIONS)

        ;sort by date? format dd.mm.yyyy
        IF WIDGET_INFO(btnSort, /BUTTON_SET) THEN BEGIN
          dateStrings=curTbl[*,0]
          dateId=INTARR(szT(0))
          FOR i=0, szT(0)-1 DO BEGIN
            dmy=STRSPLIT(dateStrings(i),'.',/EXTRACT)
            dmy=LONG(dmy)
            dateId(i)=JULDAY(dmy(1), dmy(0), dmy(2))    ;month,day,year
          ENDFOR
          sorted=SORT(dateID)
          FOR j=0, szT(1)-1 DO BEGIN
            temp=curTbl[*,j]
            curTbl[*,j]=temp(sorted)
          ENDFOR
        ENDIF

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
        newTable[0,0:N_ELEMENTS(headers.(type))-1]=TRANSPOSE(headers.(type))
        IF ~WIDGET_INFO(btnHeaders, /BUTTON_SET) THEN newTable=newTable[1:szT(0),*]

        ;transpose
        IF WIDGET_INFO(btnTrans, /BUTTON_SET) THEN newTable=TRANSPOSE(newTable)

        CLIPBOARD.set, STRJOIN(newTable, STRING(9B))
      END

      ELSE:
    ENDCASE
  ENDIF

  ;******************* Exit program ***********************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
  ENDIF
end