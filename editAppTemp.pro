
;getSiemensQC - extraction of figures to table from Siemens QC reports (CT constancy/daily and PET daily QC)
;Copyright (C) 2019  Ellen Wasbo, Stavanger University Hospital, Norway
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

pro editAppTemp, GROUP_LEADER = mainbase, modePETCT
  COMMON Edit, tblTemp, mode
  COMMON var

  mode=modePETCT
  modeText=''
  col2Text=''
  appendTempPET=!Null
  appendTempCT=!Null
  CASE mode of
  0: BEGIN  
    structNames=TAG_NAMES(config)
    IF structNames.HasValue('APPENDTEMPPET') THEN BEGIN
      appendTempPET=config.APPENDTEMPPET
      IF N_ELEMENTS(appendTempPET) EQ 1 THEN appendTempPET=!Null
    ENDIF
    modeText='PET'
    col2Text='ICS Name'
    END
  1: BEGIN
    structNames=TAG_NAMES(config)
    IF structNames.HasValue('APPENDTEMPCT') THEN BEGIN
      appendTempCT=config.APPENDTEMPCT
      IF N_ELEMENTS(appendTempCT) EQ 1 THEN appendTempCT=!Null
    ENDIF
    modeText='CT'
    col2Text='Serial Number'
    END
  ELSE:
  ENDCASE

  editAppTempbox = WIDGET_BASE(TITLE='Edit/manage append templates for '+modeText+' results ',  $
    /COLUMN, XSIZE=800, YSIZE=400, GROUP_LEADER=mainbase, /MODAL)

  tblTemp=WIDGET_TABLE(editAppTempbox, ALIGNMENT=1, SCR_XSIZE=770, XSIZE=3, YSIZE=100, SCR_YSIZE=300, column_labels=['LabName',col2Text,'txtFileAddress'], COLUMN_WIDTHS=[80,80,550], /EDITABLE)

  CASE mode of
    0: BEGIN
      IF appendTempPET NE !Null THEN BEGIN
        szAT=SIZE(appendTempPET, /DIMENSIONS)
        appTable=STRARR(3,100)
        IF N_ELEMENTs(szAT) EQ 1 THEN appTable[*,0]=appendTempPET ELSE appTable[*,0:szAT(1)-1]=appendTempPET
        WIDGET_CONTROL, tblTemp, SET_VALUE=appTable
      ENDIF
    END
    1: BEGIN
      IF appendTempCT NE !Null THEN BEGIN
        szAT=SIZE(appendTempCT, /DIMENSIONS)
        appTable=STRARR(3,100)
        IF N_ELEMENTs(szAT) EQ 1 THEN appTable[*,0]=appendTempCT ELSE appTable[*,0:szAT(1)-1]=appendTempCT
        WIDGET_CONTROL, tblTemp, SET_VALUE=appTable
      ENDIF
    END
    ELSE:
  ENDCASE

  bButtons=WIDGET_BASE(editAppTempbox, /ROW)
  lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=50)
  btnSetCurr=WIDGET_BUTTON(bButtons, VALUE='Import from clipboard', UVALUE='e_import',FONT=font1)
  btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Export to clipboard', UVALUE='e_export', FONT=font1)
  lblBtns1=WIDGET_LABEL(bButtons, VALUE='', XSIZE=50)
  btnSetCurr=WIDGET_BUTTON(bButtons, VALUE='Save', UVALUE='e_save',FONT=font1)
  btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Cancel', UVALUE='e_cancel', FONT=font1)

  WIDGET_CONTROL, editAppTempbox, /REALIZE
  XMANAGER, 'editAppTemp', editAppTempbox

end

pro editAppTemp_event, event

  COMMON Edit
  COMMON VAR

  WIDGET_CONTROL, event.ID, GET_UVALUE=uval
  eevTop=event.Top
  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'e_save':BEGIN
        WIDGET_CONTROL, tblTemp, GET_VALUE=curTbl
        ;no-empty rows in first second column similar 
        empt1=WHERE(curTbl[0,*] EQ '')
        empt3=WHERE(curTbl[2,*] EQ '')
        empt2=WHERE(curTbl[1,*] EQ '')
        IF ~ARRAY_EQUAL(empt1, empt3) THEN sv=DIALOG_MESSAGE('Some templates are missing address or som address is missing LabName. Correct this.', DIALOG_PARENT=eevTop) ELSE BEGIN
          IF ~ARRAY_EQUAL(empt1, empt2) THEN sv=DIALOG_MESSAGE('Some templates are missing serial number of the CT. Correct this.', DIALOG_PARENT=eevTop) ELSE BEGIN
            ;save active part of table
            nonEmpt=WHERE(curTbl[0,*] NE '')
            curTbl=curTbl[*,nonEmpt]
            
            oldAppendTempPET=-1
            oldAppendTempCT=-1
            structNames=TAG_NAMES(config)
            IF structNames.HasValue('APPENDTEMPCT') THEN oldAppendTempCT=config.APPENDTEMPCT
            IF structNames.HasValue('APPENDTEMPPET') THEN oldAppendTempPET=config.APPENDTEMPPET
            
            CASE mode OF
              0:config=CREATE_STRUCT('PET',config.PET,'CT',config.CT,'months',config.months,'APPENDTEMPCT', oldAppendTempCT, 'APPENDTEMPPET',curTbl)
              1:config=CREATE_STRUCT('PET',config.PET,'CT',config.CT,'months',config.months,'APPENDTEMPCT', curTbl, 'APPENDTEMPPET', oldAppendTempPET)
              ELSE:
            ENDCASE
              
            thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('getSiemensQC'))+'\'
            SAVE, config, FILENAME=thisPath+'config.dat'
            WIDGET_CONTROL, Event.top, /DESTROY
          ENDELSE
        ENDELSE      

      END
      'e_export': BEGIN
        WIDGET_CONTROL, tblTemp, GET_VALUE=curTbl
        CLIPBOARD.set, STRJOIN(curTbl, STRING(9B))
        END
      
      'e_import':BEGIN
          clipres=CLIPBOARD.GET()
          nRows=N_ELEMENTS(clipRes)
          IF nRows GT 0 THEN BEGIN
            WIDGET_CONTROL, tblTemp, GET_VALUE=curTbl
            nonEmpt=WHERE(curTbl[0,*] NE '', nAlready)
            testNcols=STRSPLIT(clipres(0),STRING(9B),/EXTRACT)
            newTbl=curTbl
            IF N_ELEMENTS(testNcols) EQ 3 THEN BEGIN
              IF nAlready GE 1 THEN BEGIN
                sv=DIALOG_MESSAGE('Overwrite current table?', /QUESTION, DIALOG_PARENT=eevTop)
                IF sv EQ 'Yes' THEN nAlready = 0
              ENDIF
              FOR i=0, nRows-1 DO newTbl[*,i+nAlready]=STRSPLIT(STRJOIN(STRSPLIT(clipres(i),',',/EXTRACT),'.'),STRING(9B),/EXTRACT)
              WIDGET_CONTROL, tblTemp, SET_VALUE=newTbl
            ENDIF ELSE sv=DIALOG_MESSAGE('Wrong format. Expecting 3 columns', DIALOG_PARENT=eevTop)
          ENDIF
        END

      'e_cancel': WIDGET_CONTROL, Event.top, /DESTROY
      ELSE:
    ENDCASE
  ENDIF

end
