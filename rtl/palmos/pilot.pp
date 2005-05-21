{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by H.M. Swartjes
    Parts Copyright (c) 1999-2000 by the Free Pascal development team

    Interface unit for PalmOS calls

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Pilot;

interface
   uses
      SysTraps;

   const

      BTRUE = 256;
      BFALSE = 0;
{events.h}
      nilEvent = 0;
      penDownEvent = 1;
      penUpEvent = 2;
      penMoveEvent = 3;
      keyDownEvent = 4;
      winEnterEvent = 5;
      winExitEvent = 6;
      ctlEnterEvent = 7;
      ctlExitEvent = 8;
      ctlSelectEvent = 9;
      ctlRepeatEvent = 10;
      lstEnterEvent = 11;
      lstSelectEvent = 12;
      lstExitEvent = 13;
      popSelectEvent = 14;
      fldEnterEvent = 15;
      fldHeightChangedEvent = 16;
      fldChangedEvent = 17;
      tblEnterEvent = 18;
      tblSelectEvent = 19;
      daySelectEvent = 20;
      menuEvent = 21;
      appStopEvent = 22;
      frmLoadEvent = 23;
      frmOpenEvent = 24;
      frmGotoEvent = 25;
      frmUpdateEvent = 26;
      frmSaveEvent = 27;
      frmCloseEvent = 28;
      frmTitleEnterEvent = 29;
      frmTitleSelectEvent = 30;
      tblExitEvent = 31;
      sclEnterEvent = 32;
      sclExitEvent = 33;
      sclRepeatEvent = 34;

{table.h}
      checkboxTableItem = 0;
      customTableItem = 256;
      dateTableItem = 512;
      labelTableItem = 768;
      numericTableItem = 1024;
      popupTriggerTableItem = 1280;
      textTableItem = 1536;
      textWithNoteTableItem = 2048;
      timeTableItem = 2303;

      AsmTrap = $4e4f;
      AsmCleanup = $4fef;
      AsmPushInteger = $3e80;
      AsmPushLong = $2e80;
      AsmPushPtr = $2e88;

      NinetySix = -1391668096; { This is the number of seconds on January 1, 1996. Think Pascal doesn't }
{ know about unsigned longints, so I use this value regularly to put theSeconds in the positive range. }

   type
      Handle = ^Pointer;
      UInt = word;
      OsType = UInt;

      Cstring = packed array[1..32] of char;
      Cstr16 = packed array[1..16] of char;
      Cstr8 = packed array[1..8] of char;
      PString = packed record
            case boolean of
               true: (
                     p: String;
               );
               false: (
                     c: array[0..255] of char
               );
         end;

      DmSearchStateType = packed array[1..8] of Integer;

      SystemPreferencesType = packed record
            version: Integer;
            country: byte;
            dateFormat: byte;
            longDateFormat: byte;
            weekStartDay: byte;
            timeFormat: byte;
            numberFormat: byte;
            autoOffDuration: byte;
            sysSoundLevel: byte;
            alarmSoundLevel: byte;
            hideSecretRecords: byte;
            deviceLocked: byte;
            sysPrefFlags: Integer;
            sysBatteryKind: byte;
         end;

      RectangleType = record
            left: Integer;
            top: Integer;
            width: Integer;
            Height: Integer;
         end;

      DateTimeType = packed record
            second: Integer;
            minute: Integer;
            hour: Integer;
            day: Integer;
            month: Integer;
            year: Integer;
            weekDay: Integer;
         end;

      TimeType = packed record
            hours: Byte;
            Minues: Byte;
         end;

      SysAppInfoType = packed record
            cmd: Integer;
            cmdPBP: Ptr;
            launchFlags: Integer;
            taskID: Longint;
            codeH: Handle;
            dbP: Integer;
            stackP: Ptr;
            globalsChunkP: Ptr;
            memOwnerID: Integer;
            dmAccessP: Ptr;
            dmLastErr: Integer;
            errExceptionP: Ptr
         end;

      SysAppInfoPtr = ^SySAppInfoType;

      ListType = packed record
            id: integer;
            Left: Integer;
            Top: Integer;
            Width: Integer;
            Height: Integer;
            attr: byte;
            padding: byte;
            itemsText: longint;
            numItems: integer;
            currentItem: integer;
            topItem: integer;
            font: integer;
            popupWin: longint;
            drawItemsCallback: longint;
         end;

      ListPtr = ^ListType;

      ControlType = packed record
            id: integer;
            Left: Integer;
            Top: Integer;
            Width: Integer;
            Height: Integer;
            text: Longint;
            attr: integer;
            style: byte;
            font: byte;
            group: byte;
            padding: byte;
         end;

      ControlPtr = ^ControlType;

      eventType = packed record
            eType: Integer;
            penDown: Boolean;
            screenX: Integer;
            screenY: Integer;
            case Integer of
               0: (
                     data1, data2, data3, data4, data5, data6, data7, data8: Integer
               );
               penUpEvent: (
                     startX, startY, endX, endY: Integer
               );
               ctlSelectEvent: (
                     controlID: Integer;
                     pControl: Ptr;
                     on: Boolean
               );
               menuEvent: (
                     itemID: Integer;
               );
               frmLoadEvent: (
                     formID: Integer
               );
               lstEnterEvent: (
                     listID: Integer;
                     pList: Ptr;
                     selection: Integer;
               );
               tblSelectEvent: (
                     tableID: Integer;
                     pTable: Ptr;
                     row: Integer;
                     column: Integer;
               );
               popSelectEvent: (
                     popControlID: Integer;
                     controlP: Ptr;
                     popListID: Integer;
                     listP: Ptr;
                     popSelection: Integer;
                     priorSelection: Integer;
               );
               keyDownEvent: (
                     chr: Integer;
                     keyCode: Integer;
                     modifiers: Integer;
               );
               sclEnterEvent, sclExitEvent, sclRepeatEvent: (
                     scrollBarID: Integer;
                     scrollBar: Ptr;
                     value, newValue: Integer;
                     time: LongInt;
               );
         end;

      CustomPatternType = array[1..2] of LongInt;

{ ------------- Alm ------------- }

   function AlmGetAlarm (var ref: LongInt; id: LongInt; card: Integer): Longint;
     syscall SysTrapAlmGetAlarm;

   procedure AlmSetAlarm (quiet: Integer; theSeconds: Longint; ref: LongInt; id: Longint; card: Integer);
      syscall SysTrapAlmSetAlarm;

{ ------------- Ctl ------------- }
   function CtlGetLabel (ControlP: Ptr): Ptr;
     syscall SysTrapCtlGetLabel;

   function CtlGetValue (ControlP: Ptr): Integer;
      syscall SysTrapCtlGetValue;

   procedure CtlHideControl (ControlP: Ptr);
      syscall SysTrapCtlHideControl;

   procedure CtlSetLabel (newLabel: Ptr; ControlP: Ptr);
      syscall SysTrapCtlSetLabel;

   procedure CtlSetUsable (status: Integer; ControlP: Ptr);
      syscall SysTrapCtlSetUsable;

   procedure CtlSetValue (newValue: Integer; ControlP: Ptr);
      syscall SysTrapCtlSetValue;

   procedure CtlShowControl (ControlP: Ptr);
      syscall SysTrapCtlShowControl;

{ ------------- Dm ------------- }

   procedure DmCloseDatabase (db: Ptr);
      syscall SysTrapDmCloseDatabase;

   function DmCreateDatabase (resDB: Integer; theType, theCreator: OSType; theName: Ptr; cardNo: Integer): Integer;
      syscall SysTrapDmCreateDatabase;

   function DmDatabaseInfo (theCreator, theType, sortInfoID, appInfoID, modNum, bckUpDate, modDate, crDate, version, attributes, theName: Ptr; dbID: LongInt; cardNo: Integer): Integer;
      syscall SysTrapDmDatabaseInfo;

   function DmDeleteDatabase (localID: LongInt; cardNo: Integer): Integer;
      syscall SysTrapDmDeleteDatabase;

   procedure DmDeleteRecord (index: Integer; db: Ptr);
      syscall SysTrapDmDeleteRecord;

   function DmFindDatabase (theName: Ptr; cardNo: Integer): LongInt;
      syscall SysTrapDmFindDatabase;

   function DmGetNextDatabaseByTypeCreator (var dbID: LongInt; var cardNo: Integer; onlyLatestVersion: Integer; creator, myType: OSType; stateInfo: DmSearchStateType; newSearch: Integer): Integer;
      syscall SysTrapDmGetNextDatabaseByTypeCreator;

   function DmGetRecord (index: Integer; db: Ptr): Ptr;
      syscall SysTrapDmGetRecord;

   function DmGet1Resource (ID: Integer; ResType: OSType): Ptr;
      syscall SysTrapDmGet1Resource;

   function DmNewRecord (size: Longint; var at: Integer; db: Ptr): Ptr;
      syscall SysTrapDmNewRecord;

   function DmNumRecords (db: Ptr): Integer;
      syscall SysTrapDmNumRecords;

   function DmNumRecordsInCategory (category: Integer; db: Ptr): Integer;
      syscall SysTrapDmNumRecordsInCategory;

   function DmOpenDatabase (mode: Integer; dbID: Ptr; cardNo: Integer): Ptr;
      syscall SysTrapDmOpenDatabase;

   function DmOpenDatabaseByTypeCreator (mode: Integer; theCreator, theType: OSType): Ptr;
      syscall SysTrapDmOpenDatabaseByTypeCreator;

{mode=1:read, 2:write; 3:readWrite}

   function DmOpenDatabaseInfo (resDB, cardNo, mode, openCount, dbID, db: Ptr): Integer;
      syscall SysTrapDmOpenDatabaseInfo;

   function DmQueryNextInCategory (category: Integer; var index: Integer; db: Ptr): Ptr;
      syscall SysTrapDmQueryNextInCategory;

   function DmQueryRecord (index: Integer; db: Ptr): Ptr;
      syscall SysTrapDmQueryRecord;

   function DmRecordInfo (localIDP, chunkIDP: Ptr; var attr: Integer; index: Integer; db: Ptr): Integer;
      syscall SysTrapDmRecordInfo;

   procedure DmReleaseRecord (dirty, index: Integer; db: Ptr);
      syscall SysTrapDmReleaseRecord;

   procedure DmRemoveRecord (index: Integer; db: Ptr);
      syscall SysTrapDmRemoveRecord;

   function DmSeekRecordInCategory (Category, Direction, Offset: Integer; var Index: Integer; dbRef: Ptr): Integer;
      syscall SysTrapDmSeekRecordInCategory;

   function DmSet (byteValue: Integer; bytes, offset: LongInt; recordP: Ptr): Integer;
      syscall SysTrapDmSet;

   function DmSetDatabaseInfo (theCreator, theType, sortInfoID, appInfoID, modNum, bckUpDate, modDate, crDate, version, attributes, theName: Ptr; dbID: LongInt; cardNo: Integer): Integer;
      syscall SysTrapDmSetDatabaseInfo;

   function DmSetRecordInfo (localIDP: Ptr; var attr: Integer; index: Integer; db: Ptr): Integer;
      syscall SysTrapDmSetRecordInfo;

   function DmWrite (bytes: LongInt; src: Ptr; offset: LongInt; recordP: Ptr): Integer;
      syscall SysTrapDmWrite;

{ ------------- Evt ------------- }

   procedure EvtGetEvent (timeOut: LongInt; var event: EventType);
      syscall SysTrapEvtGetEvent;

   procedure EvtFlushPenQueue;
      syscall SysTrapEvtFlushPenQueue;

{ ------------- Fld ------------- }

   procedure FldCopy (fld: Ptr);
      syscall SysTrapFldCopy;

   procedure FldCut (fld: Ptr);
      syscall SysTrapFldCut;

   procedure FldDelete (last, first: Integer; fld: Ptr);
      syscall SysTrapFldDelete;

   procedure FldEraseField (fld: Ptr);
      syscall SysTrapFldEraseField;

   function FldGetTextHandle (fld: Ptr): Ptr;
      syscall SysTrapFldGetTextHandle;

   function FldGetTextLength (fld: Ptr): Integer;
      syscall SysTrapFldGetTextLength;

   function FldGetTextPtr (fld: Ptr): Ptr;
      syscall SysTrapFldGetTextPtr;

   procedure FldGrabFocus (fld: Ptr);
      syscall SysTrapFldGrabFocus;

   function FldInsert (insertLen: Integer; insertChars, FieldPtr: Ptr): Integer;
      syscall SysTrapFldInsert;

   procedure FldPaste (fld: Ptr);
      syscall SysTrapFldPaste;

   procedure FldSetInsPtPosition (pos: Integer; fld: Ptr);
      syscall SysTrapFldSetInsPtPosition;

   procedure FldSetTextPtr (textPtr, fld: Ptr);
      syscall SysTrapFldSetTextPtr;

   procedure FldSetTextHandle (textHandle, fld: Ptr);
      syscall SysTrapFldSetTextHandle;

   procedure FldSetSelection (endPosition: Integer; startPosition: Integer; fld: Ptr);
      syscall SysTrapFldSetSelection;

   procedure FldUndo (fld: Ptr);
      syscall SysTrapFldUndo;

{ ------------- Fnt ------------- }

   procedure FntCharsInWidth (fit, textLen, width, recText: Ptr);
      syscall SysTrapFntCharsInWidth;

   function FntCharsWidth (theLength: Integer; theString: Ptr): Integer;
      syscall SysTrapFntCharsWidth;

   function FntDefineFont (fontP: Ptr; FontID: Integer): Integer;
      syscall SysTrapFntDefineFont;

   function FntSetFont (fontID: Integer): Integer;
      syscall SysTrapFntSetFont;

{ ------------- Frm ------------- }

   function FrmAlert (alertID: Integer): Integer;
      syscall SysTrapFrmAlert;

   procedure FrmCloseAllForms;
      syscall SysTrapFrmCloseAllForms;

   procedure FrmDeleteForm (frm: Ptr);
      syscall SysTrapFrmDeleteForm;

   procedure FrmDoDialog (theForm: Ptr);
      syscall SysTrapFrmDoDialog;

   procedure FrmDrawForm (theForm: Ptr);
      syscall SysTrapFrmDrawForm;

   function FrmGetActiveForm: Ptr;
      syscall SysTrapFrmGetActiveForm;

   function FrmGetActiveFormID: Integer;
      syscall SysTrapFrmGetActiveFormID;

   function FrmGetFocus (theForm: Ptr): Integer;
      syscall sysTrapFrmGetFocus;

   function FrmGetFormPtr (id: Integer): Ptr;
      syscall SysTrapFrmGetFormPtr;

   function FrmGetObjectID (ObjIndex: Integer; frm: Ptr): Integer;
      syscall SysTrapFrmGetObjectID;

   function FrmGetObjectIndex (ObjID: Integer; frm: Ptr): Integer;
      syscall SysTrapFrmGetObjectIndex;

   function FrmGetObjectPtr (ObjIndex: Integer; frm: Ptr): Ptr;
      syscall SysTrapFrmGetObjectPtr;

   procedure FrmGotoForm (frmID: integer);
      syscall SysTrapFrmGotoForm;

   function FrmHandleEvent (event: eventType; frm: Ptr): Integer;
      syscall SysTrapFrmHandleEvent;

   procedure FrmHideObject (objIndex: Integer; frm: Ptr);
      syscall SysTrapFrmHideObject;

   function FrmInitForm (frmID: integer): Ptr;
      syscall SysTrapFrmInitForm;

   procedure FrmPopupForm (frmID: integer);
      syscall SysTrapFrmPopupForm;

   procedure FrmReturnToForm (frmID: integer);
      syscall SysTrapFrmReturnToForm;

   procedure FrmSetActiveForm (theForm: Ptr);
      syscall SysTrapFrmSetActiveForm;

   procedure FrmSetFocus (fieldIndex: Integer; frm: Ptr);
      syscall sysTrapFrmSetFocus;

   procedure FrmShowObject (objIndex: Integer; frm: Ptr);
      syscall SysTrapFrmShowObject;

{ ------------- Grf ------------- }

   procedure GrfSetState (upperShift, numLock, capsLock: Integer);
      syscall sysTrapGrfSetState;

{ ------------- Lst ------------- }

   function LstGetSelection (theList: Ptr): Integer;
      syscall SysTrapLstGetSelection;

   function LstGetSelectionText (itemNum: Integer; theList: Ptr): Ptr;
      syscall SysTrapLstGetSelectionText;

   procedure LstSetHeight (visibleItems: Integer; theList: Ptr);
      syscall SysTrapLstSetHeight;

   procedure LstSetListChoices (numItems: Integer; LstArray: Ptr; theList: Ptr);
      syscall SysTrapLstSetListChoices;

   procedure LstSetPosition (y, x: Integer; theList: Ptr);
      syscall SysTrapLstSetPosition;

   procedure LstSetSelection (itemNum: Integer; theList: Ptr);
      syscall SysTrapLstSetSelection;

{ ------------- Mem ------------- }

   function MemHandleLock (h: Ptr): Ptr;
      syscall SysTrapMemHandleLock;

   function MemHandleNew (size: LongInt): Ptr;
      syscall SysTrapMemHandleNew;

   function MemHandleResize (size: LongInt; h: Ptr): Integer;
      syscall SysTrapMemHandleResize;

   procedure MemMove (length: LongInt; source, dest: Ptr);
      syscall SysTrapMemMove;

   function MemHandleUnLock (h: Ptr): Integer;
      syscall SysTrapMemHandleUnLock;

   function MemPtrUnlock (p: Ptr): Integer;
      syscall SysTrapMemPtrUnlock;

{ ------------- Menu ------------- }

   procedure MenuDispose (theMenu: Ptr);
      syscall SysTrapMenuDispose;

   function MenuGetActiveMenu: Ptr;
      syscall SysTrapMenuGetActiveMenu;

   procedure MenuEraseStatus (theMenu: Ptr);
      syscall SysTrapMenuEraseStatus;

   function MenuHandleEvent (var error: Integer; event: eventType; menuP: Ptr): Integer;
      syscall SysTrapMenuHandleEvent;

   function MenuInit (menuID: Integer): Ptr;
      syscall SysTrapMenuInit;

   function MenuSetActiveMenu (theMenu: Ptr): Ptr;
      syscall SysTrapMenuSetActiveMenu;

{ ------------- Pref ------------- }

   procedure PrefGetPreferences (var prefs: SystemPreferencesType);
      syscall SysTrapPrefGetPreferences;

   function PrefGetAppPreferencesV10 (prefsSize: Integer; prefs: Ptr; version: Integer; Creator: OSType): Integer;
      syscall SysTrapPrefGetAppPreferencesV10;

   procedure PrefSetAppPreferencesV10 (prefsSize: Integer; prefs: Ptr; version: Integer; Creator: OSType);
      syscall SysTrapPrefSetAppPreferencesV10;

{ ------------- Scroll ------------- }

   procedure SclDrawScrollBar (theBar: Ptr);
      syscall sysTrapSclDrawScrollBar;

   function SclHandleEvent (event: eventType; ScrollBar: Ptr): Integer;
      syscall SysTrapSclHandleEvent;

   procedure SclSetScrollBar (pageSize, max, min, value: Integer; theBar: Ptr);
      syscall sysTrapSclSetScrollBar;

{ ------------- Snd ------------- }

   procedure SndPlaySystemSound (beepID: Byte);
      syscall SysTrapSndPlaySystemSound;

{ ------------- Str ------------- }

   procedure StrCopy (s, d: Ptr);
      syscall SysTrapStrCopy;

   procedure StrIToA (i: Longint; s: Ptr);
      syscall SysTrapStrIToA;

   function StrLen (s: Ptr): Integer;
      syscall SysTrapStrLen;

{ ------------- Sys ------------- }

   procedure SysCurAppDatabase (var id: Longint; var card: Integer);
      syscall SysTrapSysCurAppDatabase;

   function SysFormPointerArrayToStrings (numFields: Integer; p: Ptr): Ptr;
      syscall SysTrapSysFormPointerArrayToStrings;

   function SysHandleEvent (event: eventType): Integer;
      syscall SysTrapSysHandleEvent;

   procedure SysKeyboardDialog;
      syscall SysTrapSysKeyboardDialog;

   function SysUIAppSwitch (cmdPBP: Ptr; cmd: Integer; dbID: LongInt; cardNo: Integer): Integer;
      syscall SysTrapSysUIAppSwitch;

{ ------------- Tbl ------------- }

   procedure TblDrawTable (table: Ptr);
      syscall SysTrapTblDrawTable;

   function TblGetItemInt (column, row: Integer; table: Ptr): Integer;
      syscall SysTrapTblGetItemInt;

   function TblGetNumberOfRows (table: Ptr): Integer;
      syscall SysTrapTblGetNumberOfRows;

   procedure TblSetColumnUsable (usable, row: Integer; table: Ptr);
      syscall SysTrapTblSetColumnUsable;

   procedure TblSetItemInt (value, column, row: Integer; table: Ptr);
      syscall SysTrapTblSetItemInt;

   procedure TblSetItemPtr (thePtr: Ptr; column, row: Integer; table: Ptr);
      syscall SysTrapTblSetItemPtr;

   procedure TblSetItemStyle (style, column, row: Integer; table: Ptr);
      syscall SysTrapTblSetItemStyle;

   procedure TblSetRowSelectable (selectable, row: Integer; table: Ptr);
      syscall SysTrapTblSetRowSelectable;

   procedure TblSetRowUsable (usable, row: Integer; table: Ptr);
      syscall SysTrapTblSetRowUsable;

{ ------------- Tim ------------- }

   function DaysInMonth (year, month: Integer): Integer;
      syscall SysTrapDaysInMonth;

   function DayOfWeek (year, day, month: Integer): Integer;
      syscall SysTrapDayOfWeek;

   function TimDateTimeToSeconds (var dt: DateTimeType): LongInt;
      syscall SysTrapTimDateTimeToSeconds;

   function TimGetSeconds: LongInt;
      syscall SysTrapTimGetSeconds;

   procedure TimSetSeconds (theSeconds: LongInt);
      syscall SysTrapTimSetSeconds;

   procedure TimSecondsToDateTime (var dt: DateTimeType; theSecs: Longint);
      syscall SysTrapTimSecondsToDateTime;

{ ------------- Win ------------- }

   procedure WinDrawChars (y, x, len: Integer; theString: Ptr);
      syscall SysTrapWinDrawChars;

   procedure WinDrawGrayRectangleFrame (var theRect: Rectangletype; frameType: Integer);
      syscall SysTrapWinDrawGrayRectangleFrame;

   procedure WinDrawRectangle (cornerDiam: Integer; var theRect: Rectangletype);
      syscall SysTrapWinDrawRectangle;

   procedure WinEraseRectangle (corenerDiam: Integer; var theRect: Rectangletype);
      syscall SysTrapWinEraseRectangle;

   procedure WinFillRectangle (cornerDiam: Integer; var theRect: Rectangletype);
      syscall SysTrapWinFillRectangle;

   procedure WinSetPattern (var pattern: CustomPatternType);
      syscall SysTrapWinSetPattern;


implementation

end.
