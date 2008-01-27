{
    Copyright (c) 2000-2003 by WPS Toolkit Project - Christian Langanke
    Copyright (c) 2003-2004 by Yuri Prokushev (prokushev@freemail.ru)

    Workplace Toolkit API

    This file is part of the WPS Toolkit package and is free software.
    You can redistribute it and/or modify it under the terms of the GNU
    Library General Public License as published by the Free Software
    Foundation, in version 2 as it comes in the "COPYING.LIB" file
    of the WPS Toolkit main distribution. This library is distributed
    in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.

 **********************************************************************}

Unit wpstk;

Interface

{$Mode ObjFpc}

Uses
  OS2Def, PMWin;

// Please, rename wpstk.dll to another name to avoid version conflicts
const
  wpstkdll='wpstk';

Function WtkQueryVersion(pszBuffer: PChar; ulBuflen: Cardinal): Cardinal; cdecl;
  external wpstkdll name 'WtkQueryVersion';

//*** handle types used by EA manager ***************************************/
Type
  LHANDLE=Cardinal;
  HEA=LHANDLE;
  PHEA=^HEA;

Const
  HEA_CREATE=HEA(-1);

//*** EA type wildcard for WtkFindNextEaValue *******************************/
  WTK_EAT_ANY=-1;

//*** get size of all values of an EA ***************************************/
Function WtkQueryEaSize(pszName, pszEaName: PChar; var pulSize: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkQueryEaSize';

//*** read complete ea into memory and scan it ******************************/
Function WtkReadEa(pszName: PChar; pszEaName: PChar; P_HEA: PHEA): Longint; cdecl;
  external wpstkdll name 'WtkReadEa';

Function WtkFindFirstEaValue(_HEA: HEA; var pulEaType: Cardinal; pszValue: PChar; var pulBuflen: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkFindFirstEaValue';

Function WtkFindNextEaValue(_HEA: HEA; var pulEaType: Cardinal; pszValue: PChar; var pulBuflen: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkFindNextEaValue';

//*** create new EA list in memory ******************************************/
Function WtkCreateEa(pszName: PChar; pszEaName: PChar; _PHEA: PHEA): Longint; cdecl;
  external wpstkdll name 'WtkCreateEa';

//*** append a value ********************************************************/
Function WtkAppendEaValue(HEA_: HEA; ulMultiType: Cardinal; ulEaType: Cardinal; var pbValue: Byte; ulValuelen: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkAppendEaValue';

//*** write EA to disk ******************************************************/
Function WtkSaveEa(_HEA: HEA; fWriteThru: Longbool; fEA: Byte): Longint; cdecl;
  external wpstkdll name 'WtkSaveEa';

//*** destroy EA in memory **************************************************/
Function WtkCloseEa(_HEA: hea): Longint; cdecl;
  external wpstkdll name 'WtkCloseEa';

//*** WtkGetNextFile alike API, does WtkReadEa/WtkFind*EaValue/WtkCloseEa in one API */
Function WtkGetNextEaValue(pszName, pszEaName: PChar; var pulEaType: Cardinal; PHEA_: PHEA; pszValue: PChar; var pulBuflen: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkGetNextEaValue';

//*** read/appendWrite EA in one step ***************************************/
Function WtkPutEaValue(pszName, pszEaName: PChar; ulMultiType, ulEaType: Cardinal; var pbValue: Byte; ulValuelen: Cardinal; fWriteThru: Longbool; fEA: Byte): Longint; cdecl;
  external wpstkdll name 'WtkPutEaValue';

//*** simplified APIs to read/write exatcly one string EA********************/
Function WtkWriteStringEa(pszName, pszEaName, pszEaValue: PChar): Longint; cdecl;
  external wpstkdll name 'WtkWriteStringEa';
Function WtkReadStringEa(pszName, pszEaName, pszEaValue: PChar; var pulBuflen: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkReadStringEa';

Const
  WTK_CFGSYS_UPDATE_ADD=0;
  WTK_CFGSYS_UPDATE_DELETE=1;

Function WtkUpdateConfigsys(pszUpdate: PChar; ulUpdateMode: Cardinal; pszBackupExtension: PChar): Longint; cdecl;
  external wpstkdll name 'WtkUpdateConfigsys';

//* file handle type and structures */
Type
  HINIT=LHANDLE;
  PHINIT=^HINIT;


// structure to override default behaviour of parser
  TINITPARMS=record
    // specify all valid comment characters
    // - specify nil to use ';' as default and only comment character
    // - specify '/' to use "//" (c++ style comments)
    pszCommentChars: PChar;
    // specify all valid delimiter characters
    // - specify nil to use '=' as default and only delimiter character
    // - when multiple characters are specified, the first will be used
    //   for new entries
    pszDelimiterChars: PChar;
    // define layout for keys in new files
    // (if keys already exist, the layout of
    //  the last read key is used)
    //
    // [SECTION]
    //
    //      newkeyname1   =   keyvalue1
    // |   |  -> ulKeyIndent
    //
    //      newkeyname2   =   keyvalue2
    //      |            | ->  ulKeyNameLen
    //
    //      newkeyname3   =   keyvalue3
    //                     | | -> ulValueIndent
    ulKeyIndent,
    ulKeyNameLen,
    ulValueIndent: Cardinal;
  end;
  PINITPARMS=^TINITPARMS;

//* define open modes (WtkOpenInitProfile - ulOpenMode) */
Const
  WTK_INIT_OPEN_READONLY=$0000;
  WTK_INIT_OPEN_READWRITE=$0001;

  WTK_INIT_OPEN_ALLOWERRORS=$8000;
  WTK_INIT_OPEN_INMEMORY=$FFFF;

//* define update modes (WtkOpenInitProfile - ulUpdateMode) */
  WTK_INIT_UPDATE_DISCARDCOMMENTS=$0001;
  WTK_INIT_UPDATE_SOFTDELETEKEYS=$0002;

//* open and close file */
Function WtkOpenInitProfile(pszName: PChar; var phinit: HINIT; ulOpenMode: Cardinal;
                          ulUpdateMode: Cardinal; pip: PINITPARMS): Longint; cdecl;
  external wpstkdll name 'WtkOpenInitProfile';

Function WtkCloseInitProfile(HINIT: HINIT; fUpdate: Longbool): Longint; cdecl;
  external wpstkdll name 'WtkCloseInitProfile';

const
  WTK_INIT_CLOSE_DISCARD=0;
  WTK_INIT_CLOSE_UPDATE=1;

Function WtkCloseInitProfileBackup(_hinit: HINIT; fUpdateOriginal: Longbool; pszBackupFile: PChar): Longint; cdecl;
  external wpstkdll name 'WtkCloseInitProfileBackup';
Function WtkInitProfileModified(_hinit: HINIT): Longbool; cdecl;
  external wpstkdll name 'WtkInitProfileModified';

//* query values */
Function WtkQueryInitProfileString(_HINIT: hinit; pszSectionName, pszKeyName, pszDefault, pszBuffer: PChar; ulBuflen: Cardinal): Cardinal; cdecl;
  external wpstkdll name 'WtkQueryInitProfileString';
Function WtkQueryInitProfileSize(_HINIT: hinit; pszSectionName, pszKeyName: PChar; var pulDataLen: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkQueryInitProfileSize';

//* update or delete keys and/or sections */
Function WtkWriteInitProfileString(_HINIT: hinit; pszSectionName, pszKeyName, pszNewValue: PChar): Longint; cdecl;
  external wpstkdll name 'WtkWriteInitProfileString';

//* file handle type and structures */

Type
  HSYSLEVEL = LHANDLE;
  PHSYSLEVEL = ^HSYSLEVEL;

Const
  WTK_SYSLEVEL_MAXLEN_COMPID = 9;
  WTK_SYSLEVEL_MAXLEN_NAME = 79;
  WTK_SYSLEVEL_MAXLEN_PREFIX = 2;
  WTK_SYSLEVEL_MAXLEN_CSDNUM = 4;
  WTK_SYSLEVEL_MAXLEN_APPTYPE = 8;

//* external data structure                 */
//* NOTE:                                   */
//*  - read/write version fields in hex     */
//*  - use the WTK_SYSLEVEL_MAXLEN_* with   */
//*    strncpy() to properly copy strings   */
//*    to string fields (see sample !)      */

Type
  TSYLEVELINFO = record
    cbSize : Cardinal;
    usSysId : Word;
    szComponentId : array[0..(WTK_SYSLEVEL_MAXLEN_COMPID + 1)-1] of CHAR;
    szName : array[0..(WTK_SYSLEVEL_MAXLEN_NAME + 1)-1] of CHAR;
    bVersionMajor : BYTE;
    bVersionMinor : BYTE;
    bVersionRefresh : BYTE;
    szCsdPrefix : array[0..(WTK_SYSLEVEL_MAXLEN_PREFIX + 1)-1] of CHAR;
    chCsdLanguage : CHAR;
    szCurrentCsd : array[0..(WTK_SYSLEVEL_MAXLEN_CSDNUM + 1)-1] of CHAR;
    szPreviousCsd : array[0..(WTK_SYSLEVEL_MAXLEN_CSDNUM + 1)-1] of CHAR;
    usDate : Word;
    szAppType : array[0..(WTK_SYSLEVEL_MAXLEN_APPTYPE + 1)-1] of CHAR;
  end;
  PSYLEVELINFO = ^TSYLEVELINFO;

Function WtkOpenSyslevel(pszName: PChar; var phsl: HSYSLEVEL): Longint; cdecl;
  external wpstkdll name 'WtkOpenSyslevel';
Function WtkCloseSyslevel(hsl: HSYSLEVEL; ulUpdateMode: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkCloseSyslevel';

Const
  WTK_SYSLEVEL_CLOSE_DISCARD = 0;
  WTK_SYSLEVEL_CLOSE_UPDATE = 1;

Function WtkQuerySyslevelInfo(hsl: HSYSLEVEL; var psli: TSYLEVELINFO): Longint; cdecl;
  external wpstkdll name 'WtkCloseSyslevel';
Function WtkSetSyslevelInfo(hsl: HSYSLEVEL; ulUpdateFlags: Cardinal; var psli: TSYLEVELINFO): Longint; cdecl;
  external wpstkdll name 'WtkCloseSyslevel';

Const
  WTK_SYSLEVEL_UPDATE_ALL = -(1);
  WTK_SYSLEVEL_UPDATE_SYSID = $00000001;
  WTK_SYSLEVEL_UPDATE_COMPONENTID = $00000002;
  WTK_SYSLEVEL_UPDATE_NAME = $00000004;
  WTK_SYSLEVEL_UPDATE_VERSION = $00000008;
  WTK_SYSLEVEL_UPDATE_CSDPREFIX = $00000010;
  WTK_SYSLEVEL_UPDATE_CSDLANGUAGE = $00000020;
  WTK_SYSLEVEL_UPDATE_CURRENTCSD = $00000040;
  WTK_SYSLEVEL_UPDATE_PREVIOUSCSD = $00000080;
  WTK_SYSLEVEL_UPDATE_DATE = $00000010;
  WTK_SYSLEVEL_UPDATE_APPTYPE = $00000020;

  {* flags for MmfAlloc parm ulOpenFlags                                       */ }
  {* NOTE: for all except MMF_ACCESS_READWRITE, only write by others is denied */ }
  {*       otherwise both read and write by others is denied                   */ }
  MMF_ACCESS_READONLY = $00000000;
  MMF_ACCESS_WRITEONLY = $00000001;
  MMF_ACCESS_READWRITE = $00000002;
  MMF_OPENMODE_OPENFILE = $00000000;
  MMF_OPENMODE_RESETFILE = $00010000;

  {* some sizes for usage with MmfAlloc parameter ulMaxSize */ }
  MMF_MAXSIZE_KB = 1024;
  MMF_MAXSIZE_MB = 1048576;

  {* special NULL filename for MmfAlloc parameter pszFilename */ }
  MMF_FILE_INMEMORY = nil;
  {* define a handle type */ }

//* define a handle type */
Type
  THMMF = LHANDLE;
  PHMMF = ^THMMF;

//* prototypes */
Function WtkInitializeMmf(var phmmf: THMMF; ulMaxBuffer: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkInitializeMmf';
Function WtkTerminateMmf(hmmf: THMMF): Longint; cdecl;
  external wpstkdll name 'WtkTerminateMmf';
Function WtkAllocMmf(hmmf: THMMF; Var ppvdata: Pointer; pszFilename: PChar; ulOpenFlags, ulMaxSize: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkAllocMmf';
Function WtkFreeMmf(hmmf: THMMF; pvData: Pointer): Longint; cdecl;
  external wpstkdll name 'WtkFreeMmfl';
Function WtkUpdateMmf(hmmf: THMMF; pvData: Pointer): Longint; cdecl;
  external wpstkdll name 'WtkUpdateMmf';

Function WtkSetMmfSize(hmmf: THMMF; pvData: Pointer; ulNewSize: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkSetMmfSize';
Function WtkQueryMmfSize(hmmf: THMMF; pvData: Pointer; var pulSize: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkQueryMmfSize';

//* only be used for string translation for indexed */
//* values by callback STM_CALLBACK_QUERYSTRING     */
Const
  _STM_MAX_SETTINGSVALUE = 64;

//*** handle types used by Setting manager **********************************/
Type
  HSETTINGTABLE = LHANDLE;
  HVALUETABLE = LHANDLE;

//*** callback procedure prototype ******************************************/
Type
  TFnCB=Function(ulAction: Cardinal; pvData, pvObjectInstance, pvObjectData: Pointer): Longbool; cdecl;
  PFNCB = ^TFnCB;

//*** callback actions ******************************************************/

Const
  { reports that class is being initialized          }
  STM_CALLBACK_REPORTINIT = $0000;
  { queries infos about (sub)value                   }
  STM_CALLBACK_QUERYVALUEINFO = $0001;
  { queries target buffer when creating value table  }
  STM_CALLBACK_QUERYTARGETBUF = $0002;
  { ask WPS class to validate a (sub)value           }
  STM_CALLBACK_VALIDATE = $0003;
  { reports the change of a setting                  }
  STM_CALLBACK_REPORTCHANGED = $0004;
  { asks for update of target buffers                }
  STM_CALLBACK_QUERYVALUE = $0005;
  { translates strings to array indicees             }
  STM_CALLBACK_QUERYINDEX = $0006;
  { translates strings to array indicees             }
  STM_CALLBACK_QUERYSTRING = $0007;
  { asks for initialization of GUI controls,         }
  STM_CALLBACK_INITCONTROL = $0008;
  { useful for listboxes comboboxes etc.             }
  { reports errors from WtkValidateObjectValueTable  }
  STM_CALLBACK_REPORTERROR = $0009;
  { reports that settings have been saved            }
  STM_CALLBACK_REPORTSAVED = $000A;
  { reports that class settings table is destroyed   }
  STM_CALLBACK_REPORTDESTROYED = $000B;

//*** data structure for callback STM_CALLBACK_REPORTINIT *************/
//*** Note: on this callback, pvObjectInstance and pvObjectData are NULL ****/
Type
  TCBREPORTINIT = record
    fInitialized : Longbool; //*  no data really required here yet       */
  end;

//*** data structures for callback STM_CALLBACK_QUERYVALUEINFO ********/
//*** Note: on this callback, pvObjectInstance and pvObjectData are NULL ****/
  TCBQUERYVALUEINFO = record
    pszName : PChar;                     //* out - name of setting                   */
    ulSettingId : Cardinal;              //* out - id of setting                     */
    ulValueIndex : Cardinal;             //* out - index of subvalue                 */
    ulValueType : Cardinal;              //* #in - type of value - see VALUETYPE_*   */
    usDialogid : Word;                   //*  in - id of dialog containing control   */
    usControlid : Word;                  //*  in - id of control                     */
    pfnwpSubclass : proc;               //*  in - subclass window proc for control  */
    pszDetailsTitle : PChar;             //*  in - title of folderdetails view       */
  end;
  PCBQUERYVALUEINFO = ^TCBQUERYVALUEINFO;

//*** data structures for callback CALLBACK_QUERYTARGETBUF ************/
//* values marked with # MUST be specified, all others are optional */

Type
  TCBQUERYTARGETBUF = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    pszName : PChar;            //* out - name of setting                   */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    ulValueIndex : Cardinal;    //* out - index of subvalue (only > 0 for multivalue settings)   */
    ulBufMax : Cardinal;        //* #in - len of target buffer              */
    pvTarget : Pointer;         //* #in - target buffer                     */
  end;
  PCBQUERYTARGETBUF = ^TCBQUERYTARGETBUF;

//* supported GUI controls:                                                             */
//* - WC_ENTRYFIELD WC_MLE WC_BUTTON (checkbox and radio button)                        */
//* - WC_LISTBOX WC_COMBOBOX WC_SPINBUTTON                                              */
//* - WC_SLIDER WC_CIRCULARSLIDER                                                       */

//* types of possible values                                                            */
//*                                                                                     */
//* type          validation target buffer possible GUI update                          */
//* ------------- ---------- ------------- -------------------------------              */
//* STRING        NO         CHAR[]        <all supported> except WC_*SLIDER            */
//*                                        - WC_COMBOBOX, WC_SPINBUTTON as listbox      */
//* INDEX         YES        LONG          <all supported>                              */
//* ITEM          YES        LONG          selects item with WC_LISTBOX, WC_COMBOBOX,   */
//*                                        behaves like LONG otherwise                  */
//* LONG          NO         LONG          <all supported>                              */
//* TRUEFALSE     YES        BOOL          <all supported> except WC_*SLIDER            */
//* YESNO         YES        BOOL          <all supported> except WC_*SLIDER            */
//* ONOFF         YES        BOOL          <all supported> except WC_*SLIDER            */


//*** value types for value target buffers **********************************/
Const
  STM_VALUETYPE_STRING = 0;
  STM_VALUETYPE_INDEX = 1;
  STM_VALUETYPE_LONG = 2;
  STM_VALUETYPE_TRUEFALSE = 3;
  STM_VALUETYPE_YESNO = 4;
  STM_VALUETYPE_ONOFF = 5;
  { for details only !  }
  STM_VALUETYPE_CDATE = 6;
  { for details only !  }
  STM_VALUETYPE_CTIME = 7;
  STM_VALUETYPE_INDEXITEM = 8;

//*** data structures for callback CALLBACK_VALIDATION ****************/
Type
  TCBVALIDATE = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    pszName : PChar;            //* out - name of setting                   */
    pszValue : PChar;           //* out - name of value to be validated     */
    ulValueIndex : Cardinal;    //* out - index of subvalue                 */
    fResult : Longbool;         //*  in - result of validation              */
  end;                          //* return TRUE if your callback validates, */
  PCBVALIDATE = ^TCBVALIDATE;   //* FALSE for using standard validation     */

//*** data structures for callback CALLBACK_REPORTCHANGED ****************/
Type
  TCBREPORTCHANGED = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    pszName : PChar;            //* out - name of setting                   */
  end;
  PCBREPORTCHANGED = ^TCBREPORTCHANGED;

//*** data structures for callback CALLBACK_QUERYVALUE ****************/
  TCBQUERYVALUE = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    pszName : PChar;            //* out - name of setting                   */
    ulQueryIndex : Cardinal;    //* out - index of query                    */
  end;
  PCBQUERYVALUE = ^TCBQUERYVALUE;

//*** data structures for callback CALLBACK_QUERYINDEX ****************/
  TCBQUERYINDEX = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    pszName : PChar;            //* out - name of setting                   */
    pszValue : PChar;           //* out - string value to be translated     */
    ulValueIndex : Cardinal;    //* out - index of subvalue                 */
    ulStringIndex : Cardinal;   //*  in - index to be used                  */
  end;                          //* return TRUE if processed, else FALSE    */
  PCBQUERYINDEX = ^TCBQUERYINDEX;

//*** data structures for callback CALLBACK_QUERYSTRING ***************/
  TCBQUERYSTRING = record
    pvObjectInstance : Pointer;                            // out - somSelf of object instance        */
    ulSettingId : Cardinal;                                // out - id of setting                     */
    pszName : PChar;                                       // out - name of setting                   */
    ulStringIndex : Cardinal;                              // out - index to be translated            */
    ulValueIndex : Cardinal;                               // out - index of subvalue                 */
    szValue : array[0..(_STM_MAX_SETTINGSVALUE)-1] of CHAR;// in - string value to be used      */
  end;                                                     // return TRUE if processed, else FALSE    */
  PCBQUERYSTRING = ^TCBQUERYSTRING;

//*** data structures for callback CALLBACK_INITCONTROL ***************/
  TCBINITCONTROL = record
    usDialogid : Word;  //* out - id of dialog                      */
    usControlid : Word; //* out - id of control                     */
    hwndDialog : HWND;  //* out - handle of dialog                  */
    hwndControl : HWND; //* out - handle of control                 */
  end;
  PCBINITCONTROL = ^TCBINITCONTROL;

//*** data structures for callback STM_CALLBACK_REPORTERROR ***********/
  TCBREPORTERROR = record
    pvObjectInstance : Pointer; //* out - somSelf of object instance        */
    ulSettingId : Cardinal;     //* out - id of setting                     */
    pszName : PChar;            //* out - name of setting                   */
    usDialogid : Word;          //* out - id of dialog                      */
    usControlid : Word;         //* out - id of control                     */
    hwndDialog : HWND;          //* out - handle of dialog                  */
    hwndControl : HWND;         //* out - handle of control                 */
  end;                          //* return TRUE to ignore error             */
  PCBREPORTERROR = ^TCBREPORTERROR;

//*** data structures for callback STM_CALLBACK_REPORTSAVED ***********/
  TCBREPORTSAVED = record
    fSaved : Longbool; //*  no data really required here yet       */
  end;
  PCBREPORTSAVED = ^TCBREPORTSAVED;

//*** data structures for callback STM_CALLBACK_DESTROYED *************/
//*** Note: on this callback, pvObjectInstance and pvObjectData are NULL ***/
  TCBREPORTDESTROYED = record
    fDestroyed : Longbool; //*  no data really required here yet       */
  end;
  PCBREPORTDESTROYED = ^TCBREPORTDESTROYED;

// ----------------------------------------
// prototypes
// ----------------------------------------

//*** prototypes for (de)initializing the settings table for the metaclass **/
Function WtkCreateClassSettingsTable(pvObjectClass: Pointer; pfnCallbackValue: PFNCB): HSETTINGTABLE; cdecl;
  external wpstkdll name 'WtkCreateClassSettingsTable';
Function WtkDestroyClassSettingsTable(hst: HSETTINGTABLE): Longbool; cdecl;
  external wpstkdll name 'WtkDestroyClassSettingsTable';
Function WtkAddClassSetting(hst: HSETTINGTABLE; ulSettingId: Cardinal; pszSetting: Pchar; ulQueryCount: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkAddClassSetting';
Function WtkAddClassDetail(hst: HSETTINGTABLE; ulSettingId: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkAddClassDetail';
Function WtkCloseClassSettingsTable(hst: HSETTINGTABLE): Longbool; cdecl;
  external wpstkdll name 'WtkCloseClassSettingsTable';
Function WtkDumpClassSettingsTable(hst: HSETTINGTABLE): Longbool; cdecl;  // for testing purposes only, dumps to console
  external wpstkdll name 'WtkDumpClassSettingsTable';

//*** prototypes for maintaining setting values for object instances ********/
Function WtkCreateObjectValueTable(hst: HSETTINGTABLE; pvObjectInstance, pvObjectData: Pointer): HVALUETABLE; cdecl;
  external wpstkdll name 'WtkCreateObjectValueTable';
Function WtkDestroyObjectValueTable(hvt: HVALUETABLE): Longbool; cdecl;
  external wpstkdll name 'WtkDestroyObjectValueTable';
Function WtkEvaluateObjectSettings(hvt: HVALUETABLE; pszSetup: PChar): Longbool; cdecl;
  external wpstkdll name 'WtkEvaluateObjectSettings';
Function WtkQueryObjectSettings(hvt: HVALUETABLE; pszBuffer: PChar; var pulMaxlen: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkQueryObjectSettings';

//*** prototypes for providing automatic updates to GUI controls ************/
Function WtkRegisterSettingsDialog(hvt: HVALUETABLE; hwndDialog: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkRegisterSettingsDialog';
Function WtkDeregisterSettingsDialog(hvt: HVALUETABLE; hwndDialog: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkDeregisterSettingsDialog';
Function WtkReadObjectValueTable(hvt: HVALUETABLE; hwndDialog: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkReadObjectValueTable';
Function WtkWriteObjectValueTable(hvt: HVALUETABLE; hwndDialog: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkWriteObjectValueTable';
Function WtkQueryGUIControlsChanged(hvt: HVALUETABLE; hwnd_: HWND; mp1, mp2:MPARAM; var pfOrgValue: Longbool): Longbool; cdecl;
  external wpstkdll name 'WtkQueryGUIControlsChanged';

//*** prototype for using same dialog templates under WARP 3 and WARP 4 *****/
Function WtkRelocateNotebookpageControls(hwndDialog: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkRelocateNotebookpageControls';

//*** prototypes for providing automatic validation of controls   *************/
//*** on close of settings notebook.                              *************/
//*** NOTE: WtkValidateObjectValueTable needs not to be           *************/
//***       explicitely called for this, but is avaliable anyway. *************/
Function WtkRegisterSettingsNotebook(hvt: HVALUETABLE; hwndNotebook: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkRegisterSettingsNotebook';
Function WtkValidateObjectValueTable(hvt: HVALUETABLE; hwndNotebook: HWND): Longbool; cdecl;
  external wpstkdll name 'WtkValidateObjectValueTable';

//*** prototypes for providing details data *********************************/
Function WtkQueryClassDetailsInfo(hst: HSETTINGTABLE; var ppClassFieldInfo: Pointer;
                                         var pSize: Cardinal; ulParentColumns: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkQueryClassDetailsInfo';
Function WtkQueryObjectDetailsData(hvt: HVALUETABLE; var ppDetailsData: Pointer; var pcp: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkQueryObjectDetailsData';

//*** prototypes for saving/restoring data in WPS repository ****************/
Function WtkSaveObjectState(hvt: HVALUETABLE; pszClass: PChar): Longint; cdecl;
  external wpstkdll name 'WtkSaveObjectState';
Function WtkRestoreObjectState(hvt: HVALUETABLE; pszClass: PChar): Longint; cdecl;
  external wpstkdll name 'WtkRestoreObjectState';

//*** prototypes for saving/restoring data in extern ini file ***************/
//*** NOTE: you have to make sure yourself that you save to   ***************/
//***       a unique place per object instance !!!            ***************/
//*** NOTE: specify USER or SYSTEM as filename to write to    ***************/
//***       HINI_USERPROFILE or HINI_SYSTEMPROFILE            ***************/
Function WtkSaveObjectSettings(hvt: HVALUETABLE; pszFilename, pszApp, pszKey: PChar): Longint; cdecl;
  external wpstkdll name 'WtkSaveObjectSettings';
Function WtkRestoreObjectSettings(hvt: HVALUETABLE; pszFilename, pszApp, pszKey: PChar): Longint; cdecl;
  external wpstkdll name 'WtkRestoreObjectSettings';

Function WtkGetTextMessage(var pTable: PChar; cTable: Cardinal;
                                   var pbBuffer: Byte; cbBuffer: Cardinal;
                                   pszMessageName, pszFile: PChar;
                                   var pcbMsg: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkGetTextMessage';

// data structures for 32-bit memory model

Type
  TBINDATA = record
    cbSize : Cardinal;
    bData : array[0..0] of BYTE;
  end;
  PBINDATA = ^TBINDATA;

//*** prototypes for calculating CRC of data in memory *******************/
//*** initialize pulCRC32 with -1 on first call **************************/
Function WtkCalcMemCRC32(pvData: Pointer; ulDatalen: Cardinal; var pulCRC32: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkCalcMemCRC32';

//*** prototypes for calculating CRC of files ****************************/
Function WtkCalcFileCRC32(pszFilename: PChar; var pulCRC32: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkCalcFileCRC32';

//*** prototypes for general functions **************************************/
Function WtkQueryClassIndex(HWND_: hwnd): PChar; cdecl;
  external wpstkdll name 'WtkQueryClassIndex';
Function WtkIsOfPublicPmClass(HWND_: hwnd; pszClassIndex: PChar; ulPrimaryWindowStyle: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkIsOfPublicPmClass';

//*** prototypes for specialized functions for certain window classes *******/
Function WtkInitializeNumSpinbuttonArray(hwndSpinbutton: HWND; ulMinValue: Cardinal;
                                               ulMaxValue: Cardinal; ulStep: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkInitializeNumSpinbuttonArray';
Function WtkQueryNumSpinbuttonIndex(hwndSpinbutton: HWND; ulMinValue: Cardinal;
                                 ulMaxValue: Cardinal; ulStep: Cardinal; ulValue: Cardinal): Longint; cdecl;
  external wpstkdll name 'WtkQueryNumSpinbuttonIndex';

//*** prototypes for filling MLEs from diverse resources ********************/
Function WtkAddTextResourceToMLE(HWND_: hwnd; ulControlId: Cardinal;
                              hmod: Cardinal;  ulResourceType, ulResId: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkAddTextResourceToMLE';

//*** prototypes for PM error functions *************************************/
Function WtkSetErrorInfo(rc: Cardinal): Longbool; cdecl;
  external wpstkdll name 'WtkSetErrorInfo';

Const
  PMHERR_USE_EXISTING_ERRORINFO = -(1);

//*** check existance of files & directories ********************************/
Function WtkFileExists(pszName: PChar): Longbool;
Function WtkDirExists(pszName: PChar): Longbool;
Function WtkFileMaskExists(pszFileMask, pszFirstFile: PChar; ulBuflen: Cardinal): Longbool;
Function WtkIsFile(pszName: PChar): Longbool;      (* equivalent to WtkFileExists *)
Function WtkIsDirectory(pszName: PChar): Longbool; (* equivalent to WtkDirExists  *)

//*** get fullname of directory or file *************************************/
Function WtkQueryFullname(pszName, pszBuffer: PChar; ulBuflen: Cardinal): Longint;

//*** extended file handling ************************************************/
Function WtkDeleteFile(pszName: PChar): Longint;
Function WtkMoveFile(pszOld, pszNew: PChar): Longint;

//*** query disk and directory at one time **********************************/
Function WtkQueryCurrentDir(ulDiskNum: Cardinal; pszBuffer: PChar; ulBuflen: Cardinal): Longint;
Function WtkSetCurrentDir(pszDirectory: PChar): Longint;

//*** create/delete path ****************************************************/
Function WtkCreatePath(pszPath: PChar): Longint;
Function WtkDeletePath(pszPath: PChar): Longint;

//*** easy version of DosFindFirst/DosFindNext ******************************/
Function WtkGetNextFile(pszFileMask: PChar; phdir: PHDIR;
                                pszNextFile: PChar; ulBuflen: Cardinal): Longint;
Function WtkGetNextDirectory(pszFileMask: PChar; phdir: PHDIR;
                             pszNextDirectory: PChar; ulBuflen: Cardinal): Longint;

//*** search part of filename ***********************************************/
Function WtkFilespec(pszName: PChar; ulPart: Cardinal): PChar;

Const
  WTK_FILESPEC_PATHNAME = 1;
  WTK_FILESPEC_NAME = 2;
  WTK_FILESPEC_EXTENSION = 3;

//*** get specific information about file ***********************************/
Function WtkFileModified(pszName: PChar; pfs3: PFILESTATUS3): Longbool;
Function WtkQueryFileSize(pszName: PChar): Cardinal;

//*** read file into memory  ************************************************/
Function WtkReadFile(pszName: PChar; ppszBuffer: PPChar; pulBuflen: PCardinal): Longint;
Function WtkWriteFile(pszName, pszBuffer: PChar; ulBuflen: Cardinal; fAppend: Longbool): Longint;

//*** create tmp file *******************************************************/
Function WtkCreateTmpFile(pszFileMask, pszBuffer: PChar; ulBuflen: Cardinal): Longint;

//*** check file contents ***************************************************/
Function WtkFileIsEmpty(pszName: PChar): Longbool;

//*** prototypes for opening devices ****************************************/
Function WtkOpenDevice(pszName: PChar; phdevice: PHFILE; ulOpenMode: Cardinal): Longint;

Const
  WTK_OPENDEVICE_SHARED = $0000;
  WTK_OPENDEVICE_EXCLUSIVE = $0001;
  WTK_OPENDEVICE_NOCACHE = $0002;
  { implicitely set, when pszName specifies drive  }
  WTK_OPENDEVICE_BLOCKDEVICE = $0004;

//*** prototypes for performing an I/o Control transaction ******************/
Function WtkDevIOCtl(pszName: PChar; ulOpenMode: Cardinal;
                             ulCategory: Cardinal; ulFunction: Cardinal;
                             pvParams: Pointer; pcbParmLen: PCardinal;
                             pvData: Pointer; pcbDataLen: PCardinal): Longint;

//*** prototypes for module information functions ***************************/
APIRET APIENTRY WtkGetPackageFilename( PFN pfn, PChar pszSubdir, PChar pszFilename,
                              PChar pszFileext, PChar pszBuffer, ULONG ulBuflen);
APIRET APIENTRY WtkGetModuleInfo( PFN pfn, PHMODULE phmod, PChar pszBuffer, ULONG ulBuflen);
APIRET APIENTRY WtkGetModulePath( PFN pfn, PChar pszBuffer, ULONG ulBuflen);
HMODULE APIENTRY WtkGetModuleHandle( PFN pfn);

BOOL APIENTRY WtkIsRegularExpressionValid( PChar pszExpression);
APIRET APIENTRY WtkMatchRegularExpression( PChar pszExpression, PChar pszText,
                                           PChar pszBuffer, ULONG ulBuflen);
APIRET APIENTRY WtkSubstRegularExpressionMatch( PChar pszExpression, PChar pszText,
                                                PChar pszReplacePattern,
                                                PChar pszBuffer, ULONG ulBuflen);

//*** prototypes for system configuration functions *************************/
Function WtkIsWarp4: Longbool; cdecl;
  external wpstkdll name 'WtkIsWarp4';

// distinct between OS/2 Warp and eComStation
Function WtkQueryOperatingSystem: Cardinal; cdecl;
  external wpstkdll name 'WtkQueryOperatingSystem';

Const
  WTK_OSTYPE_OS2 = $0000;
  WTK_OSTYPE_ECS = $0001;

Function WtkIsOS2: Longbool; cdecl;
  external wpstkdll name 'WtkIsOS2';
Function WtkIseComStation: Longbool; cdecl;
  external wpstkdll name 'WtkIseComStation';

Function WtkQueryBootDrive: Char; cdecl;
  external wpstkdll name 'WtkQueryBootDrive';
Function WtkQuerySysLanguage: PChar; cdecl;
  external wpstkdll name 'WtkQuerySysLanguage';

Function WtkReboot: Cardinal; cdecl;
  external wpstkdll name 'WtkReboot';


//*** prototypes for DATETIME struct handling ****************************/
BOOL APIENTRY WtkSetDateTime( UCHAR uchDay, UCHAR uchMonth, USHORT usYear, UCHAR uchHours,
                              UCHAR uchMinutes, UCHAR uchSeconds, PDATETIME pdt);
BOOL APIENTRY WtkDateTimeToTime( PDATETIME pdt, time_t* ptime);
BOOL APIENTRY WtkTimeToDateTime( time_t* ptime, PDATETIME pdt);

//*** prototypes for FTIME FDATE struct handling *************************/
BOOL APIENTRY WtkSetFDateTime( UCHAR uchDay, UCHAR uchMonth, USHORT usYear, UCHAR uchHours,
                               UCHAR uchMinutes, UCHAR uchSeconds, PFDATE pfdate, PFTIME pftime);
BOOL APIENTRY WtkFDateTimeToTime( PFDATE pfdate, PFTIME pftime, time_t* ptime);
BOOL APIENTRY WtkTimeToFDateTime( time_t* ptime, PFDATE pfdate, PFTIME pftime);

//*** prototypes for CDATE/CTIME struct handling ****************************/
BOOL APIENTRY WtkSetCDateTime( UCHAR uchDay, UCHAR uchMonth, USHORT usYear, UCHAR uchHours,
                               UCHAR uchMinutes, UCHAR uchSeconds, PCDATE pcdate, PCTIME pctime);
BOOL APIENTRY WtkCDateTimeToTime( PCDATE pcdate, PCTIME pctime, time_t* ptime);
BOOL APIENTRY WtkTimeToCDateTime( time_t* ptime, PCDATE pcdate, PCTIME pctime);

//*** prototypes for getting a timestamp *********************************/
Const
  WTK_TIMESTAMP_SORTEDDATETIME = 0;
  WTK_TIMESTAMP_SORTEDDATE = 1;
  WTK_TIMESTAMP_SORTEDTIME = 2;
  WTK_TIMESTAMP_NLSDATETIME = 3;
  WTK_TIMESTAMP_NLSDATE = 4;
  WTK_TIMESTAMP_NLSTIME = 5;

APIRET APIENTRY WtkQueryDateTimeStamp( PDATETIME pdt, ULONG ulStampType,
                                                                             PChar pszBuffer, ULONG ulBuflen);
APIRET APIENTRY WtkQueryFDateTimeStamp( PFDATE pfdate, PFTIME pftime, ULONG ulStampType,
                                        PChar pszBuffer, ULONG ulBuflen);
APIRET APIENTRY WtkQueryCDateTimeStamp( PCDATE pcdate, PCTIME pctime, ULONG ulStampType,
                                        PChar pszBuffer, ULONG ulBuflen);

//*** prototypes for getting last write time from a file or directory ****/
APIRET APIENTRY WtkQueryFileDateTime( PDATETIME pdt, PChar pszName);
APIRET APIENTRY WtkQueryFileFDateTime( PFDATE pfdate, PFTIME pftime, PChar pszName);
APIRET APIENTRY WtkQueryFileCDateTime( PCDATE pcdate, PCTIME pctime, PChar pszName);


implementation

end.
