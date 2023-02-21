{****************************************************************************


    This file is part of the Free Pascal run time library.
    Copyrigth (c) 2003 by Yuri Prokushev (prokushev@freemail.ru)

    OS/2 Presentation Manager Workplace functions and types.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit pmwp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  OS2Api.os2def, OS2Api.pmwin;
{$ELSE FPC_DOTTEDUNITS}
uses
  os2def, pmwin;
{$ENDIF FPC_DOTTEDUNITS}

//*** Common types *******************************************************/

type
  HOBJECT=Cardinal;

//*** Object management calls ********************************************/

//*** Standard object classes *****************************************/

const
  CCHMAXCLASS            = 3;      // Length of a classname

  QC_First               = 0;      // Codes for OA_QueryContent
  QC_Next                = 1;
  QC_Last                = 2;

  LOCATION_DESKTOP = PAnsiChar($FFFF0001); // Current Desktop
         // use instead of <WP_DESKTOP>

//*** An object's appearance (icon or bitmap or outline) **************/

type
  OBJECTIMAGE=record     // oimg
    hptrObject: Cardinal;
  end;
  POBJECTIMAGE=^OBJECTIMAGE;

//*** Class info structure returned by WinEnumObjectClasses ***********/
type
  POBJCLASS=^OBJCLASS;
  OBJCLASS=record         // ocls
    pNext: POBJCLASS;     // Null for the last structure..
    pszClassName: PAnsiChar;  // Class name
    pszModName: PAnsiChar;    // Module name
  end;

//*** Workplace object management functions ***************************/

Function WinRegisterObjectClass(pszClassName,
                                pszModName: PAnsiChar): Longbool; cdecl;
    external 'PMWP' index 200;

Function WinDeRegisterObjectClass(pszClassName: PAnsiChar): Longbool; cdecl;
    external 'PMWP' index 201;

Function WinReplaceObjectClass(pszOldClassName,
                               pszNewClassName: PAnsiChar;
                               fReplace: Longbool): Longbool; cdecl;
    external 'PMWP' index 219;

Function WinEnumObjectClasses(VAR apObjClass: OBJCLASS;
                              VAR pulSize: Cardinal): Longbool; cdecl;
    external 'PMWP' index 205;

Function WinCreateObject(pszClassName,
                         pszTitle,
                         pszSetupString,
                         pszLocation: PAnsiChar;
                         ulFlags: Cardinal): HObject; cdecl;
    external 'PMWP' index 281;

const
  CO_FAILIFEXISTS    = 0;
  CO_REPLACEIFEXISTS = 1;
  CO_UPDATEIFEXISTS  = 2;

Function WinSetObjectData(aobject: HOBJECT;
                          pszSetupString: PAnsiChar): Longbool; cdecl;
    external 'PMWP' index 250;

Function WinDestroyObject(aobject: HOBJECT): Longbool; cdecl;
    external 'PMWP' index 251;

Function WinQueryObject(pszObjectID: PAnsiChar): HObject; cdecl;
    external 'PMWP' index 252;

Function WinSaveObject(ahObject: HOBJECT;
                       fAsync: Longbool): Longbool; cdecl;
    external 'PMWP' index 285;

Function WinOpenObject(ahObject: HOBJECT;
                       ulView: Cardinal;
                       Flag: Longbool): Longbool; cdecl;
    external 'PMWP' index 286;

Function WinMoveObject(hObjectofObject: HOBJECT;
                       hObjectofDest: HOBJECT;
                       ulReserved: Cardinal): HObject; cdecl;
    external 'PMWP' index 287;

Function WinCopyObject(hObjectofObject: HOBJECT;
                       hObjectofDest: HOBJECT;
                       ulReserved: Cardinal): HObject; cdecl;
    external 'PMWP' index 288;

Function WinCreateShadow(hObjectofObject: HOBJECT;
                         hObjectofDest: HOBJECT;
                         ulReserved: Cardinal): HObject; cdecl;
    external 'PMWP' index 289;

Function WinQueryActiveDesktopPathname(pszPathName: PAnsiChar;
                                       ulSize: Cardinal): Longbool; cdecl;
    external 'PMWP' index 262;

Function WinQueryObjectPath(ahobject: HOBJECT;
                            pszPathName: PAnsiChar;
                            ulSize: Cardinal): Longbool; cdecl;
    external 'PMWP' index 263;

Function WinRestartWPDServer(fState: Longbool): Cardinal; cdecl;
    external 'PMWP' index 463;

Function WinIsWPDServerReady: Longbool; cdecl;
    external 'PMWP' index 465;

Function WinRestartSOMDD(fState: Longbool): Cardinal; cdecl;
    external 'PMWP' index 464;

Function WinIsSOMDDReady: Longbool; cdecl;
    external 'PMWP' index 480;

//*** Object settings notebook page insertion structure ******************/

type
  PAGEINFO=record     // pginf
    cb: Cardinal;
    hwndPage: HWnd;
    pfnwp: proc;
    resid: Cardinal;
    pCreateParams: Pointer;
    dlgid: Word;
    usPageStyleFlags: Word;
    usPageInsertFlags: Word;
    usSettingsFlags: Word;
    pszName: PAnsiChar;
    idDefaultHelpPanel: Word;
    usReserved2: Word;
    pszHelpLibraryName: PAnsiChar;
    pHelpSubtable: ^Word;   // PHELPSUBTABLE when PMHELP.H is included
    hmodHelpSubtable: Cardinal;
    ulPageInsertId: Cardinal;
  end;
  PPAGEINFO=^PAGEINFO;

const
  SETTINGS_PAGE_NUMBERS   = $01;

//*** Utility apis +******************************************************/

type
  ICONPOS=record     // icp
    ptlIcon: POINTL;                    // Location
    szIdentity: Array[0..1-1] of AnsiChar;  // Object identity string
  end;
  PICONPOS=^ICONPOS;

//*********************************************************************/
Function WinSetFileIcon(pszFileName: PAnsiChar;
                    var pIcon: ICONINFO): Longbool; cdecl;
    external 'PMWP' index 210;

Function WinFreeFileIcon(hptr: Cardinal): Longbool; cdecl;
    external 'PMWP' index 216;

Function WinLoadFileIcon(pszFileName: PAnsiChar;
                         fPrivate: Longbool): Cardinal; cdecl;
    external 'PMWP' index 209;

Function WinStoreWindowPos(pszAppName,
                           pszKeyName: PAnsiChar;
                           ahwnd: HWND): Longbool; cdecl;
    external 'PMWP' index 207;

Function WinRestoreWindowPos(pszAppName,
                             pszKeyName: PAnsiChar;
                             ahwnd: HWND): Longbool; cdecl;
    external 'PMWP' index 208;

Function WinShutdownSystem(ahab: HAB;
                           ahmq: HMQ): Longbool; cdecl;
    external 'PMWP' index 149;

implementation

end.
{
// Not implemented/not documented APIs
WinShutdownAndReboot    PMWP     152 ?
WinShutdown             PMWP     153 ?
OldWinCreateObject      PMWP     202 ?
WinRestartWorkplace     PMWP     221 ?
ShlGetUserWordPtr       PMWP     224 ?
WinUnlockSystem         PMWP     282 ?
WinLockupSystem         PMWP     283 ?
WinNotebookButtonFromID PMWP     511 ?
WinWaitForShell         PMWP     512 ?
}

{
// Not founded indexes
Function WinSetFileIconN(pszFileName: PAnsiChar
                         pIcnInfo: PICONINFO;
                         ulIconIndex: Cardinal): Longbool; cdecl;
    external 'PMWP' index ???;

Function WinLoadFileIconN(pszFileName: PAnsiChar;
                          fPrivate: Longbool,
                          ulIconIndex: Cardinal): Cardinal; cdecl;
    external 'PMWP' index ???;
}

