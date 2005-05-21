{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Yuri Prokushev (prokushev@freemail.ru).

    OS/2 Presentation Manager Shell constants, types, messages and
    function declarations.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Warning: This code is alfa. Future versions
 of this unit might not be compatible.}
unit pmshl;

Interface

uses
  Os2Def, PmWin;



// common types, constants and function declarations

// maximum title length
const
  MaxNameL=60;

// program handle
type
   HProgram=Cardinal;       // hprog
   PHProgram=^HProgram;
   HAPP=Cardinal;

// ini file handle
type
  HINI=Cardinal;           // hini
  PHINI=^HINI;

const
  HINI_PROFILE         = 0;
  HINI_USERPROFILE     =-1;
  HINI_SYSTEMPROFILE   =-2;
  HINI_USER    = HINI_USERPROFILE;
  HINI_SYSTEM  = HINI_SYSTEMPROFILE;

type
  PPrfProfile=^PrfProfile;
  PrfProfile=record      // prfpro
    cchUserName: Cardinal;
    pszUserName: PChar;
    cchSysName: Cardinal;
    pszSysName: PChar;
  end;

// program list section

// maximum path length
const
  MAXPATHL=128;

// root group handle
const
  SGH_ROOT = -1;

type
  PHPROGARRAY=^HPROGARRAY;
  HPROGARRAY=record       // hpga
    ahprog: Array[1..1] of HProgram;
  end;

  PROGCATEGORY=Cardinal;       // progc

  PPROGCATEGORY=^PROGCATEGORY;

// values acceptable for PROGCATEGORY for PM groups
const
  PROG_DEFAULT             =0;
  PROG_FULLSCREEN          =1;
  PROG_WINDOWABLEVIO       =2;
  PROG_PM                  =3;
  PROG_GROUP               =5;
  PROG_REAL                =4;
  PROG_VDM                 =4;
  PROG_WINDOWEDVDM         =7;
  PROG_DLL                 =6;
  PROG_PDD                 =8;
  PROG_VDD                 =9;
  PROG_WINDOW_REAL         =10;
  PROG_WINDOW_PROT         =11;
  PROG_30_STD              =11;
  PROG_WINDOW_AUTO         =12;
  PROG_SEAMLESSVDM         =13;
  PROG_30_STDSEAMLESSVDM   =13;
  PROG_SEAMLESSCOMMON      =14;
  PROG_30_STDSEAMLESSCOMMON =14;
  PROG_31_STDSEAMLESSVDM   =15;
  PROG_31_STDSEAMLESSCOMMON =16;
  PROG_31_ENHSEAMLESSVDM   =17;
  PROG_31_ENHSEAMLESSCOMMON =18;
  PROG_31_ENH              =19;
  PROG_31_STD              =20;
  PROG_DOS_GAME            =21;
  PROG_WIN_GAME            =22;
  PROG_DOS_MODE            =23;
  PROG_RESERVED            =255;

type
  PProgType=^ProgType;
  ProgType=record         // progt
    progc: ProgCategory;
    fbVisible: Cardinal;
  end;

// visibility flag for PROGTYPE structure
const
  SHE_VISIBLE        = $00;
  SHE_INVISIBLE      = $01;
  SHE_RESERVED       = $FF;

// Protected group flag for PROGTYPE structure
const
  SHE_UNPROTECTED     =$00;
  SHE_PROTECTED       =$02;

// Structures associated with 'Prf' calls
type
  PPROGDETAILS=^PROGDETAILS;
  PROGDETAILS=record    // progde
    Length: Cardinal;         // set this to sizeof(PROGDETAILS)
    progt: ProgType;
    pszTitle: PChar;       // any of the pointers can be NULL
    pszExecutable: PChar;
    pszParameters: PChar;
    pszStartupDir: PChar;
    pszIcon: PChar;
    pszEnvironment: PChar; // this is terminated by  /0/0
    swpInitial: SWP;     // this replaces XYWINSIZE
  end;

  PPROGTITLE=^PROGTITLE;
  PROGTITLE=record             // progti
    hprog: HProgram;
    progt: ProgType;
    pszTitle: PChar;
  End;

// Program List API Function Definitions

// Program List API available 'Prf' calls

Function PrfQueryProgramTitles(ahini: HINI; hprogGroup: HProgram;
                               var pTitles: PROGTITLE; cchBufferMax: Cardinal;
                               var pulCount: Cardinal): Cardinal; cdecl;
    external 'PMSHAPI' index 113;

//*********************************************************************/
//*  NOTE: string information is concatanated after the array of      */
//*        PROGTITLE structures so you need to allocate storage       */
//*        greater than sizeof(PROGTITLE)*cPrograms to query programs */
//*        in a group.                                                */
//*                                                                   */
//*  PrfQueryProgramTitles recommended usage to obtain titles of all  */
//*  programs in a group (Hgroup=SGH_ROOT is for all groups):         */
//*                                                                   */
//*  BufLen = PrfQueryProgramTitles(Hini, Hgroup,                     */
//*                                  (PPROGTITLE)NULL, 0, &Count);    */
//*                                                                   */
//*  Alocate buffer of  Buflen                                        */
//*                                                                   */
//*  Len = PrfQueryProgramTitles(Hini, Hgroup, (PPROGTITLE)pBuffer,   */
//*                               BufLen, pCount);                    */
//*                                                                   */
//*********************************************************************/

Function PrfAddProgram(ahini: HINI; var pDetails: PROGDETAILS;
                       hprogGroup: HProgram): HProgram; cdecl;
    external 'PMSHAPI' index 109;

Function PrfChangeProgram(ahini: HINI;hprog: HProgram;
                          var pDetails: PROGDETAILS): Longbool; cdecl;
    external 'PMSHAPI' index 110;

Function PrfQueryDefinition(ahini: HINI; hprog: HProgram;
                            var pDetails: PROGDETAILS;
                            cchBufferMax: Cardinal): Cardinal; cdecl;
    external 'PMSHAPI' index 111;

//*********************************************************************/
//*  NOTE: string information is concatanated after the PROGDETAILS   */
//*        field structure so you need to allocate storage greater    */
//*        than sizeof(PROGDETAILS) to query programs                 */
//*                                                                   */
//*  PrfQueryDefinition recomended usage:                             */
//*                                                                   */
//*  bufferlen = PrfQueryDefinition(Hini,Hprog,(PPROGDETAILS)NULL,0)  */
//*                                                                   */
//*  Alocate buffer of bufferlen bytes                                */
//*  set Length field (0 will be supported)                           */
//*                                                                   */
//*  (PPROGDETAILS)pBuffer->Length=sizeof(PPROGDETAILS)               */
//*                                                                   */
//*  len = PrfQueryDefinition(Hini, Hprog, (PPROGDETAILS)pBuffer,     */
//*      bufferlen)                                                   */
//*********************************************************************/

Function PrfRemoveProgram(ahini: HINI; hprog: HProgram): Longbool; cdecl;
    external 'PMSHAPI' index 104;

Function PrfQueryProgramHandle(ahini: HINI; const pszExe: PChar;
                               aphprogArray: HPROGARRAY; cchBufferMax: Cardinal;
                               var pulCount: Cardinal): Longbool; cdecl;
    external 'PMSHAPI' index 58;

Function PrfCreateGroup(ahini: HINI; const pszTitle: PChar;
                        chVisibility: Byte): HProgram; cdecl;
    external 'PMSHAPI' index 55;

Function PrfDestroyGroup(ahini: HINI; hprogGroup: HProgram): Longbool; cdecl;
    external 'PMSHAPI' index 106;

Function PrfQueryProgramCategory(ahini: HINI; const pszExe: PChar): PROGCATEGORY; cdecl;
    external 'PMSHAPI' index 59;

Function WinStartApp(hwndNotify: HWND; var pDetails: PROGDETAILS; const pszParams: PChar;
                     Reserved: Pointer; fbOptions: Cardinal): HAPP; cdecl;
    external 'PMSHAPI' index 119;

// bit values for Options parameter
const
  SAF_VALIDFLAGS  =$001F;

  SAF_INSTALLEDCMDLINE  =$0001;     // use installed parameters
  SAF_STARTCHILDAPP     =$0002;     // related application
  SAF_MAXIMIZED         =$0004;     // Start App maximized
  SAF_MINIMIZED         =$0008;     // Start App minimized, if !SAF_MAXIMIZED
  SAF_BACKGROUND        =$0010;     // Start app in the background


Function WinTerminateApp(ahapp: HAPP): Longbool; cdecl;
    external 'PMSHAPI' index 130;



// switch list section
type
  HSWITCH=Cardinal;        // hsw
  PHSWITCH=^HSWITCH;

  PSWCNTRL=^SWCNTRL;
  SWCNTRL=record          // swctl
    hwnd_: HWND;
    hwndIcon: HWND;
    hprog: HProgram;
    idProcess: Cardinal;
    idSession: Cardinal;
    uchVisibility: Cardinal;
    fbJump: Cardinal;
    szSwtitle: Array[1..MaxNameL+4] of Char;
    bProgType: Cardinal;
  end;

// visibility flag for SWCNTRL structure
const
  SWL_VISIBLE    =$04;
  SWL_INVISIBLE  =$01;
  SWL_GRAYED     =$02;

// jump flag for SWCNTRL structure
const
  SWL_JUMPABLE    =$02;
  SWL_NOTJUMPABLE =$01;

// Switching Program functions
Function WinAddSwitchEntry(VAR aps: SWCNTRL): HSWITCH; cdecl;
    external 'PMSHAPI' index 120;
Function WinRemoveSwitchEntry(ah:HSWITCH): Cardinal; cdecl;
    external 'PMSHAPI' index 129;

type
  PSWENTRY=^SWENTRY;
  SWENTRY=record          // swent
    hswitch_: HSWITCH;
    swctl: SWCNTRL;
  end;

  PSWBLOCK=^SWBLOCK;
  SWBLOCK=record          // swblk
    cswentry: Cardinal;
    aswentry: Array[1..1] of SWENTRY;
  End;

// 32-bit versions of these APIs have 32-bit parameters
Function WinChangeSwitchEntry(hswitchSwitch: HSWITCH;
                              var pswctlSwitchData: SWCNTRL): Cardinal; cdecl;
    external 'PMSHAPI' index 123;

Function WinCreateSwitchEntry(ahab: HAB; var pswctlSwitchData: SWCNTRL): HSWITCH; cdecl;
    external 'PMSHAPI' index 121;

Function WinQuerySessionTitle(ahab: HAB; usSession: Cardinal;
                              var pszTitle: PChar;
                              usTitlelen: Cardinal): Cardinal; cdecl;
    external 'PMSHAPI' index 122;

Function WinQuerySwitchEntry(hswitchSwitch: HSWITCH;
                             var pswctlSwitchData: SWCNTRL): Cardinal; cdecl;
    external 'PMSHAPI' index 124;


Function WinQuerySwitchHandle(ahwnd: HWND; pidProcess: Cardinal): HSWITCH; cdecl;
    external 'PMSHAPI' index 125;

Function WinQuerySwitchList(ahab: HAB;var pswblkSwitchEntries: SWBLOCK;
                            usDataLength: Cardinal): Cardinal; cdecl;
    external 'PMSHAPI' index 126;

Function WinQueryTaskSizePos(ahab: HAB; usScreenGroup: Cardinal;
                             var pswpPositionData: SWP): Cardinal; cdecl;
    external 'PMSHAPI' index 127;

Function WinQueryTaskTitle(usSession: Cardinal; var pszTitle: PChar;
                           usTitlelen: Cardinal): Cardinal; cdecl;
   external 'PMSHAPI' index 128;

Function WinSwitchToProgram(hswitchSwHandle: HSWITCH): Cardinal; cdecl;
    external 'PMSHAPI' index 131;

// OS2.INI Access functions

Function PrfQueryProfileInt(ahini: HINI; const pszApp, pszKey: PChar;
                            sDefault: Longint): Longint; cdecl;
    external 'PMSHAPI' index 114;

Function PrfQueryProfileString(ahini: HINI; const pszApp, pszKey, pszDefault: PChar;
                               var pBuffer; cchBufferMax: Cardinal): Cardinal; cdecl;
    external 'PMSHAPI' index 115;

Function PrfWriteProfileString(ahini: HINI; const pszApp, pszKey, pszData: PChar): Longbool; cdecl;
    external 'PMSHAPI' index 116;

Function PrfQueryProfileSize(ahini: HINI; const pszApp, pszKey: PChar;
                             var pulReqLen: Cardinal): Longbool; cdecl;
    external 'PMSHAPI' index 101;

Function PrfQueryProfileData(ahini: HINI; const pszApp, pszKey: PChar; var pBuffer;
                             var pulBuffLen: Cardinal): Longbool; cdecl;
    external 'PMSHAPI' index 117;

Function PrfWriteProfileData(ahini: HINI; const pszApp, pszKey: PChar; var pData;
                             cchDataLen: Cardinal): Longbool; cdecl;
    external 'PMSHAPI' index 118;

Function PrfOpenProfile(ahab: HAB;const pszFileName: PChar): HINI; cdecl;
    external 'PMSHAPI' index 102;

Function PrfCloseProfile(ahini: HINI): Longbool; cdecl;
    external 'PMSHAPI' index 103;

Function PrfReset(ahab: HAB; var apPrfProfile: PrfProfile): Longbool; cdecl;
    external 'PMSHAPI' index 108;

Function PrfQueryProfile(ahab: HAB; var apPrfProfile: PrfProfile): Longbool; cdecl;
    external 'PMSHAPI' index 107;

// public message, broadcast on WinReset
const
  PL_ALTERED=$008E;  // WM_SHELLFIRST + 0E

Implementation

End.
