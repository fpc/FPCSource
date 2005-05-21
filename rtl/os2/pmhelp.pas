{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Yuri Prokushev (prokushev@freemail.ru).

    OS/2 Presentation Manager Information Presentation Facility,
    Help Manager declarations.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Warning: This code is alfa. Future versions
 of this unit might not be compatible.}

unit pmhelp;

interface

{$MODE OBJFPC}

uses
  os2def;

resourcestring
  msg_failedtodisplay='Failed to display help panel.';
  msg_failedtoload='Failed to load help manager.';

//************************************************************************/
//* HelpSubTable entry structure                                         */
//************************************************************************/
type
  HelpSubTable=Word;
  PHelpSubTable=^HelpSubTable;

//************************************************************************/
//* HelpTable entry structure                                            */
//*                                                                      */
//*  Pack the structure HELPTABLE so that it is identical in the 32-Bit  */
//*  and 16-Bit world.  We have to do this because the HelpTable can     */
//*  reside either in memory or in the application's resources.          */
//************************************************************************/
type
{$PACKRECORDS 2}
  PHelpTable=^HelpTable;
  HelpTable=record
    idAppWindow: Word;
    phstHelpSubTable: PHelpSubTable;
    idExtPanel: Word;
  End;


//************************************************************************/
//* IPF Initialization Structure used on the                             */
//* WinCreateHelpInstance() call.                                        */
//************************************************************************/

type
   PHelpInit=^HelpInit;
   HelpInit=record
     cb: cardinal;
     ulReturnCode: cardinal;
     pszTutorialName: PChar;
     phtHelpTable: PHelpTable;
     hmodHelpTableModule: cardinal;
     hmodAccelActionBarModule: cardinal;
     idAccelTable: cardinal;
     idActionBar: cardinal;
     pszHelpWindowTitle: PChar;
     fShowPanelId: cardinal;
     pszHelpLibraryName: PChar;
   End;

//************************************************************************/
//* Search parent chain indicator for HM_SET_ACTIVE_WINDOW message.      */
//************************************************************************/

const
  HWnd_Parent = 0;

//************************************************************************/
//* Constants used to define whether user wants to display panel using   */
//* panel number or panel name.                                          */
//************************************************************************/

const
  HM_ResourceID = 0;
  HM_PanelName  = 1;

  HMPanelType_Number  =0;
  HMPanelType_Name    =1;

//************************************************************************/
//* Constants used to define how the panel IDs are displayed on          */
//* help panels.                                                         */
//************************************************************************/

const
  CMIC_Hide_Panel_ID        =$0000;
  CMIC_Show_Panel_ID        =$0001;
  CMIC_Toggle_Panel_ID      =$0002;


//************************************************************************/
//* IPF message base.                                                    */
//************************************************************************/
const
  HM_Msg_Base               =$0220;

//************************************************************************/
//* Messages applications can send to the IPF.                           */
//************************************************************************/
const
  HM_Dismiss_Window              =HM_Msg_Base+$0001;
  HM_Display_Help                =HM_Msg_Base+$0002;
  HM_Ext_Help                    =HM_Msg_Base+$0003;
  HM_General_Help                =HM_Ext_Help;
  HM_Set_Active_Window           =HM_Msg_Base+$0004;
  HM_Load_Help_Table             =HM_Msg_Base+$0005;
  HM_Create_Help_Table           =HM_Msg_Base+$0006;
  HM_Set_Help_Window_Title       =HM_Msg_Base+$0007;
  HM_Set_Show_Panel_ID           =HM_Msg_Base+$0008;
  HM_Replace_Help_For_Help       =HM_Msg_Base+$0009;
  HM_Replace_Using_Help          =HM_Replace_Help_For_Help;
  HM_Help_Index                  =HM_Msg_Base+$000a;
  HM_Help_Contents               =HM_Msg_Base+$000b;
  HM_Keys_Help                   =HM_Msg_Base+$000c;
  HM_Set_Help_Library_Name       =HM_Msg_Base+$000d;

  HM_Set_OBJCOM_Window           =HM_Msg_Base+$0018;
  HM_Upadte_OBJCOM_Window_Chain  =HM_Msg_Base+$0019;
  HM_Query_DDF_Data              =HM_Msg_Base+$001a;
  HM_Invalidate_DDF_Data         =HM_Msg_Base+$001b;
  HM_Query                       =HM_Msg_Base+$001c;
  HM_Set_CoverPage_Size          =HM_Msg_Base+$001d;

//************************************************************************/
//* Constants used to query the info from IPF in HM_QUERY message        */
//************************************************************************/

//* Hi word in lParam 1 */
const
  HMQW_COVERPAGE           =$0001;
  HMQW_INDEX               =$0002;
  HMQW_TOC                 =$0003;
  HMQW_SEARCH              =$0004;
  HMQW_VIEWPAGES           =$0005;
  HMQW_LIBRARY             =$0006;
  HMQW_VIEWPORT            =$0007;
  HMQW_OBJCOM_WINDOW       =$0008;
  HMQW_INSTANCE            =$0009;
  HMQW_ACTIVEVIEWPORT      =$000a;
  CONTROL_SELECTED         =$000b;

  HMQW_GROUP_VIEWPORT      =$00f1;
  HMQW_RES_VIEWPORT        =$00f2;
  USERDATA                 =$00f3;

//* Lo word in lParam1 of HMQW_VIEWPORT */
  HMQVP_NUMBER             =$0001;
  HMQVP_NAME               =$0002;
  HMQVP_GROUP              =$0003;

//************************************************************************/
//* Predefined Control IDs                                               */
//************************************************************************/

const
  CTRL_PREVIOUS_ID             =$0001;
  CTRL_SEARCH_ID               =$0002;
  CTRL_PRINT_ID                =$0003;
  CTRL_INDEX_ID                =$0004;
  CTRL_CONTENTS_ID             =$0005;
  CTRL_BACK_ID                 =$0006;
  CTRL_FORWARD_ID              =$0007;
  CTRL_TUTORIAL_ID             =$00FF;

  CTRL_USER_ID_BASE            =257;

//************************************************************************/
//* Messages the IPF sends to the applications active window             */
//* as defined by the IPF.                                               */
//************************************************************************/

const
  HM_ERROR                       =HM_Msg_Base+$000e;
  HM_HELPSUBITEM_NOT_FOUND       =HM_Msg_Base+$000f;
  HM_QUERY_KEYS_HELP             =HM_Msg_Base+$0010;
  HM_TUTORIAL                    =HM_Msg_Base+$0011;
  HM_EXT_HELP_UNDEFINED          =HM_Msg_Base+$0012;
  HM_GENERAL_HELP_UNDEFINED      =HM_EXT_HELP_UNDEFINED;
  HM_ACTIONBAR_COMMAND           =HM_Msg_Base+$0013;
  HM_INFORM                      =HM_Msg_Base+$0014;
  HM_NOTIFY                      =HM_Msg_Base+$0022;
  HM_SET_USERDATA                =HM_Msg_Base+$0023;
  HM_CONTROL                     =HM_Msg_Base+$0024;

//**********************************************************************/
//* notify information for HM_NOTIFY                                   */
//**********************************************************************/
const
  OPEN_COVERPAGE        =$0001;
  OPEN_PAGE             =$0002;
  SWAP_PAGE             =$0003;
  OPEN_TOC              =$0004;
  OPEN_INDEX            =$0005;
  OPEN_HISTORY          =$0006;
  OPEN_SEARCH_HIT_LIST  =$0007;
  OPEN_LIBRARY          =$0008;
  HELP_REQUESTED        =$0009;

//**********************************************************************/
//* HMERR_NO_FRAME_WND_IN_CHAIN - There is no frame window in the      */
//* window chain from which to find or set the associated help         */
//* instance.                                                          */
//**********************************************************************/
const
  HMERR_NO_FRAME_WND_IN_CHAIN                =$00001001;

//*********************************************************************/
//* HMERR_INVALID_ASSOC_APP_WND - The application window handle       */
//* specified on the WinAssociateHelpInstance() call is not a valid   */
//* window handle.                                                    */
//*********************************************************************/
const
  HMERR_INVALID_ASSOC_APP_WND                =$00001002;

//**********************************************************************/
//* HMERR_INVALID_ASSOC_HELP_INST - The help instance handle specified */
//* on the WinAssociateHelpInstance() call is not a valid              */
//* window handle.                                                     */
//**********************************************************************/
const
  HMERR_INVALID_ASSOC_HELP_INST              =$00001003;

//**********************************************************************/
//* HMERR_INVALID_DESTROY_HELP_INST - The window handle specified      */
//* as the help instance to destroy is not of the help instance class. */
//**********************************************************************/
const
  HMERR_INVALID_DESTROY_HELP_INST            =$00001004;

//**********************************************************************/
//* HMERR_NO_HELP_INST_IN_CHAIN - The parent or owner chain of the     */
//* application window specified does not have a help instance         */
//* associated with it.                                                */
//**********************************************************************/
const
  HMERR_NO_HELP_INST_IN_CHAIN                =$00001005;

//**********************************************************************/
//* HMERR_INVALID_HELP_INSTANCE_HDL - The handle specified to be a     */
//* help instance does not have the class name of a IPF                */
//* help instance.                                                     */
//**********************************************************************/
const
  HMERR_INVALID_HELP_INSTANCE_HDL            =$00001006;

//*********************************************************************/
//* HMERR_INVALID_QUERY_APP_WND - The application window specified on */
//* a WinQueryHelpInstance() call is not a valid window handle.       */
//*********************************************************************/
const
  HMERR_INVALID_QUERY_APP_WND                =$00001007;

//*********************************************************************/
//* HMERR_HELP_INST_CALLED_INVALID -  The handle of the help instance */
//* specified on an API call to the IPF does not have the             */
//* class name of an IPF help instance.                               */
//*********************************************************************/
const
  HMERR_HELP_INST_CALLED_INVALID             =$00001008;

  HMERR_HELPTABLE_UNDEFINE                   =$00001009;
  HMERR_HELP_INSTANCE_UNDEFINE               =$0000100a;
  HMERR_HELPITEM_NOT_FOUND                   =$0000100b;
  HMERR_INVALID_HELPSUBITEM_SIZE             =$0000100c;
  HMERR_HELPSUBITEM_NOT_FOUND                =$0000100d;

//*********************************************************************/
//* HMERR_INDEX_NOT_FOUND - No index in library file.                 */
//*********************************************************************/
const
  HMERR_INDEX_NOT_FOUND                      =$00002001;

//**********************************************************************/
//* HMERR_CONTENT_NOT_FOUND - Library file does not have any contents. */
//**********************************************************************/
const
  HMERR_CONTENT_NOT_FOUND                    =$00002002;

//*********************************************************************/
//* HMERR_OPEN_LIB_FILE     - Cannot open library file                */
//*********************************************************************/
const
  HMERR_OPEN_LIB_FILE                        =$00002003;

//*********************************************************************/
//* HMERR_READ_LIB_FILE     - Cannot read library file                */
//*********************************************************************/
const
  HMERR_READ_LIB_FILE                        =$00002004;

//*********************************************************************/
//* HMERR_CLOSE_LIB_FILE    - Cannot close library file               */
//*********************************************************************/
const
  HMERR_CLOSE_LIB_FILE                       =$00002005;

//*********************************************************************/
//* HMERR_INVALID_LIB_FILE  - Improper library file provided          */
//*********************************************************************/
const
  HMERR_INVALID_LIB_FILE                     =$00002006;

//************************************************************************/
//* HMERR_NO_MEMORY - Unable to allocate the requested amount of memory. */
//************************************************************************/
const
  HMERR_NO_MEMORY                            =$00002007;

//*********************************************************************/
//* HMERR_ALLOCATE_SEGMENT - Unable                                   */
//* to allocate a segment of memory for memory allocation requested   */
//* from the IPF.                                                     */
//*********************************************************************/
const
  HMERR_ALLOCATE_SEGMENT                     =$00002008;

//*********************************************************************/
//* HMERR_FREE_MEMORY - Unable to free allocated  memory              */
//*********************************************************************/
const
  HMERR_FREE_MEMORY                          =$00002009;

//*********************************************************************/
//* HMERR_PANEL_NOT_FOUND  - Unable                                   */
//* to find a help panel requested to help manager                    */
//*********************************************************************/
const
  HMERR_PANEL_NOT_FOUND                      =$00002010;

//*********************************************************************/
//* HMERR_DATABASE_NOT_OPEN - Unable to read the unopened database    */
//*********************************************************************/
const
  HMERR_DATABASE_NOT_OPEN                    =$00002011;

//*********************************************************************/
//* HMERR_DDL_ERROR - Unable to load resource dll                     */
//*********************************************************************/
const
  HMERR_LOAD_DLL                              =$00002013;

//********************************************************************/
//* AC Viewport stucture definitions                                 */
//********************************************************************/
type
  PACVP=^ACVP;
  ACVP=record
    cb: cardinal;
    hAB: HAB;
    hmq: HMQ;
    ObjectID: cardinal;         // object identifier
    hWndParent: HWND;        // IPF viewport client handle
    hWndOwner: HWND;         // IPF viewport client handle
    hWndACVP: HWND;          // applications frame window hwnd
  end;

//*******************************************************************/
//* Define Handle to DDF                                            */
//*******************************************************************/
Type
  HDDF=pointer;

// DdfHyperText Flags
const
  REFERENCE_BY_ID     =0;
  REFERENCE_BY_RES    =1;

// DdfBeginList formatting flags
  HMBT_NONE           =1;
  HMBT_ALL            =2;
  HMBT_FIT            =3;

  HMLS_SINGLELINE     =1;
  HMLS_DOUBLELINE     =2;

// DdfBitmap alignment flags
  ART_RUNIN           =$10;
  ART_LEFT            =$01;
  ART_RIGHT           =$02;
  ART_CENTER          =$04;

// DdfSetColor Color Flag
  CLR_UNCHANGED       =-6;

//*******************************************************************/
// error codes returned by DDF API functions                        */
//*******************************************************************/
const
  HMERR_DDF_MEMORY                  =$3001;
  HMERR_DDF_ALIGN_TYPE              =$3002;
  HMERR_DDF_BACKCOLOR               =$3003;
  HMERR_DDF_FORECOLOR               =$3004;
  HMERR_DDF_FONTSTYLE               =$3005;
  HMERR_DDF_REFTYPE                 =$3006;
  HMERR_DDF_LIST_UNCLOSED           =$3007;
  HMERR_DDF_LIST_UNINITIALIZED      =$3008;
  HMERR_DDF_LIST_BREAKTYPE          =$3009;
  HMERR_DDF_LIST_SPACING            =$300A;
  HMERR_DDF_HINSTANCE               =$300B;
  HMERR_DDF_EXCEED_MAX_LENGTH       =$300C;
  HMERR_DDF_EXCEED_MAX_INC          =$300D;
  HMERR_DDF_INVALID_DDF             =$300E;
  HMERR_DDF_FORMAT_TYPE             =$300F;
  HMERR_DDF_INVALID_PARM            =$3010;
  HMERR_DDF_INVALID_FONT            =$3011;
  HMERR_DDF_SEVERE                  =$3012;

//************************************************************************/
//* Window Help API declarations.                                        */
//************************************************************************/

function WinDestroyHelpInstance(hwndHelpInstance: HWND): Longbool; cdecl;
function WinCreateHelpInstance(ahab: HAB; var phinitHMInitStructure: HELPINIT): HWND; cdecl;
function WinAssociateHelpInstance(hwndHelpInstance, hwndApp: HWND): Longbool; cdecl;
function WinQueryHelpInstance(hwndApp: HWND): HWND; cdecl;
function WinLoadHelpTable(hwndHelpInstance: HWND; idHelpTable: cardinal; Module: cardinal): Longbool; cdecl;
function WinCreateHelpTable(hwndHelpInstance: HWND; var phtHelpTable: HELPTABLE): Longbool; cdecl;
function DdfInitialize(hwndHelpInstance: HWND; cbBuffer, ulIncrement: cardinal): HDDF; cdecl;
function DdfPara(ahddf: HDDF):Longbool; cdecl;
function DdfSetFormat(ahddf: HDDF; fFormatType: cardinal): Longbool; cdecl;
function DdfSetTextAlign(ahddf: HDDF; fAlign: cardinal): Longbool; cdecl;
function DdfSetColor(ahddf: HDDF; fBackColor, fForColor: Longint): Longbool; cdecl;
function DdfInform(ahddf: HDDF; var pszText: PChar; resInformNumber: cardinal): Longbool; cdecl;
function DdfSetFontStyle(ahddf: HDDF; fFontStyle: cardinal): Longbool; cdecl;
function DdfHyperText(ahddf: HDDF; var pszText, pszReference: PChar; fReferenceType: cardinal): Longbool; cdecl;
function DdfBeginList(ahddf: HDDF; ulWidthDT, fBreakType, fSpacing: cardinal): Longbool; cdecl;
function DdfListItem(ahddf: HDDF; var pszTerm, pszDescription: PChar): Longbool; cdecl;
function DdfEndList(ahddf: HDDF): Longbool; cdecl;
function DdfMetafile(ahddf: HDDF; ahmf: cardinal; var prclRect: RECTL): Longbool; cdecl;
function DdfText(ahddf: HDDF; var pszText: PChar): Longbool; cdecl;
function DdfSetFont(ahddf: HDDF; var pszFaceName: PChar; ulWidth, ulHeight: cardinal): Longbool; cdecl;
function DdfBitmap(ahddf: HDDF; hbm: HBITMAP; fAlign: cardinal): Longbool; cdecl;

implementation

const
  HELPMGRDLL='HELPMGR';

function WinDestroyHelpInstance(hwndHelpInstance: HWND): Longbool; cdecl;
    external HELPMGRDLL index 52;
function WinCreateHelpInstance(ahab: HAB; var phinitHMInitStructure: HELPINIT): HWND; cdecl;
    external HELPMGRDLL index 51;
function WinAssociateHelpInstance(hwndHelpInstance, hwndApp: HWND): Longbool; cdecl;
    external HELPMGRDLL index 54;
function WinQueryHelpInstance(hwndApp: HWND): HWND; cdecl;
    external HELPMGRDLL index 53;
function WinLoadHelpTable(hwndHelpInstance: HWND; idHelpTable: cardinal; Module: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 55;
function WinCreateHelpTable(hwndHelpInstance: HWND; var phtHelpTable: HELPTABLE): Longbool; cdecl;
    external HELPMGRDLL index 56;
function DdfInitialize(hwndHelpInstance: HWND; cbBuffer, ulIncrement: cardinal): HDDF; cdecl;
    external HELPMGRDLL index 74;
function DdfPara (ahddf: HDDF): Longbool; cdecl;
    external HELPMGRDLL index 75;
function DdfSetFormat(ahddf: HDDF; fFormatType: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 76;
function DdfSetTextAlign (ahddf: HDDF; fAlign: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 77;
function DdfSetColor(ahddf: HDDF; fBackColor, fForColor: Longint): Longbool; cdecl;
    external HELPMGRDLL index 78;
function DdfInform(ahddf: HDDF; var pszText: PChar; resInformNumber: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 79;
function DdfSetFontStyle(ahddf: HDDF; fFontStyle: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 80;
function DdfHyperText(ahddf: HDDF; var pszText, pszReference: PChar; fReferenceType: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 81;
function DdfBeginList(ahddf: HDDF; ulWidthDT, fBreakType, fSpacing: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 82;
function DdfListItem(ahddf: HDDF; var pszTerm, pszDescription: PChar): Longbool; cdecl;
    external HELPMGRDLL index 83;
function DdfEndList(ahddf: HDDF): Longbool; cdecl;
    external HELPMGRDLL index 84;
function DdfMetafile(ahddf: HDDF; ahmf: cardinal; var prclRect: RECTL): Longbool; cdecl;
    external HELPMGRDLL index 86;
function DdfText(ahddf: HDDF; var pszText: PChar): Longbool; cdecl;
    external HELPMGRDLL index 85;
function DdfSetFont(ahddf: HDDF; var pszFaceName: PChar; ulWidth, ulHeight: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 87;
function DdfBitmap(ahddf: HDDF; hbm: HBITMAP; fAlign: cardinal): Longbool; cdecl;
    external HELPMGRDLL index 88;

end.
