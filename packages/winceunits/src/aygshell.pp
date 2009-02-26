{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006-2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Declarations for aygshell WinCE API

}

{exported functions list = to do,
 * please remove functions done *
 
     Exports

       ordinal    name

           146    ?ClearFontManager@@YAXXZ (void __cdecl ClearFontManager(void))
           166    ?CreateBackgroundSpec@@YAJPAPAVIBackgroundSpec@@@Z (long __cdecl CreateBackgroundSpec(class IBackgroundSpec * *))
           148    ?GetAppMetric@@YAHW4_enAppMetricID@@@Z (int __cdecl GetAppMetric(enum _enAppMetricID))
           145    ?GetStandardFont@@YAJW4eFontID@@PAPAUHFONT__@@@Z (long __cdecl GetStandardFont(enum eFontID,struct HFONT__ * *))
           348    ?IsScreenRotationSupported@@YAHXZ (int __cdecl IsScreenRotationSupported(void))
            63    ?MinPowerOff@@YAHXZ (int __cdecl MinPowerOff(void))
           147    ?OnSettingChange@@YAHIJ@Z (int __cdecl OnSettingChange(unsigned int,long))
           189    ?SHDrawGradientBubbleTitle@@YAXPAUtagGRADIENTTITLEINFO@@@Z (void __cdecl SHDrawGradientBubbleTitle(struct tagGRADIENTTITLEINFO *))
          2006    ?SHIdleTimerReset@@YAXXZ (void __cdecl SHIdleTimerReset(void))
           228    ?SHLoadMenuExtensions@@YAHPAUIUnknown@@PBG1PAPAX@Z (int __cdecl SHLoadMenuExtensions(struct IUnknown *,unsigned short const *,unsigned short const *,void * *))
            66    ?SHMenuBar_GetMenu@@YAPAUHMENU__@@PAUHWND__@@H@Z (struct HMENU__ * __cdecl SHMenuBar_GetMenu(struct HWND__ *,int))
           140    ADChgTaskList
           139    ADChgTrustedSrcList
           142    ADRegisterCallback
           141    ADTaskInfo
           143    AssociateNoteWithCall
            52    CancelSIPUp
            46    ComboBoxEditSubProc
            29    ComboEditAutoComplete
            45    ComboSubProc
           190    CreateImageCache
           330    DPI_ExtractIconEx
           311    DPI_LoadImageFile
           310    DPI_LoadLibraryRes
           237    DisplayNotRunnableAppDialog
           226    DoEditContextMenu
           191    DrawAlignedIcon
            44    EditSubProc
            13    FreeRegisteredAppInfo
           137    GetProtocol
            11    GetRegisteredAppInfo
           286    HIDPI_ImageList_LoadImage
           287    HIDPI_ImageList_ReplaceIcon
            92    IsFullScreenWindow
           347    IsModulePreWinCE421
            62    IsSANMessage
            70    LFHeightForPoint
           302    LoadHTML
           241    LoadStringEtcOver
           180    NotifyAppsOnEvent
            24    PathAddBackslash
            26    PathCombine
            23    PathFindExtension
            27    PathFindFileName
           160    PathFindNextComponent
           116    PathIsPrefix
            28    PathIsRelative
            25    PathRemoveBackslash
            42    PathRemoveBlanks
           107    PhoneGetCallProperties
           106    PhoneGetCallPropertyBag
           159    PhoneShowCallLog
           158    SHAnimateListviewOpen
           117    SHAnimateRects
            22    SHAppNotifyDone
           130    SHBoldFontAllowed
           289    SHBorderPolyline
           288    SHBorderRectangle
           104    SHBox
           103    SHBoxEx
           114    SHChangeNotifyDeregister
           115    SHChangeNotifyFree
           113    SHChangeNotifyRegister
            48    SHCheckForContextMenu
            86    SHClearStartedBit
            55    SHColorDisplay
            37    SHCommandBar_EnableCommand
            38    SHCommandBar_GetClientRect
            36    SHCommandBar_GetCommandBarByID
           223    SHCopyBitmap
           306    SHCopyIcon
           164    SHCreateCOleWindow
            41    SHCreateContextMenu
            43    SHCreateMainWindow
            74    SHCreateMenuBarInternal
           108    SHCreateNewItem
            53    SHCreateSystemFont
            35    SHCreateWorkerWindow
           329    SHDeleteTodayWallpaper
           109    SHDocManagerCreate
           112    SHDocManagerDestroy
           111    SHDocManagerQuery
           110    SHDocManagerRegister
           233    SHDrawBranding
           234    SHDrawClippedText
           331    SHDrawTextOverImage
            58    SHDrawUnderline
           136    SHDrawUnderlineColor
            49    SHEnableEditMenu
           192    SHEnableRadio
            81    SHEndProfileObj
          2013    SHEnumFiles
          2020    SHEnumFolders
            98    SHEnumPropSheetHandlers
           203    SHEscapeAccelerators
           201    SHEscapeBubbleHtml
           168    SHFadeImage
            54    SHFillRectClr
           132    SHFindForegroundMenuBar
           283    SHFindMenuBarInternal
           209    SHFindPreviousInstance
           212    SHFindPreviousInstanceEx
            71    SHFontMgrCreate
            73    SHFontMgrDestroy
            72    SHFontMgrManageFonts
            33    SHForceBaseState
            99    SHForceBaseStateEx
           102    SHFreeContextMenuExtensions
           120    SHFreeScanners
           213    SHGetActiveDialog
           292    SHGetBitmapDimensions
           285    SHGetBitmapLogPixels
           225    SHGetCarrierBranding
           224    SHGetCarrierBrandingFlag
            96    SHGetDeviceFeatureLevel
           299    SHGetDisplayRotation
           128    SHGetEmergencyCallList
           218    SHGetFontHeight
           232    SHGetInputContext
           133    SHGetKOBits
           305    SHGetLandscapeRotationSettings
           345    SHGetLegacySupportWindow
           242    SHGetLocaleInfo
           167    SHGetMessageBoxIcon
           281    SHGetMetric
           172    SHGetNavBarItemRect
           163    SHGetPowerOnTime
           153    SHGetPresetMessage
           282    SHGetScreenOrientation
           204    SHGetSimToolkitMenu
          2014    SHGetSoundFileList
           177    SHGetStyleBkColor
           178    SHGetStyleColor
           179    SHGetStyleFont
           217    SHGetSystemDefaultLCID
           144    SHGetTimeFormat
           312    SHGetUIMetrics
           219    SHGetUiInfo
           221    SHGradientDeInit
           222    SHGradientDraw
           220    SHGradientInit
            31    SHHandleActivate
            30    SHHandleSipChange
            76    SHHdrGrpSepLineDraw
             8    SHImListPopup
           149    SHInitPresetMessages
           181    SHInsertPresetMessage
           129    SHInvalidateScreen
           101    SHInvokeContextMenuCommand
           123    SHIsLocked
           295    SHIsPreOzoneUpdate
            94    SHIsPreRapierApp
           100    SHLoadContextMenuExtensions
           230    SHLoadFileContextMenuExtensions
           313    SHLoadFontFromResource
            91    SHLoadMenuPopup
           216    SHLoadSKFromReg
           121    SHLock
           138    SHMakeCall
           235    SHMakeValidFilename
            32    SHMessageBox
            80    SHNewProfileObj
           155    SHNotificationAdd
           173    SHNotificationGetData
           157    SHNotificationRemove
           156    SHNotificationUpdate
           208    SHNotifyAppsOnCallConnect
           210    SHNotifyAppsOnCarkit
           165    SHNotifyAppsOnDock
           195    SHNotifyAppsOnHeadset
           214    SHNotifyAppsOnIncomingCall
           211    SHNotifyAppsOnSpeakerPhone
           135    SHOnFullScreenAppActivate
           238    SHOnMissedCallCountChange
          2008    SHOnPluginDataChange
           227    SHOnVoiceMailCountChange
           162    SHPaintBubbleFrame
           294    SHPolyline
           152    SHPopulatePresetMessageMenu
           175    SHPreProcessLogFont
           229    SHQueryMenuExtensions
           297    SHRCMLDialogProc
           293    SHRectangle
          2022    SHRefreshStartMenu
           215    SHRegGetHLMDWValue
           176    SHRegSetValueEx
           346    SHReleaseLegacySupportWindow
           150    SHReleasePresetMessages
           202    SHReplaceString
           245    SHResizeDialogProc
            50    SHRunCpl
            82    SHRunFontManager
           151    SHRunPresetMessagesEdit
           239    SHRunSafeApplet
           174    SHSameWindowProcesses
            95    SHSavePWWarning
           118    SHScanBuffer
           119    SHScanFile
            97    SHSendBackToFocusWindow
           169    SHSetAsWatermark
           184    SHSetBack
           161    SHSetBubbleRegion
           298    SHSetDisplayRotation
           131    SHSetForegroundLastActivePopup
           231    SHSetInputContext
           134    SHSetKOBits
           154    SHSetPresetMessage
           170    SHSetSimToolkitMenu
          1003    SHSetSoftKey
           300    SHSetStretchMode
            59    SHSetWindowBits
            67    SHShowContextMenu
          1004    SHShowSoftKeys
            79    SHSignalDone
           314    SHSipMightBlockUI
           308    SHSkipDialogInitialFocus
           207    SHSoundManGetDisplayName
           205    SHSoundManGetDisplayNameList
           206    SHSoundManGetFileName
            93    SHStartAndBlock
            85    SHStartIfNeeded
            87    SHStartProfile
           284    SHStretchBitmap
           290    SHStretchBltBitmap
           291    SHStretchBltBitmapEx
           307    SHStretchIcon
           105    SHTextBox
           171    SHToolkitQueryShell
            57    SHTrackPopupMenu
          1000    SHTurnScreenOn
           240    SHUnEscapeAccelerators
           122    SHUnlock
          2003    SHVoiceTagDelete
          2002    SHVoiceTagPlayback
          2001    SHVoiceTagRecognize
          2000    SHVoiceTagTrain
           124    SHWriteLockState
           301    SetDialogAutoScrollBar
            12    SetRegisteredAppInfo
           296    SetWindowPosOnRotate
            15    Shell_Alloc
            17    Shell_AllocString
            18    Shell_CatStrAlloc
            16    Shell_Free
            14    Shell_HeapCreate
            19    Shell_LoadStringAlloc
            20    Shell_RegAllocString
            51    StrStrI
            47    SubClassThisWindow
           199    TZFindClose
           197    TZFindNext
           196    TZFindOpen
           198    TZGetData
          1005    UIHGetTextToStruct
          1007    UIHLimitTextControls
          1008    UIHSetHWNDToStruct
          1006    UIHSetTextFromStruct
           236    VerifyTrust
}

unit aygshell;

interface

uses windows;

{$calling cdecl}

//*****************************************************************************
// consts
//*****************************************************************************
const
  UserDLLAyg    = 'aygshell';

  CEM_UPCASEALLWORDS     = (WM_USER + 1);
  CEM_ENABLEUPCASE       = (WM_USER + 2);

  {Gesture notifications}
  GN_CONTEXTMENU = 1000;

  IDC_COMMANDBANDS       = 100;
  {Shell Menubar support}
  // These defines MUST be < 100.  This is so apps can use these defines
  // to get strings from the shell.
  IDS_SHNEW       =  1;
  IDS_SHEDIT      =  2;
  IDS_SHTOOLS     =  3;
  IDS_SHVIEW      =  4;
  IDS_SHFILE      =  5;
  IDS_SHGO        =  6;
  IDS_SHFAVORITES =  7;
  IDS_SHOPEN      =  8;

  {Shared New menu support}
  IDM_SHAREDNEW        = 10;
  IDM_SHAREDNEWDEFAULT = 11;

  NOMENU                  = $FFFF;
  SHA_INPUTDIALOG         = $0001;
  SHACTI_FSIPUP           = $0001;
  SHACTI_FSIPONDEACTIVATE = $0002;
  SHACTI_FSIPRESERVED     = $FFFF xor SHACTI_FSIPUP xor SHACTI_FSIPUP;

  {Valid dwFlags}
  SHCMBF_EMPTYBAR      = $0001;
  SHCMBF_HIDDEN        = $0002; // create it hidden
  SHCMBF_HIDESIPBUTTON = $0004;
  SHCMBF_COLORBK       = $0008;
  SHCMBF_HMENU         = $0010; // specify an hmenu for resource rather than toolbar info

  SHCMBM_SETSUBMENU    = (WM_USER + 400); // wparam == id of button, lParam == hmenu, return is old hmenu
  SHCMBM_GETSUBMENU    = (WM_USER + 401); // lParam == ID
  SHCMBM_GETMENU       = (WM_USER + 402); // get the owning hmenu (as specified in the load resource)
  SHCMBM_OVERRIDEKEY   = (WM_USER + 403);
  SHCMBM_SETBKCOLOR    = (WM_USER + 406); // lParam == COLORREF


  {Valid mask values}
  SHIDIM_FLAGS         = $0001;
  {Valid flags}
  SHIDIF_DONEBUTTON          = $0001;
  SHIDIF_SIZEDLG             = $0002;
  SHIDIF_SIZEDLGFULLSCREEN   = $0004;
  SHIDIF_SIPDOWN             = $0008;
  SHIDIF_FULLSCREENNOMENUBAR = $0010;
  SHIDIF_EMPTYMENU           = $0020;
  SHIDIF_WANTSCROLLBAR       = $0040;

  SHMBOF_NODEFAULT  = $00000001; // do not do default handling of this key
  SHMBOF_NOTIFY     = $00000002; // send us the WM_* messages for this key

  {Gesture flags}
  SHRG_RETURNCMD      = $00000001;
  SHRG_NOTIFYPARENT   = $00000002;
  SHRG_LONGDELAY      = $00000008;
  SHRG_NOANIMATION    = $00000010;

  {Sip info}
  SIP_STATUS_UNAVAILABLE = 0;
  SIP_STATUS_AVAILABLE  = 1;

  SIPF_OFF    = $00000000;
  SIPF_ON     = $00000001;
  SIPF_DOCKED = $00000002;
  SIPF_LOCKED = $00000004;

  {Supported system parameters}
  SPI_SETCOMPLETIONINFO  = 223;
  SPI_SETSIPINFO         = 224;
  SPI_GETSIPINFO         = 225;
  SPI_SETCURRENTIM       = 226;
  SPI_GETCURRENTIM       = 227;
  SPI_APPBUTTONCHANGE    = 228;
  SPI_RESERVED           = 229;
  SPI_SYNCSETTINGSCHANGE = 230;

  WC_SIPPREF             = 'SIPPREF';

  {SHFullScreen - valide states}
  SHFS_SHOWTASKBAR     = $0001;
  SHFS_HIDETASKBAR     = $0002;
  SHFS_SHOWSIPBUTTON   = $0004;
  SHFS_HIDESIPBUTTON   = $0008;
  SHFS_SHOWSTARTICON   = $0010;
  SHFS_HIDESTARTICON   = $0020;

  { DoneButton - Valid state}
  SHDB_SHOW            = $0001;
  SHDB_HIDE            = $0002;
  SHDB_SHOWCANCEL      = $0004;   // added by Windows Mobile 5.0
  WS_NONAVDONEBUTTON   = WS_MINIMIZEBOX;

//*****************************************************************************
// types
//*****************************************************************************

type
  //Struct sent through WM_NOTIFY when SHRG_NOTIFYPARENT is used
  NMRGINFO = record
    hdr : NMHDR;
    ptAction : POINT;
    dwItemSpec : DWORD;
  end;
  PNMRGINFO=^NMRGINFO;

  SHACTIVATEINFO = record
    cbSize : DWORD;
    hwndLastFocus : HWND ;
    bits : Longint ;
  end;
  PSHACTIVATEINFO=^SHACTIVATEINFO;

  SHINITDLGINFO = record
    dwMask: DWORD;
    hDlg: HWND;
    dwFlags : DWORD ;
   end;
  PSHINITDLGINFO=^SHINITDLGINFO;

  SHMENUBARINFO = record
    cbSize : DWORD;        // IN  - Indicates which members of struct are valid
    hwndParent : HWND ;    // IN
    dwFlags : DWORD ;      // IN  - Some features we want
    nToolBarId : UINT ;    // IN  - Which toolbar are we using
    hInstRes : HINST;      // IN  - Instance that owns the resources
    nBmpId : longint;
    cBmpImages : longint;  // IN  - Count of bitmap images
    hwndMB : HWND ;        // OUT
    clrBk : COLORREF ;     // IN  - background color of the menu bar (excluding sip)
  end;
  PSHMENUBARINFO=^SHMENUBARINFO;

  {SHRecognizeGesture}
  SHRGI = record
    cbSize     : DWORD;
    hwndClient : HWND ;
    ptDown     : POINT;
    dwFlags    : DWORD;
  end;
  SHRGINFO=SHRGI;
  PSHRGINFO=^SHRGI;

  {Sip info}
  SIPINFO = record
    cbSize           : DWORD;
    fdwFlags         : DWORD;
    rcVisibleDesktop : RECT;
    rcSipRect        : RECT;
    dwImDataSize     : DWORD;
    pvImData         : pointer;
  end;
  TSIPINFO=SIPINFO;
  PSIPINFO=^SIPINFO;
  
  SIPSTATE= (SIP_UP= 0,SIP_DOWN,SIP_FORCEDOWN,SIP_UNCHANGED,SIP_INPUTDIALOG);

  CAMERACAPTURE_STILLQUALITY= (CAMERACAPTURE_STILLQUALITY_DEFAULT=0, CAMERACAPTURE_STILLQUALITY_LOW, CAMERACAPTURE_STILLQUALITY_NORMAL,
    CAMERACAPTURE_STILLQUALITY_HIGH);

  CAMERACAPTURE_VIDEOTYPE= (CAMERACAPTURE_VIDEOTYPE_ALL = $FFFF, CAMERACAPTURE_VIDEOTYPE_STANDARD = 1,
    CAMERACAPTURE_VIDEOTYPE_MESSAGING = 2);

  CAMERACAPTURE_MODE= (CAMERACAPTURE_MODE_STILL = 0, CAMERACAPTURE_MODE_VIDEOONLY, CAMERACAPTURE_MODE_VIDEOWITHAUDIO);

  TSHCAMERACAPTURE = record
    cbSize             : DWORD;
    hwndOwner          : HWND;
    szFile             : array[0..(MAX_PATH)-1] of WCHAR;
    pszInitialDir      : LPCTSTR;
    pszDefaultFileName : LPCTSTR;
    pszTitle           : LPCTSTR;
    StillQuality       : CAMERACAPTURE_STILLQUALITY;
    VideoTypes         : CAMERACAPTURE_VIDEOTYPE;
    nResolutionWidth   : DWORD;
    nResolutionHeight  : DWORD;
    nVideoTimeLimit    : DWORD;
    Mode               : CAMERACAPTURE_MODE;
  end;
  PSHCAMERACAPTURE=^TSHCAMERACAPTURE;


//*****************************************************************************
// functions
//*****************************************************************************

function ExitWindowsEx(uFlags:UINT; dwReserved:DWORD):WINBOOL; external UserDLLAyg name 'ExitWindowsEx';
function SHCloseApps( dwMemSought : DWORD ): WINBOOL; external UserDLLAyg name 'SHCloseApps';
function SHCreateMenuBar(pmbi : PSHMENUBARINFO ): WINBOOL; external UserDLLAyg name 'SHCreateMenuBar';
function SHDoneButton(hwndRequester: HWND ; dwState : DWORD ): WINBOOL; external UserDLLAyg name 'SHDoneButton';
function SHFindMenuBar(hwnd:HWND) : HWND; external UserDLLAyg name 'SHFindMenuBar';
function SHFullScreen(hwmdRequester: hWnd; dwState: DWord): WINBOOL; external UserDLLAyg name 'SHFullScreen';  {Pocket PC  special controls}
function SHGetAutoRunPath( pAutoRunPath : LPTSTR ): WINBOOL; external UserDLLAyg name 'SHGetAutoRunPath';  
function SHHandleWMActivate(hwnd:HWND; wParam:WPARAM; lParam:LPARAM; psai: PSHACTIVATEINFO; dwFlags:DWORD  ): WINBOOL; external UserDLLAyg index 84;
function SHHandleWMSettingChange(hwnd:HWND; wParam:WPARAM; lParam:LPARAM; psai: PSHACTIVATEINFO): WINBOOL; external UserDLLAyg index 83;
function SHInitDialog(pshidi: PSHINITDLGINFO): WINBOOL; external UserDLLAyg name 'SHInitDialog';
function SHInitExtraControls: WINBOOL; external UserDLLAyg name 'SHInitExtraControls';
procedure SHInputDialog(hwnd : HWND; uMsg : UINT; wParam: WPARAM ); external UserDLLAyg name 'SHInputDialog';
function SHGetAppKeyAssoc( ptszApp: LPCTSTR ): Byte; external UserDLLAyg name 'SHGetAppKeyAssoc';
function SHSetAppKeyWndAssoc( bVk: BYTE ; hwnd : HWND ): WINBOOL; external UserDLLAyg name 'SHSetAppKeyWndAssoc';
function SHSetNavBarText(hwndRequester : HWND; pszText : LPCTSTR): WINBOOL; external UserDLLAyg name 'SHSetNavBarText';
function SHLoadImageResource(hinst: HINST; uIdGif: UINT ): HBITMAP; external UserDLLAyg index 64;
function SHLoadImageFile(pszFileName: LPCTSTR ) : HBITMAP; external UserDLLAyg index 75;
procedure SHNavigateBack; external UserDLLAyg index 183;
function SHSipInfo(uiAction: UINT; uiParam: UINT; pvParam: PVOID; fWinIni: UINT  ): WINBOOL; external UserDLLAyg name 'SHSipInfo';
function SHSipPreference(hwnd: HWND ; st : SIPSTATE ) : WINBOOL; external UserDLLAyg name 'SHSipPreference';
function SHRecognizeGesture(var shrg : SHRGINFO): DWORD; external UserDLLAyg name 'SHRecognizeGesture';
function SHCameraCapture(var shcc: TSHCAMERACAPTURE): HRESULT; external UserDLLAyg name 'SHCameraCapture';

implementation

end.
