{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  aygshell.h

  Declarations for aygshell WinCE API

  Changes :

  09-28-2005 : orinaudo@gmail.com
   First release
  
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
          2021    DMProcessConfigXML
           330    DPI_ExtractIconEx
           311    DPI_LoadImageFile
           310    DPI_LoadLibraryRes
           237    DisplayNotRunnableAppDialog
           226    DoEditContextMenu
           191    DrawAlignedIcon
            44    EditSubProc
          2011    ExitWindowsEx
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
            10    SHCloseApps
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
            69    SHDoneButton
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
           188    SHFindMenuBar
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
            65    SHFullScreen
           213    SHGetActiveDialog
             2    SHGetAppKeyAssoc
            90    SHGetAutoRunPath
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
            56    SHInitDialog
             9    SHInitExtraControls
           149    SHInitPresetMessages
            39    SHInputDialog
           181    SHInsertPresetMessage
           129    SHInvalidateScreen
           101    SHInvokeContextMenuCommand
           123    SHIsLocked
           295    SHIsPreOzoneUpdate
            94    SHIsPreRapierApp
           100    SHLoadContextMenuExtensions
           230    SHLoadFileContextMenuExtensions
           313    SHLoadFontFromResource
            75    SHLoadImageFile
            64    SHLoadImageResource
            91    SHLoadMenuPopup
           216    SHLoadSKFromReg
           121    SHLock
           138    SHMakeCall
           235    SHMakeValidFilename
            32    SHMessageBox
           183    SHNavigateBack
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
            40    SHRecognizeGesture
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
             3    SHSetAppKeyWndAssoc
           169    SHSetAsWatermark
           184    SHSetBack
           161    SHSetBubbleRegion
           298    SHSetDisplayRotation
           131    SHSetForegroundLastActivePopup
           231    SHSetInputContext
           134    SHSetKOBits
            88    SHSetNavBarText
           154    SHSetPresetMessage
           170    SHSetSimToolkitMenu
          1003    SHSetSoftKey
           300    SHSetStretchMode
            59    SHSetWindowBits
            67    SHShowContextMenu
          1004    SHShowSoftKeys
            79    SHSignalDone
             4    SHSipInfo
           314    SHSipMightBlockUI
            21    SHSipPreference
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

{$ifdef read_interface}

{$PACKRECORDS C}

//*****************************************************************************
// consts
//*****************************************************************************
const
  NOMENU                  = $FFFF;
  SHA_INPUTDIALOG         = $0001;
  SHACTI_FSIPUP           = $0001;
  SHACTI_FSIPONDEACTIVATE = $0002;
  SHACTI_FSIPRESERVED     = $FFFF xor SHACTI_FSIPUP xor SHACTI_FSIPUP;

  SHCMBF_EMPTYBAR      = $0001;
  SHCMBF_HIDDEN        = $0002; // create it hidden
  SHCMBF_HIDESIPBUTTON = $0004;
  SHCMBF_COLORBK       = $0008;
  SHCMBF_HMENU         = $0010; // specify an hmenu for resource rather than toolbar info


//*****************************************************************************
// types
//*****************************************************************************

type

  SHACTIVATEINFO = record
    cbSize : DWORD;
    hwndLastFocus : HWND ;
    bits : Longint ;
  end;
  PSHACTIVATEINFO=^SHACTIVATEINFO;

  tagSHMENUBARINFO = record
    cbSize : DWORD;        // IN  - Indicates which members of struct are valid
    hwndParent : HWND ;    // IN
    dwFlags : DWORD ;      // IN  - Some features we want
    nToolBarId : UINT ;    // IN  - Which toolbar are we using
    hInstRes : HINST;      // IN  - Instance that owns the resources
    nBmpId : Integer;
    cBmpImages : Integer;  // IN  - Count of bitmap images
    hwndMB : HWND ;        // OUT
    clrBk : COLORREF ;     // IN  - background color of the menu bar (excluding sip)
  end;
  SHMENUBARINFO=tagSHMENUBARINFO;
  PSHMENUBARINFO=^tagSHMENUBARINFO;

//*****************************************************************************
// functions
//*****************************************************************************

function SHCreateMenuBar(pmbi : PSHMENUBARINFO ): WINBOOL; external UserDLLAyg name 'SHCreateMenuBar';
function SHHandleWMActivate(hwnd:HWND; wParam:WPARAM; lParam:LPARAM; psai: PSHACTIVATEINFO; dwFlags:DWORD  ): WINBOOL; external UserDLLAyg name 'SHHandleWMActivate';
function SHHandleWMSettingChange(hwnd:HWND; wParam:WPARAM; lParam:LPARAM; psai: PSHACTIVATEINFO): WINBOOL; external UserDLLAyg name 'SHHandleWMSettingChange';

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}


