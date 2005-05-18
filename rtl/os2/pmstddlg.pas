{****************************************************************************


                          PMSTDDLG interface unit
                     FPC Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Klaempfl
                    Copyright (c) 2002 by Yuri Prokushev

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

unit PMStdDlg;

  interface

    uses
       os2def,doscalls,pmwin,pmgpi;

    const
       FDS_CENTER = $00000001;
       FDS_CUSTOM = $00000002;
       FDS_FILTERUNION = $00000004;
       FDS_HELPBUTTON = $00000008;
       FDS_APPLYBUTTON = $00000010;
       FDS_PRELOAD_VOLINFO = $00000020;
       FDS_MODELESS = $00000040;
       FDS_INCLUDE_EAS = $00000080;
       FDS_OPEN_DIALOG = $00000100;
       FDS_SAVEAS_DIALOG = $00000200;
       FDS_MULTIPLESEL = $00000400;
       FDS_ENABLEFILELB = $00000800;
       FDS_EFSELECTION = 0;
       FDS_LBSELECTION = 1;
       FDS_SUCCESSFUL = 0;
       FDS_ERR_DEALLOCATE_MEMORY = 1;
       FDS_ERR_FILTER_TRUNC = 2;
       FDS_ERR_INVALID_DIALOG = 3;
       FDS_ERR_INVALID_DRIVE = 4;
       FDS_ERR_INVALID_FILTER = 5;
       FDS_ERR_INVALID_PATHFILE = 6;
       FDS_ERR_OUT_OF_MEMORY = 7;
       FDS_ERR_PATH_TOO_LONG = 8;
       FDS_ERR_TOO_MANY_FILE_TYPES = 9;
       FDS_ERR_INVALID_VERSION = 10;
       FDS_ERR_INVALID_CUSTOM_HANDLE = 11;
       FDS_ERR_DIALOG_LOAD_ERROR = 12;
       FDS_ERR_DRIVE_ERROR = 13;
       FDM_FILTER = WM_USER+40;
       FDM_VALIDATE = WM_USER+41;
       FDM_ERROR = WM_USER+42;

    type
       APSZ = array [0..0] of PChar;

       PAPSZ = ^APSZ;

       FileDlg = record
          cbSize : cardinal;
          fl : cardinal;
          ulUser : cardinal;
          lReturn : longint;
          lSRC : longint;
          pszTitle : PChar;
          pszOKButton : PChar;
          pfnDlgProc : Pointer;
          pszIType : PChar;
          papszITypeList : PAPSZ;
          pszIDrive : PChar;
          papszIDriveList : PAPSZ;
          hMod : cardinal;
          szFullFile : array [0..MaxPathLength-1] of char;
          papszFQFilename : PAPSZ;
          ulFQFCount : cardinal;
          usDlgId : word;
          x : integer;
          y : integer;
          sEAType : integer;
       end;

       PFileDlg = ^FileDlg;


    function WinFileDlg (hwndP: HWnd; hwndO: HWnd; pfild: PFileDlg) : HWnd; cdecl;

    function WinDefFileDlgProc (hwnd : HWnd;msg : cardinal;mp1 : MPARAM;mp2 : MPARAM) : MRESULT; cdecl;

    function WinFreeFileDlgList(papszFQFilename : PAPSZ) : Longbool; cdecl;

    const
       DID_FILE_DIALOG = 256;
       DID_FILENAME_TXT = 257;
       DID_FILENAME_ED = 258;
       DID_DRIVE_TXT = 259;
       DID_DRIVE_CB = 260;
       DID_FILTER_TXT = 261;
       DID_FILTER_CB = 262;
       DID_DIRECTORY_TXT = 263;
       DID_DIRECTORY_LB = 264;
       DID_FILES_TXT = 265;
       DID_FILES_LB = 266;
       DID_HELP_PB = 267;
       DID_APPLY_PB = 268;
       DID_OK_PB = DID_OK;
       DID_CANCEL_PB = DID_CANCEL;
       IDS_FILE_ALL_FILES_SELECTOR = 1000;
       IDS_FILE_BACK_CUR_PATH = 1001;
       IDS_FILE_BACK_PREV_PATH = 1002;
       IDS_FILE_BACK_SLASH = 1003;
       IDS_FILE_BASE_FILTER = 1004;
       IDS_FILE_BLANK = 1005;
       IDS_FILE_COLON = 1006;
       IDS_FILE_DOT = 1007;
       IDS_FILE_DRIVE_LETTERS = 1008;
       IDS_FILE_FWD_CUR_PATH = 1009;
       IDS_FILE_FWD_PREV_PATH = 1010;
       IDS_FILE_FORWARD_SLASH = 1011;
       IDS_FILE_PARENT_DIR = 1012;
       IDS_FILE_Q_MARK = 1013;
       IDS_FILE_SPLAT = 1014;
       IDS_FILE_SPLAT_DOT = 1015;
       IDS_FILE_SAVEAS_TITLE = 1016;
       IDS_FILE_SAVEAS_FILTER_TXT = 1017;
       IDS_FILE_SAVEAS_FILENM_TXT = 1018;
       IDS_FILE_DUMMY_FILE_NAME = 1019;
       IDS_FILE_DUMMY_FILE_EXT = 1020;
       IDS_FILE_DUMMY_DRIVE = 1021;
       IDS_FILE_DUMMY_ROOT_DIR = 1022;
       IDS_FILE_PATH_PTR = 1023;
       IDS_FILE_VOLUME_PREFIX = 1024;
       IDS_FILE_VOLUME_SUFFIX = 1025;
       IDS_FILE_PATH_PTR2 = 1026;
       IDS_FILE_INVALID_CHARS = 1027;
       IDS_FILE_BAD_DRIVE_NAME = 1100;
       IDS_FILE_BAD_DRIVE_OR_PATH_NAME = 1101;
       IDS_FILE_BAD_FILE_NAME = 1102;
       IDS_FILE_BAD_FQF = 1103;
       IDS_FILE_BAD_NETWORK_NAME = 1104;
       IDS_FILE_BAD_SUB_DIR_NAME = 1105;
       IDS_FILE_DRIVE_NOT_AVAILABLE = 1106;
       IDS_FILE_FQFNAME_TOO_LONG = 1107;
       IDS_FILE_OPEN_DIALOG_NOTE = 1108;
       IDS_FILE_PATH_TOO_LONG = 1109;
       IDS_FILE_SAVEAS_DIALOG_NOTE = 1110;
       IDS_FILE_DRIVE_DISK_CHANGE = 1120;
       IDS_FILE_DRIVE_NOT_READY = 1122;
       IDS_FILE_DRIVE_LOCKED = 1123;
       IDS_FILE_DRIVE_NO_SECTOR = 1124;
       IDS_FILE_DRIVE_SOME_ERROR = 1125;
       IDS_FILE_DRIVE_INVALID = 1126;
       IDS_FILE_INSERT_DISK_NOTE = 1127;
       IDS_FILE_OK_WHEN_READY = 1128;

    type
       FontDlg = record
          cbSize : cardinal;
          hpsScreen : HPS;
          hpsPrinter : HPS;
          pszTitle : PChar;
          pszPreview : PChar;
          pszPtSizeList : PChar;
          pfnDlgProc : Pointer;
          pszFamilyname : PChar;
          fxPointSize : longint;
          fl : cardinal;
          flFlags : cardinal;
          flType : cardinal;
          flTypeMask : cardinal;
          flStyle : cardinal;
          flStyleMask : cardinal;
          clrFore : longint;
          clrBack : longint;
          ulUser : cardinal;
          lReturn : longint;
          lSRC : longint;
          lEmHeight : longint;
          lXHeight : longint;
          lExternalLeading : longint;
          hMod : cardinal;
          _fAttrs : FATTRS;
          sNominalPointSize : integer;
          usWeight : word;
          usWidth : word;
          x : integer;
          y : integer;
          usDlgId : word;
          usFamilyBufLen : word;
          usReserved : word;
       end;

       PFontDlg = ^FontDlg;

    const
       FNTS_CENTER = $00000001;
       FNTS_CUSTOM = $00000002;
       FNTS_OWNERDRAWPREVIEW = $00000004;
       FNTS_HELPBUTTON = $00000008;
       FNTS_APPLYBUTTON = $00000010;
       FNTS_RESETBUTTON = $00000020;
       FNTS_MODELESS = $00000040;
       FNTS_INITFROMFATTRS = $00000080;
       FNTS_BITMAPONLY = $00000100;
       FNTS_VECTORONLY = $00000200;
       FNTS_FIXEDWIDTHONLY = $00000400;
       FNTS_PROPORTIONALONLY = $00000800;
       FNTS_NOSYNTHESIZEDFONTS = $00001000;
       FNTF_NOVIEWSCREENFONTS = 1;
       FNTF_NOVIEWPRINTERFONTS = 2;
       FNTF_SCREENFONTSELECTED = 4;
       FNTF_PRINTERFONTSELECTED = 8;
       CLRC_FOREGROUND = 1;
       CLRC_BACKGROUND = 2;
       FNTI_BITMAPFONT = $0001;
       FNTI_VECTORFONT = $0002;
       FNTI_FIXEDWIDTHFONT = $0004;
       FNTI_PROPORTIONALFONT = $0008;
       FNTI_SYNTHESIZED = $0010;
       FNTI_DEFAULTLIST = $0020;
       FNTI_FAMILYNAME = $0100;
       FNTI_STYLENAME = $0200;
       FNTI_POINTSIZE = $0400;
       FNTS_SUCCESSFUL = 0;
       FNTS_ERR_INVALID_DIALOG = 3;
       FNTS_ERR_ALLOC_SHARED_MEM = 4;
       FNTS_ERR_INVALID_PARM = 5;
       FNTS_ERR_OUT_OF_MEMORY = 7;
       FNTS_ERR_INVALID_VERSION = 10;
       FNTS_ERR_DIALOG_LOAD_ERROR = 12;
       FNTM_FACENAMECHANGED = WM_USER+50;
       FNTM_POINTSIZECHANGED = WM_USER+51;
       FNTM_STYLECHANGED = WM_USER+52;
       FNTM_COLORCHANGED = WM_USER+53;
       FNTM_UPDATEPREVIEW = WM_USER+54;
       FNTM_FILTERLIST = WM_USER+55;

    type
       StyleChange = record
          usWeight : word;
          usWeightOld : word;
          usWidth : word;
          usWidthOld : word;
          flType : cardinal;
          flTypeOld : cardinal;
          flTypeMask : cardinal;
          flTypeMaskOld : cardinal;
          flStyle : cardinal;
          flStyleOld : cardinal;
          flStyleMask : cardinal;
          flStyleMaskOld : cardinal;
       end;

       PStyleChange = ^StyleChange;


    function WinFontDlg(hwndP : HWnd;hwndO : HWnd;pfntd : PFontDlg) : HWnd; cdecl;

    function WinDefFontDlgProc(_hwnd : HWnd;msg : cardinal;mp1 : MParam;mp2 : MParam) : MResult; cdecl;

    const
       DID_FONT_DIALOG = 300;
       DID_NAME = 301;
       DID_STYLE = 302;
       DID_DISPLAY_FILTER = 303;
       DID_PRINTER_FILTER = 304;
       DID_SIZE = 305;
       DID_SAMPLE = 306;
       DID_OUTLINE = 307;
       DID_UNDERSCORE = 308;
       DID_STRIKEOUT = 309;
       DID_HELP_BUTTON = 310;
       DID_APPLY_BUTTON = 311;
       DID_RESET_BUTTON = 312;
       DID_OK_BUTTON = DID_OK;
       DID_CANCEL_BUTTON = DID_CANCEL;
       DID_NAME_PREFIX = 313;
       DID_STYLE_PREFIX = 314;
       DID_SIZE_PREFIX = 315;
       DID_SAMPLE_GROUPBOX = 316;
       DID_EMPHASIS_GROUPBOX = 317;
       IDS_FONT_SAMPLE = 350;
       IDS_FONT_BLANK = 351;
       IDS_FONT_KEY_0 = 352;
       IDS_FONT_KEY_9 = 353;
       IDS_FONT_KEY_SEP = 354;
       IDS_FONT_DISP_ONLY = 355;
       IDS_FONT_PRINTER_ONLY = 356;
       IDS_FONT_COMBINED = 357;
       IDS_FONT_WEIGHT1 = 358;
       IDS_FONT_WEIGHT2 = 359;
       IDS_FONT_WEIGHT3 = 360;
       IDS_FONT_WEIGHT4 = 361;
       IDS_FONT_WEIGHT5 = 362;
       IDS_FONT_WEIGHT6 = 363;
       IDS_FONT_WEIGHT7 = 364;
       IDS_FONT_WEIGHT8 = 365;
       IDS_FONT_WEIGHT9 = 366;
       IDS_FONT_WIDTH1 = 367;
       IDS_FONT_WIDTH2 = 368;
       IDS_FONT_WIDTH3 = 369;
       IDS_FONT_WIDTH4 = 370;
       IDS_FONT_WIDTH5 = 371;
       IDS_FONT_WIDTH6 = 372;
       IDS_FONT_WIDTH7 = 373;
       IDS_FONT_WIDTH8 = 374;
       IDS_FONT_WIDTH9 = 375;
       IDS_FONT_OPTION0 = 376;
       IDS_FONT_OPTION1 = 377;
       IDS_FONT_OPTION2 = 378;
       IDS_FONT_OPTION3 = 379;
       IDS_FONT_POINT_SIZE_LIST = 380;
       SPBS_ALLCHARACTERS = $00000000;
       SPBS_NUMERICONLY = $00000001;
       SPBS_READONLY = $00000002;
       SPBS_MASTER = $00000010;
       SPBS_SERVANT = $00000000;
       SPBS_JUSTDEFAULT = $00000000;
       SPBS_JUSTLEFT = $00000008;
       SPBS_JUSTRIGHT = $00000004;
       SPBS_JUSTCENTER = $0000000C;
       SPBS_NOBORDER = $00000020;
       SPBS_FASTSPIN = $00000100;
       SPBS_PADWITHZEROS = $00000080;
       SPBN_UPARROW = $20A;
       SPBN_DOWNARROW = $20B;
       SPBN_ENDSPIN = $20C;
       SPBN_CHANGE = $20D;
       SPBN_SETFOCUS = $20E;
       SPBN_KILLFOCUS = $20F;
       SPBM_OVERRIDESETLIMITS = $200;
       SPBM_QUERYLIMITS = $201;
       SPBM_SETTEXTLIMIT = $202;
       SPBM_SPINUP = $203;
       SPBM_SPINDOWN = $204;
       SPBM_QUERYVALUE = $205;
       SPBQ_UPDATEIFVALID = 0;
       SPBQ_ALWAYSUPDATE = 1;
       SPBQ_DONOTUPDATE = 3;
       SPBM_SETARRAY = $206;
       SPBM_SETLIMITS = $207;
       SPBM_SETCURRENTVALUE = $208;
       SPBM_SETMASTER = $209;
       PMERR_NOT_DRAGGING = $1f00;
       PMERR_ALREADY_DRAGGING = $1f01;
       MSGF_DRAG = $0010;
       WM_DRAGFIRST = $0310;
       WM_DRAGLAST = $032f;
       DM_DROP = $032f;
       DM_DRAGOVER = $032e;
       DM_DRAGLEAVE = $032d;
       DM_DROPHELP = $032c;
       DM_ENDCONVERSATION = $032b;
       DM_PRINT = $032a;
       DM_RENDER = $0329;
       DM_RENDERCOMPLETE = $0328;
       DM_RENDERPREPARE = $0327;
       DM_DRAGFILECOMPLETE = $0326;
       DM_EMPHASIZETARGET = $0325;
       DM_DRAGERROR = $0324;
       DM_FILERENDERED = $0323;
       DM_RENDERFILE = $0322;
       DM_DRAGOVERNOTIFY = $0321;
       DM_PRINTOBJECT = $0320;
       DM_DISCARDOBJECT = $031f;
       DRT_ASM = 'Assembler Code';
       DRT_BASIC = 'BASIC Code';
       DRT_BINDATA = 'Binary Data';
       DRT_BITMAP = 'Bitmap';
       DRT_C = 'C Code';
       DRT_COBOL = 'COBOL Code';
       DRT_DLL = 'Dynamic Link Library';
       DRT_DOSCMD = 'DOS Command File';
       DRT_EXE = 'Executable';
       DRT_FORTRAN = 'FORTRAN Code';
       DRT_ICON = 'Icon';
       DRT_LIB = 'Library';
       DRT_METAFILE = 'Metafile';
       DRT_OS2CMD = 'OS/2 Command File';
       DRT_PASCAL = 'Pascal Code';
       DRT_RESOURCE = 'Resource File';
       DRT_TEXT = 'Plain Text';
       DRT_UNKNOWN = 'Unknown';
       DOR_NODROP = $0000;
       DOR_DROP = $0001;
       DOR_NODROPOP = $0002;
       DOR_NEVERDROP = $0003;
       DO_COPYABLE = $0001;
       DO_MOVEABLE = $0002;
       DO_LINKABLE = $0004;
       DC_OPEN = $0001;
       DC_REF = $0002;
       DC_GROUP = $0004;
       DC_CONTAINER = $0008;
       DC_PREPARE = $0010;
       DC_REMOVEABLEMEDIA = $0020;
       DO_DEFAULT = $BFFE;
       DO_UNKNOWN = $BFFF;
       DO_COPY = $0010;
       DO_MOVE = $0020;
       DO_LINK = $0018;
       DO_CREATE = $0040;
       DMFL_TARGETSUCCESSFUL = $0001;
       DMFL_TARGETFAIL = $0002;
       DMFL_NATIVERENDER = $0004;
       DMFL_RENDERRETRY = $0008;
       DMFL_RENDEROK = $0010;
       DMFL_RENDERFAIL = $0020;
       DRG_ICON = $00000001;
       DRG_BITMAP = $00000002;
       DRG_POLYGON = $00000004;
       DRG_STRETCH = $00000008;
       DRG_TRANSPARENT = $00000010;
       DRG_CLOSED = $00000020;
       DME_IGNOREABORT = 1;
       DME_IGNORECONTINUE = 2;
       DME_REPLACE = 3;
       DME_RETRY = 4;
       DF_MOVE = $0001;
       DF_SOURCE = $0002;
       DF_SUCCESSFUL = $0004;
       DRR_SOURCE = 1;
       DRR_TARGET = 2;
       DRR_ABORT = 3;
       DFF_MOVE = 1;
       DFF_COPY = 2;
       DFF_DELETE = 3;

    type
       HStr = cardinal;

       DragItem = record
          hwndItem : HWnd;
          ulItemID : cardinal;
          hstrType : HStr;
          hstrRMF : HStr;
          hstrContainerName : HStr;
          hstrSourceName : HStr;
          hstrTargetName : HStr;
          cxOffset : integer;
          cyOffset : integer;
          fsControl : word;
          fsSupportedOps : word;
       end;

       PDragItem = ^DragItem;

       DragInfo = record
          cbDraginfo : cardinal;
          cbDragitem : word;
          usOperation : word;
          hwndSource : HWnd;
          xDrop : integer;
          yDrop : integer;
          cditem : word;
          usReserved : word;
       end;

       PDragInfo = ^DragInfo;

       DragImage = record
          cb : word;
          cptl : word;
          hImage : cardinal;
          sizlStretch : SizeL;
          fl : cardinal;
          cxOffset : integer;
          cyOffset : integer;
       end;

       PDragImage = ^DragImage;

       DragTransfer = record
          cb : cardinal;
          hwndClient : HWnd;
          pditem : PDragItem;
          hstrSelectedRMF : HStr;
          hstrRenderToName : HStr;
          ulTargetInfo : cardinal;
          usOperation : word;
          fsReply : word;
       end;

       PDragTransfer = ^DragTransfer;

       RenderFile = record
          hwndDragFiles : HWnd;
          hstrSource : HStr;
          hstrTarget : HStr;
          fMove : word;
          usRsvd : word;
       end;

       PRenderFile = ^RenderFile;


    function DrgAcceptDroppedFiles(hwnd : HWnd;pszPath : PChar;pszTypes : PChar;ulDefaultOp : cardinal;ulRsvd : cardinal) : Longbool; cdecl;

    function DrgAllocDraginfo(cditem : cardinal) : PDragInfo; cdecl;

    function DrgAllocDragtransfer(cdxfer : cardinal) : PDragTransfer; cdecl;

    function DrgDrag(hwndSource : HWnd;pdinfo : PDragInfo;pdimg : PDragImage;cdimg : cardinal;vkTerminate : longint; var pRsvd) : HWnd; cdecl;

    type
       PPSZ = ^PChar;


    function DrgDragFiles(hwnd : HWnd;apszFiles : PPSZ;apszTypes : PPSZ;apszTargets : PPSZ;cFiles : cardinal;hptrDrag : cardinal;vkTerm : cardinal;fSourceRender : Longbool;ulRsvd : cardinal) : Longbool; cdecl;

    function DrgPostTransferMsg(hwnd : HWnd;msg : cardinal;pdxfer : PDragTransfer;fl : cardinal;ulRsvd : cardinal;fRetry : Longbool) : Longbool; cdecl;

    function DrgQueryDragitem(pdinfo : PDragInfo;cbBuffer : cardinal;pditem : PDragItem;iItem : cardinal) : Longbool; cdecl;

    function DrgQueryDragitemCount(pdinfo : PDragInfo) : cardinal; cdecl;

    function DrgQueryDragitemPtr(pdinfo : PDragInfo;i : cardinal) : PDragItem; cdecl;

    function DrgQueryNativeRMF(pditem : PDragItem;cbBuffer : cardinal;pBuffer : PChar) : Longbool; cdecl;

    function DrgQueryNativeRMFLen(pditem : PDragItem) : cardinal; cdecl;

    function DrgQueryStrName(hstr : HStr;cbBuffer : cardinal;pBuffer : PChar) : cardinal; cdecl;

    function DrgQueryStrNameLen(hstr : HStr) : cardinal; cdecl;

    function DrgQueryTrueType(pditem : PDragItem;cbBuffer : cardinal;pBuffer : PChar) : Longbool; cdecl;

    function DrgQueryTrueTypeLen(pditem : PDragItem) : cardinal; cdecl;

    function DrgSendTransferMsg(hwnd : HWnd;msg : cardinal;mp1 : MParam;mp2 : MParam) : MResult; cdecl;

    function DrgSetDragitem(pdinfo : PDragInfo;pditem : PDragItem;cbBuffer : cardinal;iItem : cardinal) : Longbool; cdecl;

    function DrgSetDragImage(pdinfo : PDragInfo;pdimg : PDragImage;cdimg : cardinal; var pRsvd) : Longbool; cdecl;

    function DrgVerifyTypeSet(pditem : PDragItem;pszType : PChar;cbMatch : cardinal;pszMatch : PChar) : Longbool; cdecl;

    function DrgAccessDraginfo(pdinfo : PDragInfo) : Longbool; cdecl;

    function DrgAddStrHandle(psz : PChar) : HStr; cdecl;

    function DrgDeleteDraginfoStrHandles(pdinfo : PDragInfo) : Longbool; cdecl;

    function DrgDeleteStrHandle(hstr : HStr) : Longbool; cdecl;

    function DrgFreeDraginfo(pdinfo : PDragInfo) : Longbool; cdecl;

    function DrgFreeDragtransfer(pdxfer : PDragTransfer) : Longbool; cdecl;

    function DrgGetPS(hwnd : HWnd) : HPS; cdecl;

    function DrgPushDraginfo(pdinfo : PDragInfo;hwndDest : HWnd) : Longbool; cdecl;

    function DrgReleasePS(hps : HPS) : Longbool; cdecl;

    function DrgSetDragPointer(pdinfo : PDragInfo;hptr : cardinal) : Longbool; cdecl;

    function DrgVerifyNativeRMF(pditem : PDragItem;pszRMF : PChar) : Longbool; cdecl;

    function DrgVerifyRMF(pditem : PDragItem;pszMech : PChar;pszFmt : PChar) : Longbool; cdecl;

    function DrgVerifyTrueType(pditem : PDragItem;pszType : PChar) : Longbool; cdecl;

    function DrgVerifyType(pditem : PDragItem;pszType : PChar) : Longbool; cdecl;

    const
       PMERR_NOFILTERED_ITEMS = $1f02;
       PMERR_COMPARISON_FAILED = $1f03;
       PMERR_RECORD_CURRENTLY_INSERTED = $1f04;
       PMERR_FI_CURRENTLY_INSERTED = $1f05;
       CCS_EXTENDSEL = $00000001;
       CCS_MULTIPLESEL = $00000002;
       CCS_SINGLESEL = $00000004;
       CCS_AUTOPOSITION = $00000008;
       CCS_VERIFYPOINTERS = $00000010;
       CCS_READONLY = $00000020;
       CCS_MINIRECORDCORE = $00000040;
       CV_TEXT = $00000001;
       CV_NAME = $00000002;
       CV_ICON = $00000004;
       CV_DETAIL = $00000008;
       CV_FLOW = $00000010;
       CV_MINI = $00000020;
       CV_TREE = $00000040;
       CA_CONTAINERTITLE = $00000200;
       CA_TITLESEPARATOR = $00000400;
       CA_TITLELEFT = $00000800;
       CA_TITLERIGHT = $00001000;
       CA_TITLECENTER = $00002000;
       CA_OWNERDRAW = $00004000;
       CA_DETAILSVIEWTITLES = $00008000;
       CA_ORDEREDTARGETEMPH = $00010000;
       CA_DRAWBITMAP = $00020000;
       CA_DRAWICON = $00040000;
       CA_TITLEREADONLY = $00080000;
       CA_OWNERPAINTBACKGROUND = $00100000;
       CA_MIXEDTARGETEMPH = $00200000;
       CA_TREELINE = $00400000;
       CID_LEFTCOLTITLEWND = $7FF0;
       CID_RIGHTCOLTITLEWND = $7FF1;
       CID_BLANKBOX = $7FF2;
       CID_HSCROLL = $7FF3;
       CID_RIGHTHSCROLL = $7FF4;
       CID_CNRTITLEWND = $7FF5;
       CID_LEFTDVWND = $7FF7;
       CID_RIGHTDVWND = $7FF8;
       CID_VSCROLL = $7FF9;
       CID_MLE = $7FFA;

    type
       TreeItemDesc = record
          hbmExpanded : HBitmap;
          hbmCollapsed : HBitmap;
          hptrExpanded : cardinal;
          hptrCollapsed : cardinal;
       end;

       PTreeItemDesc = ^TreeItemDesc;

       PFieldInfo = ^FieldInfo;

       FieldInfo = record
          cb : cardinal;
          flData : cardinal;
          flTitle : cardinal;
          pTitleData : Pointer;
          offStruct : cardinal;
          pUserData : Pointer;
          pNextFieldInfo : PFieldInfo;
          cxWidth : cardinal;
       end;

       PRecordCore = ^RecordCore;

       RecordCore = record
          cb : cardinal;
          flRecordAttr : cardinal;
          ptlIcon : PointL;
          preccNextRecord : PRecordCore;
          pszIcon : PChar;
          hptrIcon : cardinal;
          hptrMiniIcon : cardinal;
          hbmBitmap : HBitmap;
          hbmMiniBitmap : HBitmap;
          pTreeItemDesc : PTreeItemDesc;
          pszText : PChar;
          pszName : PChar;
          pszTree : PChar;
       end;

       PMiniRecordCore = ^MiniRecordCore;

       MINIRECORDCORE = record
          cb : cardinal;
          flRecordAttr : cardinal;
          ptlIcon : PointL;
          preccNextRecord : PMiniRecordCore;
          pszIcon : PChar;
          hptrIcon : cardinal;
       end;

       CNRInfo = record
          cb : cardinal;
          pSortRecord : Pointer;
          pFieldInfoLast : PFieldInfo;
          pFieldInfoObject : PFieldInfo;
          pszCnrTitle : PChar;
          flWindowAttr : cardinal;
          ptlOrigin : PointL;
          cDelta : cardinal;
          cRecords : cardinal;
          slBitmapOrIcon : SizeL;
          slTreeBitmapOrIcon : SizeL;
          hbmExpanded : HBitmap;
          hbmCollapsed : HBitmap;
          hptrExpanded : cardinal;
          hptrCollapsed : cardinal;
          cyLineSpacing : longint;
          cxTreeIndent : longint;
          cxTreeLine : longint;
          cFields : cardinal;
          xVertSplitbar : longint;
       end;

       PCNRInfo = ^CNRInfo;

       CDate = record
          day : Byte;
          month : Byte;
          year : word;
       end;

       PCDate = ^CDate;

       CTime = record
          hours : Byte;
          minutes : Byte;
          seconds : Byte;
          ucReserved : Byte;
       end;

       PCTime = ^CTime;

    const
       CFA_LEFT = $00000001;
       CFA_RIGHT = $00000002;
       CFA_CENTER = $00000004;
       CFA_TOP = $00000008;
       CFA_VCENTER = $00000010;
       CFA_BOTTOM = $00000020;
       CFA_INVISIBLE = $00000040;
       CFA_BITMAPORICON = $00000100;
       CFA_SEPARATOR = $00000200;
       CFA_HORZSEPARATOR = $00000400;
       CFA_STRING = $00000800;
       CFA_OWNER = $00001000;
       CFA_DATE = $00002000;
       CFA_TIME = $00004000;
       CFA_FIREADONLY = $00008000;
       CFA_FITITLEREADONLY = $00010000;
       CFA_ULONG = $00020000;
       CRA_SELECTED = $00000001;
       CRA_TARGET = $00000002;
       CRA_CURSORED = $00000004;
       CRA_INUSE = $00000008;
       CRA_FILTERED = $00000010;
       CRA_DROPONABLE = $00000020;
       CRA_RECORDREADONLY = $00000040;
       CRA_EXPANDED = $00000080;
       CRA_COLLAPSED = $00000100;
       CM_ALLOCDETAILFIELDINFO = $0330;
       CM_ALLOCRECORD = $0331;
       CM_ARRANGE = $0332;
       CM_ERASERECORD = $0333;
       CM_FILTER = $0334;
       CM_FREEDETAILFIELDINFO = $0335;
       CM_FREERECORD = $0336;
       CM_HORZSCROLLSPLITWINDOW = $0337;
       CM_INSERTDETAILFIELDINFO = $0338;
       CM_INSERTRECORD = $0339;
       CM_INVALIDATEDETAILFIELDINFO = $033a;
       CM_INVALIDATERECORD = $033b;
       CM_PAINTBACKGROUND = $033c;
       CM_QUERYCNRINFO = $033d;
       CM_QUERYDETAILFIELDINFO = $033e;
       CM_QUERYDRAGIMAGE = $033f;
       CM_QUERYRECORD = $0340;
       CM_QUERYRECORDEMPHASIS = $0341;
       CM_QUERYRECORDFROMRECT = $0342;
       CM_QUERYRECORDRECT = $0343;
       CM_QUERYVIEWPORTRECT = $0344;
       CM_REMOVEDETAILFIELDINFO = $0345;
       CM_REMOVERECORD = $0346;
       CM_SCROLLWINDOW = $0347;
       CM_SEARCHSTRING = $0348;
       CM_SETCNRINFO = $0349;
       CM_SETRECORDEMPHASIS = $034a;
       CM_SORTRECORD = $034b;
       CM_OPENEDIT = $034c;
       CM_CLOSEEDIT = $034d;
       CM_COLLAPSETREE = $034e;
       CM_EXPANDTREE = $034f;
       CM_QUERYRECORDINFO = $0350;
       CN_DRAGAFTER = 101;
       CN_DRAGLEAVE = 102;
       CN_DRAGOVER = 103;
       CN_DROP = 104;
       CN_DROPHELP = 105;
       CN_ENTER = 106;
       CN_INITDRAG = 107;
       CN_EMPHASIS = 108;
       CN_KILLFOCUS = 109;
       CN_SCROLL = 110;
       CN_QUERYDELTA = 111;
       CN_SETFOCUS = 112;
       CN_REALLOCPSZ = 113;
       CN_BEGINEDIT = 114;
       CN_ENDEDIT = 115;
       CN_COLLAPSETREE = 116;
       CN_EXPANDTREE = 117;
       CN_HELP = 118;
       CN_CONTEXTMENU = 119;

    type
       CNRDragInit = record
          hwndCnr : HWnd;
          pRecord : PRecordCore;
          x : longint;
          y : longint;
          cx : longint;
          cy : longint;
       end;

       PCNRDragInit = ^CNRDragInit;

       FieldInfoInsert = record
          cb : cardinal;
          pFieldInfoOrder : PFieldInfo;
          fInvalidateFieldInfo : cardinal;
          cFieldInfoInsert : cardinal;
       end;

       PFieldInfoInsert = ^FieldInfoInsert;

       RecordInsert = record
          cb : cardinal;
          pRecordOrder : PRecordCore;
          pRecordParent : PRecordCore;
          fInvalidateRecord : cardinal;
          zOrder : cardinal;
          cRecordsInsert : cardinal;
       end;

       PRecordInsert = ^RecordInsert;

       QueryRecFromRect = record
          cb : cardinal;
          rect : RectL;
          fsSearch : cardinal;
       end;

       PQueryRecFromRect = ^QueryRecFromRect;

       QueryRecordRect = record
          cb : cardinal;
          pRecord : PRecordCore;
          fRightSplitWindow : cardinal;
          fsExtent : cardinal;
       end;

       PQueryRecordRect = ^QueryRecordRect;

       SearchString = record
          cb : cardinal;
          pszSearch : PChar;
          fsPrefix : cardinal;
          fsCaseSensitive : cardinal;
          usView : cardinal;
       end;

       PSearchString = ^SearchString;

       CNRDragInfo = record
          pDragInfo : PDragInfo;
          pRecord : PRecordCore;
       end;

       PCNRDragInfo = ^CNRDragInfo;

       NotifyRecordEmphasis = record
          hwndCnr : HWnd;
          pRecord : PRecordCore;
          fEmphasisMask : cardinal;
       end;

       PNotifyRecordEmphasis = ^NotifyRecordEmphasis;

       NotifyRecordEnter = record
          hwndCnr : HWnd;
          fKey : cardinal;
          pRecord : PRecordCore;
       end;

       PNotifyRecordEnter = ^NotifyRecordEnter;

       NotifyDelta = record
          hwndCnr : HWnd;
          fDelta : cardinal;
       end;

       PNotifyDelta = ^NotifyDelta;

       NotifyScroll = record
          hwndCnr : HWnd;
          lScrollInc : longint;
          fScroll : cardinal;
       end;

       PNotifyScroll = ^NotifyScroll;

       CNREditData = record
          cb : cardinal;
          hwndCnr : HWnd;
          pRecord : PRecordCore;
          pFieldInfo : PFieldInfo;
          ppszText : PPSZ;
          cbText : cardinal;
          id : cardinal;
       end;

       PCNREditData = ^CNREditData;

       OwnerBackground = record
          hwnd : HWnd;
          hps : HPS;
          rclBackground : RectL;
          idWindow : longint;
       end;

       POwnerBackground = ^OwnerBackground;

       CNRDrawItemInfo = record
          pRecord : PRecordCore;
          pFieldInfo : PFieldInfo;
       end;

       PCNRDrawItemInfo = ^CNRDrawItemInfo;

    const
       CMA_TOP = $0001;
       CMA_BOTTOM = $0002;
       CMA_LEFT = $0004;
       CMA_RIGHT = $0008;
       CMA_FIRST = $0010;
       CMA_LAST = $0020;
       CMA_END = $0040;
       CMA_PREV = $0080;
       CMA_NEXT = $0100;
       CMA_HORIZONTAL = $0200;
       CMA_VERTICAL = $0400;
       CMA_ICON = $0800;
       CMA_TEXT = $1000;
       CMA_PARTIAL = $2000;
       CMA_COMPLETE = $4000;
       CMA_PARENT = $0001;
       CMA_FIRSTCHILD = $0002;
       CMA_LASTCHILD = $0004;
       CMA_CNRTITLE = $0001;
       CMA_DELTA = $0002;
       CMA_FLWINDOWATTR = $0004;
       CMA_LINESPACING = $0008;
       CMA_PFIELDINFOLAST = $0010;
       CMA_PSORTRECORD = $0020;
       CMA_PTLORIGIN = $0040;
       CMA_SLBITMAPORICON = $0080;
       CMA_XVERTSPLITBAR = $0100;
       CMA_PFIELDINFOOBJECT = $0200;
       CMA_TREEICON = $0400;
       CMA_TREEBITMAP = $0800;
       CMA_CXTREEINDENT = $1000;
       CMA_CXTREELINE = $2000;
       CMA_SLTREEBITMAPORICON = $4000;
       CMA_ITEMORDER = $0001;
       CMA_WINDOW = $0002;
       CMA_WORKSPACE = $0004;
       CMA_ZORDER = $0008;
       CMA_DELTATOP = $0001;
       CMA_DELTABOT = $0002;
       CMA_DELTAHOME = $0004;
       CMA_DELTAEND = $0008;
       CMA_NOREPOSITION = $0001;
       CMA_REPOSITION = $0002;
       CMA_TEXTCHANGED = $0004;
       CMA_ERASE = $0008;
       CMA_FREE = $0001;
       CMA_INVALIDATE = $0002;
       SLM_ADDDETENT = $0369;
       SLM_QUERYDETENTPOS = $036a;
       SLM_QUERYSCALETEXT = $036b;
       SLM_QUERYSLIDERINFO = $036c;
       SLM_QUERYTICKPOS = $036d;
       SLM_QUERYTICKSIZE = $036e;
       SLM_REMOVEDETENT = $036f;
       SLM_SETSCALETEXT = $0370;
       SLM_SETSLIDERINFO = $0371;
       SLM_SETTICKSIZE = $0372;
       SLN_CHANGE = 1;
       SLN_SLIDERTRACK = 2;
       SLN_SETFOCUS = 3;
       SLN_KILLFOCUS = 4;

    type
       SLDCData = record
          cbSize : cardinal;
          usScale1Increments : word;
          usScale1Spacing : word;
          usScale2Increments : word;
          usScale2Spacing : word;
       end;

       PSLDCData = ^SLDCData;

    const
       SLS_HORIZONTAL = $00000000;
       SLS_VERTICAL = $00000001;
       SLS_CENTER = $00000000;
       SLS_BOTTOM = $00000002;
       SLS_TOP = $00000004;
       SLS_LEFT = $00000002;
       SLS_RIGHT = $00000004;
       SLS_SNAPTOINCREMENT = $00000008;
       SLS_BUTTONSBOTTOM = $00000010;
       SLS_BUTTONSTOP = $00000020;
       SLS_BUTTONSLEFT = $00000010;
       SLS_BUTTONSRIGHT = $00000020;
       SLS_OWNERDRAW = $00000040;
       SLS_READONLY = $00000080;
       SLS_RIBBONSTRIP = $00000100;
       SLS_HOMEBOTTOM = $00000000;
       SLS_HOMETOP = $00000200;
       SLS_HOMELEFT = $00000000;
       SLS_HOMERIGHT = $00000200;
       SLS_PRIMARYSCALE1 = $00000000;
       SLS_PRIMARYSCALE2 = $00000400;
       SMA_SCALE1 = $0001;
       SMA_SCALE2 = $0002;
       SMA_SHAFTDIMENSIONS = $0000;
       SMA_SHAFTPOSITION = $0001;
       SMA_SLIDERARMDIMENSIONS = $0002;
       SMA_SLIDERARMPOSITION = $0003;
       SMA_RANGEVALUE = $0000;
       SMA_INCREMENTVALUE = $0001;
       SMA_SETALLTICKS = $FFFF;
       SDA_RIBBONSTRIP = $0001;
       SDA_SLIDERSHAFT = $0002;
       SDA_BACKGROUND = $0003;
       SDA_SLIDERARM = $0004;
       PMERR_UPDATE_IN_PROGRESS = $1f06;
       SLDERR_INVALID_PARAMETERS = -1;
       VM_QUERYITEM = $0375;
       VM_QUERYITEMATTR = $0376;
       VM_QUERYMETRICS = $0377;
       VM_QUERYSELECTEDITEM = $0378;
       VM_SELECTITEM = $0379;
       VM_SETITEM = $037a;
       VM_SETITEMATTR = $037b;
       VM_SETMETRICS = $037c;
       VN_SELECT = 120;
       VN_ENTER = 121;
       VN_DRAGLEAVE = 122;
       VN_DRAGOVER = 123;
       VN_DROP = 124;
       VN_DROPHELP = 125;
       VN_INITDRAG = 126;
       VN_SETFOCUS = 127;
       VN_KILLFOCUS = 128;
       VN_HELP = 129;

    type
       VSCData = record
          cbSize : cardinal;
          usRowCount : word;
          usColumnCount : word;
       end;

       PVSCData = ^VSCData;

       VSDragInit = record
          hwnd : HWnd;
          x : longint;
          y : longint;
          cx : longint;
          cy : longint;
          usRow : word;
          usColumn : word;
       end;

       PVSDragInit = ^VSDragInit;

       VSDragInfo = record
          pDragInfo : PDragInfo;
          usRow : word;
          usColumn : word;
       end;

       PVSDragInfo = ^VSDragInfo;

       VSText = record
          pszItemText : PChar;
          ulBufLen : cardinal;
       end;

       PVSText = ^VSText;

    const
       VS_BITMAP = $0001;
       VS_ICON = $0002;
       VS_TEXT = $0004;
       VS_RGB = $0008;
       VS_COLORINDEX = $0010;
       VS_BORDER = $0020;
       VS_ITEMBORDER = $0040;
       VS_SCALEBITMAPS = $0080;
       VS_RIGHTTOLEFT = $0100;
       VS_OWNERDRAW = $0200;
       VIA_BITMAP = $0001;
       VIA_ICON = $0002;
       VIA_TEXT = $0004;
       VIA_RGB = $0008;
       VIA_COLORINDEX = $0010;
       VIA_OWNERDRAW = $0020;
       VIA_DISABLED = $0040;
       VIA_DRAGGABLE = $0080;
       VIA_DROPONABLE = $0100;
       VMA_ITEMSIZE = $0001;
       VMA_ITEMSPACING = $0002;
       VDA_ITEM = $0001;
       VDA_ITEMBACKGROUND = $0002;
       VDA_SURROUNDING = $0003;
       VDA_BACKGROUND = $0004;
       VSERR_INVALID_PARAMETERS = -1;
       BKM_CALCPAGERECT = $0353;
       BKM_DELETEPAGE = $0354;
       BKM_INSERTPAGE = $0355;
       BKM_INVALIDATETABS = $0356;
       BKM_TURNTOPAGE = $0357;
       BKM_QUERYPAGECOUNT = $0358;
       BKM_QUERYPAGEID = $0359;
       BKM_QUERYPAGEDATA = $035a;
       BKM_QUERYPAGEWINDOWHWND = $035b;
       BKM_QUERYTABBITMAP = $035c;
       BKM_QUERYTABTEXT = $035d;
       BKM_SETDIMENSIONS = $035e;
       BKM_SETPAGEDATA = $035f;
       BKM_SETPAGEWINDOWHWND = $0360;
       BKM_SETSTATUSLINETEXT = $0361;
       BKM_SETTABBITMAP = $0362;
       BKM_SETTABTEXT = $0363;
       BKM_SETNOTEBOOKCOLORS = $0364;
       BKM_QUERYPAGESTYLE = $0365;
       BKM_QUERYSTATUSLINETEXT = $0366;
       BKN_PAGESELECTED = 130;
       BKN_NEWPAGESIZE = 131;
       BKN_HELP = 132;
       BKN_PAGEDELETED = 133;
       BKA_ALL = $0001;
       BKA_SINGLE = $0002;
       BKA_TAB = $0004;
       BKA_LAST = $0002;
       BKA_FIRST = $0004;
       BKA_NEXT = $0008;
       BKA_PREV = $0010;
       BKA_TOP = $0020;
       BKA_MAJORTAB = $0001;
       BKA_MINORTAB = $0002;
       BKA_PAGEBUTTON = $0100;
       BKA_STATUSTEXTON = $0001;
       BKA_MAJOR = $0040;
       BKA_MINOR = $0080;
       BKA_AUTOPAGESIZE = $0100;
       BKA_END = $0200;
       BKA_TEXT = $0400;
       BKA_BITMAP = $0800;
       BKS_BACKPAGESBR = $00000001;
       BKS_BACKPAGESBL = $00000002;
       BKS_BACKPAGESTR = $00000004;
       BKS_BACKPAGESTL = $00000008;
       BKS_MAJORTABRIGHT = $00000010;
       BKS_MAJORTABLEFT = $00000020;
       BKS_MAJORTABTOP = $00000040;
       BKS_MAJORTABBOTTOM = $00000080;
       BKS_SQUARETABS = $00000000;
       BKS_ROUNDEDTABS = $00000100;
       BKS_POLYGONTABS = $00000200;
       BKS_SOLIDBIND = $00000000;
       BKS_SPIRALBIND = $00000400;
       BKS_STATUSTEXTLEFT = $00000000;
       BKS_STATUSTEXTRIGHT = $00001000;
       BKS_STATUSTEXTCENTER = $00002000;
       BKS_TABTEXTLEFT = $00000000;
       BKS_TABTEXTRIGHT = $00004000;
       BKS_TABTEXTCENTER = $00008000;
       BKA_BACKGROUNDPAGECOLORINDEX = $0001;
       BKA_BACKGROUNDPAGECOLOR = $0002;
       BKA_BACKGROUNDMAJORCOLORINDEX = $0003;
       BKA_BACKGROUNDMAJORCOLOR = $0004;
       BKA_BACKGROUNDMINORCOLORINDEX = $0005;
       BKA_BACKGROUNDMINORCOLOR = $0006;
       BKA_FOREGROUNDMAJORCOLORINDEX = $0007;
       BKA_FOREGROUNDMAJORCOLOR = $0008;
       BKA_FOREGROUNDMINORCOLORINDEX = $0009;
       BKA_FOREGROUNDMINORCOLOR = $000A;
       BOOKERR_INVALID_PARAMETERS = -1;

    type
       BookText = record
          pString : PChar;
          textLen : cardinal;
       end;

       PBookText = ^BookText;

       DeleteNotify = record
          hwndBook : HWnd;
          hwndPage : HWnd;
          ulAppPageData : cardinal;
          hbmTab : HBitmap;
       end;

       PDeleteNotify = ^DeleteNotify;

       PageSelectNotify = record
          hwndBook : HWnd;
          ulPageIdCur : cardinal;
          ulPageIdNew : cardinal;
       end;

       PPageSelectNotify = ^PageSelectNotify;

  implementation

    function WinFileDlg(hwndP : HWnd;hwndO : HWnd;pfild : PFileDlg) : HWnd; cdecl;
        external 'PMCTLS' index 4;
    function WinDefFileDlgProc(hwnd : HWnd;msg : cardinal;mp1 : MParam;mp2 : MParam) : MResult; cdecl;
        external 'PMCTLS' index 5;
    function WinFreeFileDlgList(papszFQFilename : PAPSZ) : Longbool; cdecl;
        external 'PMCTLS' index 6;
    function WinFontDlg(hwndP : HWnd;hwndO : HWnd;pfntd : PFontDlg) : HWnd; cdecl;
        external 'PMCTLS' index 2;
    function WinDefFontDlgProc(_hwnd : HWnd;msg : cardinal;mp1 : MParam;mp2 : MParam) : MResult; cdecl;
        external 'PMCTLS' index 3;
    function DrgAcceptDroppedFiles(hwnd : HWnd;pszPath : PChar;pszTypes : PChar;ulDefaultOp : cardinal;ulRsvd : cardinal) : Longbool; cdecl;
        external 'PMDRAG' index 66;
    function DrgAllocDraginfo(cditem : cardinal) : PDragInfo; cdecl;
        external 'PMDRAG' index 34;
    function DrgAllocDragtransfer(cdxfer : cardinal) : PDragTransfer; cdecl;
        external 'PMDRAG' index 35;
    function DrgDrag(hwndSource : HWnd;pdinfo : PDragInfo;pdimg : PDragImage;cdimg : cardinal;vkTerminate : longint; var pRsvd) : HWnd; cdecl;
        external 'PMDRAG' index 38;
    function DrgDragFiles(hwnd : HWnd;apszFiles : PPSZ;apszTypes : PPSZ;apszTargets : PPSZ;cFiles : cardinal;hptrDrag : cardinal;vkTerm : cardinal;fSourceRender : Longbool;ulRsvd : cardinal) : Longbool; cdecl;
        external 'PMDRAG' index 65;
    function DrgPostTransferMsg(hwnd : HWnd;msg : cardinal;pdxfer : PDragTransfer;fl : cardinal;ulRsvd : cardinal;fRetry : Longbool) : Longbool; cdecl;
        external 'PMDRAG' index 42;
    function DrgQueryDragitem(pdinfo : PDragInfo;cbBuffer : cardinal;pditem : PDragItem;iItem : cardinal) : Longbool; cdecl;
        external 'PMDRAG' index 44;
    function DrgQueryDragitemCount(pdinfo : PDragInfo) : cardinal; cdecl;
        external 'PMDRAG' index 45;
    function DrgQueryDragitemPtr(pdinfo : PDragInfo;i : cardinal) : PDragItem; cdecl;
        external 'PMDRAG' index 46;
    function DrgQueryNativeRMF(pditem : PDragItem;cbBuffer : cardinal;pBuffer : PCHAR) : Longbool; cdecl;
        external 'PMDRAG' index 47;
    function DrgQueryNativeRMFLen(pditem : PDragItem) : cardinal; cdecl;
        external 'PMDRAG' index 48;
    function DrgQueryStrName(hstr : HStr;cbBuffer : cardinal;pBuffer : PChar) : cardinal; cdecl;
        external 'PMDRAG' index 49;
    function DrgQueryStrNameLen(hstr : HStr) : cardinal; cdecl;
        external 'PMDRAG' index 50;
    function DrgQueryTrueType(pditem : PDragItem;cbBuffer : cardinal;pBuffer : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 51;
    function DrgQueryTrueTypeLen(pditem : PDragItem) : cardinal; cdecl;
        external 'PMDRAG' index 52;
    function DrgSendTransferMsg(hwnd : HWnd;msg : cardinal;mp1 : MParam;mp2 : MParam) : MResult; cdecl;
        external 'PMDRAG' index 54;
    function DrgSetDragitem(pdinfo : PDragInfo;pditem : PDragItem;cbBuffer : cardinal;iItem : cardinal) : Longbool; cdecl;
        external 'PMDRAG' index 57;
    function DrgSetDragImage(pdinfo : PDragInfo;pdimg : PDragImage;cdimg : cardinal; var pRsvd) : Longbool; cdecl;
        external 'PMDRAG' index 56;
    function DrgVerifyTypeSet(pditem : PDragItem;pszType : PChar;cbMatch : cardinal;pszMatch : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 62;
    function DrgAccessDraginfo(pdinfo : PDragInfo) : Longbool; cdecl;
        external 'PMDRAG' index 32;
    function DrgAddStrHandle(PSZ : PChar) : HStr; cdecl;
        external 'PMDRAG' index 33;
    function DrgDeleteDraginfoStrHandles(pdinfo : PDragInfo) : Longbool; cdecl;
        external 'PMDRAG' index 36;
    function DrgDeleteStrHandle(hstr : HStr) : Longbool; cdecl;
        external 'PMDRAG' index 37;
    function DrgFreeDraginfo(pdinfo : PDragInfo) : Longbool; cdecl;
        external 'PMDRAG' index 39;
    function DrgFreeDragtransfer(pdxfer : PDragTransfer) : Longbool; cdecl;
        external 'PMDRAG' index 40;
    function DrgGetPS(hwnd : HWnd) : HPS; cdecl;
        external 'PMDRAG' index 41;
    function DrgPushDraginfo(pdinfo : PDragInfo;hwndDest : HWnd) : Longbool; cdecl;
        external 'PMDRAG' index 43;
    function DrgReleasePS(hps : HPS) : Longbool; cdecl;
        external 'PMDRAG' index 53;
    function DrgSetDragPointer(pdinfo : PDragInfo;hptr : cardinal) : Longbool; cdecl;
        external 'PMDRAG' index 55;
    function DrgVerifyNativeRMF(pditem : PDragItem;pszRMF : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 58;
    function DrgVerifyRMF(pditem : PDragItem;pszMech : PChar;pszFmt : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 59;
    function DrgVerifyTrueType(pditem : PDragItem;pszType : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 60;
    function DrgVerifyType(pditem : PDragItem;pszType : PChar) : Longbool; cdecl;
        external 'PMDRAG' index 61;

end.
