{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit pmwin;

  interface

    uses
       os2def;

    type
       MPARAM = pointer;

       PMPARAM = ^MPARAM;

       MRESULT = pointer;

       PMRESULT = ^MRESULT;

       {!!!!!!!!! eigentlich Prozedurevariablen }
       FNWP = pointer;
       PFN = pointer;  { muá auáerdem in OS2DEF.PP definiert werden }

       PFNWP = ^FNWP;

    const
       WS_VISIBLE = $80000000;
       WS_DISABLED = $40000000;
       WS_CLIPCHILDREN = $20000000;
       WS_CLIPSIBLINGS = $10000000;
       WS_PARENTCLIP = $08000000;
       WS_SAVEBITS = $04000000;
       WS_SYNCPAINT = $02000000;
       WS_MINIMIZED = $01000000;
       WS_MAXIMIZED = $00800000;
       WS_ANIMATE = $00400000;
       WS_GROUP = $00010000;
       WS_TABSTOP = $00020000;
       WS_MULTISELECT = $00040000;
       CS_MOVENOTIFY = $00000001;
       CS_SIZEREDRAW = $00000004;
       CS_HITTEST = $00000008;
       CS_PUBLIC = $00000010;
       CS_FRAME = $00000020;
       CS_CLIPCHILDREN = $20000000;
       CS_CLIPSIBLINGS = $10000000;
       CS_PARENTCLIP = $08000000;
       CS_SAVEBITS = $04000000;
       CS_SYNCPAINT = $02000000;

       HWND_DESKTOP = 1;
       HWND_OBJECT = 2;
       HWND_TOP = 3;
       HWND_BOTTOM = 4;
       HWND_THREADCAPTURE = 5;

    function WinRegisterClass(hab : HAB;pszClassName : PSZ;pfnWndProc : PFNWP;flStyle : ULONG;cbWindowData : ULONG) : BOOL;

    function WinDefWindowProc(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;

    function WinDestroyWindow(hwnd : HWND) : BOOL;

    function WinShowWindow(hwnd : HWND;fShow : BOOL) : BOOL;

    function WinQueryWindowRect(hwnd : HWND;prclDest : PRECTL) : BOOL;

    function WinGetPS(hwnd : HWND) : HPS;

    function WinReleasePS(hps : HPS) : BOOL;

    function WinEndPaint(hps : HPS) : BOOL;

    function WinGetClipPS(hwnd : HWND;hwndClip : HWND;fl : ULONG) : HPS;

    function WinIsWindowShowing(hwnd : HWND) : BOOL;

    function WinBeginPaint(hwnd : HWND;hps : HPS;prclPaint : PRECTL) : HPS;

    function WinOpenWindowDC(hwnd : HWND) : HDC;

    function WinScrollWindow(hwnd : HWND;dx : LONG;dy : LONG;prclScroll : PRECTL;prclClip : PRECTL;hrgnUpdate : HRGN;prclUpdate : PRECTL;rgfsw : ULONG) : LONG;

    const
       PSF_LOCKWINDOWUPDATE = $0001;
       PSF_CLIPUPWARDS = $0002;
       PSF_CLIPDOWNWARDS = $0004;
       PSF_CLIPSIBLINGS = $0008;
       PSF_CLIPCHILDREN = $0010;
       PSF_PARENTCLIP = $0020;
       SW_SCROLLCHILDREN = $0001;
       SW_INVALIDATERGN = $0002;

    function WinFillRect(hps : HPS;prcl : PRECTL;lColor : LONG) : BOOL;

    type
       QVERSDATA = record
          environment : USHORT;
          version : USHORT;
       end;

       PQVERSDATA = ^QVERSDATA;

    const
       QV_OS2 = $0000;
       QV_CMS = $0001;
       QV_TSO = $0002;
       QV_TSOBATCH = $0003;
       QV_OS400 = $0004;

    function WinQueryVersion(hab : HAB) : ULONG;

    function WinInitialize(flOptions : ULONG) : HAB;

    function WinTerminate(hab : HAB) : BOOL;

    function WinQueryAnchorBlock(hwnd : HWND) : HAB;

    function WinCreateWindow(hwndParent : HWND;pszClass : PSZ;pszName : PSZ;flStyle : ULONG;x : LONG;y : LONG;cx : LONG;cy : LONG;hwndOwner : HWND;hwndInsertBehind : HWND;id : ULONG;pCtlData : PVOID;pPresParams : PVOID) : HWND;

    function WinEnableWindow(hwnd : HWND;fEnable : BOOL) : BOOL;

    function WinIsWindowEnabled(hwnd : HWND) : BOOL;

    function WinEnableWindowUpdate(hwnd : HWND;fEnable : BOOL) : BOOL;

    function WinIsWindowVisible(hwnd : HWND) : BOOL;

    function WinQueryWindowText(hwnd : HWND;cchBufferMax : LONG;pchBuffer : PCH) : LONG;

    function WinSetWindowText(hwnd : HWND;pszText : PSZ) : BOOL;

    function WinQueryWindowTextLength(hwnd : HWND) : LONG;

    function WinWindowFromID(hwndParent : HWND;id : ULONG) : HWND;

    function WinIsWindow(hab : HAB;hwnd : HWND) : BOOL;

    function WinQueryWindow(hwnd : HWND;cmd : LONG) : HWND;

    function WinMultWindowFromIDs(hwndParent : HWND;prghwnd : PHWND;idFirst : ULONG;idLast : ULONG) : LONG;

    const
       QW_NEXT = 0;
       QW_PREV = 1;
       QW_TOP = 2;
       QW_BOTTOM = 3;
       QW_OWNER = 4;
       QW_PARENT = 5;
       QW_NEXTTOP = 6;
       QW_PREVTOP = 7;
       QW_FRAMEOWNER = 8;

    function WinSetParent(hwnd : HWND;hwndNewParent : HWND;fRedraw : BOOL) : BOOL;

    function WinIsChild(hwnd : HWND;hwndParent : HWND) : BOOL;

    function WinSetOwner(hwnd : HWND;hwndNewOwner : HWND) : BOOL;

    function WinQueryWindowProcess(hwnd : HWND;ppid : PPID;ptid : PTID) : BOOL;

    function WinQueryObjectWindow(hwndDesktop : HWND) : HWND;

    function WinQueryDesktopWindow(hab : HAB;hdc : HDC) : HWND;

    type
       SWP = record
          fl : ULONG;
          cy : LONG;
          cx : LONG;
          y : LONG;
          x : LONG;
          hwndInsertBehind : HWND;
          hwnd : HWND;
          ulReserved1 : ULONG;
          ulReserved2 : ULONG;
       end;

       PSWP = ^SWP;


    function WinSetWindowPos(hwnd : HWND;hwndInsertBehind : HWND;x : LONG;y : LONG;cx : LONG;cy : LONG;fl : ULONG) : BOOL;

    function WinSetMultWindowPos(hab : HAB;pswp : PSWP;cswp : ULONG) : BOOL;

    function WinQueryWindowPos(hwnd : HWND;pswp : PSWP) : BOOL;

    const
       AWP_MINIMIZED = $00010000;
       AWP_MAXIMIZED = $00020000;
       AWP_RESTORED = $00040000;
       AWP_ACTIVATE = $00080000;
       AWP_DEACTIVATE = $00100000;
       SWP_SIZE = $0001;
       SWP_MOVE = $0002;
       SWP_ZORDER = $0004;
       SWP_SHOW = $0008;
       SWP_HIDE = $0010;
       SWP_NOREDRAW = $0020;
       SWP_NOADJUST = $0040;
       SWP_ACTIVATE = $0080;
       SWP_DEACTIVATE = $0100;
       SWP_EXTSTATECHANGE = $0200;
       SWP_MINIMIZE = $0400;
       SWP_MAXIMIZE = $0800;
       SWP_RESTORE = $1000;
       SWP_FOCUSACTIVATE = $2000;
       SWP_FOCUSDEACTIVATE = $4000;
       SWP_NOAUTOCLOSE = $8000;

    function WinUpdateWindow(hwnd : HWND) : BOOL;

    function WinInvalidateRect(hwnd : HWND;pwrc : PRECTL;fIncludeChildren : BOOL) : BOOL;

    function WinInvalidateRegion(hwnd : HWND;hrgn : HRGN;fIncludeChildren : BOOL) : BOOL;

    function WinInvertRect(hps : HPS;prcl : PRECTL) : BOOL;

    function WinDrawBitmap(hpsDst : HPS;hbm : HBITMAP;pwrcSrc : PRECTL;pptlDst : PPOINTL;clrFore : LONG;clrBack : LONG;fl : ULONG) : BOOL;

    const
       DBM_NORMAL = $0000;
       DBM_INVERT = $0001;
       DBM_HALFTONE = $0002;
       DBM_STRETCH = $0004;
       DBM_IMAGEATTRS = $0008;

    function WinDrawText(hps : HPS;cchText : LONG;lpchText : PCH;prcl : PRECTL;clrFore : LONG;clrBack : LONG;flCmd : ULONG) : LONG;

    const
       DT_LEFT = $0000;
       DT_QUERYEXTENT = $0002;
       DT_UNDERSCORE = $0010;
       DT_STRIKEOUT = $0020;
       DT_TEXTATTRS = $0040;
       DT_EXTERNALLEADING = $0080;
       DT_CENTER = $0100;
       DT_RIGHT = $0200;
       DT_TOP = $0000;
       DT_VCENTER = $0400;
       DT_BOTTOM = $0800;
       DT_HALFTONE = $1000;
       DT_MNEMONIC = $2000;
       DT_WORDBREAK = $4000;
       DT_ERASERECT = $8000;

    function WinDrawBorder(hps : HPS;prcl : PRECTL;cx : LONG;cy : LONG;clrFore : LONG;clrBack : LONG;flCmd : ULONG) : BOOL;

    const
       DB_PATCOPY = $0000;
       DB_PATINVERT = $0001;
       DB_DESTINVERT = $0002;
       DB_AREAMIXMODE = $0003;
       DB_ROP = $0007;
       DB_INTERIOR = $0008;
       DB_AREAATTRS = $0010;
       DB_STANDARD = $0100;
       DB_DLGBORDER = $0200;

    function WinLoadString(hab : HAB;hmod : HMODULE;id : ULONG;cchMax : LONG;pchBuffer : PSZ) : LONG;

    function WinLoadMessage(hab : HAB;hmod : HMODULE;id : ULONG;cchMax : LONG;pchBuffer : PSZ) : LONG;

    function WinSetActiveWindow(hwndDesktop : HWND;hwnd : HWND) : BOOL;

    type
       CREATESTRUCT = record
          pPresParams : PVOID;
          pCtlData : PVOID;
          id : ULONG;
          hwndInsertBehind : HWND;
          hwndOwner : HWND;
          cy : LONG;
          cx : LONG;
          y : LONG;
          x : LONG;
          flStyle : ULONG;
          pszText : PSZ;
          pszClass : PSZ;
          hwndParent : HWND;
       end;

       PCREATESTRUCT = ^CREATESTRUCT;

       CLASSINFO = record
          flClassStyle : ULONG;
          pfnWindowProc : PFNWP;
          cbWindowData : ULONG;
       end;

       PCLASSINFO = ^CLASSINFO;


    function WinSubclassWindow(hwnd : HWND;pfnwp : PFNWP) : PFNWP;

    function WinQueryClassName(hwnd : HWND;cchMax : LONG;pch : PCH) : LONG;

    function WinQueryClassInfo(hab : HAB;pszClassName : PSZ;pClassInfo : PCLASSINFO) : BOOL;

    function WinQueryActiveWindow(hwndDesktop : HWND) : HWND;

    function WinIsThreadActive(hab : HAB) : BOOL;

    function WinQuerySysModalWindow(hwndDesktop : HWND) : HWND;

    function WinSetSysModalWindow(hwndDesktop : HWND;hwnd : HWND) : BOOL;

    function WinQueryWindowUShort(hwnd : HWND;index : LONG) : USHORT;

    function WinSetWindowUShort(hwnd : HWND;index : LONG;us : USHORT) : BOOL;

    function WinQueryWindowULong(hwnd : HWND;index : LONG) : ULONG;

    function WinSetWindowULong(hwnd : HWND;index : LONG;ul : ULONG) : BOOL;

    function WinQueryWindowPtr(hwnd : HWND;index : LONG) : PVOID;

    function WinSetWindowPtr(hwnd : HWND;index : LONG;p : PVOID) : BOOL;

    function WinSetWindowBits(hwnd : HWND;index : LONG;flData : ULONG;flMask : ULONG) : BOOL;

    const
       QWS_USER = 0;
       QWS_ID = -1;
       QWS_MIN = -1;
       QWL_USER = 0;
       QWL_STYLE = -2;
       QWP_PFNWP = -3;
       QWL_HMQ = -4;
       QWL_RESERVED = -5;
       QWL_MIN = -6;
       QWL_HHEAP = $0004;
       QWL_HWNDFOCUSSAVE = $0018;
       QWL_DEFBUTTON = $0040;
       QWL_PSSCBLK = $0048;
       QWL_PFEPBLK = $004c;
       QWL_PSTATBLK = $0050;
       QWS_FLAGS = $0008;
       QWS_RESULT = $000a;
       QWS_XRESTORE = $000c;
       QWS_YRESTORE = $000e;
       QWS_CXRESTORE = $0010;
       QWS_CYRESTORE = $0012;
       QWS_XMINIMIZE = $0014;
       QWS_YMINIMIZE = $0016;

    type
       HENUM = LHANDLE;


    function WinBeginEnumWindows(hwnd : HWND) : HENUM;

    function WinGetNextWindow(henum : HENUM) : HWND;

    function WinEndEnumWindows(henum : HENUM) : BOOL;

    function WinWindowFromPoint(hwnd : HWND;pptl : PPOINTL;fChildren : BOOL) : HWND;

    function WinMapWindowPoints(hwndFrom : HWND;hwndTo : HWND;prgptl : PPOINTL;cwpt : LONG) : BOOL;

    function WinValidateRect(hwnd : HWND;prcl : PRECTL;fIncludeChildren : BOOL) : BOOL;

    function WinValidateRegion(hwnd : HWND;hrgn : HRGN;fIncludeChildren : BOOL) : BOOL;

    function WinWindowFromDC(hdc : HDC) : HWND;

    function WinQueryWindowDC(hwnd : HWND) : HDC;

    function WinGetScreenPS(hwndDesktop : HWND) : HPS;

    function WinLockWindowUpdate(hwndDesktop : HWND;hwndLockUpdate : HWND) : BOOL;

    function WinLockVisRegions(hwndDesktop : HWND;fLock : BOOL) : BOOL;

    function WinQueryUpdateRect(hwnd : HWND;prcl : PRECTL) : BOOL;

    function WinQueryUpdateRegion(hwnd : HWND;hrgn : HRGN) : LONG;

    function WinExcludeUpdateRegion(hps : HPS;hwnd : HWND) : LONG;

    type
       QMSG = record
          hwnd : HWND;
          msg : ULONG;
          mp1 : MPARAM;
          mp2 : MPARAM;
          time : ULONG;
          ptl : POINTL;
          reserved : ULONG;
       end;

       PQMSG = ^QMSG;

    const
       WM_NULL = $0000;
       WM_CREATE = $0001;
       WM_DESTROY = $0002;
       WM_ENABLE = $0004;
       WM_SHOW = $0005;
       WM_MOVE = $0006;
       WM_SIZE = $0007;
       WM_ADJUSTWINDOWPOS = $0008;
       WM_CALCVALIDRECTS = $0009;
       WM_SETWINDOWPARAMS = $000a;
       WM_QUERYWINDOWPARAMS = $000b;
       WM_HITTEST = $000c;
       WM_ACTIVATE = $000d;
       WM_SETFOCUS = $000f;
       WM_SETSELECTION = $0010;
       WM_PPAINT = $0011;
       WM_PSETFOCUS = $0012;
       WM_PSYSCOLORCHANGE = $0013;
       WM_PSIZE = $0014;
       WM_PACTIVATE = $0015;
       WM_PCONTROL = $0016;
       WM_COMMAND = $0020;
       WM_SYSCOMMAND = $0021;
       WM_HELP = $0022;
       WM_PAINT = $0023;
       WM_TIMER = $0024;
       WM_SEM1 = $0025;
       WM_SEM2 = $0026;
       WM_SEM3 = $0027;
       WM_SEM4 = $0028;
       WM_CLOSE = $0029;
       WM_QUIT = $002a;
       WM_SYSCOLORCHANGE = $002b;
       WM_SYSVALUECHANGED = $002d;
       WM_APPTERMINATENOTIFY = $002e;
       WM_PRESPARAMCHANGED = $002f;
       WM_CONTROL = $0030;
       WM_VSCROLL = $0031;
       WM_HSCROLL = $0032;
       WM_INITMENU = $0033;
       WM_MENUSELECT = $0034;
       WM_MENUEND = $0035;
       WM_DRAWITEM = $0036;
       WM_MEASUREITEM = $0037;
       WM_CONTROLPOINTER = $0038;
       WM_QUERYDLGCODE = $003a;
       WM_INITDLG = $003b;
       WM_SUBSTITUTESTRING = $003c;
       WM_MATCHMNEMONIC = $003d;
       WM_SAVEAPPLICATION = $003e;
       WM_HELPBASE = $0F00;
       WM_HELPTOP = $0FFF;
       WM_USER = $1000;
       CMDSRC_PUSHBUTTON = 1;
       CMDSRC_MENU = 2;
       CMDSRC_ACCELERATOR = 3;
       CMDSRC_FONTDLG = 4;
       CMDSRC_FILEDLG = 5;
       CMDSRC_PRINTDLG = 6;
       CMDSRC_COLORDLG = 7;
       CMDSRC_OTHER = 0;
{$PACKRECORDS 1}


    type
       CMDMSG = record
          cmd : USHORT;
          unused : USHORT;
          source : USHORT;
          fMouse : USHORT;
       end;

       PCMDMSG = ^CMDMSG;

{$PACKRECORDS NORMAL}
       MQINFO = record
          cb : ULONG;
          pid : PID;
          tid : TID;
          cmsgs : ULONG;
          pReserved : PVOID;
       end;

       PMQINFO = ^MQINFO;

    function WinSendMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;

    function WinCreateMsgQueue(hab : HAB;cmsg : LONG) : HMQ;

    function WinDestroyMsgQueue(hmq : HMQ) : BOOL;

    function WinQueryQueueInfo(hmq : HMQ;pmqi : PMQINFO;cbCopy : ULONG) : BOOL;

    function WinCancelShutdown(hmq : HMQ;fCancelAlways : BOOL) : BOOL;

    function WinGetMsg(hab : HAB;pqmsg : PQMSG;hwndFilter : HWND;msgFilterFirst : ULONG;msgFilterLast : ULONG) : BOOL;

    function WinPeekMsg(hab : HAB;pqmsg : PQMSG;hwndFilter : HWND;msgFilterFirst : ULONG;msgFilterLast : ULONG;fl : ULONG) : BOOL;

    function WinDispatchMsg(hab : HAB;pqmsg : PQMSG) : MRESULT;

    function WinPostMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : BOOL;

    function WinRegisterUserMsg(hab : HAB;msgid : ULONG;datatype1 : LONG;dir1 : LONG;datatype2 : LONG;dir2 : LONG;datatyper : LONG) : BOOL;

    function WinRegisterUserDatatype(hab : HAB;datatype : LONG;count : LONG;types : PLONG) : BOOL;

    function WinSetMsgMode(hab : HAB;classname : PSZ;control : LONG) : BOOL;

    function WinSetSynchroMode(hab : HAB;mode : LONG) : BOOL;

    const
       PM_REMOVE = $0001;
       PM_NOREMOVE = $0000;
       RUM_IN = 1;
       RUM_OUT = 2;
       RUM_INOUT = 3;
       SMD_DELAYED = $0001;
       SMD_IMMEDIATE = $0002;
       SSM_SYNCHRONOUS = $0001;
       SSM_ASYNCHRONOUS = $0002;
       SSM_MIXED = $0003;
       CVR_ALIGNLEFT = $0001;
       CVR_ALIGNBOTTOM = $0002;
       CVR_ALIGNRIGHT = $0004;
       CVR_ALIGNTOP = $0008;
       CVR_REDRAW = $0010;
       HT_NORMAL = 0;
       HT_TRANSPARENT = (-1);
       HT_DISCARD = (-2);
       HT_ERROR = (-3);

    type
       WNDPARAMS = record
          fsStatus : ULONG;
          cchText : ULONG;
          pszText : PSZ;
          cbPresParams : ULONG;
          pPresParams : PVOID;
          cbCtlData : ULONG;
          pCtlData : PVOID;
       end;

       PWNDPARAMS = ^WNDPARAMS;

    const
       WPM_TEXT = $0001;
       WPM_CTLDATA = $0002;
       WPM_PRESPARAMS = $0004;
       WPM_CCHTEXT = $0008;
       WPM_CBCTLDATA = $0010;
       WPM_CBPRESPARAMS = $0020;

    function WinInSendMsg(hab : HAB) : BOOL;

    function WinBroadcastMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM;rgf : ULONG) : BOOL;

    const
       BMSG_POST = $0000;
       BMSG_SEND = $0001;
       BMSG_POSTQUEUE = $0002;
       BMSG_DESCENDANTS = $0004;
       BMSG_FRAMEONLY = $0008;

    function WinWaitMsg(hab : HAB;msgFirst : ULONG;msgLast : ULONG) : BOOL;

    function WinQueryQueueStatus(hwndDesktop : HWND) : ULONG;

    const
       QS_KEY = $0001;
       QS_MOUSEBUTTON = $0002;
       QS_MOUSEMOVE = $0004;
       QS_MOUSE = $0006;
       QS_TIMER = $0008;
       QS_PAINT = $0010;
       QS_POSTMSG = $0020;
       QS_SEM1 = $0040;
       QS_SEM2 = $0080;
       QS_SEM3 = $0100;
       QS_SEM4 = $0200;
       QS_SENDMSG = $0400;

    function WinQueryMsgPos(hab : HAB;pptl : PPOINTL) : BOOL;

    function WinQueryMsgTime(hab : HAB) : ULONG;

    type
       HEV = ULONG;

       HMTX = ULONG;

       HMUX = ULONG;


    function WinWaitEventSem(hev : HEV;ulTimeout : ULONG) : APIRET;

    function WinRequestMutexSem(hmtx : HMTX;ulTimeout : ULONG) : APIRET;

    function WinWaitMuxWaitSem(hmux : HMUX;ulTimeout : ULONG;pulUser : PULONG) : APIRET;

    function WinPostQueueMsg(hmq : HMQ;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : BOOL;

    const
       SMIM_ALL = $0EFF;
       SMI_NOINTEREST = $0001;
       SMI_INTEREST = $0002;
       SMI_RESET = $0004;
       SMI_AUTODISPATCH = $0008;

    function WinSetMsgInterest(hwnd : HWND;msg_class : ULONG;control : LONG) : BOOL;

    function WinSetClassMsgInterest(hab : HAB;pszClassName : PSZ;msg_class : ULONG;control : LONG) : BOOL;

    function WinSetFocus(hwndDesktop : HWND;hwndSetFocus : HWND) : BOOL;

    function WinFocusChange(hwndDesktop : HWND;hwndSetFocus : HWND;flFocusChange : ULONG) : BOOL;

    const
       FC_NOSETFOCUS = $0001;
       FC_NOBRINGTOTOP = FC_NOSETFOCUS;
       FC_NOLOSEFOCUS = $0002;
       FC_NOBRINGTOPFIRSTWINDOW = FC_NOLOSEFOCUS;
       FC_NOSETACTIVE = $0004;
       FC_NOLOSEACTIVE = $0008;
       FC_NOSETSELECTION = $0010;
       FC_NOLOSESELECTION = $0020;
       QFC_NEXTINCHAIN = $0001;
       QFC_ACTIVE = $0002;
       QFC_FRAME = $0003;
       QFC_SELECTACTIVE = $0004;
       QFC_PARTOFCHAIN = $0005;

    function WinSetCapture(hwndDesktop : HWND;hwnd : HWND) : BOOL;

    function WinQueryCapture(hwndDesktop : HWND) : HWND;

    const
       WM_MOUSEFIRST = $0070;
       WM_MOUSELAST = $0079;
       WM_BUTTONCLICKFIRST = $0071;
       WM_BUTTONCLICKLAST = $0079;
       WM_MOUSEMOVE = $0070;
       WM_BUTTON1DOWN = $0071;
       WM_BUTTON1UP = $0072;
       WM_BUTTON1DBLCLK = $0073;
       WM_BUTTON2DOWN = $0074;
       WM_BUTTON2UP = $0075;
       WM_BUTTON2DBLCLK = $0076;
       WM_BUTTON3DOWN = $0077;
       WM_BUTTON3UP = $0078;
       WM_BUTTON3DBLCLK = $0079;
       WM_EXTMOUSEFIRST = $0410;
       WM_EXTMOUSELAST = $0419;
       WM_CHORD = $0410;
       WM_BUTTON1MOTIONSTART = $0411;
       WM_BUTTON1MOTIONEND = $0412;
       WM_BUTTON1CLICK = $0413;
       WM_BUTTON2MOTIONSTART = $0414;
       WM_BUTTON2MOTIONEND = $0415;
       WM_BUTTON2CLICK = $0416;
       WM_BUTTON3MOTIONSTART = $0417;
       WM_BUTTON3MOTIONEND = $0418;
       WM_BUTTON3CLICK = $0419;
       WM_MOUSETRANSLATEFIRST = $0420;
       WM_MOUSETRANSLATELAST = $0428;
       WM_BEGINDRAG = $0420;
       WM_ENDDRAG = $0421;
       WM_SINGLESELECT = $0422;
       WM_OPEN = $0423;
       WM_CONTEXTMENU = $0424;
       WM_CONTEXTHELP = $0425;
       WM_TEXTEDIT = $0426;
       WM_BEGINSELECT = $0427;
       WM_ENDSELECT = $0428;

    function WinQueryFocus(hwndDesktop : HWND) : HWND;

    const
       WM_CHAR = $007a;
       WM_VIOCHAR = $007b;
       KC_NONE = $0000;
       KC_CHAR = $0001;
       KC_VIRTUALKEY = $0002;
       KC_SCANCODE = $0004;
       KC_SHIFT = $0008;
       KC_CTRL = $0010;
       KC_ALT = $0020;
       KC_KEYUP = $0040;
       KC_PREVDOWN = $0080;
       KC_LONEKEY = $0100;
       KC_DEADKEY = $0200;
       KC_COMPOSITE = $0400;
       KC_INVALIDCOMP = $0800;
       KC_TOGGLE = $1000;
       KC_INVALIDCHAR = $2000;
       KC_DBCSRSRVD1 = $4000;
       KC_DBCSRSRVD2 = $8000;
{$PACKRECORDS 1}


    type
       MSEMSG = record
          x : SHORT;
          y : SHORT;
          codeHitTest : USHORT;
          fsInp : USHORT;
       end;

       PMSEMSG = ^MSEMSG;

       CHRMSG = record
          fs : USHORT;
          cRepeat : UCHAR;
          scancode : UCHAR;
          chr : USHORT;
          vkey : USHORT;
       end;

       PCHRMSG = ^CHRMSG;

{$PACKRECORDS NORMAL}
    const
       INP_NONE = $0000;
       INP_KBD = $0001;
       INP_MULT = $0002;
       INP_RES2 = $0004;
       INP_SHIFT = $0008;
       INP_CTRL = $0010;
       INP_ALT = $0020;
       INP_RES3 = $0040;
       INP_RES4 = $0080;
       INP_IGNORE = $FFFF;
       VK_BUTTON1 = $01;
       VK_BUTTON2 = $02;
       VK_BUTTON3 = $03;
       VK_BREAK = $04;
       VK_BACKSPACE = $05;
       VK_TAB = $06;
       VK_BACKTAB = $07;
       VK_NEWLINE = $08;
       VK_SHIFT = $09;
       VK_CTRL = $0A;
       VK_ALT = $0B;
       VK_ALTGRAF = $0C;
       VK_PAUSE = $0D;
       VK_CAPSLOCK = $0E;
       VK_ESC = $0F;
       VK_SPACE = $10;
       VK_PAGEUP = $11;
       VK_PAGEDOWN = $12;
       VK_END = $13;
       VK_HOME = $14;
       VK_LEFT = $15;
       VK_UP = $16;
       VK_RIGHT = $17;
       VK_DOWN = $18;
       VK_PRINTSCRN = $19;
       VK_INSERT = $1A;
       VK_DELETE = $1B;
       VK_SCRLLOCK = $1C;
       VK_NUMLOCK = $1D;
       VK_ENTER = $1E;
       VK_SYSRQ = $1F;
       VK_F1 = $20;
       VK_F2 = $21;
       VK_F3 = $22;
       VK_F4 = $23;
       VK_F5 = $24;
       VK_F6 = $25;
       VK_F7 = $26;
       VK_F8 = $27;
       VK_F9 = $28;
       VK_F10 = $29;
       VK_F11 = $2A;
       VK_F12 = $2B;
       VK_F13 = $2C;
       VK_F14 = $2D;
       VK_F15 = $2E;
       VK_F16 = $2F;
       VK_F17 = $30;
       VK_F18 = $31;
       VK_F19 = $32;
       VK_F20 = $33;
       VK_F21 = $34;
       VK_F22 = $35;
       VK_F23 = $36;
       VK_F24 = $37;
       VK_ENDDRAG = $38;
       VK_MENU = VK_F10;
       VK_DBCSFIRST = $0080;
       VK_DBCSLAST = $00ff;
       VK_USERFIRST = $0100;
       VK_USERLAST = $01ff;

    function WinGetKeyState(hwndDesktop : HWND;vkey : LONG) : LONG;

    function WinGetPhysKeyState(hwndDesktop : HWND;sc : LONG) : LONG;

    function WinEnablePhysInput(hwndDesktop : HWND;fEnable : BOOL) : BOOL;

    function WinIsPhysInputEnabled(hwndDesktop : HWND) : BOOL;

    function WinSetKeyboardStateTable(hwndDesktop : HWND;pKeyStateTable : PBYTE;fSet : BOOL) : BOOL;

    const
       WM_JOURNALNOTIFY = $007c;
       JRN_QUEUESTATUS = $00000001;
       JRN_PHYSKEYSTATE = $00000002;

    function WinGetDlgMsg(hwndDlg : HWND;pqmsg : PQMSG) : BOOL;

    function WinLoadDlg(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;hmod : HMODULE;idDlg : ULONG;pCreateParams : PVOID) : HWND;

    function WinDlgBox(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;hmod : HMODULE;idDlg : ULONG;pCreateParams : PVOID) : ULONG;

    function WinDismissDlg(hwndDlg : HWND;usResult : ULONG) : BOOL;

    function WinQueryDlgItemShort(hwndDlg : HWND;idItem : ULONG;pResult : PSHORT;fSigned : BOOL) : BOOL;

    function WinSetDlgItemShort(hwndDlg : HWND;idItem : ULONG;usValue : USHORT;fSigned : BOOL) : BOOL;

    function WinSetDlgItemText(hwndDlg : HWND;idItem : ULONG;pszText : PSZ) : BOOL;

    function WinQueryDlgItemText(hwndDlg : HWND;idItem : ULONG;cchBufferMax : LONG;pchBuffer : PSZ) : ULONG;

    function WinQueryDlgItemTextLength(hwndDlg : HWND;idItem : ULONG) : LONG;

    function WinDefDlgProc(hwndDlg : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;

    const
       DID_OK = 1;
       DID_CANCEL = 2;
       DID_ERROR = $ffff;

    function WinAlarm(hwndDesktop : HWND;rgfType : ULONG) : BOOL;

    const
       WA_WARNING = 0;
       WA_NOTE = 1;
       WA_ERROR = 2;
       WA_CWINALARMS = 3;

    function WinMessageBox(hwndParent : HWND;hwndOwner : HWND;pszText : PSZ;pszCaption : PSZ;idWindow : ULONG;flStyle : ULONG) : ULONG;

    const
       MB_OK = $0000;
       MB_OKCANCEL = $0001;
       MB_RETRYCANCEL = $0002;
       MB_ABORTRETRYIGNORE = $0003;
       MB_YESNO = $0004;
       MB_YESNOCANCEL = $0005;
       MB_CANCEL = $0006;
       MB_ENTER = $0007;
       MB_ENTERCANCEL = $0008;
       MB_NOICON = $0000;
       MB_CUANOTIFICATION = $0000;
       MB_ICONQUESTION = $0010;
       MB_ICONEXCLAMATION = $0020;
       MB_CUAWARNING = $0020;
       MB_ICONASTERISK = $0030;
       MB_ICONHAND = $0040;
       MB_CUACRITICAL = $0040;
       MB_QUERY = MB_ICONQUESTION;
       MB_WARNING = MB_CUAWARNING;
       MB_INFORMATION = MB_ICONASTERISK;
       MB_CRITICAL = MB_CUACRITICAL;
       MB_ERROR = MB_CRITICAL;
       MB_DEFBUTTON1 = $0000;
       MB_DEFBUTTON2 = $0100;
       MB_DEFBUTTON3 = $0200;
       MB_APPLMODAL = $0000;
       MB_SYSTEMMODAL = $1000;
       MB_HELP = $2000;
       MB_MOVEABLE = $4000;
       MBID_OK = 1;
       MBID_CANCEL = 2;
       MBID_ABORT = 3;
       MBID_RETRY = 4;
       MBID_IGNORE = 5;
       MBID_YES = 6;
       MBID_NO = 7;
       MBID_HELP = 8;
       MBID_ENTER = 9;
       MBID_ERROR = $ffff;
       DLGC_ENTRYFIELD = $0001;
       DLGC_BUTTON = $0002;
       DLGC_RADIOBUTTON = $0004;
       DLGC_STATIC = $0008;
       DLGC_DEFAULT = $0010;
       DLGC_PUSHBUTTON = $0020;
       DLGC_CHECKBOX = $0040;
       DLGC_SCROLLBAR = $0080;
       DLGC_MENU = $0100;
       DLGC_TABONCLICK = $0200;
       DLGC_MLE = $0400;

    function WinProcessDlg(hwndDlg : HWND) : ULONG;

    function WinSendDlgItemMsg(hwndDlg : HWND;idItem : ULONG;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;

    function WinMapDlgPoints(hwndDlg : HWND;prgwptl : PPOINTL;cwpt : ULONG;fCalcWindowCoords : BOOL) : BOOL;

    function WinEnumDlgItem(hwndDlg : HWND;hwnd : HWND;code : ULONG) : HWND;

    function WinSubstituteStrings(hwnd : HWND;pszSrc : PSZ;cchDstMax : LONG;pszDst : PSZ) : LONG;

    const
       EDI_FIRSTTABITEM = 0;
       EDI_LASTTABITEM = 1;
       EDI_NEXTTABITEM = 2;
       EDI_PREVTABITEM = 3;
       EDI_FIRSTGROUPITEM = 4;
       EDI_LASTGROUPITEM = 5;
       EDI_NEXTGROUPITEM = 6;
       EDI_PREVGROUPITEM = 7;
{$PACKRECORDS 2}


    type
       DLGTITEM = record
          fsItemStatus : USHORT;
          cChildren : USHORT;
          cchClassName : USHORT;
          offClassName : USHORT;
          cchText : USHORT;
          offText : USHORT;
          flStyle : ULONG;
          x : SHORT;
          y : SHORT;
          cx : SHORT;
          cy : SHORT;
          id : USHORT;
          offPresParams : USHORT;
          offCtlData : USHORT;
       end;

       PDLGTITEM = ^DLGTITEM;

       DLGTEMPLATE = record
          cbTemplate : USHORT;
          _type : USHORT;
          codepage : USHORT;
          offadlgti : USHORT;
          fsTemplateStatus : USHORT;
          iItemFocus : USHORT;
          coffPresParams : USHORT;
          adlgti : array[0..0] of DLGTITEM;
       end;

       PDLGTEMPLATE = ^DLGTEMPLATE;

{$PACKRECORDS NORMAL}

    function WinCreateDlg(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;pdlgt : PDLGTEMPLATE;pCreateParams : PVOID) : HWND;

    const
       SS_TEXT = $0001;
       SS_GROUPBOX = $0002;
       SS_ICON = $0003;
       SS_BITMAP = $0004;
       SS_FGNDRECT = $0005;
       SS_HALFTONERECT = $0006;
       SS_BKGNDRECT = $0007;
       SS_FGNDFRAME = $0008;
       SS_HALFTONEFRAME = $0009;
       SS_BKGNDFRAME = $000a;
       SS_SYSICON = $000b;
       SS_AUTOSIZE = $0040;
       SM_SETHANDLE = $0100;
       SM_QUERYHANDLE = $0101;
       BS_PUSHBUTTON = 0;
       BS_CHECKBOX = 1;
       BS_AUTOCHECKBOX = 2;
       BS_RADIOBUTTON = 3;
       BS_AUTORADIOBUTTON = 4;
       BS_3STATE = 5;
       BS_AUTO3STATE = 6;
       BS_USERBUTTON = 7;
       BS_PRIMARYSTYLES = $000f;
       BS_BITMAP = $0040;
       BS_ICON = $0080;
       BS_HELP = $0100;
       BS_SYSCOMMAND = $0200;
       BS_DEFAULT = $0400;
       BS_NOPOINTERFOCUS = $0800;
       BS_NOBORDER = $1000;
       BS_NOCURSORSELECT = $2000;
       BS_AUTOSIZE = $4000;
{$PACKRECORDS 2}


    type
       BTNCDATA = record
          cb : USHORT;
          fsCheckState : USHORT;
          fsHiliteState : USHORT;
          hImage : LHANDLE;
       end;

       PBTNCDATA = ^BTNCDATA;

{$PACKRECORDS NORMAL}
       USERBUTTON = record
          hwnd : HWND;
          hps : HPS;
          fsState : ULONG;
          fsStateOld : ULONG;
       end;

       PUSERBUTTON = ^USERBUTTON;

    const
       BM_CLICK = $0120;
       BM_QUERYCHECKINDEX = $0121;
       BM_QUERYHILITE = $0122;
       BM_SETHILITE = $0123;
       BM_QUERYCHECK = $0124;
       BM_SETCHECK = $0125;
       BM_SETDEFAULT = $0126;
       BN_CLICKED = 1;
       BN_DBLCLICKED = 2;
       BN_PAINT = 3;
       BDS_HILITED = $0100;
       BDS_DISABLED = $0200;
       BDS_DEFAULT = $0400;
       ES_LEFT = $00000000;
       ES_CENTER = $00000001;
       ES_RIGHT = $00000002;
       ES_AUTOSCROLL = $00000004;
       ES_MARGIN = $00000008;
       ES_AUTOTAB = $00000010;
       ES_READONLY = $00000020;
       ES_COMMAND = $00000040;
       ES_UNREADABLE = $00000080;
       ES_AUTOSIZE = $00000200;
       ES_ANY = $00000000;
       ES_SBCS = $00001000;
       ES_DBCS = $00002000;
       ES_MIXED = $00003000;
       CBS_SIMPLE = $0001;
       CBS_DROPDOWN = $0002;
       CBS_DROPDOWNLIST = $0004;
       CBS_COMPATIBLE = $0008;
       CBID_LIST = $029A;
       CBID_EDIT = $029B;
       CBM_SHOWLIST = $0170;
       CBM_HILITE = $0171;
       CBM_ISLISTSHOWING = $0172;
       CBN_EFCHANGE = 1;
       CBN_EFSCROLL = 2;
       CBN_MEMERROR = 3;
       CBN_LBSELECT = 4;
       CBN_LBSCROLL = 5;
       CBN_SHOWLIST = 6;
       CBN_ENTER = 7;
{$PACKRECORDS 2}


    type
       ENTRYFDATA = record
          cb : USHORT;
          cchEditLimit : USHORT;
          ichMinSel : USHORT;
          ichMaxSel : USHORT;
       end;

       PENTRYFDATA = ^ENTRYFDATA;

{$PACKRECORDS NORMAL}
    const
       EM_QUERYCHANGED = $0140;
       EM_QUERYSEL = $0141;
       EM_SETSEL = $0142;
       EM_SETTEXTLIMIT = $0143;
       EM_CUT = $0144;
       EM_COPY = $0145;
       EM_CLEAR = $0146;
       EM_PASTE = $0147;
       EM_QUERYFIRSTCHAR = $0148;
       EM_SETFIRSTCHAR = $0149;
       EM_QUERYREADONLY = $014a;
       EM_SETREADONLY = $014b;
       EM_SETINSERTMODE = $014c;
       EN_SETFOCUS = $0001;
       EN_KILLFOCUS = $0002;
       EN_CHANGE = $0004;
       EN_SCROLL = $0008;
       EN_MEMERROR = $0010;
       EN_OVERFLOW = $0020;
       EN_INSERTMODETOGGLE = $0040;
       LS_MULTIPLESEL = $00000001;
       LS_OWNERDRAW = $00000002;
       LS_NOADJUSTPOS = $00000004;
       LS_HORZSCROLL = $00000008;
       LS_EXTENDEDSEL = $00000010;
       LN_SELECT = 1;
       LN_SETFOCUS = 2;
       LN_KILLFOCUS = 3;
       LN_SCROLL = 4;
       LN_ENTER = 5;
       LM_QUERYITEMCOUNT = $0160;
       LM_INSERTITEM = $0161;
       LM_SETTOPINDEX = $0162;
       LM_DELETEITEM = $0163;
       LM_SELECTITEM = $0164;
       LM_QUERYSELECTION = $0165;
       LM_SETITEMTEXT = $0166;
       LM_QUERYITEMTEXTLENGTH = $0167;
       LM_QUERYITEMTEXT = $0168;
       LM_SETITEMHANDLE = $0169;
       LM_QUERYITEMHANDLE = $016a;
       LM_SEARCHSTRING = $016b;
       LM_SETITEMHEIGHT = $016c;
       LM_QUERYTOPINDEX = $016d;
       LM_DELETEALL = $016e;
       LIT_CURSOR = (-4);
       LIT_ERROR = (-3);
       LIT_MEMERROR = (-2);
       LIT_NONE = (-1);
       LIT_FIRST = (-1);
       LIT_END = (-1);
       LIT_SORTASCENDING = (-2);
       LIT_SORTDESCENDING = (-3);
       LSS_SUBSTRING = $0001;
       LSS_PREFIX = $0002;
       LSS_CASESENSITIVE = $0004;
       MS_ACTIONBAR = $00000001;
       MS_TITLEBUTTON = $00000002;
       MS_VERTICALFLIP = $00000004;
       MS_CONDITIONALCASCADE = $00000040;

    function WinLoadMenu(hwndFrame : HWND;hmod : HMODULE;idMenu : ULONG) : HWND;

    const
       MM_INSERTITEM = $0180;
       MM_DELETEITEM = $0181;
       MM_QUERYITEM = $0182;
       MM_SETITEM = $0183;
       MM_QUERYITEMCOUNT = $0184;
       MM_STARTMENUMODE = $0185;
       MM_ENDMENUMODE = $0186;
       MM_REMOVEITEM = $0188;
       MM_SELECTITEM = $0189;
       MM_QUERYSELITEMID = $018a;
       MM_QUERYITEMTEXT = $018b;
       MM_QUERYITEMTEXTLENGTH = $018c;
       MM_SETITEMHANDLE = $018d;
       MM_SETITEMTEXT = $018e;
       MM_ITEMPOSITIONFROMID = $018f;
       MM_ITEMIDFROMPOSITION = $0190;
       MM_QUERYITEMATTR = $0191;
       MM_SETITEMATTR = $0192;
       MM_ISITEMVALID = $0193;
       MM_QUERYITEMRECT = $0194;
       MM_QUERYDEFAULTITEMID = $0431;
       MM_SETDEFAULTITEMID = $0432;

    function WinCreateMenu(hwndParent : HWND;lpmt : PVOID) : HWND;

    type
       OWNERITEM = record
          hwnd : HWND;
          hps : HPS;
          fsState : ULONG;
          fsAttribute : ULONG;
          fsStateOld : ULONG;
          fsAttributeOld : ULONG;
          rclItem : RECTL;
          idItem : LONG;
          hItem : ULONG;
       end;

       POWNERITEM = ^OWNERITEM;

{$PACKRECORDS 2}

       MENUITEM = record
          iPosition : SHORT;
          afStyle : USHORT;
          afAttribute : USHORT;
          id : USHORT;
          hwndSubMenu : HWND;
          hItem : ULONG;
       end;

       PMENUITEM = ^MENUITEM;

{$PACKRECORDS NORMAL}
    const
       MIT_END = (-1);
       MIT_NONE = (-1);
       MIT_MEMERROR = (-1);
       MIT_ERROR = (-1);
       MIT_FIRST = (-2);
       MIT_LAST = (-3);
       MID_NONE = MIT_NONE;
       MID_ERROR = (-1);
       MIS_TEXT = $0001;
       MIS_BITMAP = $0002;
       MIS_SEPARATOR = $0004;
       MIS_OWNERDRAW = $0008;
       MIS_SUBMENU = $0010;
       MIS_MULTMENU = $0020;
       MIS_SYSCOMMAND = $0040;
       MIS_HELP = $0080;
       MIS_STATIC = $0100;
       MIS_BUTTONSEPARATOR = $0200;
       MIS_BREAK = $0400;
       MIS_BREAKSEPARATOR = $0800;
       MIS_GROUP = $1000;
       MIS_SINGLE = $2000;
       MIA_NODISMISS = $0020;
       MIA_FRAMED = $1000;
       MIA_CHECKED = $2000;
       MIA_DISABLED = $4000;
       MIA_HILITED = $8000;

    function WinPopupMenu(hwndParent : HWND;hwndOwner : HWND;hwndMenu : HWND;x : LONG;y : LONG;idItem : LONG;fs : ULONG) : BOOL;

    const
       PU_POSITIONONITEM = $0001;
       PU_HCONSTRAIN = $0002;
       PU_VCONSTRAIN = $0004;
       PU_NONE = $0000;
       PU_MOUSEBUTTON1DOWN = $0008;
       PU_MOUSEBUTTON2DOWN = $0010;
       PU_MOUSEBUTTON3DOWN = $0018;
       PU_SELECTITEM = $0020;
       PU_MOUSEBUTTON1 = $0040;
       PU_MOUSEBUTTON2 = $0080;
       PU_MOUSEBUTTON3 = $0100;
       PU_KEYBOARD = $0200;
       SBS_HORZ = 0;
       SBS_VERT = 1;
       SBS_THUMBSIZE = 2;
       SBS_AUTOTRACK = 4;
       SBS_AUTOSIZE = $2000;
       SBM_SETSCROLLBAR = $01a0;
       SBM_SETPOS = $01a1;
       SBM_QUERYPOS = $01a2;
       SBM_QUERYRANGE = $01a3;
       SBM_SETTHUMBSIZE = $01a6;
       SB_LINEUP = 1;
       SB_LINEDOWN = 2;
       SB_LINELEFT = 1;
       SB_LINERIGHT = 2;
       SB_PAGEUP = 3;
       SB_PAGEDOWN = 4;
       SB_PAGELEFT = 3;
       SB_PAGERIGHT = 4;
       SB_SLIDERTRACK = 5;
       SB_SLIDERPOSITION = 6;
       SB_ENDSCROLL = 7;
{$PACKRECORDS 2}


    type
       SBCDATA = record
          cb : USHORT;
          sHilite : USHORT;
          posFirst : SHORT;
          posLast : SHORT;
          posThumb : SHORT;
          cVisible : SHORT;
          cTotal : SHORT;
       end;

       PSBCDATA = ^SBCDATA;

{$PACKRECORDS NORMAL}
{$PACKRECORDS 2}

       FRAMECDATA = record
          cb : USHORT;
          flCreateFlags : ULONG;
          hmodResources : USHORT;
          idResources : USHORT;
       end;

       PFRAMECDATA = ^FRAMECDATA;

{$PACKRECORDS NORMAL}
    const
       FCF_TITLEBAR = $00000001;
       FCF_SYSMENU = $00000002;
       FCF_MENU = $00000004;
       FCF_SIZEBORDER = $00000008;
       FCF_MINBUTTON = $00000010;
       FCF_MAXBUTTON = $00000020;
       FCF_MINMAX = $00000030;
       FCF_VERTSCROLL = $00000040;
       FCF_HORZSCROLL = $00000080;
       FCF_DLGBORDER = $00000100;
       FCF_BORDER = $00000200;
       FCF_SHELLPOSITION = $00000400;
       FCF_TASKLIST = $00000800;
       FCF_NOBYTEALIGN = $00001000;
       FCF_NOMOVEWITHOWNER = $00002000;
       FCF_ICON = $00004000;
       FCF_ACCELTABLE = $00008000;
       FCF_SYSMODAL = $00010000;
       FCF_SCREENALIGN = $00020000;
       FCF_MOUSEALIGN = $00040000;
       FCF_HIDEBUTTON = $01000000;
       FCF_HIDEMAX = $01000020;
       FCF_DBE_APPSTAT = $80000000;
       FCF_AUTOICON = $40000000;
       FCF_STANDARD = $0000CC3F;
       FS_ICON = $00000001;
       FS_ACCELTABLE = $00000002;
       FS_SHELLPOSITION = $00000004;
       FS_TASKLIST = $00000008;
       FS_NOBYTEALIGN = $00000010;
       FS_NOMOVEWITHOWNER = $00000020;
       FS_SYSMODAL = $00000040;
       FS_DLGBORDER = $00000080;
       FS_BORDER = $00000100;
       FS_SCREENALIGN = $00000200;
       FS_MOUSEALIGN = $00000400;
       FS_SIZEBORDER = $00000800;
       FS_AUTOICON = $00001000;
       FS_DBE_APPSTAT = $00008000;
       FS_STANDARD = $0000000F;
       FF_FLASHWINDOW = $0001;
       FF_ACTIVE = $0002;
       FF_FLASHHILITE = $0004;
       FF_OWNERHIDDEN = $0008;
       FF_DLGDISMISSED = $0010;
       FF_OWNERDISABLED = $0020;
       FF_SELECTED = $0040;
       FF_NOACTIVATESWP = $0080;

    function WinCreateStdWindow(hwndParent : HWND;flStyle : ULONG;pflCreateFlags : PULONG;pszClientClass : PSZ;pszTitle : PSZ;styleClient : ULONG;hmod : HMODULE;idResources : ULONG;phwndClient : PHWND) : HWND;

    function WinFlashWindow(hwndFrame : HWND;fFlash : BOOL) : BOOL;

    const
       WM_FLASHWINDOW = $0040;
       WM_FORMATFRAME = $0041;
       WM_UPDATEFRAME = $0042;
       WM_FOCUSCHANGE = $0043;
       WM_SETBORDERSIZE = $0044;
       WM_TRACKFRAME = $0045;
       WM_MINMAXFRAME = $0046;
       WM_SETICON = $0047;
       WM_QUERYICON = $0048;
       WM_SETACCELTABLE = $0049;
       WM_QUERYACCELTABLE = $004a;
       WM_TRANSLATEACCEL = $004b;
       WM_QUERYTRACKINFO = $004c;
       WM_QUERYBORDERSIZE = $004d;
       WM_NEXTMENU = $004e;
       WM_ERASEBACKGROUND = $004f;
       WM_QUERYFRAMEINFO = $0050;
       WM_QUERYFOCUSCHAIN = $0051;
       WM_OWNERPOSCHANGE = $0052;
       WM_CALCFRAMERECT = $0053;
       WM_WINDOWPOSCHANGED = $0055;
       WM_ADJUSTFRAMEPOS = $0056;
       WM_QUERYFRAMECTLCOUNT = $0059;
       WM_QUERYHELPINFO = $005B;
       WM_SETHELPINFO = $005C;
       WM_ERROR = $005D;
       WM_REALIZEPALETTE = $005E;
       FI_FRAME = $00000001;
       FI_OWNERHIDE = $00000002;
       FI_ACTIVATEOK = $00000004;
       FI_NOMOVEWITHOWNER = $00000008;

    function WinCreateFrameControls(hwndFrame : HWND;pfcdata : PFRAMECDATA;pszTitle : PSZ) : BOOL;

    function WinCalcFrameRect(hwndFrame : HWND;prcl : PRECTL;fClient : BOOL) : BOOL;

    function WinGetMinPosition(hwnd : HWND;pswp : PSWP;pptl : PPOINTL) : BOOL;

    function WinGetMaxPosition(hwnd : HWND;pswp : PSWP) : BOOL;

    type
       HSAVEWP = LHANDLE;


    function WinSaveWindowPos(hsvwp : HSAVEWP;pswp : PSWP;cswp : ULONG) : BOOL;

    const
       FID_SYSMENU = $8002;
       FID_TITLEBAR = $8003;
       FID_MINMAX = $8004;
       FID_MENU = $8005;
       FID_VERTSCROLL = $8006;
       FID_HORZSCROLL = $8007;
       FID_CLIENT = $8008;
       FID_DBE_APPSTAT = $8010;
       FID_DBE_KBDSTAT = $8011;
       FID_DBE_PECIC = $8012;
       FID_DBE_KKPOPUP = $8013;
       SC_SIZE = $8000;
       SC_MOVE = $8001;
       SC_MINIMIZE = $8002;
       SC_MAXIMIZE = $8003;
       SC_CLOSE = $8004;
       SC_NEXT = $8005;
       SC_APPMENU = $8006;
       SC_SYSMENU = $8007;
       SC_RESTORE = $8008;
       SC_NEXTFRAME = $8009;
       SC_NEXTWINDOW = $8010;
       SC_TASKMANAGER = $8011;
       SC_HELPKEYS = $8012;
       SC_HELPINDEX = $8013;
       SC_HELPEXTENDED = $8014;
       SC_SWITCHPANELIDS = $8015;
       SC_DBE_FIRST = $8018;
       SC_DBE_LAST = $801F;
       SC_BEGINDRAG = $8020;
       SC_ENDDRAG = $8021;
       SC_SELECT = $8022;
       SC_OPEN = $8023;
       SC_CONTEXTMENU = $8024;
       SC_CONTEXTHELP = $8025;
       SC_TEXTEDIT = $8026;
       SC_BEGINSELECT = $8027;
       SC_ENDSELECT = $8028;
       SC_WINDOW = $8029;
       SC_HIDE = $802a;
       TBM_SETHILITE = $01e3;
       TBM_QUERYHILITE = $01e4;

    function WinCopyRect(hab : HAB;prclDst : PRECTL;prclSrc : PRECTL) : BOOL;

    function WinSetRect(hab : HAB;prcl : PRECTL;xLeft : LONG;yBottom : LONG;xRight : LONG;yTop : LONG) : BOOL;

    function WinIsRectEmpty(hab : HAB;prcl : PRECTL) : BOOL;

    function WinEqualRect(hab : HAB;prcl1 : PRECTL;prcl2 : PRECTL) : BOOL;

    function WinSetRectEmpty(hab : HAB;prcl : PRECTL) : BOOL;

    function WinOffsetRect(hab : HAB;prcl : PRECTL;cx : LONG;cy : LONG) : BOOL;

    function WinInflateRect(hab : HAB;prcl : PRECTL;cx : LONG;cy : LONG) : BOOL;

    function WinPtInRect(hab : HAB;prcl : PRECTL;pptl : PPOINTL) : BOOL;

    function WinIntersectRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;

    function WinUnionRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;

    function WinSubtractRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;

    function WinMakeRect(hab : HAB;pwrc : PRECTL) : BOOL;

    function WinMakePoints(hab : HAB;pwpt : PPOINTL;cwpt : ULONG) : BOOL;

    function WinQuerySysValue(hwndDesktop : HWND;iSysValue : LONG) : LONG;

    function WinSetSysValue(hwndDesktop : HWND;iSysValue : LONG;lValue : LONG) : BOOL;

    const
       SV_SWAPBUTTON = 0;
       SV_DBLCLKTIME = 1;
       SV_CXDBLCLK = 2;
       SV_CYDBLCLK = 3;
       SV_CXSIZEBORDER = 4;
       SV_CYSIZEBORDER = 5;
       SV_ALARM = 6;
       SV_RESERVEDFIRST1 = 7;
       SV_RESERVEDLAST1 = 8;
       SV_CURSORRATE = 9;
       SV_FIRSTSCROLLRATE = 10;
       SV_SCROLLRATE = 11;
       SV_NUMBEREDLISTS = 12;
       SV_WARNINGFREQ = 13;
       SV_NOTEFREQ = 14;
       SV_ERRORFREQ = 15;
       SV_WARNINGDURATION = 16;
       SV_NOTEDURATION = 17;
       SV_ERRORDURATION = 18;
       SV_RESERVEDFIRST = 19;
       SV_RESERVEDLAST = 19;
       SV_CXSCREEN = 20;
       SV_CYSCREEN = 21;
       SV_CXVSCROLL = 22;
       SV_CYHSCROLL = 23;
       SV_CYVSCROLLARROW = 24;
       SV_CXHSCROLLARROW = 25;
       SV_CXBORDER = 26;
       SV_CYBORDER = 27;
       SV_CXDLGFRAME = 28;
       SV_CYDLGFRAME = 29;
       SV_CYTITLEBAR = 30;
       SV_CYVSLIDER = 31;
       SV_CXHSLIDER = 32;
       SV_CXMINMAXBUTTON = 33;
       SV_CYMINMAXBUTTON = 34;
       SV_CYMENU = 35;
       SV_CXFULLSCREEN = 36;
       SV_CYFULLSCREEN = 37;
       SV_CXICON = 38;
       SV_CYICON = 39;
       SV_CXPOINTER = 40;
       SV_CYPOINTER = 41;
       SV_DEBUG = 42;
       SV_CMOUSEBUTTONS = 43;
       SV_CPOINTERBUTTONS = 43;
       SV_POINTERLEVEL = 44;
       SV_CURSORLEVEL = 45;
       SV_TRACKRECTLEVEL = 46;
       SV_CTIMERS = 47;
       SV_MOUSEPRESENT = 48;
       SV_CXBYTEALIGN = 49;
       SV_CXALIGN = 49;
       SV_CYBYTEALIGN = 50;
       SV_CYALIGN = 50;
       SV_NOTRESERVED = 56;
       SV_EXTRAKEYBEEP = 57;
       SV_SETLIGHTS = 58;
       SV_INSERTMODE = 59;
       SV_MENUROLLDOWNDELAY = 64;
       SV_MENUROLLUPDELAY = 65;
       SV_ALTMNEMONIC = 66;
       SV_TASKLISTMOUSEACCESS = 67;
       SV_CXICONTEXTWIDTH = 68;
       SV_CICONTEXTLINES = 69;
       SV_CHORDTIME = 70;
       SV_CXCHORD = 71;
       SV_CYCHORD = 72;
       SV_CXMOTION = 73;
       SV_CYMOTION = 74;
       SV_BEGINDRAG = 75;
       SV_ENDDRAG = 76;
       SV_SINGLESELECT = 77;
       SV_OPEN = 78;
       SV_CONTEXTMENU = 79;
       SV_CONTEXTHELP = 80;
       SV_TEXTEDIT = 81;
       SV_BEGINSELECT = 82;
       SV_ENDSELECT = 83;
       SV_BEGINDRAGKB = 84;
       SV_ENDDRAGKB = 85;
       SV_SELECTKB = 86;
       SV_OPENKB = 87;
       SV_CONTEXTMENUKB = 88;
       SV_CONTEXTHELPKB = 89;
       SV_TEXTEDITKB = 90;
       SV_BEGINSELECTKB = 91;
       SV_ENDSELECTKB = 92;
       SV_ANIMATION = 93;
       SV_ANIMATIONSPEED = 94;
       SV_MONOICONS = 95;
       SV_KBDALTERED = 96;
       SV_PRINTSCREEN = 97;
       SV_CSYSVALUES = 98;

    type
       PARAM = record
          id : ULONG;
          cb : ULONG;
          ab : array[0..1-1] of BYTE;
       end;

       NPPARAM = ^PARAM;

       PPARAM = ^PARAM;

       PRESPARAMS = record
          cb : ULONG;
          aparam : array[0..1-1] of PARAM;
       end;

       NPPRESPARAMS = ^PRESPARAMS;

       PPRESPARAMS = ^PRESPARAMS;


    function WinSetPresParam(hwnd : HWND;id : ULONG;cbParam : ULONG;pbParam : PVOID) : BOOL;

    function WinQueryPresParam(hwnd : HWND;id1 : ULONG;id2 : ULONG;pulId : PULONG;cbBuf : ULONG;pbBuf : PVOID;fs : ULONG) : ULONG;

    function WinRemovePresParam(hwnd : HWND;id : ULONG) : BOOL;

    const
       PP_FOREGROUNDCOLOR = 1;
       PP_FOREGROUNDCOLORINDEX = 2;
       PP_BACKGROUNDCOLOR = 3;
       PP_BACKGROUNDCOLORINDEX = 4;
       PP_HILITEFOREGROUNDCOLOR = 5;
       PP_HILITEFOREGROUNDCOLORINDEX = 6;
       PP_HILITEBACKGROUNDCOLOR = 7;
       PP_HILITEBACKGROUNDCOLORINDEX = 8;
       PP_DISABLEDFOREGROUNDCOLOR = 9;
       PP_DISABLEDFOREGROUNDCOLORINDEX = 10;
       PP_DISABLEDBACKGROUNDCOLOR = 11;
       PP_DISABLEDBACKGROUNDCOLORINDEX = 12;
       PP_BORDERCOLOR = 13;
       PP_BORDERCOLORINDEX = 14;
       PP_FONTNAMESIZE = 15;
       PP_FONTHANDLE = 16;
       PP_RESERVED = 17;
       PP_ACTIVECOLOR = 18;
       PP_ACTIVECOLORINDEX = 19;
       PP_INACTIVECOLOR = 20;
       PP_INACTIVECOLORINDEX = 21;
       PP_ACTIVETEXTFGNDCOLOR = 22;
       PP_ACTIVETEXTFGNDCOLORINDEX = 23;
       PP_ACTIVETEXTBGNDCOLOR = 24;
       PP_ACTIVETEXTBGNDCOLORINDEX = 25;
       PP_INACTIVETEXTFGNDCOLOR = 26;
       PP_INACTIVETEXTFGNDCOLORINDEX = 27;
       PP_INACTIVETEXTBGNDCOLOR = 28;
       PP_INACTIVETEXTBGNDCOLORINDEX = 29;
       PP_SHADOW = 30;
       PP_MENUFOREGROUNDCOLOR = 31;
       PP_MENUFOREGROUNDCOLORINDEX = 32;
       PP_MENUBACKGROUNDCOLOR = 33;
       PP_MENUBACKGROUNDCOLORINDEX = 34;
       PP_MENUHILITEFGNDCOLOR = 35;
       PP_MENUHILITEFGNDCOLORINDEX = 36;
       PP_MENUHILITEBGNDCOLOR = 37;
       PP_MENUHILITEBGNDCOLORINDEX = 38;
       PP_MENUDISABLEDFGNDCOLOR = 39;
       PP_MENUDISABLEDFGNDCOLORINDEX = 40;
       PP_MENUDISABLEDBGNDCOLOR = 41;
       PP_MENUDISABLEDBGNDCOLORINDEX = 42;
       PP_USER = $8000;
       QPF_NOINHERIT = $0001;
       QPF_ID1COLORINDEX = $0002;
       QPF_ID2COLORINDEX = $0004;
       QPF_PURERGBCOLOR = $0008;
       QPF_VALIDFLAGS = $000F;

    function WinQuerySysColor(hwndDesktop : HWND;clr : LONG;lReserved : LONG) : LONG;

    function WinSetSysColors(hwndDesktop : HWND;flOptions : ULONG;flFormat : ULONG;clrFirst : LONG;cclr : ULONG;pclr : PLONG) : BOOL;

    const
       SYSCLR_SHADOWHILITEBGND = (-50);
       SYSCLR_SHADOWHILITEFGND = (-49);
       SYSCLR_SHADOWTEXT = (-48);
       SYSCLR_ENTRYFIELD = (-47);
       SYSCLR_MENUDISABLEDTEXT = (-46);
       SYSCLR_MENUHILITE = (-45);
       SYSCLR_MENUHILITEBGND = (-44);
       SYSCLR_PAGEBACKGROUND = (-43);
       SYSCLR_FIELDBACKGROUND = (-42);
       SYSCLR_BUTTONLIGHT = (-41);
       SYSCLR_BUTTONMIDDLE = (-40);
       SYSCLR_BUTTONDARK = (-39);
       SYSCLR_BUTTONDEFAULT = (-38);
       SYSCLR_TITLEBOTTOM = (-37);
       SYSCLR_SHADOW = (-36);
       SYSCLR_ICONTEXT = (-35);
       SYSCLR_DIALOGBACKGROUND = (-34);
       SYSCLR_HILITEFOREGROUND = (-33);
       SYSCLR_HILITEBACKGROUND = (-32);
       SYSCLR_INACTIVETITLETEXTBGND = (-31);
       SYSCLR_ACTIVETITLETEXTBGND = (-30);
       SYSCLR_INACTIVETITLETEXT = (-29);
       SYSCLR_ACTIVETITLETEXT = (-28);
       SYSCLR_OUTPUTTEXT = (-27);
       SYSCLR_WINDOWSTATICTEXT = (-26);
       SYSCLR_SCROLLBAR = (-25);
       SYSCLR_BACKGROUND = (-24);
       SYSCLR_ACTIVETITLE = (-23);
       SYSCLR_INACTIVETITLE = (-22);
       SYSCLR_MENU = (-21);
       SYSCLR_WINDOW = (-20);
       SYSCLR_WINDOWFRAME = (-19);
       SYSCLR_MENUTEXT = (-18);
       SYSCLR_WINDOWTEXT = (-17);
       SYSCLR_TITLETEXT = (-16);
       SYSCLR_ACTIVEBORDER = (-15);
       SYSCLR_INACTIVEBORDER = (-14);
       SYSCLR_APPWORKSPACE = (-13);
       SYSCLR_HELPBACKGROUND = (-12);
       SYSCLR_HELPTEXT = (-11);
       SYSCLR_HELPHILITE = (-10);
       SYSCLR_CSYSCOLORS = 41;

    function WinStartTimer(hab : HAB;hwnd : HWND;idTimer : ULONG;dtTimeout : ULONG) : ULONG;

    function WinStopTimer(hab : HAB;hwnd : HWND;idTimer : ULONG) : BOOL;

    function WinGetCurrentTime(hab : HAB) : ULONG;

    const
       TID_CURSOR = $ffff;
       TID_SCROLL = $fffe;
       TID_FLASHWINDOW = $fffd;
       TID_USERMAX = $7fff;

    type
       HACCEL = LHANDLE;

    const
       AF_CHAR = $0001;
       AF_VIRTUALKEY = $0002;
       AF_SCANCODE = $0004;
       AF_SHIFT = $0008;
       AF_CONTROL = $0010;
       AF_ALT = $0020;
       AF_LONEKEY = $0040;
       AF_SYSCOMMAND = $0100;
       AF_HELP = $0200;
{$PACKRECORDS 2}


    type
       ACCEL = record
          fs : USHORT;
          key : USHORT;
          cmd : USHORT;
       end;

       PACCEL = ^ACCEL;

       ACCELTABLE = record
          cAccel : USHORT;
          codepage : USHORT;
          aaccel : array[0..1-1] of ACCEL;
       end;

       PACCELTABLE = ^ACCELTABLE;

{$PACKRECORDS NORMAL}

    function WinLoadAccelTable(hab : HAB;hmod : HMODULE;idAccelTable : ULONG) : HACCEL;

    function WinCopyAccelTable(haccel : HACCEL;pAccelTable : PACCELTABLE;cbCopyMax : ULONG) : ULONG;

    function WinCreateAccelTable(hab : HAB;pAccelTable : PACCELTABLE) : HACCEL;

    function WinDestroyAccelTable(haccel : HACCEL) : BOOL;

    function WinTranslateAccel(hab : HAB;hwnd : HWND;haccel : HACCEL;pqmsg : PQMSG) : BOOL;

    function WinSetAccelTable(hab : HAB;haccel : HACCEL;hwndFrame : HWND) : BOOL;

    function WinQueryAccelTable(hab : HAB;hwndFrame : HWND) : HACCEL;

    const
       EAF_DEFAULTOWNER = $0001;
       EAF_UNCHANGEABLE = $0002;
       EAF_REUSEICON = $0004;

    type
       TRACKINFO = record
          cxBorder : LONG;
          cyBorder : LONG;
          cxGrid : LONG;
          cyGrid : LONG;
          cxKeyboard : LONG;
          cyKeyboard : LONG;
          rclTrack : RECTL;
          rclBoundary : RECTL;
          ptlMinTrackSize : POINTL;
          ptlMaxTrackSize : POINTL;
          fs : ULONG;
       end;

       PTRACKINFO = ^TRACKINFO;


    function WinTrackRect(hwnd : HWND;hps : HPS;pti : PTRACKINFO) : BOOL;

    function WinShowTrackRect(hwnd : HWND;fShow : BOOL) : BOOL;

    const
       TF_LEFT = $0001;
       TF_TOP = $0002;
       TF_RIGHT = $0004;
       TF_BOTTOM = $0008;
       TF_MOVE = $000F;
       TF_SETPOINTERPOS = $0010;
       TF_GRID = $0020;
       TF_STANDARD = $0040;
       TF_ALLINBOUNDARY = $0080;
       TF_VALIDATETRACKRECT = $0100;
       TF_PARTINBOUNDARY = $0200;
       WM_RENDERFMT = $0060;
       WM_RENDERALLFMTS = $0061;
       WM_DESTROYCLIPBOARD = $0062;
       WM_PAINTCLIPBOARD = $0063;
       WM_SIZECLIPBOARD = $0064;
       WM_HSCROLLCLIPBOARD = $0065;
       WM_VSCROLLCLIPBOARD = $0066;
       WM_DRAWCLIPBOARD = $0067;
       CF_TEXT = 1;
       CF_BITMAP = 2;
       CF_DSPTEXT = 3;
       CF_DSPBITMAP = 4;
       CF_METAFILE = 5;
       CF_DSPMETAFILE = 6;
       CF_PALETTE = 9;
       SZFMT_TEXT = '#1';
       SZFMT_BITMAP = '#2';
       SZFMT_DSPTEXT = '#3';
       SZFMT_DSPBITMAP = '#4';
       SZFMT_METAFILE = '#5';
       SZFMT_DSPMETAFILE = '#6';
       SZFMT_PALETTE = '#9';
       SZFMT_SYLK = 'Sylk';
       SZFMT_DIF = 'Dif';
       SZFMT_TIFF = 'Tiff';
       SZFMT_OEMTEXT = 'OemText';
       SZFMT_DIB = 'Dib';
       SZFMT_OWNERDISPLAY = 'OwnerDisplay';
       SZFMT_LINK = 'Link';
       SZFMT_METAFILEPICT = 'MetaFilePict';
       SZFMT_DSPMETAFILEPICT = 'DspMetaFilePict';
       SZFMT_CPTEXT = 'Codepage Text';
       SZDDEFMT_RTF = 'Rich Text Format';
       SZDDEFMT_PTRPICT = 'Printer_Picture';
{$PACKRECORDS 2}


    type
       MFP = record
          sizeBounds : POINTL;
          sizeMM : POINTL;
          cbLength : ULONG;
          mapMode : USHORT;
          reserved : USHORT;
          abData : array[0..1-1] of BYTE;
       end;

       PMFP = ^MFP;

       CPTEXT = record
          idCountry : USHORT;
          usCodepage : USHORT;
          usLangID : USHORT;
          usSubLangID : USHORT;
          abText : array[0..1-1] of BYTE;
       end;

       PCPTEXT = ^CPTEXT;

{$PACKRECORDS NORMAL}

    function WinSetClipbrdOwner(hab : HAB;hwnd : HWND) : BOOL;

    function WinSetClipbrdData(hab : HAB;ulData : ULONG;fmt : ULONG;rgfFmtInfo : ULONG) : BOOL;

    function WinQueryClipbrdData(hab : HAB;fmt : ULONG) : ULONG;

    function WinQueryClipbrdFmtInfo(hab : HAB;fmt : ULONG;prgfFmtInfo : PULONG) : BOOL;

    function WinSetClipbrdViewer(hab : HAB;hwndNewClipViewer : HWND) : BOOL;

    const
       CFI_OWNERFREE = $0001;
       CFI_OWNERDISPLAY = $0002;
       CFI_POINTER = $0400;
       CFI_HANDLE = $0200;

    function WinEnumClipbrdFmts(hab : HAB;fmt : ULONG) : ULONG;

    function WinEmptyClipbrd(hab : HAB) : BOOL;

    function WinOpenClipbrd(hab : HAB) : BOOL;

    function WinCloseClipbrd(hab : HAB) : BOOL;

    function WinQueryClipbrdOwner(hab : HAB) : HWND;

    function WinQueryClipbrdViewer(hab : HAB) : HWND;

    function WinDestroyCursor(hwnd : HWND) : BOOL;

    function WinShowCursor(hwnd : HWND;fShow : BOOL) : BOOL;

    function WinCreateCursor(hwnd : HWND;x : LONG;y : LONG;cx : LONG;cy : LONG;fs : ULONG;prclClip : PRECTL) : BOOL;

    const
       CURSOR_SOLID = $0000;
       CURSOR_HALFTONE = $0001;
       CURSOR_FRAME = $0002;
       CURSOR_FLASH = $0004;
       CURSOR_SETPOS = $8000;

    type
       CURSORINFO = record
          hwnd : HWND;
          x : LONG;
          y : LONG;
          cx : LONG;
          cy : LONG;
          fs : ULONG;
          rclClip : RECTL;
       end;

       PCURSORINFO = ^CURSORINFO;


    function WinQueryCursorInfo(hwndDesktop : HWND;pCursorInfo : PCURSORINFO) : BOOL;

    type
       HPOINTER = LHANDLE;


    function WinSetPointer(hwndDesktop : HWND;hptrNew : HPOINTER) : BOOL;

    function WinSetPointerOwner(hptr : HPOINTER;pid : PID;fDestroy : BOOL) : BOOL;

    function WinShowPointer(hwndDesktop : HWND;fShow : BOOL) : BOOL;

    function WinQuerySysPointer(hwndDesktop : HWND;iptr : LONG;fLoad : BOOL) : HPOINTER;

    const
       SPTR_ARROW = 1;
       SPTR_TEXT = 2;
       SPTR_WAIT = 3;
       SPTR_SIZE = 4;
       SPTR_MOVE = 5;
       SPTR_SIZENWSE = 6;
       SPTR_SIZENESW = 7;
       SPTR_SIZEWE = 8;
       SPTR_SIZENS = 9;
       SPTR_APPICON = 10;
       SPTR_ICONINFORMATION = 11;
       SPTR_ICONQUESTION = 12;
       SPTR_ICONERROR = 13;
       SPTR_ICONWARNING = 14;
       SPTR_CPTR = 14;
       SPTR_ILLEGAL = 18;
       SPTR_FILE = 19;
       SPTR_FOLDER = 20;
       SPTR_MULTFILE = 21;
       SPTR_PROGRAM = 22;
       SPTR_HANDICON = SPTR_ICONERROR;
       SPTR_QUESICON = SPTR_ICONQUESTION;
       SPTR_BANGICON = SPTR_ICONWARNING;
       SPTR_NOTEICON = SPTR_ICONINFORMATION;

    function WinLoadPointer(hwndDesktop : HWND;hmod : HMODULE;idres : ULONG) : HPOINTER;

    function WinCreatePointer(hwndDesktop : HWND;hbmPointer : HBITMAP;fPointer : BOOL;xHotspot : LONG;yHotspot : LONG) : HPOINTER;

    function WinSetPointerPos(hwndDesktop : HWND;x : LONG;y : LONG) : BOOL;

    function WinDestroyPointer(hptr : HPOINTER) : BOOL;

    function WinQueryPointer(hwndDesktop : HWND) : HPOINTER;

    function WinQueryPointerPos(hwndDesktop : HWND;pptl : PPOINTL) : BOOL;

    type
       POINTERINFO = record
          fPointer : ULONG;
          xHotspot : LONG;
          yHotspot : LONG;
          hbmPointer : HBITMAP;
          hbmColor : HBITMAP;
          hbmMiniPointer : HBITMAP;
          hbmMiniColor : HBITMAP;
       end;

       PPOINTERINFO = ^POINTERINFO;


    function WinCreatePointerIndirect(hwndDesktop : HWND;pptri : PPOINTERINFO) : HPOINTER;

    function WinQueryPointerInfo(hptr : HPOINTER;pPointerInfo : PPOINTERINFO) : BOOL;

    function WinDrawPointer(hps : HPS;x : LONG;y : LONG;hptr : HPOINTER;fs : ULONG) : BOOL;

    const
       DP_NORMAL = $0000;
       DP_HALFTONED = $0001;
       DP_INVERTED = $0002;

    function WinGetSysBitmap(hwndDesktop : HWND;ibm : ULONG) : HBITMAP;

    const
       SBMP_OLD_SYSMENU = 1;
       SBMP_OLD_SBUPARROW = 2;
       SBMP_OLD_SBDNARROW = 3;
       SBMP_OLD_SBRGARROW = 4;
       SBMP_OLD_SBLFARROW = 5;
       SBMP_MENUCHECK = 6;
       SBMP_OLD_CHECKBOXES = 7;
       SBMP_BTNCORNERS = 8;
       SBMP_OLD_MINBUTTON = 9;
       SBMP_OLD_MAXBUTTON = 10;
       SBMP_OLD_RESTOREBUTTON = 11;
       SBMP_OLD_CHILDSYSMENU = 12;
       SBMP_DRIVE = 15;
       SBMP_FILE = 16;
       SBMP_FOLDER = 17;
       SBMP_TREEPLUS = 18;
       SBMP_TREEMINUS = 19;
       SBMP_PROGRAM = 22;
       SBMP_MENUATTACHED = 23;
       SBMP_SIZEBOX = 24;
       SBMP_SYSMENU = 25;
       SBMP_MINBUTTON = 26;
       SBMP_MAXBUTTON = 27;
       SBMP_RESTOREBUTTON = 28;
       SBMP_CHILDSYSMENU = 29;
       SBMP_SYSMENUDEP = 30;
       SBMP_MINBUTTONDEP = 31;
       SBMP_MAXBUTTONDEP = 32;
       SBMP_RESTOREBUTTONDEP = 33;
       SBMP_CHILDSYSMENUDEP = 34;
       SBMP_SBUPARROW = 35;
       SBMP_SBDNARROW = 36;
       SBMP_SBLFARROW = 37;
       SBMP_SBRGARROW = 38;
       SBMP_SBUPARROWDEP = 39;
       SBMP_SBDNARROWDEP = 40;
       SBMP_SBLFARROWDEP = 41;
       SBMP_SBRGARROWDEP = 42;
       SBMP_SBUPARROWDIS = 43;
       SBMP_SBDNARROWDIS = 44;
       SBMP_SBLFARROWDIS = 45;
       SBMP_SBRGARROWDIS = 46;
       SBMP_COMBODOWN = 47;
       SBMP_CHECKBOXES = 48;

    function WinSetHook(hab : HAB;hmq : HMQ;iHook : LONG;pfnHook : PFN;hmod : HMODULE) : BOOL;

    function WinReleaseHook(hab : HAB;hmq : HMQ;iHook : LONG;pfnHook : PFN;hmod : HMODULE) : BOOL;

    function WinCallMsgFilter(hab : HAB;pqmsg : PQMSG;msgf : ULONG) : BOOL;

    const
       HK_SENDMSG = 0;
       HK_INPUT = 1;
       HK_MSGFILTER = 2;
       HK_JOURNALRECORD = 3;
       HK_JOURNALPLAYBACK = 4;
       HK_HELP = 5;
       HK_LOADER = 6;
       HK_REGISTERUSERMSG = 7;
       HK_MSGCONTROL = 8;
       HK_PLIST_ENTRY = 9;
       HK_PLIST_EXIT = 10;
       HK_FINDWORD = 11;
       HK_CODEPAGECHANGED = 12;
       HK_WINDOWDC = 15;
       HK_DESTROYWINDOW = 16;
       HK_CHECKMSGFILTER = 20;
       HMQ_CURRENT = 1;
       MSGF_DIALOGBOX = 1;
       MSGF_MESSAGEBOX = 2;
       MSGF_TRACK = 8;
       MSGF_DDEPOSTMSG = 3;
       HLPM_FRAME = (-1);
       HLPM_WINDOW = (-2);
       HLPM_MENU = (-3);
       PM_MODEL_1X = 0;
       PM_MODEL_2X = 1;

    type
       SMHSTRUCT = record
          mp2 : MPARAM;
          mp1 : MPARAM;
          msg : ULONG;
          hwnd : HWND;
          model : ULONG;
       end;

       PSMHSTRUCT = ^SMHSTRUCT;

    const
       LHK_DELETEPROC = 1;
       LHK_DELETELIB = 2;
       LHK_LOADPROC = 3;
       LHK_LOADLIB = 4;
       MCHK_MSGINTEREST = 1;
       MCHK_CLASSMSGINTEREST = 2;
       MCHK_SYNCHRONISATION = 3;
       MCHK_MSGMODE = 4;
       RUMHK_DATATYPE = 1;
       RUMHK_MSG = 2;

    function WinSetClassThunkProc(pszClassname : PSZ;pfnThunkProc : PFN) : BOOL;

    function WinQueryClassThunkProc(pszClassname : PSZ) : PFN;

    function WinSetWindowThunkProc(hwnd : HWND;pfnThunkProc : PFN) : BOOL;

    function WinQueryWindowThunkProc(hwnd : HWND) : PFN;

    function WinQueryWindowModel(hwnd : HWND) : LONG;

    function WinQueryCp(hmq : HMQ) : ULONG;

    function WinSetCp(hmq : HMQ;idCodePage : ULONG) : BOOL;

    function WinQueryCpList(hab : HAB;ccpMax : ULONG;prgcp : PULONG) : ULONG;

    function WinCpTranslateString(hab : HAB;cpSrc : ULONG;pszSrc : PSZ;cpDst : ULONG;cchDestMax : ULONG;pchDest : PSZ) : BOOL;

    function WinCpTranslateChar(hab : HAB;cpSrc : ULONG;chSrc : UCHAR;cpDst : ULONG) : UCHAR;

    function WinUpper(hab : HAB;idcp : ULONG;idcc : ULONG;psz : PSZ) : ULONG;

    function WinUpperChar(hab : HAB;idcp : ULONG;idcc : ULONG;c : ULONG) : ULONG;

    function WinNextChar(hab : HAB;idcp : ULONG;idcc : ULONG;psz : PSZ) : PSZ;

    function WinPrevChar(hab : HAB;idcp : ULONG;idcc : ULONG;pszStart : PSZ;psz : PSZ) : PSZ;

    function WinCompareStrings(hab : HAB;idcp : ULONG;idcc : ULONG;psz1 : PSZ;psz2 : PSZ;reserved : ULONG) : ULONG;

    const
       WCS_ERROR = 0;
       WCS_EQ = 1;
       WCS_LT = 2;
       WCS_GT = 3;

    type
       HATOMTBL = LHANDLE;

       ATOM = ULONG;

    function WinCreateAtomTable(cbInitial : ULONG;cBuckets : ULONG) : HATOMTBL;

    function WinDestroyAtomTable(hAtomTbl : HATOMTBL) : HATOMTBL;

    function WinAddAtom(hAtomTbl : HATOMTBL;pszAtomName : PSZ) : ATOM;

    function WinFindAtom(hAtomTbl : HATOMTBL;pszAtomName : PSZ) : ATOM;

    function WinDeleteAtom(hAtomTbl : HATOMTBL;atom : ATOM) : ATOM;

    function WinQueryAtomUsage(hAtomTbl : HATOMTBL;atom : ATOM) : ULONG;

    function WinQueryAtomLength(hAtomTbl : HATOMTBL;atom : ATOM) : ULONG;

    function WinQueryAtomName(hAtomTbl : HATOMTBL;atom : ATOM;pchBuffer : PSZ;cchBufferMax : ULONG) : ULONG;

    const
       WINDBG_HWND_NOT_DESTROYED = $1022;
       WINDBG_HPTR_NOT_DESTROYED = $1023;
       WINDBG_HACCEL_NOT_DESTROYED = $1024;
       WINDBG_HENUM_NOT_DESTROYED = $1025;
       WINDBG_VISRGN_SEM_BUSY = $1026;
       WINDBG_USER_SEM_BUSY = $1027;
       WINDBG_DC_CACHE_BUSY = $1028;
       WINDBG_HOOK_STILL_INSTALLED = $1029;
       WINDBG_WINDOW_STILL_LOCKED = $102a;
       WINDBG_UPDATEPS_ASSERTION_FAIL = $102b;
       WINDBG_SENDMSG_WITHIN_USER_SEM = $102c;
       WINDBG_USER_SEM_NOT_ENTERED = $102d;
       WINDBG_PROC_NOT_EXPORTED = $102e;
       WINDBG_BAD_SENDMSG_HWND = $102f;
       WINDBG_ABNORMAL_EXIT = $1030;
       WINDBG_INTERNAL_REVISION = $1031;
       WINDBG_INITSYSTEM_FAILED = $1032;
       WINDBG_HATOMTBL_NOT_DESTROYED = $1033;
       WINDBG_WINDOW_UNLOCK_WAIT = $1035;

    type
       ERRINFO = record
          cbFixedErrInfo : ULONG;
          idError : ERRORID;
          cDetailLevel : ULONG;
          offaoffszMsg : ULONG;
          offBinaryData : ULONG;
       end;

       PERRINFO = ^ERRINFO;

    function WinGetLastError(hab : HAB) : ERRORID;

    function WinGetErrorInfo(hab : HAB) : PERRINFO;

    function WinFreeErrorInfo(perrinfo : PERRINFO) : BOOL;

    const
       SZDDESYS_TOPIC = 'System';
       SZDDESYS_ITEM_TOPICS = 'Topics';
       SZDDESYS_ITEM_SYSITEMS = 'SysItems';
       SZDDESYS_ITEM_RTNMSG = 'ReturnMessage';
       SZDDESYS_ITEM_STATUS = 'Status';
       SZDDESYS_ITEM_FORMATS = 'Formats';
       SZDDESYS_ITEM_SECURITY = 'Security';
       SZDDESYS_ITEM_ITEMFORMATS = 'ItemFormats';
       SZDDESYS_ITEM_HELP = 'Help';
       SZDDESYS_ITEM_PROTOCOLS = 'Protocols';
       SZDDESYS_ITEM_RESTART = 'Restart';

    type
       CONVCONTEXT = record
          cb : ULONG;
          fsContext : ULONG;
          idCountry : ULONG;
          usCodepage : ULONG;
          usLangID : ULONG;
          usSubLangID : ULONG;
       end;

       PCONVCONTEXT = ^CONVCONTEXT;

    const
       DDECTXT_CASESENSITIVE = $0001;

    type
       DDEINIT = record
          cb : ULONG;
          pszAppName : PSZ;
          pszTopic : PSZ;
          offConvContext : ULONG;
       end;

       PDDEINIT = ^DDEINIT;

       DDESTRUCT = record
          cbData : ULONG;
          fsStatus : USHORT;
          usFormat : USHORT;
          offszItemName : USHORT;
          offabData : USHORT;
       end;

       PDDESTRUCT = ^DDESTRUCT;

    const
       DDE_FACK = $0001;
       DDE_FBUSY = $0002;
       DDE_FNODATA = $0004;
       DDE_FACKREQ = $0008;
       DDE_FRESPONSE = $0010;
       DDE_NOTPROCESSED = $0020;
       DDE_FRESERVED = $00C0;
       DDE_FAPPSTATUS = $FF00;
       DDEFMT_TEXT = $0001;

    function WinDdeInitiate(hwndClient : HWND;pszAppName : PSZ;pszTopicName : PSZ;pcctxt : PCONVCONTEXT) : BOOL;

    function WinDdeRespond(hwndClient : HWND;hwndServer : HWND;pszAppName : PSZ;pszTopicName : PSZ;pcctxt : PCONVCONTEXT) : MRESULT;

    function WinDdePostMsg(hwndTo : HWND;hwndFrom : HWND;wm : ULONG;pddest : PDDESTRUCT;flOptions : ULONG) : BOOL;

    const
       DDEPM_RETRY = $00000001;
       DDEPM_NOFREE = $00000002;
       WM_DDE_FIRST = $00A0;
       WM_DDE_INITIATE = $00A0;
       WM_DDE_REQUEST = $00A1;
       WM_DDE_ACK = $00A2;
       WM_DDE_DATA = $00A3;
       WM_DDE_ADVISE = $00A4;
       WM_DDE_UNADVISE = $00A5;
       WM_DDE_POKE = $00A6;
       WM_DDE_EXECUTE = $00A7;
       WM_DDE_TERMINATE = $00A8;
       WM_DDE_INITIATEACK = $00A9;
       WM_DDE_LAST = $00AF;
       WM_QUERYCONVERTPOS = $00b0;
       QCP_CONVERT = $0001;
       QCP_NOCONVERT = $0000;

    type
       HLIB = HMODULE;

       PHLIB = PHMODULE;

    function WinDeleteProcedure(hab : HAB;wndproc : PFNWP) : BOOL;

    function WinDeleteLibrary(hab : HAB;libhandle : HLIB) : BOOL;

    function WinLoadProcedure(hab : HAB;libhandle : HLIB;procname : PSZ) : PFNWP;

    function WinLoadLibrary(hab : HAB;libname : PSZ) : HLIB;

    type
       DESKTOP = record
          cbSize : ULONG;
          hbm : HBITMAP;
          x : LONG;
          y : LONG;
          fl : ULONG;
          lTileCount : LONG;
          szFile : array[0..260-1] of CHAR;
       end;

       PDESKTOP = ^DESKTOP;

    function WinSetDesktopBkgnd(hwndDesktop : HWND;pdskNew : PDESKTOP) : HBITMAP;

    function WinQueryDesktopBkgnd(hwndDesktop : HWND;pdsk : PDESKTOP) : BOOL;

    const
       SDT_DESTROY = $0001;
       SDT_NOBKGND = $0002;
       SDT_TILE = $0004;
       SDT_SCALE = $0008;
       SDT_PATTERN = $0010;
       SDT_CENTER = $0020;
       SDT_RETAIN = $0040;
       SDT_LOADFILE = $0080;

    function WinRealizePalette(hwnd : HWND;hps : HPS;pcclr : PULONG) : LONG;

    const
       STR_DLLNAME = 'keyremap';
       WM_DBCSFIRST = $00b0;
       WM_DBCSLAST = $00cf;

    function HWNDFROMMP(mp : MPARAM) : HWND;
    function SHORT1FROMMP(mp : MPARAM) : USHORT;
    function SHORT2FROMMP(mp : MPARAM) : USHORT;

  implementation

    function WinRegisterClass(hab : HAB;pszClassName : PSZ;pfnWndProc : PFNWP;flStyle : ULONG;cbWindowData : ULONG) : BOOL;[SYSTEM];
    function WinDefWindowProc(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;[SYSTEM];
    function WinDestroyWindow(hwnd : HWND) : BOOL;[SYSTEM];
    function WinShowWindow(hwnd : HWND;fShow : BOOL) : BOOL;[SYSTEM];
    function WinQueryWindowRect(hwnd : HWND;prclDest : PRECTL) : BOOL;[SYSTEM];
    function WinGetPS(hwnd : HWND) : HPS;[SYSTEM];
    function WinReleasePS(hps : HPS) : BOOL;[SYSTEM];
    function WinEndPaint(hps : HPS) : BOOL;[SYSTEM];
    function WinGetClipPS(hwnd : HWND;hwndClip : HWND;fl : ULONG) : HPS;[SYSTEM];
    function WinIsWindowShowing(hwnd : HWND) : BOOL;[SYSTEM];
    function WinBeginPaint(hwnd : HWND;hps : HPS;prclPaint : PRECTL) : HPS;[SYSTEM];
    function WinOpenWindowDC(hwnd : HWND) : HDC;[SYSTEM];
    function WinScrollWindow(hwnd : HWND;dx : LONG;dy : LONG;prclScroll : PRECTL;prclClip : PRECTL;hrgnUpdate : HRGN;prclUpdate : PRECTL;rgfsw : ULONG) : LONG;[SYSTEM];
    function WinFillRect(hps : HPS;prcl : PRECTL;lColor : LONG) : BOOL;[SYSTEM];
    function WinQueryVersion(hab : HAB) : ULONG;[SYSTEM];
    function WinInitialize(flOptions : ULONG) : HAB;[SYSTEM];
    function WinTerminate(hab : HAB) : BOOL;[SYSTEM];
    function WinQueryAnchorBlock(hwnd : HWND) : HAB;[SYSTEM];
    function WinCreateWindow(hwndParent : HWND;pszClass : PSZ;pszName : PSZ;flStyle : ULONG;x : LONG;y : LONG;cx : LONG;cy : LONG;hwndOwner : HWND;hwndInsertBehind : HWND;id : ULONG;pCtlData : PVOID;pPresParams : PVOID) : HWND;[SYSTEM];
    function WinEnableWindow(hwnd : HWND;fEnable : BOOL) : BOOL;[SYSTEM];
    function WinIsWindowEnabled(hwnd : HWND) : BOOL;[SYSTEM];
    function WinEnableWindowUpdate(hwnd : HWND;fEnable : BOOL) : BOOL;[SYSTEM];
    function WinIsWindowVisible(hwnd : HWND) : BOOL;[SYSTEM];
    function WinQueryWindowText(hwnd : HWND;cchBufferMax : LONG;pchBuffer : PCH) : LONG;[SYSTEM];
    function WinSetWindowText(hwnd : HWND;pszText : PSZ) : BOOL;[SYSTEM];
    function WinQueryWindowTextLength(hwnd : HWND) : LONG;[SYSTEM];
    function WinWindowFromID(hwndParent : HWND;id : ULONG) : HWND;[SYSTEM];
    function WinIsWindow(hab : HAB;hwnd : HWND) : BOOL;[SYSTEM];
    function WinQueryWindow(hwnd : HWND;cmd : LONG) : HWND;[SYSTEM];
    function WinMultWindowFromIDs(hwndParent : HWND;prghwnd : PHWND;idFirst : ULONG;idLast : ULONG) : LONG;[SYSTEM];
    function WinSetParent(hwnd : HWND;hwndNewParent : HWND;fRedraw : BOOL) : BOOL;[SYSTEM];
    function WinIsChild(hwnd : HWND;hwndParent : HWND) : BOOL;[SYSTEM];
    function WinSetOwner(hwnd : HWND;hwndNewOwner : HWND) : BOOL;[SYSTEM];
    function WinQueryWindowProcess(hwnd : HWND;ppid : PPID;ptid : PTID) : BOOL;[SYSTEM];
    function WinQueryObjectWindow(hwndDesktop : HWND) : HWND;[SYSTEM];
    function WinQueryDesktopWindow(hab : HAB;hdc : HDC) : HWND;[SYSTEM];
    function WinSetWindowPos(hwnd : HWND;hwndInsertBehind : HWND;x : LONG;y : LONG;cx : LONG;cy : LONG;fl : ULONG) : BOOL;[SYSTEM];
    function WinSetMultWindowPos(hab : HAB;pswp : PSWP;cswp : ULONG) : BOOL;[SYSTEM];
    function WinQueryWindowPos(hwnd : HWND;pswp : PSWP) : BOOL;[SYSTEM];
    function WinUpdateWindow(hwnd : HWND) : BOOL;[SYSTEM];
    function WinInvalidateRect(hwnd : HWND;pwrc : PRECTL;fIncludeChildren : BOOL) : BOOL;[SYSTEM];
    function WinInvalidateRegion(hwnd : HWND;hrgn : HRGN;fIncludeChildren : BOOL) : BOOL;[SYSTEM];
    function WinInvertRect(hps : HPS;prcl : PRECTL) : BOOL;[SYSTEM];
    function WinDrawBitmap(hpsDst : HPS;hbm : HBITMAP;pwrcSrc : PRECTL;pptlDst : PPOINTL;clrFore : LONG;clrBack : LONG;fl : ULONG) : BOOL;[SYSTEM];
    function WinDrawText(hps : HPS;cchText : LONG;lpchText : PCH;prcl : PRECTL;clrFore : LONG;clrBack : LONG;flCmd : ULONG) : LONG;[SYSTEM];
    function WinDrawBorder(hps : HPS;prcl : PRECTL;cx : LONG;cy : LONG;clrFore : LONG;clrBack : LONG;flCmd : ULONG) : BOOL;[SYSTEM];
    function WinLoadString(hab : HAB;hmod : HMODULE;id : ULONG;cchMax : LONG;pchBuffer : PSZ) : LONG;[SYSTEM];
    function WinLoadMessage(hab : HAB;hmod : HMODULE;id : ULONG;cchMax : LONG;pchBuffer : PSZ) : LONG;[SYSTEM];
    function WinSetActiveWindow(hwndDesktop : HWND;hwnd : HWND) : BOOL;[SYSTEM];
    function WinSubclassWindow(hwnd : HWND;pfnwp : PFNWP) : PFNWP;[SYSTEM];
    function WinQueryClassName(hwnd : HWND;cchMax : LONG;pch : PCH) : LONG;[SYSTEM];
    function WinQueryClassInfo(hab : HAB;pszClassName : PSZ;pClassInfo : PCLASSINFO) : BOOL;[SYSTEM];
    function WinQueryActiveWindow(hwndDesktop : HWND) : HWND;[SYSTEM];
    function WinIsThreadActive(hab : HAB) : BOOL;[SYSTEM];
    function WinQuerySysModalWindow(hwndDesktop : HWND) : HWND;[SYSTEM];
    function WinSetSysModalWindow(hwndDesktop : HWND;hwnd : HWND) : BOOL;[SYSTEM];
    function WinQueryWindowUShort(hwnd : HWND;index : LONG) : USHORT;[SYSTEM];
    function WinSetWindowUShort(hwnd : HWND;index : LONG;us : USHORT) : BOOL;[SYSTEM];
    function WinQueryWindowULong(hwnd : HWND;index : LONG) : ULONG;[SYSTEM];
    function WinSetWindowULong(hwnd : HWND;index : LONG;ul : ULONG) : BOOL;[SYSTEM];
    function WinQueryWindowPtr(hwnd : HWND;index : LONG) : PVOID;[SYSTEM];
    function WinSetWindowPtr(hwnd : HWND;index : LONG;p : PVOID) : BOOL;[SYSTEM];
    function WinSetWindowBits(hwnd : HWND;index : LONG;flData : ULONG;flMask : ULONG) : BOOL;[SYSTEM];
    function WinBeginEnumWindows(hwnd : HWND) : HENUM;[SYSTEM];
    function WinGetNextWindow(henum : HENUM) : HWND;[SYSTEM];
    function WinEndEnumWindows(henum : HENUM) : BOOL;[SYSTEM];
    function WinWindowFromPoint(hwnd : HWND;pptl : PPOINTL;fChildren : BOOL) : HWND;[SYSTEM];
    function WinMapWindowPoints(hwndFrom : HWND;hwndTo : HWND;prgptl : PPOINTL;cwpt : LONG) : BOOL;[SYSTEM];
    function WinValidateRect(hwnd : HWND;prcl : PRECTL;fIncludeChildren : BOOL) : BOOL;[SYSTEM];
    function WinValidateRegion(hwnd : HWND;hrgn : HRGN;fIncludeChildren : BOOL) : BOOL;[SYSTEM];
    function WinWindowFromDC(hdc : HDC) : HWND;[SYSTEM];
    function WinQueryWindowDC(hwnd : HWND) : HDC;[SYSTEM];
    function WinGetScreenPS(hwndDesktop : HWND) : HPS;[SYSTEM];
    function WinLockWindowUpdate(hwndDesktop : HWND;hwndLockUpdate : HWND) : BOOL;[SYSTEM];
    function WinLockVisRegions(hwndDesktop : HWND;fLock : BOOL) : BOOL;[SYSTEM];
    function WinQueryUpdateRect(hwnd : HWND;prcl : PRECTL) : BOOL;[SYSTEM];
    function WinQueryUpdateRegion(hwnd : HWND;hrgn : HRGN) : LONG;[SYSTEM];
    function WinExcludeUpdateRegion(hps : HPS;hwnd : HWND) : LONG;[SYSTEM];
    function WinSendMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;[SYSTEM];
    function WinCreateMsgQueue(hab : HAB;cmsg : LONG) : HMQ;[SYSTEM];
    function WinDestroyMsgQueue(hmq : HMQ) : BOOL;[SYSTEM];
    function WinQueryQueueInfo(hmq : HMQ;pmqi : PMQINFO;cbCopy : ULONG) : BOOL;[SYSTEM];
    function WinCancelShutdown(hmq : HMQ;fCancelAlways : BOOL) : BOOL;[SYSTEM];
    function WinGetMsg(hab : HAB;pqmsg : PQMSG;hwndFilter : HWND;msgFilterFirst : ULONG;msgFilterLast : ULONG) : BOOL;[SYSTEM];
    function WinPeekMsg(hab : HAB;pqmsg : PQMSG;hwndFilter : HWND;msgFilterFirst : ULONG;msgFilterLast : ULONG;fl : ULONG) : BOOL;[SYSTEM];
    function WinDispatchMsg(hab : HAB;pqmsg : PQMSG) : MRESULT;[SYSTEM];
    function WinPostMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : BOOL;[SYSTEM];
    function WinRegisterUserMsg(hab : HAB;msgid : ULONG;datatype1 : LONG;dir1 : LONG;datatype2 : LONG;dir2 : LONG;datatyper : LONG) : BOOL;[SYSTEM];
    function WinRegisterUserDatatype(hab : HAB;datatype : LONG;count : LONG;types : PLONG) : BOOL;[SYSTEM];
    function WinSetMsgMode(hab : HAB;classname : PSZ;control : LONG) : BOOL;[SYSTEM];
    function WinSetSynchroMode(hab : HAB;mode : LONG) : BOOL;[SYSTEM];
    function WinInSendMsg(hab : HAB) : BOOL;[SYSTEM];
    function WinBroadcastMsg(hwnd : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM;rgf : ULONG) : BOOL;[SYSTEM];
    function WinWaitMsg(hab : HAB;msgFirst : ULONG;msgLast : ULONG) : BOOL;[SYSTEM];
    function WinQueryQueueStatus(hwndDesktop : HWND) : ULONG;[SYSTEM];
    function WinQueryMsgPos(hab : HAB;pptl : PPOINTL) : BOOL;[SYSTEM];
    function WinQueryMsgTime(hab : HAB) : ULONG;[SYSTEM];
    function WinWaitEventSem(hev : HEV;ulTimeout : ULONG) : APIRET;[SYSTEM];
    function WinRequestMutexSem(hmtx : HMTX;ulTimeout : ULONG) : APIRET;[SYSTEM];
    function WinWaitMuxWaitSem(hmux : HMUX;ulTimeout : ULONG;pulUser : PULONG) : APIRET;[SYSTEM];
    function WinPostQueueMsg(hmq : HMQ;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : BOOL;[SYSTEM];
    function WinSetMsgInterest(hwnd : HWND;msg_class : ULONG;control : LONG) : BOOL;[SYSTEM];
    function WinSetClassMsgInterest(hab : HAB;pszClassName : PSZ;msg_class : ULONG;control : LONG) : BOOL;[SYSTEM];
    function WinSetFocus(hwndDesktop : HWND;hwndSetFocus : HWND) : BOOL;[SYSTEM];
    function WinFocusChange(hwndDesktop : HWND;hwndSetFocus : HWND;flFocusChange : ULONG) : BOOL;[SYSTEM];
    function WinSetCapture(hwndDesktop : HWND;hwnd : HWND) : BOOL;[SYSTEM];
    function WinQueryCapture(hwndDesktop : HWND) : HWND;[SYSTEM];
    function WinQueryFocus(hwndDesktop : HWND) : HWND;[SYSTEM];
    function WinGetKeyState(hwndDesktop : HWND;vkey : LONG) : LONG;[SYSTEM];
    function WinGetPhysKeyState(hwndDesktop : HWND;sc : LONG) : LONG;[SYSTEM];
    function WinEnablePhysInput(hwndDesktop : HWND;fEnable : BOOL) : BOOL;[SYSTEM];
    function WinIsPhysInputEnabled(hwndDesktop : HWND) : BOOL;[SYSTEM];
    function WinSetKeyboardStateTable(hwndDesktop : HWND;pKeyStateTable : PBYTE;fSet : BOOL) : BOOL;[SYSTEM];
    function WinGetDlgMsg(hwndDlg : HWND;pqmsg : PQMSG) : BOOL;[SYSTEM];
    function WinLoadDlg(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;hmod : HMODULE;idDlg : ULONG;pCreateParams : PVOID) : HWND;[SYSTEM];
    function WinDlgBox(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;hmod : HMODULE;idDlg : ULONG;pCreateParams : PVOID) : ULONG;[SYSTEM];
    function WinDismissDlg(hwndDlg : HWND;usResult : ULONG) : BOOL;[SYSTEM];
    function WinQueryDlgItemShort(hwndDlg : HWND;idItem : ULONG;pResult : PSHORT;fSigned : BOOL) : BOOL;[SYSTEM];
    function WinSetDlgItemShort(hwndDlg : HWND;idItem : ULONG;usValue : USHORT;fSigned : BOOL) : BOOL;[SYSTEM];
    function WinSetDlgItemText(hwndDlg : HWND;idItem : ULONG;pszText : PSZ) : BOOL;[SYSTEM];
    function WinQueryDlgItemText(hwndDlg : HWND;idItem : ULONG;cchBufferMax : LONG;pchBuffer : PSZ) : ULONG;[SYSTEM];
    function WinQueryDlgItemTextLength(hwndDlg : HWND;idItem : ULONG) : LONG;[SYSTEM];
    function WinDefDlgProc(hwndDlg : HWND;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;[SYSTEM];
    function WinAlarm(hwndDesktop : HWND;rgfType : ULONG) : BOOL;[SYSTEM];
    function WinMessageBox(hwndParent : HWND;hwndOwner : HWND;pszText : PSZ;pszCaption : PSZ;idWindow : ULONG;flStyle : ULONG) : ULONG;[SYSTEM];
    function WinProcessDlg(hwndDlg : HWND) : ULONG;[SYSTEM];
    function WinSendDlgItemMsg(hwndDlg : HWND;idItem : ULONG;msg : ULONG;mp1 : MPARAM;mp2 : MPARAM) : MRESULT;[SYSTEM];
    function WinMapDlgPoints(hwndDlg : HWND;prgwptl : PPOINTL;cwpt : ULONG;fCalcWindowCoords : BOOL) : BOOL;[SYSTEM];
    function WinEnumDlgItem(hwndDlg : HWND;hwnd : HWND;code : ULONG) : HWND;[SYSTEM];
    function WinSubstituteStrings(hwnd : HWND;pszSrc : PSZ;cchDstMax : LONG;pszDst : PSZ) : LONG;[SYSTEM];
    function WinCreateDlg(hwndParent : HWND;hwndOwner : HWND;pfnDlgProc : PFNWP;pdlgt : PDLGTEMPLATE;pCreateParams : PVOID) : HWND;[SYSTEM];
    function WinLoadMenu(hwndFrame : HWND;hmod : HMODULE;idMenu : ULONG) : HWND;[SYSTEM];
    function WinCreateMenu(hwndParent : HWND;lpmt : PVOID) : HWND;[SYSTEM];
    function WinPopupMenu(hwndParent : HWND;hwndOwner : HWND;hwndMenu : HWND;x : LONG;y : LONG;idItem : LONG;fs : ULONG) : BOOL;[SYSTEM];
    function WinCreateStdWindow(hwndParent : HWND;flStyle : ULONG;pflCreateFlags : PULONG;pszClientClass : PSZ;pszTitle : PSZ;styleClient : ULONG;hmod : HMODULE;idResources : ULONG;phwndClient : PHWND) : HWND;[SYSTEM];
    function WinFlashWindow(hwndFrame : HWND;fFlash : BOOL) : BOOL;[SYSTEM];
    function WinCreateFrameControls(hwndFrame : HWND;pfcdata : PFRAMECDATA;pszTitle : PSZ) : BOOL;[SYSTEM];
    function WinCalcFrameRect(hwndFrame : HWND;prcl : PRECTL;fClient : BOOL) : BOOL;[SYSTEM];
    function WinGetMinPosition(hwnd : HWND;pswp : PSWP;pptl : PPOINTL) : BOOL;[SYSTEM];
    function WinGetMaxPosition(hwnd : HWND;pswp : PSWP) : BOOL;[SYSTEM];
    function WinSaveWindowPos(hsvwp : HSAVEWP;pswp : PSWP;cswp : ULONG) : BOOL;[SYSTEM];
    function WinCopyRect(hab : HAB;prclDst : PRECTL;prclSrc : PRECTL) : BOOL;[SYSTEM];
    function WinSetRect(hab : HAB;prcl : PRECTL;xLeft : LONG;yBottom : LONG;xRight : LONG;yTop : LONG) : BOOL;[SYSTEM];
    function WinIsRectEmpty(hab : HAB;prcl : PRECTL) : BOOL;[SYSTEM];
    function WinEqualRect(hab : HAB;prcl1 : PRECTL;prcl2 : PRECTL) : BOOL;[SYSTEM];
    function WinSetRectEmpty(hab : HAB;prcl : PRECTL) : BOOL;[SYSTEM];
    function WinOffsetRect(hab : HAB;prcl : PRECTL;cx : LONG;cy : LONG) : BOOL;[SYSTEM];
    function WinInflateRect(hab : HAB;prcl : PRECTL;cx : LONG;cy : LONG) : BOOL;[SYSTEM];
    function WinPtInRect(hab : HAB;prcl : PRECTL;pptl : PPOINTL) : BOOL;[SYSTEM];
    function WinIntersectRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;[SYSTEM];
    function WinUnionRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;[SYSTEM];
    function WinSubtractRect(hab : HAB;prclDst : PRECTL;prclSrc1 : PRECTL;prclSrc2 : PRECTL) : BOOL;[SYSTEM];
    function WinMakeRect(hab : HAB;pwrc : PRECTL) : BOOL;[SYSTEM];
    function WinMakePoints(hab : HAB;pwpt : PPOINTL;cwpt : ULONG) : BOOL;[SYSTEM];
    function WinQuerySysValue(hwndDesktop : HWND;iSysValue : LONG) : LONG;[SYSTEM];
    function WinSetSysValue(hwndDesktop : HWND;iSysValue : LONG;lValue : LONG) : BOOL;[SYSTEM];
    function WinSetPresParam(hwnd : HWND;id : ULONG;cbParam : ULONG;pbParam : PVOID) : BOOL;[SYSTEM];
    function WinQueryPresParam(hwnd : HWND;id1 : ULONG;id2 : ULONG;pulId : PULONG;cbBuf : ULONG;pbBuf : PVOID;fs : ULONG) : ULONG;[SYSTEM];
    function WinRemovePresParam(hwnd : HWND;id : ULONG) : BOOL;[SYSTEM];
    function WinQuerySysColor(hwndDesktop : HWND;clr : LONG;lReserved : LONG) : LONG;[SYSTEM];
    function WinSetSysColors(hwndDesktop : HWND;flOptions : ULONG;flFormat : ULONG;clrFirst : LONG;cclr : ULONG;pclr : PLONG) : BOOL;[SYSTEM];
    function WinStartTimer(hab : HAB;hwnd : HWND;idTimer : ULONG;dtTimeout : ULONG) : ULONG;[SYSTEM];
    function WinStopTimer(hab : HAB;hwnd : HWND;idTimer : ULONG) : BOOL;[SYSTEM];
    function WinGetCurrentTime(hab : HAB) : ULONG;[SYSTEM];
    function WinLoadAccelTable(hab : HAB;hmod : HMODULE;idAccelTable : ULONG) : HACCEL;[SYSTEM];
    function WinCopyAccelTable(haccel : HACCEL;pAccelTable : PACCELTABLE;cbCopyMax : ULONG) : ULONG;[SYSTEM];
    function WinCreateAccelTable(hab : HAB;pAccelTable : PACCELTABLE) : HACCEL;[SYSTEM];
    function WinDestroyAccelTable(haccel : HACCEL) : BOOL;[SYSTEM];
    function WinTranslateAccel(hab : HAB;hwnd : HWND;haccel : HACCEL;pqmsg : PQMSG) : BOOL;[SYSTEM];
    function WinSetAccelTable(hab : HAB;haccel : HACCEL;hwndFrame : HWND) : BOOL;[SYSTEM];
    function WinQueryAccelTable(hab : HAB;hwndFrame : HWND) : HACCEL;[SYSTEM];
    function WinTrackRect(hwnd : HWND;hps : HPS;pti : PTRACKINFO) : BOOL;[SYSTEM];
    function WinShowTrackRect(hwnd : HWND;fShow : BOOL) : BOOL;[SYSTEM];
    function WinSetClipbrdOwner(hab : HAB;hwnd : HWND) : BOOL;[SYSTEM];
    function WinSetClipbrdData(hab : HAB;ulData : ULONG;fmt : ULONG;rgfFmtInfo : ULONG) : BOOL;[SYSTEM];
    function WinQueryClipbrdData(hab : HAB;fmt : ULONG) : ULONG;[SYSTEM];
    function WinQueryClipbrdFmtInfo(hab : HAB;fmt : ULONG;prgfFmtInfo : PULONG) : BOOL;[SYSTEM];
    function WinSetClipbrdViewer(hab : HAB;hwndNewClipViewer : HWND) : BOOL;[SYSTEM];
    function WinEnumClipbrdFmts(hab : HAB;fmt : ULONG) : ULONG;[SYSTEM];
    function WinEmptyClipbrd(hab : HAB) : BOOL;[SYSTEM];
    function WinOpenClipbrd(hab : HAB) : BOOL;[SYSTEM];
    function WinCloseClipbrd(hab : HAB) : BOOL;[SYSTEM];
    function WinQueryClipbrdOwner(hab : HAB) : HWND;[SYSTEM];
    function WinQueryClipbrdViewer(hab : HAB) : HWND;[SYSTEM];
    function WinDestroyCursor(hwnd : HWND) : BOOL;[SYSTEM];
    function WinShowCursor(hwnd : HWND;fShow : BOOL) : BOOL;[SYSTEM];
    function WinCreateCursor(hwnd : HWND;x : LONG;y : LONG;cx : LONG;cy : LONG;fs : ULONG;prclClip : PRECTL) : BOOL;[SYSTEM];
    function WinQueryCursorInfo(hwndDesktop : HWND;pCursorInfo : PCURSORINFO) : BOOL;[SYSTEM];
    function WinSetPointer(hwndDesktop : HWND;hptrNew : HPOINTER) : BOOL;[SYSTEM];
    function WinSetPointerOwner(hptr : HPOINTER;pid : PID;fDestroy : BOOL) : BOOL;[SYSTEM];
    function WinShowPointer(hwndDesktop : HWND;fShow : BOOL) : BOOL;[SYSTEM];
    function WinQuerySysPointer(hwndDesktop : HWND;iptr : LONG;fLoad : BOOL) : HPOINTER;[SYSTEM];
    function WinLoadPointer(hwndDesktop : HWND;hmod : HMODULE;idres : ULONG) : HPOINTER;[SYSTEM];
    function WinCreatePointer(hwndDesktop : HWND;hbmPointer : HBITMAP;fPointer : BOOL;xHotspot : LONG;yHotspot : LONG) : HPOINTER;[SYSTEM];
    function WinSetPointerPos(hwndDesktop : HWND;x : LONG;y : LONG) : BOOL;[SYSTEM];
    function WinDestroyPointer(hptr : HPOINTER) : BOOL;[SYSTEM];
    function WinQueryPointer(hwndDesktop : HWND) : HPOINTER;[SYSTEM];
    function WinQueryPointerPos(hwndDesktop : HWND;pptl : PPOINTL) : BOOL;[SYSTEM];
    function WinCreatePointerIndirect(hwndDesktop : HWND;pptri : PPOINTERINFO) : HPOINTER;[SYSTEM];
    function WinQueryPointerInfo(hptr : HPOINTER;pPointerInfo : PPOINTERINFO) : BOOL;[SYSTEM];
    function WinDrawPointer(hps : HPS;x : LONG;y : LONG;hptr : HPOINTER;fs : ULONG) : BOOL;[SYSTEM];
    function WinGetSysBitmap(hwndDesktop : HWND;ibm : ULONG) : HBITMAP;[SYSTEM];
    function WinSetHook(hab : HAB;hmq : HMQ;iHook : LONG;pfnHook : PFN;hmod : HMODULE) : BOOL;[SYSTEM];
    function WinReleaseHook(hab : HAB;hmq : HMQ;iHook : LONG;pfnHook : PFN;hmod : HMODULE) : BOOL;[SYSTEM];
    function WinCallMsgFilter(hab : HAB;pqmsg : PQMSG;msgf : ULONG) : BOOL;[SYSTEM];
    function WinSetClassThunkProc(pszClassname : PSZ;pfnThunkProc : PFN) : BOOL;[SYSTEM];
    function WinQueryClassThunkProc(pszClassname : PSZ) : PFN;[SYSTEM];
    function WinSetWindowThunkProc(hwnd : HWND;pfnThunkProc : PFN) : BOOL;[SYSTEM];
    function WinQueryWindowThunkProc(hwnd : HWND) : PFN;[SYSTEM];
    function WinQueryWindowModel(hwnd : HWND) : LONG;[SYSTEM];
    function WinQueryCp(hmq : HMQ) : ULONG;[SYSTEM];
    function WinSetCp(hmq : HMQ;idCodePage : ULONG) : BOOL;[SYSTEM];
    function WinQueryCpList(hab : HAB;ccpMax : ULONG;prgcp : PULONG) : ULONG;[SYSTEM];
    function WinCpTranslateString(hab : HAB;cpSrc : ULONG;pszSrc : PSZ;cpDst : ULONG;cchDestMax : ULONG;pchDest : PSZ) : BOOL;[SYSTEM];
    function WinCpTranslateChar(hab : HAB;cpSrc : ULONG;chSrc : UCHAR;cpDst : ULONG) : UCHAR;[SYSTEM];
    function WinUpper(hab : HAB;idcp : ULONG;idcc : ULONG;psz : PSZ) : ULONG;[SYSTEM];
    function WinUpperChar(hab : HAB;idcp : ULONG;idcc : ULONG;c : ULONG) : ULONG;[SYSTEM];
    function WinNextChar(hab : HAB;idcp : ULONG;idcc : ULONG;psz : PSZ) : PSZ;[SYSTEM];
    function WinPrevChar(hab : HAB;idcp : ULONG;idcc : ULONG;pszStart : PSZ;psz : PSZ) : PSZ;[SYSTEM];
    function WinCompareStrings(hab : HAB;idcp : ULONG;idcc : ULONG;psz1 : PSZ;psz2 : PSZ;reserved : ULONG) : ULONG;[SYSTEM];
    function WinCreateAtomTable(cbInitial : ULONG;cBuckets : ULONG) : HATOMTBL;[SYSTEM];
    function WinDestroyAtomTable(hAtomTbl : HATOMTBL) : HATOMTBL;[SYSTEM];
    function WinAddAtom(hAtomTbl : HATOMTBL;pszAtomName : PSZ) : ATOM;[SYSTEM];
    function WinFindAtom(hAtomTbl : HATOMTBL;pszAtomName : PSZ) : ATOM;[SYSTEM];
    function WinDeleteAtom(hAtomTbl : HATOMTBL;atom : ATOM) : ATOM;[SYSTEM];
    function WinQueryAtomUsage(hAtomTbl : HATOMTBL;atom : ATOM) : ULONG;[SYSTEM];
    function WinQueryAtomLength(hAtomTbl : HATOMTBL;atom : ATOM) : ULONG;[SYSTEM];
    function WinQueryAtomName(hAtomTbl : HATOMTBL;atom : ATOM;pchBuffer : PSZ;cchBufferMax : ULONG) : ULONG;[SYSTEM];
    function WinGetLastError(hab : HAB) : ERRORID;[SYSTEM];
    function WinGetErrorInfo(hab : HAB) : PERRINFO;[SYSTEM];
    function WinFreeErrorInfo(perrinfo : PERRINFO) : BOOL;[SYSTEM];
    function WinDdeInitiate(hwndClient : HWND;pszAppName : PSZ;pszTopicName : PSZ;pcctxt : PCONVCONTEXT) : BOOL;[SYSTEM];
    function WinDdeRespond(hwndClient : HWND;hwndServer : HWND;pszAppName : PSZ;pszTopicName : PSZ;pcctxt : PCONVCONTEXT) : MRESULT;[SYSTEM];
    function WinDdePostMsg(hwndTo : HWND;hwndFrom : HWND;wm : ULONG;pddest : PDDESTRUCT;flOptions : ULONG) : BOOL;[SYSTEM];
    function WinDeleteProcedure(hab : HAB;wndproc : PFNWP) : BOOL;[SYSTEM];
    function WinDeleteLibrary(hab : HAB;libhandle : HLIB) : BOOL;[SYSTEM];
    function WinLoadProcedure(hab : HAB;libhandle : HLIB;procname : PSZ) : PFNWP;[SYSTEM];
    function WinLoadLibrary(hab : HAB;libname : PSZ) : HLIB;[SYSTEM];
    function WinSetDesktopBkgnd(hwndDesktop : HWND;pdskNew : PDESKTOP) : HBITMAP;[SYSTEM];
    function WinQueryDesktopBkgnd(hwndDesktop : HWND;pdsk : PDESKTOP) : BOOL;[SYSTEM];
    function WinRealizePalette(hwnd : HWND;hps : HPS;pcclr : PULONG) : LONG;[SYSTEM];

    function HWNDFROMMP(mp : MPARAM) : HWND;

      begin
         HWNDFROMMP:=HWND(mp);
      end;

    function SHORT1FROMMP(mp : MPARAM) : USHORT;

      begin
         SHORT1FROMMP:=lo(ULONG(mp));
      end;

    function SHORT2FROMMP(mp : MPARAM) : USHORT;

      begin
         SHORT2FROMMP:=hi(ULONG(mp));
      end;

end.
