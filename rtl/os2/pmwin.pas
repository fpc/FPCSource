{****************************************************************************


    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    Copyright (c) 1999-2000 by Ramon Bosque
    Copyrigth (c) 2003 by Yuri Prokushev

    OS/2 Presentation Manager windowing functions, plus common
    PM constants and types (PMWIN.DLL interface unit).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit pmwin;

{$mode objfpc}

{$MACRO ON}

  interface

    uses
       os2def;

const
  MaxMB2DText = 70;

    type
       proc=function (hwnd,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;
       QVERSDATA = record
        environment : word;
        version : word;
       end;
       PQVERSDATA = ^QVERSDATA;
       SWP = record
        fl : cardinal;
        cy : longint;
        cx : longint;
        y : longint;
        x : longint;
        hwndInsertBehind : cardinal;
        hwnd : cardinal;
        ulReserved1 : cardinal;
        ulReserved2 : cardinal;
       end;
       PSWP = ^SWP;
       CREATESTRUCT = record
        pPresParams : pointer;
        pCtlData : pointer;
        id : cardinal;
        hwndInsertBehind : cardinal;
        hwndOwner : cardinal;
        cy : longint;
        cx : longint;
        y : longint;
        x : longint;
        flStyle : cardinal;
        pszText : pchar;
        pszClass : pchar;
        hwndParent : cardinal;
       end;
       PCREATESTRUCT = ^CREATESTRUCT;
       CLASSINFO = record
        flClassStyle : cardinal;
        pfnWindowProc : proc;
        cbWindowData : cardinal;
       end;
       PCLASSINFO = ^CLASSINFO;
       QMSG = record
        hwnd : cardinal;
        msg : cardinal;
        mp1 : pointer;
        mp2 : pointer;
        time : cardinal;
        ptl : POINTL;
        reserved : cardinal;
       end;
       PQMSG = ^QMSG;
       MQINFO = record
        cb : cardinal;
        pid : cardinal;
        tid : cardinal;
        cmsgs : cardinal;
        pReserved : pointer;
       end;
       PMQINFO = ^MQINFO;
       WNDPARAMS = record
        fsStatus : cardinal;
        cchText : cardinal;
        pszText : pchar;
        cbPresParams : cardinal;
        pPresParams : pointer;
        cbCtlData : cardinal;
        pCtlData : pointer;
       end;
       PWNDPARAMS = ^WNDPARAMS;
       USERBUTTON = record
        hwnd : cardinal;
        hps : cardinal;
        fsState : cardinal;
        fsStateOld : cardinal;
       end;
       PUSERBUTTON = ^USERBUTTON;
       OWNERITEM = record
        hwnd : cardinal;
        hps : cardinal;
        fsState : cardinal;
        fsAttribute : cardinal;
        fsStateOld : cardinal;
        fsAttributeOld : cardinal;
        rclItem : TRectl;
        idItem : longint;
        hItem : cardinal;
       end;
       POWNERITEM = ^OWNERITEM;
       PARAM = record
        id : cardinal;
        cb : cardinal;
        ab : array[0..1-1] of BYTE;
       end;
       PPARAM = ^PARAM;
       PRESPARAMS = record
        cb : cardinal;
        aparam : array[0..1-1] of PARAM;
       end;
       PPRESPARAMS = ^PRESPARAMS;
       TRACKINFO = record
        cxBorder : longint;
        cyBorder : longint;
        cxGrid : longint;
        cyGrid : longint;
        cxKeyboard : longint;
        cyKeyboard : longint;
        rclTrack : TRectl;
        rclBoundary : TRectl;
        ptlMinTrackSize : POINTL;
        ptlMaxTrackSize : POINTL;
        fs : cardinal;
       end;
       PTRACKINFO = ^TRACKINFO;
       CURSORINFO = record
        hwnd : cardinal;
        x : longint;
        y : longint;
        cx : longint;
        cy : longint;
        fs : cardinal;
        rclClip : TRectl;
       end;
       PCURSORINFO = ^CURSORINFO;
       POINTERINFO = record
        fPointer : cardinal;
        xHotspot : longint;
        yHotspot : longint;
        hbmPointer : cardinal;
        hbmColor : cardinal;
        hbmMiniPointer : cardinal;
        hbmMiniColor : cardinal;
       end;
       PPOINTERINFO = ^POINTERINFO;
       SMHSTRUCT = record
        mp2 : pointer;
        mp1 : pointer;
        msg : cardinal;
        hwnd : cardinal;
        model : cardinal;
       end;
       PSMHSTRUCT = ^SMHSTRUCT;
       ERRINFO = record
        cbFixedErrInfo : cardinal;
        idError : cardinal;
        cDetailLevel : cardinal;
        offaoffszMsg : cardinal;
        offBinaryData : cardinal;
       end;
       PERRINFO = ^ERRINFO;
       CONVCONTEXT = record
        cb : cardinal;
        fsContext : cardinal;
        idCountry : cardinal;
        usCodepage : cardinal;
        usLangID : cardinal;
        usSubLangID : cardinal;
       end;
       PCONVCONTEXT = ^CONVCONTEXT;
       DDEINIT = record
        cb : cardinal;
        pszAppName : pchar;
        pszTopic : pchar;
        offConvContext : cardinal;
       end;
       PDDEINIT = ^DDEINIT;
       DDESTRUCT = record
        cbData : cardinal;
        fsStatus : word;
        usFormat : word;
        offszItemName : word;
        offabData : word;
       end;
       PDDESTRUCT = ^DDESTRUCT;
       DESKTOP = record
        cbSize : cardinal;
        hbm : cardinal;
        x : longint;
        y : longint;
        fl : cardinal;
        lTileCount : longint;
        szFile : array[0..260-1] of shortint;
       end;
       PDESKTOP = ^DESKTOP;
{$PACKRECORDS 1}
       CMDMSG = record
        cmd : word;
        unused : word;
        source : word;
        fMouse : word;
       end;
       PCMDMSG = ^CMDMSG;
       MSEMSG = record
        x : integer;
        y : integer;
        codeHitTest : word;
        fsInp : word;
       end;
       PMSEMSG = ^MSEMSG;
       CHRMSG = record
        fs : word;
        cRepeat : byte;
        scancode : byte;
        chr : word;
        vkey : word;
       end;
       PCHRMSG = ^CHRMSG;
{$PACKRECORDS NORMAL}

{$PACKRECORDS 2}
    DLGTITEM = record
        fsItemStatus : word;
        cChildren : word;
        cchClassName : word;
        offClassName : word;
        cchText : word;
        offText : word;
        flStyle : cardinal;
        x : integer;
        y : integer;
        cx : integer;
        cy : integer;
        id : word;
        offPresParams : word;
        offCtlData : word;
       end;
       PDLGTITEM = ^DLGTITEM;
       DLGTEMPLATE = record
        cbTemplate : word;
        _type : word;
        codepage : word;
        offadlgti : word;
        fsTemplateStatus : word;
        iItemFocus : word;
        coffPresParams : word;
        adlgti : array[0..0] of DLGTITEM;
       end;
       PDLGTEMPLATE = ^DLGTEMPLATE;
       BTNCDATA = record
        cb : word;
        fsCheckState : word;
        fsHiliteState : word;
        hImage : cardinal;
       end;
       PBTNCDATA = ^BTNCDATA;
       ENTRYFDATA = record
        cb : word;
        cchEditLimit : word;
        ichMinSel : word;
        ichMaxSel : word;
       end;
       PENTRYFDATA = ^ENTRYFDATA;
       MENUITEM = record
        iPosition : integer;
        afStyle : word;
        afAttribute : word;
        id : word;
        hwndSubMenu : cardinal;
        hItem : cardinal;
       end;
       PMENUITEM = ^MENUITEM;
       SBCDATA = record
        cb : word;
        sHilite : word;
        posFirst : integer;
        posLast : integer;
        posThumb : integer;
        cVisible : integer;
        cTotal : integer;
       end;
       PSBCDATA = ^SBCDATA;
       FRAMECDATA = record
        cb : word;
        flCreateFlags : cardinal;
        hmodResources : word;
        idResources : word;
       end;
       PFRAMECDATA = ^FRAMECDATA;
       ACCEL = record
        fs : word;
        key : word;
        cmd : word;
       end;
       PACCEL = ^ACCEL;
       ACCELTABLE = record
        cAccel : word;
        codepage : word;
        aaccel : array[0..1-1] of ACCEL;
       end;
       PACCELTABLE = ^ACCELTABLE;
       MFP = record
        sizeBounds : POINTL;
        sizeMM : POINTL;
        cbLength : cardinal;
        mapMode : word;
        reserved : word;
        abData : array[0..1-1] of BYTE;
       end;
       PMFP = ^MFP;
       CPTEXT = record
        idCountry : word;
        usCodepage : word;
        usLangID : word;
        usSubLangID : word;
        abText : array[0..1-1] of BYTE;
       end;
       PCPTEXT = ^CPTEXT;

(* Type definitions for WinMessageBox2 *)
  MB2D = record
    achText: array [0..MaxMB2DText] of char;
    idButtons: cardinal;
    flStyle: cardinal;
  end;
  TMB2D = MB2D;
  PMB2D = ^TMB2D;

  MB2Info = record
    cb: cardinal;                   (* size of data              *)
    hIcon: cardinal;                (* icon handle               *)
    cButtons: cardinal;             (* number of buttons         *)
    flStyle: cardinal;              (* icon style flags          *)
    hwndNotify: cardinal;           (* owner notification handle *)
    MB2D: array [0..0] of TMB2D;    (* button definitions        *)
  end;
  TMB2Info = MB2Info;
  PMB2Info = ^TMB2Info;

//***************************************************************************\
//*  FontRangeEntry
//*
//*     ulRun         = number of consecutive glyphs contained in the font
//*     ulSkip        = number of consecutive glyphs skipped in the font,
//*                     ulSkip == 0 --> Last FontRangeEntry in table
//***************************************************************************/
type
  FONTRANGEENTRY=record       // fre
    ulRun: Cardinal;
    ulSkip: Cardinal;
  end;
  PFONTRANGEENTRY=^FONTRANGEENTRY;

//***************************************************************************\
//*  FontCharDef
//*
//*     ulGlyphOffset = offset to rendered character bitmap (0 from driver)
//*     sAspace       = pre-character space
//*     sBspace       = character width (always non-zero)
//*     sCspace       = post-character space
//***************************************************************************/
type
  FONTCHARDEF=record          // fcd
    ulGlyphOffset: Cardinal;
    sAspace: Integer;
    sBspace: Word;
    sCspace: integer;
  end;
  PFONTCHARDEF=^FONTCHARDEF;

//***************************************************************************\
//*  FocaMetricsExtension
//***************************************************************************/
type
  FOCAMETRICSEXT=record // fme
    ulSize: Cardinal;            // Total size of extension
    ulFlags: Cardinal;           // Reserved, must be 0
    ulGlyphCount: Cardinal;
    ulDefaultIndex: Cardinal;
    ulRangeTableEntries: Cardinal;
    afreRangeTable: Array[1..1] of FONTRANGEENTRY;
  end;
  PFOCAMETRICSEXT=^FOCAMETRICSEXT;

//**************************************************************************
type
  FOCAMETRICS=record    // foca
    ulIdentity: Cardinal;
    ulSize: Cardinal;
    szFamilyname: Array[1..32] of Char;
    szFacename: Array[1..32] of Char;
    usRegistryId: Integer;
    usCodePage: Integer;
    yEmHeight: Integer;
    yXHeight: Integer;
    yMaxAscender: Integer;
    yMaxDescender: Integer;
    yLowerCaseAscent: Integer;
    yLowerCaseDescent: Integer;
    yInternalLeading: Integer;
    yExternalLeading: Integer;
    xAveCharWidth: Integer;
    xMaxCharInc: Integer;
    xEmInc: Integer;
    yMaxBaselineExt: Integer;
    sCharSlope: Integer;
    sInlineDir: Integer;
    sCharRot: Integer;
    usWeightClass: Word;
    usWidthClass: Word;
    xDeviceRes: Integer;
    yDeviceRes: Integer;
    usFirstChar: Integer;
    usLastChar: Integer;
    usDefaultChar: Integer;
    usBreakChar: Integer;
    usNominalPointSize: Integer;
    usMinimumPointSize: Integer;
    usMaximumPointSize: Integer;
    fsTypeFlags: Integer;
    fsDefn: Integer;
    fsSelectionFlags: Integer;
    fsCapabilities: Integer;
    ySubscriptXSize: Integer;
    ySubscriptYSize: Integer;
    ySubscriptXOffset: Integer;
    ySubscriptYOffset: Integer;
    ySuperscriptXSize: Integer;
    ySuperscriptYSize: Integer;
    ySuperscriptXOffset: Integer;
    ySuperscriptYOffset: Integer;
    yUnderscoreSize: Integer;
    yUnderscorePosition: Integer;
    yStrikeoutSize: Integer;
    yStrikeoutPosition: Integer;
    usKerningPairs: Integer;
    sFamilyClass: Integer;
    pszDeviceNameOffset: PChar;
  end;
  PFOCAMETRICS=^FOCAMETRICS;

// REUSE - long offset to extension relative to FocaMetrics
{$define loffExtension:=pszDeviceNameOffset}

type
  FONTFILEMETRICS=record   // ffm
    ulIdentity: Cardinal;
    ulSize: Cardinal;
    szFamilyname: Array[0..32-1] of Char;
    szFacename: Array[0..32-1] of Char;
    usRegistryId: Integer;
    usCodePage: Integer;
    yEmHeight: Integer;
    yXHeight: Integer;
    yMaxAscender: Integer;
    yMaxDescender: Integer;
    yLowerCaseAscent: Integer;
    yLowerCaseDescent: Integer;
    yInternalLeading: Integer;
    yExternalLeading: Integer;
    xAveCharWidth: Integer;
    xMaxCharInc: Integer;
    xEmInc: Integer;
    yMaxBaselineExt: Integer;
    sCharSlope: Integer;
    sInlineDir: Integer;
    sCharRot: Integer;
    usWeightClass: Word;
    usWidthClass: Word;
    xDeviceRes: Integer;
    yDeviceRes: Integer;
    usFirstChar: Integer;
    usLastChar: Integer;
    usDefaultChar: Integer;
    usBreakChar: Integer;
    usNominalPointSize: Integer;
    usMinimumPointSize: Integer;
    usMaximumPointSize: Integer;
    fsTypeFlags: Integer;
    fsDefn: Integer;
    fsSelectionFlags: Integer;
    fsCapabilities: Integer;
    ySubscriptXSize: Integer;
    ySubscriptYSize: Integer;
    ySubscriptXOffset: Integer;
    ySubscriptYOffset: Integer;
    ySuperscriptXSize: Integer;
    ySuperscriptYSize: Integer;
    ySuperscriptXOffset: Integer;
    ySuperscriptYOffset: Integer;
    yUnderscoreSize: Integer;
    yUnderscorePosition: Integer;
    yStrikeoutSize: Integer;
    yStrikeoutPosition: Integer;
    usKerningPairs: Integer;
    sFamilyClass: Integer;
    ulReserved: Cardinal;
    anose: PANOSE;
  end;
  PFONTFILEMETRICS=^FONTFILEMETRICS;

  FONTDEFINITIONHEADER=record    // fdh
    ulIdentity: Cardinal;
    ulSize: Cardinal;
    fsFontdef: Integer;
    fsChardef: Integer;
    usCellSize: Integer;
    xCellWidth: Integer;
    yCellHeight: Integer;
    xCellIncrement: Integer;
    xCellA: Integer;
    xCellB: Integer;
    xCellC: Integer;
    pCellBaseOffset: Integer;
  end;
  PFONTDEFINITIONHEADER=^FONTDEFINITIONHEADER;

const
  FONTDEFFONT1     =$0047; // set width, height, inc. & base offset
  FONTDEFFONT2     =$0042; // set height & base offset
  FONTDEFFONT3     =$0042; // set height & base offset
  FONTDEFCHAR1     =$0081; // set char offset and width
  FONTDEFCHAR2     =$0081; // set char offset and width
  FONTDEFCHAR3     =$00b8; // set char offset, A, B, and C space
  SPACE_UNDEF      =$8000; // space undefined = take default
  FONTDEFFOCA32    =$4000;
  FONTDEFDEVFONT   =$2000; // Device or Downloadable font

type
  FONTSIGNATURE=record    // fs
    ulIdentity: Cardinal;
    ulSize: Cardinal;
    achSignature: Array[0..12-1] of Char;
  end;
  PFONTSIGNATURE=^FONTSIGNATURE;

  ADDITIONALMETRICS=record    // am
    ulIdentity: Cardinal;
    ulSize: Cardinal;
    anose: PANOSE;
  end;
  PADDITIONALMETRICS=^ADDITIONALMETRICS;

  FOCAFONT=record    // ff
    fsSignature: FONTSIGNATURE;
    fmMetrics: FOCAMETRICS;
    fdDefinitions: FONTDEFINITIONHEADER;
  end;
  PFOCAFONT=^FOCAFONT;

const
  FONT_SIGNATURE          =$fffffffe;// Identity header start
  FONT_METRICS            =$00000001;// Identity metrics
  FONT_DEFINITION         =$00000002;// Identity definition
  FONT_KERNPAIRS          =$00000003;// Identity Kern Pairs
  FONT_ADDITIONALMETRICS  =$00000004;// Identity Additional Metrics
  FONT_ENDRECORD          =$ffffffff;// Identity record end

type
  FOCAFONT32=FOCAFONT;
  PFOCAFONT32=^FOCAFONT32;

// Options for QueryFonts
const
  QUERY_PUBLIC_FONTS      =$0001;
  QUERY_PRIVATE_FONTS     =$0002;

  CDEF_GENERIC            =$0001;
  CDEF_BOLD               =$0002;
  CDEF_ITALIC             =$0004;
  CDEF_UNDERSCORE         =$0008;
  CDEF_STRIKEOUT          =$0010;
  CDEF_OUTLINE            =$0020;

const
  //*************************************************************************
  //* MLE Window styles ( in addition to WS_* )
  //*************************************************************************/
  MLS_WORDWRAP             = $00000001;
  MLS_BORDER               = $00000002;
  MLS_VSCROLL              = $00000004;
  MLS_HSCROLL              = $00000008;
  MLS_READONLY             = $00000010;
  MLS_IGNORETAB            = $00000020;
  MLS_DISABLEUNDO          = $00000040;
  MLS_LIMITVSCROLL         = $00000080;

  //*************************************************************************
  //* MLE External Data Types
  //*************************************************************************/
type
   IPT=Longint;        // insertion point
   PIPT=^IPT;          // insertion point
   PIX=Longint;        // pixel
   LINE=Cardinal;      // Line number

   FORMATRECT=record   // MLEFRD
     cxFormat: Longint;           // format rectangle width
     cyFormat: Longint;           // format rectangle height
   end;
   PMLEFORMATRECT=^FORMATRECT;

   MLECTLDATA=record    // MLECTL
     cbCtlData: Word;          // Length of the MLECTLDATA structure
     afIEFormat: Word;         // import/export format
     cchText: Cardinal;        // text limit
     iptAnchor: IPT;           // beginning of selection
     iptCursor: IPT;           // ending of selection
     cxFormat: Longint;        // format rectangle width
     cyFormat: Longint;        // format rectangle height
     afFormatFlags: Cardinal;  // formatting rectangle flags
     pHWXCtlData: Pointer;     // reserved for Pen CtlData (penpm.h)
   end;
   PMLECTLDATA=^MLECTLDATA;

  //*************************************************************************
  //* afFormatFlags mask
  //*************************************************************************/
const
  MLFFMTRECT_LIMITHORZ       =$00000001;
  MLFFMTRECT_LIMITVERT       =$00000002;
  MLFFMTRECT_MATCHWINDOW     =$00000004;
  MLFFMTRECT_FORMATRECT      =$00000007;

  //************************************************************************
  //* afIEFormat - Import/Export Format flags
  //************************************************************************
  MLFIE_CFTEXT             =  0;
  MLFIE_NOTRANS            =  1;
  MLFIE_WINFMT             =  2;
  MLFIE_RTF                =  3;

  //*************************************************************************
  //* MLE color types: MLM_QUERY(TEXT/BACK)COLOR, MLM_SET(TEXT/BACK)COLOR
  //*************************************************************************/
  MLE_INDEX = 0;
  MLE_RGB   = 1;

  //*************************************************************************
  //* MLN_OVERFLOW structure
  //*************************************************************************/
type
  MLEOVERFLOW=record    // overflow
    afErrInd: Cardinal;            // see mask below
    nBytesOver: Longint;           // number of bytes overflowed
    pixHorzOver: Longint;          // number of pixels horizontally overflow
    pixVertOver: Longint;          // number of pixels vertically overflowed
  end;
  POVERFLOW=^MLEOVERFLOW;

  //*************************************************************************
  //* afErrInd - error format rectangle flags
  //*************************************************************************/
const
  MLFEFR_RESIZE            =  $00000001;
  MLFEFR_TABSTOP           =  $00000002;
  MLFEFR_FONT              =  $00000004;
  MLFEFR_TEXT              =  $00000008;
  MLFEFR_WORDWRAP          =  $00000010;
  MLFETL_TEXTBYTES         =  $00000020;

  //*************************************************************************
  //* MLN_MARGIN structure
  //*************************************************************************/
type
  MLEMARGSTRUCT=record    // margin
    afMargins: Word;      // margin indicator
    usMouMsg: Word;       // mouse message
    iptNear: IPT;         // the geometrically nearest insertion point
  end;
  PMARGSTRUCT=^MLEMARGSTRUCT;

  //*************************************************************************
  //* afFlags - margin notification indicators
  //*************************************************************************/
const
  MLFMARGIN_LEFT             =$0001;
  MLFMARGIN_BOTTOM           =$0002;
  MLFMARGIN_RIGHT            =$0003;
  MLFMARGIN_TOP              =$0004;

  //*************************************************************************
  // MLM_QUERYSELECTION flags
  //************************************************************************/
  MLFQS_MINMAXSEL           = 0;
  MLFQS_MINSEL              = 1;
  MLFQS_MAXSEL              = 2;
  MLFQS_ANCHORSEL           = 3;
  MLFQS_CURSORSEL           = 4;

  //*************************************************************************
  //* MLN_CLPBDFAIL flags
  //*************************************************************************/
  MLFCLPBD_TOOMUCHTEXT       =$00000001;
  MLFCLPBD_ERROR             =$00000002;

  //*************************************************************************
  //* MLM_SEARCH structure
  //*************************************************************************/
type
  MLE_SEARCHDATA=record // search
    cb: Word;             // size of search spec structure
    pchFind: PChar;       // string to search for
    pchReplace: PChar;    // string to replace with
    cchFind: Integer;     // length of pchFindString
    cchReplace: Integer;  // length of replace string
    iptStart: IPT;        // point at which to start search
      // (negative indicates cursor pt)
      // becomes pt where string found
    iptStop: IPT;         // point at which to stop search
      // (negative indicates EOT)
    cchFound: Word;       // Length of found string at iptStart
  end;
  PMLE_SEARCHDATA=^MLE_SEARCHDATA;

  //*************************************************************************
  //* MLM_SEARCH style flags
  //*************************************************************************/
const
  MLFSEARCH_CASESENSITIVE    =$00000001;
  MLFSEARCH_SELECTMATCH      =$00000002;
  MLFSEARCH_CHANGEALL        =$00000004;

  //*************************************************************************
  //* MLE messages - MLM from 0x01b0 to 0x01de; MLN from 0x0001 to 0x000f
  //*************************************************************************/
   // formatting messages
  MLM_SETTEXTLIMIT           =$01b0;
  MLM_QUERYTEXTLIMIT         =$01b1;
  MLM_SETFORMATRECT          =$01b2;
  MLM_QUERYFORMATRECT        =$01b3;
  MLM_SETWRAP                =$01b4;
  MLM_QUERYWRAP              =$01b5;
  MLM_SETTABSTOP             =$01b6;
  MLM_QUERYTABSTOP           =$01b7;
  MLM_SETREADONLY            =$01b8;
  MLM_QUERYREADONLY          =$01b9;

  // text content manipulation and queries messages
  MLM_QUERYCHANGED           =$01ba;
  MLM_SETCHANGED             =$01bb;
  MLM_QUERYLINECOUNT         =$01bc;
  MLM_CHARFROMLINE           =$01bd;
  MLM_LINEFROMCHAR           =$01be;
  MLM_QUERYLINELENGTH        =$01bf;
  MLM_QUERYTEXTLENGTH        =$01c0;

  // text import and export messages
  MLM_FORMAT                 =$01c1;
  MLM_SETIMPORTEXPORT        =$01c2;
  MLM_IMPORT                 =$01c3;
  MLM_EXPORT                 =$01c4;
  MLM_DELETE                 =$01c6;
  MLM_QUERYFORMATLINELENGTH  =$01c7;
  MLM_QUERYFORMATTEXTLENGTH  =$01c8;
  MLM_INSERT                 =$01c9;

  // selection messages
  MLM_SETSEL                 =$01ca;
  MLM_QUERYSEL               =$01cb;
  MLM_QUERYSELTEXT           =$01cc;

  // undo and redo messages
  MLM_QUERYUNDO              =$01cd;
  MLM_UNDO                   =$01ce;
  MLM_RESETUNDO              =$01cf;

  // text attributes messages
  MLM_QUERYFONT              =$01d0;
  MLM_SETFONT                =$01d1;
  MLM_SETTEXTCOLOR           =$01d2;
  MLM_QUERYTEXTCOLOR         =$01d3;
  MLM_SETBACKCOLOR           =$01d4;
  MLM_QUERYBACKCOLOR         =$01d5;

  // scrolling messages
  MLM_QUERYFIRSTCHAR         =$01d6;
  MLM_SETFIRSTCHAR           =$01d7;

  // clipboard messages
  MLM_CUT                    =$01d8;
  MLM_COPY                   =$01d9;
  MLM_PASTE                  =$01da;
  MLM_CLEAR                  =$01db;

  // display manipulation messages
  MLM_ENABLEREFRESH          =$01dc;
  MLM_DISABLEREFRESH         =$01dd;

  // search message
  MLM_SEARCH                 =$01de;
  MLM_QUERYIMPORTEXPORT      =$01df;

  // notification messages
  MLN_OVERFLOW               =$0001;
  MLN_PIXHORZOVERFLOW        =$0002;
  MLN_PIXVERTOVERFLOW        =$0003;
  MLN_TEXTOVERFLOW           =$0004;
  MLN_VSCROLL                =$0005;
  MLN_HSCROLL                =$0006;
  MLN_CHANGE                 =$0007;
  MLN_SETFOCUS               =$0008;
  MLN_KILLFOCUS              =$0009;
  MLN_MARGIN                 =$000a;
  MLN_SEARCHPAUSE            =$000b;
  MLN_MEMERROR               =$000c;
  MLN_UNDOOVERFLOW           =$000d;
  MLN_CLPBDFAIL              =$000f;


const
  DTYP_USER              =(16384);

  DTYP_CTL_ARRAY         =(1);
  DTYP_CTL_PARRAY        =(-1);
  DTYP_CTL_OFFSET        =(2);
  DTYP_CTL_LENGTH        =(3);

//**********************************************************************/
//* Ordinary datatypes                                                 */
//**********************************************************************/
  DTYP_ACCEL             =(28);
  DTYP_ACCELTABLE        =(29);
  DTYP_ARCPARAMS         =(38);
  DTYP_AREABUNDLE        =(139);
  DTYP_ATOM              =(90);
  DTYP_BITMAPINFO        =(60);
  DTYP_BITMAPINFOHEADER  =(61);
  DTYP_BITMAPINFO2       =(170);
  DTYP_BITMAPINFOHEADER2 =(171);
  DTYP_BIT16             =(20);
  DTYP_BIT32             =(21);
  DTYP_BIT8              =(19);
  DTYP_BOOL              =(18);
  DTYP_BTNCDATA          =(35);
  DTYP_BYTE              =(13);
  DTYP_CATCHBUF          =(141);
  DTYP_CHAR              =(15);
  DTYP_CHARBUNDLE        =(135);
  DTYP_CLASSINFO         =(95);
  DTYP_COUNT2            =(93);
  DTYP_COUNT2B           =(70);
  DTYP_COUNT2CH          =(82);
  DTYP_COUNT4            =(152);
  DTYP_COUNT4B           =(42);
  DTYP_CPID              =(57);
  DTYP_CREATESTRUCT      =(98);
  DTYP_CURSORINFO        =(34);
  DTYP_DEVOPENSTRUC      =(124);
  DTYP_DLGTEMPLATE       =(96);
  DTYP_DLGTITEM          =(97);
  DTYP_ENTRYFDATA        =(127);
  DTYP_ERRORID           =(45);
  DTYP_FATTRS            =(75);
  DTYP_FFDESCS           =(142);
  DTYP_FIXED             =(99);
  DTYP_FONTMETRICS       =(74);
  DTYP_FRAMECDATA        =(144);
  DTYP_GRADIENTL         =(48);
  DTYP_HAB               =(10);
  DTYP_HACCEL            =(30);
  DTYP_HAPP              =(146);
  DTYP_HATOMTBL          =(91);
  DTYP_HBITMAP           =(62);
  DTYP_HCINFO            =(46);
  DTYP_HDC               =(132);
  DTYP_HENUM             =(117);
  DTYP_HHEAP             =(109);
  DTYP_HINI              =(53);
  DTYP_HLIB              =(147);
  DTYP_HMF               =(85);
  DTYP_HMQ               =(86);
  DTYP_HPOINTER          =(106);
  DTYP_HPROGRAM          =(131);
  DTYP_HPS               =(12);
  DTYP_HRGN              =(116);
  DTYP_HSEM              =(140);
  DTYP_HSPL              =(32);
  DTYP_HSWITCH           =(66);
  DTYP_HVPS              =(58);
  DTYP_HWND              =(11);
  DTYP_IDENTITY          =(133);
  DTYP_IDENTITY4         =(169);
  DTYP_IMAGEBUNDLE       =(136);
  DTYP_INDEX2            =(81);
  DTYP_IPT               =(155);
  DTYP_KERNINGPAIRS      =(118);
  DTYP_LENGTH2           =(68);
  DTYP_LENGTH4           =(69);
  DTYP_LINEBUNDLE        =(137);
  DTYP_LONG              =(25);
  DTYP_MARKERBUNDLE      =(138);
  DTYP_MATRIXLF          =(113);
  DTYP_MLECTLDATA        =(161);
  DTYP_MLEMARGSTRUCT     =(157);
  DTYP_MLEOVERFLOW       =(158);
  DTYP_OFFSET2B          =(112);
  DTYP_OWNERITEM         =(154);
  DTYP_PID               =(92);
  DTYP_PIX               =(156);
  DTYP_POINTERINFO       =(105);
  DTYP_POINTL            =(77);
  DTYP_PROGCATEGORY      =(129);
  DTYP_PROGRAMENTRY      =(128);
  DTYP_PROGTYPE          =(130);
  DTYP_PROPERTY2         =(88);
  DTYP_PROPERTY4         =(89);
  DTYP_QMSG              =(87);
  DTYP_RECTL             =(121);
  DTYP_RESID             =(125);
  DTYP_RGB               =(111);
  DTYP_RGNRECT           =(115);
  DTYP_SBCDATA           =(159);
  DTYP_SEGOFF            =(126);
  DTYP_SHORT             =(23);
  DTYP_SIZEF             =(101);
  DTYP_SIZEL             =(102);
  DTYP_STRL              =(17);
  DTYP_STR16             =(40);
  DTYP_STR32             =(37);
  DTYP_STR64             =(47);
  DTYP_STR8              =(33);
  DTYP_SWBLOCK           =(63);
  DTYP_SWCNTRL           =(64);
  DTYP_SWENTRY           =(65);
  DTYP_SWP               =(31);
  DTYP_TID               =(104);
  DTYP_TIME              =(107);
  DTYP_TRACKINFO         =(73);
  DTYP_UCHAR             =(22);
  DTYP_ULONG             =(26);
  DTYP_USERBUTTON        =(36);
  DTYP_USHORT            =(24);
  DTYP_WIDTH4            =(108);
  DTYP_WNDPARAMS         =(83);
  DTYP_WNDPROC           =(84);
  DTYP_WPOINT            =(59);
  DTYP_WRECT             =(55);
  DTYP_XYWINSIZE         =(52);


//**********************************************************************/
//* Pointer datatypes                                                  */
//**********************************************************************/
  DTYP_PACCEL            =(-28);
  DTYP_PACCELTABLE       =(-29);
  DTYP_PARCPARAMS        =(-38);
  DTYP_PAREABUNDLE       =(-139);
  DTYP_PATOM             =(-90);
  DTYP_PBITMAPINFO       =(-60);
  DTYP_PBITMAPINFOHEADER =(-61);
  DTYP_PBITMAPINFO2      =(-170);
  DTYP_PBITMAPINFOHEADER2=(-171);
  DTYP_PBIT16            =(-20);
  DTYP_PBIT32            =(-21);
  DTYP_PBIT8             =(-19);
  DTYP_PBOOL             =(-18);
  DTYP_PBTNCDATA         =(-35);
  DTYP_PBYTE             =(-13);
  DTYP_PCATCHBUF         =(-141);
  DTYP_PCHAR             =(-15);
  DTYP_PCHARBUNDLE       =(-135);
  DTYP_PCLASSINFO        =(-95);
  DTYP_PCOUNT2           =(-93);
  DTYP_PCOUNT2B          =(-70);
  DTYP_PCOUNT2CH         =(-82);
  DTYP_PCOUNT4           =(-152);
  DTYP_PCOUNT4B          =(-42);
  DTYP_PCPID             =(-57);
  DTYP_PCREATESTRUCT     =(-98);
  DTYP_PCURSORINFO       =(-34);
  DTYP_PDEVOPENSTRUC     =(-124);
  DTYP_PDLGTEMPLATE      =(-96);
  DTYP_PDLGTITEM         =(-97);
  DTYP_PENTRYFDATA       =(-127);
  DTYP_PERRORID          =(-45);
  DTYP_PFATTRS           =(-75);
  DTYP_PFFDESCS          =(-142);
  DTYP_PFIXED            =(-99);
  DTYP_PFONTMETRICS      =(-74);
  DTYP_PFRAMECDATA       =(-144);
  DTYP_PGRADIENTL        =(-48);
  DTYP_PHAB              =(-10);
  DTYP_PHACCEL           =(-30);
  DTYP_PHAPP             =(-146);
  DTYP_PHATOMTBL         =(-91);
  DTYP_PHBITMAP          =(-62);
  DTYP_PHCINFO           =(-46);
  DTYP_PHDC              =(-132);
  DTYP_PHENUM            =(-117);
  DTYP_PHHEAP            =(-109);
  DTYP_PHINI             =(-53);
  DTYP_PHLIB             =(-147);
  DTYP_PHMF              =(-85);
  DTYP_PHMQ              =(-86);
  DTYP_PHPOINTER         =(-106);
  DTYP_PHPROGRAM         =(-131);
  DTYP_PHPS              =(-12);
  DTYP_PHRGN             =(-116);
  DTYP_PHSEM             =(-140);
  DTYP_PHSPL             =(-32);
  DTYP_PHSWITCH          =(-66);
  DTYP_PHVPS             =(-58);
  DTYP_PHWND             =(-11);
  DTYP_PIDENTITY         =(-133);
  DTYP_PIDENTITY4        =(-169);
  DTYP_PIMAGEBUNDLE      =(-136);
  DTYP_PINDEX2           =(-81);
  DTYP_PIPT              =(-155);
  DTYP_PKERNINGPAIRS     =(-118);
  DTYP_PLENGTH2          =(-68);
  DTYP_PLENGTH4          =(-69);
  DTYP_PLINEBUNDLE       =(-137);
  DTYP_PLONG             =(-25);
  DTYP_PMARKERBUNDLE     =(-138);
  DTYP_PMATRIXLF         =(-113);
  DTYP_PMLECTLDATA       =(-161);
  DTYP_PMLEMARGSTRUCT    =(-157);
  DTYP_PMLEOVERFLOW      =(-158);
  DTYP_POFFSET2B         =(-112);
  DTYP_POWNERITEM        =(-154);
  DTYP_PPID              =(-92);
  DTYP_PPIX              =(-156);
  DTYP_PPOINTERINFO      =(-105);
  DTYP_PPOINTL           =(-77);
  DTYP_PPROGCATEGORY     =(-129);
  DTYP_PPROGRAMENTRY     =(-128);
  DTYP_PPROGTYPE         =(-130);
  DTYP_PPROPERTY2        =(-88);
  DTYP_PPROPERTY4        =(-89);
  DTYP_PQMSG             =(-87);
  DTYP_PRECTL            =(-121);
  DTYP_PRESID            =(-125);
  DTYP_PRGB              =(-111);
  DTYP_PRGNRECT          =(-115);
  DTYP_PSBCDATA          =(-159);
  DTYP_PSEGOFF           =(-126);
  DTYP_PSHORT            =(-23);
  DTYP_PSIZEF            =(-101);
  DTYP_PSIZEL            =(-102);
  DTYP_PSTRL             =(-17);
  DTYP_PSTR16            =(-40);
  DTYP_PSTR32            =(-37);
  DTYP_PSTR64            =(-47);
  DTYP_PSTR8             =(-33);
  DTYP_PSWBLOCK          =(-63);
  DTYP_PSWCNTRL          =(-64);
  DTYP_PSWENTRY          =(-65);
  DTYP_PSWP              =(-31);
  DTYP_PTID              =(-104);
  DTYP_PTIME             =(-107);
  DTYP_PTRACKINFO        =(-73);
  DTYP_PUCHAR            =(-22);
  DTYP_PULONG            =(-26);
  DTYP_PUSERBUTTON       =(-36);
  DTYP_PUSHORT           =(-24);
  DTYP_PWIDTH4           =(-108);
  DTYP_PWNDPARAMS        =(-83);
  DTYP_PWNDPROC          =(-84);
  DTYP_PWPOINT           =(-59);
  DTYP_PWRECT            =(-55);
  DTYP_PXYWINSIZE        =(-52);

{$PACKRECORDS NORMAL}

{Names beginning with T for compatibility}
type
      TQVERSDATA = QVERSDATA;
      TSWP = SWP;
      TCREATESTRUCT = CREATESTRUCT;
      TCLASSINFO = CLASSINFO;
      TQMSG = QMSG;
      TMQINFO = MQINFO;
      TWNDPARAMS = WNDPARAMS;
      TUSERBUTTON = USERBUTTON;
      TOWNERITEM = OWNERITEM;
      TPARAM = PARAM;
      TPRESPARAMS = PRESPARAMS;
      TTRACKINFO = TRACKINFO;
      TCURSORINFO = CURSORINFO;
      TPOINTERINFO = POINTERINFO;
      TSMHSTRUCT = SMHSTRUCT;
      TERRINFO = ERRINFO;
      TCONVCONTEXT = CONVCONTEXT;
      TDDEINIT = DDEINIT;
      TDDESTRUCT = DDESTRUCT;
      TDESKTOP = DESKTOP;
      TCMDMSG = CMDMSG;
      TMSEMSG = MSEMSG;
      TCHRMSG = CHRMSG;
      TDLGTITEM = DLGTITEM;
      TDLGTEMPLATE = DLGTEMPLATE;
      TBTNCDATA = BTNCDATA;
      TENTRYFDATA = ENTRYFDATA;
      TMENUITEM = MENUITEM;
      TSBCDATA = SBCDATA;
      TFRAMECDATA = FRAMECDATA;
      TACCEL = ACCEL;
      TACCELTABLE = ACCELTABLE;
      TMPF = MFP;
      TCPTEXT = CPTEXT;
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

       PSF_LOCKWINDOWUPDATE = $0001;
       PSF_CLIPUPWARDS = $0002;
       PSF_CLIPDOWNWARDS = $0004;
       PSF_CLIPSIBLINGS = $0008;
       PSF_CLIPCHILDREN = $0010;
       PSF_PARENTCLIP = $0020;
       SW_SCROLLCHILDREN = $0001;
       SW_INVALIDATERGN = $0002;

       QV_OS2 = $0000;
       QV_CMS = $0001;
       QV_TSO = $0002;
       QV_TSOBATCH = $0003;
       QV_OS400 = $0004;

       QW_NEXT = 0;
       QW_PREV = 1;
       QW_TOP = 2;
       QW_BOTTOM = 3;
       QW_OWNER = 4;
       QW_PARENT = 5;
       QW_NEXTTOP = 6;
       QW_PREVTOP = 7;
       QW_FRAMEOWNER = 8;

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

       DBM_NORMAL = $0000;
       DBM_INVERT = $0001;
       DBM_HALFTONE = $0002;
       DBM_STRETCH = $0004;
       DBM_IMAGEATTRS = $0008;

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

       DB_PATCOPY = $0000;
       DB_PATINVERT = $0001;
       DB_DESTINVERT = $0002;
       DB_AREAMIXMODE = $0003;
       DB_ROP = $0007;
       DB_INTERIOR = $0008;
       DB_AREAATTRS = $0010;
       DB_STANDARD = $0100;
       DB_DLGBORDER = $0200;

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

       WPM_TEXT = $0001;
       WPM_CTLDATA = $0002;
       WPM_PRESPARAMS = $0004;
       WPM_CCHTEXT = $0008;
       WPM_CBCTLDATA = $0010;
       WPM_CBPRESPARAMS = $0020;

       BMSG_POST = $0000;
       BMSG_SEND = $0001;
       BMSG_POSTQUEUE = $0002;
       BMSG_DESCENDANTS = $0004;
       BMSG_FRAMEONLY = $0008;

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

       SMIM_ALL = $0EFF;
       SMI_NOINTEREST = $0001;
       SMI_INTEREST = $0002;
       SMI_RESET = $0004;
       SMI_AUTODISPATCH = $0008;

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

       WM_JOURNALNOTIFY = $007c;
       JRN_QUEUESTATUS = $00000001;
       JRN_PHYSKEYSTATE = $00000002;

       DID_OK = 1;
       DID_CANCEL = 2;
       DID_ERROR = $ffff;

       WA_WARNING = 0;
       WA_NOTE = 1;
       WA_ERROR = 2;
       WA_CWINALARMS = 3;

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

       EDI_FIRSTTABITEM = 0;
       EDI_LASTTABITEM = 1;
       EDI_NEXTTABITEM = 2;
       EDI_PREVTABITEM = 3;
       EDI_FIRSTGROUPITEM = 4;
       EDI_LASTGROUPITEM = 5;
       EDI_NEXTGROUPITEM = 6;
       EDI_PREVGROUPITEM = 7;

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

// Menu control messages
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
       MM_DELETEITEMBYPOS = $01f1;  //UNDOCUMENTED
       MM_QUERYDEFAULTITEMID = $0431;
       MM_SETDEFAULTITEMID = $0432;

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
       SV_TRACKRectlEVEL = 46;
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

       TID_CURSOR = $ffff;
       TID_SCROLL = $fffe;
       TID_FLASHWINDOW = $fffd;
       TID_USERMAX = $7fff;

       AF_CHAR = $0001;
       AF_VIRTUALKEY = $0002;
       AF_SCANCODE = $0004;
       AF_SHIFT = $0008;
       AF_CONTROL = $0010;
       AF_ALT = $0020;
       AF_LONEKEY = $0040;
       AF_SYSCOMMAND = $0100;
       AF_HELP = $0200;

       EAF_DEFAULTOWNER = $0001;
       EAF_UNCHANGEABLE = $0002;
       EAF_REUSEICON = $0004;

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

       CFI_OWNERFREE = $0001;
       CFI_OWNERDISPLAY = $0002;
       CFI_POINTER = $0400;
       CFI_HANDLE = $0200;

       CURSOR_SOLID = $0000;
       CURSOR_HALFTONE = $0001;
       CURSOR_FRAME = $0002;
       CURSOR_FLASH = $0004;
       CURSOR_SETPOS = $8000;

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

       DP_NORMAL = $0000;
       DP_HALFTONED = $0001;
       DP_INVERTED = $0002;

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

       {WinCompareStrings}
       WCS_ERROR = 0;
       WCS_EQ = 1;
       WCS_LT = 2;
       WCS_GT = 3;

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
       WINDBG_BAD_SENDMSG_cardinal = $102f;
       WINDBG_ABNORMAL_EXIT = $1030;
       WINDBG_INTERNAL_REVISION = $1031;
       WINDBG_INITSYSTEM_FAILED = $1032;
       WINDBG_HATOMTBL_NOT_DESTROYED = $1033;
       WINDBG_WINDOW_UNLOCK_WAIT = $1035;

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

       DDECTXT_CASESENSITIVE = $0001;

       {DDE}
       DDE_FACK = $0001;
       DDE_FBUSY = $0002;
       DDE_FNODATA = $0004;
       DDE_FACKREQ = $0008;
       DDE_FRESPONSE = $0010;
       DDE_NOTPROCESSED = $0020;
       DDE_FRESERVED = $00C0;
       DDE_FAPPSTATUS = $FF00;
       DDEFMT_TEXT = $0001;
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

       SDT_DESTROY = $0001;
       SDT_NOBKGND = $0002;
       SDT_TILE = $0004;
       SDT_SCALE = $0008;
       SDT_PATTERN = $0010;
       SDT_CENTER = $0020;
       SDT_RETAIN = $0040;
       SDT_LOADFILE = $0080;

       STR_DLLNAME = 'keyremap';
       WM_DBCSFIRST = $00b0;
       WM_DBCSLAST = $00cf;

{ Standard Window Classes - for WinCreateWCWindow }
       WC_FRAME            =$ffff0001;
       WC_COMBOBOX         =$ffff0002;
       WC_BUTTON           =$ffff0003;
       WC_MENU             =$ffff0004;
       WC_STATIC           =$ffff0005;
       WC_ENTRYFIELD       =$ffff0006;
       WC_LISTBOX          =$ffff0007;
       WC_SCROLLBAR        =$ffff0008;
       WC_TITLEBAR         =$ffff0009;
       WC_MLE              =$ffff000A;
       { 000B to 000F reserved }
       WC_APPSTAT          =$ffff0010;
       WC_KBDSTAT          =$ffff0011;
       WC_PECIC            =$ffff0012;
       WC_DBE_KKPOPUP      =$ffff0013;
       { 0014 to 001F reserved }
       WC_SPINBUTTON       =$ffff0020;
       { 0021 to 0024 reserved }
       WC_CONTAINER        =$ffff0025;
       WC_SLIDER           =$ffff0026;
       WC_VALUESET         =$ffff0027;
       WC_NOTEBOOK         =$ffff0028;
       { 0029 to 002C used by PEN }
       WC_PENFIRST         =$ffff0029;
       WC_PENLAST          =$ffff002C;
       { 002D to 0030 reserved }
       { 0030 to 003F reserved }
       WC_MMPMFIRST        =$ffff0040;
       WC_MMPMLAST         =$ffff004f;

{ PM error constants }
       PMERR_INVALID_HWND = $1001;
       PMERR_INVALID_HMQ = $1002;
       PMERR_PARAMETER_OUT_OF_RANGE = $1003;
       PMERR_WINDOW_LOCK_UNDERFLOW = $1004;
       PMERR_WINDOW_LOCK_OVERFLOW = $1005;
       PMERR_BAD_WINDOW_LOCK_COUNT = $1006;
       PMERR_WINDOW_NOT_LOCKED = $1007;
       PMERR_INVALID_SELECTOR = $1008;
       PMERR_CALL_FROM_WRONG_THREAD = $1009;
       PMERR_RESOURCE_NOT_FOUND = $100A;
       PMERR_INVALID_STRING_PARM = $100B;
       PMERR_INVALID_HHEAP = $100C;
       PMERR_INVALID_HEAP_POINTER = $100D;
       PMERR_INVALID_HEAP_SIZE_PARM = $100E;
       PMERR_INVALID_HEAP_SIZE = $100F;
       PMERR_INVALID_HEAP_SIZE_WORD = $1010;
       PMERR_HEAP_OUT_OF_MEMORY = $1011;
       PMERR_HEAP_MAX_SIZE_REACHED = $1012;
       PMERR_INVALID_HATOMTBL = $1013;
       PMERR_INVALID_ATOM = $1014;
       PMERR_INVALID_ATOM_NAME = $1015;
       PMERR_INVALID_INTEGER_ATOM = $1016;
       PMERR_ATOM_NAME_NOT_FOUND = $1017;
       PMERR_QUEUE_TOO_LARGE = $1018;
       PMERR_INVALID_FLAG = $1019;
       PMERR_INVALID_HACCEL = $101A;
       PMERR_INVALID_HPTR = $101B;
       PMERR_INVALID_HENUM = $101C;
       PMERR_INVALID_SRC_CODEPAGE = $101D;
       PMERR_INVALID_DST_CODEPAGE = $101E;
       PMERR_UNKNOWN_COMPONENT_ID = $101f;
       PMERR_UNKNOWN_ERROR_CODE = $1020;
       PMERR_SEVERITY_LEVELS = $1021;
       PMERR_INVALID_RESOURCE_FORMAT = $1034;
       PMERR_NO_MSG_QUEUE = $1036;
       PMERR_WIN_DEBUGMSG = $1037;
       PMERR_QUEUE_FULL = $1038;
       PMERR_LIBRARY_LOAD_FAILED = $1039;
       PMERR_PROCEDURE_LOAD_FAILED = $103A;
       PMERR_LIBRARY_DELETE_FAILED = $103B;
       PMERR_PROCEDURE_DELETE_FAILED = $103C;
       PMERR_ARRAY_TOO_LARGE = $103D;
       PMERR_ARRAY_TOO_SMALL = $103E;
       PMERR_DATATYPE_ENTRY_BAD_INDEX = $103F;
       PMERR_DATATYPE_ENTRY_CTL_BAD = $1040;
       PMERR_DATATYPE_ENTRY_CTL_MISS = $1041;
       PMERR_DATATYPE_ENTRY_INVALID = $1042;
       PMERR_DATATYPE_ENTRY_NOT_NUM = $1043;
       PMERR_DATATYPE_ENTRY_NOT_OFF = $1044;
       PMERR_DATATYPE_INVALID = $1045;
       PMERR_DATATYPE_NOT_UNIQUE = $1046;
       PMERR_DATATYPE_TOO_LONG = $1047;
       PMERR_DATATYPE_TOO_SMALL = $1048;
       PMERR_DIRECTION_INVALID = $1049;
       PMERR_INVALID_HAB = $104A;
       PMERR_INVALID_HSTRUCT = $104D;
       PMERR_LENGTH_TOO_SMALL = $104E;
       PMERR_MSGID_TOO_SMALL = $104F;
       PMERR_NO_HANDLE_ALLOC = $1050;
       PMERR_NOT_IN_A_PM_SESSION = $1051;
       PMERR_MSG_QUEUE_ALREADY_EXISTS = $1052;
       PMERR_OLD_RESOURCE = $1055;
       PMERR_INVALID_PIB = $1101;
       PMERR_INSUFF_SPACE_TO_ADD = $1102;
       PMERR_INVALID_GROUP_HANDLE = $1103;
       PMERR_DUPLICATE_TITLE = $1104;
       PMERR_INVALID_TITLE = $1105;
       PMERR_HANDLE_NOT_IN_GROUP = $1107;
       PMERR_INVALID_TARGET_HANDLE = $1106;
       PMERR_INVALID_PATH_STATEMENT = $1108;
       PMERR_NO_PROGRAM_FOUND = $1109;
       PMERR_INVALID_BUFFER_SIZE = $110A;
       PMERR_BUFFER_TOO_SMALL = $110B;
       PMERR_PL_INITIALISATION_FAIL = $110C;
       PMERR_CANT_DESTROY_SYS_GROUP = $110D;
       PMERR_INVALID_TYPE_CHANGE = $110E;
       PMERR_INVALID_PROGRAM_HANDLE = $110F;
       PMERR_NOT_CURRENT_PL_VERSION = $1110;
       PMERR_INVALID_CIRCULAR_REF = $1111;
       PMERR_MEMORY_ALLOCATION_ERR = $1112;
       PMERR_MEMORY_DEALLOCATION_ERR = $1113;
       PMERR_TASK_HEADER_TOO_BIG = $1114;
       PMERR_INVALID_INI_FILE_HANDLE = $1115;
       PMERR_MEMORY_SHARE = $1116;
       PMERR_OPEN_QUEUE = $1117;
       PMERR_CREATE_QUEUE = $1118;
       PMERR_WRITE_QUEUE = $1119;
       PMERR_READ_QUEUE = $111A;
       PMERR_CALL_NOT_EXECUTED = $111B;
       PMERR_UNKNOWN_APIPKT = $111C;
       PMERR_INITHREAD_EXISTS = $111D;
       PMERR_CREATE_THREAD = $111E;
       PMERR_NO_HK_PROFILE_INSTALLED = $111F;
       PMERR_INVALID_DIRECTORY = $1120;
       PMERR_WILDCARD_IN_FILENAME = $1121;
       PMERR_FILENAME_BUFFER_FULL = $1122;
       PMERR_FILENAME_TOO_LONG = $1123;
       PMERR_INI_FILE_IS_SYS_OR_USER = $1124;
       PMERR_BROADCAST_PLMSG = $1125;
       PMERR_190_INIT_DONE = $1126;
       PMERR_HMOD_FOR_PMSHAPI = $1127;
       PMERR_SET_HK_PROFILE = $1128;
       PMERR_API_NOT_ALLOWED = $1129;
       PMERR_INI_STILL_OPEN = $112A;
       PMERR_PROGDETAILS_NOT_IN_INI = $112B;
       PMERR_PIBSTRUCT_NOT_IN_INI = $112C;
       PMERR_INVALID_DISKPROGDETAILS = $112D;
       PMERR_PROGDETAILS_READ_FAILURE = $112E;
       PMERR_PROGDETAILS_WRITE_FAILURE = $112F;
       PMERR_PROGDETAILS_QSIZE_FAILURE = $1130;
       PMERR_INVALID_PROGDETAILS = $1131;
       PMERR_SHEPROFILEHOOK_NOT_FOUND = $1132;
       PMERR_190PLCONVERTED = $1133;
       PMERR_FAILED_TO_CONVERT_INI_PL = $1134;
       PMERR_PMSHAPI_NOT_INITIALISED = $1135;
       PMERR_INVALID_SHELL_API_HOOK_ID = $1136;
       PMERR_DOS_ERROR = $1200;
       PMERR_NO_SPACE = $1201;
       PMERR_INVALID_SWITCH_HANDLE = $1202;
       PMERR_NO_HANDLE = $1203;
       PMERR_INVALID_PROCESS_ID = $1204;
       PMERR_NOT_SHELL = $1205;
       PMERR_INVALID_WINDOW = $1206;
       PMERR_INVALID_POST_MSG = $1207;
       PMERR_INVALID_PARAMETERS = $1208;
       PMERR_INVALID_PROGRAM_TYPE = $1209;
       PMERR_NOT_EXTENDED_FOCUS = $120A;
       PMERR_INVALID_SESSION_ID = $120B;
       PMERR_SMG_INVALID_ICON_FILE = $120C;
       PMERR_SMG_ICON_NOT_CREATED = $120D;
       PMERR_SHL_DEBUG = $120E;
       PMERR_OPENING_INI_FILE = $1301;
       PMERR_INI_FILE_CORRUPT = $1302;
       PMERR_INVALID_PARM = $1303;
       PMERR_NOT_IN_IDX = $1304;
       PMERR_NO_ENTRIES_IN_GROUP = $1305;
       PMERR_INI_WRITE_FAIL = $1306;
       PMERR_IDX_FULL = $1307;
       PMERR_INI_PROTECTED = $1308;
       PMERR_MEMORY_ALLOC = $1309;
       PMERR_INI_INIT_ALREADY_DONE = $130A;
       PMERR_INVALID_INTEGER = $130B;
       PMERR_INVALID_ASCIIZ = $130C;
       PMERR_CAN_NOT_CALL_SPOOLER = $130D;
       PMERR_VALIDATION_REJECTED = PMERR_CAN_NOT_CALL_SPOOLER;
       PMERR_WARNING_WINDOW_NOT_KILLED = $1401;
       PMERR_ERROR_INVALID_WINDOW = $1402;
       PMERR_ALREADY_INITIALIZED = $1403;
       PMERR_MSG_PROG_NO_MOU = $1405;
       PMERR_MSG_PROG_NON_RECOV = $1406;
       PMERR_WINCONV_INVALID_PATH = $1407;
       PMERR_PI_NOT_INITIALISED = $1408;
       PMERR_PL_NOT_INITIALISED = $1409;
       PMERR_NO_TASK_MANAGER = $140A;
       PMERR_SAVE_NOT_IN_PROGRESS = $140B;
       PMERR_NO_STACK_SPACE = $140C;
       PMERR_INVALID_COLR_FIELD = $140d;
       PMERR_INVALID_COLR_VALUE = $140e;
       PMERR_COLR_WRITE = $140f;
       PMERR_TARGET_FILE_EXISTS = $1501;
       PMERR_SOURCE_SAME_AS_TARGET = $1502;
       PMERR_SOURCE_FILE_NOT_FOUND = $1503;
       PMERR_INVALID_NEW_PATH = $1504;
       PMERR_TARGET_FILE_NOT_FOUND = $1505;
       PMERR_INVALID_DRIVE_NUMBER = $1506;
       PMERR_NAME_TOO_LONG = $1507;
       PMERR_NOT_ENOUGH_ROOM_ON_DISK = $1508;
       PMERR_NOT_ENOUGH_MEM = $1509;
       PMERR_LOG_DRV_DOES_NOT_EXIST = $150B;
       PMERR_INVALID_DRIVE = $150C;
       PMERR_ACCESS_DENIED = $150D;
       PMERR_NO_FIRST_SLASH = $150E;
       PMERR_READ_ONLY_FILE = $150F;
       PMERR_GROUP_PROTECTED = $151F;
       PMERR_INVALID_PROGRAM_CATEGORY = $152F;
       PMERR_INVALID_APPL = $1530;
       PMERR_CANNOT_START = $1531;
       PMERR_STARTED_IN_BACKGROUND = $1532;
       PMERR_INVALID_HAPP = $1533;
       PMERR_CANNOT_STOP = $1534;
       PMERR_INTERNAL_ERROR_1 = $1601;
       PMERR_INTERNAL_ERROR_2 = $1602;
       PMERR_INTERNAL_ERROR_3 = $1603;
       PMERR_INTERNAL_ERROR_4 = $1604;
       PMERR_INTERNAL_ERROR_5 = $1605;
       PMERR_INTERNAL_ERROR_6 = $1606;
       PMERR_INTERNAL_ERROR_7 = $1607;
       PMERR_INTERNAL_ERROR_8 = $1608;
       PMERR_INTERNAL_ERROR_9 = $1609;
       PMERR_INTERNAL_ERROR_10 = $160A;
       PMERR_INTERNAL_ERROR_11 = $160B;
       PMERR_INTERNAL_ERROR_12 = $160C;
       PMERR_INTERNAL_ERROR_13 = $160D;
       PMERR_INTERNAL_ERROR_14 = $160E;
       PMERR_INTERNAL_ERROR_15 = $160F;
       PMERR_INTERNAL_ERROR_16 = $1610;
       PMERR_INTERNAL_ERROR_17 = $1611;
       PMERR_INTERNAL_ERROR_18 = $1612;
       PMERR_INTERNAL_ERROR_19 = $1613;
       PMERR_INTERNAL_ERROR_20 = $1614;
       PMERR_INTERNAL_ERROR_21 = $1615;
       PMERR_INTERNAL_ERROR_22 = $1616;
       PMERR_INTERNAL_ERROR_23 = $1617;
       PMERR_INTERNAL_ERROR_24 = $1618;
       PMERR_INTERNAL_ERROR_25 = $1619;
       PMERR_INTERNAL_ERROR_26 = $161A;
       PMERR_INTERNAL_ERROR_27 = $161B;
       PMERR_INTERNAL_ERROR_28 = $161C;
       PMERR_INTERNAL_ERROR_29 = $161D;
       PMERR_INVALID_FREE_MESSAGE_ID = $1630;
       PMERR_FUNCTION_NOT_SUPPORTED = $1641;
       PMERR_INVALID_ARRAY_COUNT = $1642;
       PMERR_INVALID_LENGTH = $1643;
       PMERR_INVALID_BUNDLE_TYPE = $1644;
       PMERR_INVALID_PARAMETER = $1645;
       PMERR_INVALID_NUMBER_OF_PARMS = $1646;
       PMERR_GREATER_THAN_64K = $1647;
       PMERR_INVALID_PARAMETER_TYPE = $1648;
       PMERR_NEGATIVE_STRCOND_DIM = $1649;
       PMERR_INVALID_NUMBER_OF_TYPES = $164A;
       PMERR_INCORRECT_HSTRUCT = $164B;
       PMERR_INVALID_ARRAY_SIZE = $164C;
       PMERR_INVALID_CONTROL_DATATYPE = $164D;
       PMERR_INCOMPLETE_CONTROL_SEQU = $164E;
       PMERR_INVALID_DATATYPE = $164F;
       PMERR_INCORRECT_DATATYPE = $1650;
       PMERR_NOT_SELF_DESCRIBING_DTYP = $1651;
       PMERR_INVALID_CTRL_SEQ_INDEX = $1652;
       PMERR_INVALID_TYPE_FOR_LENGTH = $1653;
       PMERR_INVALID_TYPE_FOR_OFFSET = $1654;
       PMERR_INVALID_TYPE_FOR_MPARAM = $1655;
       PMERR_INVALID_MESSAGE_ID = $1656;
       PMERR_C_LENGTH_TOO_SMALL = $1657;
       PMERR_APPL_STRUCTURE_TOO_SMALL = $1658;
       PMERR_INVALID_ERRORINFO_HANDLE = $1659;
       PMERR_INVALID_CHARACTER_INDEX = $165A;
       WPERR_PROTECTED_CLASS = $1700;
       WPERR_INVALID_CLASS = $1701;
       WPERR_INVALID_SUPERCLASS = $1702;
       WPERR_NO_MEMORY = $1703;
       WPERR_SEMAPHORE_ERROR = $1704;
       WPERR_BUFFER_TOO_SMALL = $1705;
       WPERR_CLSLOADMOD_FAILED = $1706;
       WPERR_CLSPROCADDR_FAILED = $1707;
       WPERR_OBJWORD_LOCATION = $1708;
       WPERR_INVALID_OBJECT = $1709;
       WPERR_MEMORY_CLEANUP = $170A;
       WPERR_INVALID_MODULE = $170B;
       WPERR_INVALID_OLDCLASS = $170C;
       WPERR_INVALID_NEWCLASS = $170D;
       WPERR_NOT_IMMEDIATE_CHILD = $170E;
       WPERR_NOT_WORKPLACE_CLASS = $170F;
       WPERR_CANT_REPLACE_METACLS = $1710;
       WPERR_INI_FILE_WRITE = $1711;
       WPERR_INVALID_FOLDER = $1712;
       WPERR_BUFFER_OVERFLOW = $1713;
       WPERR_OBJECT_NOT_FOUND = $1714;
       WPERR_INVALID_HFIND = $1715;
       WPERR_INVALID_COUNT = $1716;
       WPERR_INVALID_BUFFER = $1717;
       WPERR_ALREADY_EXISTS = $1718;
       WPERR_INVALID_FLAGS = $1719;
       WPERR_INVALID_OBJECTID = $1720;
       PMERR_OK = $0000;
       PMERR_ALREADY_IN_AREA = $2001;
       PMERR_ALREADY_IN_ELEMENT = $2002;
       PMERR_ALREADY_IN_PATH = $2003;
       PMERR_ALREADY_IN_SEG = $2004;
       PMERR_AREA_INCOMPLETE = $2005;
       PMERR_BASE_ERROR = $2006;
       PMERR_BITBLT_LENGTH_EXCEEDED = $2007;
       PMERR_BITMAP_IN_USE = $2008;
       PMERR_BITMAP_IS_SELECTED = $2009;
       PMERR_BITMAP_NOT_FOUND = $200A;
       PMERR_BITMAP_NOT_SELECTED = $200B;
       PMERR_BOUNDS_OVERFLOW = $200C;
       PMERR_CALLED_SEG_IS_CHAINED = $200D;
       PMERR_CALLED_SEG_IS_CURRENT = $200E;
       PMERR_CALLED_SEG_NOT_FOUND = $200F;
       PMERR_CANNOT_DELETE_ALL_DATA = $2010;
       PMERR_CANNOT_REPLACE_ELEMENT_0 = $2011;
       PMERR_COL_TABLE_NOT_REALIZABLE = $2012;
       PMERR_COL_TABLE_NOT_REALIZED = $2013;
       PMERR_COORDINATE_OVERFLOW = $2014;
       PMERR_CORR_FORMAT_MISMATCH = $2015;
       PMERR_DATA_TOO_LONG = $2016;
       PMERR_DC_IS_ASSOCIATED = $2017;
       PMERR_DESC_STRING_TRUNCATED = $2018;
       PMERR_DEVICE_DRIVER_ERROR_1 = $2019;
       PMERR_DEVICE_DRIVER_ERROR_2 = $201A;
       PMERR_DEVICE_DRIVER_ERROR_3 = $201B;
       PMERR_DEVICE_DRIVER_ERROR_4 = $201C;
       PMERR_DEVICE_DRIVER_ERROR_5 = $201D;
       PMERR_DEVICE_DRIVER_ERROR_6 = $201E;
       PMERR_DEVICE_DRIVER_ERROR_7 = $201F;
       PMERR_DEVICE_DRIVER_ERROR_8 = $2020;
       PMERR_DEVICE_DRIVER_ERROR_9 = $2021;
       PMERR_DEVICE_DRIVER_ERROR_10 = $2022;
       PMERR_DEV_FUNC_NOT_INSTALLED = $2023;
       PMERR_DOSOPEN_FAILURE = $2024;
       PMERR_DOSREAD_FAILURE = $2025;
       PMERR_DRIVER_NOT_FOUND = $2026;
       PMERR_DUP_SEG = $2027;
       PMERR_DYNAMIC_SEG_SEQ_ERROR = $2028;
       PMERR_DYNAMIC_SEG_ZERO_INV = $2029;
       PMERR_ELEMENT_INCOMPLETE = $202A;
       PMERR_ESC_CODE_NOT_SUPPORTED = $202B;
       PMERR_EXCEEDS_MAX_SEG_LENGTH = $202C;
       PMERR_FONT_AND_MODE_MISMATCH = $202D;
       PMERR_FONT_FILE_NOT_LOADED = $202E;
       PMERR_FONT_NOT_LOADED = $202F;
       PMERR_FONT_TOO_BIG = $2030;
       PMERR_HARDWARE_INIT_FAILURE = $2031;
       PMERR_HBITMAP_BUSY = $2032;
       PMERR_HDC_BUSY = $2033;
       PMERR_HRGN_BUSY = $2034;
       PMERR_HUGE_FONTS_NOT_SUPPORTED = $2035;
       PMERR_ID_HAS_NO_BITMAP = $2036;
       PMERR_IMAGE_INCOMPLETE = $2037;
       PMERR_INCOMPAT_COLOR_FORMAT = $2038;
       PMERR_INCOMPAT_COLOR_OPTIONS = $2039;
       PMERR_INCOMPATIBLE_BITMAP = $203A;
       PMERR_INCOMPATIBLE_METAFILE = $203B;
       PMERR_INCORRECT_DC_TYPE = $203C;
       PMERR_INSUFFICIENT_DISK_SPACE = $203D;
       PMERR_INSUFFICIENT_MEMORY = $203E;
       PMERR_INV_ANGLE_PARM = $203F;
       PMERR_INV_ARC_CONTROL = $2040;
       PMERR_INV_AREA_CONTROL = $2041;
       PMERR_INV_ARC_POINTS = $2042;
       PMERR_INV_ATTR_MODE = $2043;
       PMERR_INV_BACKGROUND_COL_ATTR = $2044;
       PMERR_INV_BACKGROUND_MIX_ATTR = $2045;
       PMERR_INV_BITBLT_MIX = $2046;
       PMERR_INV_BITBLT_STYLE = $2047;
       PMERR_INV_BITMAP_DIMENSION = $2048;
       PMERR_INV_BOX_CONTROL = $2049;
       PMERR_INV_BOX_ROUNDING_PARM = $204A;
       PMERR_INV_CHAR_ANGLE_ATTR = $204B;
       PMERR_INV_CHAR_DIRECTION_ATTR = $204C;
       PMERR_INV_CHAR_MODE_ATTR = $204D;
       PMERR_INV_CHAR_POS_OPTIONS = $204E;
       PMERR_INV_CHAR_SET_ATTR = $204F;
       PMERR_INV_CHAR_SHEAR_ATTR = $2050;
       PMERR_INV_CLIP_PATH_OPTIONS = $2051;
       PMERR_INV_CODEPAGE = $2052;
       PMERR_INV_COLOR_ATTR = $2053;
       PMERR_INV_COLOR_DATA = $2054;
       PMERR_INV_COLOR_FORMAT = $2055;
       PMERR_INV_COLOR_INDEX = $2056;
       PMERR_INV_COLOR_OPTIONS = $2057;
       PMERR_INV_COLOR_START_INDEX = $2058;
       PMERR_INV_COORD_OFFSET = $2059;
       PMERR_INV_COORD_SPACE = $205A;
       PMERR_INV_COORDINATE = $205B;
       PMERR_INV_CORRELATE_DEPTH = $205C;
       PMERR_INV_CORRELATE_TYPE = $205D;
       PMERR_INV_CURSOR_BITMAP = $205E;
       PMERR_INV_DC_DATA = $205F;
       PMERR_INV_DC_TYPE = $2060;
       PMERR_INV_DEVICE_NAME = $2061;
       PMERR_INV_DEV_MODES_OPTIONS = $2062;
       PMERR_INV_DRAW_CONTROL = $2063;
       PMERR_INV_DRAW_VALUE = $2064;
       PMERR_INV_DRAWING_MODE = $2065;
       PMERR_INV_DRIVER_DATA = $2066;
       PMERR_INV_DRIVER_NAME = $2067;
       PMERR_INV_DRAW_BORDER_OPTION = $2068;
       PMERR_INV_EDIT_MODE = $2069;
       PMERR_INV_ELEMENT_OFFSET = $206A;
       PMERR_INV_ELEMENT_POINTER = $206B;
       PMERR_INV_END_PATH_OPTIONS = $206C;
       PMERR_INV_ESC_CODE = $206D;
       PMERR_INV_ESCAPE_DATA = $206E;
       PMERR_INV_EXTENDED_LCID = $206F;
       PMERR_INV_FILL_PATH_OPTIONS = $2070;
       PMERR_INV_FIRST_CHAR = $2071;
       PMERR_INV_FONT_ATTRS = $2072;
       PMERR_INV_FONT_FILE_DATA = $2073;
       PMERR_INV_FOR_THIS_DC_TYPE = $2074;
       PMERR_INV_FORMAT_CONTROL = $2075;
       PMERR_INV_FORMS_CODE = $2076;
       PMERR_INV_FONTDEF = $2077;
       PMERR_INV_GEOM_LINE_WIDTH_ATTR = $2078;
       PMERR_INV_GETDATA_CONTROL = $2079;
       PMERR_INV_GRAPHICS_FIELD = $207A;
       PMERR_INV_HBITMAP = $207B;
       PMERR_INV_HDC = $207C;
       PMERR_INV_HJOURNAL = $207D;
       PMERR_INV_HMF = $207E;
       PMERR_INV_HPS = $207F;
       PMERR_INV_HRGN = $2080;
       PMERR_INV_ID = $2081;
       PMERR_INV_IMAGE_DATA_LENGTH = $2082;
       PMERR_INV_IMAGE_DIMENSION = $2083;
       PMERR_INV_IMAGE_FORMAT = $2084;
       PMERR_INV_IN_AREA = $2085;
       PMERR_INV_IN_CALLED_SEG = $2086;
       PMERR_INV_IN_CURRENT_EDIT_MODE = $2087;
       PMERR_INV_IN_DRAW_MODE = $2088;
       PMERR_INV_IN_ELEMENT = $2089;
       PMERR_INV_IN_IMAGE = $208A;
       PMERR_INV_IN_PATH = $208B;
       PMERR_INV_IN_RETAIN_MODE = $208C;
       PMERR_INV_IN_SEG = $208D;
       PMERR_INV_IN_VECTOR_SYMBOL = $208E;
       PMERR_INV_INFO_TABLE = $208F;
       PMERR_INV_JOURNAL_OPTION = $2090;
       PMERR_INV_KERNING_FLAGS = $2091;
       PMERR_INV_LENGTH_OR_COUNT = $2092;
       PMERR_INV_LINE_END_ATTR = $2093;
       PMERR_INV_LINE_JOIN_ATTR = $2094;
       PMERR_INV_LINE_TYPE_ATTR = $2095;
       PMERR_INV_LINE_WIDTH_ATTR = $2096;
       PMERR_INV_LOGICAL_ADDRESS = $2097;
       PMERR_INV_MARKER_BOX_ATTR = $2098;
       PMERR_INV_MARKER_SET_ATTR = $2099;
       PMERR_INV_MARKER_SYMBOL_ATTR = $209A;
       PMERR_INV_MATRIX_ELEMENT = $209B;
       PMERR_INV_MAX_HITS = $209C;
       PMERR_INV_METAFILE = $209D;
       PMERR_INV_METAFILE_LENGTH = $209E;
       PMERR_INV_METAFILE_OFFSET = $209F;
       PMERR_INV_MICROPS_DRAW_CONTROL = $20A0;
       PMERR_INV_MICROPS_FUNCTION = $20A1;
       PMERR_INV_MICROPS_ORDER = $20A2;
       PMERR_INV_MIX_ATTR = $20A3;
       PMERR_INV_MODE_FOR_OPEN_DYN = $20A4;
       PMERR_INV_MODE_FOR_REOPEN_SEG = $20A5;
       PMERR_INV_MODIFY_PATH_MODE = $20A6;
       PMERR_INV_MULTIPLIER = $20A7;
       PMERR_INV_NESTED_FIGURES = $20A8;
       PMERR_INV_OR_INCOMPAT_OPTIONS = $20A9;
       PMERR_INV_ORDER_LENGTH = $20AA;
       PMERR_INV_ORDERING_PARM = $20AB;
       PMERR_INV_OUTSIDE_DRAW_MODE = $20AC;
       PMERR_INV_PAGE_VIEWPORT = $20AD;
       PMERR_INV_PATH_ID = $20AE;
       PMERR_INV_PATH_MODE = $20AF;
       PMERR_INV_PATTERN_ATTR = $20B0;
       PMERR_INV_PATTERN_REF_PT_ATTR = $20B1;
       PMERR_INV_PATTERN_SET_ATTR = $20B2;
       PMERR_INV_PATTERN_SET_FONT = $20B3;
       PMERR_INV_PICK_APERTURE_OPTION = $20B4;
       PMERR_INV_PICK_APERTURE_POSN = $20B5;
       PMERR_INV_PICK_APERTURE_SIZE = $20B6;
       PMERR_INV_PICK_NUMBER = $20B7;
       PMERR_INV_PLAY_METAFILE_OPTION = $20B8;
       PMERR_INV_PRIMITIVE_TYPE = $20B9;
       PMERR_INV_PS_SIZE = $20BA;
       PMERR_INV_PUTDATA_FORMAT = $20BB;
       PMERR_INV_QUERY_ELEMENT_NO = $20BC;
       PMERR_INV_RECT = $20BD;
       PMERR_INV_REGION_CONTROL = $20BE;
       PMERR_INV_REGION_MIX_MODE = $20BF;
       PMERR_INV_REPLACE_MODE_FUNC = $20C0;
       PMERR_INV_RESERVED_FIELD = $20C1;
       PMERR_INV_RESET_OPTIONS = $20C2;
       PMERR_INV_RGBCOLOR = $20C3;
       PMERR_INV_SCAN_START = $20C4;
       PMERR_INV_SEG_ATTR = $20C5;
       PMERR_INV_SEG_ATTR_VALUE = $20C6;
       PMERR_INV_SEG_CH_LENGTH = $20C7;
       PMERR_INV_SEG_NAME = $20C8;
       PMERR_INV_SEG_OFFSET = $20C9;
       PMERR_INV_SETID = $20CA;
       PMERR_INV_SETID_TYPE = $20CB;
       PMERR_INV_SET_VIEWPORT_OPTION = $20CC;
       PMERR_INV_SHARPNESS_PARM = $20CD;
       PMERR_INV_SOURCE_OFFSET = $20CE;
       PMERR_INV_STOP_DRAW_VALUE = $20CF;
       PMERR_INV_TRANSFORM_TYPE = $20D0;
       PMERR_INV_USAGE_PARM = $20D1;
       PMERR_INV_VIEWING_LIMITS = $20D2;
       PMERR_JFILE_BUSY = $20D3;
       PMERR_JNL_FUNC_DATA_TOO_LONG = $20D4;
       PMERR_KERNING_NOT_SUPPORTED = $20D5;
       PMERR_LABEL_NOT_FOUND = $20D6;
       PMERR_MATRIX_OVERFLOW = $20D7;
       PMERR_METAFILE_INTERNAL_ERROR = $20D8;
       PMERR_METAFILE_IN_USE = $20D9;
       PMERR_METAFILE_LIMIT_EXCEEDED = $20DA;
       PMERR_NAME_STACK_FULL = $20DB;
       PMERR_NOT_CREATED_BY_DEVOPENDC = $20DC;
       PMERR_NOT_IN_AREA = $20DD;
       PMERR_NOT_IN_DRAW_MODE = $20DE;
       PMERR_NOT_IN_ELEMENT = $20DF;
       PMERR_NOT_IN_IMAGE = $20E0;
       PMERR_NOT_IN_PATH = $20E1;
       PMERR_NOT_IN_RETAIN_MODE = $20E2;
       PMERR_NOT_IN_SEG = $20E3;
       PMERR_NO_BITMAP_SELECTED = $20E4;
       PMERR_NO_CURRENT_ELEMENT = $20E5;
       PMERR_NO_CURRENT_SEG = $20E6;
       PMERR_NO_METAFILE_RECORD_HANDLE = $20E7;
       PMERR_ORDER_TOO_BIG = $20E8;
       PMERR_OTHER_SET_ID_REFS = $20E9;
       PMERR_OVERRAN_SEG = $20EA;
       PMERR_OWN_SET_ID_REFS = $20EB;
       PMERR_PATH_INCOMPLETE = $20EC;
       PMERR_PATH_LIMIT_EXCEEDED = $20ED;
       PMERR_PATH_UNKNOWN = $20EE;
       PMERR_PEL_IS_CLIPPED = $20EF;
       PMERR_PEL_NOT_AVAILABLE = $20F0;
       PMERR_PRIMITIVE_STACK_EMPTY = $20F1;
       PMERR_PROLOG_ERROR = $20F2;
       PMERR_PROLOG_SEG_ATTR_NOT_SET = $20F3;
       PMERR_PS_BUSY = $20F4;
       PMERR_PS_IS_ASSOCIATED = $20F5;
       PMERR_RAM_JNL_FILE_TOO_SMALL = $20F6;
       PMERR_REALIZE_NOT_SUPPORTED = $20F7;
       PMERR_REGION_IS_CLIP_REGION = $20F8;
       PMERR_RESOURCE_DEPLETION = $20F9;
       PMERR_SEG_AND_REFSEG_ARE_SAME = $20FA;
       PMERR_SEG_CALL_RECURSIVE = $20FB;
       PMERR_SEG_CALL_STACK_EMPTY = $20FC;
       PMERR_SEG_CALL_STACK_FULL = $20FD;
       PMERR_SEG_IS_CURRENT = $20FE;
       PMERR_SEG_NOT_CHAINED = $20FF;
       PMERR_SEG_NOT_FOUND = $2100;
       PMERR_SEG_STORE_LIMIT_EXCEEDED = $2101;
       PMERR_SETID_IN_USE = $2102;
       PMERR_SETID_NOT_FOUND = $2103;
       PMERR_STARTDOC_NOT_ISSUED = $2104;
       PMERR_STOP_DRAW_OCCURRED = $2105;
       PMERR_TOO_MANY_METAFILES_IN_USE = $2106;
       PMERR_TRUNCATED_ORDER = $2107;
       PMERR_UNCHAINED_SEG_ZERO_INV = $2108;
       PMERR_UNSUPPORTED_ATTR = $2109;
       PMERR_UNSUPPORTED_ATTR_VALUE = $210A;
       PMERR_ENDDOC_NOT_ISSUED = $210B;
       PMERR_PS_NOT_ASSOCIATED = $210C;
       PMERR_INV_FLOOD_FILL_OPTIONS = $210D;
       PMERR_INV_FACENAME = $210E;
       PMERR_PALETTE_SELECTED = $210F;
       PMERR_NO_PALETTE_SELECTED = $2110;
       PMERR_INV_HPAL = $2111;
       PMERR_PALETTE_BUSY = $2112;
       PMERR_START_POINT_CLIPPED = $2113;
       PMERR_NO_FILL = $2114;
       PMERR_INV_FACENAMEDESC = $2115;
       PMERR_INV_BITMAP_DATA = $2116;
       PMERR_INV_CHAR_ALIGN_ATTR = $2117;
       PMERR_INV_HFONT = $2118;
       PMERR_HFONT_IS_SELECTED = $2119;
       PMERR_SPL_DRIVER_ERROR = $4001;
       PMERR_SPL_DEVICE_ERROR = $4002;
       PMERR_SPL_DEVICE_NOT_INSTALLED = $4003;
       PMERR_SPL_QUEUE_ERROR = $4004;
       PMERR_SPL_INV_HSPL = $4005;
       PMERR_SPL_NO_DISK_SPACE = $4006;
       PMERR_SPL_NO_MEMORY = $4007;
       PMERR_SPL_PRINT_ABORT = $4008;
       PMERR_SPL_SPOOLER_NOT_INSTALLED = $4009;
       PMERR_SPL_INV_FORMS_CODE = $400A;
       PMERR_SPL_INV_PRIORITY = $400B;
       PMERR_SPL_NO_FREE_JOB_ID = $400C;
       PMERR_SPL_NO_DATA = $400D;
       PMERR_SPL_INV_TOKEN = $400E;
       PMERR_SPL_INV_DATATYPE = $400F;
       PMERR_SPL_PROCESSOR_ERROR = $4010;
       PMERR_SPL_INV_JOB_ID = $4011;
       PMERR_SPL_JOB_NOT_PRINTING = $4012;
       PMERR_SPL_JOB_PRINTING = $4013;
       PMERR_SPL_QUEUE_ALREADY_EXISTS = $4014;
       PMERR_SPL_INV_QUEUE_NAME = $4015;
       PMERR_SPL_QUEUE_NOT_EMPTY = $4016;
       PMERR_SPL_DEVICE_ALREADY_EXISTS = $4017;
       PMERR_SPL_DEVICE_LIMIT_REACHED = $4018;
       PMERR_SPL_STATUS_STRING_TRUNC = $4019;
       PMERR_SPL_INV_LENGTH_OR_COUNT = $401A;
       PMERR_SPL_FILE_NOT_FOUND = $401B;
       PMERR_SPL_CANNOT_OPEN_FILE = $401C;
       PMERR_SPL_DRIVER_NOT_INSTALLED = $401D;
       PMERR_SPL_INV_PROCESSOR_DATTYPE = $401E;
       PMERR_SPL_INV_DRIVER_DATATYPE = $401F;
       PMERR_SPL_PROCESSOR_NOT_INST = $4020;
       PMERR_SPL_NO_SUCH_LOG_ADDRESS = $4021;
       PMERR_SPL_PRINTER_NOT_FOUND = $4022;
       PMERR_SPL_DD_NOT_FOUND = $4023;
       PMERR_SPL_QUEUE_NOT_FOUND = $4024;
       PMERR_SPL_MANY_QUEUES_ASSOC = $4025;
       PMERR_SPL_NO_QUEUES_ASSOCIATED = $4026;
       PMERR_SPL_INI_FILE_ERROR = $4027;
       PMERR_SPL_NO_DEFAULT_QUEUE = $4028;
       PMERR_SPL_NO_CURRENT_FORMS_CODE = $4029;
       PMERR_SPL_NOT_AUTHORISED = $402A;
       PMERR_SPL_TEMP_NETWORK_ERROR = $402B;
       PMERR_SPL_HARD_NETWORK_ERROR = $402C;
       PMERR_DEL_NOT_ALLOWED = $402D;
       PMERR_CANNOT_DEL_QP_REF = $402E;
       PMERR_CANNOT_DEL_QNAME_REF = $402F;
       PMERR_CANNOT_DEL_PRINTER_DD_REF = $4030;
       PMERR_CANNOT_DEL_PRN_NAME_REF = $4031;
       PMERR_CANNOT_DEL_PRN_ADDR_REF = $4032;
       PMERR_SPOOLER_QP_NOT_DEFINED = $4033;
       PMERR_PRN_NAME_NOT_DEFINED = $4034;
       PMERR_PRN_ADDR_NOT_DEFINED = $4035;
       PMERR_PRINTER_DD_NOT_DEFINED = $4036;
       PMERR_PRINTER_QUEUE_NOT_DEFINED = $4037;
       PMERR_PRN_ADDR_IN_USE = $4038;
       PMERR_SPL_TOO_MANY_OPEN_FILES = $4039;
       PMERR_SPL_CP_NOT_REQD = $403A;
       PMERR_UNABLE_TO_CLOSE_DEVICE = $4040;
       PMERR_SPL_ERROR_1 = (SPLERR_BASE+4001);
       PMERR_SPL_ERROR_2 = (SPLERR_BASE+4002);
       PMERR_SPL_ERROR_3 = (SPLERR_BASE+4003);
       PMERR_SPL_ERROR_4 = (SPLERR_BASE+4004);
       PMERR_SPL_ERROR_5 = (SPLERR_BASE+4005);
       PMERR_SPL_ERROR_6 = (SPLERR_BASE+4006);
       PMERR_SPL_ERROR_7 = (SPLERR_BASE+4007);
       PMERR_SPL_ERROR_8 = (SPLERR_BASE+4008);
       PMERR_SPL_ERROR_9 = (SPLERR_BASE+4009);
       PMERR_SPL_ERROR_10 = (SPLERR_BASE+4010);
       PMERR_SPL_ERROR_11 = (SPLERR_BASE+4011);
       PMERR_SPL_ERROR_12 = (SPLERR_BASE+4012);
       PMERR_SPL_ERROR_13 = (SPLERR_BASE+4013);
       PMERR_SPL_ERROR_14 = (SPLERR_BASE+4014);
       PMERR_SPL_ERROR_15 = (SPLERR_BASE+4015);
       PMERR_SPL_ERROR_16 = (SPLERR_BASE+4016);
       PMERR_SPL_ERROR_17 = (SPLERR_BASE+4017);
       PMERR_SPL_ERROR_18 = (SPLERR_BASE+4018);
       PMERR_SPL_ERROR_19 = (SPLERR_BASE+4019);
       PMERR_SPL_ERROR_20 = (SPLERR_BASE+4020);
       PMERR_SPL_ERROR_21 = (SPLERR_BASE+4021);
       PMERR_SPL_ERROR_22 = (SPLERR_BASE+4022);
       PMERR_SPL_ERROR_23 = (SPLERR_BASE+4023);
       PMERR_SPL_ERROR_24 = (SPLERR_BASE+4024);
       PMERR_SPL_ERROR_25 = (SPLERR_BASE+4025);
       PMERR_SPL_ERROR_26 = (SPLERR_BASE+4026);
       PMERR_SPL_ERROR_27 = (SPLERR_BASE+4027);
       PMERR_SPL_ERROR_28 = (SPLERR_BASE+4028);
       PMERR_SPL_ERROR_29 = (SPLERR_BASE+4029);
       PMERR_SPL_ERROR_30 = (SPLERR_BASE+4030);
       PMERR_SPL_ERROR_31 = (SPLERR_BASE+4031);
       PMERR_SPL_ERROR_32 = (SPLERR_BASE+4032);
       PMERR_SPL_ERROR_33 = (SPLERR_BASE+4033);
       PMERR_SPL_ERROR_34 = (SPLERR_BASE+4034);
       PMERR_SPL_ERROR_35 = (SPLERR_BASE+4035);
       PMERR_SPL_ERROR_36 = (SPLERR_BASE+4036);
       PMERR_SPL_ERROR_37 = (SPLERR_BASE+4037);
       PMERR_SPL_ERROR_38 = (SPLERR_BASE+4038);
       PMERR_SPL_ERROR_39 = (SPLERR_BASE+4039);
       PMERR_SPL_ERROR_40 = (SPLERR_BASE+4040);
       PMERR_SPLMSGBOX_INFO_CAPTION = (SPLERR_BASE+4041);
       PMERR_SPLMSGBOX_WARNING_CAPTION = (SPLERR_BASE+4042);
       PMERR_SPLMSGBOX_ERROR_CAPTION = (SPLERR_BASE+4043);
       PMERR_SPLMSGBOX_SEVERE_CAPTION = (SPLERR_BASE+4044);
       PMERR_SPLMSGBOX_JOB_DETAILS = (SPLERR_BASE+4045);
       PMERR_SPLMSGBOX_ERROR_ACTION = (SPLERR_BASE+4046);
       PMERR_SPLMSGBOX_SEVERE_ACTION = (SPLERR_BASE+4047);
       PMERR_SPLMSGBOX_BIT_0_TEXT = (SPLERR_BASE+4048);
       PMERR_SPLMSGBOX_BIT_1_TEXT = (SPLERR_BASE+4049);
       PMERR_SPLMSGBOX_BIT_2_TEXT = (SPLERR_BASE+4050);
       PMERR_SPLMSGBOX_BIT_3_TEXT = (SPLERR_BASE+4051);
       PMERR_SPLMSGBOX_BIT_4_TEXT = (SPLERR_BASE+4052);
       PMERR_SPLMSGBOX_BIT_5_TEXT = (SPLERR_BASE+4053);
       PMERR_SPLMSGBOX_BIT_15_TEXT = (SPLERR_BASE+4054);
       PMERR_SPL_NOPATHBUFFER = (SPLERR_BASE+4055);
       PMERR_SPL_ALREADY_INITIALISED = (SPLERR_BASE+4093);
       PMERR_SPL_ERROR = (SPLERR_BASE+4095);
       PMERR_INV_TYPE = $5001;
       PMERR_INV_CONV = $5002;
       PMERR_INV_SEGLEN = $5003;
       PMERR_DUP_SEGNAME = $5004;
       PMERR_INV_XFORM = $5005;
       PMERR_INV_VIEWLIM = $5006;
       PMERR_INV_3DCOORD = $5007;
       PMERR_SMB_OVFLOW = $5008;
       PMERR_SEG_OVFLOW = $5009;
       PMERR_PIC_DUP_FILENAME = $5010;

    function WinRegisterClass(hab : cardinal;pszClassName : pchar;pfnWndProc : proc;flStyle,cbWindowData : cardinal) : longbool; cdecl;
    function WinDefWindowProc(hwnd,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;
    function WinDestroyWindow(hwnd : cardinal) : longbool; cdecl;
    function WinShowWindow(hwnd : cardinal;fShow : longbool) : longbool;  cdecl;
    function WinQueryWindowRect(hwnd : cardinal;var rclDest : TRectl) : longbool; cdecl;
    function WinQueryWindowRect(hwnd : cardinal;prclDest : PRectl) : longbool; cdecl;
    function WinGetPS(hwnd : cardinal) : cardinal;  cdecl;
    function WinReleasePS(hps : cardinal) : longbool;  cdecl;
    function WinEndPaint(hps : cardinal) : longbool; cdecl;
    function WinGetClipPS(hwnd,hwndClip,fl : cardinal) : cardinal; cdecl;
    function WinIsWindowShowing(hwnd : cardinal) : longbool; cdecl;
    function WinBeginPaint(hwnd,hps : cardinal;var rclPaint : TRectl) : cardinal; cdecl;
    function WinBeginPaint(hwnd,hps : cardinal;prclPaint: PRectl) : cardinal; cdecl;
    function WinOpenWindowDC(hwnd : cardinal) : cardinal; cdecl;
    function WinScrollWindow(hwnd : cardinal;dx,dy : longint;var rclScroll,rclClip : TRectl;hrgnUpdate : cardinal;var rclUpdate : TRectl;rgfsw : cardinal) : longint; cdecl;
    function WinScrollWindow(hwnd : cardinal;dx,dy : longint;prclScroll,prclClip : PRectl;hrgnUpdate : cardinal;prclUpdate : PRectl;rgfsw : cardinal) : longint; cdecl;
    function WinFillRect(hps : cardinal;var rcl : TRectl;lColor : longint) : longbool; cdecl;
    function WinFillRect(hps : cardinal;prcl : PRectl;lColor : longint) : longbool; cdecl;
    function WinQueryVersion(hab : cardinal) : cardinal; cdecl;
    function WinInitialize(flOptions : cardinal) : cardinal; cdecl;
    function WinTerminate(hab : cardinal) : longbool; cdecl;
    function WinQueryAnchorBlock(hwnd : cardinal) : cardinal; cdecl;
    function WinCreateWindow(hwndParent : cardinal;pszClass,pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;
    function WinCreateWindow(hwndParent : cardinal;pszClass : cardinal;pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;
    function WinCreateWCWindow(hwndParent : cardinal;pszClass : cardinal;pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;
    function WinEnableWindow(hwnd : cardinal;fEnable : longbool) : longbool; cdecl;
    function WinIsWindowEnabled(hwnd : cardinal) : longbool; cdecl;
    function WinEnableWindowUpdate(hwnd : cardinal;fEnable : longbool) : longbool; cdecl;
    function WinIsWindowVisible(hwnd : cardinal) : longbool; cdecl;
    function WinQueryWindowText(hwnd : cardinal;cchBufferMax : longint;pchBuffer : pchar) : longint; cdecl;
    function WinSetWindowText(hwnd : cardinal;pszText : pchar) : longbool; cdecl;
    function WinQueryWindowTextLength(hwnd : cardinal) : longint; cdecl;
    function WinWindowFromID(hwndParent,id : cardinal) : cardinal; cdecl;
    function WinIsWindow(hab,hwnd : cardinal) : longbool; cdecl;
    function WinQueryWindow(hwnd : cardinal;cmd : longint) : cardinal; cdecl;
    function WinMultWindowFromIDs(hwndParent : cardinal;var prghwnd : cardinal;idFirst,idLast : cardinal) : longint; cdecl;
    function WinMultWindowFromIDs(hwndParent : cardinal;prghwnd : PCardinal;idFirst,idLast : cardinal) : longint; cdecl;
    function WinSetParent(hwnd,hwndNewParent : cardinal;fRedraw : longbool) : longbool; cdecl;
    function WinIsChild(hwnd,hwndParent : cardinal) : longbool; cdecl;
    function WinSetOwner(hwnd,hwndNewOwner : cardinal) : longbool; cdecl;
    function WinQueryWindowProcess(hwnd : cardinal;var _pid,_tid : cardinal) : longbool; cdecl;
    function WinQueryWindowProcess(hwnd : cardinal;_ppid,_ptid : PCardinal) : longbool; cdecl;
    function WinQueryObjectWindow(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinQueryDesktopWindow(hab,hdc : cardinal) : cardinal; cdecl;
    function WinSetWindowPos(hwnd,hwndInsertBehind : cardinal;x,y,cx,cy : longint;fl : cardinal) : longbool; cdecl;
    function WinSetMultWindowPos(hab : cardinal;var _swp : TSWP;cswp : cardinal) : longbool; cdecl;
    function WinSetMultWindowPos(hab : cardinal;_pswp : PSWP;cswp : cardinal) : longbool; cdecl;
    function WinQueryWindowPos(hwnd : cardinal;var _swp : TSWP) : longbool; cdecl;
    function WinQueryWindowPos(hwnd : cardinal;_pswp : PSWP) : longbool; cdecl;
    function WinUpdateWindow(hwnd : cardinal) : longbool; cdecl;
    function WinInvalidateRect(hwnd : cardinal;var wrc : TRectl;fIncludeChildren : longbool) : longbool; cdecl;
    function WinInvalidateRect(hwnd : cardinal;pwrc : PRectl;fIncludeChildren : longbool) : longbool; cdecl;
    function WinInvalidateRegion(hwnd,hrgn : cardinal;fIncludeChildren : longbool) : longbool; cdecl;
    function WinInvertRect(hps : cardinal;var rcl : TRectl) : longbool; cdecl;
    function WinInvertRect(hps : cardinal;prcl : PRectl) : longbool; cdecl;
    function WinDrawBitmap(hpsDst,hbm : cardinal;var wrcSrc : TRectl;var ptlDst : POINTL;clrFore : longint;clrBack : longint;fl : cardinal) : longbool; cdecl;
    function WinDrawBitmap(hpsDst,hbm : cardinal;pwrcSrc : PRectl;pptlDst : PPOINTL;clrFore : longint;clrBack : longint;fl : cardinal) : longbool; cdecl;
    function WinDrawText(hps : cardinal;cchText : longint;lpchText : pchar;var rcl : TRectl;clrFore,clrBack : longint;flCmd : cardinal) : longint; cdecl;
    function WinDrawText(hps : cardinal;cchText : longint;lpchText : pchar;prcl : PRectl;clrFore,clrBack : longint;flCmd : cardinal) : longint; cdecl;
    function WinDrawBorder(hps : cardinal;var rcl : TRectl;cx,cy : longint;clrFore,clrBack : longint;flCmd : cardinal) : longbool; cdecl;
    function WinDrawBorder(hps : cardinal;prcl : PRectl;cx,cy : longint;clrFore,clrBack : longint;flCmd : cardinal) : longbool; cdecl;
    function WinLoadString(hab,hmod,id : cardinal;cchMax : longint;pchBuffer : pchar) : longint; cdecl;
    function WinLoadMessage(hab,hmod,id : cardinal;cchMax : longint;pchBuffer : pchar) : longint; cdecl;
    function WinSetActiveWindow(hwndDesktop,hwnd : cardinal) : longbool; cdecl;
    function WinSubclassWindow(hwnd : cardinal;pfnwp : proc) : proc; cdecl;
    function WinQueryClassName(hwnd : cardinal;cchMax : longint;pch : pchar) : longint; cdecl;
    function WinQueryClassInfo(hab : cardinal;pszClassName : pchar;var _ClassInfo : TClassInfo) : longbool; cdecl;
    function WinQueryClassInfo(hab : cardinal;pszClassName : pchar;_PClassInfo : PClassInfo) : longbool; cdecl;
    function WinQueryActiveWindow(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinIsThreadActive(hab : cardinal) : longbool; cdecl;
    function WinQuerySysModalWindow(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinSetSysModalWindow(hwndDesktop,hwnd : cardinal) : longbool; cdecl;
    function WinQueryWindowUShort(hwnd : cardinal;index : longint) : word; cdecl;
    function WinSetWindowUShort(hwnd : cardinal;index : longint;us : word) : longbool; cdecl;
    function WinQueryWindowULong(hwnd : cardinal;index : longint) : cardinal; cdecl;
    function WinSetWindowULong(hwnd : cardinal;index : longint;ul : cardinal) : longbool; cdecl;
    function WinQueryWindowPtr(hwnd : cardinal;index : longint) : pointer; cdecl;
    function WinSetWindowPtr(hwnd : cardinal;index : longint;p : pointer) : longbool; cdecl;
    function WinSetWindowBits(hwnd : cardinal;index : longint;flData,flMask : cardinal) : longbool; cdecl;
    function WinBeginEnumWindows(hwnd : cardinal) : cardinal; cdecl;
    function WinGetNextWindow(henum : cardinal) : cardinal; cdecl;
    function WinEndEnumWindows(henum : cardinal) : longbool; cdecl;
    function WinWindowFromPoint(hwnd : cardinal;var ptl : TPointL;fChildren : longbool) : cardinal; cdecl;
    function WinWindowFromPoint(hwnd : cardinal;pptl : PPointL;fChildren : longbool) : cardinal; cdecl;
    function WinMapWindowPoints(hwndFrom,hwndTo : cardinal;var prgptl : TPointL;cwpt : longint) : longbool; cdecl;
    function WinMapWindowPoints(hwndFrom,hwndTo : cardinal;prgptl : PPointL;cwpt : longint) : longbool; cdecl;
    function WinValidateRect(hwnd : cardinal;var rcl : TRectl;fIncludeChildren : longbool) : longbool; cdecl;
    function WinValidateRect(hwnd : cardinal;prcl : PRectl;fIncludeChildren : longbool) : longbool; cdecl;
    function WinValidateRegion(hwnd,hrgn : cardinal;fIncludeChildren : longbool) : longbool; cdecl;
    function WinWindowFromDC(hdc : cardinal) : cardinal; cdecl;
    function WinQueryWindowDC(hwnd : cardinal) : cardinal; cdecl;
    function WinGetScreenPS(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinLockWindowUpdate(hwndDesktop,hwndLockUpdate : cardinal) : longbool; cdecl;
    function WinLockVisRegions(hwndDesktop : cardinal;fLock : longbool) : longbool; cdecl;
    function WinQueryUpdateRect(hwnd : cardinal;var rcl : TRectl) : longbool; cdecl;
    function WinQueryUpdateRect(hwnd : cardinal;prcl : PRectl) : longbool; cdecl;
    function WinQueryUpdateRegion(hwnd,hrgn : cardinal) : longint; cdecl;
    function WinExcludeUpdateRegion(hps,hwnd : cardinal) : longint; cdecl;
    function WinSendMsg(hwnd,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;
    function WinCreateMsgQueue(hab : cardinal;cmsg : longint) : cardinal; cdecl;
    function WinDestroyMsgQueue(hmq : cardinal) : longbool; cdecl;
    function WinQueryQueueInfo(hmq : cardinal;var mqi : TMQInfo;cbCopy : cardinal) : longbool; cdecl;
    function WinQueryQueueInfo(hmq : cardinal;pmqi : PMQInfo;cbCopy : cardinal) : longbool; cdecl;
    function WinCancelShutdown(hmq : cardinal;fCancelAlways : longbool) : longbool; cdecl;
    function WinGetMsg(hab : cardinal;var _qmsg : TQMsg;hwndFilter,msgFilterFirst,msgFilterLast : cardinal) : longbool; cdecl;
    function WinGetMsg(hab : cardinal;_pqmsg : PQMsg;hwndFilter,msgFilterFirst,msgFilterLast : cardinal) : longbool; cdecl;
    function WinPeekMsg(hab : cardinal;var _qmsg : TQMsg;hwndFilter,msgFilterFirst,msgFilterLast,fl : cardinal) : longbool; cdecl;
    function WinPeekMsg(hab : cardinal;_pqmsg : PQMsg;hwndFilter,msgFilterFirst,msgFilterLast,fl : cardinal) : longbool; cdecl;
    function WinDispatchMsg(hab : cardinal;var _qmsg : TQMsg) : pointer; cdecl;
    function WinDispatchMsg(hab : cardinal;_pqmsg : PQMsg) : pointer; cdecl;
    function WinPostMsg(hwnd,msg : cardinal;mp1,mp2 : pointer) : longbool; cdecl;
    function WinRegisterUserMsg(hab,msgid : cardinal;datatype1,dir1,datatype2,dir2,datatyper : longint) : longbool; cdecl;
    function WinRegisterUserDatatype(hab : cardinal;datatype,count : longint;var types : longint) : longbool; cdecl;
    function WinSetMsgMode(hab : cardinal;classname : pchar;control : longint) : longbool; cdecl;
    function WinSetSynchroMode(hab : cardinal;mode : longint) : longbool;  cdecl;
    function WinInSendMsg(hab : cardinal) : longbool; cdecl;
    function WinBroadcastMsg(hwnd,msg : cardinal;mp1,mp2 : pointer;rgf : cardinal) : longbool; cdecl;
    function WinWaitMsg(hab,msgFirst,msgLast : cardinal) : longbool; cdecl;
    function WinQueryQueueStatus(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinQueryMsgPos(hab : cardinal;var ptl : TPointL) : longbool; cdecl;
    function WinQueryMsgPos(hab : cardinal;pptl : PPointL) : longbool; cdecl;
    function WinQueryMsgTime(hab : cardinal) : cardinal; cdecl;
    function WinWaitEventSem(hev,ulTimeout : cardinal) : cardinal; cdecl;
    function WinRequestMutexSem(hmtx,ulTimeout : cardinal) : cardinal; cdecl;
    function WinWaitMuxWaitSem(hmux,ulTimeout:cardinal;var ulUser : cardinal) : cardinal; cdecl;
    function WinWaitMuxWaitSem(hmux,ulTimeout:cardinal;pulUser : PCardinal) : cardinal; cdecl;
    function WinPostQueueMsg(hmq,msg : cardinal;mp1,mp2 : pointer) : longbool; cdecl;
    function WinSetMsgInterest(hwnd,msg_class : cardinal;control : longint) : longbool; cdecl;
    function WinSetClassMsgInterest(hab : cardinal;pszClassName : pchar;msg_class : cardinal;control : longint) : longbool; cdecl;
    function WinSetFocus(hwndDesktop,hwndSetFocus : cardinal) : longbool; cdecl;
    function WinFocusChange(hwndDesktop,hwndSetFocus,flFocusChange : cardinal) : longbool; cdecl;
    function WinSetCapture(hwndDesktop,hwnd : cardinal) : longbool; cdecl;
    function WinQueryCapture(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinQueryFocus(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinGetKeyState(hwndDesktop : cardinal;vkey : longint) : longint; cdecl;
    function WinGetPhysKeyState(hwndDesktop : cardinal;sc : longint) : longint; cdecl;
    function WinEnablePhysInput(hwndDesktop : cardinal;fEnable : longbool) : longbool; cdecl;
    function WinIsPhysInputEnabled(hwndDesktop : cardinal) : longbool; cdecl;
    function WinSetKeyboardStateTable(hwndDesktop : cardinal;var KeyStateTable;fSet : longbool) : longbool; cdecl;
    function WinSetKeyboardStateTable(hwndDesktop : cardinal;pKeyStateTable : pointer;fSet : longbool) : longbool; cdecl;
    function WinGetDlgMsg(hwndDlg : cardinal;var _qmsg : TQMsg) : longbool; cdecl;
    function WinGetDlgMsg(hwndDlg : cardinal;_pqmsg : PQMsg) : longbool; cdecl;
    function WinLoadDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;hmod,idDlg : cardinal;pCreateParams : pointer) : cardinal; cdecl;
    function WinDlgBox(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;hmod,idDlg : cardinal;pCreateParams : pointer) : cardinal; cdecl;
    function WinDismissDlg(hwndDlg,usResult : cardinal) : longbool; cdecl;
    function WinQueryDlgItemShort(hwndDlg,idItem : cardinal;var _Result : integer;fSigned : longbool) : longbool; cdecl;
    function WinQueryDlgItemShort(hwndDlg,idItem : cardinal;PResult : PInteger;fSigned : longbool) : longbool; cdecl;
    function WinSetDlgItemShort(hwndDlg,idItem : cardinal;usValue : word;fSigned : longbool) : longbool; cdecl;
    function WinSetDlgItemText(hwndDlg,idItem : cardinal;pszText : pchar) : longbool; cdecl;
    function WinQueryDlgItemText(hwndDlg,idItem : cardinal;cchBufferMax : longint;pchBuffer : pchar) : cardinal; cdecl;
    function WinQueryDlgItemTextLength(hwndDlg,idItem : cardinal) : longint; cdecl;
    function WinDefDlgProc(hwndDlg,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;
    function WinAlarm(hwndDesktop,rgfType : cardinal) : longbool; cdecl;
    function WinMessageBox(hwndParent,hwndOwner : cardinal;pszText,pszCaption : pchar;idWindow,flStyle : cardinal) : cardinal; cdecl;
(*
    function WinMessageBox2(hwndParent,hwndOwner: cardinal;pszText,pszCaption: PChar; idWindow: cardinal; MBInfo: PMB2Info): cardinal; cdecl;
*)
    function WinProcessDlg(hwndDlg : cardinal) : cardinal; cdecl;
    function WinSendDlgItemMsg(hwndDlg,idItem,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;
    function WinMapDlgPoints(hwndDlg : cardinal;var prgwptl : TPointL;cwpt : cardinal;fCalcWindowCoords : longbool) : longbool; cdecl;
    function WinMapDlgPoints(hwndDlg : cardinal;prgwptl : PPointL;cwpt : cardinal;fCalcWindowCoords : longbool) : longbool; cdecl;
    function WinEnumDlgItem(hwndDlg,hwnd,code : cardinal) : cardinal; cdecl;
    function WinSubstituteStrings(hwnd : cardinal;pszSrc : pchar;cchDstMax : longint;pszDst : pchar) : longint; cdecl;
    function WinCreateDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;var dlgt : TDlgTemplate;pCreateParams : pointer) : cardinal; cdecl;
    function WinCreateDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;pdlgt : PDlgTemplate;pCreateParams : pointer) : cardinal; cdecl;
    function WinLoadMenu(hwndFrame,hmod,idMenu : cardinal) : cardinal; cdecl;
    function WinCreateMenu(hwndParent : cardinal;lpmt : pointer) : cardinal; cdecl;
    function WinPopupMenu(hwndParent,hwndOwner,hwndMenu : cardinal;x,y,idItem : longint;fs : cardinal) : longbool; cdecl;
    function WinCreateStdWindow(hwndParent,flStyle : cardinal;var flCreateFlags : cardinal;pszClientClass,pszTitle : pchar;styleClient,hmod,idResources : cardinal;var hwndClient : cardinal) : cardinal; cdecl;
    function WinCreateStdWindow(hwndParent,flStyle : cardinal;pflCreateFlags : PCardinal;pszClientClass,pszTitle : pchar;styleClient,hmod,idResources : cardinal;phwndClient : PCardinal) : cardinal; cdecl;
    function WinFlashWindow(hwndFrame : cardinal;fFlash : longbool) : longbool; cdecl;
    function WinCreateFrameControls(hwndFrame : cardinal;var fcdata : TFrameCData;pszTitle : pchar) : longbool;  cdecl;
    function WinCreateFrameControls(hwndFrame : cardinal;pfcdata : PFrameCData;pszTitle : pchar) : longbool;  cdecl;
    function WinCalcFrameRect(hwndFrame : cardinal;var rcl : TRectl;fClient : longbool) : longbool; cdecl;
    function WinCalcFrameRect(hwndFrame : cardinal;prcl : PRectl;fClient : longbool) : longbool; cdecl;
    function WinGetMinPosition(hwnd : cardinal;var _swp : TSWP;var pptl : POINTL) : longbool; cdecl;
    function WinGetMinPosition(hwnd : cardinal;_pswp : PSWP;var pptl : POINTL) : longbool; cdecl;
    function WinGetMaxPosition(hwnd : cardinal;var _swp : TSWP) : longbool; cdecl;
    function WinGetMaxPosition(hwnd : cardinal;_pswp : PSWP) : longbool; cdecl;
    function WinSaveWindowPos(hsvwp : cardinal;var _swp : TSWP;cswp : cardinal) : longbool; cdecl;
    function WinSaveWindowPos(hsvwp : cardinal;_pswp : PSWP;cswp : cardinal) : longbool; cdecl;
    function WinCopyRect(hab : cardinal;var rclDst, rclSrc : TRectl) : longbool; cdecl;
    function WinCopyRect(hab : cardinal;prclDst, prclSrc : PRectl) : longbool; cdecl;
    function WinSetRect(hab : cardinal;var rcl : TRectl;xLeft,yBottom,xRight,yTop : longint) : longbool; cdecl;
    function WinSetRect(hab : cardinal;_prcl : PRectl;xLeft,yBottom,xRight,yTop : longint) : longbool; cdecl;
    function WinIsRectEmpty(hab : cardinal;var rcl : TRectl) : longbool; cdecl;
    function WinIsRectEmpty(hab : cardinal;prcl : PRectl) : longbool; cdecl;
    function WinEqualRect(hab : cardinal;var rcl1,rcl2 : TRectl) : longbool; cdecl;
    function WinEqualRect(hab : cardinal;prcl1,prcl2 : PRectl) : longbool; cdecl;
    function WinSetRectEmpty(hab : cardinal;var rcl : TRectl) : longbool; cdecl;
    function WinSetRectEmpty(hab : cardinal;prcl : PRectl) : longbool; cdecl;
    function WinOffsetRect(hab : cardinal;var rcl : TRectl;cx,cy : longint) : longbool; cdecl;
    function WinOffsetRect(hab : cardinal;prcl : PRectl;cx,cy : longint) : longbool; cdecl;
    function WinInflateRect(hab : cardinal;var rcl : TRectl;cx,cy : longint) : longbool; cdecl;
    function WinInflateRect(hab : cardinal;prcl : PRectl;cx,cy : longint) : longbool; cdecl;
    function WinPtInRect(hab : cardinal;var rcl : TRectl;var ptl : TPointL) : longbool; cdecl;
    function WinPtInRect(hab : cardinal;prcl : PRectl;pptl : PPointL) : longbool; cdecl;
    function WinIntersectRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;
    function WinIntersectRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;
    function WinUnionRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;
    function WinUnionRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;
    function WinSubtractRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;
    function WinSubtractRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;
    function WinMakeRect(hab : cardinal;var wrc : TRectl) : longbool; cdecl;
    function WinMakeRect(hab : cardinal;pwrc : PRectl) : longbool; cdecl;
    function WinMakePoints(hab : cardinal;var wpt : TPointL;cwpt : cardinal) : longbool; cdecl;
    function WinMakePoints(hab : cardinal;pwpt : PPointL;cwpt : cardinal) : longbool; cdecl;
    function WinQuerySysValue(hwndDesktop : cardinal;iSysValue : longint) : longint; cdecl;
    function WinSetSysValue(hwndDesktop : cardinal;iSysValue,lValue : longint) : longbool; cdecl;
    function WinSetPresParam(hwnd,id,cbParam : cardinal;pbParam : pointer) : longbool;  cdecl;
    function WinQueryPresParam(hwnd,id1,id2 : cardinal;var ulId : cardinal;cbBuf : cardinal;pbBuf : pointer;fs : cardinal) : cardinal; cdecl;
    function WinQueryPresParam(hwnd,id1,id2 : cardinal;pulId : PCardinal;cbBuf : cardinal;pbBuf : pointer;fs : cardinal) : cardinal; cdecl;
    function WinRemovePresParam(hwnd,id : cardinal) : longbool; cdecl;
    function WinQuerySysColor(hwndDesktop : cardinal;clr,lReserved : longint) : longint; cdecl;
    function WinSetSysColors(hwndDesktop,flOptions,flFormat : cardinal;clrFirst : longint;cclr : cardinal;var clr : longint) : longbool; cdecl;
    function WinSetSysColors(hwndDesktop,flOptions,flFormat : cardinal;clrFirst : longint;cclr : cardinal;pclr : PLongint) : longbool; cdecl;
    function WinStartTimer(hab,hwnd,idTimer,dtTimeout : cardinal) : cardinal; cdecl;
    function WinStopTimer(hab,hwnd,idTimer : cardinal) : longbool; cdecl;
    function WinGetCurrentTime(hab : cardinal) : cardinal; cdecl;
    function WinLoadAccelTable(hab,hmod,idAccelTable : cardinal) : cardinal; cdecl;
    function WinCopyAccelTable(haccel : cardinal;var _AccelTable : TAccelTable;cbCopyMax : cardinal) : cardinal; cdecl;
    function WinCopyAccelTable(haccel : cardinal;_pAccelTable : PAccelTable;cbCopyMax : cardinal) : cardinal; cdecl;
    function WinCreateAccelTable(hab : cardinal;var _AccelTable : TAccelTable) : cardinal; cdecl;
    function WinCreateAccelTable(hab : cardinal;_pAccelTable : PAccelTable) : cardinal; cdecl;
    function WinDestroyAccelTable(haccel : cardinal) : longbool; cdecl;
    function WinTranslateAccel(hab,hwnd,haccel : cardinal;var _qmsg : TQMsg) : longbool; cdecl;
    function WinTranslateAccel(hab,hwnd,haccel : cardinal;_pqmsg : PQMsg) : longbool; cdecl;
    function WinSetAccelTable(hab,haccel,hwndFrame : cardinal) : longbool; cdecl;
    function WinQueryAccelTable(hab,hwndFrame : cardinal) : cardinal; cdecl;
    function WinTrackRect(hwnd,hps : cardinal;var ti : TTrackInfo) : longbool; cdecl;
    function WinTrackRect(hwnd,hps : cardinal;pti : PTrackInfo) : longbool; cdecl;
    function WinShowTrackRect(hwnd : cardinal;fShow : longbool) : longbool; cdecl;
    function WinSetClipbrdOwner(hab,hwnd : cardinal) : longbool; cdecl;
    function WinSetClipbrdData(hab,ulData,fmt,rgfFmtInfo : cardinal) : longbool;  cdecl;
    function WinQueryClipbrdData(hab,fmt : cardinal) : cardinal;  cdecl;
    function WinQueryClipbrdFmtInfo(hab,fmt : cardinal;var prgfFmtInfo : cardinal) : longbool; cdecl;
    function WinQueryClipbrdFmtInfo(hab,fmt : cardinal;prgfFmtInfo : PCardinal) : longbool; cdecl;
    function WinSetClipbrdViewer(hab,hwndNewClipViewer : cardinal) : longbool;  cdecl;
    function WinEnumClipbrdFmts(hab,fmt : cardinal) : cardinal; cdecl;
    function WinEmptyClipbrd(hab : cardinal) : longbool; cdecl;
    function WinOpenClipbrd(hab : cardinal) : longbool;  cdecl;
    function WinCloseClipbrd(hab : cardinal) : longbool; cdecl;
    function WinQueryClipbrdOwner(hab : cardinal) : cardinal; cdecl;
    function WinQueryClipbrdViewer(hab : cardinal) : cardinal; cdecl;
    function WinDestroyCursor(hwnd : cardinal) : longbool; cdecl;
    function WinShowCursor(hwnd : cardinal;fShow : longbool) : longbool; cdecl;
    function WinCreateCursor(hwnd : cardinal;x,y,cx,cy : longint;fs : cardinal;var rclClip : TRectl) : longbool; cdecl;
    function WinCreateCursor(hwnd : cardinal;x,y,cx,cy : longint;fs : cardinal;prclClip : PRectl) : longbool; cdecl;
    function WinQueryCursorInfo(hwndDesktop : cardinal;var _CursorInfo : TCursorInfo) : longbool; cdecl;
    function WinQueryCursorInfo(hwndDesktop : cardinal;_pCursorInfo : PCursorInfo) : longbool; cdecl;
    function WinSetPointer(hwndDesktop,hptrNew : cardinal) : longbool; cdecl;
    function WinSetPointerOwner(hptr,pid : cardinal;fDestroy : longbool) : longbool; cdecl;
    function WinShowPointer(hwndDesktop : cardinal;fShow : longbool) : longbool; cdecl;
    function WinQuerySysPointer(hwndDesktop : cardinal;iptr : longint;fLoad : longbool) : cardinal; cdecl;
    function WinLoadPointer(hwndDesktop,hmod,idres : cardinal) : cardinal; cdecl;
    function WinCreatePointer(hwndDesktop,hbmPointer : cardinal;fPointer : longbool;xHotspot,yHotspot : longint) : cardinal; cdecl;
    function WinSetPointerPos(hwndDesktop : cardinal;x,y : longint) : longbool; cdecl;
    function WinDestroyPointer(hptr : cardinal) : longbool; cdecl;
    function WinQueryPointer(hwndDesktop : cardinal) : cardinal; cdecl;
    function WinQueryPointerPos(hwndDesktop : cardinal;var ptl : TPointL) : longbool; cdecl;
    function WinQueryPointerPos(hwndDesktop : cardinal;pptl : PPointL) : longbool; cdecl;
    function WinCreatePointerIndirect(hwndDesktop : cardinal;var ptri : TPointerInfo) : cardinal; cdecl;
    function WinCreatePointerIndirect(hwndDesktop : cardinal;pptri : PPointerInfo) : cardinal; cdecl;
    function WinQueryPointerInfo(hptr : cardinal;var _PointerInfo : TPointerInfo) : longbool; cdecl;
    function WinQueryPointerInfo(hptr : cardinal;_pPointerInfo : PPointerInfo) : longbool; cdecl;
    function WinDrawPointer(hps : cardinal;x,y : longint;hptr,fs : cardinal) : longbool; cdecl;
    function WinGetSysBitmap(hwndDesktop,ibm : cardinal) : cardinal; cdecl;
    function WinSetHook(hab : cardinal;hmq : cardinal;iHook : longint;pfnHook : pointer;hmod : cardinal) : longbool; cdecl;
    function WinReleaseHook(hab,hmq : cardinal;iHook : longint;pfnHook : pointer;hmod : cardinal) : longbool; cdecl;
    function WinCallMsgFilter(hab : cardinal;var _qmsg : TQMsg;msgf : cardinal) : longbool; cdecl;
    function WinCallMsgFilter(hab : cardinal;_pqmsg : PQMsg;msgf : cardinal) : longbool; cdecl;
    function WinSetClassThunkProc(pszClassname : pchar;pfnThunkProc : pointer) : longbool; cdecl;
    function WinQueryClassThunkProc(pszClassname : pchar) : pointer; cdecl;
    function WinSetWindowThunkProc(hwnd : cardinal;pfnThunkProc : pointer) : longbool; cdecl;
    function WinQueryWindowThunkProc(hwnd : cardinal) : pointer; cdecl;
    function WinQueryWindowModel(hwnd : cardinal) : longint; cdecl;
    function WinQueryCp(hmq : cardinal) : cardinal; cdecl;
    function WinSetCp(hmq,idCodePage : cardinal) : longbool; cdecl;
    function WinQueryCpList(hab,ccpMax : cardinal;var prgcp : cardinal) : cardinal; cdecl;
    function WinQueryCpList(hab,ccpMax : cardinal;prgcp : PCardinal) : cardinal; cdecl;
    function WinCpTranslateString(hab,cpSrc : cardinal;pszSrc : pchar;cpDst,cchDestMax : cardinal;pchDest : pchar) : longbool; cdecl;
    function WinCpTranslateChar(hab,cpSrc : cardinal;chSrc : byte;cpDst : cardinal) : byte; cdecl;
    function WinUpper(hab,idcp,idcc : cardinal;psz : pchar) : cardinal; cdecl;
    function WinUpperChar(hab,idcp,idcc,c : cardinal) : cardinal; cdecl;
    function WinNextChar(hab,idcp,idcc : cardinal;psz : pchar) : pchar; cdecl;
    function WinPrevChar(hab,idcp,idcc : cardinal;pszStart,psz : pchar) : pchar; cdecl;
    function WinCompareStrings(hab,idcp,idcc : cardinal;psz1,psz2 : pchar;reserved : cardinal) : cardinal; cdecl;
    function WinCreateAtomTable(cbInitial,cBuckets : cardinal) : cardinal; cdecl;
    function WinDestroyAtomTable(hAtomTbl : cardinal) : cardinal; cdecl;
    function WinAddAtom(hAtomTbl : cardinal;pszAtomName : pchar) : cardinal; cdecl;
    function WinFindAtom(hAtomTbl : cardinal;pszAtomName : pchar) : cardinal; cdecl;
    function WinDeleteAtom(hAtomTbl,atom : cardinal) : cardinal; cdecl;
    function WinQueryAtomUsage(hAtomTbl,atom : cardinal) : cardinal; cdecl;
    function WinQueryAtomLength(hAtomTbl,atom : cardinal) : cardinal; cdecl;
    function WinQueryAtomName(hAtomTbl,atom : cardinal;pchBuffer : pchar;cchBufferMax : cardinal) : cardinal; cdecl;
    function WinGetLastError(hab : cardinal) : cardinal; cdecl;
    function WinGetErrorInfo(hab : cardinal) : PERRINFO; cdecl;
    function WinFreeErrorInfo(var perrinfo : ERRINFO) : longbool; cdecl;
    {DDE Functions}
    function WinDdeInitiate(hwndClient : cardinal;pszAppName,pszTopicName : pchar;var cctxt : TConvContext) : longbool; cdecl;
    function WinDdeInitiate(hwndClient : cardinal;pszAppName,pszTopicName : pchar;pcctxt : PConvContext) : longbool; cdecl;
    function WinDdeRespond(hwndClient,hwndServer : cardinal;pszAppName,pszTopicName : pchar;var cctxt : TConvContext) : pointer; cdecl;
    function WinDdeRespond(hwndClient,hwndServer : cardinal;pszAppName,pszTopicName : pchar;pcctxt : PConvContext) : pointer; cdecl;
    function WinDdePostMsg(hwndTo,hwndFrom,wm : cardinal;var ddest : TDDEStruct;flOptions : cardinal) : longbool; cdecl;
    function WinDdePostMsg(hwndTo,hwndFrom,wm : cardinal;pddest : PDDEStruct;flOptions : cardinal) : longbool; cdecl;
    {Library related functions}
    function WinDeleteProcedure(hab : cardinal;wndproc : proc) : longbool; cdecl;
    function WinDeleteLibrary(hab,libhandle : cardinal) : longbool; cdecl;
    function WinLoadProcedure(hab,libhandle : cardinal;procname : pchar) : proc; cdecl;
    function WinLoadLibrary(hab : cardinal;libname : pchar) : cardinal; cdecl;
    function WinSetDesktopBkgnd(hwndDesktop : cardinal;var dskNew : TDesktop) : cardinal; cdecl;
    function WinSetDesktopBkgnd(hwndDesktop : cardinal;pdskNew : PDesktop) : cardinal; cdecl;
    function WinQueryDesktopBkgnd(hwndDesktop : cardinal;var dsk : TDesktop) : longbool; cdecl;
    function WinQueryDesktopBkgnd(hwndDesktop : cardinal;pdsk : PDesktop) : longbool; cdecl;
    function WinRealizePalette(hwnd,hps : cardinal;var cclr : cardinal) : longint; cdecl;
    function WinRealizePalette(hwnd,hps : cardinal;pcclr : PCardinal) : longint; cdecl;
    function WinQuerySystemAtomTable: cardinal; cdecl;
    function CardinalFromMP (MP: pointer): cardinal; cdecl;
    function Integer1FromMP (MP: pointer): word; cdecl;
    function Integer2FromMP (MP: pointer): word; cdecl;

const
  SEI_BREAKPOINT   =$8000;  // Always enter an INT 3 breakpt
  SEI_NOBEEP       =$4000;  // Do not call DosBeep
  SEI_NOPROMPT     =$2000;  // Do not prompt the user
  SEI_DBGRSRVD     =$1000;  // Reserved for debug use

  SEI_STACKTRACE   =$0001;  // save the stack trace
  SEI_REGISTERS    =$0002;  // save the registers
  SEI_ARGCOUNT     =$0004;  // first USHORT in args is arg count
  SEI_DOSERROR     =$0008;  // first USHORT in args is OS2 error code
  SEI_RESERVED     =$0FE0;  // Reserved for future use

  SEI_DEBUGONLY    = (SEI_BREAKPOINT or SEI_NOBEEP or SEI_NOPROMPT or SEI_RESERVED);

//****************************************************************************
//* Note that when SEI_ARGCOUNT, SEI_DOSERROR are specified
//* together, then the implied order of the parameters is:
//*
//*
//*  WinSetErrorInfo( MAKEERRORID( .... ),
//*                   SEI_ARGCOUNT | SEI_DOSERROR,
//*                   argCount,
//*                   dosErrorCode);
//*
//****************************************************************************/

//ERRORID APIENTRY WinSetErrorInfo(ERRORID, ULONG, ...);
Function WinSetErrorInfo(ErrID: ErrorID; Flags: Cardinal; Params: Array of const): ErrorID; external 'pmwin' index 263;

  implementation

    function WinRegisterClass(hab : cardinal;pszClassName : pchar;pfnWndProc : proc;flStyle,cbWindowData : cardinal) : longbool; cdecl;external 'pmwin' index 926;
    function WinDefWindowProc(hwnd,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;external 'pmwin' index 911;
    function WinDestroyWindow(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 728;
    function WinShowWindow(hwnd : cardinal;fShow : longbool) : longbool; cdecl;external 'pmwin' index 883;
    function WinQueryWindowRect(hwnd : cardinal;var rclDest : TRectl) : longbool; cdecl;external 'pmwin' index 840;
    function WinQueryWindowRect(hwnd : cardinal;prclDest : PRectl) : longbool; cdecl;external 'pmwin' index 840;
    function WinGetPS(hwnd : cardinal) : cardinal; cdecl;external 'pmwin' index 757;
    function WinReleasePS(hps : cardinal) : longbool; cdecl;external 'pmwin' index 848;
    function WinEndPaint(hps : cardinal) : longbool; cdecl;external 'pmwin' index 738;
    function WinGetClipPS(hwnd,hwndClip,fl : cardinal) : cardinal; cdecl;external 'pmwin' index 749;
    function WinIsWindowShowing(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 774;
    function WinBeginPaint(hwnd,hps : cardinal; var rclPaint : TRectl) : cardinal; cdecl;external 'pmwin' index 703;
    function WinBeginPaint(hwnd,hps : cardinal; prclPaint : PRectl) : cardinal; cdecl;external 'pmwin' index 703;
    function WinOpenWindowDC(hwnd : cardinal) : cardinal; cdecl;external 'pmwin' index 794;
    function WinScrollWindow(hwnd : cardinal;dx,dy : longint;var rclScroll,rclClip : TRectl;hrgnUpdate : cardinal;var rclUpdate : TRectl;rgfsw : cardinal) : longint; cdecl;external 'pmwin' index 849;
    function WinScrollWindow(hwnd : cardinal;dx,dy : longint;prclScroll,prclClip : PRectl;hrgnUpdate : cardinal;prclUpdate : PRectl;rgfsw : cardinal) : longint; cdecl;external 'pmwin' index 849;
    function WinFillRect(hps : cardinal;var rcl : TRectl;lColor : longint) : longbool; cdecl;external 'pmwin' index 743;
    function WinFillRect(hps : cardinal;prcl : PRectl;lColor : longint) : longbool; cdecl;external 'pmwin' index 743;
    function WinQueryVersion(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 833;
    function WinInitialize(flOptions : cardinal) : cardinal; cdecl;external 'pmwin' index 763;
    function WinTerminate(hab : cardinal) : longbool; cdecl;external 'pmwin' index 888;
    function WinQueryAnchorBlock(hwnd : cardinal) : cardinal; cdecl;external 'pmwin' index 800;
    function WinCreateWindow(hwndParent : cardinal;pszClass,pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;external 'pmwin' index 909;
    function WinCreateWindow(hwndParent : cardinal;pszClass : cardinal;pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;external 'pmwin' index 909;
    function WinCreateWCWindow(hwndParent : cardinal;pszClass : cardinal;pszName : pchar;flStyle : cardinal;x,y,cx,cy : longint;hwndOwner,hwndInsertBehind,id : cardinal;pCtlData,pPresParams : pointer) : cardinal; cdecl;external 'pmwin' index 909;
    function WinEnableWindow(hwnd : cardinal;fEnable : longbool) : longbool; cdecl;external 'pmwin' index 735;
    function WinIsWindowEnabled(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 773;
    function WinEnableWindowUpdate(hwnd : cardinal;fEnable : longbool) : longbool; cdecl;external 'pmwin' index 736;
    function WinIsWindowVisible(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 775;
    function WinQueryWindowText(hwnd : cardinal;cchBufferMax : longint; pchBuffer : pchar) : longint; cdecl;external 'pmwin' index 841;
    function WinSetWindowText(hwnd : cardinal;pszText : pchar) : longbool; cdecl;external 'pmwin' index 877;
    function WinQueryWindowTextLength(hwnd : cardinal) : longint; cdecl;external 'pmwin' index 842;
    function WinWindowFromID(hwndParent,id : cardinal) : cardinal; cdecl;external 'pmwin' index 899;
    function WinIsWindow(hab,hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 772;
    function WinQueryWindow(hwnd : cardinal;cmd : longint) : cardinal; cdecl;external 'pmwin' index 834;
    function WinMultWindowFromIDs(hwndParent : cardinal;var prghwnd : cardinal;idFirst,idLast : cardinal) : longint; cdecl;external 'pmwin' index 917;
    function WinMultWindowFromIDs(hwndParent : cardinal;prghwnd : PCardinal;idFirst,idLast : cardinal) : longint; cdecl;external 'pmwin' index 917;
    function WinSetParent(hwnd,hwndNewParent : cardinal;fRedraw : longbool) : longbool; cdecl;external 'pmwin' index 865;
    function WinIsChild(hwnd,hwndParent : cardinal) : longbool; cdecl;external 'pmwin' index 768;
    function WinSetOwner(hwnd,hwndNewOwner : cardinal) : longbool; cdecl;external 'pmwin' index 864;
    function WinQueryWindowProcess(hwnd : cardinal;var _pid,_tid : cardinal) : longbool; cdecl;external 'pmwin' index 838;
    function WinQueryWindowProcess(hwnd : cardinal;_ppid,_ptid : PCardinal) : longbool; cdecl;external 'pmwin' index 838;
    function WinQueryObjectWindow(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 820;
    function WinQueryDesktopWindow(hab,hdc : cardinal) : cardinal; cdecl;external 'pmwin' index 813;
    function WinSetWindowPos(hwnd,hwndInsertBehind : cardinal;x,y,cx,cy : longint;fl : cardinal) : longbool; cdecl;external 'pmwin' index 875;
    function WinSetMultWindowPos(hab : cardinal;var _swp : TSWP;cswp : cardinal) : longbool; cdecl;external 'pmwin' index 863;
    function WinSetMultWindowPos(hab : cardinal;_pswp : PSWP;cswp : cardinal) : longbool; cdecl;external 'pmwin' index 863;
    function WinQueryWindowPos(hwnd : cardinal;var _swp : TSWP) : longbool; cdecl;external 'pmwin' index 837;
    function WinQueryWindowPos(hwnd : cardinal;_pswp : PSWP) : longbool; cdecl;external 'pmwin' index 837;
    function WinUpdateWindow(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 892;
    function WinInvalidateRect(hwnd : cardinal;var wrc : TRectl;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 765;
    function WinInvalidateRect(hwnd : cardinal;pwrc : PRectl;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 765;
    function WinInvalidateRegion(hwnd,hrgn : cardinal;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 766;
    function WinInvertRect(hps : cardinal;var rcl : TRectl) : longbool; cdecl;external 'pmwin' index 767;
    function WinInvertRect(hps : cardinal;prcl : PRectl) : longbool; cdecl;external 'pmwin' index 767;
    function WinDrawBitmap(hpsDst,hbm : cardinal;var wrcSrc : TRectl;var ptlDst : TPointL;clrFore,clrBack : longint;fl : cardinal) : longbool; cdecl;external 'pmwin' index 730;
    function WinDrawBitmap(hpsDst,hbm : cardinal;pwrcSrc : PRectl;pptlDst : PPointL;clrFore,clrBack : longint;fl : cardinal) : longbool; cdecl;external 'pmwin' index 730;
    function WinDrawText(hps : cardinal;cchText : longint;lpchText : pchar;var rcl : TRectl;clrFore,clrBack : longint;flCmd : cardinal) : longint; cdecl;external 'pmwin' index 913;
    function WinDrawText(hps : cardinal;cchText : longint;lpchText : pchar;prcl : PRectl;clrFore,clrBack : longint;flCmd : cardinal) : longint; cdecl;external 'pmwin' index 913;
    function WinDrawBorder(hps : cardinal;var rcl : TRectl;cx,cy,clrFore,clrBack : longint;flCmd : cardinal) : longbool; cdecl;external 'pmwin' index 731;
    function WinDrawBorder(hps : cardinal;prcl : PRectl;cx,cy,clrFore,clrBack : longint;flCmd : cardinal) : longbool; cdecl;external 'pmwin' index 731;
    function WinLoadString(hab,hmod,id : cardinal;cchMax : longint;pchBuffer : pchar) : longint; cdecl;external 'pmwin' index 781;
    function WinLoadMessage(hab,hmod,id : cardinal;cchMax : longint;pchBuffer : pchar) : longint; cdecl;external 'pmwin' index 779;
    function WinSetActiveWindow(hwndDesktop,hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 851;
    function WinSubclassWindow(hwnd : cardinal;pfnwp : proc) : proc; cdecl;external 'pmwin' index 929;
    function WinQueryClassName(hwnd : cardinal;cchMax : longint; pch : pchar) : longint; cdecl;external 'pmwin' index 805;
    function WinQueryClassInfo(hab : cardinal;pszClassName : pchar;var _ClassInfo : TClassInfo) : longbool; cdecl;external 'pmwin' index 925;
    function WinQueryClassInfo(hab : cardinal;pszClassName : pchar;_pClassInfo : PClassInfo) : longbool; cdecl;external 'pmwin' index 925;
    function WinQueryActiveWindow(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 799;
    function WinIsThreadActive(hab : cardinal) : longbool; cdecl;external 'pmwin' index 771;
    function WinQuerySysModalWindow(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 827;
    function WinSetSysModalWindow(hwndDesktop,hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 872;
    function WinQueryWindowUShort(hwnd : cardinal;index : longint) : word; cdecl;external 'pmwin' index 844;
    function WinSetWindowUShort(hwnd : cardinal;index : longint;us : word) : longbool; cdecl;external 'pmwin' index 879;
    function WinQueryWindowULong(hwnd : cardinal;index : longint) : cardinal; cdecl;external 'pmwin' index 843;
    function WinSetWindowULong(hwnd : cardinal;index : longint;ul : cardinal) : longbool; cdecl;external 'pmwin' index 878;
    function WinQueryWindowPtr(hwnd : cardinal;index : longint) : pointer; cdecl;external 'pmwin' index 839;
    function WinSetWindowPtr(hwnd : cardinal;index : longint;p : pointer) : longbool; cdecl;external 'pmwin' index 876;
    function WinSetWindowBits(hwnd : cardinal;index : longint;flData,flMask : cardinal) : longbool; cdecl;external 'pmwin' index 874;
    function WinBeginEnumWindows(hwnd : cardinal) : cardinal; cdecl;external 'pmwin' index 702;
    function WinGetNextWindow(henum : cardinal) : cardinal; cdecl;external 'pmwin' index 756;
    function WinEndEnumWindows(henum : cardinal) : longbool; cdecl;external 'pmwin' index 737;
    function WinWindowFromPoint(hwnd : cardinal;var ptl : TPointL;fChildren : longbool) : cardinal; cdecl;external 'pmwin' index 900;
    function WinWindowFromPoint(hwnd : cardinal;pptl : PPointL;fChildren : longbool) : cardinal; cdecl;external 'pmwin' index 900;
    function WinMapWindowPoints(hwndFrom,hwndTo : cardinal;var prgptl : TPointL;cwpt : longint) : longbool; cdecl;external 'pmwin' index 788;
    function WinMapWindowPoints(hwndFrom,hwndTo : cardinal;prgptl : PPointL;cwpt : longint) : longbool; cdecl;external 'pmwin' index 788;
    function WinValidateRect(hwnd : cardinal;var rcl : TRectl;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 895;
    function WinValidateRect(hwnd : cardinal;prcl : PRectl;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 895;
    function WinValidateRegion(hwnd,hrgn : cardinal;fIncludeChildren : longbool) : longbool; cdecl;external 'pmwin' index 896;
    function WinWindowFromDC(hdc : cardinal) : cardinal; cdecl;external 'pmwin' index 898;
    function WinQueryWindowDC(hwnd : cardinal) : cardinal; cdecl;external 'pmwin' index 835;
    function WinGetScreenPS(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 759;
    function WinLockWindowUpdate(hwndDesktop,hwndLockUpdate : cardinal) : longbool; cdecl;external 'pmwin' index 784;
    function WinLockVisRegions(hwndDesktop : cardinal;fLock : longbool) : longbool; cdecl;external 'pmwin' index 782;
    function WinQueryUpdateRect(hwnd : cardinal;var rcl : TRectl) : longbool; cdecl;external 'pmwin' index 831;
    function WinQueryUpdateRect(hwnd : cardinal;prcl : PRectl) : longbool; cdecl;external 'pmwin' index 831;
    function WinQueryUpdateRegion(hwnd,hrgn : cardinal) : longint; cdecl;external 'pmwin' index 832;
    function WinExcludeUpdateRegion(hps,hwnd : cardinal) : longint; cdecl;external 'pmwin' index 742;
    function WinSendMsg(hwnd,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;external 'pmwin' index 920;
    function WinCreateMsgQueue(hab : cardinal;cmsg : longint) : cardinal; cdecl;external 'pmwin' index 716;
    function WinDestroyMsgQueue(hmq : cardinal) : longbool; cdecl;external 'pmwin' index 726;
    function WinQueryQueueInfo(hmq : cardinal;var mqi : TMQInfo;cbCopy : cardinal) : longbool; cdecl;external 'pmwin' index 824;
    function WinQueryQueueInfo(hmq : cardinal;pmqi : PMQInfo;cbCopy : cardinal) : longbool; cdecl;external 'pmwin' index 824;
    function WinCancelShutdown(hmq : cardinal;fCancelAlways : longbool) : longbool; cdecl;external 'pmwin' index 705;
    function WinGetMsg(hab : cardinal;var _qmsg : TQMsg;hwndFilter,msgFilterFirst,msgFilterLast : cardinal) : longbool; cdecl;external 'pmwin' index 915;
    function WinGetMsg(hab : cardinal;_pqmsg : PQMsg;hwndFilter,msgFilterFirst,msgFilterLast : cardinal) : longbool; cdecl;external 'pmwin' index 915;
    function WinPeekMsg(hab : cardinal;var _qmsg : TQMsg;hwndFilter,msgFilterFirst,msgFilterLast,fl : cardinal) : longbool; cdecl;external 'pmwin' index 918;
    function WinPeekMsg(hab : cardinal;_pqmsg : PQMsg;hwndFilter,msgFilterFirst,msgFilterLast,fl : cardinal) : longbool; cdecl;external 'pmwin' index 918;
    function WinDispatchMsg(hab : cardinal;var _qmsg : TQMsg) : pointer; cdecl;external 'pmwin' index 912;
    function WinDispatchMsg(hab : cardinal;_pqmsg : PQMsg) : pointer; cdecl;external 'pmwin' index 912;
    function WinPostMsg(hwnd,msg : cardinal;mp1,mp2 : pointer) : longbool; cdecl;external 'pmwin' index 919;
    function WinRegisterUserMsg(hab,msgid : cardinal;datatype1,dir1,datatype2,dir2,datatyper : longint) : longbool; cdecl;external 'pmwin' index 846;
    function WinRegisterUserDatatype(hab : cardinal;datatype,count : longint;var types : longint) : longbool; cdecl;external 'pmwin' index 845;
    function WinSetMsgMode(hab : cardinal;classname :pchar;control : longint) : longbool; cdecl;external 'pmwin' index 862;
    function WinSetSynchroMode(hab : cardinal;mode : longint) : longbool; cdecl;external 'pmwin' index 870;
    function WinInSendMsg(hab : cardinal) : longbool; cdecl;external 'pmwin' index 761;
    function WinBroadcastMsg(hwnd,msg : cardinal;mp1,mp2 : pointer;rgf : cardinal) : longbool; cdecl;external 'pmwin' index 901;
    function WinWaitMsg(hab,msgFirst,msgLast : cardinal) : longbool; cdecl;external 'pmwin' index 897;
    function WinQueryQueueStatus(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 825;
    function WinQueryMsgPos(hab : cardinal;var ptl : TPointL) : longbool; cdecl;external 'pmwin' index 818;
    function WinQueryMsgPos(hab : cardinal;pptl : PPointL) : longbool; cdecl;external 'pmwin' index 818;
    function WinQueryMsgTime(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 819;
    function WinWaitEventSem(hev,ulTimeout : cardinal) : cardinal; cdecl;external 'pmwin' index 978;
    function WinRequestMutexSem(hmtx,ulTimeout : cardinal) : cardinal; cdecl;external 'pmwin' index 979;
    function WinWaitMuxWaitSem(hmux,ulTimeout : cardinal;var ulUser : cardinal) : cardinal; cdecl;external 'pmwin' index 980;
    function WinWaitMuxWaitSem(hmux,ulTimeout : cardinal;pulUser : PCardinal) : cardinal; cdecl;external 'pmwin' index 980;
    function WinPostQueueMsg(hmq,msg : cardinal;mp1,mp2 : pointer) : longbool; cdecl;external 'pmwin' index 902;
    function WinSetMsgInterest(hwnd,msg_class : cardinal;control : longint) : longbool; cdecl;external 'pmwin' index 861;
    function WinSetClassMsgInterest(hab : cardinal;pszClassName : pchar;msg_class : cardinal;control : longint) : longbool; cdecl;external 'pmwin' index 853;
    function WinSetFocus(hwndDesktop,hwndSetFocus : cardinal) : longbool; cdecl;external 'pmwin' index 860;
    function WinFocusChange(hwndDesktop,hwndSetFocus,flFocusChange : cardinal) : longbool; cdecl;external 'pmwin' index 746;
    function WinSetCapture(hwndDesktop,hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 852;
    function WinQueryCapture(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 804;
    function WinQueryFocus(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 817;
    function WinGetKeyState(hwndDesktop : cardinal;vkey : longint) : longint; cdecl;external 'pmwin' index 752;
    function WinGetPhysKeyState(hwndDesktop : cardinal;sc : longint) : longint; cdecl;external 'pmwin' index 758;
    function WinEnablePhysInput(hwndDesktop : cardinal;fEnable : longbool) : longbool; cdecl;external 'pmwin' index 734;
    function WinIsPhysInputEnabled(hwndDesktop : cardinal) : longbool; cdecl;external 'pmwin' index 769;
    function WinSetKeyboardStateTable(hwndDesktop : cardinal;var KeyStateTable;fSet : longbool) : longbool; cdecl;external 'pmwin' index 921;
    function WinSetKeyboardStateTable(hwndDesktop : cardinal;pKeyStateTable : pointer;fSet : longbool) : longbool; cdecl;external 'pmwin' index 921;
    function WinGetDlgMsg(hwndDlg : cardinal;var _qmsg : TQMsg) : longbool; cdecl;external 'pmwin' index 914;
    function WinGetDlgMsg(hwndDlg : cardinal;_pqmsg : PQMsg) : longbool; cdecl;external 'pmwin' index 914;
    function WinLoadDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;hmod,idDlg : cardinal;pCreateParams : pointer) : cardinal; cdecl;external 'pmwin' index 924;
    function WinDlgBox(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;hmod,idDlg : cardinal;pCreateParams : pointer) : cardinal; cdecl;external 'pmwin' index 923;
    function WinDismissDlg(hwndDlg,usResult : cardinal) : longbool; cdecl;external 'pmwin' index 729;
    function WinQueryDlgItemShort(hwndDlg,idItem : cardinal;var _Result : integer;fSigned : longbool) : longbool; cdecl;external 'pmwin' index 814;
    function WinQueryDlgItemShort(hwndDlg,idItem : cardinal;pResult : PInteger;fSigned : longbool) : longbool; cdecl;external 'pmwin' index 814;
    function WinSetDlgItemShort(hwndDlg,idItem : cardinal;usValue : word;fSigned : longbool) : longbool; cdecl;external 'pmwin' index 858;
    function WinSetDlgItemText(hwndDlg,idItem : cardinal;pszText : pchar) : longbool; cdecl;external 'pmwin' index 859;
    function WinQueryDlgItemText(hwndDlg,idItem : cardinal;cchBufferMax : longint;pchBuffer : pchar) : cardinal; cdecl;external 'pmwin' index 815;
    function WinQueryDlgItemTextLength(hwndDlg,idItem : cardinal) : longint; cdecl;external 'pmwin' index 816;
    function WinDefDlgProc(hwndDlg,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;external 'pmwin' index 910;
    function WinAlarm(hwndDesktop,rgfType : cardinal) : longbool; cdecl;external 'pmwin' index 701;
    function WinMessageBox(hwndParent,hwndOwner : cardinal;pszText,pszCaption : pchar;idWindow,flStyle : cardinal) : cardinal; cdecl;external 'pmwin' index 789;
(* Only available in later OS/2 versions probably???
    function WinMessageBox2(hwndParent,hwndOwner: cardinal;pszText,pszCaption: PChar; idWindow: cardinal; MBInfo: PMB2Info): cardinal; cdecl; external 'pmwin' index 1015;
*)
    function WinProcessDlg(hwndDlg : cardinal) : cardinal; cdecl;external 'pmwin' index 796;
    function WinSendDlgItemMsg(hwndDlg,idItem,msg : cardinal;mp1,mp2 : pointer) : pointer; cdecl;external 'pmwin' index 903;
    function WinMapDlgPoints(hwndDlg : cardinal;var prgwptl : TPointL;cwpt : cardinal;fCalcWindowCoords : longbool) : longbool; cdecl;external 'pmwin' index 787;
    function WinMapDlgPoints(hwndDlg : cardinal;prgwptl : PPointL;cwpt : cardinal;fCalcWindowCoords : longbool) : longbool; cdecl;external 'pmwin' index 787;
    function WinEnumDlgItem(hwndDlg,hwnd,code : cardinal) : cardinal; cdecl;external 'pmwin' index 740;
    function WinSubstituteStrings(hwnd : cardinal;pszSrc : pchar;cchDstMax : longint;pszDst : pchar) : longint; cdecl;external 'pmwin' index 886;
    function WinCreateDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;var dlgt : TDlgTemplate;pCreateParams : pointer) : cardinal; cdecl;external 'pmwin' index 922;
    function WinCreateDlg(hwndParent,hwndOwner : cardinal;pfnDlgProc : proc;pdlgt : PDlgTemplate;pCreateParams : pointer) : cardinal; cdecl;external 'pmwin' index 922;
    function WinLoadMenu(hwndFrame,hmod,idMenu : cardinal) : cardinal; cdecl;external 'pmwin' index 778;
    function WinCreateMenu(hwndParent : cardinal;lpmt : pointer) : cardinal; cdecl;external 'pmwin' index 907;
    function WinPopupMenu(hwndParent,hwndOwner,hwndMenu : cardinal;x,y,idItem : longint;fs : cardinal) : longbool; cdecl;external 'pmwin' index 937;
    function WinCreateStdWindow(hwndParent,flStyle : cardinal;var flCreateFlags : cardinal;pszClientClass,pszTitle : pchar;styleClient,hmod,idResources : cardinal;var hwndClient : cardinal) : cardinal; cdecl;external 'pmwin' index 908;
    function WinCreateStdWindow(hwndParent,flStyle : cardinal;pflCreateFlags : PCardinal;pszClientClass,pszTitle : pchar;styleClient,hmod,idResources : cardinal;phwndClient : PCardinal) : cardinal; cdecl;external 'pmwin' index 908;
    function WinFlashWindow(hwndFrame : cardinal;fFlash : longbool) : longbool; cdecl;external 'pmwin' index 745;
    function WinCreateFrameControls(hwndFrame : cardinal;var fcdata : TFrameCData;pszTitle : pchar) : longbool; cdecl;external 'pmwin' index 906;
    function WinCreateFrameControls(hwndFrame : cardinal;pfcdata : PFrameCData;pszTitle : pchar) : longbool; cdecl;external 'pmwin' index 906;
    function WinCalcFrameRect(hwndFrame : cardinal;var rcl : TRectl;fClient : longbool) : longbool; cdecl;external 'pmwin' index 704;
    function WinCalcFrameRect(hwndFrame : cardinal;prcl : PRectl;fClient : longbool) : longbool; cdecl;external 'pmwin' index 704;
    function WinGetMinPosition(hwnd : cardinal;var _swp : TSWP;var pptl : POINTL) : longbool; cdecl;external 'pmwin' index 755;
    function WinGetMinPosition(hwnd : cardinal;_pswp : PSWP;var pptl : POINTL) : longbool; cdecl;external 'pmwin' index 755;
    function WinGetMaxPosition(hwnd : cardinal;var _swp : TSWP) : longbool; cdecl;external 'pmwin' index 754;
    function WinGetMaxPosition(hwnd : cardinal;_pswp : PSWP) : longbool; cdecl;external 'pmwin' index 754;
    function WinSaveWindowPos(hsvwp : cardinal;var _swp : TSWP;cswp : cardinal) : longbool; cdecl;external 'pmwin' index 943;
    function WinSaveWindowPos(hsvwp : cardinal;_pswp : PSWP;cswp : cardinal) : longbool; cdecl;external 'pmwin' index 943;
    function WinCopyRect(hab : cardinal;var rclDst,rclSrc : TRectl) : longbool; cdecl;external 'pmwin' index 710;
    function WinCopyRect(hab : cardinal;prclDst,prclSrc : PRectl) : longbool; cdecl;external 'pmwin' index 710;
    function WinSetRect(hab : cardinal;var rcl : TRectl;xLeft,yBottom,xRight,yTop : longint) : longbool; cdecl;external 'pmwin' index 868;
    function WinSetRect(hab : cardinal;_prcl : PRectl;xLeft,yBottom,xRight,yTop : longint) : longbool; cdecl;external 'pmwin' index 868;
    function WinIsRectEmpty(hab : cardinal;var rcl : TRectl) : longbool; cdecl;external 'pmwin' index 770;
    function WinIsRectEmpty(hab : cardinal;prcl : PRectl) : longbool; cdecl;external 'pmwin' index 770;
    function WinEqualRect(hab : cardinal;var rcl1,rcl2 : TRectl) : longbool; cdecl;external 'pmwin' index 741;
    function WinEqualRect(hab : cardinal;prcl1,prcl2 : PRectl) : longbool; cdecl;external 'pmwin' index 741;
    function WinSetRectEmpty(hab : cardinal;var rcl : TRectl) : longbool; cdecl;external 'pmwin' index 869;
    function WinSetRectEmpty(hab : cardinal;prcl : PRectl) : longbool; cdecl;external 'pmwin' index 869;
    function WinOffsetRect(hab : cardinal;var rcl : TRectl;cx,cy : longint) : longbool; cdecl;external 'pmwin' index 792;
    function WinOffsetRect(hab : cardinal;prcl : PRectl;cx,cy : longint) : longbool; cdecl;external 'pmwin' index 792;
    function WinInflateRect(hab : cardinal;var rcl : TRectl;cx,cy : longint) : longbool; cdecl;external 'pmwin' index 762;
    function WinInflateRect(hab : cardinal;prcl : PRectl;cx,cy : longint) : longbool; cdecl;external 'pmwin' index 762;
    function WinPtInRect(hab : cardinal;var rcl : TRectl;var ptl : TPointL) : longbool; cdecl;external 'pmwin' index 797;
    function WinPtInRect(hab : cardinal;prcl : PRectl;pptl : PPointL) : longbool; cdecl;external 'pmwin' index 797;
    function WinIntersectRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;external 'pmwin' index 764;
    function WinIntersectRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;external 'pmwin' index 764;
    function WinUnionRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;external 'pmwin' index 891;
    function WinUnionRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;external 'pmwin' index 891;
    function WinSubtractRect(hab : cardinal;var rclDst,rclSrc1,rclSrc2 : TRectl) : longbool; cdecl;external 'pmwin' index 887;
    function WinSubtractRect(hab : cardinal;prclDst,prclSrc1,prclSrc2 : PRectl) : longbool; cdecl;external 'pmwin' index 887;
    function WinMakeRect(hab : cardinal;var wrc : TRectl) : longbool; cdecl;external 'pmwin' index 786;
    function WinMakeRect(hab : cardinal;pwrc : PRectl) : longbool; cdecl;external 'pmwin' index 786;
    function WinMakePoints(hab : cardinal;var wpt : TPointL;cwpt : cardinal) : longbool; cdecl;external 'pmwin' index 785;
    function WinMakePoints(hab : cardinal;pwpt : PPointL;cwpt : cardinal) : longbool; cdecl;external 'pmwin' index 785;
    function WinQuerySysValue(hwndDesktop : cardinal;iSysValue : longint) : longint; cdecl;external 'pmwin' index 829;
    function WinSetSysValue(hwndDesktop : cardinal;iSysValue,lValue : longint) : longbool; cdecl;external 'pmwin' index 873;
    function WinSetPresParam(hwnd,id,cbParam : cardinal;pbParam : pointer) : longbool; cdecl;external 'pmwin' index 938;
    function WinQueryPresParam(hwnd,id1,id2 : cardinal;var ulId : cardinal;cbBuf : cardinal;pbBuf : pointer;fs : cardinal) : cardinal; cdecl;external 'pmwin' index 939;
    function WinQueryPresParam(hwnd,id1,id2 : cardinal;pulId : PCardinal;cbBuf : cardinal;pbBuf : pointer;fs : cardinal) : cardinal; cdecl;external 'pmwin' index 939;
    function WinRemovePresParam(hwnd,id : cardinal) : longbool; cdecl;external 'pmwin' index 940;
    function WinQuerySysColor(hwndDesktop : cardinal;clr,lReserved : longint) : longint; cdecl;external 'pmwin' index 826;
    function WinSetSysColors(hwndDesktop,flOptions,flFormat : cardinal;clrFirst : longint;cclr : cardinal;var clr : longint) : longbool; cdecl;external 'pmwin' index 871;
    function WinSetSysColors(hwndDesktop,flOptions,flFormat : cardinal;clrFirst : longint;cclr : cardinal;pclr : PLongint) : longbool; cdecl;external 'pmwin' index 871;
    function WinStartTimer(hab,hwnd,idTimer,dtTimeout : cardinal) : cardinal; cdecl;external 'pmwin' index 884;
    function WinStopTimer(hab,hwnd,idTimer : cardinal) : longbool; cdecl;external 'pmwin' index 885;
    function WinGetCurrentTime(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 750;
    function WinLoadAccelTable(hab,hmod,idAccelTable : cardinal) : cardinal; cdecl;external 'pmwin' index 776;
    function WinCopyAccelTable(haccel : cardinal;var _AccelTable : TAccelTable;cbCopyMax : cardinal) : cardinal; cdecl;external 'pmwin' index 709;
    function WinCopyAccelTable(haccel : cardinal;_pAccelTable : PAccelTable;cbCopyMax : cardinal) : cardinal; cdecl;external 'pmwin' index 709;
    function WinCreateAccelTable(hab : cardinal;var _AccelTable : TAccelTable) : cardinal; cdecl;external 'pmwin' index 713;
    function WinCreateAccelTable(hab : cardinal;_pAccelTable : PAccelTable) : cardinal; cdecl;external 'pmwin' index 713;
    function WinDestroyAccelTable(haccel : cardinal) : longbool; cdecl;external 'pmwin' index 723;
    function WinTranslateAccel(hab,hwnd,haccel : cardinal;var _qmsg : TQMsg) : longbool; cdecl;external 'pmwin' index 904;
    function WinTranslateAccel(hab,hwnd,haccel : cardinal;_pqmsg : PQMsg) : longbool; cdecl;external 'pmwin' index 904;
    function WinSetAccelTable(hab,haccel,hwndFrame : cardinal) : longbool; cdecl;external 'pmwin' index 850;
    function WinQueryAccelTable(hab,hwndFrame : cardinal) : cardinal; cdecl;external 'pmwin' index 798;
    function WinTrackRect(hwnd,hps : cardinal;var ti : TTrackInfo) : longbool; cdecl;external 'pmwin' index 890;
    function WinTrackRect(hwnd,hps : cardinal;pti : PTrackInfo) : longbool; cdecl;external 'pmwin' index 890;
    function WinShowTrackRect(hwnd : cardinal;fShow : longbool) : longbool; cdecl;external 'pmwin' index 882;
    function WinSetClipbrdOwner(hab,hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 855;
    function WinSetClipbrdData(hab,ulData,fmt,rgfFmtInfo : cardinal) : longbool; cdecl;external 'pmwin' index 854;
    function WinQueryClipbrdData(hab,fmt : cardinal) : cardinal; cdecl;external 'pmwin' index 806;
    function WinQueryClipbrdFmtInfo(hab,fmt : cardinal;var prgfFmtInfo : cardinal) : longbool; cdecl;external 'pmwin' index 807;
    function WinQueryClipbrdFmtInfo(hab,fmt : cardinal;prgfFmtInfo : PCardinal) : longbool; cdecl;external 'pmwin' index 807;
    function WinSetClipbrdViewer(hab,hwndNewClipViewer : cardinal) : longbool; cdecl;external 'pmwin' index 856;
    function WinEnumClipbrdFmts(hab,fmt : cardinal) : cardinal; cdecl;external 'pmwin' index 739;
    function WinEmptyClipbrd(hab : cardinal) : longbool; cdecl;external 'pmwin' index 733;
    function WinOpenClipbrd(hab : cardinal) : longbool; cdecl;external 'pmwin' index 793;
    function WinCloseClipbrd(hab : cardinal) : longbool; cdecl;external 'pmwin' index 707;
    function WinQueryClipbrdOwner(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 808;
    function WinQueryClipbrdViewer(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 809;
    function WinDestroyCursor(hwnd : cardinal) : longbool; cdecl;external 'pmwin' index 725;
    function WinShowCursor(hwnd : cardinal;fShow : longbool) : longbool; cdecl;external 'pmwin' index 880;
    function WinCreateCursor(hwnd : cardinal;x,y,cx,cy : longint;fs : cardinal;var rclClip : TRectl) : longbool; cdecl;external 'pmwin' index 715;
    function WinCreateCursor(hwnd : cardinal;x,y,cx,cy : longint;fs : cardinal;prclClip : PRectl) : longbool; cdecl;external 'pmwin' index 715;
    function WinQueryCursorInfo(hwndDesktop : cardinal;var _CursorInfo : TCursorInfo) : longbool; cdecl;external 'pmwin' index 812;
    function WinQueryCursorInfo(hwndDesktop : cardinal;_pCursorInfo : PCursorInfo) : longbool; cdecl;external 'pmwin' index 812;
    function WinSetPointer(hwndDesktop,hptrNew : cardinal) : longbool; cdecl;external 'pmwin' index 866;
    function WinSetPointerOwner(hptr,pid : cardinal;fDestroy : longbool) : longbool; cdecl;external 'pmwin' index 971;
    function WinShowPointer(hwndDesktop : cardinal;fShow : longbool) : longbool; cdecl;external 'pmwin' index 881;
    function WinQuerySysPointer(hwndDesktop : cardinal;iptr : longint;fLoad : longbool) : cardinal; cdecl;external 'pmwin' index 828;
    function WinLoadPointer(hwndDesktop,hmod,idres : cardinal) : cardinal; cdecl;external 'pmwin' index 780;
    function WinCreatePointer(hwndDesktop,hbmPointer : cardinal;fPointer : longbool;xHotspot,yHotspot : longint) : cardinal; cdecl;external 'pmwin' index 717;
    function WinSetPointerPos(hwndDesktop : cardinal;x,y : longint) : longbool; cdecl;external 'pmwin' index 867;
    function WinDestroyPointer(hptr : cardinal) : longbool; cdecl;external 'pmwin' index 727;
    function WinQueryPointer(hwndDesktop : cardinal) : cardinal; cdecl;external 'pmwin' index 821;
    function WinQueryPointerPos(hwndDesktop : cardinal;var ptl : TPointL) : longbool; cdecl;external 'pmwin' index 823;
    function WinQueryPointerPos(hwndDesktop : cardinal;pptl : PPointL) : longbool; cdecl;external 'pmwin' index 823;
    function WinCreatePointerIndirect(hwndDesktop : cardinal;var ptri : TPointerInfo) : cardinal; cdecl;external 'pmwin' index 942;
    function WinCreatePointerIndirect(hwndDesktop : cardinal;pptri : PPointerInfo) : cardinal; cdecl;external 'pmwin' index 942;
    function WinQueryPointerInfo(hptr : cardinal;var _PointerInfo : TPointerInfo) : longbool; cdecl;external 'pmwin' index 822;
    function WinQueryPointerInfo(hptr : cardinal;_pPointerInfo : PPointerInfo) : longbool; cdecl;external 'pmwin' index 822;
    function WinDrawPointer(hps : cardinal;x,y : longint;hptr,fs : cardinal) : longbool; cdecl;external 'pmwin' index 732;
    function WinGetSysBitmap(hwndDesktop,ibm : cardinal) : cardinal; cdecl;external 'pmwin' index 760;
    function WinSetHook(hab,hmq : cardinal;iHook : longint;pfnHook : pointer;hmod : cardinal) : longbool; cdecl;external 'pmwin' index 928;
    function WinReleaseHook(hab,hmq : cardinal;iHook : longint;pfnHook : pointer;hmod : cardinal) : longbool; cdecl;external 'pmwin' index 927;
    function WinCallMsgFilter(hab : cardinal;var _qmsg : TQMsg;msgf : cardinal) : longbool; cdecl;external 'pmwin' index 905;
    function WinCallMsgFilter(hab : cardinal;_pqmsg : PQMsg;msgf : cardinal) : longbool; cdecl;external 'pmwin' index 905;
    function WinSetClassThunkProc(pszClassname : pchar;pfnThunkProc : pointer) : longbool; cdecl;external 'pmwin' index 959;
    function WinQueryClassThunkProc(pszClassname : pchar) : pointer; cdecl;external 'pmwin' index 960;
    function WinSetWindowThunkProc(hwnd : cardinal;pfnThunkProc : pointer) : longbool; cdecl;external 'pmwin' index 961;
    function WinQueryWindowThunkProc(hwnd : cardinal) : pointer; cdecl;external 'pmwin' index 962;
    function WinQueryWindowModel(hwnd : cardinal) : longint; cdecl;external 'pmwin' index 934;
    function WinQueryCp(hmq : cardinal) : cardinal; cdecl;external 'pmwin' index 810;
    function WinSetCp(hmq,idCodePage : cardinal) : longbool; cdecl;external 'pmwin' index 857;
    function WinQueryCpList(hab,ccpMax : cardinal;var prgcp : cardinal) : cardinal; cdecl;external 'pmwin' index 811;
    function WinQueryCpList(hab,ccpMax : cardinal;prgcp : PCardinal) : cardinal; cdecl;external 'pmwin' index 811;
    function WinCpTranslateString(hab,cpSrc : cardinal;pszSrc : pchar;cpDst,cchDestMax : cardinal;pchDest : pchar) : longbool; cdecl;external 'pmwin' index 712;
    function WinCpTranslateChar(hab,cpSrc : cardinal;chSrc : byte;cpDst : cardinal) : byte; cdecl;external 'pmwin' index 711;
    function WinUpper(hab,idcp,idcc : cardinal;psz : pchar) : cardinal; cdecl;external 'pmwin' index 893;
    function WinUpperChar(hab,idcp,idcc,c : cardinal) : cardinal; cdecl;external 'pmwin' index 894;
    function WinNextChar(hab,idcp,idcc : cardinal;psz : pchar) : pchar; cdecl;external 'pmwin' index 791;
    function WinPrevChar(hab,idcp,idcc : cardinal;pszStart,psz : pchar) : pchar; cdecl;external 'pmwin' index 795;
    function WinCompareStrings(hab,idcp,idcc : cardinal;psz1,psz2 : pchar;reserved : cardinal) : cardinal; cdecl;external 'pmwin' index 708;
    function WinCreateAtomTable(cbInitial,cBuckets : cardinal) : cardinal; cdecl;external 'pmwin' index 714;
    function WinDestroyAtomTable(hAtomTbl : cardinal) : cardinal; cdecl;external 'pmwin' index 724;
    function WinAddAtom(hAtomTbl : cardinal;pszAtomName : pchar) : cardinal; cdecl;external 'pmwin' index 700;
    function WinFindAtom(hAtomTbl : cardinal;pszAtomName : pchar) : cardinal; cdecl;external 'pmwin' index 744;
    function WinDeleteAtom(hAtomTbl,atom : cardinal) : cardinal; cdecl;external 'pmwin' index 721;
    function WinQueryAtomUsage(hAtomTbl,atom : cardinal) : cardinal; cdecl;external 'pmwin' index 803;
    function WinQueryAtomLength(hAtomTbl,atom : cardinal) : cardinal; cdecl;external 'pmwin' index 801;
    function WinQueryAtomName(hAtomTbl,atom : cardinal;pchBuffer : pchar;cchBufferMax : cardinal) : cardinal; cdecl;external 'pmwin' index 802;
    function WinGetLastError(hab : cardinal) : cardinal; cdecl;external 'pmwin' index 753;
    function WinGetErrorInfo(hab : cardinal) : PERRINFO; cdecl;external 'pmwin' index 751;
    function WinFreeErrorInfo(var perrinfo : ERRINFO) : longbool; cdecl;external 'pmwin' index 748;
    function WinDdeInitiate(hwndClient : cardinal;pszAppName,pszTopicName : pchar;var cctxt : TConvContext) : longbool; cdecl;external 'pmwin' index 718;
    function WinDdeInitiate(hwndClient : cardinal;pszAppName,pszTopicName : pchar;pcctxt : PConvContext) : longbool; cdecl;external 'pmwin' index 718;
    function WinDdeRespond(hwndClient,hwndServer : cardinal;pszAppName,pszTopicName : pchar;var cctxt : TConvContext) : pointer; cdecl;external 'pmwin' index 720;
    function WinDdeRespond(hwndClient,hwndServer : cardinal;pszAppName,pszTopicName : pchar;pcctxt : PConvContext) : pointer; cdecl;external 'pmwin' index 720;
    function WinDdePostMsg(hwndTo,hwndFrom,wm : cardinal;var ddest : DDEStruct;flOptions : cardinal) : longbool; cdecl;external 'pmwin' index 719;
    function WinDdePostMsg(hwndTo,hwndFrom,wm : cardinal;pddest : PDDEStruct;flOptions : cardinal) : longbool; cdecl;external 'pmwin' index 719;
    function WinDeleteProcedure(hab : cardinal;wndproc : proc) : longbool; cdecl;external 'pmwin' index 987;
    function WinDeleteLibrary(hab,libhandle : cardinal) : longbool; cdecl;external 'pmwin' index 722;
    function WinLoadProcedure(hab,libhandle : cardinal;procname : pchar) : proc; cdecl;external 'pmwin' index 986;
    function WinLoadLibrary(hab : cardinal;libname : pchar) : cardinal; cdecl;external 'pmwin' index 777;
    function WinSetDesktopBkgnd(hwndDesktop : cardinal;var dskNew : TDesktop) : cardinal; cdecl;external 'pmwin' index 935;
    function WinSetDesktopBkgnd(hwndDesktop : cardinal;pdskNew : PDesktop) : cardinal; cdecl;external 'pmwin' index 935;
    function WinQueryDesktopBkgnd(hwndDesktop : cardinal;var dsk : TDesktop) : longbool; cdecl;external 'pmwin' index 936;
    function WinQueryDesktopBkgnd(hwndDesktop : cardinal;pdsk : PDesktop) : longbool; cdecl;external 'pmwin' index 936;
    function WinRealizePalette(hwnd,hps : cardinal;var cclr : cardinal) : longint; cdecl;external 'pmwin' index 941;
    function WinRealizePalette(hwnd,hps : cardinal;pcclr : PCardinal) : longint; cdecl;external 'pmwin' index 941;
    function WinQuerySystemAtomTable: cardinal; cdecl; external 'pmwin' index 830;

    function CardinalFromMP (MP: pointer): cardinal; cdecl;
     begin
      CardinalFromMP := cardinal (MP);
     end;

    function Integer1FromMP (MP: pointer): word; cdecl;
     begin
      Integer1FromMP := Lo (cardinal (MP));
     end;

    function Integer2FromMP (MP: pointer): word; cdecl;
     begin
      Integer2FromMP := Hi (cardinal (MP));
     end;

end.
