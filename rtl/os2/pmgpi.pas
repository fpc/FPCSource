{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit pmgpi;

  interface
  
    uses
       os2def,pmbitmap;

    const
       GPI_ERROR = 0;
       GPI_OK = 1;
       GPI_ALTERROR = (-1);

    type
       FIXED = LONG;

       PFIXED = ^FIXED;

       FIXED88 = USHORT;

       FIXED114 = USHORT;

       SIZEL = record
          cx : LONG;
          cy : LONG;
       end;

       PSIZEL = ^SIZEL;

    const
       CLR_NOINDEX = (-254);
       PU_ARBITRARY = $0004;
       PU_PELS = $0008;
       PU_LOMETRIC = $000C;
       PU_HIMETRIC = $0010;
       PU_LOENGLISH = $0014;
       PU_HIENGLISH = $0018;
       PU_TWIPS = $001C;
       GPIF_DEFAULT = 0;
       GPIF_SHORT = $0100;
       GPIF_LONG = $0200;
       GPIT_NORMAL = 0;
       GPIT_MICRO = $1000;
       GPIA_NOASSOC = 0;
       GPIA_ASSOC = $4000;
       HDC_ERROR = -1;

    function GpiCreatePS(hab : HAB;hdc : HDC;psizlSize : PSIZEL;flOptions : ULONG) : HPS;

    function GpiDestroyPS(hps : HPS) : BOOL;

    function GpiAssociate(hps : HPS;hdc : HDC) : BOOL;

    function GpiRestorePS(hps : HPS;lPSid : LONG) : BOOL;

    function GpiSavePS(hps : HPS) : LONG;

    function GpiErase(hps : HPS) : BOOL;

    function GpiQueryDevice(hps : HPS) : HDC;

    const
       GRES_ATTRS = $0001;
       GRES_SEGMENTS = $0002;
       GRES_ALL = $0004;
       PS_UNITS = $00FC;
       PS_FORMAT = $0F00;
       PS_TYPE = $1000;
       PS_MODE = $2000;
       PS_ASSOCIATE = $4000;
       PS_NORESET = $8000;
       GPIE_SEGMENT = 0;
       GPIE_ELEMENT = 1;
       GPIE_DATA = 2;
       DCTL_ERASE = 1;
       DCTL_DISPLAY = 2;
       DCTL_BOUNDARY = 3;
       DCTL_DYNAMIC = 4;
       DCTL_CORRELATE = 5;
       DCTL_ERROR = -1;
       DCTL_OFF = 0;
       DCTL_ON = 1;
       SDW_ERROR = -1;
       SDW_OFF = 0;
       SDW_ON = 1;
       DM_ERROR = 0;
       DM_DRAW = 1;
       DM_RETAIN = 2;
       DM_DRAWANDRETAIN = 3;

    function GpiResetPS(hps : HPS;flOptions : ULONG) : BOOL;

    function GpiSetPS(hps : HPS;psizlsize : PSIZEL;flOptions : ULONG) : BOOL;

    function GpiQueryPS(hps : HPS;psizlSize : PSIZEL) : ULONG;

    function GpiErrorSegmentData(hps : HPS;plSegment : PLONG;plContext : PLONG) : LONG;

    function GpiQueryDrawControl(hps : HPS;lControl : LONG) : LONG;

    function GpiSetDrawControl(hps : HPS;lControl : LONG;lValue : LONG) : BOOL;

    function GpiQueryDrawingMode(hps : HPS) : LONG;

    function GpiSetDrawingMode(hps : HPS;lMode : LONG) : BOOL;

    function GpiQueryStopDraw(hps : HPS) : LONG;

    function GpiSetStopDraw(hps : HPS;lValue : LONG) : BOOL;

    const
       PICKAP_DEFAULT = 0;
       PICKAP_REC = 2;
       PICKSEL_VISIBLE = 0;
       PICKSEL_ALL = 1;
       GPI_HITS = 2;

    function GpiCorrelateChain(hps : HPS;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;pl2 : PLONG) : LONG;

    function GpiQueryTag(hps : HPS;plTag : PLONG) : BOOL;

    function GpiSetTag(hps : HPS;lTag : LONG) : BOOL;

    function GpiQueryPickApertureSize(hps : HPS;psizlSize : PSIZEL) : BOOL;

    function GpiSetPickApertureSize(hps : HPS;lOptions : LONG;psizlSize : PSIZEL) : BOOL;

    function GpiQueryPickAperturePosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;

    function GpiSetPickAperturePosition(hps : HPS;pptlPick : PPOINTL) : BOOL;

    function GpiQueryBoundaryData(hps : HPS;prclBoundary : PRECTL) : BOOL;

    function GpiResetBoundaryData(hps : HPS) : BOOL;

    function GpiCorrelateFrom(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;plSegTag : PLONG) : LONG;

    function GpiCorrelateSegment(hps : HPS;lSegment : LONG;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;alSegTag : PLONG) : LONG;

    const
       DFORM_NOCONV = 0;
       DFORM_S370SHORT = 1;
       DFORM_PCSHORT = 2;
       DFORM_PCLONG = 4;
       ATTR_ERROR = (-1);
       ATTR_DETECTABLE = 1;
       ATTR_VISIBLE = 2;
       ATTR_CHAINED = 6;
       ATTR_DYNAMIC = 8;
       ATTR_FASTCHAIN = 9;
       ATTR_PROP_DETECTABLE = 10;
       ATTR_PROP_VISIBLE = 11;
       ATTR_OFF = 0;
       ATTR_ON = 1;
       LOWER_PRI = (-1);
       HIGHER_PRI = 1;

    function GpiOpenSegment(hps : HPS;lSegment : LONG) : BOOL;

    function GpiCloseSegment(hps : HPS) : BOOL;

    function GpiDeleteSegment(hps : HPS;lSegid : LONG) : BOOL;

    function GpiQueryInitialSegmentAttrs(hps : HPS;lAttribute : LONG) : LONG;

    function GpiSetInitialSegmentAttrs(hps : HPS;lAttribute : LONG;lValue : LONG) : BOOL;

    function GpiQuerySegmentAttrs(hps : HPS;lSegid : LONG;lAttribute : LONG) : LONG;

    function GpiSetSegmentAttrs(hps : HPS;lSegid : LONG;lAttribute : LONG;lValue : LONG) : BOOL;

    function GpiQuerySegmentPriority(hps : HPS;lRefSegid : LONG;lOrder : LONG) : LONG;

    function GpiSetSegmentPriority(hps : HPS;lSegid : LONG;lRefSegid : LONG;lOrder : LONG) : BOOL;

    function GpiDeleteSegments(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG) : BOOL;

    function GpiQuerySegmentNames(hps : HPS;lFirstSegid : LONG;lLastSegid : LONG;lMax : LONG;alSegids : PLONG) : LONG;

    function GpiGetData(hps : HPS;lSegid : LONG;plOffset : PLONG;lFormat : LONG;lLength : LONG;pbData : PBYTE) : LONG;

    function GpiPutData(hps : HPS;lFormat : LONG;plCount : PLONG;pbData : PBYTE) : LONG;

    function GpiDrawChain(hps : HPS) : BOOL;

    function GpiDrawFrom(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG) : BOOL;

    function GpiDrawSegment(hps : HPS;lSegment : LONG) : BOOL;

    function GpiDrawDynamics(hps : HPS) : BOOL;

    function GpiRemoveDynamics(hps : HPS;lFirstSegid : LONG;lLastSegid : LONG) : BOOL;

    const
       SEGEM_ERROR = 0;
       SEGEM_INSERT = 1;
       SEGEM_REPLACE = 2;

    function GpiBeginElement(hps : HPS;lType : LONG;pszDesc : PSZ) : BOOL;

    function GpiEndElement(hps : HPS) : BOOL;

    function GpiLabel(hps : HPS;lLabel : LONG) : BOOL;

    function GpiElement(hps : HPS;lType : LONG;pszDesc : PSZ;lLength : LONG;pbData : PBYTE) : LONG;

    function GpiQueryElement(hps : HPS;lOff : LONG;lMaxLength : LONG;pbData : PBYTE) : LONG;

    function GpiDeleteElement(hps : HPS) : BOOL;

    function GpiDeleteElementRange(hps : HPS;lFirstElement : LONG;lLastElement : LONG) : BOOL;

    function GpiDeleteElementsBetweenLabels(hps : HPS;lFirstLabel : LONG;lLastLabel : LONG) : BOOL;

    function GpiQueryEditMode(hps : HPS) : LONG;

    function GpiSetEditMode(hps : HPS;lMode : LONG) : BOOL;

    function GpiQueryElementPointer(hps : HPS) : LONG;

    function GpiSetElementPointer(hps : HPS;lElement : LONG) : BOOL;

    function GpiOffsetElementPointer(hps : HPS;loffset : LONG) : BOOL;

    function GpiQueryElementType(hps : HPS;plType : PLONG;lLength : LONG;pszData : PSZ) : LONG;

    function GpiSetElementPointerAtLabel(hps : HPS;lLabel : LONG) : BOOL;

    const
       CVTC_WORLD = 1;
       CVTC_MODEL = 2;
       CVTC_DEFAULTPAGE = 3;
       CVTC_PAGE = 4;
       CVTC_DEVICE = 5;
       TRANSFORM_REPLACE = 0;
       TRANSFORM_ADD = 1;
       TRANSFORM_PREEMPT = 2;

    type
       MATRIXLF = record
          fxM11 : FIXED;
          fxM12 : FIXED;
          lM13 : LONG;
          fxM21 : FIXED;
          fxM22 : FIXED;
          lM23 : LONG;
          lM31 : LONG;
          lM32 : LONG;
          lM33 : LONG;
       end;

       PMATRIXLF = ^MATRIXLF;


    function GpiQuerySegmentTransformMatrix(hps : HPS;lSegid : LONG;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;

    function GpiSetSegmentTransformMatrix(hps : HPS;lSegid : LONG;lCount : LONG;pmatlfarray : PMATRIXLF;lOptions : LONG) : BOOL;

    function GpiConvert(hps : HPS;lSrc : LONG;lTarg : LONG;lCount : LONG;aptlPoints : PPOINTL) : BOOL;

    function GpiConvertWithMatrix(hps : HPS;lCountp : LONG;aptlPoints : PPOINTL;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;

    function GpiQueryModelTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;

    function GpiSetModelTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : BOOL;

    function GpiCallSegmentMatrix(hps : HPS;lSegment : LONG;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : LONG;

    function GpiQueryDefaultViewMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;

    function GpiSetDefaultViewMatrix(hps : HPS;lCount : LONG;pmatlfarray : PMATRIXLF;lOptions : LONG) : BOOL;

    function GpiQueryPageViewport(hps : HPS;prclViewport : PRECTL) : BOOL;

    function GpiSetPageViewport(hps : HPS;prclViewport : PRECTL) : BOOL;

    function GpiQueryViewingTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;

    function GpiSetViewingTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : BOOL;

    function GpiTranslate(hps : HPS;pmatrixlf : PMATRIXLF;long : LONG;ppointl : PPOINTL) : BOOL;

    function GpiScale(hps : HPS;p1 : PMATRIXLF;p2 : LONG;p3 : PFIXED;p4 : PPOINTL) : BOOL;

    function GpiRotate(p1 : HPS;p2 : PMATRIXLF;p3 : LONG;p4 : FIXED;p5 : PPOINTL) : BOOL;

    function GpiSetGraphicsField(hps : HPS;prclField : PRECTL) : BOOL;

    function GpiQueryGraphicsField(hps : HPS;prclField : PRECTL) : BOOL;

    function GpiSetViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;

    function GpiQueryViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;

    const
       MPATH_STROKE = 6;
       FPATH_ALTERNATE = 0;
       FPATH_WINDING = 2;
       FPATH_EXCL = 0;
       FPATH_INCL = 8;
       SCP_ALTERNATE = 0;
       SCP_WINDING = 2;
       SCP_AND = 4;
       SCP_RESET = 0;
       SCP_EXCL = 0;
       SCP_INCL = 8;

    function GpiBeginPath(hps : HPS;lPath : LONG) : BOOL;

    function GpiEndPath(hps : HPS) : BOOL;

    function GpiCloseFigure(hps : HPS) : BOOL;

    function GpiModifyPath(hps : HPS;lPath : LONG;lMode : LONG) : BOOL;

    function GpiFillPath(hps : HPS;lPath : LONG;lOptions : LONG) : LONG;

    function GpiSetClipPath(hps : HPS;lPath : LONG;lOptions : LONG) : BOOL;

    function GpiOutlinePath(hps : HPS;lPath : LONG;lOptions : LONG) : LONG;

    function GpiPathToRegion(GpiH : HPS;lPath : LONG;lOptions : LONG) : HRGN;

    function GpiStrokePath(hps : HPS;lPath : LONG;flOptions : ULONG) : LONG;

    const
       LCOL_RESET = $0001;
       LCOL_REALIZABLE = $0002;
       LCOL_PURECOLOR = $0004;
       LCOL_OVERRIDE_DEFAULT_COLORS = $0008;
       LCOL_REALIZED = $0010;
       LCOLF_DEFAULT = 0;
       LCOLF_INDRGB = 1;
       LCOLF_CONSECRGB = 2;
       LCOLF_RGB = 3;
       LCOLF_PALETTE = 4;
       LCOLOPT_REALIZED = $0001;
       LCOLOPT_INDEX = $0002;
       QLCT_ERROR = (-1);
       QLCT_RGB = (-2);
       QLCT_NOTLOADED = (-1);
       QCD_LCT_FORMAT = 0;
       QCD_LCT_LOINDEX = 1;
       QCD_LCT_HIINDEX = 2;
       QCD_LCT_OPTIONS = 3;
       PAL_ERROR = (-1);
       PC_RESERVED = $01;
       PC_EXPLICIT = $02;
       PC_NOCOLLAPSE = $04;

    function GpiCreateLogColorTable(hps : HPS;flOptions : ULONG;lFormat : LONG;lStart : LONG;lCount : LONG;alTable : PLONG) : BOOL;

    function GpiQueryColorData(hps : HPS;lCount : LONG;alArray : PLONG) : BOOL;

    function GpiQueryLogColorTable(hps : HPS;flOptions : ULONG;lStart : LONG;lCount : LONG;alArray : PLONG) : LONG;

    function GpiQueryRealColors(hps : HPS;flOptions : ULONG;lStart : LONG;lCount : LONG;alColors : PLONG) : LONG;

    function GpiQueryNearestColor(hps : HPS;flOptions : ULONG;lRgbIn : LONG) : LONG;

    function GpiQueryColorIndex(hps : HPS;flOptions : ULONG;lRgbColor : LONG) : LONG;

    function GpiQueryRGBColor(hps : HPS;flOptions : ULONG;lColorIndex : LONG) : LONG;

    function GpiCreatePalette(hab : HAB;flOptions : ULONG;ulFormat : ULONG;ulCount : ULONG;aulTable : PULONG) : HPAL;

    function GpiDeletePalette(hpal : HPAL) : BOOL;

    function GpiSelectPalette(hps : HPS;hpal : HPAL) : HPAL;

    function GpiAnimatePalette(hpal : HPAL;ulFormat : ULONG;ulStart : ULONG;ulCount : ULONG;aulTable : PULONG) : LONG;

    function GpiSetPaletteEntries(hpal : HPAL;ulFormat : ULONG;ulStart : ULONG;ulCount : ULONG;aulTable : PULONG) : BOOL;

    function GpiQueryPalette(hps : HPS) : HPAL;

    function GpiQueryPaletteInfo(hpal : HPAL;hps : HPS;flOptions : ULONG;ulStart : ULONG;ulCount : ULONG;aulArray : PULONG) : LONG;

    const
       CLR_FALSE = (-5);
       CLR_TRUE = (-4);
       CLR_ERROR = (-255);
       CLR_DEFAULT = (-3);
       CLR_WHITE = (-2);
       CLR_BLACK = (-1);
       CLR_BACKGROUND = 0;
       CLR_BLUE = 1;
       CLR_RED = 2;
       CLR_PINK = 3;
       CLR_GREEN = 4;
       CLR_CYAN = 5;
       CLR_YELLOW = 6;
       CLR_NEUTRAL = 7;
       CLR_DARKGRAY = 8;
       CLR_DARKBLUE = 9;
       CLR_DARKRED = 10;
       CLR_DARKPINK = 11;
       CLR_DARKGREEN = 12;
       CLR_DARKCYAN = 13;
       CLR_BROWN = 14;
       CLR_PALEGRAY = 15;
       RGB_ERROR = (-255);
       RGB_BLACK = $00000000;
       RGB_BLUE = $000000FF;
       RGB_GREEN = $0000FF00;
       RGB_CYAN = $0000FFFF;
       RGB_RED = $00FF0000;
       RGB_PINK = $00FF00FF;
       RGB_YELLOW = $00FFFF00;
       RGB_WHITE = $00FFFFFF;
       BA_NOBOUNDARY = 0;
       BA_BOUNDARY = $0001;
       BA_ALTERNATE = 0;
       BA_WINDING = $0002;
       BA_EXCL = 0;
       BA_INCL = 8;
       DRO_FILL = 1;
       DRO_OUTLINE = 2;
       DRO_OUTLINEFILL = 3;
       PATSYM_ERROR = (-1);
       PATSYM_DEFAULT = 0;
       PATSYM_DENSE1 = 1;
       PATSYM_DENSE2 = 2;
       PATSYM_DENSE3 = 3;
       PATSYM_DENSE4 = 4;
       PATSYM_DENSE5 = 5;
       PATSYM_DENSE6 = 6;
       PATSYM_DENSE7 = 7;
       PATSYM_DENSE8 = 8;
       PATSYM_VERT = 9;
       PATSYM_HORIZ = 10;
       PATSYM_DIAG1 = 11;
       PATSYM_DIAG2 = 12;
       PATSYM_DIAG3 = 13;
       PATSYM_DIAG4 = 14;
       PATSYM_NOSHADE = 15;
       PATSYM_SOLID = 16;
       PATSYM_HALFTONE = 17;
       PATSYM_HATCH = 18;
       PATSYM_DIAGHATCH = 19;
       PATSYM_BLANK = 64;
       LCID_ERROR = (-1);
       LCID_DEFAULT = 0;

    function GpiSetColor(hps : HPS;lColor : LONG) : BOOL;

    function GpiQueryColor(hps : HPS) : LONG;

    function GpiBox(hps : HPS;lControl : LONG;pptlPoint : PPOINTL;lHRound : LONG;lVRound : LONG) : LONG;

    function GpiMove(hps : HPS;pptlPoint : PPOINTL) : BOOL;

    function GpiLine(hps : HPS;pptlEndPoint : PPOINTL) : LONG;

    function GpiPolyLine(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;

    function GpiPolyLineDisjoint(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;

    function GpiSetPattern(hps : HPS;lPatternSymbol : LONG) : BOOL;

    function GpiQueryPattern(hps : HPS) : LONG;

    function GpiBeginArea(hps : HPS;flOptions : ULONG) : BOOL;

    function GpiEndArea(hps : HPS) : LONG;

    function GpiCharString(hps : HPS;lCount : LONG;pchString : PCH) : LONG;

    function GpiCharStringAt(hps : HPS;pptlPoint : PPOINTL;lCount : LONG;pchString : PCH) : LONG;

    const
       AM_ERROR = (-1);
       AM_PRESERVE = 0;
       AM_NOPRESERVE = 1;
       FM_ERROR = (-1);
       FM_DEFAULT = 0;
       FM_OR = 1;
       FM_OVERPAINT = 2;
       FM_LEAVEALONE = 5;
       FM_XOR = 4;
       FM_AND = 6;
       FM_SUBTRACT = 7;
       FM_MASKSRCNOT = 8;
       FM_ZERO = 9;
       FM_NOTMERGESRC = 10;
       FM_NOTXORSRC = 11;
       FM_INVERT = 12;
       FM_MERGESRCNOT = 13;
       FM_NOTCOPYSRC = 14;
       FM_MERGENOTSRC = 15;
       FM_NOTMASKSRC = 16;
       FM_ONE = 17;
       BM_ERROR = (-1);
       BM_DEFAULT = 0;
       BM_OR = 1;
       BM_OVERPAINT = 2;
       BM_LEAVEALONE = 5;
       BM_XOR = 4;
       BM_AND = 6;
       BM_SUBTRACT = 7;
       BM_MASKSRCNOT = 8;
       BM_ZERO = 9;
       BM_NOTMERGESRC = 10;
       BM_NOTXORSRC = 11;
       BM_INVERT = 12;
       BM_MERGESRCNOT = 13;
       BM_NOTCOPYSRC = 14;
       BM_MERGENOTSRC = 15;
       BM_NOTMASKSRC = 16;
       BM_ONE = 17;
       BM_SRCTRANSPARENT = 18;
       BM_DESTTRANSPARENT = 19;
       LINETYPE_ERROR = (-1);
       LINETYPE_DEFAULT = 0;
       LINETYPE_DOT = 1;
       LINETYPE_SHORTDASH = 2;
       LINETYPE_DASHDOT = 3;
       LINETYPE_DOUBLEDOT = 4;
       LINETYPE_LONGDASH = 5;
       LINETYPE_DASHDOUBLEDOT = 6;
       LINETYPE_SOLID = 7;
       LINETYPE_INVISIBLE = 8;
       LINETYPE_ALTERNATE = 9;
       LINEWIDTH_ERROR = (-1);
       LINEWIDTH_DEFAULT = 0;
       LINEWIDTH_NORMAL = $00010000;
       LINEWIDTH_THICK = $00020000;
       LINEWIDTHGEOM_ERROR = (-1);
       LINEEND_ERROR = (-1);
       LINEEND_DEFAULT = 0;
       LINEEND_FLAT = 1;
       LINEEND_SQUARE = 2;
       LINEEND_ROUND = 3;
       LINEJOIN_ERROR = (-1);
       LINEJOIN_DEFAULT = 0;
       LINEJOIN_BEVEL = 1;
       LINEJOIN_ROUND = 2;
       LINEJOIN_MITRE = 3;
       CHDIRN_ERROR = (-1);
       CHDIRN_DEFAULT = 0;
       CHDIRN_LEFTRIGHT = 1;
       CHDIRN_TOPBOTTOM = 2;
       CHDIRN_RIGHTLEFT = 3;
       CHDIRN_BOTTOMTOP = 4;
       TA_NORMAL_HORIZ = $0001;
       TA_LEFT = $0002;
       TA_CENTER = $0003;
       TA_RIGHT = $0004;
       TA_STANDARD_HORIZ = $0005;
       TA_NORMAL_VERT = $0100;
       TA_TOP = $0200;
       TA_HALF = $0300;
       TA_BASE = $0400;
       TA_BOTTOM = $0500;
       TA_STANDARD_VERT = $0600;
       CM_ERROR = (-1);
       CM_DEFAULT = 0;
       CM_MODE1 = 1;
       CM_MODE2 = 2;
       CM_MODE3 = 3;
       MARKSYM_ERROR = (-1);
       MARKSYM_DEFAULT = 0;
       MARKSYM_CROSS = 1;
       MARKSYM_PLUS = 2;
       MARKSYM_DIAMOND = 3;
       MARKSYM_SQUARE = 4;
       MARKSYM_SIXPOINTSTAR = 5;
       MARKSYM_EIGHTPOINTSTAR = 6;
       MARKSYM_SOLIDDIAMOND = 7;
       MARKSYM_SOLIDSQUARE = 8;
       MARKSYM_DOT = 9;
       MARKSYM_SMALLCIRCLE = 10;
       MARKSYM_BLANK = 64;
       CHS_OPAQUE = $0001;
       CHS_VECTOR = $0002;
       CHS_LEAVEPOS = $0008;
       CHS_CLIP = $0010;
       CHS_UNDERSCORE = $0200;
       CHS_STRIKEOUT = $0400;
       PRIM_LINE = 1;
       PRIM_CHAR = 2;
       PRIM_MARKER = 3;
       PRIM_AREA = 4;
       PRIM_IMAGE = 5;
       LBB_COLOR = $0001;
       LBB_BACK_COLOR = $0002;
       LBB_MIX_MODE = $0004;
       LBB_BACK_MIX_MODE = $0008;
       LBB_WIDTH = $0010;
       LBB_GEOM_WIDTH = $0020;
       LBB_TYPE = $0040;
       LBB_END = $0080;
       LBB_JOIN = $0100;
       CBB_COLOR = $0001;
       CBB_BACK_COLOR = $0002;
       CBB_MIX_MODE = $0004;
       CBB_BACK_MIX_MODE = $0008;
       CBB_SET = $0010;
       CBB_MODE = $0020;
       CBB_BOX = $0040;
       CBB_ANGLE = $0080;
       CBB_SHEAR = $0100;
       CBB_DIRECTION = $0200;
       CBB_TEXT_ALIGN = $0400;
       CBB_EXTRA = $0800;
       CBB_BREAK_EXTRA = $1000;
       MBB_COLOR = $0001;
       MBB_BACK_COLOR = $0002;
       MBB_MIX_MODE = $0004;
       MBB_BACK_MIX_MODE = $0008;
       MBB_SET = $0010;
       MBB_SYMBOL = $0020;
       MBB_BOX = $0040;
       ABB_COLOR = $0001;
       ABB_BACK_COLOR = $0002;
       ABB_MIX_MODE = $0004;
       ABB_BACK_MIX_MODE = $0008;
       ABB_SET = $0010;
       ABB_SYMBOL = $0020;
       ABB_REF_POINT = $0040;
       IBB_COLOR = $0001;
       IBB_BACK_COLOR = $0002;
       IBB_MIX_MODE = $0004;
       IBB_BACK_MIX_MODE = $0008;

    type
       ARCPARAMS = record
          lP : LONG;
          lQ : LONG;
          lR : LONG;
          lS : LONG;
       end;

       PARCPARAMS = ^ARCPARAMS;

       SIZEF = record
          cx : FIXED;
          cy : FIXED;
       end;

       PSIZEF = ^SIZEF;

       GRADIENTL = record
          x : LONG;
          y : LONG;
       end;

       PGRADIENTL = ^GRADIENTL;

       LINEBUNDLE = record
          lColor : LONG;
          lBackColor : LONG;
          usMixMode : USHORT;
          usBackMixMode : USHORT;
          fxWidth : FIXED;
          lGeomWidth : LONG;
          usType : USHORT;
          usEnd : USHORT;
          usJoin : USHORT;
          usReserved : USHORT;
       end;

       PLINEBUNDLE = ^LINEBUNDLE;

       CHARBUNDLE = record
          lColor : LONG;
          lBackColor : LONG;
          usMixMode : USHORT;
          usBackMixMode : USHORT;
          usSet : USHORT;
          usPrecision : USHORT;
          sizfxCell : SIZEF;
          ptlAngle : POINTL;
          ptlShear : POINTL;
          usDirection : USHORT;
          usTextAlign : USHORT;
          fxExtra : FIXED;
          fxBreakExtra : FIXED;
       end;

       PCHARBUNDLE = ^CHARBUNDLE;

       MARKERBUNDLE = record
          lColor : LONG;
          lBackColor : LONG;
          usMixMode : USHORT;
          usBackMixMode : USHORT;
          usSet : USHORT;
          usSymbol : USHORT;
          sizfxCell : SIZEF;
       end;

       PMARKERBUNDLE = ^MARKERBUNDLE;

       AREABUNDLE = record
          lColor : LONG;
          lBackColor : LONG;
          usMixMode : USHORT;
          usBackMixMode : USHORT;
          usSet : USHORT;
          usSymbol : USHORT;
          ptlRefPoint : POINTL;
       end;

       PAREABUNDLE = ^AREABUNDLE;

       IMAGEBUNDLE = record
          lColor : LONG;
          lBackColor : LONG;
          usMixMode : USHORT;
          usBackMixMode : USHORT;
       end;

       PIMAGEBUNDLE = ^IMAGEBUNDLE;

       PBUNDLE = PVOID;

    const
       TXTBOX_TOPLEFT = 0;
       TXTBOX_BOTTOMLEFT = 1;
       TXTBOX_TOPRIGHT = 2;
       TXTBOX_BOTTOMRIGHT = 3;
       TXTBOX_CONCAT = 4;
       TXTBOX_COUNT = 5;
       PVIS_ERROR = 0;
       PVIS_INVISIBLE = 1;
       PVIS_VISIBLE = 2;
       RVIS_ERROR = 0;
       RVIS_INVISIBLE = 1;
       RVIS_PARTIAL = 2;
       RVIS_VISIBLE = 3;

    function GpiSetAttrMode(hps : HPS;lMode : LONG) : BOOL;

    function GpiQueryAttrMode(hps : HPS) : LONG;

    function GpiSetAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;flDefMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;

    function GpiQueryAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : LONG;

    function GpiSetBackColor(hps : HPS;lColor : LONG) : BOOL;

    function GpiQueryBackColor(hps : HPS) : LONG;

    function GpiSetMix(hps : HPS;lMixMode : LONG) : BOOL;

    function GpiQueryMix(hps : HPS) : LONG;

    function GpiSetBackMix(hps : HPS;lMixMode : LONG) : BOOL;

    function GpiQueryBackMix(hps : HPS) : LONG;

    function GpiSetLineType(hps : HPS;lLineType : LONG) : BOOL;

    function GpiQueryLineType(hps : HPS) : LONG;

    function GpiSetLineWidth(hps : HPS;fxLineWidth : FIXED) : BOOL;

    function GpiQueryLineWidth(hps : HPS) : FIXED;

    function GpiSetLineWidthGeom(hps : HPS;lLineWidth : LONG) : BOOL;

    function GpiQueryLineWidthGeom(hps : HPS) : LONG;

    function GpiSetLineEnd(hps : HPS;lLineEnd : LONG) : BOOL;

    function GpiQueryLineEnd(hps : HPS) : LONG;

    function GpiSetLineJoin(hps : HPS;lLineJoin : LONG) : BOOL;

    function GpiQueryLineJoin(hps : HPS) : LONG;

    function GpiSetCurrentPosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;

    function GpiQueryCurrentPosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;

    function GpiSetArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;

    function GpiQueryArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;

    function GpiPointArc(hps : HPS;pptl2 : PPOINTL) : LONG;

    function GpiFullArc(hps : HPS;lControl : LONG;fxMultiplier : FIXED) : LONG;

    function GpiPartialArc(hps : HPS;pptlCenter : PPOINTL;fxMultiplier : FIXED;fxStartAngle : FIXED;fxSweepAngle : FIXED) : LONG;

    function GpiPolyFillet(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;

    function GpiPolySpline(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;

    function GpiPolyFilletSharp(hps : HPS;lCount : LONG;aptlPoints : PPOINTL;afxPoints : PFIXED) : LONG;

    function GpiSetPatternSet(hps : HPS;lSet : LONG) : BOOL;

    function GpiQueryPatternSet(hps : HPS) : LONG;

    function GpiSetPatternRefPoint(hps : HPS;pptlRefPoint : PPOINTL) : BOOL;

    function GpiQueryPatternRefPoint(hps : HPS;pptlRefPoint : PPOINTL) : BOOL;

    function GpiQueryCharStringPos(hps : HPS;flOptions : ULONG;lCount : LONG;pchString : PCH;alXincrements : PLONG;aptlPositions : PPOINTL) : BOOL;

    function GpiQueryCharStringPosAt(hps : HPS;pptlStart : PPOINTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alXincrements : PLONG;aptlPositions : PPOINTL) : BOOL;

    function GpiQueryTextBox(hps : HPS;lCount1 : LONG;pchString : PCH;lCount2 : LONG;aptlPoints : PPOINTL) : BOOL;

    function GpiQueryDefCharBox(hps : HPS;psizlSize : PSIZEL) : BOOL;

    function GpiSetCharSet(hps : HPS;llcid : LONG) : BOOL;

    function GpiQueryCharSet(hps : HPS) : LONG;

    function GpiSetCharBox(hps : HPS;psizfxBox : PSIZEF) : BOOL;

    function GpiQueryCharBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;

    function GpiSetCharAngle(hps : HPS;pgradlAngle : PGRADIENTL) : BOOL;

    function GpiQueryCharAngle(hps : HPS;pgradlAngle : PGRADIENTL) : BOOL;

    function GpiSetCharShear(hps : HPS;pptlAngle : PPOINTL) : BOOL;

    function GpiQueryCharShear(hps : HPS;pptlShear : PPOINTL) : BOOL;

    function GpiSetCharDirection(hps : HPS;lDirection : LONG) : BOOL;

    function GpiQueryCharDirection(hps : HPS) : LONG;

    function GpiSetCharMode(hps : HPS;lMode : LONG) : BOOL;

    function GpiQueryCharMode(hps : HPS) : LONG;

    function GpiSetTextAlignment(hps : HPS;lHoriz : LONG;lVert : LONG) : BOOL;

    function GpiQueryTextAlignment(hps : HPS;plHoriz : PLONG;plVert : PLONG) : BOOL;

    function GpiCharStringPos(hps : HPS;prclRect : PRECTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alAdx : PLONG) : LONG;

    function GpiCharStringPosAt(hps : HPS;pptlStart : PPOINTL;prclRect : PRECTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alAdx : PLONG) : LONG;

    function GpiSetCharExtra(hps : HPS;Extra : FIXED) : BOOL;

    function GpiSetCharBreakExtra(hps : HPS;BreakExtra : FIXED) : BOOL;

    function GpiQueryCharExtra(hps : HPS;Extra : PFIXED) : BOOL;

    function GpiQueryCharBreakExtra(hps : HPS;BreakExtra : PFIXED) : BOOL;

    function GpiMarker(hps : HPS;pptlPoint : PPOINTL) : LONG;

    function GpiPolyMarker(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;

    function GpiSetMarker(hps : HPS;lSymbol : LONG) : BOOL;

    function GpiSetMarkerBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;

    function GpiSetMarkerSet(hps : HPS;lSet : LONG) : BOOL;

    function GpiQueryMarker(hps : HPS) : LONG;

    function GpiQueryMarkerBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;

    function GpiQueryMarkerSet(hps : HPS) : LONG;

    function GpiImage(hps : HPS;lFormat : LONG;psizlImageSize : PSIZEL;lLength : LONG;pbData : PBYTE) : LONG;

    function GpiPop(hps : HPS;lCount : LONG) : BOOL;

    function GpiPtVisible(hps : HPS;pptlPoint : PPOINTL) : LONG;

    function GpiRectVisible(hps : HPS;prclRectangle : PRECTL) : LONG;

    function GpiComment(hps : HPS;lLength : LONG;pbData : PBYTE) : BOOL;

    const
       FONT_DEFAULT = 1;
       FONT_MATCH = 2;
       LCIDT_FONT = 6;
       LCIDT_BITMAP = 7;
       LCID_ALL = (-1);

    type
       KERNINGPAIRS = record
          sFirstChar : SHORT;
          sSecondChar : SHORT;
          lKerningAmount : LONG;
       end;

       PKERNINGPAIRS = ^KERNINGPAIRS;

       FACENAMEDESC = record
          usSize : USHORT;
          usWeightClass : USHORT;
          usWidthClass : USHORT;
          usReserved : USHORT;
          flOptions : ULONG;
       end;

       PFACENAMEDESC = ^FACENAMEDESC;

    const
       FWEIGHT_DONT_CARE = 0;
       FWEIGHT_ULTRA_LIGHT = 1;
       FWEIGHT_EXTRA_LIGHT = 2;
       FWEIGHT_LIGHT = 3;
       FWEIGHT_SEMI_LIGHT = 4;
       FWEIGHT_NORMAL = 5;
       FWEIGHT_SEMI_BOLD = 6;
       FWEIGHT_BOLD = 7;
       FWEIGHT_EXTRA_BOLD = 8;
       FWEIGHT_ULTRA_BOLD = 9;
       FWIDTH_DONT_CARE = 0;
       FWIDTH_ULTRA_CONDENSED = 1;
       FWIDTH_EXTRA_CONDENSED = 2;
       FWIDTH_CONDENSED = 3;
       FWIDTH_SEMI_CONDENSED = 4;
       FWIDTH_NORMAL = 5;
       FWIDTH_SEMI_EXPANDED = 6;
       FWIDTH_EXPANDED = 7;
       FWIDTH_EXTRA_EXPANDED = 8;
       FWIDTH_ULTRA_EXPANDED = 9;
       FTYPE_ITALIC = $0001;
       FTYPE_ITALIC_DONT_CARE = $0002;
       FTYPE_OBLIQUE = $0004;
       FTYPE_OBLIQUE_DONT_CARE = $0008;
       FTYPE_ROUNDED = $0010;
       FTYPE_ROUNDED_DONT_CARE = $0020;
       QFA_PUBLIC = 1;
       QFA_PRIVATE = 2;
       QFA_ERROR = GPI_ALTERROR;
       QF_PUBLIC = $0001;
       QF_PRIVATE = $0002;
       QF_NO_GENERIC = $0004;
       QF_NO_DEVICE = $0008;

    type
       FFDESCS = array[0..1,0..FACESIZE-1] of char;
    
       PFFDESCS = ^FFDESCS;

       FFDESCS2 = record
          cbLength : ULONG;
          cbFacenameOffset : ULONG;
          abFamilyName : array[0..1-1] of BYTE;
       end;

       PFFDESCS2 = ^FFDESCS2;

    function GpiCreateLogFont(hps : HPS;pName : PSTR8;lLcid : LONG;pfatAttrs : PFATTRS) : LONG;

    function GpiDeleteSetId(hps : HPS;lLcid : LONG) : BOOL;

    function GpiLoadFonts(hab : HAB;pszFilename : PSZ) : BOOL;

    function GpiUnloadFonts(hab : HAB;pszFilename : PSZ) : BOOL;

    function GpiQueryFonts(hps : HPS;flOptions : ULONG;pszFacename : PSZ;plReqFonts : PLONG;lMetricsLength : LONG;afmMetrics : PFONTMETRICS) : LONG;

    function GpiQueryFontMetrics(hps : HPS;lMetricsLength : LONG;pfmMetrics : PFONTMETRICS) : BOOL;

    function GpiQueryKerningPairs(hps : HPS;lCount : LONG;akrnprData : PKERNINGPAIRS) : LONG;

    function GpiQueryWidthTable(hps : HPS;lFirstChar : LONG;lCount : LONG;alData : PLONG) : BOOL;

    function GpiQueryNumberSetIds(hps : HPS) : LONG;

    function GpiQuerySetIds(hps : HPS;lCount : LONG;alTypes : PLONG;aNames : PSTR8;allcids : PLONG) : BOOL;

    function GpiQueryFaceString(PS : HPS;FamilyName : PSZ;attrs : PFACENAMEDESC;length : LONG;CompoundFaceName : PSZ) : ULONG;

    function GpiQueryLogicalFont(PS : HPS;lcid : LONG;name : PSTR8;attrs : PFATTRS;length : LONG) : BOOL;

    function GpiQueryFontAction(anchor : HAB;options : ULONG) : ULONG;

    function GpiLoadPublicFonts(p1 : HAB;p2 : PSZ) : BOOL;

    function GpiUnloadPublicFonts(p1 : HAB;p2 : PSZ) : BOOL;

    function GpiSetCp(hps : HPS;ulCodePage : ULONG) : BOOL;

    function GpiQueryCp(hps : HPS) : ULONG;

    function GpiQueryFontFileDescriptions(hab : HAB;pszFilename : PSZ;plCount : PLONG;affdescsNames : PFFDESCS) : LONG;

    function GpiQueryFullFontFileDescs(hab : HAB;pszFilename : PSZ;plCount : PLONG;pNames : PVOID;plNamesBuffLength : PLONG) : LONG;

    const
       ROP_SRCCOPY = $00CC;
       ROP_SRCPAINT = $00EE;
       ROP_SRCAND = $0088;
       ROP_SRCINVERT = $0066;
       ROP_SRCERASE = $0044;
       ROP_NOTSRCCOPY = $0033;
       ROP_NOTSRCERASE = $0011;
       ROP_MERGECOPY = $00C0;
       ROP_MERGEPAINT = $00BB;
       ROP_PATCOPY = $00F0;
       ROP_PATPAINT = $00FB;
       ROP_PATINVERT = $005A;
       ROP_DSTINVERT = $0055;
       ROP_ZERO = $0000;
       ROP_ONE = $00FF;
       BBO_OR = 0;
       BBO_AND = 1;
       BBO_IGNORE = 2;
       BBO_PAL_COLORS = 4;
       BBO_NO_COLOR_INFO = 8;
       FF_BOUNDARY = 0;
       FF_SURFACE = 1;
       HBM_ERROR = -1;

    function GpiBitBlt(hpsTarget : HPS;hpsSource : HPS;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;

    function GpiDeleteBitmap(hbm : HBITMAP) : BOOL;

    function GpiLoadBitmap(hps : HPS;Resource : HMODULE;idBitmap : ULONG;lWidth : LONG;lHeight : LONG) : HBITMAP;

    function GpiSetBitmap(hps : HPS;hbm : HBITMAP) : HBITMAP;

    function GpiWCBitBlt(hpsTarget : HPS;hbmSource : HBITMAP;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;

    const
       CBM_INIT = $0004;
       BMB_ERROR = (-1);

    function GpiCreateBitmap(hps : HPS;pbmpNew : PBITMAPINFOHEADER2;flOptions : ULONG;pbInitData : PBYTE;pbmiInfoTable : PBITMAPINFO2) : HBITMAP;

    function GpiSetBitmapBits(hps : HPS;lScanStart : LONG;lScans : LONG;pbBuffer : PBYTE;pbmiInfoTable : PBITMAPINFO2) : LONG;

    function GpiSetBitmapDimension(hbm : HBITMAP;psizlBitmapDimension : PSIZEL) : BOOL;

    function GpiSetBitmapId(hps : HPS;hbm : HBITMAP;lLcid : LONG) : BOOL;

    function GpiQueryBitmapBits(hps : HPS;lScanStart : LONG;lScans : LONG;pbBuffer : PBYTE;pbmiInfoTable : PBITMAPINFO2) : LONG;

    function GpiQueryBitmapDimension(hbm : HBITMAP;psizlBitmapDimension : PSIZEL) : BOOL;

    function GpiQueryBitmapHandle(hps : HPS;lLcid : LONG) : HBITMAP;

    function GpiQueryBitmapParameters(hbm : HBITMAP;pbmpData : PBITMAPINFOHEADER) : BOOL;

    function GpiQueryBitmapInfoHeader(hbm : HBITMAP;pbmpData : PBITMAPINFOHEADER2) : BOOL;

    function GpiQueryDeviceBitmapFormats(hps : HPS;lCount : LONG;alArray : PLONG) : BOOL;

    function GpiSetPel(hps : HPS;pptlPoint : PPOINTL) : LONG;

    function GpiQueryPel(hps : HPS;pptlPoint : PPOINTL) : LONG;

    function GpiFloodFill(hps : HPS;lOptions : LONG;lColor : LONG) : LONG;

    function GpiDrawBits(hps : HPS;pBits : PVOID;pbmiInfoTable : PBITMAPINFO2;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;

    const
       CRGN_OR = 1;
       CRGN_COPY = 2;
       CRGN_XOR = 4;
       CRGN_AND = 6;
       CRGN_DIFF = 7;
       RECTDIR_LFRT_TOPBOT = 1;
       RECTDIR_RTLF_TOPBOT = 2;
       RECTDIR_LFRT_BOTTOP = 3;
       RECTDIR_RTLF_BOTTOP = 4;

    type
       RGNRECT = record
          ircStart : ULONG;
          crc : ULONG;
          crcReturned : ULONG;
          ulDirection : ULONG;
       end;

       PRGNRECT = ^RGNRECT;

    const
       RGN_ERROR = 0;
       RGN_NULL = 1;
       RGN_RECT = 2;
       RGN_COMPLEX = 3;
       PRGN_ERROR = 0;
       PRGN_OUTSIDE = 1;
       PRGN_INSIDE = 2;
       RRGN_ERROR = 0;
       RRGN_OUTSIDE = 1;
       RRGN_PARTIAL = 2;
       RRGN_INSIDE = 3;
       EQRGN_ERROR = 0;
       EQRGN_NOTEQUAL = 1;
       EQRGN_EQUAL = 2;
       HRGN_ERROR = -1;

    function GpiCombineRegion(hps : HPS;hrgnDest : HRGN;hrgnSrc1 : HRGN;hrgnSrc2 : HRGN;lMode : LONG) : LONG;

    function GpiCreateRegion(hps : HPS;lCount : LONG;arclRectangles : PRECTL) : HRGN;

    function GpiDestroyRegion(hps : HPS;hrgn : HRGN) : BOOL;

    function GpiEqualRegion(hps : HPS;hrgnSrc1 : HRGN;hrgnSrc2 : HRGN) : LONG;

    function GpiOffsetRegion(hps : HPS;Hrgn : HRGN;pptlOffset : PPOINTL) : BOOL;

    function GpiPaintRegion(hps : HPS;hrgn : HRGN) : LONG;

    function GpiFrameRegion(hps : HPS;hrgn : HRGN;thickness : PSIZEL) : LONG;

    function GpiPtInRegion(hps : HPS;hrgn : HRGN;pptlPoint : PPOINTL) : LONG;

    function GpiQueryRegionBox(hps : HPS;hrgn : HRGN;prclBound : PRECTL) : LONG;

    function GpiQueryRegionRects(hps : HPS;hrgn : HRGN;prclBound : PRECTL;prgnrcControl : PRGNRECT;prclRect : PRECTL) : BOOL;

    function GpiRectInRegion(hps : HPS;hrgn : HRGN;prclRect : PRECTL) : LONG;

    function GpiSetRegion(hps : HPS;hrgn : HRGN;lcount : LONG;arclRectangles : PRECTL) : BOOL;

    function GpiSetClipRegion(hps : HPS;hrgn : HRGN;phrgnOld : PHRGN) : LONG;

    function GpiQueryClipRegion(hps : HPS) : HRGN;

    function GpiQueryClipBox(hps : HPS;prclBound : PRECTL) : LONG;

    function GpiExcludeClipRectangle(hps : HPS;prclRectangle : PRECTL) : LONG;

    function GpiIntersectClipRectangle(hps : HPS;prclRectangle : PRECTL) : LONG;

    function GpiOffsetClipRegion(hps : HPS;pptlPoint : PPOINTL) : LONG;

    const
       PMF_SEGBASE = 0;
       PMF_LOADTYPE = 1;
       PMF_RESOLVE = 2;
       PMF_LCIDS = 3;
       PMF_RESET = 4;
       PMF_SUPPRESS = 5;
       PMF_COLORTABLES = 6;
       PMF_COLORREALIZABLE = 7;
       PMF_DEFAULTS = 8;
       PMF_DELETEOBJECTS = 9;
       RS_DEFAULT = 0;
       RS_NODISCARD = 1;
       LC_DEFAULT = 0;
       LC_NOLOAD = 1;
       LC_LOADDISC = 3;
       LT_DEFAULT = 0;
       LT_NOMODIFY = 1;
       LT_ORIGINALVIEW = 4;
       RES_DEFAULT = 0;
       RES_NORESET = 1;
       RES_RESET = 2;
       SUP_DEFAULT = 0;
       SUP_NOSUPPRESS = 1;
       SUP_SUPPRESS = 2;
       CTAB_DEFAULT = 0;
       CTAB_NOMODIFY = 1;
       CTAB_REPLACE = 3;
       CTAB_REPLACEPALETTE = 4;
       CREA_DEFAULT = 0;
       CREA_REALIZE = 1;
       CREA_NOREALIZE = 2;
       CREA_DOREALIZE = 3;
       DDEF_DEFAULT = 0;
       DDEF_IGNORE = 1;
       DDEF_LOADDISC = 3;
       DOBJ_DEFAULT = 0;
       DOBJ_NODELETE = 1;
       DOBJ_DELETE = 2;
       RSP_DEFAULT = 0;
       RSP_NODISCARD = 1;

    function GpiCopyMetaFile(hmf : HMF) : HMF;

    function GpiDeleteMetaFile(hmf : HMF) : BOOL;

    function GpiLoadMetaFile(hab : HAB;pszFilename : PSZ) : HMF;

    function GpiPlayMetaFile(hps : HPS;hmf : HMF;lCount1 : LONG;alOptarray : PLONG;plSegCount : PLONG;lCount2 : LONG;pszDesc : PSZ) : LONG;

    function GpiQueryMetaFileBits(hmf : HMF;lOffset : LONG;lLength : LONG;pbData : PBYTE) : BOOL;

    function GpiQueryMetaFileLength(hmf : HMF) : LONG;

    function GpiSaveMetaFile(hmf : HMF;pszFilename : PSZ) : BOOL;

    function GpiSetMetaFileBits(hmf : HMF;lOffset : LONG;lLength : LONG;pbBuffer : PBYTE) : BOOL;

    function GpiQueryDefArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;

    function GpiQueryDefAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;

    function GpiQueryDefTag(hps : HPS;plTag : PLONG) : BOOL;

    function GpiQueryDefViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;

    function GpiSetDefArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;

    function GpiSetDefAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;

    function GpiSetDefTag(hps : HPS;lTag : LONG) : BOOL;

    function GpiSetDefViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;

    type
       POLYGON = record
          ulPoints : ULONG;
          aPointl : PPOINTL;
       end;

       PPOLYGON = ^POLYGON;

       POLYSET = record
          ulPolys : ULONG;
          aPolygon : array[0..1-1] of POLYGON;
       end;

       PPOLYSET = ^POLYSET;

    const
       POLYGON_NOBOUNDARY = 0;
       POLYGON_BOUNDARY = $0001;
       POLYGON_ALTERNATE = 0;
       POLYGON_WINDING = $0002;
       POLYGON_EXCL = 0;
       POLYGON_INCL = $0008;

    function GpiPolygons(hps : HPS;ulCount : ULONG;paplgn : PPOLYGON;flOptions : ULONG;flModel : ULONG) : LONG;

  implementation

    function GpiCreatePS(hab : HAB;hdc : HDC;psizlSize : PSIZEL;flOptions : ULONG) : HPS;[SYSTEM];
    function GpiDestroyPS(hps : HPS) : BOOL;[SYSTEM];
    function GpiAssociate(hps : HPS;hdc : HDC) : BOOL;[SYSTEM];
    function GpiRestorePS(hps : HPS;lPSid : LONG) : BOOL;[SYSTEM];
    function GpiSavePS(hps : HPS) : LONG;[SYSTEM];
    function GpiErase(hps : HPS) : BOOL;[SYSTEM];
    function GpiQueryDevice(hps : HPS) : HDC;[SYSTEM];
    function GpiResetPS(hps : HPS;flOptions : ULONG) : BOOL;[SYSTEM];
    function GpiSetPS(hps : HPS;psizlsize : PSIZEL;flOptions : ULONG) : BOOL;[SYSTEM];
    function GpiQueryPS(hps : HPS;psizlSize : PSIZEL) : ULONG;[SYSTEM];
    function GpiErrorSegmentData(hps : HPS;plSegment : PLONG;plContext : PLONG) : LONG;[SYSTEM];
    function GpiQueryDrawControl(hps : HPS;lControl : LONG) : LONG;[SYSTEM];
    function GpiSetDrawControl(hps : HPS;lControl : LONG;lValue : LONG) : BOOL;[SYSTEM];
    function GpiQueryDrawingMode(hps : HPS) : LONG;[SYSTEM];
    function GpiSetDrawingMode(hps : HPS;lMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryStopDraw(hps : HPS) : LONG;[SYSTEM];
    function GpiSetStopDraw(hps : HPS;lValue : LONG) : BOOL;[SYSTEM];
    function GpiCorrelateChain(hps : HPS;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;pl2 : PLONG) : LONG;[SYSTEM];
    function GpiQueryTag(hps : HPS;plTag : PLONG) : BOOL;[SYSTEM];
    function GpiSetTag(hps : HPS;lTag : LONG) : BOOL;[SYSTEM];
    function GpiQueryPickApertureSize(hps : HPS;psizlSize : PSIZEL) : BOOL;[SYSTEM];
    function GpiSetPickApertureSize(hps : HPS;lOptions : LONG;psizlSize : PSIZEL) : BOOL;[SYSTEM];
    function GpiQueryPickAperturePosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiSetPickAperturePosition(hps : HPS;pptlPick : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryBoundaryData(hps : HPS;prclBoundary : PRECTL) : BOOL;[SYSTEM];
    function GpiResetBoundaryData(hps : HPS) : BOOL;[SYSTEM];
    function GpiCorrelateFrom(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;plSegTag : PLONG) : LONG;[SYSTEM];
    function GpiCorrelateSegment(hps : HPS;lSegment : LONG;lType : LONG;pptlPick : PPOINTL;lMaxHits : LONG;lMaxDepth : LONG;alSegTag : PLONG) : LONG;[SYSTEM];
    function GpiOpenSegment(hps : HPS;lSegment : LONG) : BOOL;[SYSTEM];
    function GpiCloseSegment(hps : HPS) : BOOL;[SYSTEM];
    function GpiDeleteSegment(hps : HPS;lSegid : LONG) : BOOL;[SYSTEM];
    function GpiQueryInitialSegmentAttrs(hps : HPS;lAttribute : LONG) : LONG;[SYSTEM];
    function GpiSetInitialSegmentAttrs(hps : HPS;lAttribute : LONG;lValue : LONG) : BOOL;[SYSTEM];
    function GpiQuerySegmentAttrs(hps : HPS;lSegid : LONG;lAttribute : LONG) : LONG;[SYSTEM];
    function GpiSetSegmentAttrs(hps : HPS;lSegid : LONG;lAttribute : LONG;lValue : LONG) : BOOL;[SYSTEM];
    function GpiQuerySegmentPriority(hps : HPS;lRefSegid : LONG;lOrder : LONG) : LONG;[SYSTEM];
    function GpiSetSegmentPriority(hps : HPS;lSegid : LONG;lRefSegid : LONG;lOrder : LONG) : BOOL;[SYSTEM];
    function GpiDeleteSegments(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG) : BOOL;[SYSTEM];
    function GpiQuerySegmentNames(hps : HPS;lFirstSegid : LONG;lLastSegid : LONG;lMax : LONG;alSegids : PLONG) : LONG;[SYSTEM];
    function GpiGetData(hps : HPS;lSegid : LONG;plOffset : PLONG;lFormat : LONG;lLength : LONG;pbData : PBYTE) : LONG;[SYSTEM];
    function GpiPutData(hps : HPS;lFormat : LONG;plCount : PLONG;pbData : PBYTE) : LONG;[SYSTEM];
    function GpiDrawChain(hps : HPS) : BOOL;[SYSTEM];
    function GpiDrawFrom(hps : HPS;lFirstSegment : LONG;lLastSegment : LONG) : BOOL;[SYSTEM];
    function GpiDrawSegment(hps : HPS;lSegment : LONG) : BOOL;[SYSTEM];
    function GpiDrawDynamics(hps : HPS) : BOOL;[SYSTEM];
    function GpiRemoveDynamics(hps : HPS;lFirstSegid : LONG;lLastSegid : LONG) : BOOL;[SYSTEM];
    function GpiBeginElement(hps : HPS;lType : LONG;pszDesc : PSZ) : BOOL;[SYSTEM];
    function GpiEndElement(hps : HPS) : BOOL;[SYSTEM];
    function GpiLabel(hps : HPS;lLabel : LONG) : BOOL;[SYSTEM];
    function GpiElement(hps : HPS;lType : LONG;pszDesc : PSZ;lLength : LONG;pbData : PBYTE) : LONG;[SYSTEM];
    function GpiQueryElement(hps : HPS;lOff : LONG;lMaxLength : LONG;pbData : PBYTE) : LONG;[SYSTEM];
    function GpiDeleteElement(hps : HPS) : BOOL;[SYSTEM];
    function GpiDeleteElementRange(hps : HPS;lFirstElement : LONG;lLastElement : LONG) : BOOL;[SYSTEM];
    function GpiDeleteElementsBetweenLabels(hps : HPS;lFirstLabel : LONG;lLastLabel : LONG) : BOOL;[SYSTEM];
    function GpiQueryEditMode(hps : HPS) : LONG;[SYSTEM];
    function GpiSetEditMode(hps : HPS;lMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryElementPointer(hps : HPS) : LONG;[SYSTEM];
    function GpiSetElementPointer(hps : HPS;lElement : LONG) : BOOL;[SYSTEM];
    function GpiOffsetElementPointer(hps : HPS;loffset : LONG) : BOOL;[SYSTEM];
    function GpiQueryElementType(hps : HPS;plType : PLONG;lLength : LONG;pszData : PSZ) : LONG;[SYSTEM];
    function GpiSetElementPointerAtLabel(hps : HPS;lLabel : LONG) : BOOL;[SYSTEM];
    function GpiQuerySegmentTransformMatrix(hps : HPS;lSegid : LONG;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;[SYSTEM];
    function GpiSetSegmentTransformMatrix(hps : HPS;lSegid : LONG;lCount : LONG;pmatlfarray : PMATRIXLF;lOptions : LONG) : BOOL;[SYSTEM];
    function GpiConvert(hps : HPS;lSrc : LONG;lTarg : LONG;lCount : LONG;aptlPoints : PPOINTL) : BOOL;[SYSTEM];
    function GpiConvertWithMatrix(hps : HPS;lCountp : LONG;aptlPoints : PPOINTL;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;[SYSTEM];
    function GpiQueryModelTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;[SYSTEM];
    function GpiSetModelTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : BOOL;[SYSTEM];
    function GpiCallSegmentMatrix(hps : HPS;lSegment : LONG;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : LONG;[SYSTEM];
    function GpiQueryDefaultViewMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;[SYSTEM];
    function GpiSetDefaultViewMatrix(hps : HPS;lCount : LONG;pmatlfarray : PMATRIXLF;lOptions : LONG) : BOOL;[SYSTEM];
    function GpiQueryPageViewport(hps : HPS;prclViewport : PRECTL) : BOOL;[SYSTEM];
    function GpiSetPageViewport(hps : HPS;prclViewport : PRECTL) : BOOL;[SYSTEM];
    function GpiQueryViewingTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF) : BOOL;[SYSTEM];
    function GpiSetViewingTransformMatrix(hps : HPS;lCount : LONG;pmatlfArray : PMATRIXLF;lOptions : LONG) : BOOL;[SYSTEM];
    function GpiTranslate(hps : HPS;pmatrixlf : PMATRIXLF;long : LONG;ppointl : PPOINTL) : BOOL;[SYSTEM];
    function GpiScale(hps : HPS;p1 : PMATRIXLF;p2 : LONG;p3 : PFIXED;p4 : PPOINTL) : BOOL;[SYSTEM];
    function GpiRotate(p1 : HPS;p2 : PMATRIXLF;p3 : LONG;p4 : FIXED;p5 : PPOINTL) : BOOL;[SYSTEM];
    function GpiSetGraphicsField(hps : HPS;prclField : PRECTL) : BOOL;[SYSTEM];
    function GpiQueryGraphicsField(hps : HPS;prclField : PRECTL) : BOOL;[SYSTEM];
    function GpiSetViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;[SYSTEM];
    function GpiQueryViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;[SYSTEM];
    function GpiBeginPath(hps : HPS;lPath : LONG) : BOOL;[SYSTEM];
    function GpiEndPath(hps : HPS) : BOOL;[SYSTEM];
    function GpiCloseFigure(hps : HPS) : BOOL;[SYSTEM];
    function GpiModifyPath(hps : HPS;lPath : LONG;lMode : LONG) : BOOL;[SYSTEM];
    function GpiFillPath(hps : HPS;lPath : LONG;lOptions : LONG) : LONG;[SYSTEM];
    function GpiSetClipPath(hps : HPS;lPath : LONG;lOptions : LONG) : BOOL;[SYSTEM];
    function GpiOutlinePath(hps : HPS;lPath : LONG;lOptions : LONG) : LONG;[SYSTEM];
    function GpiPathToRegion(GpiH : HPS;lPath : LONG;lOptions : LONG) : HRGN;[SYSTEM];
    function GpiStrokePath(hps : HPS;lPath : LONG;flOptions : ULONG) : LONG;[SYSTEM];
    function GpiCreateLogColorTable(hps : HPS;flOptions : ULONG;lFormat : LONG;lStart : LONG;lCount : LONG;alTable : PLONG) : BOOL;[SYSTEM];
    function GpiQueryColorData(hps : HPS;lCount : LONG;alArray : PLONG) : BOOL;[SYSTEM];
    function GpiQueryLogColorTable(hps : HPS;flOptions : ULONG;lStart : LONG;lCount : LONG;alArray : PLONG) : LONG;[SYSTEM];
    function GpiQueryRealColors(hps : HPS;flOptions : ULONG;lStart : LONG;lCount : LONG;alColors : PLONG) : LONG;[SYSTEM];
    function GpiQueryNearestColor(hps : HPS;flOptions : ULONG;lRgbIn : LONG) : LONG;[SYSTEM];
    function GpiQueryColorIndex(hps : HPS;flOptions : ULONG;lRgbColor : LONG) : LONG;[SYSTEM];
    function GpiQueryRGBColor(hps : HPS;flOptions : ULONG;lColorIndex : LONG) : LONG;[SYSTEM];
    function GpiCreatePalette(hab : HAB;flOptions : ULONG;ulFormat : ULONG;ulCount : ULONG;aulTable : PULONG) : HPAL;[SYSTEM];
    function GpiDeletePalette(hpal : HPAL) : BOOL;[SYSTEM];
    function GpiSelectPalette(hps : HPS;hpal : HPAL) : HPAL;[SYSTEM];
    function GpiAnimatePalette(hpal : HPAL;ulFormat : ULONG;ulStart : ULONG;ulCount : ULONG;aulTable : PULONG) : LONG;[SYSTEM];
    function GpiSetPaletteEntries(hpal : HPAL;ulFormat : ULONG;ulStart : ULONG;ulCount : ULONG;aulTable : PULONG) : BOOL;[SYSTEM];
    function GpiQueryPalette(hps : HPS) : HPAL;[SYSTEM];
    function GpiQueryPaletteInfo(hpal : HPAL;hps : HPS;flOptions : ULONG;ulStart : ULONG;ulCount : ULONG;aulArray : PULONG) : LONG;[SYSTEM];
    function GpiSetColor(hps : HPS;lColor : LONG) : BOOL;[SYSTEM];
    function GpiQueryColor(hps : HPS) : LONG;[SYSTEM];
    function GpiBox(hps : HPS;lControl : LONG;pptlPoint : PPOINTL;lHRound : LONG;lVRound : LONG) : LONG;[SYSTEM];
    function GpiMove(hps : HPS;pptlPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiLine(hps : HPS;pptlEndPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiPolyLine(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;[SYSTEM];
    function GpiPolyLineDisjoint(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;[SYSTEM];
    function GpiSetPattern(hps : HPS;lPatternSymbol : LONG) : BOOL;[SYSTEM];
    function GpiQueryPattern(hps : HPS) : LONG;[SYSTEM];
    function GpiBeginArea(hps : HPS;flOptions : ULONG) : BOOL;[SYSTEM];
    function GpiEndArea(hps : HPS) : LONG;[SYSTEM];
    function GpiCharString(hps : HPS;lCount : LONG;pchString : PCH) : LONG;[SYSTEM];
    function GpiCharStringAt(hps : HPS;pptlPoint : PPOINTL;lCount : LONG;pchString : PCH) : LONG;[SYSTEM];
    function GpiSetAttrMode(hps : HPS;lMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryAttrMode(hps : HPS) : LONG;[SYSTEM];
    function GpiSetAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;flDefMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;[SYSTEM];
    function GpiQueryAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : LONG;[SYSTEM];
    function GpiSetBackColor(hps : HPS;lColor : LONG) : BOOL;[SYSTEM];
    function GpiQueryBackColor(hps : HPS) : LONG;[SYSTEM];
    function GpiSetMix(hps : HPS;lMixMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryMix(hps : HPS) : LONG;[SYSTEM];
    function GpiSetBackMix(hps : HPS;lMixMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryBackMix(hps : HPS) : LONG;[SYSTEM];
    function GpiSetLineType(hps : HPS;lLineType : LONG) : BOOL;[SYSTEM];
    function GpiQueryLineType(hps : HPS) : LONG;[SYSTEM];
    function GpiSetLineWidth(hps : HPS;fxLineWidth : FIXED) : BOOL;[SYSTEM];
    function GpiQueryLineWidth(hps : HPS) : FIXED;[SYSTEM];
    function GpiSetLineWidthGeom(hps : HPS;lLineWidth : LONG) : BOOL;[SYSTEM];
    function GpiQueryLineWidthGeom(hps : HPS) : LONG;[SYSTEM];
    function GpiSetLineEnd(hps : HPS;lLineEnd : LONG) : BOOL;[SYSTEM];
    function GpiQueryLineEnd(hps : HPS) : LONG;[SYSTEM];
    function GpiSetLineJoin(hps : HPS;lLineJoin : LONG) : BOOL;[SYSTEM];
    function GpiQueryLineJoin(hps : HPS) : LONG;[SYSTEM];
    function GpiSetCurrentPosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryCurrentPosition(hps : HPS;pptlPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiSetArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;[SYSTEM];
    function GpiQueryArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;[SYSTEM];
    function GpiPointArc(hps : HPS;pptl2 : PPOINTL) : LONG;[SYSTEM];
    function GpiFullArc(hps : HPS;lControl : LONG;fxMultiplier : FIXED) : LONG;[SYSTEM];
    function GpiPartialArc(hps : HPS;pptlCenter : PPOINTL;fxMultiplier : FIXED;fxStartAngle : FIXED;fxSweepAngle : FIXED) : LONG;[SYSTEM];
    function GpiPolyFillet(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;[SYSTEM];
    function GpiPolySpline(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;[SYSTEM];
    function GpiPolyFilletSharp(hps : HPS;lCount : LONG;aptlPoints : PPOINTL;afxPoints : PFIXED) : LONG;[SYSTEM];
    function GpiSetPatternSet(hps : HPS;lSet : LONG) : BOOL;[SYSTEM];
    function GpiQueryPatternSet(hps : HPS) : LONG;[SYSTEM];
    function GpiSetPatternRefPoint(hps : HPS;pptlRefPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryPatternRefPoint(hps : HPS;pptlRefPoint : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryCharStringPos(hps : HPS;flOptions : ULONG;lCount : LONG;pchString : PCH;alXincrements : PLONG;aptlPositions : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryCharStringPosAt(hps : HPS;pptlStart : PPOINTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alXincrements : PLONG;aptlPositions : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryTextBox(hps : HPS;lCount1 : LONG;pchString : PCH;lCount2 : LONG;aptlPoints : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryDefCharBox(hps : HPS;psizlSize : PSIZEL) : BOOL;[SYSTEM];
    function GpiSetCharSet(hps : HPS;llcid : LONG) : BOOL;[SYSTEM];
    function GpiQueryCharSet(hps : HPS) : LONG;[SYSTEM];
    function GpiSetCharBox(hps : HPS;psizfxBox : PSIZEF) : BOOL;[SYSTEM];
    function GpiQueryCharBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;[SYSTEM];
    function GpiSetCharAngle(hps : HPS;pgradlAngle : PGRADIENTL) : BOOL;[SYSTEM];
    function GpiQueryCharAngle(hps : HPS;pgradlAngle : PGRADIENTL) : BOOL;[SYSTEM];
    function GpiSetCharShear(hps : HPS;pptlAngle : PPOINTL) : BOOL;[SYSTEM];
    function GpiQueryCharShear(hps : HPS;pptlShear : PPOINTL) : BOOL;[SYSTEM];
    function GpiSetCharDirection(hps : HPS;lDirection : LONG) : BOOL;[SYSTEM];
    function GpiQueryCharDirection(hps : HPS) : LONG;[SYSTEM];
    function GpiSetCharMode(hps : HPS;lMode : LONG) : BOOL;[SYSTEM];
    function GpiQueryCharMode(hps : HPS) : LONG;[SYSTEM];
    function GpiSetTextAlignment(hps : HPS;lHoriz : LONG;lVert : LONG) : BOOL;[SYSTEM];
    function GpiQueryTextAlignment(hps : HPS;plHoriz : PLONG;plVert : PLONG) : BOOL;[SYSTEM];
    function GpiCharStringPos(hps : HPS;prclRect : PRECTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alAdx : PLONG) : LONG;[SYSTEM];
    function GpiCharStringPosAt(hps : HPS;pptlStart : PPOINTL;prclRect : PRECTL;flOptions : ULONG;lCount : LONG;pchString : PCH;alAdx : PLONG) : LONG;[SYSTEM];
    function GpiSetCharExtra(hps : HPS;Extra : FIXED) : BOOL;[SYSTEM];
    function GpiSetCharBreakExtra(hps : HPS;BreakExtra : FIXED) : BOOL;[SYSTEM];
    function GpiQueryCharExtra(hps : HPS;Extra : PFIXED) : BOOL;[SYSTEM];
    function GpiQueryCharBreakExtra(hps : HPS;BreakExtra : PFIXED) : BOOL;[SYSTEM];
    function GpiMarker(hps : HPS;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiPolyMarker(hps : HPS;lCount : LONG;aptlPoints : PPOINTL) : LONG;[SYSTEM];
    function GpiSetMarker(hps : HPS;lSymbol : LONG) : BOOL;[SYSTEM];
    function GpiSetMarkerBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;[SYSTEM];
    function GpiSetMarkerSet(hps : HPS;lSet : LONG) : BOOL;[SYSTEM];
    function GpiQueryMarker(hps : HPS) : LONG;[SYSTEM];
    function GpiQueryMarkerBox(hps : HPS;psizfxSize : PSIZEF) : BOOL;[SYSTEM];
    function GpiQueryMarkerSet(hps : HPS) : LONG;[SYSTEM];
    function GpiImage(hps : HPS;lFormat : LONG;psizlImageSize : PSIZEL;lLength : LONG;pbData : PBYTE) : LONG;[SYSTEM];
    function GpiPop(hps : HPS;lCount : LONG) : BOOL;[SYSTEM];
    function GpiPtVisible(hps : HPS;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiRectVisible(hps : HPS;prclRectangle : PRECTL) : LONG;[SYSTEM];
    function GpiComment(hps : HPS;lLength : LONG;pbData : PBYTE) : BOOL;[SYSTEM];
    function GpiCreateLogFont(hps : HPS;pName : PSTR8;lLcid : LONG;pfatAttrs : PFATTRS) : LONG;[SYSTEM];
    function GpiDeleteSetId(hps : HPS;lLcid : LONG) : BOOL;[SYSTEM];
    function GpiLoadFonts(hab : HAB;pszFilename : PSZ) : BOOL;[SYSTEM];
    function GpiUnloadFonts(hab : HAB;pszFilename : PSZ) : BOOL;[SYSTEM];
    function GpiQueryFonts(hps : HPS;flOptions : ULONG;pszFacename : PSZ;plReqFonts : PLONG;lMetricsLength : LONG;afmMetrics : PFONTMETRICS) : LONG;[SYSTEM];
    function GpiQueryFontMetrics(hps : HPS;lMetricsLength : LONG;pfmMetrics : PFONTMETRICS) : BOOL;[SYSTEM];
    function GpiQueryKerningPairs(hps : HPS;lCount : LONG;akrnprData : PKERNINGPAIRS) : LONG;[SYSTEM];
    function GpiQueryWidthTable(hps : HPS;lFirstChar : LONG;lCount : LONG;alData : PLONG) : BOOL;[SYSTEM];
    function GpiQueryNumberSetIds(hps : HPS) : LONG;[SYSTEM];
    function GpiQuerySetIds(hps : HPS;lCount : LONG;alTypes : PLONG;aNames : PSTR8;allcids : PLONG) : BOOL;[SYSTEM];
    function GpiQueryFaceString(PS : HPS;FamilyName : PSZ;attrs : PFACENAMEDESC;length : LONG;CompoundFaceName : PSZ) : ULONG;[SYSTEM];
    function GpiQueryLogicalFont(PS : HPS;lcid : LONG;name : PSTR8;attrs : PFATTRS;length : LONG) : BOOL;[SYSTEM];
    function GpiQueryFontAction(anchor : HAB;options : ULONG) : ULONG;[SYSTEM];
    function GpiLoadPublicFonts(p1 : HAB;p2 : PSZ) : BOOL;[SYSTEM];
    function GpiUnloadPublicFonts(p1 : HAB;p2 : PSZ) : BOOL;[SYSTEM];
    function GpiSetCp(hps : HPS;ulCodePage : ULONG) : BOOL;[SYSTEM];
    function GpiQueryCp(hps : HPS) : ULONG;[SYSTEM];
    function GpiQueryFontFileDescriptions(hab : HAB;pszFilename : PSZ;plCount : PLONG;affdescsNames : PFFDESCS) : LONG;[SYSTEM];
    function GpiQueryFullFontFileDescs(hab : HAB;pszFilename : PSZ;plCount : PLONG;pNames : PVOID;plNamesBuffLength : PLONG) : LONG;[SYSTEM];
    function GpiBitBlt(hpsTarget : HPS;hpsSource : HPS;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;[SYSTEM];
    function GpiDeleteBitmap(hbm : HBITMAP) : BOOL;[SYSTEM];
    function GpiLoadBitmap(hps : HPS;Resource : HMODULE;idBitmap : ULONG;lWidth : LONG;lHeight : LONG) : HBITMAP;[SYSTEM];
    function GpiSetBitmap(hps : HPS;hbm : HBITMAP) : HBITMAP;[SYSTEM];
    function GpiWCBitBlt(hpsTarget : HPS;hbmSource : HBITMAP;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;[SYSTEM];
    function GpiCreateBitmap(hps : HPS;pbmpNew : PBITMAPINFOHEADER2;flOptions : ULONG;pbInitData : PBYTE;pbmiInfoTable : PBITMAPINFO2) : HBITMAP;[SYSTEM];
    function GpiSetBitmapBits(hps : HPS;lScanStart : LONG;lScans : LONG;pbBuffer : PBYTE;pbmiInfoTable : PBITMAPINFO2) : LONG;[SYSTEM];
    function GpiSetBitmapDimension(hbm : HBITMAP;psizlBitmapDimension : PSIZEL) : BOOL;[SYSTEM];
    function GpiSetBitmapId(hps : HPS;hbm : HBITMAP;lLcid : LONG) : BOOL;[SYSTEM];
    function GpiQueryBitmapBits(hps : HPS;lScanStart : LONG;lScans : LONG;pbBuffer : PBYTE;pbmiInfoTable : PBITMAPINFO2) : LONG;[SYSTEM];
    function GpiQueryBitmapDimension(hbm : HBITMAP;psizlBitmapDimension : PSIZEL) : BOOL;[SYSTEM];
    function GpiQueryBitmapHandle(hps : HPS;lLcid : LONG) : HBITMAP;[SYSTEM];
    function GpiQueryBitmapParameters(hbm : HBITMAP;pbmpData : PBITMAPINFOHEADER) : BOOL;[SYSTEM];
    function GpiQueryBitmapInfoHeader(hbm : HBITMAP;pbmpData : PBITMAPINFOHEADER2) : BOOL;[SYSTEM];
    function GpiQueryDeviceBitmapFormats(hps : HPS;lCount : LONG;alArray : PLONG) : BOOL;[SYSTEM];
    function GpiSetPel(hps : HPS;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiQueryPel(hps : HPS;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiFloodFill(hps : HPS;lOptions : LONG;lColor : LONG) : LONG;[SYSTEM];
    function GpiDrawBits(hps : HPS;pBits : PVOID;pbmiInfoTable : PBITMAPINFO2;lCount : LONG;aptlPoints : PPOINTL;lRop : LONG;flOptions : ULONG) : LONG;[SYSTEM];
    function GpiCombineRegion(hps : HPS;hrgnDest : HRGN;hrgnSrc1 : HRGN;hrgnSrc2 : HRGN;lMode : LONG) : LONG;[SYSTEM];
    function GpiCreateRegion(hps : HPS;lCount : LONG;arclRectangles : PRECTL) : HRGN;[SYSTEM];
    function GpiDestroyRegion(hps : HPS;hrgn : HRGN) : BOOL;[SYSTEM];
    function GpiEqualRegion(hps : HPS;hrgnSrc1 : HRGN;hrgnSrc2 : HRGN) : LONG;[SYSTEM];
    function GpiOffsetRegion(hps : HPS;Hrgn : HRGN;pptlOffset : PPOINTL) : BOOL;[SYSTEM];
    function GpiPaintRegion(hps : HPS;hrgn : HRGN) : LONG;[SYSTEM];
    function GpiFrameRegion(hps : HPS;hrgn : HRGN;thickness : PSIZEL) : LONG;[SYSTEM];
    function GpiPtInRegion(hps : HPS;hrgn : HRGN;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiQueryRegionBox(hps : HPS;hrgn : HRGN;prclBound : PRECTL) : LONG;[SYSTEM];
    function GpiQueryRegionRects(hps : HPS;hrgn : HRGN;prclBound : PRECTL;prgnrcControl : PRGNRECT;prclRect : PRECTL) : BOOL;[SYSTEM];
    function GpiRectInRegion(hps : HPS;hrgn : HRGN;prclRect : PRECTL) : LONG;[SYSTEM];
    function GpiSetRegion(hps : HPS;hrgn : HRGN;lcount : LONG;arclRectangles : PRECTL) : BOOL;[SYSTEM];
    function GpiSetClipRegion(hps : HPS;hrgn : HRGN;phrgnOld : PHRGN) : LONG;[SYSTEM];
    function GpiQueryClipRegion(hps : HPS) : HRGN;[SYSTEM];
    function GpiQueryClipBox(hps : HPS;prclBound : PRECTL) : LONG;[SYSTEM];
    function GpiExcludeClipRectangle(hps : HPS;prclRectangle : PRECTL) : LONG;[SYSTEM];
    function GpiIntersectClipRectangle(hps : HPS;prclRectangle : PRECTL) : LONG;[SYSTEM];
    function GpiOffsetClipRegion(hps : HPS;pptlPoint : PPOINTL) : LONG;[SYSTEM];
    function GpiCopyMetaFile(hmf : HMF) : HMF;[SYSTEM];
    function GpiDeleteMetaFile(hmf : HMF) : BOOL;[SYSTEM];
    function GpiLoadMetaFile(hab : HAB;pszFilename : PSZ) : HMF;[SYSTEM];
    function GpiPlayMetaFile(hps : HPS;hmf : HMF;lCount1 : LONG;alOptarray : PLONG;plSegCount : PLONG;lCount2 : LONG;pszDesc : PSZ) : LONG;[SYSTEM];
    function GpiQueryMetaFileBits(hmf : HMF;lOffset : LONG;lLength : LONG;pbData : PBYTE) : BOOL;[SYSTEM];
    function GpiQueryMetaFileLength(hmf : HMF) : LONG;[SYSTEM];
    function GpiSaveMetaFile(hmf : HMF;pszFilename : PSZ) : BOOL;[SYSTEM];
    function GpiSetMetaFileBits(hmf : HMF;lOffset : LONG;lLength : LONG;pbBuffer : PBYTE) : BOOL;[SYSTEM];
    function GpiQueryDefArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;[SYSTEM];
    function GpiQueryDefAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;[SYSTEM];
    function GpiQueryDefTag(hps : HPS;plTag : PLONG) : BOOL;[SYSTEM];
    function GpiQueryDefViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;[SYSTEM];
    function GpiSetDefArcParams(hps : HPS;parcpArcParams : PARCPARAMS) : BOOL;[SYSTEM];
    function GpiSetDefAttrs(hps : HPS;lPrimType : LONG;flAttrMask : ULONG;ppbunAttrs : PBUNDLE) : BOOL;[SYSTEM];
    function GpiSetDefTag(hps : HPS;lTag : LONG) : BOOL;[SYSTEM];
    function GpiSetDefViewingLimits(hps : HPS;prclLimits : PRECTL) : BOOL;[SYSTEM];
    function GpiPolygons(hps : HPS;ulCount : ULONG;paplgn : PPOLYGON;flOptions : ULONG;flModel : ULONG) : LONG;[SYSTEM];

end.
