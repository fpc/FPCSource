{****************************************************************************


                            PMGPI interface unit
                     FPC Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Klaempfl
                    Copyright (c) 1999-2000 by Ramon Bosque

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

 ****************************************************************************}

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

unit pmgpi;

interface

{$MACRO ON}

uses    os2def,pmbitmap;

const   GPI_ERROR                           =       0;
        GPI_OK                              =       1;
        GPI_ALTERROR                        =    (-1);

        CLR_NOINDEX                         =  (-254);

        PU_ARBITRARY                        =  $0004;
        PU_PELS                             =  $0008;
        PU_LOMETRIC                         =  $000C;
        PU_HIMETRIC                         =  $0010;
        PU_LOENGLISH                        =  $0014;
        PU_HIENGLISH                        =  $0018;
        PU_TWIPS                            =  $001C;
        GPIF_DEFAULT                        =      0;
        GPIF_SHORT                          =  $0100;
        GPIF_LONG                           =  $0200;
        GPIT_NORMAL                         =      0;
        GPIT_MICRO                          =  $1000;
        GPIA_NOASSOC                        =      0;
        GPIA_ASSOC                          =  $4000;
        HDC_ERROR                           =     -1;

        GRES_ATTRS                          =  $0001;
        GRES_SEGMENTS                       =  $0002;
        GRES_ALL                            =  $0004;
        PS_UNITS                            =  $00FC;
        PS_FORMAT                           =  $0F00;
        PS_TYPE                             =  $1000;
        PS_MODE                             =  $2000;
        PS_ASSOCIATE                        =  $4000;
        PS_NORESET                          =  $8000;
        GPIE_SEGMENT                        =      0;
        GPIE_ELEMENT                        =      1;
        GPIE_DATA                           =      2;
        DCTL_ERASE                          =      1;
        DCTL_DISPLAY                        =      2;
        DCTL_BOUNDARY                       =      3;
        DCTL_DYNAMIC                        =      4;
        DCTL_CORRELATE                      =      5;
        DCTL_ERROR                          =     -1;
        DCTL_OFF                            =      0;
        DCTL_ON                             =      1;
        SDW_ERROR                           =     -1;
        SDW_OFF                             =      0;
        SDW_ON                              =      1;
        DM_ERROR                            =      0;
        DM_DRAW                             =      1;
        DM_RETAIN                           =      2;
        DM_DRAWANDRETAIN                    =      3;

        PICKAP_DEFAULT                      =      0;
        PICKAP_REC                          =      2;
        PICKSEL_VISIBLE                     =      0;
        PICKSEL_ALL                         =      1;
        GPI_HITS                            =      2;

        DFORM_NOCONV                        =      0;
        DFORM_S370SHORT                     =      1;
        DFORM_PCSHORT                       =      2;
        DFORM_PCLONG                        =      4;
        ATTR_ERROR                          =   (-1);
        ATTR_DETECTABLE                     =      1;
        ATTR_VISIBLE                        =      2;
        ATTR_CHAINED                        =      6;
        ATTR_DYNAMIC                        =      8;
        ATTR_FASTCHAIN                      =      9;
        ATTR_PROP_DETECTABLE                =     10;
        ATTR_PROP_VISIBLE                   =     11;
        ATTR_OFF                            =      0;
        ATTR_ON                             =      1;
        LOWER_PRI                           =   (-1);
        HIGHER_PRI                          =      1;

        SEGEM_ERROR                         =      0;
        SEGEM_INSERT                        =      1;
        SEGEM_REPLACE                       =      2;

        CVTC_WORLD                          =      1;
        CVTC_MODEL                          =      2;
        CVTC_DEFAULTPAGE                    =      3;
        CVTC_PAGE                           =      4;
        CVTC_DEVICE                         =      5;
        TRANSFORM_REPLACE                   =      0;
        TRANSFORM_ADD                       =      1;
        TRANSFORM_PREEMPT                   =      2;

        MPATH_STROKE                        =      6;
        FPATH_ALTERNATE                     =      0;
        FPATH_WINDING                       =      2;
        FPATH_EXCL                          =      0;
        FPATH_INCL                          =      8;
        SCP_ALTERNATE                       =      0;
        SCP_WINDING                         =      2;
        SCP_AND                             =      4;
        SCP_RESET                           =      0;
        SCP_EXCL                            =      0;
        SCP_INCL                            =      8;

        LCOL_RESET                          =  $0001;
        LCOL_REALIZABLE                     =  $0002;
        LCOL_PURECOLOR                      =  $0004;
        LCOL_OVERRIDE_DEFAULT_COLORS        =  $0008;
        LCOL_REALIZED                       =  $0010;
        LCOLF_DEFAULT                       =      0;
        LCOLF_INDRGB                        =      1;
        LCOLF_CONSECRGB                     =      2;
        LCOLF_RGB                           =      3;
        LCOLF_PALETTE                       =      4;
        LCOLOPT_REALIZED                    =  $0001;
        LCOLOPT_INDEX                       =  $0002;
        QLCT_ERROR                          =   (-1);
        QLCT_RGB                            =   (-2);
        QLCT_NOTLOADED                      =   (-1);
        QCD_LCT_FORMAT                      =      0;
        QCD_LCT_LOINDEX                     =      1;
        QCD_LCT_HIINDEX                     =      2;
        QCD_LCT_OPTIONS                     =      3;
        PAL_ERROR                           =   (-1);
        PC_RESERVED                         =    $01;
        PC_EXPLICIT                         =    $02;
        PC_NOCOLLAPSE                       =    $04;

        CLR_false                           =   (-5);
        CLR_true                            =   (-4);
        CLR_error                           = (-255);
        CLR_default                         =   (-3);
        CLR_white                           =   (-2);
        CLR_black                           =   (-1);
        CLR_background                      =      0;
        CLR_blue                            =      1;
        CLR_red                             =      2;
        CLR_pink                            =      3;
        CLR_green                           =      4;
        CLR_cyan                            =      5;
        CLR_yellow                          =      6;
        CLR_neutral                         =      7;
        CLR_darkgray                        =      8;
        CLR_darkblue                        =      9;
        CLR_darkred                         =     10;
        CLR_darkpink                        =     11;
        CLR_darkgreen                       =     12;
        CLR_darkcyan                        =     13;
        CLR_brown                           =     14;
        CLR_palegray                        =     15;

        RGB_error                        =    (-255);
        RGB_black                        = $00000000;
        RGB_blue                         = $000000FF;
        RGB_green                        = $0000FF00;
        RGB_cyan                         = $0000FFFF;
        RGB_red                          = $00FF0000;
        RGB_pink                         = $00FF00FF;
        RGB_yellow                       = $00FFFF00;
        RGB_white                        = $00FFFFFF;

        BA_NOBOUNDARY                       =      0;
        BA_BOUNDARY                         =  $0001;
        BA_ALTERNATE                        =      0;
        BA_WINDING                          =  $0002;
        BA_EXCL                             =      0;
        BA_INCL                             =      8;
        DRO_FILL                            =      1;
        DRO_OUTLINE                         =      2;
        DRO_OUTLINEFILL                     =      3;
        PATSYM_ERROR                        =   (-1);
        PATSYM_DEFAULT                      =      0;
        PATSYM_DENSE1                       =      1;
        PATSYM_DENSE2                       =      2;
        PATSYM_DENSE3                       =      3;
        PATSYM_DENSE4                       =      4;
        PATSYM_DENSE5                       =      5;
        PATSYM_DENSE6                       =      6;
        PATSYM_DENSE7                       =      7;
        PATSYM_DENSE8                       =      8;
        PATSYM_VERT                         =      9;
        PATSYM_HORIZ                        =     10;
        PATSYM_DIAG1                        =     11;
        PATSYM_DIAG2                        =     12;
        PATSYM_DIAG3                        =     13;
        PATSYM_DIAG4                        =     14;
        PATSYM_NOSHADE                      =     15;
        PATSYM_SOLID                        =     16;
        PATSYM_HALFTONE                     =     17;
        PATSYM_HATCH                        =     18;
        PATSYM_DIAGHATCH                    =     19;
        PATSYM_BLANK                        =     64;
        LCID_ERROR                          =   (-1);
        LCID_DEFAULT                        =      0;

        AM_ERROR                            =   (-1);
        AM_PRESERVE                         =      0;
        AM_NOPRESERVE                       =      1;
        FM_ERROR                            =   (-1);
        FM_DEFAULT                          =      0;
        FM_OR                               =      1;
        FM_OVERPAINT                        =      2;
        FM_LEAVEALONE                       =      5;
        FM_XOR                              =      4;
        FM_AND                              =      6;
        FM_SUBTRACT                         =      7;
        FM_MASKSRCNOT                       =      8;
        FM_ZERO                             =      9;
        FM_NOTMERGESRC                      =     10;
        FM_NOTXORSRC                        =     11;
        FM_INVERT                           =     12;
        FM_MERGESRCNOT                      =     13;
        FM_NOTCOPYSRC                       =     14;
        FM_MERGENOTSRC                      =     15;
        FM_NOTMASKSRC                       =     16;
        FM_ONE                              =     17;
        BM_ERROR                            =   (-1);
        BM_DEFAULT                          =      0;
        BM_OR                               =      1;
        BM_OVERPAINT                        =      2;
        BM_LEAVEALONE                       =      5;
        BM_XOR                              =      4;
        BM_AND                              =      6;
        BM_SUBTRACT                         =      7;
        BM_MASKSRCNOT                       =      8;
        BM_ZERO                             =      9;
        BM_NOTMERGESRC                      =     10;
        BM_NOTXORSRC                        =     11;
        BM_INVERT                           =     12;
        BM_MERGESRCNOT                      =     13;
        BM_NOTCOPYSRC                       =     14;
        BM_MERGENOTSRC                      =     15;
        BM_NOTMASKSRC                       =     16;
        BM_ONE                              =     17;
        BM_SRCTRANSPARENT                   =     18;
        BM_DESTTRANSPARENT                  =     19;
        LINETYPE_ERROR                      =   (-1);
        LINETYPE_DEFAULT                    =      0;
        LINETYPE_DOT                        =      1;
        LINETYPE_SHORTDASH                  =      2;
        LINETYPE_DASHDOT                    =      3;
        LINETYPE_DOUBLEDOT                  =      4;
        LINETYPE_LONGDASH                   =      5;
        LINETYPE_DASHDOUBLEDOT              =      6;
        LINETYPE_SOLID                      =      7;
        LINETYPE_INVISIBLE                  =      8;
        LINETYPE_ALTERNATE                  =      9;
        LINEWIDTH_ERROR                     =   (-1);
        LINEWIDTH_DEFAULT                   =      0;
        LINEWIDTH_NORMAL                =  $00010000;
        LINEWIDTH_THICK                 =  $00020000;
        LINEWIDTHGEOM_ERROR             =       (-1);
        LINEEND_ERROR                       =   (-1);
        LINEEND_DEFAULT                     =      0;
        LINEEND_FLAT                        =      1;
        LINEEND_SQUARE                      =      2;
        LINEEND_ROUND                       =      3;
        LINEJOIN_ERROR                      =   (-1);
        LINEJOIN_DEFAULT                    =      0;
        LINEJOIN_BEVEL                      =      1;
        LINEJOIN_ROUND                      =      2;
        LINEJOIN_MITRE                      =      3;
        CHDIRN_ERROR                        =   (-1);
        CHDIRN_DEFAULT                      =      0;
        CHDIRN_LEFTRIGHT                    =      1;
        CHDIRN_TOPBOTTOM                    =      2;
        CHDIRN_RIGHTLEFT                    =      3;
        CHDIRN_BOTTOMTOP                    =      4;
        TA_NORMAL_HORIZ                     =  $0001;
        TA_LEFT                             =  $0002;
        TA_CENTER                           =  $0003;
        TA_RIGHT                            =  $0004;
        TA_STANDARD_HORIZ                   =  $0005;
        TA_NORMAL_VERT                      =  $0100;
        TA_TOP                              =  $0200;
        TA_HALF                             =  $0300;
        TA_BASE                             =  $0400;
        TA_BOTTOM                           =  $0500;
        TA_STANDARD_VERT                    =  $0600;
        CM_ERROR                            =   (-1);
        CM_DEFAULT                          =      0;
        CM_MODE1                            =      1;
        CM_MODE2                            =      2;
        CM_MODE3                            =      3;
        MARKSYM_ERROR                       =   (-1);
        MARKSYM_DEFAULT                     =      0;
        MARKSYM_CROSS                       =      1;
        MARKSYM_PLUS                        =      2;
        MARKSYM_DIAMOND                     =      3;
        MARKSYM_SQUARE                      =      4;
        MARKSYM_SIXPOINTSTAR                =      5;
        MARKSYM_EIGHTPOINTSTAR              =      6;
        MARKSYM_SOLIDDIAMOND                =      7;
        MARKSYM_SOLIDSQUARE                 =      8;
        MARKSYM_DOT                         =      9;
        MARKSYM_SMALLCIRCLE                 =     10;
        MARKSYM_BLANK                       =     64;
        CHS_OPAQUE                          =  $0001;
        CHS_VECTOR                          =  $0002;
        CHS_LEAVEPOS                        =  $0008;
        CHS_CLIP                            =  $0010;
        CHS_UNDERSCORE                      =  $0200;
        CHS_STRIKEOUT                       =  $0400;
        PRIM_LINE                           =      1;
        PRIM_CHAR                           =      2;
        PRIM_MARKER                         =      3;
        PRIM_AREA                           =      4;
        PRIM_IMAGE                          =      5;
        LBB_COLOR                           =  $0001;
        LBB_BACK_COLOR                      =  $0002;
        LBB_MIX_MODE                        =  $0004;
        LBB_BACK_MIX_MODE                   =  $0008;
        LBB_WIDTH                           =  $0010;
        LBB_GEOM_WIDTH                      =  $0020;
        LBB_TYPE                            =  $0040;
        LBB_END                             =  $0080;
        LBB_JOIN                            =  $0100;
        CBB_COLOR                           =  $0001;
        CBB_BACK_COLOR                      =  $0002;
        CBB_MIX_MODE                        =  $0004;
        CBB_BACK_MIX_MODE                   =  $0008;
        CBB_SET                             =  $0010;
        CBB_MODE                            =  $0020;
        CBB_BOX                             =  $0040;
        CBB_ANGLE                           =  $0080;
        CBB_SHEAR                           =  $0100;
        CBB_DIRECTION                       =  $0200;
        CBB_TEXT_ALIGN                      =  $0400;
        CBB_EXTRA                           =  $0800;
        CBB_BREAK_EXTRA                     =  $1000;
        MBB_COLOR                           =  $0001;
        MBB_BACK_COLOR                      =  $0002;
        MBB_MIX_MODE                        =  $0004;
        MBB_BACK_MIX_MODE                   =  $0008;
        MBB_SET                             =  $0010;
        MBB_SYMBOL                          =  $0020;
        MBB_BOX                             =  $0040;
        ABB_COLOR                           =  $0001;
        ABB_BACK_COLOR                      =  $0002;
        ABB_MIX_MODE                        =  $0004;
        ABB_BACK_MIX_MODE                   =  $0008;
        ABB_SET                             =  $0010;
        ABB_SYMBOL                          =  $0020;
        ABB_REF_POINT                       =  $0040;
        IBB_COLOR                           =  $0001;
        IBB_BACK_COLOR                      =  $0002;
        IBB_MIX_MODE                        =  $0004;
        IBB_BACK_MIX_MODE                   =  $0008;

        TXTBOX_TOPLEFT                      =      0;
        TXTBOX_BOTTOMLEFT                   =      1;
        TXTBOX_TOPRIGHT                     =      2;
        TXTBOX_BOTTOMRIGHT                  =      3;
        TXTBOX_CONCAT                       =      4;
        TXTBOX_COUNT                        =      5;
        PVIS_ERROR                          =      0;
        PVIS_INVISIBLE                      =      1;
        PVIS_VISIBLE                        =      2;
        RVIS_ERROR                          =      0;
        RVIS_INVISIBLE                      =      1;
        RVIS_PARTIAL                        =      2;
        RVIS_VISIBLE                        =      3;

        FONT_DEFAULT                        =      1;
        FONT_MATCH                          =      2;
        LCIDT_FONT                          =      6;
        LCIDT_BITMAP                        =      7;
        LCID_ALL                            =   (-1);

        FWEIGHT_DONT_CARE                   =      0;
        FWEIGHT_ULTRA_LIGHT                 =      1;
        FWEIGHT_EXTRA_LIGHT                 =      2;
        FWEIGHT_LIGHT                       =      3;
        FWEIGHT_SEMI_LIGHT                  =      4;
        FWEIGHT_NORMAL                      =      5;
        FWEIGHT_SEMI_BOLD                   =      6;
        FWEIGHT_BOLD                        =      7;
        FWEIGHT_EXTRA_BOLD                  =      8;
        FWEIGHT_ULTRA_BOLD                  =      9;
        FWIDTH_DONT_CARE                    =      0;
        FWIDTH_ULTRA_CONDENSED              =      1;
        FWIDTH_EXTRA_CONDENSED              =      2;
        FWIDTH_CONDENSED                    =      3;
        FWIDTH_SEMI_CONDENSED               =      4;
        FWIDTH_NORMAL                       =      5;
        FWIDTH_SEMI_EXPANDED                =      6;
        FWIDTH_EXPANDED                     =      7;
        FWIDTH_EXTRA_EXPANDED               =      8;
        FWIDTH_ULTRA_EXPANDED               =      9;
        FTYPE_ITALIC                        =  $0001;
        FTYPE_ITALIC_DONT_CARE              =  $0002;
        FTYPE_OBLIQUE                       =  $0004;
        FTYPE_OBLIQUE_DONT_CARE             =  $0008;
        FTYPE_ROUNDED                       =  $0010;
        FTYPE_ROUNDED_DONT_CARE             =  $0020;
        QFA_PUBLIC                          =      1;
        QFA_PRIVATE                         =      2;
        QFA_ERROR                      =GPI_ALTERROR;
        QF_PUBLIC                           =  $0001;
        QF_PRIVATE                          =  $0002;
        QF_NO_GENERIC                       =  $0004;
        QF_NO_DEVICE                        =  $0008;

        ROP_SRCCOPY                         =  $00CC;
        ROP_SRCPAINT                        =  $00EE;
        ROP_SRCAND                          =  $0088;
        ROP_SRCINVERT                       =  $0066;
        ROP_SRCERASE                        =  $0044;
        ROP_NOTSRCCOPY                      =  $0033;
        ROP_NOTSRCERASE                     =  $0011;
        ROP_MERGECOPY                       =  $00C0;
        ROP_MERGEPAINT                      =  $00BB;
        ROP_PATCOPY                         =  $00F0;
        ROP_PATPAINT                        =  $00FB;
        ROP_PATINVERT                       =  $005A;
        ROP_DSTINVERT                       =  $0055;
        ROP_ZERO                            =  $0000;
        ROP_ONE                             =  $00FF;
        BBO_OR                              =      0;
        BBO_AND                             =      1;
        BBO_IGNORE                          =      2;
        BBO_PAL_COLORS                      =      4;
        BBO_NO_COLOR_INFO                   =      8;
        FF_BOUNDARY                         =      0;
        FF_SURFACE                          =      1;
        HBM_ERROR                           =     -1;

        {Bitmaps}
        CBM_INIT                            =  $0004;
        BMB_ERROR                           =   (-1);

        {Regions}
        CRGN_OR                             =      1;
        CRGN_COPY                           =      2;
        CRGN_XOR                            =      4;
        CRGN_AND                            =      6;
        CRGN_DIFF                           =      7;
        RECTDIR_LFRT_TOPBOT                 =      1;
        RECTDIR_RTLF_TOPBOT                 =      2;
        RECTDIR_LFRT_BOTTOP                 =      3;
        RECTDIR_RTLF_BOTTOP                 =      4;
        RGN_ERROR                           =      0;
        RGN_NULL                            =      1;
        RGN_RECT                            =      2;
        RGN_COMPLEX                         =      3;
        PRGN_ERROR                          =      0;
        PRGN_OUTSIDE                        =      1;
        PRGN_INSIDE                         =      2;
        RRGN_ERROR                          =      0;
        RRGN_OUTSIDE                        =      1;
        RRGN_PARTIAL                        =      2;
        RRGN_INSIDE                         =      3;
        EQRGN_ERROR                         =      0;
        EQRGN_NOTEQUAL                      =      1;
        EQRGN_EQUAL                         =      2;
        HRGN_ERROR                          =     -1;

        {Metafiles}
        PMF_SEGBASE                         =      0;
        PMF_LOADTYPE                        =      1;
        PMF_RESOLVE                         =      2;
        PMF_LCIDS                           =      3;
        PMF_RESET                           =      4;
        PMF_SUPPRESS                        =      5;
        PMF_COLORTABLES                     =      6;
        PMF_COLORREALIZABLE                 =      7;
        PMF_DEFAULTS                        =      8;
        PMF_DELETEOBJECTS                   =      9;
        RS_DEFAULT                          =      0;
        RS_NODISCARD                        =      1;
        LC_DEFAULT                          =      0;
        LC_NOLOAD                           =      1;
        LC_LOADDISC                         =      3;
        LT_DEFAULT                          =      0;
        LT_NOMODIFY                         =      1;
        LT_ORIGINALVIEW                     =      4;
        RES_DEFAULT                         =      0;
        RES_NORESET                         =      1;
        RES_RESET                           =      2;
        SUP_DEFAULT                         =      0;
        SUP_NOSUPPRESS                      =      1;
        SUP_SUPPRESS                        =      2;
        CTAB_DEFAULT                        =      0;
        CTAB_NOMODIFY                       =      1;
        CTAB_REPLACE                        =      3;
        CTAB_REPLACEPALETTE                 =      4;
        CREA_DEFAULT                        =      0;
        CREA_REALIZE                        =      1;
        CREA_NOREALIZE                      =      2;
        CREA_DOREALIZE                      =      3;
        DDEF_DEFAULT                        =      0;
        DDEF_IGNORE                         =      1;
        DDEF_LOADDISC                       =      3;
        DOBJ_DEFAULT                        =      0;
        DOBJ_NODELETE                       =      1;
        DOBJ_DELETE                         =      2;
        RSP_DEFAULT                         =      0;
        RSP_NODISCARD                       =      1;

        {Polygons}
        POLYGON_NOBOUNDARY                  =      0;
        POLYGON_BOUNDARY                    =  $0001;
        POLYGON_ALTERNATE                   =      0;
        POLYGON_WINDING                     =  $0002;
        POLYGON_EXCL                        =      0;
        POLYGON_INCL                        =  $0008;

type    SizeL=record
            cx,cy:longint;
        end;
        PSizeL=^SizeL;
        TSizeL=SizeL;

        MatrixLF=record
            fxm11:longint;
            fxm12:longint;
            lm13:longint;
            fxm21:longint;
            fxm22:longint;
            lm23:longint;
            lm31:longint;
            lm32:longint;
            lm33:longint;
        end;
        PMatrixLF=^MatrixLF;
        TMatrixLF=MatrixLF;

        ArcParams=record
            lp,lq,lr,ls:longint;
        end;
        PArcParams=^ArcParams;
        TArcParams=ArcParams;

        SizeF=record
            cx,cy:longint;
        end;
        PSizeF=^SizeF;
        TSizeF=SizeF;

        GradientL=record
            x,y:longint;
        end;
        PGradientL=^GradientL;
        TGradientL=GradientL;

        LineBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            fxWidth:longint;
            lGeomWidth:longint;
            usType:word;
            usEnd:word;
            usJoin:word;
            usReserved:word;
        end;
        PLineBundle=^LineBundle;
        TLineBundle=LineBundle;

        CharBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usPrecision:word;
            sizfxCell:sizef;
            ptlAngle:pointl;
            ptlShear:pointl;
            usDirection:word;
            usTextAlign:word;
            fxExtra:longint;
            fxBreakExtra:longint;
        end;
        PCharBundle=^CharBundle;
        TCharBundle=CharBundle;

        MarkerBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usSymbol:word;
            sizFxCell:SizeF;
        end;
        PMarkerBundle=^MarkerBundle;
        TMarkerBundle=MarkerBundle;

        AreaBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
            usSet:word;
            usSymbol:word;
            ptlRefPoint:pointl;
        end;
        PAreaBundle=^AreaBundle;
        TAreaBundle=AreaBundle;

        ImageBundle=record
            lColor:longint;
            lBackColor:longint;
            usMixMode:word;
            usBackMixMode:word;
        end;
        PImageBundle=^ImageBundle;
        TImageBundle=ImageBundle;

        KerningPairs=record
            sFirstChar:integer;
            sSecondChar:integer;
            lKerningAmount:longint;
        end;
        PKerningPairs=^KerningPairs;
        TKerningPairs=KerningPairs;

        FaceNameDesc=record
            usSize:word;
            usWeightClass:word;
            usWidthClass:word;
            usReserved:word;
            flOptions:cardinal;
        end;
        PFaceNameDesc=^FaceNameDesc;
        TFaceNameDesc=FaceNameDesc;

        FFDescs=array[0..1,0..FaceSize-1] of char;
        PFFDescs=^FFDescs;
        TFFDescs = FFDescs;

        FFDescs2=record
            cbLength:cardinal;
            cbFacenameOffset:cardinal;
            abFamilyName:array[0..1-1] of byte;
        end;
        PFFDescs2=^FFDescs2;
        TFFDescs2=FFDescs2;

        RgnRect=record
            ircStart:cardinal;
            crc:cardinal;
            crcReturned:cardinal;
            ulDirection:cardinal;
        end;
        PRgnRect=^RgnRect;
        TRgnRect=RgnRect;

        Polygon=record
            ulPoints:cardinal;
            aPointl:Ppointl;
        end;
        PPolygon=^Polygon;
        TPolygon=Polygon;

        Polyset=record
            ulPolys:cardinal;
            aPolygon:array[0..1-1] of TPolygon;
        end;
        PPolyset=^Polyset;
        TPolyset=Polyset;

// ===========================================================================
//*
//* The orders fall into 4 categories :-
//*
//* 1) 1-byte orders
//*
//* 2) 2-byte orders    - second byte contains the value
//*
//* 3) Long orders      - second byte gives the order length, subsequent bytes
//*                       contain the values (up to 256 bytes long)
//*
//* 4) Very long orders - third and fourth bytes gives the order length,
//*                       subsequent bytes contain the values (up to 64K long)
//*
//* ===========================================================================

//#pragma pack(1)      /* pack on byte boundary */

//***************************************************************************\
//*
//* Miscellaneous structures used in this file
//*
//***************************************************************************/

// form of RECTL with shorts instead of longs
type
  RECT1S=record        // rcs
    xLeft: Integer;
    yBottom: Integer;
    xRight: Integer;
    yTop: Integer;
  end;

// form of POINTL with 1 byte offsets instead of longs
  ODPOINT=record          // odpt
    dx: Char;
    dy: Char;
  end;

// form of SIZEL with shorts instead of longs
  SIZES=record            // sizs
    cx: Integer;
    cy: Integer;
  end;

// unsigned two-byte swapped integer
  SWPUSHORT=record        // swpus
    HiByte: Byte;
    LoByte: Byte;
  end;

//***************************************************************************\
//*
//* 1-byte orders
//*
//***************************************************************************/

// macro to tell whether this is a 1-byte order
{$define BYTE_ORDER(oc):=((oc)=OCODE_GNOP1 or (oc)=OCODE_GESD)}

// 1-byte order codes
const
  OCODE_GNOP1    =$00;            // No-operation
  OCODE_GESD     =$FF;            // End symbol definition

//***************************************************************************\
//
// 2-byte orders
//
//***************************************************************************/

// definitions to help determine whether an order code is a 2-byte order
const
  OCODE2_1       =$80;
  OCODE2_2       =$88;

{$define SHORT_ORDER(oc):=((((oc) xor OCODE2_1) and OCODE2_2)=OCODE2_2)}

// General 2-byte order structure
type
  ORDER=record        // ord
    idCode: Byte;
    uchData: Byte;
  end;

// 2-byte order codes
const
  OCODE_GBAR     =$68;           // Begin area
  OCODE_GCFIG    =$7D;           // Close figure
  OCODE_GEEL     =$49;           // End element
  OCODE_GEPTH    =$7F;           // End path
  OCODE_GEPROL   =$3E;           // End prologue
  OCODE_GPOP     =$3F;           // Pop
  OCODE_GSBMX    =$0D;           // Set background mix
  OCODE_GPSBMX   =$4D;           // Push & set b/g mix
  OCODE_GSCD     =$3A;           // Set char direction
  OCODE_GPSCD    =$7A;           // Push & set char direction
  OCODE_GSCR     =$39;           // Set char precision
  OCODE_GPSCR    =$79;           // Push & set char precision
  OCODE_GSCS     =$38;           // Set char set
  OCODE_GPSCS    =$78;           // Push & set char set
  OCODE_GSCOL    =$0A;           // Set color
  OCODE_GPSCOL   =$4A;           // Push & set color
  OCODE_GSLE     =$1A;           // Set line end
  OCODE_GPSLE    =$5A;           // Push & set line end
  OCODE_GSLJ     =$1B;           // Set line join
  OCODE_GPSLJ    =$5B;           // Push & set line join
  OCODE_GSLT     =$18;           // Set line type
  OCODE_GPSLT    =$58;           // Push & set line type
  OCODE_GSLW     =$19;           // Set line width
  OCODE_GPSLW    =$59;           // Push & set line width
  OCODE_GSMP     =$3B;           // Set marker precision
  OCODE_GPSMP    =$7B;           // Push & set marker precision
  OCODE_GSMS     =$3C;           // Set marker set
  OCODE_GPSMS    =$7C;           // Push & set marker set
  OCODE_GSMT     =$29;           // Set marker symbol
  OCODE_GPSMT    =$69;           // Push & set marker symbol
  OCODE_GSMX     =$0C;           // Set mix
  OCODE_GPSMX    =$4C;           // Push & set mix
  OCODE_GSPS     =$08;           // Set pattern set
  OCODE_GPSPS    =$48;           // Push & set pattern set
  OCODE_GSPT     =$28;           // Set pattern symbol
  OCODE_GPSPT    =$09;           // Push & set pattern symbol

// constants for 2-byte orders

// Begin area
const
  GBAR_RESERVED   =$80;
  GBAR_BOUNDARY   =$C0;
  GBAR_NOBOUNDARY =$80;
  GBAR_WINDING    =$A0;
  GBAR_ALTERNATE  =$80;

// Set Character Precision
const
  GSCR_PRECISION  =$0F;

//***************************************************************************\
//*
//* Long orders
//*
//***************************************************************************/

// definitions to help determine whether an order code is a long order
const
  OCODE_VLONG    =$FE;

{$define LONG_ORDER(oc):=(not((oc)=OCODE_VLONG or BYTE_ORDER(oc) or SHORT_ORDER(oc)))}

// long order structure
const
  LORDER_ML=253;

type
  LORDER=record           // lord
    idCode: Byte;
    uchLength: Byte;
    uchData: Array[0..LORDER_ML-1] of Byte;
  end;

// Long orders for which the length of data is normally zero
const
  OCODE_GEAR     =$60;            // End Area
  OCODE_GEIMG    =$93;            // End Image

// Long orders for which the data is contained in a type already defined

// Character String
const
  OCODE_GCCHST  = $83;            // char string at curr posn
  GCCHST_MC     = 255;            // Max len of string in bytes

  OCODE_GCHST   = $C3;            // char string at given pos
  GCHST_SMC     = 251;             // Max len of string (S)
  GCHST_LMC     = 247;             // Max len of string (L)

// Character String Move
  OCODE_GCCHSTM = $B1;            // char string move at c.p.
  GCCHSTM_MC    = 255;             // Max len of string in byte

  OCODE_GCHSTM  = $F1;            // char string move at g.p.
  GCHSTM_SMC    = 251;             // Max len of string (S)
  GCHSTM_LMC    = 247;             // Max len of string (L)

// Comment
  OCODE_GCOMT   = $01;            // Comment
  GCOMT_ML      = 255;             // Maximum len of comment data

// Image
  OCODE_GIMD    = $92;            // Image data
  GIMD_ML       = 255;             // Maximum len of image data

// Full Arc
  OCODE_GCFARC  = $87;            // full arc at current posn
  OCODE_GFARC   = $C7;            // full arc at given posn

// Label
  OCODE_GLABL   = $D3;            // Label

// Set Current Position
  OCODE_GSCP    = $21;            // Set current position
  OCODE_GPSCP   = $61;            // Push and set curr posn

// Bezier spline
  OCODE_GCBEZ   = $A5;            // Bezier spline at curr pos
  GCBEZ_SMB     = 21;              // Max number of splines (S)
  GCBEZ_LMB     = 10;              // Max number of splines (L)

  OCODE_GBEZ    = $E5;            // Bezier spline at given pos
  GBEZ_SMB      = 20;              // Max number of splines (S)
  GBEZ_LMB      = 10;              // Max number of splines (L)

// Fillet
  OCODE_GCFLT   = $85;            // fillet at current posn
  GCFLT_SMP     = 63;              // Max number of points (S)
  GCFLT_LMP     = 31;              // Max number of points (L)

  OCODE_GFLT    = $C5;            // fillet at given position
  GFLT_SMP      = 62;              // Max number of points (S)
  GFLT_LMP      = 30;              // Max number of points (L)

// Polyline
  OCODE_GCLINE  = $81;            // polyline at current posn
  GCLINE_SMP    = 63;              // Max number of points (S)
  GCLINE_LMP    = 31;              // Max number of points (L)

  OCODE_GLINE   = $C1;            // polyline at given posn
  GLINE_SMP     = 62;              // Max number of points (S)
  GLINE_LMP     = 30;              // Max number of points (L)

// Polymarker
  OCODE_GCMRK   = $82;            // marker at current posn
  GCMRK_SMP     = 63;              // Max number of points (S)
  GCMRK_LMP     = 31;              // Max number of points (L)

  OCODE_GMRK    = $C2;            // marker at given posn
  GMRK_SMP      = 62;              // Max number of points (S)
  GMRK_LMP      = 30;              // Max number of points (L)

// Relative Line
  OCODE_GCRLINE  =$A1;            // Relative line at curr pos
  GCRLINE_MP     =127;             // Max number of points

  OCODE_GRLINE  = $E1;            // Relative line at givn pos
  GRLINE_SMP    = 125;             // Max number of points (S)
  GRLINE_LMP    = 123;             // Max number of points (L)

// Set Background Color
  OCODE_GSBCOL  = $25;            // Set background color
  OCODE_GPSBCOL = $65;            // Push and set b/g color

// Set Extended Color
  OCODE_GSECOL  = $26;            // Set extended color
  OCODE_GPSECOL = $66;            // Push and set ext color

// Extended Color values
  SECOL_DEFAULT0  =$0000;
  SECOL_DEFAULT1  =$FF00;
  SECOL_NEUTRAL   =$FF07;
  SECOL_RESET     =$FF08;

// Set Character Angle
  OCODE_GSCA    = $34;            // Set character angle
  OCODE_GPSCA   = $74;            // Push and set char angle

// Set Character Shear
  OCODE_GSCH    = $35;            // Set character shear
  OCODE_GPSCH   = $75;            // Push and set char shear

// Set Fractional Line Width
  OCODE_GSFLW   = $11;            // Set fractional line width
  OCODE_GPSFLW  = $51;            // Push and set frac l width

// Set Pick Identifier
  OCODE_GSPIK   = $43;            // Set pick identifier
  OCODE_GPSPIK  = $23;            // Push and set pick id


// Long Orders for which a structure can be defined for the data

// Arc
  OCODE_GCARC   = $86;            // Arc at Current Position
  OCODE_GARC    = $C6;            // Arc at Given Position

type
  ORDERS_GCARC=record     // osgcarc
    ptInter: POINTS;
    ptEnd: POINTS;
  end;

  ORDERL_GCARC=record     // olgcarc
    ptInter: POINTL;
    ptEnd: POINTL;
  end;

// Begin Element
const
  OCODE_GBEL    = $D2;            // Begin Element

  GBEL_DL       = 251;

type
  ORDER_GBEL=record       // ogbel
    lElementType: Longint;
    achDesc: Array[0..GBEL_DL-1] of Char;
  end;

// Begin Image
const
  OCODE_GCBIMG  = $91;            // Begin Image at curr posn
  OCODE_GBIMG   = $D1;            // Begin Image at given posn

type
  ORDER_GCBIMG=record     // ogbimg
    uchFormat: Byte;
    uchReserved: Byte;
    cx: SWPUSHORT;
    cy: SWPUSHORT;
  end;

// Begin Path
const
  OCODE_GBPTH   = $D0;            // Begin Path

type
  ORDER_GBPTH=record      // ogbpth
    usReserved: Word;
    idPath: Longint;
  end;

// Box
const
  OCODE_GCBOX    =$80;            // Box at current position
  OCODE_GBOX     =$C0;            // Box at given position

type
  ORDERS_GCBOX=record     // osgcbox
    fbFlags: Byte;
    uchReserved: Byte;
    ptCorner: POINTS;
    hAxis: Integer;
    vAxis: Integer;
  end;

  ORDERL_GCBOX=record     // olgcbox
    fbFlags: Byte;
    uchReserved: Byte;
    ptCorner: POINTL;
    hAxis: Longint;
    vAxis: Longint;
  end;

const
  GCBOX_FILL     =$40;
  GCBOX_BOUNDARY =$20;

// Call Segment
  OCODE_GCALLS   =$07;            // call segment

type
  ORDER_GCALLS=record     // ogcalls
    sReserved: Word;
    idSegment: Longint;
  end;

// Fill Path
const
  OCODE_GFPTH   =$D7;            // Fill path

type
  ORDER_GFPTH=record     // ogfpth
    fbFlags: Byte;
    uchReserved: Byte;
    idPath: Longint;
  end;

const
  GFPTH_ALTERNATE =$00;
  GFPTH_WINDING   =$40;
  GFPTH_MODIFY    =$20;

// Outline Path
  OCODE_GOPTH    =$D4;            // Outline Path

type
  ORDER_GOPTH=record     // ogopth
    fbFlags: Byte;
    uchReserved: Byte;
    idPath: Longint;
  end;

// Modify Path
const
  OCODE_GMPTH =$D8;               // modify path

type
  ORDER_GMPTH=record      // ogmpth
    uchMode: Byte;
    uchReserved: Byte;
    idPath: Longint;
  end;

const
  GMPTH_STROKE   =$06;

// Partial Arc
  OCODE_GCPARC   =$A3;            // Partial arc at curr posn
  OCODE_GPARC    =$E3;            // Partial arc at given posn

type
  ORDERS_GCPARC=record    // osgcparc
    ptCenter: POINTS;
    ufx88Multiplier: FIXED88;
    usStartAngle: Longint;
    usSweepAngle: Longint;
  end;

  ORDERL_GCPARC=record    // olgcparc
    ptCenter: POINTL;
    ufxMultiplier: FIXED;
    usStartAngle: Longint;
    usSweepAngle: Longint;
  end;

// Set Clip Path
const
  OCODE_GSCPTH   =$B4;            // Set clip path

type
  ORDER_GSCPTH=record     // ogscpth
    fbFlags: Byte;
    uchReserved: Byte;
    idPath: Longint;
  end;

const
  GSCPTH_ALTERNATE =$00;
  GSCPTH_WINDING   =$40;
  GSCPTH_RESET     =$00;
  GSCPTH_INTERSECT =$20;

// Set Arc Parameters
  OCODE_GSAP     =$22;            // Set arc parameters
  OCODE_GPSAP    =$62;            // Push and set arc params

type
  ORDERS_GSAP=record      // osgsap
    p: Integer;
    q: Integer;
    r: Integer;
    s: Integer;
  end;

  ORDERL_GSAP=record      // olgsap
    p: Longint;
    q: Longint;
    r: Longint;
    s: Longint;
  end;

// Set Background Indexed Color
const
  OCODE_GSBICOL  =$A7;            // Set b/g indexed color
  OCODE_GPSBICOL =$E7;            // Push and set b/g ind color
  OCODE_GSICOL   =$A6;            // Set indexed color
  OCODE_GPSICOL  =$E6;            // Push and set indexd color


type
  ORDER_GSBICOL=record    // ogbicol
    fbFlags: Byte;
    auchColor: Array[0..3-1] of Byte;
  end;

const
  SICOL_SPECIFY  =$00;
  SICOL_SPECIAL  =$40;
  SICOL_DEFAULT  =$80;
  SICOL_BLACK    =1;
  SICOL_WHITE    =2;
  SICOL_ONES     =4;
  SICOL_ZEROES   =5;

// Set Character Cell
  OCODE_GSCC     =$33;            // Set character cell
  OCODE_GPSCC    =$03;            // Push and set char cell

type
  ORDERS_GSCC=record      // osgscc
    cxInt: Integer;
    cyInt: Integer;
    cxFract: Word;
    cyFract: Word;
    fbFlags: Byte;
    uchReserved: Byte;
  end;

  ORDERL_GSCC=record      // olgscc
    cxInt: Longint;
    cyInt: Longint;
    cxFract: Word;
    cyFract: Word;
    fbFlags: Byte;
    uchReserved: Byte;
  end;

const
  GSCC_ZERODEF   =$00;
  GSCC_ZEROZERO  =$80;

// Set Marker Cell
  OCODE_GSMC     =$37;            // Set marker cell
  OCODE_GPSMC    =$77;            // Push and set marker cell

type
  ORDERS_GSMC=record      // osgsmc
    cx: Integer;
    cy: Integer;
    fbFlags: Byte;
    uchReserved: Byte;
  end;

  ORDERL_GSMC=record      // olgsmc
    cx: Longint;
    cy: Longint;
    fbFlags: Byte;
    uchReserved: Byte;
  end;

const
  GSMC_ZERODEF   =$00;
  GSMC_ZEROZERO  =$80;

// Set Pattern Reference Point
  OCODE_GSPRP    =$A0;            // Set pattern ref point
  OCODE_GPSPRP   =$E0;            // Push and set patt ref pt

type
  ORDERS_GSPRP=record     // osgsprp
    fbFlags: Byte;
    uchReserved: Byte;
    ptPos: POINTS;
  end;

  ORDERL_GSPRP=record     // olgsprp
    fbFlags: Byte;
    uchReserved: Byte;
    ptPos: POINTL;
  end;

const
  GSPRP_DEFAULT  =$80;
  GSPRP_SPECIFY  =$00;


// Set Individual Attribute
  OCODE_GSIA     =$14;            // Set individual attribute
  OCODE_GPSIA    =$54;            // Push and set ind attr

  GSIA_VL=3;

type
  ORDER_GSIA=record       // ogsia
    uchAttrType: Byte;
    uchPrimType: Byte;
    fbFlags: Byte;
    auchValue: Array[0..GSIA_VL-1] of Byte;
  end;

const
  GSIA_COLOR     =$01;
  GSIA_BCOLOR    =$02;
  GSIA_MIX       =$03;
  GSIA_BMIX      =$04;
  GSIA_LINE      =$01;
  GSIA_CHAR      =$02;
  GSIA_MARKER    =$03;
  GSIA_PATTERN   =$04;
  GSIA_IMAGE     =$05;
  GSIA_SPECIFY   =$00;
  GSIA_SPECIAL   =$40;
  GSIA_DEFAULT   =$80;
  GSIA_BLACK     =1;
  GSIA_WHITE     =2;
  GSIA_ONES      =4;
  GSIA_ZEROES    =5;


// Set Model /Viewing Transform
  OCODE_GSTM     =$24;            // Set model transform
  OCODE_GPSTM    =$64;            // Push and set model tfm

  OCODE_GSTV     =$31;            // Set Viewing Transform

  GSTM_ML        =16;

type
  ORDERS_GSTM=record       // osgstm
    uchReserved: Byte;
    fbFlags: Byte;
    fsMask: Word;
    asMatrix: Array[0..GSTM_ML-1] of Integer;
  end;

  ORDERL_GSTM=record       // olgstm
    uchReserved: Byte;
    fbFlags: Byte;
    fsMask: Word;
    alMatrix: Array[0..GSTM_ML-1] of Longint;
  end;

const
  GSTM_M11     =$8000;
  GSTM_M12     =$4000;
  GSTM_M13     =$2000;
  GSTM_M14     =$1000;
  GSTM_M21     =$0800;
  GSTM_M22     =$0400;
  GSTM_M23     =$0200;
  GSTM_M24     =$0100;
  GSTM_M31     =$0080;
  GSTM_M32     =$0040;
  GSTM_M33     =$0020;
  GSTM_M34     =$0010;
  GSTM_M41     =$0008;
  GSTM_M42     =$0004;
  GSTM_M43     =$0002;
  GSTM_M44     =$0001;

  GSTM_UNITY     =$00;
  GSTM_AFTER     =$01;
  GSTM_BEFORE    =$02;
  GSTM_OVERWRITE =$03;

  GSTV_OVERWRITE =$00;
  GSTV_AFTER     =$04;

// Set Segment Boundary, Viewing Window
  OCODE_GSSB     =$32;            // Set segment boundary
  OCODE_GSVW     =$27;            // Set viewing window
  OCODE_GPSVW    =$67;            // Push and set view window

  GSSB_ML        =4;

type
  ORDERS_GSSB=record      // osgssb
    fbFlags: Byte;
    fbMask: Byte;
    alMatrix: Array[0..GSSB_ML-1] of Integer;
  end;

  ORDERL_GSSB=record      // olgssb
    fbFLags: Byte;
    fbMask: Byte;
    alMatrix: Array[0..GSSB_ML-1] of Longint;
  end;

const
  GSSB_XLEFT     =$20;
  GSSB_XRIGHT    =$10;
  GSSB_YBOTTOM   =$08;
  GSSB_YTOP      =$04;

  GSVW_INTERSECT =$00;
  GSVW_REPLACE   =$80;

// Set Segment Characteristics
  OCODE_GSGCH    =$04;            // Set segment characteristics

  GSGCH_ML       =254;

type
  ORDER_GSGCH=record      // ogsgch
    uchIdent: Byte;
    auchData: Array[0..GSGCH_ML-1] of Byte;
  end;

// Set Stroke Line Width
const
  OCODE_GSSLW    =$15;            // Set stroke line width
  OCODE_GPSSLW   =$55;            // Push and set strk l width

type
  ORDERS_GSSLW=record     // osgsslw
    fbFlags: Byte;
    uchReserved: Byte;
    LineWidth: Integer;
  end;

type
  ORDERL_GSSLW=record     // olgsslw
    fbFlags: Byte;
    uchReserved: Byte;
    LineWidth: Longint;
  end;

const
  GSSLW_DEFAULT  =$80;
  GSSLW_SPECIFY  =$00;

// Sharp Fillet at Current Position
  OCODE_GCSFLT   =$A4;            // Sharp fillet at curr pos
  OCODE_GSFLT    =$E4;            // Sharp fillet at given pos

  GCSFLT_SMF     =21;
  GSFLT_SMF      =20;

type
  ORDERS_GCSFLT=record    // osgcsflt
    apt: Array[0..2*GCSFLT_SMF-1] of POINTS;
    afxSharpness: Array[0..GCSFLT_SMF-1] of FIXED;
  end;

const
  GCSFLT_LMF    = 12;
  GSFLT_LMF     = 12;

type
  ORDERL_GCSFLT=record    // olgcsflt
    apt: Array[0..2*GCSFLT_SMF-1] of POINTL;
    afxSharpness: Array[0..GCSFLT_SMF-1] of FIXED;
  end;

// Bitblt
const
  OCODE_GBBLT    =$D6;            // Bitblt

type
  ORDERS_GBBLT=record      // osgbblt
    fsFlags: Word;
    usMix: Word;
    hbmSrc: HBITMAP;
    lOptions: Longint;
    rcsTargetRect: RECT1S;
    rclSourceRect: RECTL;
  end;

  ORDERL_GBBLT=record      // olgbblt
    fsFlags: Word;
    usMix: Word;
    hbmSrc: HBITMAP;
    lOptions: Longint;
    rclTargetRect: RECTL;
    rclSourceRect: RECTL;
  end;

// Char & break extra
const
  OCODE_GSCE     =$17;            // Set char extra
  OCODE_GPSCE    =$57;            // Push and set char extra
  OCODE_GSCBE    =$05;            // Set char break extra
  OCODE_GPSCBE   =$45;            // Push and set char break extra

type
  ORDER_GSCBE=record       // osgsce
    fbFlags: Byte;
    uchReserved: Byte;
    ufxextra: FIXED;
  end;
  ORDER_GSCE=ORDER_GSCBE;
  ORDER_GPSCE=ORDER_GSCBE;
  ORDER_GPSCBE=ORDER_GSCBE;

// Escape
const
  OCODE_GESCP    =$D5;            // Escape


//* type describes type of escape order, identifier gives the escape
//* order if the type is registered
const
  GESCP_ML      = 253;

type
  ORDER_GESCP=record      // ogescp
    uchType: Byte;
    uchIdent: Byte;
    auchData: Array[0..GESCP_ML-1] of Byte;           // Escape data
  end;

const
  GESCP_REG      =$80;            // identifier is registered

// Escape (Bitblt)
const
  GEBB_REGID     =$02;            // uchIdent - Bitblt

  ETYPE_GEBB     =$800200D5;

  GEBB_LMP       =29;

type
  ORDERL_GEBB=record      // olgebb
    fbFlags: Byte;
    usMix: Word;
    cPoints: Byte;
    hbmSrc: HBITMAP;
    lReserved: Longint;
    lOptions: Longint;
    aptPoints: Array[0..GEBB_LMP-1] of POINTL;
  end;

// Escape (Set Pel)
const
  GEPEL_REGID    =$01;            // uchIdent - Set Pel

  ETYPE_GEPEL         =$800100D5;

// Escape (DrawBits)
  GEDB_REGID     =$04;          // uchIdent - DrawBits

  ETYPE_GEDB          =$800400D5;

type
  ORDERL_GEDB=record      // olgedb
    fsFlags: Word;
    usMix: Word;
    pBits: Pointer;
    pbmi: PBITMAPINFO2;
    lOptions: Longint;
    rclTargetRect: RECTL;
    rclSourceRect: RECTL;
  end;

// Escape (FloodFill)
const
  GEFF_REGID     =$03;          // uchIdent - FloodFill

  ETYPE_GEFF          =$800300D5;

type
  ORDERL_GEFF=record      // olgeff
    fsFlags: Byte;
    auchColor: Array[0..3-1] of Byte;
  end;

// Element Types for attribute bundles
const
  ETYPE_LINEBUNDLE    =$0000FD01;
  ETYPE_CHARBUNDLE    =$0000FD02;
  ETYPE_MARKERBUNDLE  =$0000FD03;
  ETYPE_AREABUNDLE    =$0000FD04;
  ETYPE_IMAGEBUNDLE   =$0000FD05;

//***************************************************************************\
//*
//* Very long orders
//*
//***************************************************************************/

// macro to tell whether this is a very long order
{$define VLONG_ORDER(oc):=((oc)=OCODE_VLONG)}

// Very long order structure
const
  VORDER_ML =65531;

type
  VORDER=record           // vord
    idCode: Byte;
    uchQualifier: Byte;
    uchLength: SWPUSHORT;
    uchData: Array[0..VORDER_ML-1] of Byte;
  end;

// Character String Extended
const
  OCODEQ_GCCHSTE  =$B0;           // Qualifier - current posn
  OCODEQ_GCHSTE   =$F0;           // Qualifier - given position
  OCODEQ_GTCHSPA  =$F4;           // Tabbed Char String At

  ETYPE_GCCHSTE       =$0000FEB0;
  ETYPE_GCHSTE        =$0000FEF0;

type
  ORDERS_GCCHSTE=record    // osgcchste
    fbFlags: Byte;
    uchReserved: Byte;
    ptRect: Array[0..2-1] of POINTS;
    cchString: SWPUSHORT;
    achString: Array[0..1-1] of Char;
    adx: Array[0..1-1] of Integer;
  end;

  ORDERL_GCCHSTE=record    // olgcchste
    fbFlags: Byte;
    uchReserved: Byte;
    ptRect: Array[0..2-1] of POINTL;
    cchString: SWPUSHORT;
    achString: Array[0..1-1] of Char;
    adx: Array[0..1-1] of Longint;
  end;

  ORDERL_GTCHSPA=record   // olgcchspa
    fbFlags: Byte;
    uchReserved: Byte;
    ptRect: Array[0..2-1] of POINTL;
    cchString: SWPUSHORT;
    achString: Array[0..1-1] of Char;
    adx: Array[0..2-1] of Longint;
    tabs: Array[0..1-1] of Longint;
  end;

const
  GCCHSTE_DRAWRECT      =$80;
  GCCHSTE_NORECT        =$00;
  GCCHSTE_CLIP          =$40;
  GCCHSTE_NOCLIP        =$00;
  GCCHSTE_DEEMPHASIZE   =$20;            // Reserved
  GCCHSTE_NODEEMPHASIZE =$00;
  GCCHSTE_LEAVEPOS      =$10;
  GCCHSTE_MOVEPOS       =$00;
  GCCHSTE_UNDERSCORE    =$08;
  GCCHSTE_NOUNDERSCORE  =$00;
  GCCHSTE_STRIKEOUT     =$04;
  GCCHSTE_NOSTRIKEOUT   =$00;
  GTCHSPA_STARTPOS      =$02;
  GTCHSPA_NOSTARTPOS    =$00;

// Extended Escape
  OCODEQ_GEESCP   =$D5;           // Qualifier - extended escape

  GEESCP_ML      =65533;

type
  ORDER_GEESCP=record     // ogeescp
    uchType: Byte;
    uchIdent: Byte;
    auchData: Array[0..GEESCP_ML-1] of Byte;
  end;

//#pragma pack()    /* reset to default packing */



function GpiCreatePS(hab,hdc : cardinal;var psizlSize : SIZEL;flOptions : cardinal) : cardinal;cdecl;
function GpiDestroyPS(hps : cardinal) : longbool;cdecl;
function GpiAssociate(hps,hdc : cardinal) : longbool;cdecl;
function GpiRestorePS(hps : cardinal;lPSid : longint) : longbool;cdecl;
function GpiSavePS(hps : cardinal) : longint;cdecl;
function GpiErase(hps : cardinal) : longbool;cdecl;
function GpiQueryDevice(hps : cardinal) : cardinal;cdecl;
function GpiResetPS(hps,flOptions : cardinal) : longbool;cdecl;
function GpiSetPS(hps : cardinal;var psizlsize : SIZEL;flOptions : cardinal) : longbool;cdecl;
function GpiQueryPS(hps : cardinal;var psizlSize : SIZEL) : cardinal;cdecl;
function GpiErrorSegmentData(hps : cardinal;var plSegment,plContext : longint) : longint; cdecl;
function GpiQueryDrawControl(hps : cardinal;lControl : longint) : longint;cdecl;
function GpiSetDrawControl(hps : cardinal;lControl,lValue : longint) : longbool;cdecl;
function GpiQueryDrawingMode(hps : cardinal) : longint;cdecl;
function GpiSetDrawingMode(hps : cardinal;lMode : longint) : longbool;cdecl;
function GpiQueryStopDraw(hps : cardinal) : longint;cdecl;
function GpiSetStopDraw(hps : cardinal;lValue : longint) : longbool;cdecl;
function GpiCorrelateChain(hps : cardinal;lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var pl2 : longint) : longint;cdecl;
function GpiQueryTag(hps : cardinal;var plTag : longint) : longbool;cdecl;
function GpiSetTag(hps : cardinal;lTag : longint) : longbool;cdecl;
function GpiQueryPickApertureSize(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl;
function GpiSetPickApertureSize(hps : cardinal;lOptions : longint;var psizlSize : SIZEL) : longbool; cdecl;
function GpiQueryPickAperturePosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiSetPickAperturePosition(hps : cardinal;var pptlPick : POINTL) : longbool; cdecl;
function GpiQueryBoundaryData(hps : cardinal;var prclBoundary : RECTL) : longbool; cdecl;
function GpiResetBoundaryData(hps : cardinal) : longbool; cdecl;
function GpiCorrelateFrom(hps : cardinal;lFirstSegment,lLastSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var plSegTag : longint) : longint; cdecl;
function GpiCorrelateSegment(hps : cardinal;lSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var alSegTag : longint) : longint; cdecl;
function GpiOpenSegment(hps : cardinal;lSegment : longint) : longbool; cdecl;
function GpiCloseSegment(hps : cardinal) : longbool; cdecl;
function GpiDeleteSegment(hps : cardinal;lSegid : longint) : longbool; cdecl;
function GpiQueryInitialSegmentAttrs(hps : cardinal;lAttribute : longint) : longint; cdecl;
function GpiSetInitialSegmentAttrs(hps : cardinal;lAttribute,lValue : longint) : longbool; cdecl;
function GpiQuerySegmentAttrs(hps : cardinal;lSegid,lAttribute : longint) : longint; cdecl;
function GpiSetSegmentAttrs(hps : cardinal;lSegid,lAttribute,lValue : longint) : longbool; cdecl;
function GpiQuerySegmentPriority(hps : cardinal;lRefSegid,lOrder : longint) : longint; cdecl;
function GpiSetSegmentPriority(hps : cardinal;lSegid,lRefSegid,lOrder : longint) : longbool; cdecl;
function GpiDeleteSegments(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl;
function GpiQuerySegmentNames(hps : cardinal;lFirstSegid,lLastSegid,lMax : longint;var alSegids : longint) : longint; cdecl;
function GpiGetData(hps : cardinal;lSegid : longint;var plOffset : longint;lFormat,lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiPutData(hps : cardinal;lFormat : longint;var plCount : longint;var pbData : BYTE) : longint; cdecl;
function GpiDrawChain(hps : cardinal) : longbool; cdecl;
function GpiDrawFrom(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl;
function GpiDrawSegment(hps : cardinal;lSegment : longint) : longbool; cdecl;
function GpiDrawDynamics(hps : cardinal) : longbool; cdecl;
function GpiRemoveDynamics(hps : cardinal;lFirstSegid,lLastSegid : longint) : longbool; cdecl;
function GpiBeginElement(hps : cardinal;lType : longint;pszDesc : pchar) : longbool; cdecl;
function GpiEndElement(hps : cardinal) : longbool; cdecl;
function GpiLabel(hps : cardinal;lLabel : longint) : longbool; cdecl;
function GpiElement(hps : cardinal;lType : longint;pszDesc : pchar;lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiQueryElement(hps : cardinal;lOff,lMaxLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiDeleteElement(hps : cardinal) : longbool; cdecl;
function GpiDeleteElementRange(hps : cardinal;lFirstElement,lLastElement : longint) : longbool; cdecl;
function GpiDeleteElementsBetweenLabels(hps : cardinal;lFirstLabel,lLastLabel : longint) : longbool; cdecl;
function GpiQueryEditMode(hps : cardinal) : longint; cdecl;
function GpiSetEditMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryElementPointer(hps : cardinal) : longint; cdecl;
function GpiSetElementPointer(hps : cardinal;lElement : longint) : longbool; cdecl;
function GpiOffsetElementPointer(hps : cardinal;loffset : longint) : longbool; cdecl;
function GpiQueryElementType(hps : cardinal;var plType : longint;lLength : longint;pszData : pchar) : longint; cdecl;
function GpiSetElementPointerAtLabel(hps : cardinal;lLabel : longint) : longbool; cdecl;
function GpiQuerySegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetSegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiConvert(hps : cardinal;lSrc,lTarg,lCount : longint;var aptlPoints : POINTL) : longbool; cdecl;
function GpiConvertWithMatrix(hps : cardinal;lCountp : longint;var aptlPoints : POINTL;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiQueryModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiCallSegmentMatrix(hps : cardinal;lSegment,lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longint; cdecl;
function GpiQueryDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiQueryPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl;
function GpiSetPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl;
function GpiQueryViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl;
function GpiSetViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl;
function GpiTranslate(hps : cardinal;var pmatrixlf : MATRIXLF;long : longint;var ppointl : POINTL) : longbool; cdecl;
function GpiScale(hps : cardinal;var p1 : MATRIXLF;p2 : longint;var p3 : longint;var p4 : POINTL) : longbool; cdecl;
function GpiRotate(p1 : cardinal;var p2 : MATRIXLF;p3,p4 : longint;var p5 : POINTL) : longbool; cdecl;
function GpiSetGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl;
function GpiQueryGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl;
function GpiSetViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiQueryViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiBeginPath(hps : cardinal;lPath : longint) : longbool; cdecl;
function GpiEndPath(hps : cardinal) : longbool; cdecl;
function GpiCloseFigure(hps : cardinal) : longbool; cdecl;
function GpiModifyPath(hps : cardinal;lPath,lMode : longint) : longbool; cdecl;
function GpiFillPath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl;
function GpiSetClipPath(hps : cardinal;lPath,lOptions : longint) : longbool; cdecl;
function GpiOutlinePath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl;
function GpiPathToRegion(GpiH : cardinal;lPath,lOptions : longint) : cardinal; cdecl;
function GpiStrokePath(hps : cardinal;lPath : longint;flOptions : cardinal) : longint; cdecl;
function GpiCreateLogColorTable(hps,flOptions : cardinal;lFormat,lStart,lCount : longint;var alTable : longint) : longbool; cdecl;
function GpiQueryColorData(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl;
function GpiQueryLogColorTable(hps,flOptions : cardinal;lStart,lCount : longint;var alArray : longint) : longint; cdecl;
function GpiQueryRealColors(hps,flOptions : cardinal;lStart,lCount : longint;var alColors : longint) : longint; cdecl;
function GpiQueryNearestColor(hps,flOptions : cardinal;lRgbIn : longint) : longint; cdecl;
function GpiQueryColorIndex(hps,flOptions : cardinal;lRgbColor : longint) : longint; cdecl;
function GpiQueryRGBColor(hps,flOptions : cardinal;lColorIndex : longint) : longint; cdecl;
function GpiCreatePalette(hab,flOptions,ulFormat,ulCount : cardinal;var aulTable) : cardinal; cdecl;
function GpiDeletePalette(hpal : cardinal) : longbool; cdecl;
function GpiSelectPalette(hps,hpal : cardinal) : cardinal; cdecl;
function GpiAnimatePalette(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longint; cdecl;
function GpiSetPaletteEntries(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longbool; cdecl;
function GpiQueryPalette(hps : cardinal) : cardinal; cdecl;
function GpiQueryPaletteInfo(hpal,hps,flOptions,ulStart,ulCount : cardinal;var aulArray) : longint; cdecl;
function GpiSetColor(hps : cardinal;lColor : longint) : longbool; cdecl;
function GpiQueryColor(hps : cardinal) : longint; cdecl;
function GpiBox(hps : cardinal;lControl : longint;var pptlPoint : POINTL;lHRound,lVRound : longint) : longint; cdecl;
function GpiMove(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiLine(hps : cardinal;var pptlEndPoint : POINTL) : longint; cdecl;
function GpiPolyLine(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolyLineDisjoint(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiSetPattern(hps : cardinal;lPatternSymbol : longint) : longbool; cdecl;
function GpiQueryPattern(hps : cardinal) : longint;  cdecl;
function GpiBeginArea(hps,flOptions : cardinal) : longbool; cdecl;
function GpiEndArea(hps : cardinal) : longint; cdecl;
function GpiCharString(hps : cardinal;lCount : longint;pchString : pchar) : longint; cdecl;
function GpiCharStringAt(hps : cardinal;var pptlPoint : POINTL;lCount : longint;pchString : pchar) : longint; cdecl;
function GpiSetAttrMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryAttrMode(hps : cardinal) : longint; cdecl;
function GpiSetAttrs(hps : cardinal;lPrimType : longint;flAttrMask,flDefMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl;
function GpiQueryAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longint; cdecl;
function GpiSetBackColor(hps : cardinal;lColor : longint) : longbool; cdecl;
function GpiQueryBackColor(hps : cardinal) : longint; cdecl;
function GpiSetMix(hps : cardinal;lMixMode : longint) : longbool; cdecl;
function GpiQueryMix(hps : cardinal) : longint; cdecl;
function GpiSetBackMix(hps : cardinal;lMixMode : longint) : longbool; cdecl;
function GpiQueryBackMix(hps : cardinal) : longint; cdecl;
function GpiSetLineType(hps : cardinal;lLineType : longint) : longbool; cdecl;
function GpiQueryLineType(hps : cardinal) : longint; cdecl;
function GpiSetLineWidth(hps : cardinal;fxLineWidth : longint) : longbool; cdecl;
function GpiQueryLineWidth(hps : cardinal) : longint; cdecl;
function GpiSetLineWidthGeom(hps : cardinal;lLineWidth : longint) : longbool; cdecl;
function GpiQueryLineWidthGeom(hps : cardinal) : longint; cdecl;
function GpiSetLineEnd(hps : cardinal;lLineEnd : longint) : longbool; cdecl;
function GpiQueryLineEnd(hps : cardinal) : longint; cdecl;
function GpiSetLineJoin(hps : cardinal;lLineJoin : longint) : longbool; cdecl;
function GpiQueryLineJoin(hps : cardinal) : longint; cdecl;
function GpiSetCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiQueryCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl;
function GpiSetArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiQueryArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiPointArc(hps : cardinal;var pptl2 : POINTL) : longint; cdecl;
function GpiFullArc(hps : cardinal;lControl,fxMultiplier : longint) : longint; cdecl;
function GpiPartialArc(hps : cardinal;var pptlCenter : POINTL;fxMultiplier,fxStartAngle,fxSweepAngle : longint) : longint; cdecl;
function GpiPolyFillet(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolySpline(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiPolyFilletSharp(hps : cardinal;lCount : longint;var aptlPoints : POINTL;var afxPoints : longint) : longint; cdecl;
function GpiSetPatternSet(hps : cardinal;lSet : longint) : longbool; cdecl;
function GpiQueryPatternSet(hps : cardinal) : longint; cdecl;
function GpiSetPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl;
function GpiQueryPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl;
function GpiQueryCharStringPos(hps,flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl;
function GpiQueryCharStringPosAt(hps : cardinal;var pptlStart : POINTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl;
function GpiQueryTextBox(hps : cardinal;lCount1 : longint;pchString : pchar;lCount2 : longint;var aptlPoints : POINTL) : longbool; cdecl;
function GpiQueryDefCharBox(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl;
function GpiSetCharSet(hps : cardinal;llcid : longint) : longbool; cdecl;
function GpiQueryCharSet(hps : cardinal) : longint; cdecl;
function GpiSetCharBox(hps : cardinal;var psizfxBox : SIZEF) : longbool; cdecl;
function GpiQueryCharBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiSetCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl;
function GpiQueryCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl;
function GpiSetCharShear(hps : cardinal;var pptlAngle : POINTL) : longbool; cdecl;
function GpiQueryCharShear(hps : cardinal;var pptlShear : POINTL) : longbool; cdecl;
function GpiSetCharDirection(hps : cardinal;lDirection : longint) : longbool; cdecl;
function GpiQueryCharDirection(hps : cardinal) : longint; cdecl;
function GpiSetCharMode(hps : cardinal;lMode : longint) : longbool; cdecl;
function GpiQueryCharMode(hps : cardinal) : longint; cdecl;
function GpiSetTextAlignment(hps : cardinal;lHoriz,lVert : longint) : longbool; cdecl;
function GpiQueryTextAlignment(hps : cardinal;var plHoriz,plVert : longint) : longbool; cdecl;
function GpiCharStringPos(hps : cardinal;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl;
function GpiCharStringPosAt(hps : cardinal;var pptlStart : POINTL;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl;
function GpiSetCharExtra(hps : cardinal;Extra : longint) : longbool;  cdecl;
function GpiSetCharBreakExtra(hps : cardinal;BreakExtra : longint) : longbool; cdecl;
function GpiQueryCharExtra(hps : cardinal;var Extra : longint) : longbool; cdecl;
function GpiQueryCharBreakExtra(hps : cardinal;var BreakExtra : longint) : longbool; cdecl;
function GpiMarker(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiPolyMarker(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl;
function GpiSetMarker(hps : cardinal;lSymbol : longint) : longbool; cdecl;
function GpiSetMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiSetMarkerSet(hps : cardinal;lSet : longint) : longbool; cdecl;
function GpiQueryMarker(hps : cardinal) : longint; cdecl;
function GpiQueryMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl;
function GpiQueryMarkerSet(hps : cardinal) : longint; cdecl;
function GpiImage(hps : cardinal;lFormat : longint;var psizlImageSize : SIZEL;lLength : longint;var pbData : BYTE) : longint; cdecl;
function GpiPop(hps : cardinal;lCount : longint) : longbool; cdecl;
function GpiPtVisible(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiRectVisible(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiComment(hps : cardinal;lLength : longint;var pbData : BYTE) : longbool; cdecl;
function GpiCreateLogFont(hps : cardinal;var pName : STR8;lLcid : longint;var pfatAttrs : FATTRS) : longint; cdecl;
function GpiDeleteSetId(hps : cardinal;lLcid : longint) : longbool; cdecl;
function GpiLoadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiUnloadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiQueryFonts(hps,flOptions : cardinal;pszFacename : pchar;var plReqFonts : longint;lMetricsLength : longint;var afmMetrics : FONTMETRICS) : longint; cdecl;
function GpiQueryFontMetrics(hps : cardinal;lMetricsLength : longint;var pfmMetrics : FONTMETRICS) : longbool; cdecl;
function GpiQueryKerningPairs(hps : cardinal;lCount : longint;var akrnprData : KERNINGPAIRS) : longint; cdecl;
function GpiQueryWidthTable(hps : cardinal;lFirstChar,lCount : longint;var alData : longint) : longbool; cdecl;
function GpiQueryNumberSetIds(hps : cardinal) : longint; cdecl;
function GpiQuerySetIds(hps : cardinal;lCount : longint;var alTypes : longint;var aNames : STR8;var allcids : longint) : longbool; cdecl;
function GpiQueryFaceString(PS : cardinal;FamilyName : pchar;var attrs : FACENAMEDESC;length : longint;CompoundFaceName : pchar) : cardinal; cdecl;
function GpiQueryLogicalFont(PS : cardinal;lcid : longint;var name : STR8;var attrs : FATTRS;length : longint) : longbool; cdecl;
function GpiQueryFontAction(anchor,options : cardinal) : cardinal; cdecl;
function GpiLoadPublicFonts(p1 : cardinal;p2 : pchar):longbool; cdecl;
function GpiUnloadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl;
function GpiSetCp(hps,ulCodePage : cardinal) : longbool;  cdecl;
function GpiQueryCp(hps : cardinal) : cardinal; cdecl;
function GpiQueryFontFileDescriptions(hab : cardinal;pszFilename : pchar;var plCount : longint;var affdescsNames : FFDESCS) : longint;  cdecl;
function GpiQueryFullFontFileDescs(hab : cardinal;pszFilename : pchar;var plCount : longint;pNames : pointer;var plNamesBuffLength : longint) : longint; cdecl;
function GpiBitBlt(hpsTarget,hpsSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiDeleteBitmap(hbm : cardinal) : longbool; cdecl;
function GpiLoadBitmap(hps,Resource,idBitmap : cardinal;lWidth,lHeight : longint) : cardinal; cdecl;
function GpiSetBitmap(hps,hbm : cardinal) : cardinal; cdecl;
function GpiWCBitBlt(hpsTarget,hbmSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiCreateBitmap(hps : cardinal;var pbmpNew : Tbitmapinfoheader2;flOptions : cardinal;var pbInitData : BYTE;var pbmiInfoTable : Tbitmapinfo2) : cardinal; cdecl;
function GpiSetBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable : Tbitmapinfo2) : longint; cdecl;
function GpiSetBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl;
function GpiSetBitmapId(hps,hbm : cardinal;lLcid : longint) : longbool; cdecl;
function GpiQueryBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl;
function GpiQueryBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl;
function GpiQueryBitmapHandle(hps : cardinal;lLcid : longint) : cardinal;  cdecl;
function GpiQueryBitmapParameters(hbm : cardinal;var pbmpData : Tbitmapinfoheader) : longbool; cdecl;
function GpiQueryBitmapInfoHeader(hbm : cardinal;var pbmpData : Tbitmapinfoheader2) : longbool; cdecl;
function GpiQueryDeviceBitmapFormats(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl;
function GpiSetPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiQueryPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiFloodFill(hps : cardinal;lOptions,lColor : longint) : longint; cdecl;
function GpiDrawBits(hps : cardinal;pBits : pointer;var pbmiInfoTable :Tbitmapinfo2;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl;
function GpiCombineRegion(hps,hrgnDest,hrgnSrc1,hrgnSrc2 : cardinal;lMode : longint) : longint;  cdecl;
function GpiCreateRegion(hps : cardinal;lCount : longint;var arclRectangles : RECTL) : cardinal;  cdecl;
function GpiDestroyRegion(hps,hrgn : cardinal) : longbool;  cdecl;
function GpiEqualRegion(hps,hrgnSrc1,hrgnSrc2 : cardinal) : longint; cdecl;
function GpiOffsetRegion(hps,Hrgn : cardinal;var pptlOffset : POINTL) : longbool; cdecl;
function GpiPaintRegion(hps,hrgn : cardinal) : longint; cdecl;
function GpiFrameRegion(hps,hrgn : cardinal;var thickness : SIZEL) : longint; cdecl;
function GpiPtInRegion(hps,hrgn : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiQueryRegionBox(hps,hrgn : cardinal;var prclBound : RECTL) : longint; cdecl;
function GpiQueryRegionRects(hps,hrgn : cardinal;var prclBound : RECTL;var prgnrcControl : RGNRECT;var prclRect : RECTL) : longbool; cdecl;
function GpiRectInRegion(hps,hrgn : cardinal;var prclRect : RECTL) : longint; cdecl;
function GpiSetRegion(hps,hrgn : cardinal;lcount : longint;var arclRectangles : RECTL) : longbool;cdecl;
function GpiSetClipRegion(hps,hrgn : cardinal;var phrgnOld : cardinal) : longint; cdecl;
function GpiQueryClipRegion(hps : cardinal) : cardinal;  cdecl;
function GpiQueryClipBox(hps : cardinal;var prclBound : RECTL) : longint; cdecl;
function GpiExcludeClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiIntersectClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl;
function GpiOffsetClipRegion(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl;
function GpiCopyMetaFile(hmf : cardinal) : cardinal; cdecl;
function GpiDeleteMetaFile(hmf : cardinal) : longbool; cdecl;
function GpiLoadMetaFile(hab : cardinal;pszFilename : pchar) : cardinal; cdecl;
function GpiPlayMetaFile(hps,hmf : cardinal;lCount1 : longint;var alOptarray,plSegCount : longint;lCount2 : longint;pszDesc : pchar) : longint;  cdecl;
function GpiQueryMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbData : BYTE) : longbool;  cdecl;
function GpiQueryMetaFileLength(hmf : cardinal) : longint;  cdecl;
function GpiSaveMetaFile(hmf : cardinal;pszFilename : pchar) : longbool; cdecl;
function GpiSetMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbBuffer : BYTE) : longbool; cdecl;
function GpiQueryDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiQueryDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl;
function GpiQueryDefTag(hps : cardinal;var plTag : longint) : longbool; cdecl;
function GpiQueryDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiSetDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl;
function GpiSetDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool;cdecl;
function GpiSetDefTag(hps : cardinal;lTag : longint) : longbool; cdecl;
function GpiSetDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl;
function GpiPolygons(hps,ulCount : cardinal;var paplgn : POLYGON;flOptions,flModel : cardinal) : longint; cdecl;

implementation

function GpiCreatePS(hab,hdc : cardinal;var psizlSize : SIZEL;flOptions : cardinal) : cardinal;cdecl;external 'pmgpi' index 369;
function GpiDestroyPS(hps : cardinal) : longbool;cdecl;external 'pmgpi' index 379;
function GpiAssociate(hps,hdc : cardinal) : longbool;cdecl;external 'pmgpi' index 351;
function GpiRestorePS(hps : cardinal;lPSid : longint) : longbool;cdecl;external 'pmgpi' index 499;
function GpiSavePS(hps : cardinal) : longint;cdecl;external 'pmgpi' index 501;
function GpiErase(hps : cardinal) : longbool;cdecl;external 'pmgpi' index 389;
function GpiQueryDevice(hps : cardinal) : cardinal;cdecl;external 'pmgpi' index 444;
function GpiResetPS(hps,flOptions : cardinal) : longbool;cdecl;external 'pmgpi' index 498;
function GpiSetPS(hps : cardinal;var psizlsize : SIZEL;flOptions : cardinal) : longbool;cdecl;external 'pmgpi' index 539;
function GpiQueryPS(hps : cardinal;var psizlSize : SIZEL) : cardinal;cdecl;external 'pmgpi' index 471;
function GpiErrorSegmentData(hps : cardinal;var plSegment,plContext : longint) : longint;cdecl;external 'pmgpi' index 390;
function GpiQueryDrawControl(hps : cardinal;lControl : longint) : longint;cdecl;external 'pmgpi' index 446;
function GpiSetDrawControl(hps : cardinal;lControl,lValue : longint) : longbool;cdecl;external 'pmgpi' index 521;
function GpiQueryDrawingMode(hps : cardinal) : longint;cdecl;external 'pmgpi' index 447;
function GpiSetDrawingMode(hps : cardinal;lMode : longint) : longbool;cdecl;external 'pmgpi' index 522;
function GpiQueryStopDraw(hps : cardinal) : longint;cdecl; external 'pmgpi' index 487;
function GpiSetStopDraw(hps : cardinal;lValue : longint) : longbool; cdecl; external 'pmgpi' index 550;
function GpiCorrelateChain(hps : cardinal;lType : longint;var pptlPick : POINTL;lMaxHits : longint;lMaxDepth : longint;var pl2 : longint) : longint; cdecl; external 'pmgpi' index 366;
function GpiQueryTag(hps : cardinal;var plTag : longint) : longbool; cdecl; external 'pmgpi' index 488;
function GpiSetTag(hps : cardinal;lTag : longint) : longbool; cdecl; external 'pmgpi' index 551;
function GpiQueryPickApertureSize(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 478;
function GpiSetPickApertureSize(hps : cardinal;lOptions : longint;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 589;
function GpiQueryPickAperturePosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 477;
function GpiSetPickAperturePosition(hps : cardinal;var pptlPick : POINTL) : longbool; cdecl; external 'pmgpi' index 545;
function GpiQueryBoundaryData(hps : cardinal;var prclBoundary : RECTL) : longbool; cdecl; external 'pmgpi' index 428;
function GpiResetBoundaryData(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 497;
function GpiCorrelateFrom(hps : cardinal;lFirstSegment,lLastSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var plSegTag : longint) : longint; cdecl; external 'pmgpi' index 367;
function GpiCorrelateSegment(hps : cardinal;lSegment,lType : longint;var pptlPick : POINTL;lMaxHits,lMaxDepth : longint;var alSegTag : longint) : longint; cdecl; external 'pmgpi' index 582;
function GpiOpenSegment(hps : cardinal;lSegment : longint) : longbool; cdecl; external 'pmgpi' index 408;
function GpiCloseSegment(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 361;
function GpiDeleteSegment(hps : cardinal;lSegid : longint) : longbool; cdecl; external 'pmgpi' index 376;
function GpiQueryInitialSegmentAttrs(hps : cardinal;lAttribute : longint) : longint; cdecl; external 'pmgpi' index 455;
function GpiSetInitialSegmentAttrs(hps : cardinal;lAttribute,lValue : longint) : longbool; cdecl; external 'pmgpi' index 527;
function GpiQuerySegmentAttrs(hps : cardinal;lSegid,lAttribute : longint) : longint; cdecl; external 'pmgpi' index 482;
function GpiSetSegmentAttrs(hps : cardinal;lSegid,lAttribute,lValue : longint) : longbool; cdecl; external 'pmgpi' index 547;
function GpiQuerySegmentPriority(hps : cardinal;lRefSegid,lOrder : longint) : longint; cdecl; external 'pmgpi' index 484;
function GpiSetSegmentPriority(hps : cardinal;lSegid,lRefSegid,lOrder : longint) : longbool; cdecl; external 'pmgpi' index 548;
function GpiDeleteSegments(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl; external 'pmgpi' index 377;
function GpiQuerySegmentNames(hps : cardinal;lFirstSegid,lLastSegid,lMax : longint;var alSegids : longint) : longint; cdecl; external 'pmgpi' index 483;
function GpiGetData(hps : cardinal;lSegid : longint;var plOffset : longint;lFormat,lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 394;
function GpiPutData(hps : cardinal;lFormat : longint;var plCount : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 421;
function GpiDrawChain(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 380;
function GpiDrawFrom(hps : cardinal;lFirstSegment,lLastSegment : longint) : longbool; cdecl; external 'pmgpi' index 382;
function GpiDrawSegment(hps : cardinal;lSegment : longint) : longbool; cdecl; external 'pmgpi' index 383;
function GpiDrawDynamics(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 381;
function GpiRemoveDynamics(hps : cardinal;lFirstSegid,lLastSegid : longint) : longbool; cdecl; external 'pmgpi' index 496;
function GpiBeginElement(hps : cardinal;lType : longint;pszDesc : pchar) : longbool; cdecl; external 'pmgpi' index 353;
function GpiEndElement(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 386;
function GpiLabel(hps : cardinal;lLabel : longint) : longbool; cdecl; external 'pmgpi' index 397;
function GpiElement(hps : cardinal;lType : longint;pszDesc : pchar;lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 384;
function GpiQueryElement(hps : cardinal;lOff,lMaxLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 449;
function GpiDeleteElement(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 372;
function GpiDeleteElementRange(hps : cardinal;lFirstElement,lLastElement : longint) : longbool; cdecl; external 'pmgpi' index 373;
function GpiDeleteElementsBetweenLabels(hps : cardinal;lFirstLabel,lLastLabel : longint) : longbool; cdecl; external 'pmgpi' index 374;
function GpiQueryEditMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 448;
function GpiSetEditMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 523;
function GpiQueryElementPointer(hps : cardinal) : longint; cdecl; external 'pmgpi' index 450;
function GpiSetElementPointer(hps : cardinal;lElement : longint) : longbool; cdecl; external 'pmgpi' index 524;
function GpiOffsetElementPointer(hps : cardinal;loffset : longint) : longbool; cdecl; external 'pmgpi' index 406;
function GpiQueryElementType(hps : cardinal;var plType : longint;lLength : longint;pszData : pchar) : longint; cdecl; external 'pmgpi' index 451;
function GpiSetElementPointerAtLabel(hps : cardinal;lLabel : longint) : longbool; cdecl; external 'pmgpi' index 525;
function GpiQuerySegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 485;
function GpiSetSegmentTransformMatrix(hps : cardinal;lSegid,lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 549;
function GpiConvert(hps : cardinal;lSrc,lTarg,lCount : longint;var aptlPoints : POINTL) : longbool; cdecl; external 'pmgpi' index 364;
function GpiConvertWithMatrix(hps : cardinal;lCountp : longint;var aptlPoints : POINTL;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 618;
function GpiQueryModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 468;
function GpiSetModelTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 538;
function GpiCallSegmentMatrix(hps : cardinal;lSegment,lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longint; cdecl; external 'pmgpi' index 357;
function GpiQueryDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 443;
function GpiSetDefaultViewMatrix(hps : cardinal;lCount : longint;var pmatlfarray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 520;
function GpiQueryPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl; external 'pmgpi' index 472;
function GpiSetPageViewport(hps : cardinal;var prclViewport : RECTL) : longbool; cdecl; external 'pmgpi' index 540;
function GpiQueryViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF) : longbool; cdecl; external 'pmgpi' index 491;
function GpiSetViewingTransformMatrix(hps : cardinal;lCount : longint;var pmatlfArray : MATRIXLF;lOptions : longint) : longbool; cdecl; external 'pmgpi' index 553;
function GpiTranslate(hps : cardinal;var pmatrixlf : MATRIXLF;long : longint;var ppointl : POINTL) : longbool; cdecl; external 'pmgpi' index 564;
function GpiScale(hps : cardinal;var p1 : MATRIXLF;p2 : longint;var p3 : longint;var p4 : POINTL) : longbool; cdecl; external 'pmgpi' index 565;
function GpiRotate(p1 : cardinal;var p2 : MATRIXLF;p3,p4 : longint;var p5 : POINTL) : longbool; cdecl; external 'pmgpi' index 566;
function GpiSetGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl; external 'pmgpi' index 526;
function GpiQueryGraphicsField(hps : cardinal;var prclField : RECTL) : longbool; cdecl; external 'pmgpi' index 454;
function GpiSetViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 552;
function GpiQueryViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 490;
function GpiBeginPath(hps : cardinal;lPath : longint) : longbool; cdecl; external 'pmgpi' index 354;
function GpiEndPath(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 387;
function GpiCloseFigure(hps : cardinal) : longbool; cdecl; external 'pmgpi' index 360;
function GpiModifyPath(hps : cardinal;lPath,lMode : longint) : longbool; cdecl; external 'pmgpi' index 403;
function GpiFillPath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl; external 'pmgpi' index 392;
function GpiSetClipPath(hps : cardinal;lPath,lOptions : longint) : longbool; cdecl; external 'pmgpi' index 515;
function GpiOutlinePath(hps : cardinal;lPath,lOptions : longint) : longint; cdecl; external 'pmgpi' index 563;
function GpiPathToRegion(GpiH : cardinal;lPath,lOptions : longint) : cardinal; cdecl; external 'pmgpi' index 559;
function GpiStrokePath(hps : cardinal;lPath : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 554;
function GpiCreateLogColorTable(hps,flOptions : cardinal;lFormat,lStart,lCount : longint;var alTable : longint) : longbool; cdecl; external 'pmgpi' index 592;
function GpiQueryColorData(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl; external 'pmgpi' index 438;
function GpiQueryLogColorTable(hps,flOptions : cardinal;lStart,lCount : longint;var alArray : longint) : longint; cdecl; external 'pmgpi' index 593;
function GpiQueryRealColors(hps,flOptions : cardinal;lStart,lCount : longint;var alColors : longint) : longint; cdecl; external 'pmgpi' index 480;
function GpiQueryNearestColor(hps,flOptions : cardinal;lRgbIn : longint) : longint; cdecl; external 'pmgpi' index 469;
function GpiQueryColorIndex(hps,flOptions : cardinal;lRgbColor : longint) : longint; cdecl; external 'pmgpi' index 439;
function GpiQueryRGBColor(hps,flOptions : cardinal;lColorIndex : longint) : longint; cdecl; external 'pmgpi' index 479;
function GpiCreatePalette(hab,flOptions,ulFormat,ulCount : cardinal;var aulTable) : cardinal; cdecl; external 'pmgpi' index 594;
function GpiDeletePalette(hpal : cardinal) : longbool; cdecl; external 'pmgpi' index 577;
function GpiSelectPalette(hps,hpal : cardinal) : cardinal; cdecl; external 'pmgpi' index 578;
function GpiAnimatePalette(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longint; cdecl; external 'pmgpi' index 595;
function GpiSetPaletteEntries(hpal,ulFormat,ulStart,ulCount : cardinal;var aulTable) : longbool; cdecl; external 'pmgpi' index 596;
function GpiQueryPalette(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 579;
function GpiQueryPaletteInfo(hpal,hps,flOptions,ulStart,ulCount : cardinal;var aulArray) : longint; cdecl; external 'pmgpi' index 597;
function GpiSetColor(hps : cardinal;lColor : longint) : longbool; cdecl; external 'pmgpi' index 517;
function GpiQueryColor(hps : cardinal) : longint; cdecl; external 'pmgpi' index 437;
function GpiBox(hps : cardinal;lControl : longint;var pptlPoint : POINTL;lHRound,lVRound : longint) : longint; cdecl; external 'pmgpi' index 356;
function GpiMove(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 404;
function GpiLine(hps : cardinal;var pptlEndPoint : POINTL) : longint; cdecl; external 'pmgpi' index 398;
function GpiPolyLine(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 415;
function GpiPolyLineDisjoint(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 558;
function GpiSetPattern(hps : cardinal;lPatternSymbol : longint) : longbool; cdecl; external 'pmgpi' index 541;
function GpiQueryPattern(hps : cardinal) : longint; cdecl; external 'pmgpi' index 473;
function GpiBeginArea(hps,flOptions : cardinal) : longbool; cdecl; external 'pmgpi' index 352;
function GpiEndArea(hps : cardinal) : longint; cdecl; external 'pmgpi' index 385;
function GpiCharString(hps : cardinal;lCount : longint;pchString : pchar) : longint; cdecl; external 'pmgpi' index 358;
function GpiCharStringAt(hps : cardinal;var pptlPoint : POINTL;lCount : longint;pchString : pchar) : longint; cdecl; external 'pmgpi' index 359;
function GpiSetAttrMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 503;
function GpiQueryAttrMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 423;
function GpiSetAttrs(hps : cardinal;lPrimType : longint;flAttrMask,flDefMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 588;
function GpiQueryAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longint; cdecl; external 'pmgpi' index 583;
function GpiSetBackColor(hps : cardinal;lColor : longint) : longbool; cdecl; external 'pmgpi' index 504;
function GpiQueryBackColor(hps : cardinal) : longint; cdecl; external 'pmgpi' index 424;
function GpiSetMix(hps : cardinal;lMixMode : longint) : longbool; cdecl; external 'pmgpi' index 537;
function GpiQueryMix(hps : cardinal) : longint; cdecl; external 'pmgpi' index 467;
function GpiSetBackMix(hps : cardinal;lMixMode : longint) : longbool; cdecl; external 'pmgpi' index 505;
function GpiQueryBackMix(hps : cardinal) : longint; cdecl; external 'pmgpi' index 425;
function GpiSetLineType(hps : cardinal;lLineType : longint) : longbool; cdecl; external 'pmgpi' index 530;
function GpiQueryLineType(hps : cardinal) : longint; cdecl; external 'pmgpi' index 459;
function GpiSetLineWidth(hps : cardinal;fxLineWidth : longint) : longbool; cdecl; external 'pmgpi' index 531;
function GpiQueryLineWidth(hps : cardinal) : longint; cdecl; external 'pmgpi' index 460;
function GpiSetLineWidthGeom(hps : cardinal;lLineWidth : longint) : longbool; cdecl; external 'pmgpi' index 532;
function GpiQueryLineWidthGeom(hps : cardinal) : longint; cdecl; external 'pmgpi' index 461;
function GpiSetLineEnd(hps : cardinal;lLineEnd : longint) : longbool; cdecl; external 'pmgpi' index 528;
function GpiQueryLineEnd(hps : cardinal) : longint; cdecl; external 'pmgpi' index 457;
function GpiSetLineJoin(hps : cardinal;lLineJoin : longint) : longbool; cdecl; external 'pmgpi' index 529;
function GpiQueryLineJoin(hps : cardinal) : longint; cdecl; external 'pmgpi' index 458;
function GpiSetCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 519;
function GpiQueryCurrentPosition(hps : cardinal;var pptlPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 441;
function GpiSetArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 502;
function GpiQueryArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 422;
function GpiPointArc(hps : cardinal;var pptl2 : POINTL) : longint; cdecl; external 'pmgpi' index 412;
function GpiFullArc(hps : cardinal;lControl,fxMultiplier : longint) : longint; cdecl; external 'pmgpi' index 393;
function GpiPartialArc(hps : cardinal;var pptlCenter : POINTL;fxMultiplier,fxStartAngle,fxSweepAngle : longint) : longint; cdecl; external 'pmgpi' index 612;
function GpiPolyFillet(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 413;
function GpiPolySpline(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 417;
function GpiPolyFilletSharp(hps : cardinal;lCount : longint;var aptlPoints : POINTL;var afxPoints : longint) : longint; cdecl; external 'pmgpi' index 414;
function GpiSetPatternSet(hps : cardinal;lSet : longint) : longbool; cdecl; external 'pmgpi' index 543;
function GpiQueryPatternSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 475;
function GpiSetPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 542;
function GpiQueryPatternRefPoint(hps : cardinal;var pptlRefPoint : POINTL) : longbool; cdecl; external 'pmgpi' index 474;
function GpiQueryCharStringPos(hps,flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl; external 'pmgpi' index 584;
function GpiQueryCharStringPosAt(hps : cardinal;var pptlStart : POINTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alXincrements : longint;var aptlPositions : POINTL) : longbool; cdecl; external 'pmgpi' index 585;
function GpiQueryTextBox(hps : cardinal;lCount1 : longint;pchString : pchar;lCount2 : longint;var aptlPoints : POINTL) : longbool; cdecl; external 'pmgpi' index 489;
function GpiQueryDefCharBox(hps : cardinal;var psizlSize : SIZEL) : longbool; cdecl; external 'pmgpi' index 442;
function GpiSetCharSet(hps : cardinal;llcid : longint) : longbool; cdecl; external 'pmgpi' index 513;
function GpiQueryCharSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 433;
function GpiSetCharBox(hps : cardinal;var psizfxBox : SIZEF) : longbool; cdecl; external 'pmgpi' index 510;
function GpiQueryCharBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 430;
function GpiSetCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl; external 'pmgpi' index 509;
function GpiQueryCharAngle(hps : cardinal;var pgradlAngle : GRADIENTL) : longbool; cdecl; external 'pmgpi' index 429;
function GpiSetCharShear(hps : cardinal;var pptlAngle : POINTL) : longbool; cdecl; external 'pmgpi' index 514;
function GpiQueryCharShear(hps : cardinal;var pptlShear : POINTL) : longbool; cdecl; external 'pmgpi' index 434;
function GpiSetCharDirection(hps : cardinal;lDirection : longint) : longbool; cdecl; external 'pmgpi' index 511;
function GpiQueryCharDirection(hps : cardinal) : longint; cdecl; external 'pmgpi' index 431;
function GpiSetCharMode(hps : cardinal;lMode : longint) : longbool; cdecl; external 'pmgpi' index 512;
function GpiQueryCharMode(hps : cardinal) : longint; cdecl; external 'pmgpi' index 432;
function GpiSetTextAlignment(hps : cardinal;lHoriz,lVert : longint) : longbool; cdecl; external 'pmgpi' index 649;
function GpiQueryTextAlignment(hps : cardinal;var plHoriz,plVert : longint) : longbool; cdecl; external 'pmgpi' index 648;
function GpiCharStringPos(hps : cardinal;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl; external 'pmgpi' index 580;
function GpiCharStringPosAt(hps : cardinal;var pptlStart : POINTL;var prclRect : RECTL;flOptions : cardinal;lCount : longint;pchString : pchar;var alAdx : longint) : longint; cdecl; external 'pmgpi' index 581;
function GpiSetCharExtra(hps : cardinal;Extra : longint) : longbool; cdecl; external 'pmgpi' index 614;
function GpiSetCharBreakExtra(hps : cardinal;BreakExtra : longint) : longbool; cdecl; external 'pmgpi' index 616;
function GpiQueryCharExtra(hps : cardinal;var Extra : longint) : longbool; cdecl; external 'pmgpi' index 613;
function GpiQueryCharBreakExtra(hps : cardinal;var BreakExtra : longint) : longbool; cdecl; external 'pmgpi' index 615;
function GpiMarker(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 402;
function GpiPolyMarker(hps : cardinal;lCount : longint;var aptlPoints : POINTL) : longint; cdecl; external 'pmgpi' index 416;
function GpiSetMarker(hps : cardinal;lSymbol : longint) : longbool; cdecl; external 'pmgpi' index 533;
function GpiSetMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 534;
function GpiSetMarkerSet(hps : cardinal;lSet : longint) : longbool; cdecl; external 'pmgpi' index 535;
function GpiQueryMarker(hps : cardinal) : longint; cdecl; external 'pmgpi' index 462;
function GpiQueryMarkerBox(hps : cardinal;var psizfxSize : SIZEF) : longbool; cdecl; external 'pmgpi' index 463;
function GpiQueryMarkerSet(hps : cardinal) : longint; cdecl; external 'pmgpi' index 464;
function GpiImage(hps : cardinal;lFormat : longint;var psizlImageSize : SIZEL;lLength : longint;var pbData : BYTE) : longint; cdecl; external 'pmgpi' index 395;
function GpiPop(hps : cardinal;lCount : longint) : longbool; cdecl; external 'pmgpi' index 418;
function GpiPtVisible(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 420;
function GpiRectVisible(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 495;
function GpiComment(hps : cardinal;lLength : longint;var pbData : BYTE) : longbool; cdecl; external 'pmgpi' index 363;
function GpiCreateLogFont(hps : cardinal;var pName : STR8;lLcid : longint;var pfatAttrs : FATTRS) : longint; cdecl; external 'pmgpi' index 368;
function GpiDeleteSetId(hps : cardinal;lLcid : longint) : longbool; cdecl; external 'pmgpi' index 378;
function GpiLoadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 400;
function GpiUnloadFonts(hab : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 555;
function GpiQueryFonts(hps,flOptions : cardinal;pszFacename : pchar;var plReqFonts : longint;lMetricsLength : longint;var afmMetrics : FONTMETRICS) : longint; cdecl; external 'pmgpi' index 586;
function GpiQueryFontMetrics(hps : cardinal;lMetricsLength : longint;var pfmMetrics : FONTMETRICS) : longbool; cdecl; external 'pmgpi' index 453;
function GpiQueryKerningPairs(hps : cardinal;lCount : longint;var akrnprData : KERNINGPAIRS) : longint; cdecl; external 'pmgpi' index 456;
function GpiQueryWidthTable(hps : cardinal;lFirstChar,lCount : longint;var alData : longint) : longbool; cdecl; external 'pmgpi' index 492;
function GpiQueryNumberSetIds(hps : cardinal) : longint; cdecl; external 'pmgpi' index 470;
function GpiQuerySetIds(hps : cardinal;lCount : longint;var alTypes : longint;var aNames : STR8;var allcids : longint) : longbool; cdecl; external 'pmgpi' index 486;
function GpiQueryFaceString(PS : cardinal;FamilyName : pchar;var attrs : FACENAMEDESC;length : longint;CompoundFaceName : pchar) : cardinal; cdecl; external 'pmgpi' index 575;
function GpiQueryLogicalFont(PS : cardinal;lcid : longint;var name : STR8;var attrs : FATTRS;length : longint) : longbool; cdecl; external 'pmgpi' index 574;
function GpiQueryFontAction(anchor,options : cardinal) : cardinal; cdecl; external 'pmgpi' index 576;
function GpiLoadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl; external 'pmgpi' index 622;
function GpiUnloadPublicFonts(p1 : cardinal;p2 : pchar) : longbool; cdecl; external 'pmgpi' index 623;
function GpiSetCp(hps,ulCodePage : cardinal) : longbool; cdecl; external 'pmgpi' index 518;
function GpiQueryCp(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 440;
function GpiQueryFontFileDescriptions(hab : cardinal;pszFilename : pchar;var plCount : longint;var affdescsNames : FFDESCS) : longint; cdecl; external 'pmgpi' index 452;
function GpiQueryFullFontFileDescs(hab : cardinal;pszFilename : pchar;var plCount : longint;pNames : pointer;var plNamesBuffLength : longint) : longint; cdecl; external 'pmgpi' index 657;
function GpiBitBlt(hpsTarget,hpsSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 355;
function GpiDeleteBitmap(hbm : cardinal) : longbool; cdecl; external 'pmgpi' index 371;
function GpiLoadBitmap(hps,Resource,idBitmap:cardinal;lWidth,lHeight : longint) : cardinal; cdecl; external 'pmgpi' index 399;
function GpiSetBitmap(hps,hbm : cardinal) : cardinal; cdecl; external 'pmgpi' index 506;
function GpiWCBitBlt(hpsTarget,hbmSource : cardinal;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 557;
function GpiCreateBitmap(hps : cardinal;var pbmpNew :Tbitmapinfoheader2;flOptions : cardinal;var pbInitData : BYTE;var pbmiInfoTable :Tbitmapinfo2) : cardinal; cdecl; external 'pmgpi' index 598;
function GpiSetBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl; external 'pmgpi' index 602;
function GpiSetBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl; external 'pmgpi' index 507;
function GpiSetBitmapId(hps,hbm : cardinal;lLcid : longint) : longbool; cdecl; external 'pmgpi' index 508;
function GpiQueryBitmapBits(hps : cardinal;lScanStart,lScans : longint;var pbBuffer : BYTE;var pbmiInfoTable :Tbitmapinfo2) : longint; cdecl; external 'pmgpi' index 599;
function GpiQueryBitmapDimension(hbm : cardinal;var psizlBitmapDimension : SIZEL) : longbool; cdecl; external 'pmgpi' index 426;
function GpiQueryBitmapHandle(hps : cardinal;lLcid : longint) : cardinal; cdecl; external 'pmgpi' index 427;
function GpiQueryBitmapParameters(hbm : cardinal;var pbmpData :Tbitmapinfoheader) : longbool; cdecl; external 'pmgpi' index 573;
function GpiQueryBitmapInfoHeader(hbm : cardinal;var pbmpData :Tbitmapinfoheader2) : longbool; cdecl; external 'pmgpi' index 601;
function GpiQueryDeviceBitmapFormats(hps : cardinal;lCount : longint;var alArray : longint) : longbool; cdecl; external 'pmgpi' index 445;
function GpiSetPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 544;
function GpiQueryPel(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 476;
function GpiFloodFill(hps : cardinal;lOptions,lColor : longint) : longint; cdecl; external 'pmgpi' index 560;
function GpiDrawBits(hps : cardinal;pBits : pointer;var pbmiInfoTable :Tbitmapinfo2;lCount : longint;var aptlPoints : POINTL;lRop : longint;flOptions : cardinal) : longint; cdecl; external 'pmgpi' index 603;
function GpiCombineRegion(hps,hrgnDest,hrgnSrc1,hrgnSrc2 : cardinal;lMode : longint) : longint; cdecl; external 'pmgpi' index 362;
function GpiCreateRegion(hps : cardinal;lCount : longint;var arclRectangles : RECTL) : cardinal; cdecl; external 'pmgpi' index 370;
function GpiDestroyRegion(hps,hrgn : cardinal) : longbool; cdecl; external 'pmgpi' index 611;
function GpiEqualRegion(hps,hrgnSrc1,hrgnSrc2 : cardinal) : longint; cdecl; external 'pmgpi' index 388;
function GpiOffsetRegion(hps,Hrgn : cardinal;var pptlOffset : POINTL) : longbool; cdecl; external 'pmgpi' index 407;
function GpiPaintRegion(hps,hrgn : cardinal) : longint; cdecl; external 'pmgpi' index 409;
function GpiFrameRegion(hps,hrgn : cardinal;var thickness : SIZEL) : longint; cdecl; external 'pmgpi' index 617;
function GpiPtInRegion(hps,hrgn : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 419;
function GpiQueryRegionBox(hps,hrgn : cardinal;var prclBound : RECTL) : longint; cdecl; external 'pmgpi' index 481;
function GpiQueryRegionRects(hps,hrgn : cardinal;var prclBound : RECTL;var prgnrcControl : RGNRECT;var prclRect : RECTL) : longbool; cdecl; external 'pmgpi' index 587;
function GpiRectInRegion(hps,hrgn : cardinal;var prclRect : RECTL) : longint; cdecl; external 'pmgpi' index 494;
function GpiSetRegion(hps,hrgn : cardinal;lcount : longint;var arclRectangles : RECTL) : longbool; cdecl; external 'pmgpi' index 546;
function GpiSetClipRegion(hps,hrgn : cardinal;var phrgnOld : cardinal) : longint; cdecl; external 'pmgpi' index 516;
function GpiQueryClipRegion(hps : cardinal) : cardinal; cdecl; external 'pmgpi' index 436;
function GpiQueryClipBox(hps : cardinal;var prclBound : RECTL) : longint; cdecl; external 'pmgpi' index 435;
function GpiExcludeClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 391;
function GpiIntersectClipRectangle(hps : cardinal;var prclRectangle : RECTL) : longint; cdecl; external 'pmgpi' index 396;
function GpiOffsetClipRegion(hps : cardinal;var pptlPoint : POINTL) : longint; cdecl; external 'pmgpi' index 405;
function GpiCopyMetaFile(hmf : cardinal) : cardinal; cdecl; external 'pmgpi' index 365;
function GpiDeleteMetaFile(hmf : cardinal) : longbool; cdecl; external 'pmgpi' index 375;
function GpiLoadMetaFile(hab : cardinal;pszFilename : pchar) : cardinal; cdecl; external 'pmgpi' index 401;
function GpiPlayMetaFile(hps,hmf : cardinal;lCount1 : longint;var alOptarray,plSegCount : longint;lCount2 : longint;pszDesc : pchar) : longint; cdecl; external 'pmgpi' index 411;
function GpiQueryMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbData : BYTE) : longbool; cdecl; external 'pmgpi' index 465;
function GpiQueryMetaFileLength(hmf : cardinal) : longint; cdecl; external 'pmgpi' index 466;
function GpiSaveMetaFile(hmf : cardinal;pszFilename : pchar) : longbool; cdecl; external 'pmgpi' index 500;
function GpiSetMetaFileBits(hmf : cardinal;lOffset,lLength : longint;var pbBuffer : BYTE) : longbool; cdecl; external 'pmgpi' index 536;
function GpiQueryDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 567;
function GpiQueryDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 590;
function GpiQueryDefTag(hps : cardinal;var plTag : longint) : longbool; cdecl; external 'pmgpi' index 568;
function GpiQueryDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 569;
function GpiSetDefArcParams(hps : cardinal;var parcpArcParams : ARCPARAMS) : longbool; cdecl; external 'pmgpi' index 570;
function GpiSetDefAttrs(hps : cardinal;lPrimType : longint;flAttrMask : cardinal;ppbunAttrs : pointer) : longbool; cdecl; external 'pmgpi' index 591;
function GpiSetDefTag(hps : cardinal;lTag : longint) : longbool; cdecl; external 'pmgpi' index 571;
function GpiSetDefViewingLimits(hps : cardinal;var prclLimits : RECTL) : longbool; cdecl; external 'pmgpi' index 572;
function GpiPolygons(hps,ulCount : cardinal;var paplgn : POLYGON;flOptions,flModel : cardinal) : longint; cdecl; external 'pmgpi' index 650;

end.
