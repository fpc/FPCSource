{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 Thorsten Otto

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  PAESContrl = ^TAESContrl;
  TAESContrl = record
    opcode: SmallInt;
    case boolean of
      true: (
        nums: array[0..3] of SmallInt; );
      false: (
        num_intin: SmallInt;
        num_intout: SmallInt;
        num_addrin: SmallInt;
        num_addrout: SmallInt; );
  end;

  PAESGlobal = ^TAESGlobal;
  TAESGlobal = array[0..14] of SmallInt;

  PAESIntIn = ^TAESIntIn;
  TAESIntIn = array[0..15] of SmallInt;

  PAESIntOut = ^TAESIntOut;
  TAESIntOut = array[0..9] of SmallInt;

  PAESAddrIn = ^TAESAddrIn;
  TAESAddrIn = array[0..7] of Pointer;

  PAESAddrOut = ^TAESAddrOut;
  TAESAddrOut = array[0..1] of Pointer;

type
  PAESPB = ^TAESPB;
  TAESPB = record
    control: PAESContrl;
    global: PAESGlobal;
    intin: PAESIntIn;
    intout: PAESIntOut;
    addrin: PAESAddrIn;
    addrout: PAESAddrOut;
  end;

const
  AES_TRAP_MAGIC = $C8;

{ object flags }
const
    NONE            = 0;
    SELECTABLE      = 1;
    DEFAULT         = 2;
    F_EXIT          = 4;
    EDITABLE        = 8;
    RBUTTON         = 16;
    LASTOB          = 32;
    TOUCHEXIT       = 64;
    HIDETREE        = 128;
    INDIRECT        = 256;
    FL3DMASK        = $0600;
    FL3DNONE        = $0000;
    FL3DIND         = $0200;
    FL3DBAK         = $0400;
    FL3DACT         = $0600;
    SUBMENU         = $0800;         {* falcon aes hierarchical menus *}

{ object state }
const
    NORMAL          = $0000;
    SELECTED        = $0001;
    CROSSED         = $0002;
    CHECKED         = $0004;
    DISABLED        = $0008;
    OUTLINED        = $0010;
    SHADOWED        = $0020;
    WHITEBAK        = $0040;
    DRAW3D          = $0080;

{ AES object types }
const
    G_BOX           = 20;
    G_TEXT          = 21;
    G_BOXTEXT       = 22;
    G_IMAGE         = 23;
    G_USERDEF       = 24;
    G_IBOX          = 25;
    G_BUTTON        = 26;
    G_BOXCHAR       = 27;
    G_STRING        = 28;
    G_FTEXT         = 29;
    G_FBOXTEXT      = 30;
    G_ICON          = 31;
    G_TITLE         = 32;
    G_CICON         = 33;
    G_SWBUTTON      = 34;
    G_POPUP         = 35;
    G_WINTITLE      = 36;
    G_EDIT          = 37;
    G_SHORTCUT      = 38;
    G_SLIST         = 39;
    G_EXTBOX        = 40;
    G_OBLINK        = 41;


type
    PTEDINFO = ^TTEDINFO;
    TTEDINFO = record
        te_ptext        : Pchar;
        te_ptmplt       : Pchar;
        te_pvalid       : Pchar;
        te_font         : smallint;
        te_resvd1       : smallint;
        te_just         : smallint;
        te_color        : smallint;
        te_resvd2       : smallint;
        te_thickness    : smallint;
        te_txtlen       : smallint;
        te_tmplen       : smallint;
    end;

    PICONBLK = ^TICONBLK;
    TICONBLK = record
        ib_pmask        : Pointer;
        ib_pdata        : Pointer;
        ib_ptext        : Pchar;
        ib_char         : smallint;
        ib_xchar        : smallint;
        ib_ychar        : smallint;
        ib_xicon        : smallint;
        ib_yicon        : smallint;
        ib_wicon        : smallint;
        ib_hicon        : smallint;
        ib_xtext        : smallint;
        ib_ytext        : smallint;
        ib_wtext        : smallint;
        ib_htext        : smallint;
    end;

    PCICON = ^TCICON;
    TCICON = record
        num_planes      : smallint;
        col_data        : Pointer;
        col_mask        : Pointer;
        sel_data        : Pointer;
        sel_mask        : Pointer;
        next_res        : PCICON;
    end;

    PCICONBLK = ^TCICONBLK;
    TCICONBLK = record
        monoblk         : TICONBLK;
        mainlist        : PCICON;
    end;

    PBITBLK = ^TBITBLK;
    TBITBLK = record
        bi_pdata        : Pointer;
        bi_wb           : smallint;
        bi_hl           : smallint;
        bi_x            : smallint;
        bi_y            : smallint;
        bi_color        : smallint;
    end;

    PUSERBLK = ^TUSERBLK;
    TUSERBLK = record
        ub_code         : Pointer;
        ub_parm         : LongInt;
    end;

    POBSPEC = ^TOBSPEC;
    TOBSPEC = record
        case smallint of
            0,
            G_BOX,
            G_IBOX,
            G_BOXCHAR:  ( index:        LongInt     );
            G_BOXTEXT,
            G_TEXT,
            G_FTEXT,
            G_FBOXTEXT: ( ted_info:     PTEDINFO    );
            G_IMAGE:    ( bit_blk:      PBITBLK );
            G_USERDEF:  ( user_blk:     PUSERBLK    );
            G_BUTTON,
            G_STRING,
            G_TITLE:    ( free_string:  Pchar       );
            G_ICON:     ( icon_blk:     PICONBLK    );
            G_CICON:    ( cicon_blk:    PCICONBLK   );

            INDIRECT:   ( ob_spec:      POBSPEC );
    end;

type
  PAESOBJECT = ^TAESOBJECT;
  TAESOBJECT = record
    ob_next: smallint;   {* The next object               *}
    ob_head: smallint;   {* First child                   *}
    ob_tail: smallint;   {* Last child                    *}
    ob_type: word;       {* Object type                   *}
    ob_flags: word;      {* Manipulation flags            *}
    ob_state: word;      {* Object status                 *}
    ob_spec: TOBSPEC;    {* More under object type        *}
    ob_x: smallint;      {* X-coordinate of the object    *}
    ob_y: smallint;      {* Y-coordinate of the object    *}
    ob_width: smallint;  {* Width of the object           *}
    ob_height: smallint; {* Height of the object          *}
  end;

    PAESTree = ^TAESTree;
    TAESTree = Array[0..2339] of TAESOBJECT;

    PPARMBLK = ^TPARMBLK;
    TPARMBLK = record
        pb_tree         : PAESTree;
        pb_obj          : smallint;
        pr_prevstate    : smallint; {* maybe bug in PP; should be pb_prevstate *}
        pr_currstate    : smallint; {* maybe bug in PP; should be pb_currstate *}
        pb_x            : smallint;
        pb_y            : smallint;
        pb_w            : smallint;
        pb_h            : smallint;
        pb_xc           : smallint;
        pb_yc           : smallint;
        pb_wc           : smallint;
        pb_hc           : smallint;
        pb_parm         : LongInt;
    end;

    PRSHDR = ^TRSHDR;
    TRSHDR = record
        rsh_vrsn:       Word;
        rsh_object:     Word;
        rsh_tedinfo:    Word;
        rsh_iconblk:    Word;
        rsh_bitblk:     Word;
        rsh_frstr:      Word;
        rsh_string:     Word;
        rsh_imdata:     Word;
        rsh_frimg:      Word;
        rsh_trindex:    Word;
        rsh_nobs:       Word;
        rsh_ntree:      Word;
        rsh_nted:       Word;
        rsh_nib:        Word;
        rsh_nbb:        Word;
        rsh_nstring:    Word;
        rsh_nimages:    Word;
        rsh_rssize:     Word;
    end;

type
    PEVENT = ^TEVENT;
    TEVENT = record
        ev_mflags:      smallint;        { input parameters }
        ev_mbclicks:    smallint;
        ev_bmask:       smallint;
        ev_mbstate:     smallint;
        ev_mm1flags:    smallint;
        ev_mm1x:        smallint;
        ev_mm1y:        smallint;
        ev_mm1width:    smallint;
        ev_mm1height:   smallint;
        ev_mm2flags:    smallint;
        ev_mm2x:        smallint;
        ev_mm2y:        smallint;
        ev_mm2width:    smallint;
        ev_mm2height:   smallint;
        ev_mtlocount:   smallint;
        ev_mthicount:   smallint;
        ev_mwich:       smallint;       { output parameters }
        ev_mmox:        smallint;
        ev_mmoy:        smallint;
        ev_mmobutton:   smallint;
        ev_mmokstate:   smallint;
        ev_mkreturn:    smallint;
        ev_mbreturn:    smallint;
        ev_mmgpbuf:     ARRAY_8;
    end;

type
  PGRECT = ^TGRECT;
  TGRECT = record
    case integer of 
     0: (x: smallint;     {* X-coordinate *}
         y: smallint;     {* Y-coordinate *}
         w: smallint;     {* Width        *}
         h: smallint);    {* Height       *}
     1: (g_x: smallint;   {* X-coordinate *}
         g_y: smallint;   {* Y-coordinate *}
         g_w: smallint;   {* Width        *}
         g_h: smallint);  {* Height       *}
  end;

{ kinds, as used by wind_create() }
const
  NAME    = $01;   { Window has a title bar. }
  CLOSER  = $02;   { Window has a close box. }
  FULLER  = $04;   { Window has a fuller box. }
  MOVER   = $08;   { Window may be moved by the user. }
  INFO    = $10;   { Window has an information line. }
  SIZER   = $20;   { Window has a sizer box. }
  UPARROW = $40;   { Window has an up arrow. }
  DNARROW = $80;   { Window has a down arrow. }
  VSLIDE  = $100;  { Window has a vertical slider. }
  LFARROW = $200;  { Window has a left arrow. }
  RTARROW = $400;  { Window has a right arrow. }
  HSLIDE  = $800;  { Window has a horizontal slider. }
  MENUBAR = $1000; { Window has a menu bar (XaAES) }
  SMALLER = $4000; { Window has an iconifier. }
  ICONIFIER = SMALLER;
  BORDER  = $8000; { Window has an sizeing border }

{ messages as used by evnt_mesag() }
const
  MN_SELECTED   = 10;
  WM_REDRAW     = 20;
  WM_TOPPED     = 21;
  WM_CLOSED     = 22;
  WM_FULLED     = 23;
  WM_ARROWED    = 24;
  WM_HSLID      = 25;
  WM_VSLID      = 26;
  WM_SIZED      = 27;
  WM_MOVED      = 28;
  WM_NEWTOP     = 29;
  WM_UNTOPPED   = 30;
  WM_ONTOP      = 31;
  WM_OFFTOP     = 32;
  WM_BOTTOMED   = 33;
  WM_ICONIFY    = 34;
  WM_UNICONIFY  = 35;
  WM_ALLICONIFY = 36;
  WM_TOOLBAR    = 37;
  AC_OPEN       = 40;
  AC_CLOSE      = 41;
  CT_UPDATE     = 50;
  CT_MOVE       = 51;
  CT_NEWTOP     = 52;
  AP_TERM       = 50;
  AP_TFAIL      = 51;
  AP_RESCHG     = 57;
  SHUT_COMPLETED = 60;
  RESCHG_COMPLETED = 61;
  AP_DRAGDROP   = 63;
  SH_EXIT       = 68;          {* AES 4.0 *}
  SH_START      = 69;          {* AES 4.0 *}
  SH_WDRAW      = 72;          {* AES 4.0 *}
  SC_CHANGED    = 80;
  PRN_CHANGED   = 82;          {* NVDI *}
  FNT_CHANGED   = 83;          {* NVDI *}
  COLORS_CHANGED = 84;         {* NVDI *}
  THR_EXIT      = 88;          {* MagiC 4.5 *}
  PA_EXIT       = 89;          {* MagiC 3 *}
  CH_EXIT       = 90;
  WM_WHEEL      = 345;         {* XaAES *}
  WM_MOUSEWHEEL = 2352;
  WM_SHADED     = 22360;       {* WiNX *}
  WM_UNSHADED   = 22361;       {* WinX *}

    WA_UPPAGE       = 0;
    WA_DNPAGE       = 1;
    WA_UPLINE       = 2;
    WA_DNLINE       = 3;
    WA_LFPAGE       = 4;
    WA_RTPAGE       = 5;
    WA_LFLINE       = 6;
    WA_RTLINE       = 7;
    WA_WHEEL        = 8;

{* AP_DRAGDROP return codes *}
const
    DD_OK        = 0;
    DD_NAK       = 1;
    DD_EXT       = 2;
    DD_LEN       = 3;
    DD_TRASH     = 4;
    DD_PRINTER   = 5;
    DD_CLIPBOARD = 6;

    DD_TIMEOUT  = 4000;     {* Timeout in ms *}

    DD_NUMEXTS  = 8;        {* Number of formats *}
    DD_EXTLEN   = 4;
    DD_EXTSIZE  = DD_NUMEXTS * DD_EXTLEN;

    DD_FNAME    = 'U:\\PIPE\\DRAGDROP.AA';
    DD_NAMEMAX  = 128;      {* Maximum length of a format name *}
    DD_HDRMIN   = 9;            {* Minimum length of Drag&Drop headers *}
    DD_HDRMAX   = 8 + DD_NAMEMAX;   {* Maximum length *}

{ message flags as used by evnt_multi() }
const
  MU_KEYBD  = $0001; { Keyboard event }
  MU_BUTTON = $0002; { Button event   }
  MU_M1     = $0004; { Mouse event 1  }
  MU_M2     = $0008; { Mouse event 2  }
  MU_MESAG  = $0010; { Messages       }
  MU_TIMER  = $0020; { Timer events   }
  MU_WHEEL         = $0040;      {* AES 4.09 & XaAES *}
  MU_MX            = $0080;      {* XaAES *}
  MU_NORM_KEYBD    = $0100;      {*   "   *}
  MU_DYNAMIC_KEYBD = $0200;      {* keybd as a bunch of buttons, includes release of key *}

{ window update flags as used by wind_update() }
const
  END_UPDATE = (0);  { Screen redraw is compete and the flag set by BEG_UPDATE is reset }
  BEG_UPDATE = (1);  { Screen redraw starts, rectangle lists are frozen, flag is set to prevent any other processes updating the screen }
  END_MCTRL  = (2);  { Application releases control of the mouse to the AES and resumes mouse click message reactions }
  BEG_MCTRL  = (3);  { The application wants to have sole control over mouse button messages }
  BEG_CHECK  = $100;

{ window flags as used by wind_set()/wind_get() }
const
  WF_KIND      = (1);
  WF_NAME      = (2);
  WF_INFO      = (3);
  WF_WORKXYWH  = (4);
  WF_CURRXYWH  = (5);
  WF_PREVXYWH  = (6);
  WF_FULLXYWH  = (7);
  WF_HSLIDE    = (8);
  WF_VSLIDE    = (9);
  WF_TOP       = (10);
  WF_FIRSTXYWH = (11);
  WF_NEXTXYWH  = (12);
  WF_NEWDESK   = (14);
  WF_HSLSIZE   = (15);
  WF_VSLSIZE   = (16);
  WF_SCREEN    = (17);
  WF_COLOR     = 18;
  WF_TATTRB    = 18;
  WF_DCOLOR    = 19;
  WF_SIZTOP    = 19;
  WF_OWNER     = 20;
  WF_BEVENT    = 24;
  WF_BOTTOM    = 25;
  WF_ICONIFY   = 26;
  WF_UNICONIFY = 27;
  WF_UNICONIFYXYWH = 28;
  WF_TOOLBAR   = (30);
  WF_FTOOLBAR  = 31;
  WF_NTOOLBAR  = 32;
  WF_MENU      = 33;
  WF_WIDGET    = 34;
  WF_WHEEL     = 40;
  WF_OPTS      = 41;
  WF_TOPMOST   = 232;             {* XaAES, MyAES *}
  WF_WINX      = $5758;
  WF_WINXCFG   = $5759;
  WF_DDELAY    = $575a;
  WF_SHADE     = $575d;
  WF_STACK     = $575e;
  WF_TOPALL    = $575f;
  WF_BOTTOMALL = $5760;
  WF_XAAES     = $5841;

{ window calculation types as used by wind_calc() }
const
  WC_BORDER = 0;
  WC_WORK   = 1;

{ WF_DCOLOR objects }
const
    W_BOX           = 0;
    W_TITLE         = 1;
    W_CLOSER        = 2;
    W_NAME          = 3;
    W_FULLER        = 4;
    W_INFO          = 5;
    W_DATA          = 6;
    W_WORK          = 7;
    W_SIZER         = 8;
    W_VBAR          = 9;
    W_UPARROW       = 10;
    W_DNARROW       = 11;
    W_VSLIDE        = 12;
    W_VELEV         = 13;
    W_HBAR          = 14;
    W_LFARROW       = 15;
    W_RTARROW       = 16;
    W_HSLIDE        = 17;
    W_HELEV         = 18;
    W_SMALLER       = 19;
    W_BOTTOMER      = 20;
    W_HIDER         = 30;

{* wind_set(WF_BEVENT) *}
    BEVENT_WORK     = $0001;          {* AES 4.0  *}
    BEVENT_INFO     = $0002;          {* MagiC 6  *}

{* wind_set(WF_OPTS) bitmask flags *}
    WO0_WHEEL       = $0001;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO0_FULLREDRAW  = $0002;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO0_NOBLITW     = $0004;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO0_NOBLITH     = $0008;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO0_SENDREPOS   = $0010;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO1_NONE        = $0000;  {* see mt_wind_set() with #WF_OPTS mode *}
    WO2_NONE        = $0000;  {* see mt_wind_set() with #WF_OPTS mode *}

{* wind_set(WF_WHEEL) modes *}
    WHEEL_MESAG     = 0;   {* AES will send #WM_WHEEL messages *}
    WHEEL_ARROWED   = 1;   {* AES will send #WM_ARROWED messages *}
    WHEEL_SLIDER    = 2;   {* AES will convert mouse wheel events to slider events *}


{ AES standard object colors }
const
  WHITE    = (00);  { White          1000, 1000, 1000 }
  BLACK    = (01);  { Black             0,    0,    0 }
  RED      = (02);  { Red            1000,    0,    0 }
  GREEN    = (03);  { Green             0, 1000,    0 }
  BLUE     = (04);  { Blue              0,    0, 1000 }
  CYAN     = (05);  { Cyan              0, 1000, 1000 }
  YELLOW   = (06);  { Yellow         1000, 1000,    0 }
  MAGENTA  = (07);  { Magenta        1000,    0, 1000 }
  DWHITE   = (08);  { Light grey      752,  752,  752 }
  DBLACK   = (09);  { Dark grey       501,  501,  501 }
  DRED     = (10);  { Dark red        713,    0,    0 }
  DGREEN   = (11);  { Dark green        0,  713,    0 }
  DBLUE    = (12);  { Dark blue         0,    0,  713 }
  DCYAN    = (13);  { Dark cyan         0,  713,  713 }
  DYELLOW  = (14);  { Dark yellow     713,  713,    0 }
  DMAGENTA = (15);  { Dark magenta    713,    0,  713 }

{* editable text justification *}
const
    TE_LEFT         = 0;
    TE_RIGHT        = 1;
    TE_CNTR         = 2;
    TE_JUST_MASK    = 3;

{* font types *}
const
    GDOS_PROP        = 0; {* Speedo GDOS font *}
    GDOS_MONO        = 1; {* Speedo GDOS font, force monospace output *}
    GDOS_BITM        = 2; {* GDOS bit map font *}
    IBM              = 3;
    SMALL            = 5;
    TE_FONT_MASK     = 7;

{* editable text field definitions *}
const
    ED_START        = 0;
    ED_INIT         = 1;
    ED_CHAR         = 2;
    ED_END          = 3;
    ED_CRSR         = 100;            {* MAG!X *}
    ED_DRAW         = 103;            {* MAG!X 2.00 *}
    EDSTART         = 0;
    EDINIT          = 1;
    EDCHAR          = 2;
    EDEND           = 3;

{$IFNDEF IP_HOLLOW_defined}
{* inside patterns *}
    IP_HOLLOW       = 0;
    IP_1PATT        = 1;
    IP_2PATT        = 2;
    IP_3PATT        = 3;
    IP_4PATT        = 4;
    IP_5PATT        = 5;
    IP_6PATT        = 6;
    IP_SOLID        = 7;
{$DEFINE IP_HOLLOW_defined}
{$ENDIF}

    ROOT            = 0;
    MAX_DEPTH       = 8;

{ AES mouse cursor number }
const
  ARROW          = 0;       { Arrow                               }
  TEXT_CRSR      = 1;       { Text cursor                         }
  HOURGLASS      = 2;       { Hourglass, bee                      }
  BUSY_BEE       = 2;       { See HOURGLASS                       }
  BUSYBEE        = 2;       { See HOURGLASS                       }
  POINT_HAND     = 3;       { Pointing hand                       }
  FLAT_HAND      = 4;       { Flat hand                           }
  THIN_CROSS     = 5;       { Thin crosshairs                     }
  THICK_CROSS    = 6;       { Thick crosshairs                    }
  OUTLN_CROSS    = 7;       { Outlined crosshairs                 }
  USER_DEF       = 255;     { User-defined mouse form             }
  M_OFF          = 256;     { Switch off mouse pointer            }
  M_ON           = 257;     { Switch on mouse pointer             }
  M_SAVE         = 258;     { Save current mouse form             }
  M_LAST         = 259;     { Restore most recently saved form    }
  M_PREVIOUS     = 259;     { See M_LAST                          }
  M_RESTORE      = 260;     { Restore last shape                  }
  M_FORCE        = $8000;
  X_MRESET       = 1000;
  X_MGET         = 1001;
  X_MSET_SHAPE   = 1100;

{ Menu definitions as used by menu_bar() }
const
  MENU_INQUIRE   = -1;
  MENU_HIDE      = 0;
  MENU_SHOW      = 1;

{ Form dialog space actions, as used by form_dial() }
const
  FMD_START  = 0; { Reserve screen space for a dialog }
  FMD_GROW   = 1; { Draw several expanding rectangles from the coordinates fo_dilittlx/y/w/hto fo_dibigx/y/w/h }
  FMD_SHRINK = 2; { Draw several shrinking rectangles from fo_dibigx/y/w/h to fo_dilittlx/y/w/h }
  FMD_FINISH = 3; { Release the reserved screen space again }

{ Resource structure types as used by rsrc_gaddr()/rsrc_saddr() }
const
  R_TREE      = 0;  { Object tree                          }
  R_OBJECT    = 1;  { Individual OBJECT (TAESOBJECT)       }
  R_TEDINFO   = 2;  { TEDINFO structure                    }
  R_ICONBLK   = 3;  { ICONBLK structure                    }
  R_BITBLK    = 4;  { BITBLK structure                     }
  R_STRING    = 5;  { Free string data                     }
  R_IMAGEDATA = 6;  { Free image data                      }
  R_OBSPEC    = 7;  { ob_spec field in OBJECT (TAESOBJECT) }
  R_TEPTEXT   = 8;  { te_ptext in TEDINFO                  }
  R_TEPTMPLT  = 9;  { te_ptmplt in TEDINFO                 }
  R_TEPVALID  = 10; { te_pvalid in TEDINFO                 }
  R_IBPMASK   = 11; { ib_pmask in ICONBLK                  }
  R_IBPDATA   = 12; { ib_pdata in ICONBLK                  }
  R_IBPTEXT   = 13; { ib_ptext in ICONBLK                  }
  R_BIPDATA   = 14; { ib_pdate in BITBLK                   }
  R_FRSTR     = 15; { ad_frstr free string                 }
  R_FRIMG     = 16; { ad_frimg free image                  }

{* keyboard states (same as in bios) *}
    K_RSHIFT        = $0001;
    K_LSHIFT        = $0002;
    K_SHIFT         = $0003;
    K_CTRL          = $0004;
    K_ALT           = $0008;
    K_CAPSLOCK      = $0010;

{* appl_read modes *}
    APR_NOWAIT      = -1;   {* Do not wait for message -- see mt_appl_read() *}

{* appl_search modes *}
    APP_FIRST = 0;
    APP_NEXT  = 1;
    APP_DESK  = 2;
    X_APS_CHILD0    = $7100; {* Geneva *}
    X_APS_CHILD     = $7101; {* Geneva *}
    X_APS_CHEXIT    = -1;    {* Geneva *}

{* application type (appl_search return values) *}
    APP_SYSTEM          = $001;
    APP_APPLICATION     = $002;
    APP_ACCESSORY       = $004;
    APP_SHELL           = $008;
    APP_AESSYS          = $010;
    APP_AESTHREAD       = $020;
    APP_TASKINFO        = $100; {* XaAES extension for taskbar applications. *}
    APP_HIDDEN          = $100; {* Task is disabled; XaAES only for APP_TASKINFO *}
    APP_FOCUS           = $200; {* Active application; XaAES only for APP_TASKINFO *}

{* menu_attach modes *}
    ME_INQUIRE      = 0;
    ME_ATTACH       = 1;
    ME_REMOVE       = 2;
{* menu_attach attributes *}
    SCROLL_NO       = 0;
    SCROLL_YES      = 1;

{* menu_popup modes *}
    SCROLL_LISTBOX      = -1;

{* the objc_sysvar ob_swhich values *}
    LK3DIND      = 1;
    LK3DACT      = 2;
    INDBUTCOL    = 3;
    ACTBUTCOL    = 4;
    BACKGRCOL    = 5;
    AD3DVAL      = 6;
    MX_ENABLE3D  = 10;
    MENUCOL      = 11;

    OB_GETVAR = 0;
    OB_SETVAR = 1;

{* objc_sysvar modes *}
    SV_INQUIRE      = 0;
    SV_SET          = 1;

{* scrp_read return values *}
    SCRAP_CSV       = $0001;
    SCRAP_TXT       = $0002;
    SCRAP_GEM       = $0004;
    SCRAP_IMG       = $0008;
    SCRAP_DCA       = $0010;
    SCRAP_DIF       = $0020;
    SCRAP_USR       = $8000;

{* shel_write modes for parameter "isover" *}
    SHW_IMMED        = 0;
    SHW_CHAIN        = 1;
    SHW_DOS          = 2;
    SHW_PARALLEL   = 100;
    SHW_SINGLE     = 101;

{* shel_write sh_wdoex parameter flags in MSB *}
    SHD_PSETLIM = (1 shl 8);    { MiNT memory allocation limit }
    SHD_PRENICE = (1 shl 9);    { MiNT Prenice (priority) level }
    SHD_DFLTDIR = (1 shl 10);   { Default directory string }
    SHD_ENVIRON = (1 shl 11);   { Environment string }
    SHD_UID     = (1 shl 12);   { set user id }
    SHD_GID     = (1 shl 13);   { set group id }

{* shel_write modes for parameter "doex" *}
    SWM_LAUNCH       = 0;
    SWM_LAUNCHNOW    = 1;
    SWM_LAUNCHACC    = 3;
    SWM_SHUTDOWN     = 4;
    SWM_REZCHANGE    = 5;
    SWM_BROADCAST    = 7;
    SWM_ENVIRON      = 8;
    SWM_NEWMSG       = 9;
    SWM_AESMSG      = 10;
    SWM_THRCREATE   = 20;
    SWM_THREXIT     = 21;
    SWM_THRKILL     = 22;

{* shel_write, parameter wisgr *}
    TOSAPP              = 0;
    GEMAPP              = 1;

{* command line parser (shel_write: parameter "wiscr") *}
    CL_NORMAL       = 0;
    CL_PARSE        = 1;

{* shutdown action (shel_write: mode SWM_SHUTDOWN, parameter "wiscr") *}
    SD_ABORT        = 0;
    SD_PARTIAL      = 1;
    SD_COMPLETE     = 2;

{* shel_write: mode SWM_ENVIRON, parameter 'wisgr' *}
    ENVIRON_SIZE    = 0;
    ENVIRON_CHANGE  = 1;
    ENVIRON_COPY    = 2;

type
    PMENU = ^TMENU;
    TMENU = record
        mn_tree:        PAESTree;
        mn_menu:        smallint;
        mn_item:        smallint;
        mn_scroll:      smallint;
        mn_keystate:    smallint;
    end;

    PMN_SET = ^TMN_SET;
    TMN_SET = record
        Display:        LongInt;
        Drag:           LongInt;
        Delay:          LongInt;
        Speed:          LongInt;
        Height:     smallint;
    end;

{* extended appl_write structure *}
    PXAESMSG = ^TXAESMSG;
    TXAESMSG = record
        dst_apid: smallint;
        unique_flg: smallint;
        attached_mem: Pointer;
        msgbuf: Psmallint;
    end;

{* tail for default shell *}
    PSHELTAIL = ^TSHELTAIL;
    TSHELTAIL = record
        dummy: smallint;
        magic: longint;
        isfirst: smallint;
        lasterr: longint;
        wasgr: smallint;
    end;

{* shel_write alternative structure for sh_wpcmd parameter *}
    PSHELW = ^TSHELW;
    TSHELW = record
        newcmd: Pchar;
        psetlimit: longint;
        prenice: longint;
        defdir: pchar;
        env: pointer;
        case boolean of
          false: (uid, gid: smallint;);
          true: (flags: longint;);
    end;

