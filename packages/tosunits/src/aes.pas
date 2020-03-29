{
    Copyright (c) 2016 by Free Pascal development team

    AES interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit aes;

interface

{ The API description of this file is based on the information available
  online at: http://toshyp.atari.org }

type
  PAESContrl = ^TAESContrl;
  TAESContrl = record
    opcode: SmallInt;
    case boolean of
      true: (
        nums: array[0..3] of SmallInt; );
      false: (
        num_intin: SmallInt;
        num_addrin: SmallInt;
        num_intout: SmallInt;
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
    contrl: PAESContrl;
    global: PAESGlobal;
    intin: PAESIntIn;
    intout: PAESIntOut;
    addrin: PAESAddrIn;
    addrout: PAESAddrOut;
  end;

const
  AES_TRAP_MAGIC = $C8;

type
  PAESOBJECT = ^TAESOBJECT;
  TAESOBJECT = record
    ob_next: smallint;   {* The next object               *}
    ob_head: smallint;   {* First child                   *}
    ob_tail: smallint;   {* Last child                    *}
    ob_type: word;       {* Object type                   *}
    ob_flags: word;      {* Manipulation flags            *}
    ob_state: word;      {* Object status                 *}
    ob_spec: pointer;    {* More under object type        *}
    ob_x: smallint;      {* X-coordinate of the object    *}
    ob_y: smallint;      {* Y-coordinate of the object    *}
    ob_width: smallint;  {* Width of the object           *}
    ob_height: smallint; {* Height of the object          *}
  end;

type
  PGRECT = ^TGRECT;
  TGRECT = record
    x: smallint;   {* X-coordinate *}
    y: smallint;   {* Y-coordinate *}
    w: smallint;   {* Width        *}
    h: smallint;   {* Height       *}
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
  SMALLER = $4000; { Window has an iconifier. }

{ messages as used by evnt_mesag() }
const
  WM_REDRAW     = $0014;
  WM_TOPPED     = $0015;
  WM_CLOSED     = $0016;
  WM_FULLED     = $0017;
  WM_ARROWED    = $0018;
  WM_HSLID      = $0019;
  WM_VSLID      = $001a;
  WM_SIZED      = $001b;
  WM_MOVED      = $001c;
  WM_NEWTOP     = $001d;
  WM_UNTOPPED   = $001e;
  WM_ONTOP      = $001f;
  WM_OFFTOP     = $0020;
  WM_BOTTOMED   = $0021;
  WM_ICONIFY    = $0022;
  WM_UNICONIFY  = $0023;
  WM_ALLICONIFY = $0024;
  WM_TOOLBAR    = $0025;

{ message flags as used by evnt_multi() }
const
  MU_KEYBD  = $0001; { Keyboard event }
  MU_BUTTON = $0002; { Button event   }
  MU_M1     = $0004; { Mouse event 1  }
  MU_M2     = $0008; { Mouse event 2  }
  MU_MESAG  = $0010; { Messages       }
  MU_TIMER  = $0020; { Timer events   }

{ window update flags as used by wind_update() }
const
  END_UPDATE = (0);  { Screen redraw is compete and the flag set by BEG_UPDATE is reset }
  BEG_UPDATE = (1);  { Screen redraw starts, rectangle lists are frozen, flag is set to prevent any other processes updating the screen }
  END_MCTRL  = (2);  { Application releases control of the mouse to the AES and resumes mouse click message reactions }
  BEG_MCTRL  = (3);  { The application wants to have sole control over mouse button messages }

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
  WF_TOOLBAR   = (30);
  WF_MENU      = (33);

{ window calculation types as used by wind_calc() }
const
  WC_BORDER = 0;
  WC_WORK   = 1;

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


{ AES mouse form structure }
type
  PMFORM = ^TMFORM;
  TMFORM = record
    mf_xhot: smallint;       {* X-position hot-spot *}
    mf_yhot: smallint;       {* Y-position hot-spot *}
    mf_nplanes: smallint;    {* Number of planes    *}
    mf_fg: smallint;         {* Mask colour         *}
    mf_bg: smallint;         {* Pointer colour      *}
    mf_mask: array[0..15] of smallint;   {* Mask form           *}
    mf_data: array[0..15] of smallint;   {* Pointer form        *}
  end;

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


function appl_exit: smallint;
function appl_read(ap_rid: smallint; ap_rlength: smallint; ap_rpbuff: pointer): smallint;
function appl_write(ap_wid: smallint; ap_wlength: smallint; ap_wpbuff: pointer): smallint;
function appl_find(fname: PChar): smallint;
function appl_init: smallint;

function evnt_keybd: smallint;
function evnt_button(ev_bclicks: smallint; ev_bmask: smallint; ev_bstate: smallint;
                     ev_bmx: psmallint; ev_bmy: psmallint; ev_bbutton: psmallint; ev_bkstate: psmallint): smallint;
function evnt_mouse(ev_moflags: smallint; ev_mox: smallint; ev_moy: smallint; ev_mowidth: smallint; ev_moheight: smallint;
                    ev_momx: psmallint; ev_momy: psmallint; ev_mobutton: psmallint; ev_mokstate: psmallint): smallint;
function evnt_mesag(msg: psmallint): smallint;
function evnt_timer(ev_tlocount: smallint; ev_thicount: smallint): smallint;
function evnt_multi(ev_mflags: smallint; ev_mbclicks: smallint; ev_mbmask: smallint; ev_mbstate: smallint;
                    ev_mm1flags: smallint; ev_mm1x: smallint; ev_mm1y: smallint; ev_mm1width: smallint; ev_mm1height: smallint;
                    ev_mm2flags: smallint; ev_mm2x: smallint; ev_mm2y: smallint; ev_mm2width: smallint; ev_mm2height: smallint;
                    ev_mmgpbuff: psmallint; ev_mtlocount: smallint; ev_mthicount: smallint;
                    ev_mmox: psmallint; ev_mmoy: psmallint; ev_mmbutton: psmallint; ev_mmokstate: psmallint;
                    ev_mkreturn: psmallint; ev_mbreturn: psmallint): smallint;
function evnt_dclick(ev_dnew: smallint; ev_dgetset: smallint): smallint;

function menu_bar(me_btree: PAESOBJECT; me_bshow: smallint): smallint;
function menu_icheck(me_ctree: PAESOBJECT; me_citem: smallint; me_ccheck: smallint): smallint;
function menu_ienable(me_etree: PAESOBJECT; me_eitem: smallint; me_eenable: smallint): smallint;
function menu_tnormal(me_ntree: PAESOBJECT; me_ntitle: smallint; me_nnormal: smallint): smallint;
function menu_text(me_ttree: PAESOBJECT; me_titem: smallint; me_ttext: PChar): smallint;
function menu_register(me_rapid: smallint; me_rpstring: PChar): smallint;

function objc_add(ob_atree: PAESOBJECT; ob_aparent: smallint; ob_achild: smallint): smallint;
function objc_delete(ob_dltree: PAESOBJECT; ob_dlobject: smallint): smallint;
function objc_draw(ob_drtree: PAESOBJECT; ob_drstart: smallint;
                   ob_drdepth: smallint; ob_drxclip: smallint;
                   ob_dryclip: smallint; ob_drwclip: smallint;
                   ob_drhclip: smallint): smallint;
function objc_find(ob_ftree: PAESOBJECT; ob_fstartob: smallint;
                   ob_fdepth: smallint; ob_fmx: smallint;
                   ob_fmy: smallint): smallint;
function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     ob_ofxoff: psmallint; ob_ofyoff: psmallint): smallint;
function objc_order(ob_ortree: PAESOBJECT; ob_orobject: smallint;
                    ob_ornewpos: smallint): smallint;
function objc_edit(ob_edtree: PAESOBJECT; ob_edobject: smallint;
                   ob_edchar: smallint; ob_edidx: psmallint;
                   ob_edkind: smallint): smallint;
function objc_change(ob_ctree: PAESOBJECT; ob_cobject: smallint;
                     ob_cresvd: smallint; ob_cxclip: smallint;
                     ob_cyclip: smallint; ob_cwclip: smallint;
                     ob_chclip: smallint; ob_cnewstate: smallint;
                     ob_credraw: smallint): smallint;

function form_do(fo_dotree: PAESOBJECT; fo_dostartob: smallint): smallint;
function form_dial(fo_diflag: smallint; fo_dilittlx: smallint;
                   fo_dilittly: smallint; fo_dilittlw: smallint;
                   fo_dilittlh: smallint; fo_dibigx: smallint;
                   fo_dibigy: smallint; fo_dibigw: smallint;
                   fo_dibigh: smallint): smallint;
function form_alert(default: smallint; alertstr: PChar): smallint;
function form_error(error: smallint): smallint;
function form_center(fo_ctree: PAESOBJECT; fo_cx: psmallint;
                     fo_cy: psmallint; fo_cw: psmallint;
                     fo_ch: psmallint): smallint;
function form_keybd(fo_ktree: PAESOBJECT; fo_kobject: smallint;
                    fo_kobnext: smallint; fo_kchar: smallint;
                    fo_knxtobject: psmallint; fo_knxtchar: psmallint): smallint;
function form_button(fo_btree: PAESOBJECT; fo_bobject: smallint;
                     fo_bclicks: smallint; fo_bnxtobj: psmallint): smallint;

function graf_handle(gr_hwchar: psmallint; gr_hhchar: psmallint; gr_hwbox: psmallint; gr_hhbox: psmallint): smallint;
function graf_mouse(gr_monumber: smallint; gr_mofaddr: PMFORM): smallint;
function graf_mkstate(gr_mkmx: psmallint; gr_mkmy: psmallint;
                      gr_mkmstate: psmallint; gr_mkkstate: psmallint): smallint;

function scrp_read(sc_rpscrap: pchar): smallint;
function scrp_write(sc_wpscrap: pchar): smallint;

function fsel_input(fs_iinpath: pchar; fs_iinsel: pchar; fs_iexbutton: psmallint): smallint;
function fsel_exinput(fs_einpath: pchar; fs_einsel: pchar; fs_eexbutton: psmallint; elabel: pchar): smallint;

function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
function wind_open(handle: smallint; x, y, w, h: smallint): smallint;
function wind_close(wi_clhandle: smallint): smallint;
function wind_delete(handle: smallint): smallint;
function wind_get(wi_ghandle: smallint; wi_gfield: smallint;
                  wi_gw1: psmallint; wi_gw2: psmallint;
                  wi_gw3: psmallint; wi_gw4: psmallint): smallint;
function wind_set(wi_shandle: smallint; wi_sfield: smallint;
                  wi_sw1: smallint; wi_sw2: smallint;
                  wi_sw3: smallint; wi_sw4: smallint): smallint;
function wind_find(wi_fmx: smallint; wi_fmy: smallint): smallint;
function wind_update(wi_ubegend: smallint): smallint;
function wind_calc(wi_ctype: smallint; wi_ckind: smallint;
                   wi_cinx: smallint; wi_ciny: smallint;
                   wi_cinw: smallint; wi_cinh: smallint;
                   coutx: psmallint; couty: psmallint;
                   coutw: psmallint; couth: psmallint): smallint;
procedure wind_new;

function rsrc_load(re_lpfname: PChar): smallint;
function rsrc_free: smallint;
function rsrc_gaddr(re_gtype: smallint; re_gindex: smallint; gaddr: ppointer): smallint;
function rsrc_saddr(re_stype: smallint; re_sindex: smallint; saddr: pointer): smallint;
function rsrc_obfix(re_otree: PAESOBJECT; re_oobject: smallint): smallint;

function shel_read(sh_rpcmd: pchar; sh_rptail: pchar): smallint;
function shel_write(sh_wdoex: smallint; sh_wisgr: smallint;
                    sh_wiscr: smallint; sh_wpcmd: pchar;
                    sh_wptail: pchar): smallint;
function shel_get(sh_gaddr: pchar; sh_glen: word): smallint;
function shel_put(sh_paddr: pchar; sh_plen: word): smallint;
function shel_find(sh_fpbuff: pchar): smallint;
function shel_envrn(sh_epvalue: ppchar; sh_eparm: pchar): smallint;


function crys_if(_opcode: dword): smallint;

implementation

const
  ops_table: array[0..120,0..3] of SmallInt = (
    ( 0, 1, 0, 0 ),    // 10, appl_init
    ( 2, 1, 1, 0 ),    // 11, appl_read
    ( 2, 1, 1, 0 ),    // 12, appl_write
    ( 0, 1, 1, 0 ),    // 13, appl_find
    ( 2, 1, 1, 0 ),    // 14, appl_tplay !
    ( 1, 1, 1, 0 ),    // 15, appl_trecord !
    ( 0, 0, 0, 0 ),    // 16
    ( 0, 0, 0, 0 ),    // 17
    ( 1, 3, 1, 0 ),    // 18, appl_search (V4.0) !
    ( 0, 1, 0, 0 ),    // 19, appl_exit
    ( 0, 1, 0, 0 ),    // 20, evnt_keybd 
    ( 3, 5, 0, 0 ),    // 21, evnt_button
    ( 5, 5, 0, 0 ),    // 22, evnt_mouse
    ( 0, 1, 1, 0 ),    // 23, evnt_mesag
    ( 2, 1, 0, 0 ),    // 24, evnt_timer
    (16, 7, 1, 0 ),    // 25, evnt_multi
    ( 2, 1, 0, 0 ),    // 26, evnt_dclick
    ( 0, 0, 0, 0 ),    // 27
    ( 0, 0, 0, 0 ),    // 28
    ( 0, 0, 0, 0 ),    // 29
    ( 1, 1, 1, 0 ),    // 30, menu_bar
    ( 2, 1, 1, 0 ),    // 31, menu_icheck
    ( 2, 1, 1, 0 ),    // 32, menu_ienable
    ( 2, 1, 1, 0 ),    // 33, menu_tnormal
    ( 1, 1, 2, 0 ),    // 34, menu_text
    ( 1, 1, 1, 0 ),    // 35, menu_register
    ( 2, 1, 2, 0 ),    // 36, menu_popup (V3.3) !
    ( 2, 1, 2, 0 ),    // 37, menu_attach (V3.3) !
    ( 3, 1, 1, 0 ),    // 38, menu_istart (V3.3) !
    ( 1, 1, 1, 0 ),    // 39, menu_settings (V3.3) !
    ( 2, 1, 1, 0 ),    // 40, objc_add
    ( 1, 1, 1, 0 ),    // 41, objc_delete
    ( 6, 1, 1, 0 ),    // 42, objc_draw
    ( 4, 1, 1, 0 ),    // 43, objc_find
    ( 1, 3, 1, 0 ),    // 44, objc_offset
    ( 2, 1, 1, 0 ),    // 45, objc_order
    ( 4, 2, 1, 0 ),    // 46, objc_edit
    ( 8, 1, 1, 0 ),    // 47, objc_change
    ( 4, 3, 0, 0 ),    // 48, objc_sysvar (V3.4) !
    ( 0, 0, 0, 0 ),    // 49
    ( 1, 1, 1, 0 ),    // 50, form_do
    ( 9, 1, 0, 0 ),    // 51, form_dial
    ( 1, 1, 1, 0 ),    // 52, form_alert
    ( 1, 1, 0, 0 ),    // 53, form_error
    ( 0, 5, 1, 0 ),    // 54, form_center
    ( 3, 3, 1, 0 ),    // 55, form_keybd
    ( 2, 2, 1, 0 ),    // 56, form_button
    ( 0, 0, 0, 0 ),    // 57
    ( 0, 0, 0, 0 ),    // 58
    ( 0, 0, 0, 0 ),    // 59
    ( 0, 0, 0, 0 ),    // 60
    ( 0, 0, 0, 0 ),    // 61
    ( 0, 0, 0, 0 ),    // 62
    ( 0, 0, 0, 0 ),    // 63
    ( 0, 0, 0, 0 ),    // 64
    ( 0, 0, 0, 0 ),    // 65
    ( 0, 0, 0, 0 ),    // 66
    ( 0, 0, 0, 0 ),    // 67
    ( 0, 0, 0, 0 ),    // 68
    ( 0, 0, 0, 0 ),    // 69
    ( 4, 3, 0, 0 ),    // 70, graf_rubberbox !
    ( 8, 3, 0, 0 ),    // 71, graf_dragbox !
    ( 6, 1, 0, 0 ),    // 72, graf_movebox !
    ( 8, 1, 0, 0 ),    // 73, graf_growbox !
    ( 8, 1, 0, 0 ),    // 74, graf_shrinkbox !
    ( 4, 1, 1, 0 ),    // 75, graf_watchbox !
    ( 3, 1, 1, 0 ),    // 76, graf_slidebox !
    ( 0, 5, 0, 0 ),    // 77, graf_handle
    ( 1, 1, 1, 0 ),    // 78, graf_mouse
    ( 0, 5, 0, 0 ),    // 79, graf_mkstate
    ( 0, 1, 1, 0 ),    // 80, scrp_read
    ( 0, 1, 1, 0 ),    // 81, scrp_write
    ( 0, 0, 0, 0 ),    // 82
    ( 0, 0, 0, 0 ),    // 83
    ( 0, 0, 0, 0 ),    // 84
    ( 0, 0, 0, 0 ),    // 85
    ( 0, 0, 0, 0 ),    // 86
    ( 0, 0, 0, 0 ),    // 87
    ( 0, 0, 0, 0 ),    // 88
    ( 0, 0, 0, 0 ),    // 89
    ( 0, 2, 2, 0 ),    // 90, fsel_input
    ( 0, 2, 3, 0 ),    // 91, fsel_exinput
    ( 0, 0, 0, 0 ),    // 92
    ( 0, 0, 0, 0 ),    // 93
    ( 0, 0, 0, 0 ),    // 94
    ( 0, 0, 0, 0 ),    // 95
    ( 0, 0, 0, 0 ),    // 96
    ( 0, 0, 0, 0 ),    // 97
    ( 0, 0, 0, 0 ),    // 98
    ( 0, 0, 0, 0 ),    // 99
    ( 5, 1, 0, 0 ),    // 100, wind_create
    ( 5, 1, 0, 0 ),    // 101, wind_open
    ( 1, 1, 0, 0 ),    // 102, wind_close
    ( 1, 1, 0, 0 ),    // 103, wind_delete
    ( 2, 5, 0, 0 ),    // 104, wind_get
    ( 6, 1, 0, 0 ),    // 105, wind_set
    ( 2, 1, 0, 0 ),    // 106, wind_find
    ( 1, 1, 0, 0 ),    // 107, wind_update
    ( 6, 5, 0, 0 ),    // 108, wind_calc
    ( 0, 0, 0, 0 ),    // 109, wind_new
    ( 0, 1, 1, 0 ),    // 110, rsrc_load
    ( 0, 1, 0, 0 ),    // 111, rsrc_free
    ( 2, 1, 0, 1 ),    // 112, rsrc_gaddr
    ( 2, 1, 1, 0 ),    // 113, rsrc_saddr
    ( 1, 1, 1, 0 ),    // 114, rsrc_obfix
    ( 0, 0, 0, 0 ),    // 115, rsrc_rcfix (V4.0) !
    ( 0, 0, 0, 0 ),    // 116
    ( 0, 0, 0, 0 ),    // 117
    ( 0, 0, 0, 0 ),    // 118
    ( 0, 0, 0, 0 ),    // 119
    ( 0, 1, 2, 0 ),    // 120, shel_read
    ( 3, 1, 2, 0 ),    // 121, shel_write
    ( 1, 1, 1, 0 ),    // 122, shel_get
    ( 1, 1, 1, 0 ),    // 123, shel_put
    ( 0, 1, 1, 0 ),    // 124, shel_find
    ( 0, 1, 2, 0 ),    // 125, shel_envrn
    ( 0, 0, 0, 0 ),    // 126
    ( 0, 0, 0, 0 ),    // 127
    ( 0, 0, 0, 0 ),    // 128
    ( 0, 0, 0, 0 ),    // 129
    ( 1, 5, 0, 0 )     // 130, appl_getinfo (V4.0) !
  );

var
  _contrl: TAESContrl;
  _global: TAESGlobal;
  _intin: TAESIntIn;
  _intout: TAESIntOut;
  _addrin: TAESAddrIn;
  _addrout: TAESAddrOut;

const
  aespb: TAESPB = (
    contrl: @_contrl;
    global: @_global;
    intin: @_intin;
    intout: @_intout;
    addrin: @_addrin;
    addrout: @_addrout;
  );

function appl_exit: smallint;
begin
  appl_exit:=crys_if($13);
end;

function appl_read(ap_rid: smallint; ap_rlength: smallint; ap_rpbuff: pointer): smallint;
begin
  _intin[0]:=ap_rid;
  _intin[1]:=ap_rlength;
  _addrin[0]:=ap_rpbuff;

  appl_read:=crys_if($0b);
end;

function appl_write(ap_wid: smallint; ap_wlength: smallint; ap_wpbuff: pointer): smallint;
begin
  _intin[0]:=ap_wid;
  _intin[1]:=ap_wlength;
  _addrin[0]:=ap_wpbuff;

  appl_write:=crys_if($0c);
end;

function appl_find(fname: PChar): smallint;
begin
  _addrin[0]:=fname;
  appl_find:=crys_if($0d);
end;

function appl_init: smallint;
begin
  appl_init:=crys_if($0a);
end;


function evnt_keybd: smallint;
begin
  evnt_keybd:=crys_if($14);
end;

function evnt_button(ev_bclicks: smallint; ev_bmask: smallint; ev_bstate: smallint;
                     ev_bmx: psmallint; ev_bmy: psmallint; ev_bbutton: psmallint; ev_bkstate: psmallint): smallint;
begin
  _intin[0]:=ev_bclicks;
  _intin[1]:=ev_bmask;
  _intin[2]:=ev_bstate;

  crys_if($15);

  ev_bmx^:=_intout[1];
  ev_bmy^:=_intout[2];
  ev_bbutton^:=_intout[3];
  ev_bkstate^:=_intout[4];

  evnt_button:=_intout[0];
end;

function evnt_mouse(ev_moflags: smallint; ev_mox: smallint; ev_moy: smallint; ev_mowidth: smallint; ev_moheight: smallint;
                    ev_momx: psmallint; ev_momy: psmallint; ev_mobutton: psmallint; ev_mokstate: psmallint): smallint;
begin
  _intin[0]:=ev_moflags;
  _intin[1]:=ev_mox;
  _intin[2]:=ev_moy;
  _intin[3]:=ev_mowidth;
  _intin[4]:=ev_moheight;

  crys_if($16);

  ev_momx^:=_intout[1];
  ev_momy^:=_intout[2];
  ev_mobutton^:=_intout[3];
  ev_mokstate^:=_intout[4];

  evnt_mouse:=_intout[0];
end;

function evnt_mesag(msg: psmallint): smallint;
begin
  _addrin[0]:=msg;
  evnt_mesag:=crys_if($17);
end;

function evnt_timer(ev_tlocount: smallint; ev_thicount: smallint): smallint;
begin
  _intin[0]:=ev_tlocount;
  _intin[1]:=ev_thicount;

  evnt_timer:=crys_if($18);
end;

function evnt_multi(ev_mflags: smallint; ev_mbclicks: smallint; ev_mbmask: smallint; ev_mbstate: smallint;
                    ev_mm1flags: smallint; ev_mm1x: smallint; ev_mm1y: smallint; ev_mm1width: smallint; ev_mm1height: smallint;
                    ev_mm2flags: smallint; ev_mm2x: smallint; ev_mm2y: smallint; ev_mm2width: smallint; ev_mm2height: smallint;
                    ev_mmgpbuff: psmallint; ev_mtlocount: smallint; ev_mthicount: smallint;
                    ev_mmox: psmallint; ev_mmoy: psmallint; ev_mmbutton: psmallint; ev_mmokstate: psmallint;
                    ev_mkreturn: psmallint; ev_mbreturn: psmallint): smallint;
begin
  _intin[0]:=ev_mflags;
  _intin[1]:=ev_mbclicks;
  _intin[2]:=ev_mbmask;
  _intin[3]:=ev_mbstate;
  _intin[4]:=ev_mm1flags;
  _intin[5]:=ev_mm1x;
  _intin[6]:=ev_mm1y;
  _intin[7]:=ev_mm1width;
  _intin[8]:=ev_mm1height;
  _intin[9]:=ev_mm2flags;
  _intin[10]:=ev_mm2x;
  _intin[11]:=ev_mm2y;
  _intin[12]:=ev_mm2width;
  _intin[13]:=ev_mm2height;
  _intin[14]:=ev_mtlocount;
  _intin[15]:=ev_mthicount;
  _addrin[0]:=ev_mmgpbuff;

  crys_if($19);

  ev_mmox^:=_intout[1];
  ev_mmoy^:=_intout[2];
  ev_mmbutton^:=_intout[3];
  ev_mmokstate^:=_intout[4];
  ev_mkreturn^:=_intout[5];
  ev_mbreturn^:=_intout[6];

  evnt_multi:=_intout[0];
end;

function evnt_dclick(ev_dnew: smallint; ev_dgetset: smallint): smallint;
begin
  _intin[0]:=ev_dnew;
  _intin[1]:=ev_dgetset;

  evnt_dclick:=crys_if($1a);
end;


function menu_bar(me_btree: PAESOBJECT; me_bshow: smallint): smallint;
begin
  _intin[0]:=me_bshow;
  _addrin[0]:=me_btree;

  menu_bar:=crys_if($1e);
end;

function menu_icheck(me_ctree: PAESOBJECT; me_citem: smallint; me_ccheck: smallint): smallint;
begin
  _intin[0]:=me_citem;
  _intin[1]:=me_ccheck;
  _addrin[0]:=me_ctree;

  menu_icheck:=crys_if($1f);
end;

function menu_ienable(me_etree: PAESOBJECT; me_eitem: smallint; me_eenable: smallint): smallint;
begin
  _intin[0]:=me_eitem;
  _intin[1]:=me_eenable;
  _addrin[0]:=me_etree;

  menu_ienable:=crys_if($20);
end;

function menu_tnormal(me_ntree: PAESOBJECT; me_ntitle: smallint; me_nnormal: smallint): smallint;
begin
  _intin[0]:=me_ntitle;
  _intin[1]:=me_nnormal;
  _addrin[0]:=me_ntree;

  menu_tnormal:=crys_if($21);
end;

function menu_text(me_ttree: PAESOBJECT; me_titem: smallint; me_ttext: PChar): smallint;
begin
  _intin[0]:=me_titem;
  _addrin[0]:=me_ttree;
  _addrin[1]:=me_ttext;

  menu_text:=crys_if($22);
end;

function menu_register(me_rapid: smallint; me_rpstring: PChar): smallint;
begin
  _intin[0]:=me_rapid;
  _addrin[0]:=me_rpstring;

  menu_register:=crys_if($23);
end;


function objc_add(ob_atree: PAESOBJECT; ob_aparent: smallint; ob_achild: smallint): smallint;
begin
  _intin[0]:=ob_aparent;
  _intin[1]:=ob_achild;
  _addrin[0]:=ob_atree;

  objc_add:=crys_if($28);
end;

function objc_delete(ob_dltree: PAESOBJECT; ob_dlobject: smallint): smallint;
begin
  _intin[0]:=ob_dlobject;
  _addrin[0]:=ob_dltree;

  objc_delete:=crys_if($29);
end;

function objc_draw(ob_drtree: PAESOBJECT; ob_drstart: smallint;
                   ob_drdepth: smallint; ob_drxclip: smallint;
                   ob_dryclip: smallint; ob_drwclip: smallint;
                   ob_drhclip: smallint): smallint;
begin
  _intin[0]:=ob_drstart;
  _intin[1]:=ob_drdepth;
  _intin[2]:=ob_drxclip;
  _intin[3]:=ob_dryclip;
  _intin[4]:=ob_drwclip;
  _intin[5]:=ob_drhclip;
  _addrin[0]:=ob_drtree;

  objc_draw:=crys_if($2a);
end;

function objc_find(ob_ftree: PAESOBJECT; ob_fstartob: smallint;
                   ob_fdepth: smallint; ob_fmx: smallint;
                   ob_fmy: smallint): smallint;
begin
  _intin[0]:=ob_fstartob;
  _intin[1]:=ob_fdepth;
  _intin[2]:=ob_fmx;
  _intin[3]:=ob_fmy;
  _addrin[0]:=ob_ftree;

  objc_find:=crys_if($2b);
end;

function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     ob_ofxoff: psmallint; ob_ofyoff: psmallint): smallint;
begin
  _intin[0]:=ob_ofobject;
  _addrin[0]:=ob_oftree;

  crys_if($2c);

  ob_ofxoff^:=_intout[1];
  ob_ofyoff^:=_intout[2];

  objc_offset:=_intout[0];
end;

function objc_order(ob_ortree: PAESOBJECT; ob_orobject: smallint;
                    ob_ornewpos: smallint): smallint;
begin
  _intin[0]:=ob_orobject;
  _intin[1]:=ob_ornewpos;
  _addrin[0]:=ob_ortree;

  objc_order:=crys_if($2d);
end;

function objc_edit(ob_edtree: PAESOBJECT; ob_edobject: smallint;
                   ob_edchar: smallint; ob_edidx: psmallint;
                   ob_edkind: smallint): smallint;
begin
  _intin[0]:=ob_edobject;
  _intin[1]:=ob_edchar;
  _intin[2]:=ob_edidx^;
  _intin[3]:=ob_edkind;
  _addrin[0]:=ob_edtree;

  crys_if($2e);

  ob_edidx^:=_intout[1];
  objc_edit:=_intout[0];
end;

function objc_change(ob_ctree: PAESOBJECT; ob_cobject: smallint;
                     ob_cresvd: smallint; ob_cxclip: smallint;
                     ob_cyclip: smallint; ob_cwclip: smallint;
                     ob_chclip: smallint; ob_cnewstate: smallint;
                     ob_credraw: smallint): smallint;
begin
  _intin[0]:=ob_cobject;
  _intin[1]:=ob_cresvd;
  _intin[2]:=ob_cxclip;
  _intin[3]:=ob_cyclip;
  _intin[4]:=ob_cwclip;
  _intin[5]:=ob_chclip;
  _intin[6]:=ob_cnewstate;
  _intin[7]:=ob_credraw;

  _addrin[0]:=ob_ctree;

  objc_change:=crys_if($2f);
end;


function form_do(fo_dotree: PAESOBJECT; fo_dostartob: smallint): smallint;
begin
  _intin[0]:=fo_dostartob;
  _addrin[0]:=fo_dotree;

  form_do:=crys_if($32);
end;

function form_dial(fo_diflag: smallint; fo_dilittlx: smallint;
                   fo_dilittly: smallint; fo_dilittlw: smallint;
                   fo_dilittlh: smallint; fo_dibigx: smallint;
                   fo_dibigy: smallint; fo_dibigw: smallint;
                   fo_dibigh: smallint): smallint;
begin
  _intin[0]:=fo_diflag;
  _intin[1]:=fo_dilittlx;
  _intin[2]:=fo_dilittly;
  _intin[3]:=fo_dilittlw;
  _intin[4]:=fo_dilittlh;
  _intin[5]:=fo_dibigx;
  _intin[6]:=fo_dibigy;
  _intin[7]:=fo_dibigw;
  _intin[8]:=fo_dibigh;

  form_dial:=crys_if($33);
end;

function form_alert(default: smallint; alertstr: PChar): smallint;
begin
  _intin[0]:=default;
  _addrin[0]:=alertstr;
  form_alert:=crys_if($34);
end;

function form_error(error: smallint): smallint;
begin
  _intin[0]:=error;
  form_error:=crys_if($35);
end;

function form_center(fo_ctree: PAESOBJECT; fo_cx: psmallint;
                     fo_cy: psmallint; fo_cw: psmallint;
                     fo_ch: psmallint): smallint;
begin
  _addrin[0]:=fo_ctree;

  crys_if($36);

  fo_cx^:=_intout[1];
  fo_cy^:=_intout[2];
  fo_cw^:=_intout[3];
  fo_ch^:=_intout[4];

  form_center:=_intout[0];
end;

function form_keybd(fo_ktree: PAESOBJECT; fo_kobject: smallint;
                    fo_kobnext: smallint; fo_kchar: smallint;
                    fo_knxtobject: psmallint; fo_knxtchar: psmallint): smallint;
begin
  _intin[0]:=fo_kobject;
  _intin[1]:=fo_kchar;
  _intin[2]:=fo_kobnext;
  _addrin[0]:=fo_ktree;

  crys_if($37);

  fo_knxtobject^:=_intout[1];
  fo_knxtchar^:=_intout[2];

  form_keybd:=_intout[0];
end;

function form_button(fo_btree: PAESOBJECT; fo_bobject: smallint;
                     fo_bclicks: smallint; fo_bnxtobj: psmallint): smallint;
begin
  _intin[0]:=fo_bobject;
  _intin[1]:=fo_bclicks;
  _addrin[0]:=fo_btree;

  crys_if($38);

  fo_bnxtobj^:=_intout[1];

  form_button:=_intout[0];
end;


function graf_handle(gr_hwchar: psmallint; gr_hhchar: psmallint; gr_hwbox: psmallint; gr_hhbox: psmallint): smallint;
begin
  crys_if($4d);

  gr_hwchar^:=_intout[1];
  gr_hhchar^:=_intout[2];
  gr_hwbox^:=_intout[3];
  gr_hhbox^:=_intout[4];

  graf_handle:=_intout[0];
end;

function graf_mouse(gr_monumber: smallint; gr_mofaddr: PMFORM): smallint;
begin
  _intin[0]:=gr_monumber;
  _addrin[0]:=gr_mofaddr;

  graf_mouse:=crys_if($4e);
end;

function graf_mkstate(gr_mkmx: psmallint; gr_mkmy: psmallint;
                      gr_mkmstate: psmallint; gr_mkkstate: psmallint): smallint;
begin
  crys_if($4f);

  gr_mkmx^:=_intout[1];
  gr_mkmy^:=_intout[2];
  gr_mkmstate^:=_intout[3];
  gr_mkkstate^:=_intout[4];

  graf_mkstate:=_intout[0];
end;


function scrp_read(sc_rpscrap: pchar): smallint;
begin
  _addrin[0]:=sc_rpscrap;
  scrp_read:=crys_if($50);
end;

function scrp_write(sc_wpscrap: pchar): smallint;
begin
  _addrin[0]:=sc_wpscrap;
  scrp_write:=crys_if($51);
end;


function fsel_input(fs_iinpath: pchar; fs_iinsel: pchar; fs_iexbutton: psmallint): smallint;
begin
  _addrin[0]:=fs_iinpath;
  _addrin[1]:=fs_iinsel;

  crys_if($5a);

  fs_iexbutton^:=_intout[1];

  fsel_input:=_intout[0];
end;

function fsel_exinput(fs_einpath: pchar; fs_einsel: pchar; fs_eexbutton: psmallint; elabel: pchar): smallint;
begin
  _addrin[0]:=fs_einpath;
  _addrin[1]:=fs_einsel;
  _addrin[2]:=elabel;

  crys_if($5b);

  fs_eexbutton^:=_intout[1];

  fsel_exinput:=_intout[0];
end;


function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
begin
  _intin[0]:=kind;
  _intin[1]:=x;
  _intin[2]:=y;
  _intin[3]:=w;
  _intin[4]:=h;
  wind_create:=crys_if($64);
end;

function wind_open(handle: smallint; x, y, w, h: smallint): smallint;
begin
  _intin[0]:=handle;
  _intin[1]:=x;
  _intin[2]:=y;
  _intin[3]:=w;
  _intin[4]:=h;
  wind_open:=crys_if($65);
end;

function wind_close(wi_clhandle: smallint): smallint;
begin
  _intin[0]:=wi_clhandle;
  wind_close:=crys_if($66);
end;

function wind_delete(handle: smallint): smallint;
begin
  _intin[0]:=handle;
  wind_delete:=crys_if($67);
end;

function wind_get(wi_ghandle: smallint; wi_gfield: smallint;
                  wi_gw1: psmallint; wi_gw2: psmallint;
                  wi_gw3: psmallint; wi_gw4: psmallint): smallint;
begin
  _intin[0]:=wi_ghandle;
  _intin[1]:=wi_gfield;

  crys_if($68);

  wi_gw1^:=_intout[1];
  wi_gw2^:=_intout[2];
  wi_gw3^:=_intout[3];
  wi_gw4^:=_intout[4];

  wind_get:=_intout[0];
end;

function wind_set(wi_shandle: smallint; wi_sfield: smallint;
                  wi_sw1: smallint; wi_sw2: smallint;
                  wi_sw3: smallint; wi_sw4: smallint): smallint;
begin
  _intin[0]:=wi_shandle;
  _intin[1]:=wi_sfield;
  _intin[2]:=wi_sw1;
  _intin[3]:=wi_sw2;
  _intin[4]:=wi_sw3;
  _intin[5]:=wi_sw4;

  wind_set:=crys_if($69);
end;

function wind_find(wi_fmx: smallint; wi_fmy: smallint): smallint;
begin
  _intin[0]:=wi_fmx;
  _intin[1]:=wi_fmy;

  wind_find:=crys_if($6a);
end;

function wind_update(wi_ubegend: smallint): smallint;
begin
  _intin[0]:=wi_ubegend;
  wind_update:=crys_if($6b);
end;

function wind_calc(wi_ctype: smallint; wi_ckind: smallint;
                   wi_cinx: smallint; wi_ciny: smallint;
                   wi_cinw: smallint; wi_cinh: smallint;
                   coutx: psmallint; couty: psmallint;
                   coutw: psmallint; couth: psmallint): smallint;
begin
  _intin[0]:=wi_ctype;
  _intin[1]:=wi_ckind;
  _intin[2]:=wi_cinx;
  _intin[3]:=wi_ciny;
  _intin[4]:=wi_cinw;
  _intin[5]:=wi_cinh;

  crys_if($6c);

  coutx^:=_intout[1];
  couty^:=_intout[2];
  coutw^:=_intout[3];
  couth^:=_intout[4];

  wind_calc:=_intout[0];
end;

procedure wind_new;
begin
  crys_if($6d);
end;


function rsrc_load(re_lpfname: PChar): smallint;
begin
  _addrin[0]:=re_lpfname;
  rsrc_load:=crys_if($6e);
end;

function rsrc_free: smallint;
begin
  rsrc_free:=crys_if($6f);
end;

function rsrc_gaddr(re_gtype: smallint; re_gindex: smallint; gaddr: ppointer): smallint;
begin
  _intin[0]:=re_gtype;
  _intin[1]:=re_gindex;

  crys_if($70);

  gaddr^:=_addrout[0];

  rsrc_gaddr:=_intout[0];
end;

function rsrc_saddr(re_stype: smallint; re_sindex: smallint; saddr: pointer): smallint;
begin
  _intin[0]:=re_stype;
  _intin[1]:=re_sindex;
  _addrin[0]:=saddr;

  rsrc_saddr:=crys_if($71);
end;

function rsrc_obfix(re_otree: PAESOBJECT; re_oobject: smallint): smallint;
begin
  _intin[0]:=re_oobject;
  _addrin[0]:=re_otree;

  rsrc_obfix:=crys_if($72);
end;


function shel_read(sh_rpcmd: pchar; sh_rptail: pchar): smallint;
begin
  _addrin[0]:=sh_rpcmd;
  _addrin[1]:=sh_rptail;

   shel_read:=crys_if($78);
end;

function shel_write(sh_wdoex: smallint; sh_wisgr: smallint;
                    sh_wiscr: smallint; sh_wpcmd: pchar;
                    sh_wptail: pchar): smallint;
begin
  _intin[0]:=sh_wdoex;
  _intin[1]:=sh_wisgr;
  _intin[2]:=sh_wiscr;
  _addrin[0]:=sh_wpcmd;
  _addrin[1]:=sh_wptail;

  shel_write:=crys_if($79);
end;

function shel_get(sh_gaddr: pchar; sh_glen: word): smallint;
begin
  _intin[0]:=smallint(sh_glen);
  _addrin[0]:=sh_gaddr;

  shel_get:=crys_if($7a);
end;

function shel_put(sh_paddr: pchar; sh_plen: word): smallint;
begin
  _intin[0]:=smallint(sh_plen);
  _addrin[0]:=sh_paddr;

  shel_put:=crys_if($7b);
end;

function shel_find(sh_fpbuff: pchar): smallint;
begin
  _addrin[0]:=sh_fpbuff;

  shel_find:=crys_if($7c);
end;

function shel_envrn(sh_epvalue: ppchar; sh_eparm: pchar): smallint;
begin
  _addrin[0]:=sh_epvalue;
  _addrin[1]:=sh_eparm;

  shel_envrn:=crys_if($7d);
end;


function crys_if(_opcode: dword): smallint;
begin
  with _contrl do
    begin
      opcode:=_opcode;
      nums:=ops_table[_opcode-10];
    end;
  asm
    lea.l       aespb, a0
    move.l      a0, d1
    move.w      #AES_TRAP_MAGIC, d0
    trap        #2
  end;
  crys_if:=_intout[0];
end;


end.
