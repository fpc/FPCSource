{
    Copyright (c) 2016 by Free Pascal development team

    AES interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE FPC}
{$MODESWITCH OUT+}
{$PACKRECORDS 2}

unit aes;

interface

uses gemcmmn;

{ The API description of this file is based on the information available
  online at: https://freemint.github.io/tos.hyp/en/index.html }

{$I aestypes.inc}

type
   ARRAY_8 = gemcmmn.ARRAY_8;
   PMFORM = gemcmmn.PMFORM;
   TMFORM = gemcmmn.TMFORM;

function appl_exit: smallint;
function appl_read(ap_rid: smallint; ap_rlength: smallint; ap_rpbuff: pointer): smallint;
function appl_write(ap_wid: smallint; ap_wlength: smallint; ap_wpbuff: pointer): smallint;
function appl_find(fname: PAnsiChar): smallint;
function appl_find(ap_fpname: String): smallint;
function appl_tplay(ap_tpmem: Pointer; ap_tpnum, ap_tpscale: smallint): smallint;
function appl_trecord(ap_trmem: Pointer; ap_trcount: smallint): smallint;
function appl_bvset(ap_bvdisk, ap_bvhard: Word): smallint;
function appl_yield: smallint;
procedure _appl_yield;
function appl_search(ap_smode: smallint; ap_sname: PAnsiChar; out ap_stype, ap_sid: smallint): smallint;
function appl_search(ap_smode: smallint; out ap_sname: String; out ap_stype, ap_sid: smallint): smallint;
function appl_getinfo(ap_gtype: smallint; out ap_gout1, ap_gout2, ap_gout3, ap_gout4: smallint): smallint;
function appl_init: smallint;

function evnt_keybd: smallint;
function evnt_button(ev_bclicks: smallint; ev_bmask: smallint; ev_bstate: smallint;
                     ev_bmx: psmallint; ev_bmy: psmallint; ev_bbutton: psmallint; ev_bkstate: psmallint): smallint;
function evnt_button(ev_bclicks, ev_bmask, ev_bstate: smallint; out ev_bmx, ev_bmy, ev_bbutton, ev_bkstate: smallint): smallint;
function evnt_mouse(ev_moflags: smallint; ev_mox: smallint; ev_moy: smallint; ev_mowidth: smallint; ev_moheight: smallint;
                    ev_momx: psmallint; ev_momy: psmallint; ev_mobutton: psmallint; ev_mokstate: psmallint): smallint;
function evnt_mouse(ev_moflags, ev_mox, ev_moy, ev_mowidth, ev_moheight: smallint;
    out ev_momx, ev_momy, ev_mobutton, ev_mokstate: smallint): smallint;
function evnt_mesag(msg: psmallint): smallint;
function evnt_mesag(out ev_mgpbuff: ARRAY_8): smallint;
function evnt_timer(ev_tlocount: smallint; ev_thicount: smallint): smallint;
function evnt_multi(ev_mflags: smallint; ev_mbclicks: smallint; ev_mbmask: smallint; ev_mbstate: smallint;
                    ev_mm1flags: smallint; ev_mm1x: smallint; ev_mm1y: smallint; ev_mm1width: smallint; ev_mm1height: smallint;
                    ev_mm2flags: smallint; ev_mm2x: smallint; ev_mm2y: smallint; ev_mm2width: smallint; ev_mm2height: smallint;
                    ev_mmgpbuff: psmallint; ev_mtlocount: smallint; ev_mthicount: smallint;
                    ev_mmox: psmallint; ev_mmoy: psmallint; ev_mmbutton: psmallint; ev_mmokstate: psmallint;
                    ev_mkreturn: psmallint; ev_mbreturn: psmallint): smallint;
function evnt_multi(ev_mflags, ev_mbclicks, ev_mbmask,
        ev_mbstate, ev_mm1flags, ev_mm1x,
        ev_mm1y, ev_mm1width, ev_mm1height,
        ev_mm2flags, ev_mm2x, ev_mm2y,
        ev_mm2width, ev_mm2height: smallint;
        out ev_mmgpbuf: ARRAY_8;
        ev_mtlocount, ev_mthicount: smallint;
        out ev_mmox, ev_mmoy, ev_mmobutton,
        ev_mmokstate, ev_mkreturn,
        ev_mbreturn: smallint): smallint;
function EvntMulti(var evnt_struct: TEVENT): smallint;
function evnt_dclick(ev_dnew: smallint; ev_dgetset: smallint): smallint;

function menu_bar(me_btree: PAESOBJECT; me_bshow: smallint): smallint; overload;
function menu_icheck(me_ctree: PAESOBJECT; me_citem: smallint; me_ccheck: smallint): smallint; overload;
function menu_ienable(me_etree: PAESOBJECT; me_eitem: smallint; me_eenable: smallint): smallint; overload;
function menu_tnormal(me_ntree: PAESOBJECT; me_ntitle: smallint; me_nnormal: smallint): smallint; overload;
function menu_text(me_ttree: PAESOBJECT; me_titem: smallint; me_ttext: PAnsiChar): smallint; overload;
function menu_register(me_rapid: smallint; me_rpstring: PAnsiChar): smallint;
function menu_register(me_rapid: smallint; me_rpstring: String): smallint;
function menu_unregister(me_uapid: smallint): smallint;
function menu_popup(me_menu: PMENU; me_xpos, me_ypos: smallint; var me_mdata: TMENU): smallint;
function menu_attach(me_flag: smallint; me_tree: PAESOBJECT; me_item: smallint; me_mdata: PMENU): smallint; overload;
function menu_istart(me_flag: smallint; me_tree: PAESOBJECT; me_imenu, me_item: smallint): smallint; overload;
function menu_settings(me_flag: smallint; me_values: PMN_SET): smallint;
function menu_click(val: smallint; setit: smallint): smallint;

function objc_add(ob_atree: PAESOBJECT; ob_aparent: smallint; ob_achild: smallint): smallint; overload;
function objc_delete(ob_dltree: PAESOBJECT; ob_dlobject: smallint): smallint; overload;
function objc_draw(ob_drtree: PAESOBJECT; ob_drstart: smallint;
                   ob_drdepth: smallint; ob_drxclip: smallint;
                   ob_dryclip: smallint; ob_drwclip: smallint;
                   ob_drhclip: smallint): smallint; overload;
function objc_find(ob_ftree: PAESOBJECT; ob_fstartob: smallint;
                   ob_fdepth: smallint; ob_fmx: smallint;
                   ob_fmy: smallint): smallint; overload;
function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     ob_ofxoff: psmallint; ob_ofyoff: psmallint): smallint; overload;
function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     out ob_ofxoff, ob_ofyoff: smallint): smallint; overload;
function objc_order(ob_ortree: PAESOBJECT; ob_orobject: smallint;
                    ob_ornewpos: smallint): smallint; overload;
function objc_edit(ob_edtree: PAESOBJECT; ob_edobject: smallint;
                   ob_edchar: smallint; ob_edidx: psmallint;
                   ob_edkind: smallint): smallint; overload;
function objc_edit(ob_edtree: PAESOBJECT; ob_edobject, ob_edchar: smallint;
        var ob_edidx: smallint; ob_edkind: smallint): smallint; overload;
function objc_change(ob_ctree: PAESOBJECT; ob_cobject: smallint;
                     ob_cresvd: smallint; ob_cxclip: smallint;
                     ob_cyclip: smallint; ob_cwclip: smallint;
                     ob_chclip: smallint; ob_cnewstate: smallint;
                     ob_credraw: smallint): smallint; overload;
function objc_sysvar(ob_svmode, ob_svwhich, ob_svinval1, ob_svinval2: smallint;
                    out ob_svoutval1, ob_svoutval2: smallint): smallint;

function form_do(fo_dotree: PAESOBJECT; fo_dostartob: smallint): smallint; overload;
function form_dial(fo_diflag: smallint; fo_dilittlx: smallint;
                   fo_dilittly: smallint; fo_dilittlw: smallint;
                   fo_dilittlh: smallint; fo_dibigx: smallint;
                   fo_dibigy: smallint; fo_dibigw: smallint;
                   fo_dibigh: smallint): smallint;
function form_alert(fo_adefbttn: smallint; alertstr: PAnsiChar): smallint;
function form_alert(fo_adefbttn: smallint; fo_astring: String): smallint;
function form_error(error: smallint): smallint;
function form_center(fo_ctree: PAESOBJECT; fo_cx: psmallint;
                     fo_cy: psmallint; fo_cw: psmallint;
                     fo_ch: psmallint): smallint; overload;
function form_center(fo_ctree: PAESOBJECT; out fo_cx, fo_cy, fo_cw, fo_ch: smallint): smallint; overload;
function form_keybd(fo_ktree: PAESOBJECT; fo_kobject: smallint;
                    fo_kobnext: smallint; fo_kchar: smallint;
                    fo_knxtobject: psmallint; fo_knxtchar: psmallint): smallint; overload;
function form_keybd(fo_ktree: PAESOBJECT; fo_kobject, fo_kobnext, fo_kchar: smallint;
        out fo_knxtobject, fo_knxtchar: smallint): smallint; overload;
function form_button(fo_btree: PAESOBJECT; fo_bobject: smallint;
                     fo_bclicks: smallint; fo_bnxtobj: psmallint): smallint; overload;
function form_button(fo_btree: PAESOBJECT; fo_bobject, fo_bclicks: smallint;
        out fo_bnxtobj: smallint): smallint; overload;

function graf_rubbox(gr_rx, gr_ry, gr_rminwidth, gr_rminheight: smallint;
        out gr_rlastwidth, gr_rlastheight: smallint): smallint;
function graf_rubberbox(gr_rx, gr_ry, gr_rminwidth, gr_rminheight: smallint;
        out gr_rlastwidth, gr_rlastheight: smallint): smallint; external name 'graf_rubberbox';
function graf_dragbox(gr_dwidth, gr_dheight, gr_dstartx, gr_dstarty,
                      gr_dboundx, gr_dboundy, gr_dboundw, gr_dboundh: smallint;
              out gr_dfinishx, gr_dfinishy: smallint): smallint;
function graf_mbox(gr_mwidth, gr_mheight, gr_msourcex, gr_msourcey, gr_mdestx, gr_mdesty: smallint): smallint;
function graf_movebox(gr_mwidth, gr_mheight, gr_msourcex, gr_msourcey, gr_mdestx, gr_mdesty: smallint): smallint; external name 'graf_movebox';
function graf_growbox(gr_gstx, gr_gsty, gr_gstwidth, gr_gstheight,
                      gr_gfinx, gr_gfiny, gr_gfinwidth, gr_gfinheight :smallint): smallint;
function graf_shrinkbox(gr_sfinx, gr_sfiny, gr_sfinwidth, gr_sfinheight,
                        gr_sstx, gr_ssty, gr_sstwidth, gr_sstheight: smallint): smallint;
function graf_watchbox(gr_wptree: PAESOBJECT; gr_wobject, gr_winstate, gr_woutstate: smallint): smallint;
function graf_slidebox(gr_slptree: PAESOBJECT; gr_slparent, gr_slobject, gr_slvh: smallint): smallint;
function graf_handle(gr_hwchar: psmallint; gr_hhchar: psmallint; gr_hwbox: psmallint; gr_hhbox: psmallint): smallint;
function graf_handle(out gr_hwchar, gr_hhchar, gr_hwbox, gr_hhbox: smallint): smallint;
function graf_mouse(gr_monumber: smallint; gr_mofaddr: PMFORM): smallint;
function graf_mkstate(gr_mkmx: psmallint; gr_mkmy: psmallint;
                      gr_mkmstate: psmallint; gr_mkkstate: psmallint): smallint;
function graf_mkstate(out gr_mkmx, gr_mkmy, gr_mkmstate, gr_mkkstate: smallint): smallint;

function scrp_read(sc_rpscrap: PAnsiChar): smallint;
function scrp_read(out sc_rpscrap: String): smallint;
function scrp_write(sc_wpscrap: PAnsiChar): smallint;
function scrp_write(const sc_wpscrap: String): smallint;

function fsel_input(fs_iinpath: PAnsiChar; fs_iinsel: PAnsiChar; fs_iexbutton: psmallint): smallint;
function fsel_input(var fs_iinpath, fs_iinsel: String; out fs_iexbutton: smallint): smallint;
function fsel_exinput(fs_einpath: PAnsiChar; fs_einsel: PAnsiChar; fs_eexbutton: psmallint; elabel: PAnsiChar): smallint;
function fsel_exinput(var fs_einpath, fs_einsel: String; out fs_eexbutton: smallint;
        const fs_elabel: String): smallint;

function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
function wind_open(handle: smallint; x, y, w, h: smallint): smallint;
function wind_close(wi_clhandle: smallint): smallint;
function wind_delete(handle: smallint): smallint;
function wind_get(wi_ghandle: smallint; wi_gfield: smallint;
                  wi_gw1: psmallint; wi_gw2: psmallint;
                  wi_gw3: psmallint; wi_gw4: psmallint): smallint;
function wind_get(wi_ghandle, wi_gfield: smallint;
        out wi_gw1, wi_gw2, wi_gw3, wi_gw4: smallint): smallint;
function wind_get(wi_ghandle: smallint; wi_gfield: smallint; gr: PGRECT): smallint;
function wind_set(wi_shandle: smallint; wi_sfield: smallint;
                  wi_sw1: smallint; wi_sw2: smallint;
                  wi_sw3: smallint; wi_sw4: smallint): smallint;
function wind_set(wi_shandle: smallint; wi_sfield: smallint; ptr: Pointer): smallint;
function wind_set(wi_shandle: smallint; wi_sfield: smallint; gr: PGRECT): smallint;
function wind_find(wi_fmx: smallint; wi_fmy: smallint): smallint;
function wind_update(wi_ubegend: smallint): smallint;
function wind_calc(wi_ctype: smallint; wi_ckind: smallint;
                   wi_cinx: smallint; wi_ciny: smallint;
                   wi_cinw: smallint; wi_cinh: smallint;
                   coutx: psmallint; couty: psmallint;
                   coutw: psmallint; couth: psmallint): smallint;
function wind_calc(wi_ctype, wi_ckind, wi_cinx, wi_ciny, wi_cinw, wi_cinh : smallint;
           out wi_coutx, wi_couty, wi_coutw, wi_couth: smallint): smallint;
procedure wind_new;

function rsrc_load(re_lpfname: PAnsiChar): smallint;
function rsrc_load(re_lpfname: String): smallint;
function rsrc_free: smallint;
function rsrc_gaddr(re_gtype: smallint; re_gindex: smallint; gaddr: ppointer): smallint;
function rsrc_gaddr(re_gtype, re_gindex: smallint; out re_gaddr: Pointer): smallint;
function rsrc_saddr(re_stype: smallint; re_sindex: smallint; saddr: pointer): smallint;
function rsrc_obfix(re_otree: PAESOBJECT; re_oobject: smallint): smallint; overload;
function rsrc_rcfix(rc_header: PRSHDR): smallint;

function shel_read(sh_rpcmd: PAnsiChar; sh_rptail: PAnsiChar): smallint;
function shel_read(out sh_rpcmd, sh_rptail: String): smallint;
function shel_write(sh_wdoex: smallint; sh_wisgr: smallint;
                    sh_wiscr: smallint; sh_wpcmd: PAnsiChar;
                    sh_wptail: PAnsiChar): smallint;
function shel_write(sh_wdoex, sh_wisgr, sh_wiscr: smallint;
            const sh_wpcmd, sh_wptail: String): smallint;
function shel_get(sh_gaddr: PAnsiChar; sh_glen: word): smallint;
function shel_put(sh_paddr: PAnsiChar; sh_plen: word): smallint;
function shel_find(sh_fpbuff: PAnsiChar): smallint;
function shel_find(var sh_fpbuff: String): smallint;
function shel_envrn(sh_epvalue: PPAnsiChar; sh_eparm: PAnsiChar): smallint;
function shel_envrn(out sh_epvalue: Pointer; const sh_eparm: String): smallint;
function shel_rdef(out sh_rlpcmd, sh_rlpdir: String): smallint;
function shel_wdef(const sh_wlpcmd, sh_wlpdir: String): smallint;

function xgrf_stepcalc(xg_storgw, xg_storgh, xg_stxc, xg_styc, xg_stw, xg_sth : smallint;
            out xg_stpcx, xg_stpcy, xg_stpcnt, xg_stpxstep, xg_stpystep: smallint): smallint;
function xgrf_2box(xg_2cnt, xg_2xstep, xg_2ystep, xg_2doubled,
                   xg_2corners, xg_2xc, xg_2yc, xg_2w, xg_2h: smallint): smallint;

function crys_if(_opcode: dword): smallint;
function vq_aes: smallint;
procedure _crystal(pb: PAESPB);


(*
 * NOT YET IMPLEMENTED:
appl_control
objc_xfind
objc_xfind
objc_wchange
objc_wdraw
objc_wedit
objc_xedit
form_wkeybd
form_wbutton
form_popup
form_xdial
form_xdo
form_xerr
xfrm_popup
graf_multirubber
fsel_boxinput
wind_xget
wind_xset
shel_help
xgrf_rbox

wdgl_*
lbox_*
fnts_*
fslx_*
pdlg_*
edit_*

Geneva functions

*)


implementation

type
  aesstr = array[0..255] of AnsiChar;

const
  ops_table: array[0..120,0..3] of SmallInt = (
    ( 0, 1, 0, 0 ),    // 10, appl_init
    ( 2, 1, 1, 0 ),    // 11, appl_read
    ( 2, 1, 1, 0 ),    // 12, appl_write
    ( 0, 1, 1, 0 ),    // 13, appl_find
    ( 2, 1, 1, 0 ),    // 14, appl_tplay
    ( 1, 1, 1, 0 ),    // 15, appl_trecord
    ( 2, 1, 0, 0 ),    // 16, appl_bvset
    ( 0, 0, 0, 0 ),    // 17, appl_yield
    ( 1, 3, 1, 0 ),    // 18, appl_search (V4.0)
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
    ( 2, 1, 2, 0 ),    // 36, menu_unregister/menu_popup (V3.3)
    ( 2, 1, 2, 0 ),    // 37, menu_attach/menu_click (V3.3)
    ( 3, 1, 1, 0 ),    // 38, menu_istart (V3.3)
    ( 1, 1, 1, 0 ),    // 39, menu_settings (V3.3)
    ( 2, 1, 1, 0 ),    // 40, objc_add
    ( 1, 1, 1, 0 ),    // 41, objc_delete
    ( 6, 1, 1, 0 ),    // 42, objc_draw
    ( 4, 1, 1, 0 ),    // 43, objc_find
    ( 1, 3, 1, 0 ),    // 44, objc_offset
    ( 2, 1, 1, 0 ),    // 45, objc_order
    ( 4, 2, 1, 0 ),    // 46, objc_edit
    ( 8, 1, 1, 0 ),    // 47, objc_change
    ( 4, 3, 0, 0 ),    // 48, objc_sysvar (V3.4)
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
    ( 4, 3, 0, 0 ),    // 70, graf_rubberbox
    ( 8, 3, 0, 0 ),    // 71, graf_dragbox
    ( 6, 1, 0, 0 ),    // 72, graf_movebox
    ( 8, 1, 0, 0 ),    // 73, graf_growbox
    ( 8, 1, 0, 0 ),    // 74, graf_shrinkbox
    ( 4, 1, 1, 0 ),    // 75, graf_watchbox
    ( 3, 1, 1, 0 ),    // 76, graf_slidebox
    ( 0, 5, 0, 0 ),    // 77, graf_handle
    ( 1, 1, 1, 0 ),    // 78, graf_mouse
    ( 0, 5, 0, 0 ),    // 79, graf_mkstate
    ( 0, 1, 1, 0 ),    // 80, scrp_read
    ( 0, 1, 1, 0 ),    // 81, scrp_write
    ( 0, 1, 0, 0 ),    // 82, scrp_clear
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
    ( 0, 1, 1, 0 ),    // 115, rsrc_rcfix (V4.0)
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
    ( 0, 1, 2, 0 ),    // 126, shel_rdef
    ( 0, 1, 2, 0 ),    // 127, shel_wdef
    ( 1, 1, 2, 0 ),    // 128, shel_help
    ( 2, 1, 1, 0 ),    // 129, appl_control
    ( 1, 5, 0, 0 )     // 130, appl_getinfo (V4.0)
  );

var
  _global: TAESGlobal; public name 'aes_global';

var
  _contrl: TAESContrl;
  _intin: TAESIntIn;
  _intout: TAESIntOut;
  _addrin: TAESAddrIn;
  _addrout: TAESAddrOut;

const
  aespb: TAESPB = (
    control: @_contrl;
    global: @_global;
    intin: @_intin;
    intout: @_intout;
    addrin: @_addrin;
    addrout: @_addrout;
  ); public name 'aespb';

var menu_register_cstr: aesstr;

function appl_init: smallint;
begin
  appl_init:=crys_if(10);
end;

function appl_read(ap_rid: smallint; ap_rlength: smallint; ap_rpbuff: pointer): smallint;
begin
  _intin[0]:=ap_rid;
  _intin[1]:=ap_rlength;
  _addrin[0]:=ap_rpbuff;

  appl_read:=crys_if(11);
end;

function appl_write(ap_wid: smallint; ap_wlength: smallint; ap_wpbuff: pointer): smallint;
begin
  _intin[0]:=ap_wid;
  _intin[1]:=ap_wlength;
  _addrin[0]:=ap_wpbuff;

  appl_write:=crys_if(12);
end;

function appl_find(fname: PAnsiChar): smallint;
begin
  _addrin[0]:=fname;
  appl_find:=crys_if(13);
end;

function appl_find(ap_fpname: String): smallint;
var s: aesstr;
begin
  s := ap_fpname;
  _addrin[0]:=@s;
  appl_find:=crys_if(13);
end;

function appl_tplay(ap_tpmem: Pointer; ap_tpnum, ap_tpscale: smallint): smallint;
begin
  _intin[0]:=ap_tpnum;
  _intin[1]:=ap_tpscale;
  _addrin[0]:=ap_tpmem;
  appl_tplay:=crys_if(14);
end;

function appl_trecord(ap_trmem: Pointer; ap_trcount: smallint): smallint;
begin
  _intin[0]:=ap_trcount;
  _addrin[0]:=ap_trmem;
  appl_trecord:=crys_if(15);
end;

function appl_bvset(ap_bvdisk, ap_bvhard: Word): smallint;
begin
  _intin[0]:=ap_bvdisk;
  _intin[1]:=ap_bvhard;
  appl_bvset:=crys_if(16);
end;

function appl_yield: smallint;
begin
  appl_yield:=crys_if(17);
end;

procedure _appl_yield; assembler; nostackframe;
asm
    pea.l       (a2)
    move.w      #$c9,d0
    trap        #2
    movea.l     (a7)+,a2
end;

function appl_search(ap_smode: smallint; ap_sname: PAnsiChar; out ap_stype, ap_sid: smallint): smallint;
begin
  _intin[0]:=ap_smode;
  _addrin[0]:=ap_sname;
  appl_search:=crys_if(18);
  ap_stype:=_intout[1];
  ap_sid:=_intout[2];
end;

function appl_search(ap_smode: smallint; out ap_sname: String; out ap_stype, ap_sid: smallint): smallint;
var s: aesstr;
begin
  _intin[0]:=ap_smode;
  _addrin[0]:=@s[0];
  appl_search:=crys_if(18);
  ap_sname:=PAnsiChar(@s[0]);
  ap_stype:=_intout[1];
  ap_sid:=_intout[2];
end;

function appl_exit: smallint;
begin
  appl_exit:=crys_if(19);
end;

function appl_getinfo(ap_gtype: smallint; out ap_gout1, ap_gout2, ap_gout3, ap_gout4: smallint): smallint;
begin
  _intin[0]:=ap_gtype;
  appl_getinfo:=crys_if(130);
  ap_gout1:=_intout[1];
  ap_gout2:=_intout[2];
  ap_gout3:=_intout[3];
  ap_gout4:=_intout[4];
end;


function evnt_keybd: smallint;
begin
  evnt_keybd:=crys_if(20);
end;

function evnt_button(ev_bclicks: smallint; ev_bmask: smallint; ev_bstate: smallint;
                     ev_bmx: psmallint; ev_bmy: psmallint; ev_bbutton: psmallint; ev_bkstate: psmallint): smallint;
begin
  _intin[0]:=ev_bclicks;
  _intin[1]:=ev_bmask;
  _intin[2]:=ev_bstate;

  crys_if(21);

  ev_bmx^:=_intout[1];
  ev_bmy^:=_intout[2];
  ev_bbutton^:=_intout[3];
  ev_bkstate^:=_intout[4];

  evnt_button:=_intout[0];
end;

function evnt_button(ev_bclicks, ev_bmask, ev_bstate: smallint; out ev_bmx, ev_bmy, ev_bbutton, ev_bkstate: smallint): smallint;
begin
  _intin[0]:=ev_bclicks;
  _intin[1]:=ev_bmask;
  _intin[2]:=ev_bstate;

  crys_if(21);

  ev_bmx:=_intout[1];
  ev_bmy:=_intout[2];
  ev_bbutton:=_intout[3];
  ev_bkstate:=_intout[4];

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

  crys_if(22);

  ev_momx^:=_intout[1];
  ev_momy^:=_intout[2];
  ev_mobutton^:=_intout[3];
  ev_mokstate^:=_intout[4];

  evnt_mouse:=_intout[0];
end;

function evnt_mouse(ev_moflags, ev_mox, ev_moy, ev_mowidth, ev_moheight: smallint;
    out ev_momx, ev_momy, ev_mobutton, ev_mokstate: smallint): smallint;
begin
  _intin[0]:=ev_moflags;
  _intin[1]:=ev_mox;
  _intin[2]:=ev_moy;
  _intin[3]:=ev_mowidth;
  _intin[4]:=ev_moheight;

  crys_if(22);

  ev_momx:=_intout[1];
  ev_momy:=_intout[2];
  ev_mobutton:=_intout[3];
  ev_mokstate:=_intout[4];

  evnt_mouse:=_intout[0];
end;

function evnt_mesag(msg: psmallint): smallint;
begin
  _addrin[0]:=msg;
  evnt_mesag:=crys_if(23);
end;

function evnt_mesag(out ev_mgpbuff: ARRAY_8): smallint;
begin
  _addrin[0]:=@ev_mgpbuff;
  evnt_mesag:=crys_if(23);
end;

function evnt_timer(ev_tlocount: smallint; ev_thicount: smallint): smallint;
begin
  _intin[0]:=ev_tlocount;
  _intin[1]:=ev_thicount;

  evnt_timer:=crys_if(24);
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

  crys_if(25);

  ev_mmox^:=_intout[1];
  ev_mmoy^:=_intout[2];
  ev_mmbutton^:=_intout[3];
  ev_mmokstate^:=_intout[4];
  ev_mkreturn^:=_intout[5];
  ev_mbreturn^:=_intout[6];

  evnt_multi:=_intout[0];
end;

function evnt_multi(ev_mflags, ev_mbclicks, ev_mbmask,
        ev_mbstate, ev_mm1flags, ev_mm1x,
        ev_mm1y, ev_mm1width, ev_mm1height,
        ev_mm2flags, ev_mm2x, ev_mm2y,
        ev_mm2width, ev_mm2height: smallint;
        out ev_mmgpbuf: ARRAY_8;
        ev_mtlocount, ev_mthicount: smallint;
        out ev_mmox, ev_mmoy, ev_mmobutton,
        ev_mmokstate, ev_mkreturn,
        ev_mbreturn: smallint): smallint;
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
  _addrin[0]:=@ev_mmgpbuf;

  crys_if(25);

  ev_mmox:=_intout[1];
  ev_mmoy:=_intout[2];
  ev_mmobutton:=_intout[3];
  ev_mmokstate:=_intout[4];
  ev_mkreturn:=_intout[5];
  ev_mbreturn:=_intout[6];

  evnt_multi:=_intout[0];
end;

function EvntMulti(var evnt_struct: TEVENT): smallint;
begin
  _intin[0]:=evnt_struct.ev_mflags;
  _intin[1]:=evnt_struct.ev_mbclicks;
  _intin[2]:=evnt_struct.ev_bmask;
  _intin[3]:=evnt_struct.ev_mbstate;
  _intin[4]:=evnt_struct.ev_mm1flags;
  _intin[5]:=evnt_struct.ev_mm1x;
  _intin[6]:=evnt_struct.ev_mm1y;
  _intin[7]:=evnt_struct.ev_mm1width;
  _intin[8]:=evnt_struct.ev_mm1height;
  _intin[9]:=evnt_struct.ev_mm2flags;
  _intin[10]:=evnt_struct.ev_mm2x;
  _intin[11]:=evnt_struct.ev_mm2y;
  _intin[12]:=evnt_struct.ev_mm2width;
  _intin[13]:=evnt_struct.ev_mm2height;
  _intin[14]:=evnt_struct.ev_mtlocount;
  _intin[15]:=evnt_struct.ev_mthicount;
  _addrin[0]:=@evnt_struct.ev_mmgpbuf;

  crys_if(25);

  evnt_struct.ev_mwich:=_intout[0];
  evnt_struct.ev_mmox:=_intout[1];
  evnt_struct.ev_mmoy:=_intout[2];
  evnt_struct.ev_mmobutton:=_intout[3];
  evnt_struct.ev_mmokstate:=_intout[4];
  evnt_struct.ev_mkreturn:=_intout[5];
  evnt_struct.ev_mbreturn:=_intout[6];

  EvntMulti:=_intout[0];
end;

function evnt_dclick(ev_dnew: smallint; ev_dgetset: smallint): smallint;
begin
  _intin[0]:=ev_dnew;
  _intin[1]:=ev_dgetset;

  evnt_dclick:=crys_if(26);
end;


function menu_bar(me_btree: PAESOBJECT; me_bshow: smallint): smallint;
begin
  _intin[0]:=me_bshow;
  _addrin[0]:=me_btree;

  menu_bar:=crys_if(30);
end;

function menu_icheck(me_ctree: PAESOBJECT; me_citem: smallint; me_ccheck: smallint): smallint;
begin
  _intin[0]:=me_citem;
  _intin[1]:=me_ccheck;
  _addrin[0]:=me_ctree;

  menu_icheck:=crys_if(31);
end;

function menu_ienable(me_etree: PAESOBJECT; me_eitem: smallint; me_eenable: smallint): smallint;
begin
  _intin[0]:=me_eitem;
  _intin[1]:=me_eenable;
  _addrin[0]:=me_etree;

  menu_ienable:=crys_if(32);
end;

function menu_tnormal(me_ntree: PAESOBJECT; me_ntitle: smallint; me_nnormal: smallint): smallint;
begin
  _intin[0]:=me_ntitle;
  _intin[1]:=me_nnormal;
  _addrin[0]:=me_ntree;

  menu_tnormal:=crys_if(33);
end;

function menu_text(me_ttree: PAESOBJECT; me_titem: smallint; me_ttext: PAnsiChar): smallint;
begin
  _intin[0]:=me_titem;
  _addrin[0]:=me_ttree;
  _addrin[1]:=me_ttext;

  menu_text:=crys_if(34);
end;

function menu_register(me_rapid: smallint; me_rpstring: PAnsiChar): smallint;
begin
  _intin[0]:=me_rapid;
  _addrin[0]:=me_rpstring;

  menu_register:=crys_if(35);
end;

function menu_register(me_rapid: smallint; me_rpstring: String): smallint;
begin
  menu_register_cstr:=me_rpstring;
  _intin[0]:=me_rapid;
  _addrin[0]:=@menu_register_cstr;

  menu_register:=crys_if(35);
end;

function menu_unregister(me_uapid: smallint): smallint;
begin
  _intin[0]:=me_uapid;
  with _contrl do
    begin
      opcode:=36;
      num_intin:=1;
      num_intout:=1;
      num_addrin:=0;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_unregister:=_intout[0];
end;

function menu_popup(me_menu: PMENU; me_xpos, me_ypos: smallint; var me_mdata: TMENU): smallint;
begin
  _intin[0]:=me_xpos;
  _intin[1]:=me_ypos;
  _addrin[0]:=me_menu;
  _addrin[1]:=@me_mdata;
  with _contrl do
    begin
      opcode:=36;
      num_intin:=2;
      num_intout:=1;
      num_addrin:=2;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_popup:=_intout[0];
end;

function menu_attach(me_flag: smallint; me_tree: PAESOBJECT; me_item: smallint; me_mdata: PMENU): smallint;
begin
  _intin[0]:=me_flag;
  _intin[1]:=me_item;
  _addrin[0]:=me_tree;
  _addrin[1]:=me_mdata;
  with _contrl do
    begin
      opcode:=37;
      num_intin:=2;
      num_intout:=1;
      num_addrin:=2;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_attach:=_intout[0];
end;

function menu_click(val: smallint; setit: smallint): smallint;
begin
  _intin[0]:=val;
  _intin[1]:=setit;
  with _contrl do
    begin
      opcode:=37;
      num_intin:=2;
      num_intout:=1;
      num_addrin:=0;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_click:=_intout[0];
end;

function menu_istart(me_flag: smallint; me_tree: PAESOBJECT; me_imenu, me_item: smallint): smallint;
begin
  _intin[0]:=me_flag;
  _intin[1]:=me_imenu;
  _intin[2]:=me_item;
  _addrin[0]:=me_tree;
  with _contrl do
    begin
      opcode:=38;
      num_intin:=3;
      num_intout:=1;
      num_addrin:=1;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_istart:=_intout[0];
end;

function menu_settings(me_flag: smallint; me_values: PMN_SET): smallint;
begin
  _intin[0]:=me_flag;
  _addrin[0]:=me_values;
  with _contrl do
    begin
      opcode:=39;
      num_intin:=1;
      num_intout:=1;
      num_addrin:=1;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  menu_settings:=_intout[0];
end;


function objc_add(ob_atree: PAESOBJECT; ob_aparent: smallint; ob_achild: smallint): smallint;
begin
  _intin[0]:=ob_aparent;
  _intin[1]:=ob_achild;
  _addrin[0]:=ob_atree;

  objc_add:=crys_if(40);
end;

function objc_delete(ob_dltree: PAESOBJECT; ob_dlobject: smallint): smallint;
begin
  _intin[0]:=ob_dlobject;
  _addrin[0]:=ob_dltree;

  objc_delete:=crys_if(41);
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

  objc_draw:=crys_if(42);
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

  objc_find:=crys_if(43);
end;

function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     ob_ofxoff: psmallint; ob_ofyoff: psmallint): smallint;
begin
  _intin[0]:=ob_ofobject;
  _addrin[0]:=ob_oftree;

  crys_if(44);

  ob_ofxoff^:=_intout[1];
  ob_ofyoff^:=_intout[2];

  objc_offset:=_intout[0];
end;

function objc_offset(ob_oftree: PAESOBJECT; ob_ofobject: smallint;
                     out ob_ofxoff, ob_ofyoff: smallint): smallint;
begin
  _intin[0]:=ob_ofobject;
  _addrin[0]:=ob_oftree;

  crys_if(44);

  ob_ofxoff:=_intout[1];
  ob_ofyoff:=_intout[2];

  objc_offset:=_intout[0];
end;

function objc_order(ob_ortree: PAESOBJECT; ob_orobject: smallint;
                    ob_ornewpos: smallint): smallint;
begin
  _intin[0]:=ob_orobject;
  _intin[1]:=ob_ornewpos;
  _addrin[0]:=ob_ortree;

  objc_order:=crys_if(45);
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

  crys_if(46);

  ob_edidx^:=_intout[1];
  objc_edit:=_intout[0];
end;

function objc_edit(ob_edtree: PAESOBJECT; ob_edobject, ob_edchar: smallint;
        var ob_edidx: smallint; ob_edkind: smallint): smallint;
begin
  _intin[0]:=ob_edobject;
  _intin[1]:=ob_edchar;
  _intin[2]:=ob_edidx;
  _intin[3]:=ob_edkind;
  _addrin[0]:=ob_edtree;

  crys_if(46);

  ob_edidx:=_intout[1];
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

  objc_change:=crys_if(47);
end;

function objc_sysvar(ob_svmode, ob_svwhich, ob_svinval1, ob_svinval2: smallint;
                    out ob_svoutval1, ob_svoutval2: smallint): smallint;
begin
  _intin[0]:=ob_svmode;
  _intin[1]:=ob_svwhich;
  _intin[2]:=ob_svinval1;
  _intin[3]:=ob_svinval2;

  objc_sysvar:=crys_if(48);

  ob_svoutval1:=_intout[1];
  ob_svoutval2:=_intout[2];
end;


function form_do(fo_dotree: PAESOBJECT; fo_dostartob: smallint): smallint;
begin
  _intin[0]:=fo_dostartob;
  _addrin[0]:=fo_dotree;

  form_do:=crys_if(50);
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

  form_dial:=crys_if(51);
end;

function form_alert(fo_adefbttn: smallint; alertstr: PAnsiChar): smallint;
begin
  _intin[0]:=fo_adefbttn;
  _addrin[0]:=alertstr;
  form_alert:=crys_if(52);
end;

function form_alert(fo_adefbttn: smallint; fo_astring: String): smallint;
var s: aesstr;
begin
  s:=fo_astring;
  _intin[0]:=fo_adefbttn;
  _addrin[0]:=@s;
  form_alert:=crys_if(52);
end;

function form_error(error: smallint): smallint;
begin
  _intin[0]:=error;
  form_error:=crys_if(53);
end;

function form_center(fo_ctree: PAESOBJECT; fo_cx: psmallint;
                     fo_cy: psmallint; fo_cw: psmallint;
                     fo_ch: psmallint): smallint;
begin
  _addrin[0]:=fo_ctree;

  crys_if(54);

  fo_cx^:=_intout[1];
  fo_cy^:=_intout[2];
  fo_cw^:=_intout[3];
  fo_ch^:=_intout[4];

  form_center:=_intout[0];
end;

function form_center(fo_ctree: PAESOBJECT; out fo_cx, fo_cy, fo_cw, fo_ch: smallint): smallint;
begin
  _addrin[0]:=fo_ctree;

  crys_if(54);

  fo_cx:=_intout[1];
  fo_cy:=_intout[2];
  fo_cw:=_intout[3];
  fo_ch:=_intout[4];

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

  crys_if(55);

  fo_knxtobject^:=_intout[1];
  fo_knxtchar^:=_intout[2];

  form_keybd:=_intout[0];
end;

function form_keybd(fo_ktree: PAESOBJECT; fo_kobject, fo_kobnext, fo_kchar: smallint;
        out fo_knxtobject, fo_knxtchar: smallint): smallint;
begin
  _intin[0]:=fo_kobject;
  _intin[1]:=fo_kchar;
  _intin[2]:=fo_kobnext;
  _addrin[0]:=fo_ktree;

  crys_if(55);

  fo_knxtobject:=_intout[1];
  fo_knxtchar:=_intout[2];

  form_keybd:=_intout[0];
end;

function form_button(fo_btree: PAESOBJECT; fo_bobject: smallint;
                     fo_bclicks: smallint; fo_bnxtobj: psmallint): smallint;
begin
  _intin[0]:=fo_bobject;
  _intin[1]:=fo_bclicks;
  _addrin[0]:=fo_btree;

  crys_if(56);

  fo_bnxtobj^:=_intout[1];

  form_button:=_intout[0];
end;

function form_button(fo_btree: PAESOBJECT; fo_bobject, fo_bclicks: smallint;
        out fo_bnxtobj: smallint): smallint;
begin
  _intin[0]:=fo_bobject;
  _intin[1]:=fo_bclicks;
  _addrin[0]:=fo_btree;

  crys_if(56);

  fo_bnxtobj:=_intout[1];

  form_button:=_intout[0];
end;


function graf_rubbox(gr_rx, gr_ry, gr_rminwidth, gr_rminheight: smallint;
        out gr_rlastwidth, gr_rlastheight: smallint): smallint; alias: 'graf_rubberbox';
begin
  _intin[0]:=gr_rx;
  _intin[1]:=gr_ry;
  _intin[2]:=gr_rminwidth;
  _intin[3]:=gr_rminheight;
  graf_rubbox:=crys_if(70);

  gr_rlastwidth:=_intout[1];
  gr_rlastheight:=_intout[2];
end;

function graf_dragbox(gr_dwidth, gr_dheight, gr_dstartx, gr_dstarty,
                      gr_dboundx, gr_dboundy, gr_dboundw, gr_dboundh: smallint;
              out gr_dfinishx, gr_dfinishy: smallint): smallint;
begin
  _intin[0]:=gr_dwidth;
  _intin[1]:=gr_dheight;
  _intin[2]:=gr_dstartx;
  _intin[3]:=gr_dstarty;
  _intin[4]:=gr_dboundx;
  _intin[5]:=gr_dboundy;
  _intin[6]:=gr_dboundw;
  _intin[7]:=gr_dboundh;
  graf_dragbox:=crys_if(71);

  gr_dfinishx:=_intout[1];
  gr_dfinishy:=_intout[2];
end;

function graf_mbox(gr_mwidth, gr_mheight, gr_msourcex, gr_msourcey, gr_mdestx, gr_mdesty: smallint): smallint; alias: 'graf_movebox';
begin
  _intin[0]:=gr_mwidth;
  _intin[1]:=gr_mheight;
  _intin[2]:=gr_msourcex;
  _intin[3]:=gr_msourcey;
  _intin[4]:=gr_mdestx;
  _intin[5]:=gr_mdesty;
  graf_mbox:=crys_if(72);
end;

function graf_growbox(gr_gstx, gr_gsty, gr_gstwidth, gr_gstheight,
                      gr_gfinx, gr_gfiny, gr_gfinwidth, gr_gfinheight :smallint): smallint;
begin
  _intin[0]:=gr_gstx;
  _intin[1]:=gr_gsty;
  _intin[2]:=gr_gstwidth;
  _intin[3]:=gr_gstheight;
  _intin[4]:=gr_gfinx;
  _intin[5]:=gr_gfiny;
  _intin[6]:=gr_gfinwidth;
  _intin[7]:=gr_gfinheight;
  graf_growbox:=crys_if(73);
end;

function graf_shrinkbox(gr_sfinx, gr_sfiny, gr_sfinwidth, gr_sfinheight,
                        gr_sstx, gr_ssty, gr_sstwidth, gr_sstheight: smallint): smallint;
begin
  _intin[0]:=gr_sfinx;
  _intin[1]:=gr_sfiny;
  _intin[2]:=gr_sfinwidth;
  _intin[3]:=gr_sfinheight;
  _intin[4]:=gr_sstx;
  _intin[5]:=gr_ssty;
  _intin[6]:=gr_sstwidth;
  _intin[7]:=gr_sstheight;
  graf_shrinkbox:=crys_if(74);
end;

function graf_watchbox(gr_wptree: PAESOBJECT; gr_wobject, gr_winstate, gr_woutstate: smallint): smallint;
begin
  _intin[0]:=0;
  _intin[1]:=gr_wobject;
  _intin[2]:=gr_winstate;
  _intin[3]:=gr_woutstate;
  _addrin[0]:=gr_wptree;
  graf_watchbox:=crys_if(75);
end;

function graf_slidebox(gr_slptree: PAESOBJECT; gr_slparent, gr_slobject, gr_slvh: smallint): smallint;
begin
  _intin[0]:=gr_slparent;
  _intin[1]:=gr_slobject;
  _intin[2]:=gr_slvh;
  _addrin[0]:=gr_slptree;
  graf_slidebox:=crys_if(76);
end;

function graf_handle(gr_hwchar: psmallint; gr_hhchar: psmallint; gr_hwbox: psmallint; gr_hhbox: psmallint): smallint;
begin
  crys_if(77);

  gr_hwchar^:=_intout[1];
  gr_hhchar^:=_intout[2];
  gr_hwbox^:=_intout[3];
  gr_hhbox^:=_intout[4];

  graf_handle:=_intout[0];
end;

function graf_handle(out gr_hwchar, gr_hhchar, gr_hwbox, gr_hhbox: smallint): smallint;
begin
  crys_if(77);

  gr_hwchar:=_intout[1];
  gr_hhchar:=_intout[2];
  gr_hwbox:=_intout[3];
  gr_hhbox:=_intout[4];

  graf_handle:=_intout[0];
end;

function graf_mouse(gr_monumber: smallint; gr_mofaddr: PMFORM): smallint;
begin
  _intin[0]:=gr_monumber;
  _addrin[0]:=gr_mofaddr;

  graf_mouse:=crys_if(78);
end;

function graf_mkstate(gr_mkmx: psmallint; gr_mkmy: psmallint;
                      gr_mkmstate: psmallint; gr_mkkstate: psmallint): smallint;
begin
  crys_if(79);

  gr_mkmx^:=_intout[1];
  gr_mkmy^:=_intout[2];
  gr_mkmstate^:=_intout[3];
  gr_mkkstate^:=_intout[4];

  graf_mkstate:=_intout[0];
end;

function graf_mkstate(out gr_mkmx, gr_mkmy, gr_mkmstate, gr_mkkstate: smallint): smallint;
begin
  crys_if(79);

  gr_mkmx:=_intout[1];
  gr_mkmy:=_intout[2];
  gr_mkmstate:=_intout[3];
  gr_mkkstate:=_intout[4];

  graf_mkstate:=_intout[0];
end;


function scrp_read(sc_rpscrap: PAnsiChar): smallint;
begin
  _addrin[0]:=sc_rpscrap;
  scrp_read:=crys_if(80);
end;

function scrp_read(out sc_rpscrap: String): smallint;
var s: aesstr;
begin
  _addrin[0]:=@s;
  scrp_read:=crys_if(80);
  sc_rpscrap:=PAnsiChar(@s[0]);
end;

function scrp_write(sc_wpscrap: PAnsiChar): smallint;
begin
  _addrin[0]:=sc_wpscrap;
  scrp_write:=crys_if(81);
end;

function scrp_write(const sc_wpscrap: String): smallint;
var s: aesstr;
begin
  s:=sc_wpscrap;
  _addrin[0]:=@s;
  scrp_write:=crys_if(81);
end;

function scrp_clear: smallint;
begin
  scrp_clear:=crys_if(82);
end;


function fsel_input(fs_iinpath: PAnsiChar; fs_iinsel: PAnsiChar; fs_iexbutton: psmallint): smallint;
begin
  _addrin[0]:=fs_iinpath;
  _addrin[1]:=fs_iinsel;

  crys_if(90);

  fs_iexbutton^:=_intout[1];

  fsel_input:=_intout[0];
end;

function fsel_input(var fs_iinpath, fs_iinsel: String; out fs_iexbutton: smallint): smallint;
var s1, s2: aesstr;
begin
  s1:=fs_iinpath;
  s2:=fs_iinsel;
  _addrin[0]:=@s1;
  _addrin[1]:=@s2;

  fsel_input:=crys_if(90);
  fs_iexbutton:=_intout[1];
  fs_iinpath:=PAnsiChar(@s1[0]);
  fs_iinsel:=PAnsiChar(@s2[0]);
end;

function fsel_exinput(fs_einpath: PAnsiChar; fs_einsel: PAnsiChar; fs_eexbutton: psmallint; elabel: PAnsiChar): smallint;
begin
  _addrin[0]:=fs_einpath;
  _addrin[1]:=fs_einsel;
  _addrin[2]:=elabel;

  crys_if(91);

  fs_eexbutton^:=_intout[1];

  fsel_exinput:=_intout[0];
end;

function fsel_exinput(var fs_einpath, fs_einsel: String; out fs_eexbutton: smallint;
        const fs_elabel: String): smallint;
var s1, s2, s3: aesstr;
begin
  s1:=fs_einpath;
  s2:=fs_einsel;
  s3:=fs_elabel;
  _addrin[0]:=@s1;
  _addrin[1]:=@s2;
  _addrin[2]:=@s3;

  fsel_exinput:=crys_if(91);
  fs_eexbutton:=_intout[1];
  fs_einpath:=PAnsiChar(@s1[0]);
  fs_einsel:=PAnsiChar(@s2[0]);
end;


function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
begin
  _intin[0]:=kind;
  _intin[1]:=x;
  _intin[2]:=y;
  _intin[3]:=w;
  _intin[4]:=h;
  wind_create:=crys_if(100);
end;

function wind_open(handle: smallint; x, y, w, h: smallint): smallint;
begin
  _intin[0]:=handle;
  _intin[1]:=x;
  _intin[2]:=y;
  _intin[3]:=w;
  _intin[4]:=h;
  wind_open:=crys_if(101);
end;

function wind_close(wi_clhandle: smallint): smallint;
begin
  _intin[0]:=wi_clhandle;
  wind_close:=crys_if(102);
end;

function wind_delete(handle: smallint): smallint;
begin
  _intin[0]:=handle;
  wind_delete:=crys_if(103);
end;

function wind_get(wi_ghandle: smallint; wi_gfield: smallint;
                  wi_gw1: psmallint; wi_gw2: psmallint;
                  wi_gw3: psmallint; wi_gw4: psmallint): smallint;
begin
  _intin[0]:=wi_ghandle;
  _intin[1]:=wi_gfield;

  with _contrl do
    begin
      opcode:=104;
      num_intin:=2;
      num_intout:=5;
      num_addrin:=0;
      num_addrout:=0;
    end;
  case wi_gfield of
    WF_DCOLOR, WF_COLOR:
       begin
        _intin[2]:=wi_gw1^;
        _contrl.num_intin:=3;
       end;
    WF_INFO, WF_NAME:
       begin
        PPointer(@_intin[2])^:=wi_gw1;
        _contrl.num_intin:=4;
       end;
  end;
  _intout[3]:=0;
  _intout[4]:=0;
  _crystal(@aespb);

  case wi_gfield of
    WF_INFO, WF_NAME:
       begin
         {* special case where W1 shall not be overwritten *}
       end;
    else
       begin
        wi_gw1^:=_intout[1];
        wi_gw2^:=_intout[2];
        wi_gw3^:=_intout[3];
        wi_gw4^:=_intout[4];
      end;
  end;
  wind_get:=_intout[0];
end;

function wind_get(wi_ghandle, wi_gfield: smallint;
    out wi_gw1, wi_gw2, wi_gw3, wi_gw4: smallint): smallint;
begin
  _intin[0]:=wi_ghandle;
  _intin[1]:=wi_gfield;

  with _contrl do
    begin
      opcode:=104;
      num_intin:=2;
      num_intout:=5;
      num_addrin:=0;
      num_addrout:=0;
    end;
  case wi_gfield of
    WF_DCOLOR, WF_COLOR:
       begin
        _intin[2]:=wi_gw1;
        _contrl.num_intin:=3;
       end;
    WF_INFO, WF_NAME:
       begin
        PPointer(@_intin[2])^:=@wi_gw1;
        _contrl.num_intin:=4;
       end;
  end;
  _intout[3]:=0;
  _intout[4]:=0;
  _crystal(@aespb);

  case wi_gfield of
    WF_INFO, WF_NAME:
       begin
         {* special case where W1 shall not be overwritten *}
       end;
    else
       begin
        wi_gw1:=_intout[1];
        wi_gw2:=_intout[2];
        wi_gw3:=_intout[3];
        wi_gw4:=_intout[4];
      end;
  end;
  wind_get:=_intout[0];
end;

function wind_get(wi_ghandle: smallint; wi_gfield: smallint; gr: PGRECT): smallint;
begin
  _intin[0]:=wi_ghandle;
  _intin[1]:=wi_gfield;

  crys_if(104);

  gr^.x:=_intout[1];
  gr^.y:=_intout[2];
  gr^.w:=_intout[3];
  gr^.h:=_intout[4];

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

  wind_set:=crys_if(105);
end;

function wind_set(wi_shandle: smallint; wi_sfield: smallint; ptr: Pointer): smallint;
begin
  _intin[0]:=wi_shandle;
  _intin[1]:=wi_sfield;
  PPointer(@_intin[2])^:=ptr;
  _intin[4]:=0;
  _intin[5]:=0;

  wind_set:=crys_if(105);
end;

function wind_set(wi_shandle: smallint; wi_sfield: smallint; gr: PGRECT): smallint;
begin
  _intin[0]:=wi_shandle;
  _intin[1]:=wi_sfield;
  _intin[2]:=gr^.x;
  _intin[3]:=gr^.y;
  _intin[4]:=gr^.w;
  _intin[5]:=gr^.h;

  wind_set:=crys_if(105);
end;

function wind_find(wi_fmx: smallint; wi_fmy: smallint): smallint;
begin
  _intin[0]:=wi_fmx;
  _intin[1]:=wi_fmy;

  wind_find:=crys_if(106);
end;

function wind_update(wi_ubegend: smallint): smallint;
begin
  _intin[0]:=wi_ubegend;
  wind_update:=crys_if(107);
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

  crys_if(108);

  coutx^:=_intout[1];
  couty^:=_intout[2];
  coutw^:=_intout[3];
  couth^:=_intout[4];

  wind_calc:=_intout[0];
end;

function wind_calc(wi_ctype, wi_ckind, wi_cinx, wi_ciny, wi_cinw, wi_cinh : smallint;
           out wi_coutx, wi_couty, wi_coutw, wi_couth: smallint): smallint;
begin
  _intin[0]:=wi_ctype;
  _intin[1]:=wi_ckind;
  _intin[2]:=wi_cinx;
  _intin[3]:=wi_ciny;
  _intin[4]:=wi_cinw;
  _intin[5]:=wi_cinh;

  crys_if(108);

  wi_coutx:=_intout[1];
  wi_couty:=_intout[2];
  wi_coutw:=_intout[3];
  wi_couth:=_intout[4];

  wind_calc:=_intout[0];
end;

procedure wind_new;
begin
  crys_if(109);
end;


function rsrc_load(re_lpfname: PAnsiChar): smallint;
begin
  _addrin[0]:=re_lpfname;
  rsrc_load:=crys_if(110);
end;

function rsrc_load(re_lpfname: String): smallint;
var s: aesstr;
begin
  s:=re_lpfname;
  _addrin[0]:=@s;
  rsrc_load:=crys_if(110);
end;

function rsrc_free: smallint;
begin
  rsrc_free:=crys_if(111);
end;

function rsrc_gaddr(re_gtype: smallint; re_gindex: smallint; gaddr: ppointer): smallint;
begin
  _intin[0]:=re_gtype;
  _intin[1]:=re_gindex;

  crys_if(112);

  gaddr^:=_addrout[0];

  rsrc_gaddr:=_intout[0];
end;

function rsrc_gaddr(re_gtype, re_gindex: smallint; out re_gaddr: Pointer): smallint;
begin
  _intin[0]:=re_gtype;
  _intin[1]:=re_gindex;

  crys_if(112);

  re_gaddr:=_addrout[0];

  rsrc_gaddr:=_intout[0];
end;

function rsrc_saddr(re_stype: smallint; re_sindex: smallint; saddr: pointer): smallint;
begin
  _intin[0]:=re_stype;
  _intin[1]:=re_sindex;
  _addrin[0]:=saddr;

  rsrc_saddr:=crys_if(113);
end;

function rsrc_obfix(re_otree: PAESOBJECT; re_oobject: smallint): smallint;
begin
  _intin[0]:=re_oobject;
  _addrin[0]:=re_otree;

  rsrc_obfix:=crys_if(114);
end;

function rsrc_rcfix(rc_header: PRSHDR): smallint;
begin
  _addrin[0]:=rc_header;

  rsrc_rcfix:=crys_if(115);
end;


function shel_read(sh_rpcmd: PAnsiChar; sh_rptail: PAnsiChar): smallint;
begin
  _addrin[0]:=sh_rpcmd;
  _addrin[1]:=sh_rptail;

   shel_read:=crys_if(120);
end;

function shel_read(out sh_rpcmd, sh_rptail: String): smallint;
var s1, s2: aesstr;
begin
  _addrin[0]:=@s1;
  _addrin[1]:=@s2;

   shel_read:=crys_if(120);
   sh_rpcmd:=PAnsiChar(@s1[0]);
   sh_rptail:=PAnsiChar(@s2[0]);
end;

function shel_write(sh_wdoex: smallint; sh_wisgr: smallint;
                    sh_wiscr: smallint; sh_wpcmd: PAnsiChar;
                    sh_wptail: PAnsiChar): smallint;
begin
  _intin[0]:=sh_wdoex;
  _intin[1]:=sh_wisgr;
  _intin[2]:=sh_wiscr;
  _addrin[0]:=sh_wpcmd;
  _addrin[1]:=sh_wptail;

  shel_write:=crys_if(121);
end;

function shel_write(sh_wdoex, sh_wisgr, sh_wiscr: smallint;
            const sh_wpcmd, sh_wptail: String): smallint;
var s1: aesstr;
begin
  s1:=sh_wpcmd;
  _intin[0]:=sh_wdoex;
  _intin[1]:=sh_wisgr;
  _intin[2]:=sh_wiscr;
  _addrin[0]:=@s1;
  _addrin[1]:=@sh_wptail[0];

  shel_write:=crys_if(121);
end;

function shel_get(sh_gaddr: PAnsiChar; sh_glen: word): smallint;
begin
  _intin[0]:=smallint(sh_glen);
  _addrin[0]:=sh_gaddr;

  shel_get:=crys_if(122);
end;

function shel_put(sh_paddr: PAnsiChar; sh_plen: word): smallint;
begin
  _intin[0]:=smallint(sh_plen);
  _addrin[0]:=sh_paddr;

  shel_put:=crys_if(123);
end;

function shel_find(sh_fpbuff: PAnsiChar): smallint;
begin
  _addrin[0]:=sh_fpbuff;

  shel_find:=crys_if(124);
end;

function shel_find(var sh_fpbuff: String): smallint;
var s: aesstr;
begin
  s:=sh_fpbuff;
  _addrin[0]:=@s;

  shel_find:=crys_if(124);
  sh_fpbuff:=PAnsiChar(@s[0]);
end;

function shel_envrn(sh_epvalue: PPAnsiChar; sh_eparm: PAnsiChar): smallint;
begin
  _addrin[0]:=sh_epvalue;
  _addrin[1]:=sh_eparm;

  shel_envrn:=crys_if(125);
end;

function shel_envrn(out sh_epvalue: Pointer; const sh_eparm: String): smallint;
var s: aesstr;
begin
  s:=sh_eparm;
  _addrin[0]:=@sh_epvalue;
  _addrin[1]:=@s;

  shel_envrn:=crys_if(125);
end;

function shel_rdef(out sh_rlpcmd, sh_rlpdir: String): smallint;
var s1, s2: aesstr;
begin
  _addrin[0]:=@s1;
  _addrin[1]:=@s2;

  shel_rdef:=crys_if(126);
  sh_rlpcmd:=PAnsiChar(@s1[0]);
  sh_rlpdir:=PAnsiChar(@s2[0]);
end;

function shel_wdef(const sh_wlpcmd, sh_wlpdir: String): smallint;
var s1, s2: aesstr;
begin
  s1:=sh_wlpcmd;
  s2:=sh_wlpdir;
  _addrin[0]:=@s1;
  _addrin[1]:=@s2;

  shel_wdef:=crys_if(127);
end;


function xgrf_stepcalc(xg_storgw, xg_storgh, xg_stxc, xg_styc, xg_stw, xg_sth : smallint;
            out xg_stpcx, xg_stpcy, xg_stpcnt, xg_stpxstep, xg_stpystep: smallint): smallint;
begin
  _intin[0]:=xg_storgw;
  _intin[1]:=xg_storgh;
  _intin[2]:=xg_stxc;
  _intin[3]:=xg_styc;
  _intin[4]:=xg_stw;
  _intin[5]:=xg_sth;
  with _contrl do
    begin
      opcode:=130;
      num_intin:=6;
      num_intout:=6;
      num_addrin:=0;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  xgrf_stepcalc:=_intout[0];
  xg_stpcx:=_intout[1];
  xg_stpcy:=_intout[2];
  xg_stpcnt:=_intout[3];
  xg_stpxstep:=_intout[4];
  xg_stpystep:=_intout[5];
end;

function xgrf_2box(xg_2cnt, xg_2xstep, xg_2ystep, xg_2doubled,
                   xg_2corners, xg_2xc, xg_2yc, xg_2w, xg_2h: smallint): smallint;
begin
  _intin[0]:=xg_2cnt;
  _intin[1]:=xg_2xstep;
  _intin[2]:=xg_2ystep;
  _intin[3]:=xg_2doubled;
  _intin[4]:=xg_2corners;
  _intin[5]:=xg_2xc;
  _intin[6]:=xg_2yc;
  _intin[7]:=xg_2w;
  _intin[8]:=xg_2h;
  with _contrl do
    begin
      opcode:=131;
      num_intin:=9;
      num_intout:=1;
      num_addrin:=0;
      num_addrout:=0;
    end;
  _crystal(@aespb);
  xgrf_2box:=_intout[0];
end;


function vq_aes: smallint;
begin
  _global[0] := 0;
  vq_aes := appl_init;
  if (_global[0] = 0) then
    vq_aes := -1;
end;


function crys_if(_opcode: dword): smallint;
begin
  with _contrl do
    begin
      opcode:=_opcode;
      nums:=ops_table[_opcode-10];
    end;
  asm
    pea.l       (a2)
    lea.l       aespb,a0
    move.l      a0,d1
    move.w      #AES_TRAP_MAGIC,d0
    trap        #2
    movea.l     (a7)+,a2
  end;
  crys_if:=_intout[0];
end;


procedure _crystal(pb: PAESPB); assembler; nostackframe;
asm
    pea.l       (a2)
    move.l      pb,a0
    move.l      a0,d1
    move.w      #AES_TRAP_MAGIC,d0
    trap        #2
    movea.l     (a7)+,a2
end;

end.
