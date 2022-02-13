{
    Copyright (c) 2017 by Free Pascal development team

    VDI interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE FPC}
{$MODESWITCH OUT+}
{$PACKRECORDS 2}

unit vdi;

interface

uses gemcmmn;

{ The API description of this file is based on the information available
  online at: https://freemint.github.io/tos.hyp/en/index.html }

{$I vditypes.inc}

type
   ARRAY_8 = gemcmmn.ARRAY_8;
   PMFORM = gemcmmn.PMFORM;
   TMFORM = gemcmmn.TMFORM;

procedure vdi;
procedure vdi(pb: PVDIPB);

function vq_gdos: smallint;
function vq_vgdos: LongInt;

procedure vdi_str_to_pchar(src: psmallint; des: pchar; len: smallint);
function pchar_str_to_vdi(src: pchar; des: psmallint): longint;

procedure v_opnwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
procedure v_clswk(handle: smallint);
procedure v_clrwk(handle: smallint);
procedure v_updwk(handle: smallint);

procedure vq_chcells(handle: smallint; out rows, columns: smallint);
procedure v_exit_cur(handle: smallint);
procedure v_enter_cur(handle: smallint);
procedure v_curup(handle: smallint);
procedure v_curdown(handle: smallint);
procedure v_curright(handle: smallint);
procedure v_curleft(handle: smallint);
procedure v_curhome(handle: smallint);
procedure v_eeos(handle: smallint);
procedure v_eeol(handle: smallint);
procedure v_curaddress(handle, row, column: smallint);
procedure v_curtext(handle: smallint; const outString: String);
procedure v_rvon(handle: smallint);
procedure v_rvoff(handle: smallint);
procedure vq_curaddress(handle: smallint; out row, column: smallint);
function vq_tabstatus(handle: smallint): smallint;
procedure v_hardcopy(handle: smallint);
procedure v_dspcur(handle, x, y: smallint);
procedure v_rmcur(handle: smallint);
procedure v_form_adv(handle: smallint);
procedure v_output_window(handle: smallint; xyarray: ARRAY_4);
procedure v_clear_disp_list(handle: smallint);
procedure v_bit_image(handle: smallint; const filename: string;
                aspect, x_scale, y_scale, h_align, v_align: smallint;
                const xyarray: ARRAY_4);
procedure vq_scan(handle: smallint; out g_slice, g_page, a_slice, a_page, div_fac: smallint);
procedure v_alpha_text(handle: smallint; const outString: String);
function v_orient(handle, orientation: smallint): smallint;
function v_copies(handle, count: smallint): smallint;
procedure v_tray(handle, tray: smallint);
function v_page_size(handle, page_id: smallint): smallint;
function vs_palette(handle, palette: smallint): smallint;
procedure v_sound(handle, frequency, duration: smallint);
function vs_mute(handle, action: smallint): smallint;
procedure vt_resolution(handle, xres, yres: smallint;
                        out xset, yset: smallint);
procedure vt_axis(handle, xres, yres: smallint;
                  out xset, yset: smallint);
procedure vt_origin(handle, xorigin, yorigin: smallint);
procedure vq_tdimensions(handle: smallint; out xdimension, ydimension: smallint);
procedure vt_alignment(handle, dx, dy: smallint);
procedure vsp_film(handle, index, lightness: smallint);
function vqp_filmname(handle, index: smallint; out name: String): smallint;
procedure vsc_expose(handle, state: smallint);
procedure v_meta_extents(handle, min_x, min_y, max_x, max_y: smallint);
procedure v_write_meta(handle, num_intin: smallint; a_intin: Pointer;
                       num_ptsin: smallint;a_ptsin: Pointer);
procedure vm_pagesize(handle, pgwidth, pgheight: smallint);
procedure vm_coords(handle, llx, lly, urx, ury: smallint);
function v_bez_qual(handle, prcnt: smallint; out actual: smallint): smallint;
procedure vm_filename(handle: smallint; const filename: String);
procedure v_offset(handle, offset: smallint);
procedure v_fontinit(handle: smallint; var fh: TFONT_HDR);
procedure v_escape2000(handle, times: smallint);

procedure v_pline(handle: smallint; count: smallint; pxyarray: psmallint);
procedure v_pline(handle, count: smallint; const pxyarray: Array of smallint);
procedure v_bez(handle, count: smallint; xyarr, bezarr: Pointer;
                out extent: ARRAY_4;
                out totpts, totmoves: smallint);
procedure v_bez_fill(handle, count: smallint;
                    xyarr, bezarr: Pointer;
                    out extent: ARRAY_4;
                    out totpts, totmoves: smallint);

procedure v_pmarker(handle, count: smallint; const pxyarray: Array of smallint);
procedure v_gtext(handle: smallint; x: smallint; y: smallint; outputstring: pchar);
procedure v_gtext(handle, x, y: smallint; const outputstring: string);
procedure v_fillarea(handle, count: smallint; const pxyarray: Array of smallint);

procedure v_bar(handle: smallint; pxyarray: psmallint);
procedure v_bar(handle: smallint; const pxyarray: ARRAY_4);
procedure v_arc(handle, x, y, radius, begang, endang: smallint);
procedure v_pieslice(handle, x, y, radius, begang, endang: smallint);
procedure v_circle(handle: smallint; x: smallint; y: smallint; radius: smallint);
procedure v_ellipse(handle, x, y, xradius, yradius: smallint);
procedure v_ellarc(handle, x, y, xradius, yradius, begang, endang: smallint);
procedure v_ellpie(handle, x, y, xradius, yradius, begang, endang: smallint);
procedure v_rbox(handle: smallint; const xyarray: ARRAY_4);
procedure v_rfbox(handle: smallint; const xyarray: ARRAY_4);
procedure v_justified(handle, x, y: smallint;
        const outputstring: string;
        width, wordspace, charspace: smallint);
function v_bez_on(handle: smallint): smallint;
procedure v_bez_off(handle: smallint);

procedure vst_height(handle, height: smallint; out char_width, char_height, cell_width, cell_height: smallint);
function vst_rotation(handle, angle: smallint): smallint;

procedure vs_color(handle: smallint; index: smallint; rgb_in: psmallint);
procedure vs_color(handle, index: smallint; const rgb_in: ARRAY_3);

function vsl_type(handle, style: smallint): smallint;
function vsl_width(handle, width: smallint): smallint;
function vsl_color(handle: smallint; color_index: smallint): smallint;
function vsm_type(handle, symbol: smallint): smallint;
function vsm_height(handle, height: smallint): smallint;
function vsm_color(handle, color_index: smallint): smallint;
function vst_font(handle, font: smallint): smallint;
function vsf_interior(handle, style: smallint): smallint;
function vsf_style(handle, style_index: smallint): smallint;
function vq_color(handle, color_index, set_flag: smallint; out rgb: ARRAY_3): smallint;
procedure vrq_locator(handle, x, y: smallint; out xout, yout, term: smallint);
function vsm_locator(handle, x, y: smallint; out xout, yout, term: smallint): smallint;
procedure vrq_valuator(handle, valuator_in: smallint; out valuator_out, terminator: smallint);
procedure vsm_valuator(handle, val_in: smallint; out val_out, term, status: smallint);
procedure vrq_choice(handle, ch_in: smallint; out ch_out: smallint);
function vsm_choice(handle: smallint; out choice: smallint): smallint;
procedure vrq_string(handle, max_length, echo_mode: smallint; const echo_xy: ARRAY_2; out resString: string);
function vsm_string(handle, max_length, echo_mode: smallint; const echo_xy: ARRAY_2; out resString: string): smallint;

function vst_color(handle: smallint; color_index: smallint): smallint;
function vsf_color(handle: smallint; color_index: smallint): smallint;

function vswr_mode(handle: smallint; mode: smallint): smallint;
function vsin_mode(handle, dev_type, mode: smallint): smallint;
procedure vql_attributes(handle: smallint; out attrib: ARRAY_4);
procedure vql_attributes(handle: smallint; out attrib: ARRAY_6);
procedure vqm_attributes(handle: smallint; out attrib: ARRAY_4);
procedure vqf_attributes(handle: smallint; out attrib: ARRAY_5);
procedure vqt_attributes(handle: smallint; out attrib: ARRAY_10);
procedure vst_alignment(handle, hor_in, vert_in: smallint; out hor_out, vert_out: smallint);

procedure v_opnvwk(work_in: psmallint; handle: psmallint; work_out: psmallint); overload;
procedure v_clsvwk(handle: smallint); overload;
procedure vq_extnd(handle, owflag: smallint; WorkOut: psmallint); overload;
procedure vq_scrninfo(handle: smallint; out WorkOut: ARRAY_273);
procedure v_contourfill(handle, x, y, index: smallint);
function vsf_perimeter(handle, per_vis: smallint): smallint;

procedure v_get_pixel(handle: smallint; x: smallint; y: smallint;
                      pel: psmallint; index: psmallint);
procedure v_get_pixel(handle, x, y: smallint; out pel, index: smallint);
function vst_effects(handle, effect: smallint): smallint;
function vst_point(handle, point: smallint; out char_width, char_height, cell_width, cell_height: smallint): smallint;
procedure vsl_ends(handle, beg_style, end_style: smallint);

procedure vro_cpyfm(handle: smallint; vr_mode: smallint; pxyarray: psmallint; psrcMFDB: PMFDB; pdesMFDB: PMFDB); overload;
procedure vr_trnfm(handle: smallint; psrcMFDB, pdesMFDB: PMFDB);
procedure vr_trnfm(handle: smallint; const psrcMFDB, pdesMFDB: TMFDB);
procedure vsc_form(handle: smallint; pcur_form: PMFORM);
procedure vsf_udpat(handle: smallint; pfill_pat: Pointer; planes: smallint);
procedure vsl_udsty(handle, pattern: smallint);
procedure vr_recfl(handle: smallint; const pxyarray: ARRAY_4);
procedure vqin_mode(handle, dev_type: smallint; out input_mode: smallint);
procedure vqt_extent(handle: smallint; calcString: pchar; extent: psmallint); overload;
function vqt_width(handle, character: smallint; out cell_width, left_delta, right_delta: smallint): smallint;

procedure vex_timv(handle: smallint; tim_addr: Pointer; out otim_addr: Pointer; out tim_conv: smallint);

function vst_load_fonts(handle, select: smallint): smallint;
procedure vst_unload_fonts(handle, select: smallint);

procedure vrt_cpyfm(handle: smallint; vr_mode: smallint; pxyarray: psmallint; psrcMFDB: PMFDB; pdesMFDB: PMFDB; color_index: psmallint); overload;

procedure v_show_c(handle: smallint; reset: smallint);
procedure v_hide_c(handle: smallint);
procedure vq_mouse(handle: smallint; out pstatus, x, y: smallint);
procedure vex_butv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
procedure vex_motv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
procedure vex_curv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
procedure vex_wheelv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
procedure vq_key_s(handle: smallint; out pstatus: smallint);

procedure vs_clip(handle: smallint; clip_flag: smallint; pxyarray: psmallint);
procedure vs_clip(handle, clip_flag: smallint; const pxyarray: ARRAY_4);
procedure vs_clip_off(handle: smallint);

function vqt_name(handle, element_num: smallint; out name: String33): smallint;
procedure vqt_fontinfo(handle: smallint;
        out minADE, maxADE: smallint;
        out distances: ARRAY_5;
        out maxwidth: smallint;
        out effects: ARRAY_3);

procedure vqt_justified(handle, x, y: smallint; const outString: String;
            length, word_space, char_space: smallint;
            offsets: Pointer);

procedure vst_width(handle, width: smallint; out char_width, char_height, cell_width, cell_height: smallint);
procedure vqt_fontheader(handle: smallint; buffer: Pointer;
                         out pathname: String);
procedure vqt_trackkern(handle: smallint; out x, y: fix31);
procedure vqt_pairkern(handle, ch1, ch2: smallint; out x, y: fix31);
procedure vst_charmap(handle, mode: smallint);
function vst_map_mode(handle, mode: smallint): smallint;
procedure vst_kern(handle, tmode, pmode: smallint; out tracks, pairs: smallint);
procedure vst_track_offset(handle: smallint; offset: fix31; pairmode: smallint; out tracks, pairs: smallint);
procedure v_getbitmap_info(handle, ch: smallint;
                           out advx, advy, xoff, yoff: fix31;
                           out width, height: smallint;
                           out bitmap: pointer);

procedure v_ftext(handle, x, y: smallint; const str: String);
procedure v_ftext_offset(handle, x, y: smallint;
                         const outputstring: string;
                         const offset: Array of smallint);
procedure v_killoutline(handle: smallint; component: Pointer);
procedure v_getoutline(handle, ch: smallint;
                       const xyarray: Array of smallint;
                       const bezarray: Array of ShortInt;
                       maxverts: smallint;
                       out numverts: smallint);
procedure vst_scratch(handle, mode: smallint);
procedure vst_error(handle, mode: smallint; out errorvar: smallint);
function vst_arbpt(handle, point: smallint;
                   out chwd, chht, cellwd, cellht: smallint): smallint;
function vst_arbpt32(handle: smallint; point: fix31;
                     out chwd, chht, cellwd, cellht: smallint): fix31;
procedure vqt_advance(handle, ch: smallint; out advx, advy, remx, remy: smallint);
procedure vqt_advance32(handle, ch: smallint; out advx, advy: fix31);
function vq_devinfo(handle, devnum: smallint;
                      out devexists: smallint;
                      out filename: String;
                      out devicename: String): smallint;
procedure vqt_devinfo(handle, devnum: smallint;
                      out dev_busy: smallint;
                      out filename: String;
                      out devicename: String);

function v_savecache(handle: smallint; const filename: String): smallint;
function v_loadcache(handle: smallint; const filename: String; mode: smallint): smallint;
function v_flushcache(handle: smallint): smallint;
function vst_setsize(handle, point: smallint;
                     out chwd, chht, cellwd, cellht: smallint): smallint;
function  vst_setsize32(handle: smallint; point: fix31;
                        out chwd, chht, cellwd, cellht: smallint): fix31;
function vst_skew(handle, skew: smallint): smallint;
procedure vqt_get_table(handle: smallint; out map: Pointer);
procedure vqt_cachesize(handle, which_cache: smallint; out size: LongInt);

procedure v_set_app_buff(handle: smallint; address: Pointer; nparagraphs: smallint);


(*
 * NOT YET IMPLEMENTED:
fix31_to_point(a) ((_WORD)((((a) + 32768L) >> 16)))
point_to_fix31(a) (((fix31)(a)) << 16)

v_trays

v_ps_halftone
vq_calibrate
vq_page_name
vq_tray_names
vs_calibrate
v_etext

v_setrgbi
v_xbit_image
v_topbot
vs_bkcolor
v_pat_rotate
vs_grayoverride

v_opnbm
v_clsbm

v_get_driver_info
vqt_real_extent

vq_margins
vq_driver_info
vq_bit_image
vs_page_info
vs_crop
vq_image_type
vs_save_disp_list
vs_load_disp_list

vqt_xfntinfo
vq_ext_devinfo
vqt_ext_name
vqt_name_and_id
vst_name

vqt_char_index

vqt_is_char_available

v_color2nearest
v_color2value
v_create_ctab
v_create_itab
v_ctab_idx2value
v_ctab_idx2vdi
v_ctab_vdi2idx
v_delete_ctab
v_delete_itab
v_get_ctab_id
v_get_outline
v_open_bm
v_resize_bm
v_setrgb
v_value2color
vq_ctab
vq_ctab_entry
vq_ctab_id
vq_dflt_ctab
vq_hilite_color
vq_margins
vq_max_color
vq_min_color
vq_prn_scaling
vq_px_format
vq_weight_color
vqf_bg_color
vqf_fg_color
vql_bg_color
vql_fg_color
vqm_bg_color
vqm_fg_color
vqr_bg_color
vqr_fg_color
vqt_bg_color
vqt_fg_color
vr_transfer_bits
vs_ctab
vs_ctab_entry
vs_dflt_ctab
vs_document_info
vs_hilite_color
vs_max_color
vs_min_color
vs_weight_color
vsf_bg_color
vsf_fg_color
vsl_bg_color
vsl_fg_color
vsm_bg_color
vsm_fg_color
vsr_bg_color
vsr_fg_color
vst_bg_color
vst_fg_color
*)


implementation

const
  VDI_TRAP_MAGIC = $73;

var
  _contrl: TVDIContrl;
  _intin: TVDIIntIn;
  _intout: TVDIIntOut;
  _ptsin: TVDIPtsIn;
  _ptsout: TVDIPtsOut;

const
  pblock: TVDIPB = (
    control: @_contrl;
    intin: @_intin;
    ptsin: @_ptsin;
    intout: @_intout;
    ptsout: @_ptsout;
  ); public name 'vdipb';

function string_to_vdi(const src: String; dst: psmallint): smallint;
var
  i, len: longint;
begin
  len:=length(src);
  for i:=0 to len-1 do
    dst[i]:=byte(src[i + 1]);

  string_to_vdi:=len;
end;

procedure vdi_to_string(src: psmallint; out dst: String; len: longint);
var
  i: longint;
begin
  for i:=0 to len-1 do
    dst[i + 1]:=chr(src[i]);
  setlength(dst, len);
end;

procedure vdi; assembler; nostackframe;
asm
  pea.l       (a2)
  lea.l pblock, a0
  move.l a0, d1
  move.w #VDI_TRAP_MAGIC, d0
  trap #2
  movea.l     (a7)+,a2
end;

procedure vdi(pb: PVDIPB); assembler; nostackframe;
asm
  pea.l       (a2)
  move.l pb,a0
  move.l a0,d1
  move.w #VDI_TRAP_MAGIC,d0
  trap #2
  movea.l     (a7)+,a2
end;

function vq_gdos: smallint; assembler; nostackframe;
asm
  pea.l       (a2)
  moveq.l     #-2,d0
  trap        #2
  addq        #2,d0
  ext.l       d0
  movea.l     (a7)+,a2
end;

function vq_vgdos: LongInt; assembler; assembler; nostackframe;
asm
  pea.l       (a2)
  moveq.l     #-2,d0
  trap        #2
  movea.l     (a7)+,a2
end;

procedure vdi_str_to_pchar(src: psmallint; des: pchar; len: smallint);
begin
  while len > 0 do
    begin
      des[0]:=char(src[0]); {* Only low byte *}
      inc(src);
      inc(des);
      dec(len);
    end;
  des[0]:=#0; {* End of string *}
end;

function pchar_str_to_vdi(src: pchar; des: psmallint): longint;
var
  len: longint;
begin
  len:=0;
  repeat
    des[len]:=byte(src[len]);
    inc(len);
  until (src[len-1] = #0);

  pchar_str_to_vdi:=len-1;
end;

procedure v_opnwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  // _intin[0..15] = work_in[0..15];
  pb.intin := PVDIIntIn(work_in);
  pb.ptsin := @_ptsin;
  // work_out[0..44] = intout[0..44];
  pb.intout := PVDIIntOut(work_out);
  // work_out[45..56] = ptsout[0..11];
  pb.ptsout := PVDIPtsOut(@work_out[45]);

  _contrl[0]:=1;
  _contrl[1]:=0;
  _contrl[3]:=16;
  _contrl[5]:=0;
  _contrl[6]:=0;

  vdi(@pb);

  handle^:=_contrl[6];
end;

procedure v_clswk(handle: smallint);
begin
  _contrl[0]:=2;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_clrwk(handle: smallint);
begin
  _contrl[0]:=3;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_updwk(handle: smallint);
begin
  _contrl[0]:=4;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;


procedure vq_chcells(handle: smallint; out rows, columns: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=1;
  _contrl[6]:=handle;

  vdi;

  rows:=_intout[0];
  columns:=_intout[1];
end;

procedure v_exit_cur(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=2;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_enter_cur(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=3;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curup(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=4;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curdown(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=5;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curright(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=6;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curleft(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=7;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curhome(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=8;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_eeos(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=9;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_eeol(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=10;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curaddress(handle, row, column: smallint);
begin
  _intin[0]:=row;
  _intin[1]:=column;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=11;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_curtext(handle: smallint; const outString: String);
var len: longint;
begin
  len:=string_to_vdi(outString, @_intin[0]);
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=len;
  _contrl[5]:=12;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_rvon(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=13;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_rvoff(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=14;
  _contrl[6]:=handle;

  vdi;
end;

procedure vq_curaddress(handle: smallint; out row, column: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=15;
  _contrl[6]:=handle;

  vdi;

  row:=_intout[0];
  column:=_intout[1];
end;

function vq_tabstatus(handle: smallint): smallint;
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=16;
  _contrl[6]:=handle;

  vdi;

  vq_tabstatus:=_intout[0];
end;

procedure v_hardcopy(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=17;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_dspcur(handle, x, y: smallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _contrl[0]:=5;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=18;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_rmcur(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=19;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_form_adv(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=20;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_output_window(handle: smallint; xyarray: ARRAY_4);
begin
  _ptsin[0]:=xyarray[0];
  _ptsin[1]:=xyarray[1];
  _ptsin[2]:=xyarray[2];
  _ptsin[3]:=xyarray[3];
  _contrl[0]:=5;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=21;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_clear_disp_list(handle: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=22;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_bit_image(handle: smallint; const filename: string;
                aspect, x_scale, y_scale, h_align, v_align: smallint;
                const xyarray: ARRAY_4);
var len: longint;
begin
  _ptsin[0]:=xyarray[0];
  _ptsin[1]:=xyarray[1];
  _ptsin[2]:=xyarray[2];
  _ptsin[3]:=xyarray[3];
  _intin[0]:=aspect;
  _intin[1]:=x_scale;
  _intin[2]:=y_scale;
  _intin[3]:=h_align;
  _intin[4]:=v_align;
  len:=string_to_vdi(filename, @_intin[5]);
  _contrl[0]:=5;
  _contrl[1]:=2;
  _contrl[3]:=len+5;
  _contrl[5]:=23;
  _contrl[6]:=handle;

  vdi;
end;

procedure vq_scan(handle: smallint; out g_slice, g_page, a_slice, a_page, div_fac: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=24;
  _contrl[6]:=handle;

  vdi;

  g_slice:=_intout[0];
  g_page:=_intout[1];
  a_slice:=_intout[2];
  a_page:=_intout[3];
  div_fac:=_intout[4];
end;

procedure v_alpha_text(handle: smallint; const outString: String);
var len: longint;
begin
  len:=string_to_vdi(outString, @_intin[0]);
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=len;
  _contrl[5]:=25;
  _contrl[6]:=handle;

  vdi;
end;

function v_orient(handle, orientation: smallint): smallint;
begin
  _intin[0]:=orientation;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=27;
  _contrl[6]:=handle;

  vdi;

  v_orient:=_intout[0];
end;

function v_copies(handle, count: smallint): smallint;
begin
  _intin[0]:=count;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=28;
  _contrl[6]:=handle;

  vdi;

  v_copies:=_intout[0];
end;

procedure v_tray(handle, tray: smallint);
begin
  _intin[0]:=tray;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=29;
  _contrl[6]:=handle;

  vdi;
end;

function v_page_size(handle, page_id: smallint): smallint;
begin
  _intin[0]:=page_id;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=37;
  _contrl[6]:=handle;

  vdi;

  v_page_size:=_intout[0];
end;

function vs_palette(handle, palette: smallint): smallint;
begin
  _intin[0]:=palette;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=60;
  _contrl[6]:=handle;

  vdi;

  vs_palette:=_intout[0];
end;

procedure v_sound(handle, frequency, duration: smallint);
begin
  _intin[0]:=frequency;
  _intin[1]:=duration;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=61;
  _contrl[6]:=handle;

  vdi;
end;

function vs_mute(handle, action: smallint): smallint;
begin
  _intin[0]:=action;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=62;
  _contrl[6]:=handle;

  vdi;
end;

procedure vt_resolution(handle, xres, yres: smallint;
                        out xset, yset: smallint);
begin
  _intin[0]:=xres;
  _intin[1]:=yres;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=81;
  _contrl[6]:=handle;

  vdi;

  xset:=_intout[0];
  yset:=_intout[1];
end;

procedure vt_axis(handle, xres, yres: smallint;
                  out xset, yset: smallint);
begin
  _intin[0]:=xres;
  _intin[1]:=yres;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=82;
  _contrl[6]:=handle;

  vdi;

  xset:=_intout[0];
  yset:=_intout[1];
end;

procedure vt_origin(handle, xorigin, yorigin: smallint);
begin
  _intin[0]:=xorigin;
  _intin[1]:=yorigin;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=83;
  _contrl[6]:=handle;

  vdi;
end;

procedure vq_tdimensions(handle: smallint; out xdimension, ydimension: smallint);
begin
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=84;
  _contrl[6]:=handle;

  vdi;

  xdimension:=_intout[0];
  ydimension:=_intout[1];
end;

procedure vt_alignment(handle, dx, dy: smallint);
begin
  _intin[0]:=dx;
  _intin[1]:=dy;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=85;
  _contrl[6]:=handle;

  vdi;
end;

procedure vsp_film(handle, index, lightness: smallint);
begin
  _intin[0]:=index;
  _intin[1]:=lightness;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=91;
  _contrl[6]:=handle;

  vdi;
end;

function vqp_filmname(handle, index: smallint; out name: String): smallint;
begin
  _intin[0]:=index;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=92;
  _contrl[6]:=handle;

  vdi;

  vdi_to_string(@_intout[0], name, _contrl[4]);
  vqp_filmname:=_contrl[4];
end;

procedure vsc_expose(handle, state: smallint);
begin
  _intin[0]:=state;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=93;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_meta_extents(handle, min_x, min_y, max_x, max_y: smallint);
begin
  _ptsin[0]:=min_x;
  _ptsin[1]:=min_y;
  _ptsin[2]:=max_x;
  _ptsin[3]:=max_y;
  _contrl[0]:=5;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=98;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_write_meta(handle, num_intin: smallint; a_intin: Pointer;
                       num_ptsin: smallint;a_ptsin: Pointer);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := PVDIIntIn(a_intin);
  pb.ptsin := PVDIPtsIn(a_ptsin);
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;
  _contrl[0]:=5;
  _contrl[1]:=num_ptsin;
  _contrl[3]:=num_intin;
  _contrl[5]:=99;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure vm_pagesize(handle, pgwidth, pgheight: smallint);
begin
  _intin[0]:=0;
  _intin[1]:=pgwidth;
  _intin[2]:=pgheight;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=3;
  _contrl[5]:=99;
  _contrl[6]:=handle;

  vdi;
end;

procedure vm_coords(handle, llx, lly, urx, ury: smallint);
begin
  _intin[0]:=1;
  _intin[1]:=llx;
  _intin[2]:=lly;
  _intin[3]:=urx;
  _intin[4]:=ury;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=5;
  _contrl[5]:=99;
  _contrl[6]:=handle;

  vdi;
end;

function v_bez_qual(handle, prcnt: smallint; out actual: smallint): smallint;
begin
  _intin[0]:=32;
  _intin[1]:=1;
  _intin[2]:=prcnt;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=3;
  _contrl[5]:=99;
  _contrl[6]:=handle;

  vdi;

  actual:=_intout[0];
  v_bez_qual:=_intout[0];
end;

procedure vm_filename(handle: smallint; const filename: String);
var len: longint;
begin
  len:=string_to_vdi(filename, @_intin[0]);
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=len;
  _contrl[5]:=100;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_offset(handle, offset: smallint);
begin
  _intin[0]:=offset;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=101;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_fontinit(handle: smallint; var fh: TFONT_HDR);
begin
  PPointer(@_intin[0])^:=@fh;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=102;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_escape2000(handle, times: smallint);
begin
  _intin[0]:=times;
  _contrl[0]:=5;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=2000;
  _contrl[6]:=handle;

  vdi;
end;


procedure v_pline(handle: smallint; count: smallint; pxyarray: psmallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := PVDIPtsIn(pxyarray);
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=6;
  _contrl[1]:=count;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure v_pline(handle, count: smallint; const pxyarray: Array of smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := @pxyarray;
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=6;
  _contrl[1]:=count;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure v_bez(handle, count: smallint; xyarr, bezarr: Pointer;
                out extent: ARRAY_4;
                out totpts, totmoves: smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := PVDIIntIn(bezarr);
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := PVDIPtsIn(xyarr);
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=6;
  _contrl[1]:=count;
  _contrl[3]:=(count + 1) shr 1;
  _contrl[5]:=13;
  _contrl[6]:=handle;

  vdi(@pb);

  totpts:=_intout[0];
  totmoves:=_intout[1];
  extent[0]:=_ptsout[0];
  extent[1]:=_ptsout[1];
  extent[2]:=_ptsout[2];
  extent[3]:=_ptsout[3];
end;

procedure v_pmarker(handle, count: smallint; const pxyarray: Array of smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := @pxyarray;
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=7;
  _contrl[1]:=count;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure v_gtext(handle: smallint; x: smallint; y: smallint; outputstring: pchar);
var len: smallint;
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;

  len:=pchar_str_to_vdi(outputstring, @_intin[0]);

  _contrl[0]:=8;
  _contrl[1]:=1;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_gtext(handle, x, y: smallint; const outputstring: string);
var len: smallint;
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;

  len:=string_to_vdi(outputstring, @_intin[0]);

  _contrl[0]:=8;
  _contrl[1]:=1;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_fillarea(handle, count: smallint; const pxyarray: Array of smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := @pxyarray;
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=9;
  _contrl[1]:=count;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure v_bez_fill(handle, count: smallint;
                    xyarr, bezarr: Pointer;
                    out extent: ARRAY_4;
                    out totpts, totmoves: smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := PVDIIntIn(bezarr);
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  pb.ptsin := PVDIPtsIn(xyarr);
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;

  _contrl[0]:=9;
  _contrl[1]:=count;
  _contrl[3]:=(count + 1) shr 1;
  _contrl[5]:=13;
  _contrl[6]:=handle;

  vdi(@pb);

  totpts:=_intout[0];
  totmoves:=_intout[1];
  extent[0]:=_ptsout[0];
  extent[1]:=_ptsout[1];
  extent[2]:=_ptsout[2];
  extent[3]:=_ptsout[3];
end;

procedure v_bar(handle: smallint; pxyarray: psmallint);
begin
  // _ptsin[0..3] = pxyarray[0..3];
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];
  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=1;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_bar(handle: smallint; const pxyarray: ARRAY_4);
begin
  // _ptsin[0..3] = pxyarray[0..3];
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];
  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=1;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_arc(handle, x, y, radius, begang, endang: smallint);
begin
  _intin[0]:=begang;
  _intin[1]:=endang;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=0;
  _ptsin[3]:=0;
  _ptsin[4]:=0;
  _ptsin[5]:=0;
  _ptsin[6]:=radius;
  _ptsin[7]:=0;
  _contrl[0]:=11;
  _contrl[1]:=4;
  _contrl[3]:=2;
  _contrl[5]:=2;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_pieslice(handle, x, y, radius, begang, endang: smallint);
begin
  _intin[0]:=begang;
  _intin[1]:=endang;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=0;
  _ptsin[3]:=0;
  _ptsin[4]:=0;
  _ptsin[5]:=0;
  _ptsin[6]:=radius;
  _ptsin[7]:=0;
  _contrl[0]:=11;
  _contrl[1]:=4;
  _contrl[3]:=2;
  _contrl[5]:=3;
  _contrl[6]:=handle;

  vdi;
end;


procedure v_circle (handle: smallint; x: smallint; y: smallint; radius: smallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=0;
  _ptsin[3]:=0;
  _ptsin[4]:=radius;
  _ptsin[5]:=0;

  _contrl[0]:=11;
  _contrl[1]:=3;
  _contrl[3]:=0;
  _contrl[5]:=4;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_ellipse(handle, x, y, xradius, yradius: smallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=xradius;
  _ptsin[3]:=yradius;

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=5;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_ellarc(handle, x, y, xradius, yradius, begang, endang: smallint);
begin
  _intin[0]:=begang;
  _intin[1]:=endang;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=xradius;
  _ptsin[3]:=yradius;

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=2;
  _contrl[5]:=6;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_ellpie(handle, x, y, xradius, yradius, begang, endang: smallint);
begin
  _intin[0]:=begang;
  _intin[1]:=endang;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=xradius;
  _ptsin[3]:=yradius;

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=2;
  _contrl[5]:=7;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_rbox(handle: smallint; const xyarray: ARRAY_4);
begin
  _ptsin[0]:=xyarray[0];
  _ptsin[1]:=xyarray[1];
  _ptsin[2]:=xyarray[2];
  _ptsin[3]:=xyarray[3];

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=8;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_rfbox(handle: smallint; const xyarray: ARRAY_4);
begin
  _ptsin[0]:=xyarray[0];
  _ptsin[1]:=xyarray[1];
  _ptsin[2]:=xyarray[2];
  _ptsin[3]:=xyarray[3];

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=9;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_justified(handle, x, y: smallint;
        const outputstring: string;
        width, wordspace, charspace: smallint);
var len: smallint;
begin
  {* TODO: handle char_space $8000/$8001 (returns interspace information) *}
  _intin[0]:=wordspace;
  _intin[1]:=charspace;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=width;
  _ptsin[3]:=0;

  len:=string_to_vdi(outputstring, @_intin[2]);

  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=len+2;
  _contrl[4]:=0;
  _contrl[5]:=11;
  _contrl[6]:=handle;

  vdi;
end;

function v_bez_on(handle: smallint): smallint;
begin
  _contrl[0]:=11;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=13;
  _contrl[6]:=handle;
  _intout[0]:=0;

  vdi;

  v_bez_on:=_intout[0];
end;

procedure v_bez_off(handle: smallint);
begin
  _contrl[0]:=11;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=13;
  _contrl[6]:=handle;

  vdi;
end;

procedure vst_height(handle, height: smallint; out char_width, char_height, cell_width, cell_height: smallint);
begin
  _ptsin[0]:=0;
  _ptsin[1]:=height;

  _contrl[0]:=12;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  char_width:=_ptsout[0];
  char_height:=_ptsout[1];
  cell_width:=_ptsout[2];
  cell_height:=_ptsout[3];
end;

function vst_rotation(handle, angle: smallint): smallint;
begin
  _intin[0]:=angle;

  _contrl[0]:=13;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_rotation:=_intout[0];
end;

procedure vs_color(handle: smallint; index: smallint; rgb_in: psmallint);
begin
  _intin[0]:=index;
  _intin[1]:=rgb_in[0];
  _intin[2]:=rgb_in[1];
  _intin[3]:=rgb_in[2];

  _contrl[0]:=14;
  _contrl[1]:=0;
  _contrl[3]:=4;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vs_color(handle, index: smallint; const rgb_in: ARRAY_3);
begin
  _intin[0]:=index;
  _intin[1]:=rgb_in[0];
  _intin[2]:=rgb_in[1];
  _intin[3]:=rgb_in[2];

  _contrl[0]:=14;
  _contrl[1]:=0;
  _contrl[3]:=4;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

function vsl_type(handle, style: smallint): smallint;
begin
  _intin[0]:=style;

  _contrl[0]:=15;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsl_type:=_intout[0];
end;

function vsl_width(handle, width: smallint): smallint;
begin
  _ptsin[0]:=width;
  _ptsin[1]:=0;

  _contrl[0]:=16;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsl_width:=_ptsout[0];
end;

function vsl_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=17;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsl_color:=_intout[0];
end;

function vsm_type(handle, symbol: smallint): smallint;
begin
  _intin[0]:=symbol;

  _contrl[0]:=18;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsm_type:=_intout[0];
end;

function vsm_height(handle, height: smallint): smallint;
begin
  _ptsin[0]:=0;
  _ptsin[1]:=height;

  _contrl[0]:=19;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsm_height:=_ptsout[1];
end;

function vsm_color(handle, color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=20;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsm_color:=_intout[0];
end;

function vst_font(handle, font: smallint): smallint;
begin
  _intin[0]:=font;

  _contrl[0]:=21;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_font:=_intout[0];
end;

function vst_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=22;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_color:=_intout[0];
end;

function vsf_interior(handle, style: smallint): smallint;
begin
  _intin[0]:=style;

  _contrl[0]:=23;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsf_interior:=_intout[0];
end;

function vsf_style(handle, style_index: smallint): smallint;
begin
  _intin[0]:=style_index;

  _contrl[0]:=24;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsf_style:=_intout[0];
end;

function vsf_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=25;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsf_color:=_intout[0];
end;

function vq_color(handle, color_index, set_flag: smallint; out rgb: ARRAY_3): smallint;
begin
  _intin[0]:=color_index;
  _intin[1]:=set_flag;

  _contrl[0]:=26;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  rgb[0]:=_intout[1];
  rgb[1]:=_intout[2];
  rgb[2]:=_intout[3];
  vq_color:=_intout[0];
end;

procedure vrq_locator(handle, x, y: smallint; out xout, yout, term: smallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;

  _contrl[0]:=28;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  xout:=_ptsout[0];
  yout:=_ptsout[1];
  term:=_intout[0];
end;

function vsm_locator(handle, x, y: smallint; out xout, yout, term: smallint): smallint;
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;

  _contrl[0]:=28;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  xout:=_ptsout[0];
  yout:=_ptsout[1];
  term:=_intout[0];

  vsm_locator:=(_contrl[4] shl 1) or (_contrl[2]);
end;

procedure vrq_valuator(handle, valuator_in: smallint; out valuator_out, terminator: smallint);
begin
  _intin[0]:=valuator_in;

  _contrl[0]:=29;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  valuator_out:=_intout[0];
  terminator:=_intout[1];
end;

procedure vsm_valuator(handle, val_in: smallint; out val_out, term, status: smallint);
begin
  _intin[0]:=val_in;

  _contrl[0]:=29;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  val_out:=_intout[0];
  term:=_intout[1];
  status:=_contrl[4];
end;

procedure vrq_choice(handle, ch_in: smallint; out ch_out: smallint);
begin
  _intin[0]:=ch_in;

  _contrl[0]:=30;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  ch_out:=_intout[0];
end;

function vsm_choice(handle: smallint; out choice: smallint): smallint;
begin
  _intin[0]:=choice;

  _contrl[0]:=30;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  choice:=_intout[0];
  vsm_choice:=_contrl[4];
end;

procedure vrq_string(handle, max_length, echo_mode: smallint; const echo_xy: ARRAY_2; out resString: string);
begin
  _intin[0]:=max_length;
  _intin[1]:=echo_mode;
  _ptsin[0]:=echo_xy[0];
  _ptsin[1]:=echo_xy[1];

  _contrl[0]:=31;
  _contrl[1]:=1;
  _contrl[3]:=2;
  _contrl[4]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vdi_to_string(@_intout, resString, _contrl[4]);
end;

function vsm_string(handle, max_length, echo_mode: smallint; const echo_xy: ARRAY_2; out resString: string): smallint;
begin
  _intin[0]:=max_length;
  _intin[1]:=echo_mode;
  _ptsin[0]:=echo_xy[0];
  _ptsin[1]:=echo_xy[1];

  _contrl[0]:=31;
  _contrl[1]:=1;
  _contrl[3]:=2;
  _contrl[4]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vdi_to_string(@_intout, resString, _contrl[4]);
  vsm_string:=_contrl[4];
end;

function vswr_mode(handle: smallint; mode: smallint): smallint;
begin
  _intin[0]:=mode;

  _contrl[0]:=32;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vswr_mode:=_intout[0];
end;

function vsin_mode(handle, dev_type, mode: smallint): smallint;
begin
  _intin[0]:=mode;
  _intin[1]:=dev_type;

  _contrl[0]:=33;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsin_mode:=_intout[0];
end;

procedure vql_attributes(handle: smallint; out attrib: ARRAY_4);
begin
  _contrl[0]:=35;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  attrib[0]:=_intout[0];
  attrib[1]:=_intout[1];
  attrib[2]:=_intout[2];
  attrib[3]:=_ptsout[0];
end;

procedure vql_attributes(handle: smallint; out attrib: ARRAY_6);
begin
  _contrl[0]:=35;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  attrib[0]:=_intout[0];
  attrib[1]:=_intout[1];
  attrib[2]:=_intout[2];
  attrib[3]:=_ptsout[0];
  {* TOS/EmuTOS do not return the line end styles in intout[3/4] *}
  if (_contrl[4] >= 5) then
    begin
      attrib[4]:=_intout[3];
      attrib[5]:=_intout[4];
    end else begin
      attrib[4]:=0;
      attrib[5]:=0;
    end;
end;

procedure vqm_attributes(handle: smallint; out attrib: ARRAY_4);
begin
  _contrl[0]:=36;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  attrib[0]:=_intout[0];
  attrib[1]:=_intout[1];
  attrib[2]:=_intout[2];
  attrib[3]:=_ptsout[0];
end;

procedure vqf_attributes(handle: smallint; out attrib: ARRAY_5);
begin
  _contrl[0]:=37;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  attrib[0]:=_intout[0];
  attrib[1]:=_intout[1];
  attrib[2]:=_intout[2];
  attrib[3]:=_intout[3];
  attrib[4]:=_intout[4];
end;

procedure vqt_attributes(handle: smallint; out attrib: ARRAY_10);
begin
  _contrl[0]:=38;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  attrib[0]:=_intout[0];
  attrib[1]:=_intout[1];
  attrib[2]:=_intout[2];
  attrib[3]:=_intout[3];
  attrib[4]:=_intout[4];
  attrib[5]:=_intout[5];
  attrib[6]:=_ptsout[0];
  attrib[7]:=_ptsout[1];
  attrib[8]:=_ptsout[2];
  attrib[9]:=_ptsout[3];
end;

procedure vst_alignment(handle, hor_in, vert_in: smallint; out hor_out, vert_out: smallint);
begin
  _intin[0]:=hor_in;
  _intin[1]:=vert_in;

  _contrl[0]:=39;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  hor_out:=_intout[0];
  vert_out:=_intout[1];
end;

procedure v_opnvwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  // _intin[0..10] = work_in[0..10];
  pb.intin := PVDIIntIn(work_in);
  pb.ptsin := @_ptsin;
  // work_out[0..44] = intout[0..44];
  pb.intout := PVDIIntOut(work_out);
  // work_out[45..56] = ptsout[0..11];
  pb.ptsout := PVDIPtsOut(@work_out[45]);

  _contrl[0]:=100;
  _contrl[1]:=0;
  _contrl[3]:=11;
  _contrl[5]:=0;
  _contrl[6]:=handle^;

  vdi(@pb);

  handle^:=_contrl[6];
end;

procedure v_clsvwk(handle: smallint);
begin
  _contrl[0]:=101;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vq_extnd(handle, owflag: smallint; WorkOut: psmallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  pb.ptsin := @_ptsin;
  // work_out[0..44] = intout[0..44];
  pb.intout := PVDIIntOut(workout);
  // work_out[45..56] = ptsout[0..11];
  pb.ptsout := PVDIPtsOut(@workout[45]);

  _intin[0]:=owflag;

  _contrl[0]:=102;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;


procedure vq_scrninfo(handle: smallint; out WorkOut: ARRAY_273);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  pb.ptsin := @_ptsin;
  pb.intout := @workout;
  pb.ptsout := @_ptsout;

  _intin[0]:=2;

  _contrl[0]:=102;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=1;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure v_contourfill(handle, x, y, index: smallint);
begin
  _intin[0]:=index;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _contrl[0]:=103;
  _contrl[1]:=1;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

function vsf_perimeter(handle, per_vis: smallint): smallint;
begin
  _intin[0]:=per_vis;
  _contrl[0]:=104;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vsf_perimeter:=_intout[0];
end;

procedure v_get_pixel(handle: smallint; x: smallint; y: smallint;
                      pel: psmallint; index: psmallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _contrl[0]:=105;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  pel^:=_intout[0];
  index^:=_intout[1];
end;

procedure v_get_pixel(handle, x, y: smallint; out pel, index: smallint);
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _contrl[0]:=105;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  pel:=_intout[0];
  index:=_intout[1];
end;

function vst_effects(handle, effect: smallint): smallint;
begin
  _intin[0]:=effect;
  _contrl[0]:=106;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_effects:=_intout[0];
end;

function vst_point(handle, point: smallint; out char_width, char_height, cell_width, cell_height: smallint): smallint;
begin
  _intin[0]:=point;
  _contrl[0]:=107;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  char_width:=_ptsout[0];
  char_height:=_ptsout[1];
  cell_width:=_ptsout[2];
  cell_height:=_ptsout[3];
end;

procedure vsl_ends(handle, beg_style, end_style: smallint);
begin
  _intin[0]:=beg_style;
  _intin[1]:=end_style;
  _contrl[0]:=108;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vro_cpyfm(handle: smallint; vr_mode: smallint; pxyarray: psmallint; psrcMFDB: PMFDB; pdesMFDB: PMFDB);
begin
  _intin[0]:=vr_mode;
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];
  _ptsin[4]:=pxyarray[4];
  _ptsin[5]:=pxyarray[5];
  _ptsin[6]:=pxyarray[6];
  _ptsin[7]:=pxyarray[7];

  PPointer(@_contrl[7])^:=psrcMFDB;
  PPointer(@_contrl[9])^:=pdesMFDB;

  _contrl[0]:=109;
  _contrl[1]:=4;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vr_trnfm(handle: smallint; psrcMFDB, pdesMFDB: PMFDB);
begin
  PPointer(@_contrl[7])^:=psrcMFDB;
  PPointer(@_contrl[9])^:=pdesMFDB;

  _contrl[0]:=110;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vr_trnfm(handle: smallint; const psrcMFDB, pdesMFDB: TMFDB);
begin
  PPointer(@_contrl[7])^:=@psrcMFDB;
  PPointer(@_contrl[9])^:=@pdesMFDB;

  _contrl[0]:=110;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vsc_form(handle: smallint; pcur_form: PMFORM);
var pb: TVDIPB;
begin
  {* TODO: NVDI also returns current form in intout *}
  pb.control := @_contrl;
  pb.intin := PVDIIntIn(pcur_form);
  pb.ptsin := @_ptsin;
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;
  _contrl[0]:=111;
  _contrl[1]:=0;
  _contrl[3]:=37;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure vsf_udpat(handle: smallint; pfill_pat: Pointer; planes: smallint);
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := PVDIIntIn(pfill_pat);
  pb.ptsin := @_ptsin;
  pb.intout := @_intout;
  pb.ptsout := @_ptsout;
  _contrl[0]:=112;
  _contrl[1]:=0;
  _contrl[3]:=planes*16;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure vsl_udsty(handle, pattern: smallint);
begin
  _intin[0]:=pattern;
  _contrl[0]:=113;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vr_recfl(handle: smallint; const pxyarray: ARRAY_4);
begin
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];
  _contrl[0]:=114;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vqin_mode(handle, dev_type: smallint; out input_mode: smallint);
begin
  _intin[0]:=dev_type;
  _contrl[0]:=115;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  input_mode:=_intout[0];
end;

procedure vqt_extent(handle: smallint; calcString: pchar; extent: psmallint);
var len: smallint;
begin
  len:=pchar_str_to_vdi(calcstring, @_intin[0]);
  _contrl[0]:=116;
  _contrl[1]:=0;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  extent[0]:=_ptsout[0];
  extent[1]:=_ptsout[1];
  extent[2]:=_ptsout[2];
  extent[3]:=_ptsout[3];
  extent[4]:=_ptsout[4];
  extent[5]:=_ptsout[5];
  extent[6]:=_ptsout[6];
  extent[7]:=_ptsout[7];
end;

function vqt_width(handle, character: smallint; out cell_width, left_delta, right_delta: smallint): smallint;
begin
  _intin[0]:=character;
  _contrl[0]:=117;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  cell_width:=_ptsout[0];
  left_delta:=_ptsout[2];
  right_delta:=_ptsout[4];
  vqt_width:=_intout[0];
end;

procedure vex_timv(handle: smallint; tim_addr: Pointer; out otim_addr: Pointer; out tim_conv: smallint);
begin
  _contrl[0]:=118;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;
  PPointer(@_contrl[7])^:=tim_addr;

  vdi;

  otim_addr:=PPointer(@_contrl[9])^;
  tim_conv:=_intout[0];
end;

function vst_load_fonts(handle, select: smallint): smallint;
begin
  _intin[0]:=select;

  _contrl[0]:=119;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_load_fonts:=_intout[0];
end;

procedure vst_unload_fonts(handle, select: smallint);
begin
  _intin[0]:=select;

  _contrl[0]:=120;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vrt_cpyfm(handle: smallint; vr_mode: smallint; pxyarray: psmallint; psrcMFDB: PMFDB; pdesMFDB: PMFDB; color_index: psmallint);
begin
  _intin[0]:=vr_mode;
  _intin[1]:=color_index[0];
  _intin[2]:=color_index[1];
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];
  _ptsin[4]:=pxyarray[4];
  _ptsin[5]:=pxyarray[5];
  _ptsin[6]:=pxyarray[6];
  _ptsin[7]:=pxyarray[7];

  PPointer(@_contrl[7])^:=psrcMFDB;
  PPointer(@_contrl[9])^:=pdesMFDB;

  _contrl[0]:=121;
  _contrl[1]:=4;
  _contrl[3]:=3;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_show_c(handle: smallint; reset: smallint);
begin
  _intin[0]:=reset;

  _contrl[0]:=122;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_hide_c(handle: smallint);
begin
  _contrl[0]:=123;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vq_mouse(handle: smallint; out pstatus, x, y: smallint);
begin
  _contrl[0]:=124;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  pstatus:=_intout[0];
  x:=_ptsout[0];
  y:=_ptsout[1];
end;

procedure vex_butv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
begin
  _contrl[0]:=125;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;
  PPointer(@_contrl[7])^:=pusrcode;

  vdi;

  psavcode:=PPointer(@_contrl[9])^;
end;

procedure vex_motv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
begin
  _contrl[0]:=126;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;
  PPointer(@_contrl[7])^:=pusrcode;

  vdi;

  psavcode:=PPointer(@_contrl[9])^;
end;

procedure vex_curv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
begin
  _contrl[0]:=127;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;
  PPointer(@_contrl[7])^:=pusrcode;

  vdi;

  psavcode:=PPointer(@_contrl[9])^;
end;

procedure vq_key_s(handle: smallint; out pstatus: smallint);
begin
  _contrl[0]:=128;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  pstatus:=_intout[0];
end;

procedure vs_clip(handle: smallint; clip_flag: smallint; pxyarray: psmallint);
begin
  _intin[0]:=clip_flag;
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];

  _contrl[0]:=129;
  _contrl[1]:=2;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vs_clip(handle, clip_flag: smallint; const pxyarray: ARRAY_4);
begin
  _intin[0]:=clip_flag;
  _ptsin[0]:=pxyarray[0];
  _ptsin[1]:=pxyarray[1];
  _ptsin[2]:=pxyarray[2];
  _ptsin[3]:=pxyarray[3];

  _contrl[0]:=129;
  _contrl[1]:=2;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vs_clip_off(handle: smallint);
begin
  _intin[0]:=0;
  _ptsin[0]:=0;
  _ptsin[1]:=0;
  _ptsin[2]:=0;
  _ptsin[3]:=0;

  _contrl[0]:=129;
  _contrl[1]:=2;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

function vqt_name(handle, element_num: smallint; out name: String33): smallint;
begin
  _intin[0]:=element_num;

  _contrl[0]:=130;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vdi_to_string(@_intout[1], name, _contrl[4] - 1);

  vqt_name:=_intout[0];
end;

procedure vqt_fontinfo(handle: smallint;
        out minADE, maxADE: smallint;
        out distances: ARRAY_5;
        out maxwidth: smallint;
        out effects: ARRAY_3);
begin
  _contrl[0]:=131;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  minADE:=_intout[0];
  maxADE:=_intout[1];
  maxwidth:=_ptsout[0];
  distances[0]:=_ptsout[1];
  distances[1]:=_ptsout[3];
  distances[2]:=_ptsout[5];
  distances[3]:=_ptsout[7];
  distances[4]:=_ptsout[9];
  effects[0]:=_ptsout[2];
  effects[1]:=_ptsout[4];
  effects[2]:=_ptsout[6];
end;

procedure vqt_justified(handle, x, y: smallint; const outString: String;
            length, word_space, char_space: smallint;
            offsets: Pointer);
var len: smallint;
var pb: TVDIPB;
begin
  pb.control := @_contrl;
  pb.intin := @_intin;
  pb.ptsin := @_ptsin;
  pb.intout := @_intout;
  pb.ptsout := PVDIPtsOut(offsets);
  _intin[0]:=word_space;
  _intin[1]:=char_space;
  len:=string_to_vdi(outstring, @_intin[2]);
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _ptsin[2]:=length;
  _ptsin[3]:=0;
  _contrl[0]:=132;
  _contrl[1]:=2;
  _contrl[3]:=len+2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi(@pb);
end;

procedure vex_wheelv(handle: smallint; pusrcode: Pointer; out psavcode: Pointer);
begin
  _contrl[0]:=134;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;
  PPointer(@_contrl[7])^:=pusrcode;

  vdi;

  psavcode:=PPointer(@_contrl[9])^;
end;


procedure vst_width(handle, width: smallint; out char_width, char_height, cell_width, cell_height: smallint);
begin
  _intin[0]:=width;
  _contrl[0]:=231;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  char_width:=_intout[0];
  char_height:=_intout[1];
  cell_width:=_intout[2];
  cell_height:=_intout[3];
end;

procedure vqt_fontheader(handle: smallint; buffer: Pointer;
                         out pathname: String);
begin
  Ppointer(@_intin[0])^:=buffer;
  _contrl[0]:=232;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vdi_to_string(@_intout[0], pathname, _contrl[4]);
end;

procedure vqt_trackkern(handle: smallint; out x, y: fix31);
begin
  _contrl[0]:=234;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  x:=PLongint(@_intout[0])^;
  y:=PLongint(@_intout[2])^;
end;

procedure vqt_pairkern(handle, ch1, ch2: smallint; out x, y: fix31);
begin
  _intin[0]:=ch1;
  _intin[2]:=ch2;
  _contrl[0]:=235;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  x:=PLongint(@_intout[0])^;
  y:=PLongint(@_intout[2])^;
end;


procedure vst_charmap(handle, mode: smallint);
begin
  _intin[0]:=mode;
  _contrl[0]:=236;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

function vst_map_mode(handle, mode: smallint): smallint;
begin
  _intin[0]:=mode;
  _intin[1]:=1;
  _contrl[0]:=236;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  vst_map_mode:=_intout[0];
end;

procedure vst_kern(handle, tmode, pmode: smallint; out tracks, pairs: smallint);
begin
  _intin[0]:=tmode;
  _intin[1]:=pmode;
  _contrl[0]:=237;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  tracks:=_intout[0];
  pairs:=_intout[0];
end;

procedure vst_track_offset(handle: smallint; offset: fix31; pairmode: smallint; out tracks, pairs: smallint);
begin
  _intin[0]:=255;
  _intin[1]:=pairmode;
  PLongint(@_intin[2])^:=offset;
  _contrl[0]:=237;
  _contrl[1]:=0;
  _contrl[3]:=4;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  tracks:=_intout[0];
  pairs:=_intout[0];
end;

procedure v_getbitmap_info(handle, ch: smallint;
                           out advx, advy, xoff, yoff: fix31;
                           out width, height: smallint;
                           out bitmap: pointer);
begin
  _intin[0]:=ch;
  _contrl[0]:=239;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  width:=_intout[0];
  height:=_intout[1];
  advx:=PLongint(@_intout[2])^;
  advy:=PLongint(@_intout[4])^;
  xoff:=PLongint(@_intout[6])^;
  yoff:=PLongint(@_intout[8])^;
  if (bitmap <> nil) then
    bitmap:=PPointer(@_intout[10])^;
end;


procedure v_ftext(handle, x, y: smallint; const str: String);
var len: longint;
begin
  len:=string_to_vdi(str, @_intin[0]);
  _ptsin[0]:=x;
  _ptsin[0]:=y;
  _contrl[0]:=241;
  _contrl[1]:=1;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_ftext_offset(handle, x, y: smallint;
                         const outputstring: string;
                         const offset: Array of smallint);
var i, len: longint;
    src, dst: psmallint;
begin
  len:=string_to_vdi(outputstring, @_intin[0]);
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  src:=offset;
  dst:=@_ptsin[2];
  for i:=0 to len-1 do
    begin
      dst^:=src^;
      inc(dst);
      inc(src);
      dst^:=src^;
      inc(dst);
      inc(src);
    end;
  _contrl[0]:=241;
  _contrl[1]:=len+1;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_killoutline(handle: smallint; component: Pointer);
begin
  PPointer(@_intin[0])^:=component;
  _contrl[0]:=242;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_getoutline(handle, ch: smallint;
                       const xyarray: Array of smallint;
                       const bezarray: Array of ShortInt;
                       maxverts: smallint;
                       out numverts: smallint);
begin
  _intin[0]:=ch;
  _intin[1]:=maxverts;
  PPointer(@_intin[2])^:=@xyarray;
  PPointer(@_intin[4])^:=@bezarray;
  _contrl[0]:=243;
  _contrl[1]:=0;
  _contrl[3]:=6;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  numverts:=_intout[0];
end;

procedure vst_scratch(handle, mode: smallint);
begin
  _intin[0]:=mode;
  _contrl[0]:=244;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure vst_error(handle, mode: smallint; out errorvar: smallint);
begin
  _intin[0]:=mode;
  PPointer(@_intin[1])^:=@errorvar;
  _contrl[0]:=245;
  _contrl[1]:=0;
  _contrl[3]:=3;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
end;

function vst_arbpt(handle, point: smallint;
                   out chwd, chht, cellwd, cellht: smallint): smallint;
begin
  _intin[0]:=point;
  _contrl[0]:=246;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  chwd:=_ptsout[0];
  chht:=_ptsout[1];
  cellwd:=_ptsout[2];
  cellht:=_ptsout[3];

  vst_arbpt:=_intout[0];
end;

function vst_arbpt32(handle: smallint; point: fix31;
                     out chwd, chht, cellwd, cellht: smallint): fix31;
begin
  PLongint(@_intin[0])^:=point;
  _contrl[0]:=246;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  chwd:=_ptsout[0];
  chht:=_ptsout[1];
  cellwd:=_ptsout[2];
  cellht:=_ptsout[3];

  vst_arbpt32:=PLongint(@_intout[0])^;
end;

procedure vqt_advance(handle, ch: smallint; out advx, advy, remx, remy: smallint);
begin
  _intin[0]:=ch;
  _contrl[0]:=247;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  advx:=_ptsout[0];
  advy:=_ptsout[1];
  remx:=_ptsout[2];
  remy:=_ptsout[3];
end;

procedure vqt_advance32(handle, ch: smallint; out advx, advy: fix31);
begin
  _intin[0]:=ch;
  _contrl[0]:=247;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  advx:=PLongint(@_ptsout[4])^;
  advy:=PLongint(@_ptsout[6])^;
end;

function vq_devinfo(handle, devnum: smallint;
                      out devexists: smallint;
                      out filename: String;
                      out devicename: String): smallint;
var i, len: smallint;
    ch: char;
begin
  _intin[0]:=devnum;
  _contrl[0]:=248;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
  if (_contrl[4] = 0) or (_intout[4] = 0) then
    begin
      devexists:= 0;
      filename := '';
      devicename := '';
      vq_devinfo:=0;
    end
  else
    begin
      (* here, the driver exists *)
      devexists:=1;
      (* set the filename. The value in vdi_intout may be "DRIVER.SYS"
       * or "DRIVER   SYS". vdi_intout is not a nul-terminated string.
       * In both cases, this binding returns a valid filename: "DRIVER.SYS"
       * with a null-character to ended the string.
       *)
      len := 0;
      for i:=0 to _contrl[4]-1 do
        begin
          ch := chr(_intout[i]);
          inc(len);
          filename[len]:=ch;
          if (ch = ' ') and (chr(_intout[i+1]) <> ' ') then
            begin
              inc(len);
              filename[len]:='.';
            end
          else
            begin
              inc(len);
              filename[len]:=ch;
            end
        end;
      setlength(filename, len);

      (* device name in ptsout is a C-String, (a nul-terminated string with 8bits per characters)
       * each short value (vdi_ptsout[x]) contains 2 characters.
       * When ptsout contains a device name, NVDI/SpeedoGDOS seems to always write the value "13"
       * in vdi_control[1] (hey! this should be a read only value from the VDI point of view!!!),
       * and SpeedoGDOS 5 may set vdi_control[2] == 1 (intead of the size of vdi_ptsout, including
       * the device_name). It's seems that this value "13" (written in vdi_control[1]) has missed
       * its target (vdi_control[2]). So, here is a workaround:
       *)
       if (_contrl[2] = 1) and (_contrl[1] > 0) then
         len := _contrl[1] * 2
       else
         len := (_contrl[2] - 1) * 2;
       setlength(devicename, len);
       move(_ptsout[1], devicename[1], len);
       vq_devinfo:=_intout[0];;
    end;

end;

procedure vqt_devinfo(handle, devnum: smallint;
                      out dev_busy: smallint;
                      out filename: String;
                      out devicename: String);
var dummy: smallint;
begin
  dev_busy:=vq_devinfo(handle, devnum, dummy, filename, devicename);
end;

function v_savecache(handle: smallint; const filename: String): smallint;
var len: longint;
begin
  len:=string_to_vdi(filename, @_intin[0]);
  _contrl[0]:=249;
  _contrl[1]:=0;
  _contrl[3]:=len;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  v_savecache:=_intout[0];
end;

function v_loadcache(handle: smallint; const filename: String; mode: smallint): smallint;
var len: longint;
begin
  len:=string_to_vdi(filename, @_intin[1]);
  _intin[0]:=mode;
  _contrl[0]:=250;
  _contrl[1]:=0;
  _contrl[3]:=len+1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  v_loadcache:=_intout[0];
end;

function v_flushcache(handle: smallint): smallint;
begin
  _contrl[0]:=251;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  v_flushcache:=_intout[0];
end;

function vst_setsize(handle, point: smallint;
                     out chwd, chht, cellwd, cellht: smallint): smallint;
begin
  _intin[0]:=point;
  _contrl[0]:=252;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  chwd:=_ptsout[0];
  chht:=_ptsout[1];
  cellwd:=_ptsout[2];
  cellht:=_ptsout[3];
  vst_setsize:=_intout[0];
end;

function  vst_setsize32(handle: smallint; point: fix31;
                        out chwd, chht, cellwd, cellht: smallint): fix31;
begin
  PLongint(@_intin[0])^:=point;
  _contrl[0]:=252;
  _contrl[1]:=0;
  _contrl[3]:=2;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  chwd:=_ptsout[0];
  chht:=_ptsout[1];
  cellwd:=_ptsout[2];
  cellht:=_ptsout[3];
  vst_setsize32:=PLongint(@_intout[0])^;
end;

function vst_skew(handle, skew: smallint): smallint;
begin
  _intin[0]:=skew;
  _contrl[0]:=253;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;
  vst_skew:=_intout[0];
end;

procedure vqt_get_table(handle: smallint; out map: Pointer);
begin
  _contrl[0]:=254;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  map:=PPointer(@_intout[0])^;
end;

procedure vqt_cachesize(handle, which_cache: smallint; out size: LongInt);
begin
  _intin[0]:=which_cache;
  _contrl[0]:=255;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[5]:=0;
  _contrl[6]:=handle;

  vdi;

  size:=PLongint(@_intout[0])^;
end;


procedure v_set_app_buff(handle: smallint; address: Pointer; nparagraphs: smallint);
begin
  PPointer(@_intin[0])^:=address;
  _intin[2]:=nparagraphs;
  _contrl[0]:=-1;
  _contrl[1]:=0;
  _contrl[3]:=3;
  _contrl[5]:=6;
  _contrl[6]:=handle;

  vdi;
end;


end.
