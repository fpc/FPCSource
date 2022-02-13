{
    Copyright (c) 2022 by Free Pascal development team

    GEM interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    This is used for Pure-Pascal compatibility. For newly written code,
    consider using the aes/vdi units instead.
}

{$MODE FPC}
{$MODESWITCH OUT+}
{$PACKRECORDS 2}

unit gem;

interface

uses aes, vdi, gemcmmn;

const
        LWhite          = DWHITE;
        LBlack          = DBLACK;
        LRed            = DRED;
        LGreen          = DGREEN;
        LBlue           = DBLUE;
        LCyan           = DCYAN;
        LYellow         = DYELLOW;
        LMagenta        = DMAGENTA;

const
        BackSpace       = $0E08;
        Tab             = $0F09;
        S_Delete        = $537F;
        S_Insert        = $5200;
        Shift_Ins       = $5230;
        Return          = $1C0D;
        Enter           = $720D;
        Undo            = $6100;
        Help            = $6200;
        Home            = $4700;
        Cur_Up          = $4800;
        Cur_Down        = $5000;
        Cur_Left        = $4B00;
        Cur_Right       = $4D00;
        Shift_Home      = $4737;
        Shift_CU        = $4838;
        Shift_CD        = $5032;
        Shift_CL        = $4B34;
        Shift_CR        = $4D36;
        Esc             = $011B;
        Ctrl_A          = $1E01;
        Ctrl_B          = $3002;
        Ctrl_C          = $2E03;
        Ctrl_D          = $2004;
        Ctrl_E          = $1205;
        Ctrl_F          = $2106;
        Ctrl_G          = $2207;
        Ctrl_H          = $2308;
        Ctrl_I          = $1709;
        Ctrl_J          = $240A;
        Ctrl_K          = $250B;
        Ctrl_L          = $260C;
        Ctrl_M          = $320D;
        Ctrl_N          = $310E;
        Ctrl_O          = $180F;
        Ctrl_P          = $1910;
        Ctrl_Q          = $1011;
        Ctrl_R          = $1312;
        Ctrl_S          = $1F13;
        Ctrl_T          = $1414;
        Ctrl_U          = $1615;
        Ctrl_V          = $2F16;
        Ctrl_W          = $1117;
        Ctrl_X          = $2D18;
        Ctrl_Y          = $2C19;
        Ctrl_Z          = $151A;
        Ctrl_1          = $0211;
        Ctrl_2          = $0300;
        Ctrl_3          = $0413;
        Ctrl_4          = $0514;
        Ctrl_5          = $0615;
        Ctrl_6          = $071E;
        Ctrl_7          = $0817;
        Ctrl_8          = $0918;
        Ctrl_9          = $0A19;
        Ctrl_0          = $0B10;
        Alt_A           = $1E00;
        Alt_B           = $3000;
        Alt_C           = $2E00;
        Alt_D           = $2000;
        Alt_E           = $1200;
        Alt_F           = $2100;
        Alt_G           = $2200;
        Alt_H           = $2300;
        Alt_I           = $1700;
        Alt_J           = $2400;
        Alt_K           = $2500;
        Alt_L           = $2600;
        Alt_M           = $3200;
        Alt_N           = $3100;
        Alt_O           = $1800;
        Alt_P           = $1900;
        Alt_Q           = $1000;
        Alt_R           = $1300;
        Alt_S           = $1F00;
        Alt_T           = $1400;
        Alt_U           = $1600;
        Alt_V           = $2F00;
        Alt_W           = $1100;
        Alt_X           = $2D00;
        Alt_Y           = $2C00;
        Alt_Z           = $1500;
        Alt_1           = $7800;
        Alt_2           = $7900;
        Alt_3           = $7A00;
        Alt_4           = $7B00;
        Alt_5           = $7C00;
        Alt_6           = $7D00;
        Alt_7           = $7E00;
        Alt_8           = $7F00;
        Alt_9           = $8000;
        Alt_0           = $8100;
        F1              = $3B00;
        F2              = $3C00;
        F3              = $3D00;
        F4              = $3E00;
        F5              = $3F00;
        F6              = $4000;
        F7              = $4100;
        F8              = $4200;
        F9              = $4300;
        F10             = $4400;
        Shift_F1        = $5400;
        Shift_F2        = $5500;
        Shift_F3        = $5600;
        Shift_F4        = $5700;
        Shift_F5        = $5800;
        Shift_F6        = $5900;
        Shift_F7        = $5A00;
        Shift_F8        = $5B00;
        Shift_F9        = $5C00;
        Shift_F10       = $5D00;

        Ctrl_AE         = $2804;
        Ctrl_OE         = $2714;
        Ctrl_UE         = $1A01;
        Alt_AE          = $285D;
        Alt_OE          = $275B;
        Alt_UE          = $1A40;
        SH_Alt_AE       = $287D;
        SH_Alt_OE       = $277B;
        SH_Alt_UE       = $1A5C;



type
  control_ARRAY	= ARRAY[0..4] of smallint;
  AESPBPtr = ^AESPB;
  AESPB = record
    control: ^control_ARRAY;
    global: PAESGlobal;
    intin: PAESIntIn;
    intout: PAESIntOut;
    addrin: PAESAddrIn;
    addrout: PAESAddrOut;
  end;
    AESOBJECT = TAESOBJECT;
    AESOBJECTPtr = ^TAESOBJECT;
    VDIPB = TVDIPB;
    VDIPBPtr = ^VDIPB;
    TEDINFO = TTEDINFO;
    TEDINFOPtr = ^TEDINFO;
    ICONBLK = TICONBLK;
    ICONBLKPtr = ^ICONBLK;
    CICON = TCICON;
    CICONPtr = ^CICON;
    CICONBLK = TCICONBLK;
    CICONBLKPtr = ^CICONBLK;
    BITBLK = TBITBLK;
    BITBLKPtr = ^BITBLK;
    MFORM = TMFORM;
    MFORMPtr = ^MFORM;
    USERBLK = TUSERBLK;
    USERBLKPtr = ^USERBLK;
    OBSPEC = TOBSPEC;
    OBSPECPtr = ^OBSPEC;
    PARMBLK = TPARMBLK;
    PARMBLKPtr = ^PARMBLK;
    AESTree = TAESTree;
    AESTreePtr = ^AESTree;
    RSHDR = TRSHDR;
    RSHDRPtr = ^RSHDR;
    EVENT = TEVENT;
    EVENTPtr = ^EVENT;
    MENU = TMENU;
    MENUPtr = ^MENU;
    MN_SET = TMN_SET;
    MN_SETPtr = ^MN_SET;
    FONT_HDR = TFONT_HDR;
    FONT_HDRPtr = ^FONT_HDR;
    MFDB = vdi.TMFDB;
    MFDBPtr = ^MFDB;

    global_ARRAY    = TAESGlobal;

    workout_ARRAY   = ARRAY[0..56] of smallint;
    workin_ARRAY    = ARRAY[0..10] of smallint;
    intin_ARRAY     = TVDIIntIn;
    intout_ARRAY    = TVDIIntOut;
    ptsin_ARRAY     = TVDIPtsIn;
    ptsout_ARRAY    = TVDIPtsOut;

(*
 * PurePascal has all the AES parameter arrays exposed.
 * We don't want to do that, because various arrays are
 * implementation specific. For Compatibility we
 * need to make the global array available however;
 *)
type
        AES_block = record
            global  : TAESGlobal;
        end;

var Gem_pb: AES_block; external name 'aes_global';

(*
 * we also need to make the parameter block available,
 * so applications can define missing functions
 * that are not yet implemented here.
 *)

var
    AES_pb: AESPB; external name 'aespb';
    VDI_pb: TVDIPB; external name 'vdipb';

{*
 * overloaded AES functions that take an AESTreePtr as parameter
 *}
function menu_bar(me_btree: AESTreePtr; me_bshow: smallint): smallint; overload;
function menu_icheck(me_ctree: AESTreePtr; me_citem, me_ccheck: smallint): smallint; overload;
function menu_ienable(me_etree: AESTreePtr; me_eitem, me_eenable: smallint): smallint; overload;
function menu_tnormal(me_ntree: AESTreePtr; me_ntitle, me_nnormal: smallint): smallint; overload;
function menu_text(me_ttree: AESTreePtr; me_titem: smallint; me_ttext: String): smallint; overload;
function menu_attach(me_flag: smallint; me_tree: AESTreePtr; me_item: smallint; me_mdata: PMENU): smallint; overload;
function menu_istart(me_flag: smallint; me_tree: AESTreePtr; me_imenu, me_item: smallint): smallint; overload;

function objc_add(ob_atree: AESTreePtr;	ob_aparent, ob_achild: smallint): smallint; overload;
function objc_delete(ob_dltree: AESTreePtr; ob_dlobject: smallint): smallint; overload;
function objc_draw(ob_drtree: AESTreePtr;
                   ob_drstartob, ob_drdepth,
                   ob_drxclip, ob_dryclip,
                   ob_drwclip, ob_drhclip: smallint): smallint; overload;
function objc_find(ob_ftree: AESTreePtr;
                   ob_fstartob, ob_fdepth,
                   ob_fmx, ob_fmy: smallint): smallint; overload;
function objc_offset(ob_oftree: AESTreePtr;
                     ob_ofobject: smallint;
                     out ob_ofxoff, ob_ofyoff: smallint): smallint; overload;
function objc_order(ob_ortree: AESTreePtr;
                    ob_orobject, ob_ornewpos: smallint): smallint; overload;
function objc_edit(ob_edtree: AESTreePtr;
                   ob_edobject, ob_edchar: smallint;
                   var ob_edidx: smallint;
                   ob_edkind: smallint): smallint; overload;
function objc_change(ob_ctree: AESTreePtr;
                     ob_cobject, ob_cresvd,
                     ob_xclip, ob_yclip,
                     ob_wclip, ob_hclip,
                     ob_cnewstate,
                     ob_credraw: smallint): smallint; overload;

function form_do(fo_dotree: AESTreePtr;	fo_dostartob: smallint): smallint; overload;
function form_center(fo_ctree: AESTreePtr; out fo_cx, fo_cy, fo_cw, fo_ch: smallint): smallint; overload;
function form_keybd(fo_ktree: AESTreePtr;
                    fo_kobject, fo_kobnext, fo_kchar: smallint;
                    out fo_knxtobject, fo_knxtchar: smallint): smallint; overload;
function form_button(fo_btree: AESTreePtr; fo_bobject, fo_bclicks: smallint;
                     out fo_bnxtobj: smallint): smallint; overload;

function rsrc_obfix(re_obtree: AESTreePtr; re_oobject: smallint): smallint; overload;

{*
 * overloaded VDI functions
 *}
procedure v_opnwk(const WorkIn: workin_Array; out handle: smallint; out WorkOut: workout_Array); overload;
procedure v_opnvwk(const WorkIn: workin_Array; var handle: smallint; out WorkOut: workout_Array); overload;
procedure vq_extnd(handle, owflag: smallint; out WorkOut: workout_Array); overload;
procedure vro_cpyfm(handle, vr_mode: smallint; const pxyarray: ARRAY_8; const psrcMFDB, pdesMFDB: TMFDB); overload;
procedure vrt_cpyfm(handle, vr_mode: smallint; const pxyarray: ARRAY_8; const psrcMFDB, pdesMFDB: TMFDB; const color_index: ARRAY_2); overload;
procedure vqt_extent(handle: smallint; const calcString: String; out extent: ARRAY_8); overload;
procedure vqt_f_extent(handle: smallint; const str: String; out extent: ARRAY_8);

{*
 * Utility functions
 *}
procedure SetFreeString(tree: PAESTree; obj: smallint; const str: String);
procedure GetFreeString(tree: PAESTree; obj: smallint; out str: String);
procedure SetPtext(tree: PAESTree; obj: smallint; const str: String);
procedure GetPtext(tree: PAESTree; obj: smallint; out str: String);
procedure SetPtmplt(tree: PAESTree; obj: smallint; const str: String);
procedure GetPtmplt(tree: PAESTree; obj: smallint; out str :String);
procedure SetPvalid(tree: PAESTree; obj: smallint; const str: String);
procedure GetPvalid(tree: PAESTree; obj: smallint; out str: String);
procedure SetIcontext(tree: PAESTree; obj: smallint; const str: String);
procedure GetIcontext(tree: PAESTree; obj: smallint; out str: String);
procedure WindSetTitle(handle: smallint; const str: String; var buf: String);
procedure WindSetInfo(handle: smallint; const str: String; var buf: String);
procedure WindSetNewDesk(tree: PAESTree; firstObj: smallint);

implementation

type
  aesstr = array[0..255] of char;


function string_to_vdi(const src: String; dst: psmallint): smallint;
var
  i, len: longint;
begin
  len:=length(src);
  for i:=0 to len-1 do
    dst[i]:=byte(src[i + 1]);

  string_to_vdi:=len;
end;


procedure v_opnwk(const WorkIn: workin_Array; out handle: smallint; out WorkOut: workout_Array);
begin
  vdi.v_opnwk(@workin[0], @handle, @workout[0]);
end;


procedure v_opnvwk(const WorkIn: workin_Array; var handle: smallint; out WorkOut: workout_Array);
begin
  vdi.v_opnvwk(@workin[0], @handle, @workout[0]);
end;


procedure vq_extnd(handle, owflag: smallint; out WorkOut: workout_Array);
begin
  vdi.vq_extnd(handle, owflag, @workout[0]);
end;

procedure vro_cpyfm(handle, vr_mode: smallint; const pxyarray: ARRAY_8; const psrcMFDB, pdesMFDB: TMFDB);
begin
  vdi.vro_cpyfm(handle, vr_mode, @pxyarray, @psrcMFDB, @pdesMFDB);
end;

procedure vrt_cpyfm(handle, vr_mode: smallint; const pxyarray: ARRAY_8; const psrcMFDB, pdesMFDB: TMFDB; const color_index: ARRAY_2);
begin
  vdi.vrt_cpyfm(handle, vr_mode, @pxyarray, @psrcMFDB, @pdesMFDB, @color_index);
end;


procedure vqt_extent(handle: smallint; const calcString: String; out extent: ARRAY_8);
var len: smallint;
begin
  len:=string_to_vdi(calcstring, @vdi_pb.intin^[0]);
  vdi_pb.control^[0]:=116;
  vdi_pb.control^[1]:=0;
  vdi_pb.control^[3]:=len;
  vdi_pb.control^[5]:=0;
  vdi_pb.control^[6]:=handle;

  vdi.vdi;

  extent[0]:=vdi_pb.ptsout^[0];
  extent[1]:=vdi_pb.ptsout^[1];
  extent[2]:=vdi_pb.ptsout^[2];
  extent[3]:=vdi_pb.ptsout^[3];
  extent[4]:=vdi_pb.ptsout^[4];
  extent[5]:=vdi_pb.ptsout^[5];
  extent[6]:=vdi_pb.ptsout^[6];
  extent[7]:=vdi_pb.ptsout^[7];
end;

procedure vqt_f_extent(handle: smallint; const str: String;
                       out extent: ARRAY_8);
var len: longint;
begin
  len:=string_to_vdi(str, @vdi_pb.intin^[0]);
  vdi_pb.control^[0]:=240;
  vdi_pb.control^[1]:=0;
  vdi_pb.control^[3]:=len;
  vdi_pb.control^[5]:=0;
  vdi_pb.control^[6]:=handle;

  vdi.vdi;

  extent[0]:=vdi_pb.ptsout^[0];
  extent[1]:=vdi_pb.ptsout^[1];
  extent[2]:=vdi_pb.ptsout^[2];
  extent[3]:=vdi_pb.ptsout^[3];
  extent[4]:=vdi_pb.ptsout^[4];
  extent[5]:=vdi_pb.ptsout^[5];
  extent[6]:=vdi_pb.ptsout^[6];
  extent[7]:=vdi_pb.ptsout^[7];
end;


function menu_bar(me_btree: AESTreePtr; me_bshow: smallint): smallint;
begin
  menu_bar := aes.menu_bar(@me_btree[0], me_bshow);
end;

function menu_icheck(me_ctree: AESTreePtr; me_citem, me_ccheck: smallint): smallint;
begin
  menu_icheck := aes.menu_icheck(@me_ctree[0], me_citem, me_ccheck);
end;

function menu_ienable(me_etree: AESTreePtr; me_eitem, me_eenable: smallint): smallint;
begin
  menu_ienable := aes.menu_ienable(@me_etree[0], me_eitem, me_eenable);
end;

function menu_tnormal(me_ntree: AESTreePtr; me_ntitle, me_nnormal: smallint): smallint;
begin
  menu_tnormal := aes.menu_tnormal(@me_ntree[0], me_ntitle, me_nnormal);
end;

function menu_text(me_ttree: AESTreePtr; me_titem: smallint; me_ttext: String): smallint;
var s: aesstr;
begin
  s:=me_ttext;
  menu_text:=aes.menu_text(@me_ttree[0], me_titem, @s);
end;

function menu_attach(me_flag: smallint; me_tree: AESTreePtr; me_item: smallint; me_mdata: PMENU): smallint;
begin
  menu_attach:=aes.menu_attach(me_flag, @me_tree[0], me_item, me_mdata);
end;

function menu_istart(me_flag: smallint; me_tree: AESTreePtr; me_imenu, me_item: smallint): smallint; overload;
begin
  menu_istart:=aes.menu_istart(me_flag, @me_tree[0], me_imenu, me_item);
end;


function objc_add(ob_atree: AESTreePtr;	ob_aparent, ob_achild: smallint): smallint;
begin
  objc_add:=aes.objc_add(@ob_atree[0], ob_aparent, ob_achild);
end;

function objc_delete(ob_dltree: AESTreePtr; ob_dlobject: smallint): smallint;
begin
  objc_delete:=aes.objc_delete(@ob_dltree[0], ob_dlobject);
end;

function objc_draw(ob_drtree: AESTreePtr;
                   ob_drstartob, ob_drdepth,
                   ob_drxclip, ob_dryclip,
                   ob_drwclip, ob_drhclip: smallint): smallint;
begin
  objc_draw:=aes.objc_draw(@ob_drtree[0], ob_drstartob, ob_drdepth, ob_drxclip, ob_dryclip, ob_drwclip, ob_drhclip);
end;

function objc_find(ob_ftree: AESTreePtr;
                   ob_fstartob, ob_fdepth,
                   ob_fmx, ob_fmy: smallint): smallint;
begin
  objc_find:=aes.objc_find(@ob_ftree[0], ob_fstartob, ob_fdepth, ob_fmx, ob_fmy);
end;

function objc_offset(ob_oftree: AESTreePtr;
                     ob_ofobject: smallint;
                     out ob_ofxoff, ob_ofyoff: smallint): smallint;
begin
  objc_offset:=aes.objc_offset(@ob_oftree[0], ob_ofobject, ob_ofxoff, ob_ofyoff);
end;

function objc_order(ob_ortree: AESTreePtr;
                    ob_orobject, ob_ornewpos: smallint): smallint;
begin
  objc_order:=aes.objc_order(@ob_ortree[0], ob_orobject, ob_ornewpos);
end;

function objc_edit(ob_edtree: AESTreePtr;
                   ob_edobject, ob_edchar: smallint;
                   var ob_edidx: smallint;
                   ob_edkind: smallint): smallint;
begin
  objc_edit:=aes.objc_edit(@ob_edtree[0], ob_edobject, ob_edchar, ob_edidx, ob_edkind);
end;

function objc_change(ob_ctree: AESTreePtr;
                     ob_cobject, ob_cresvd,
                     ob_xclip, ob_yclip,
                     ob_wclip, ob_hclip,
                     ob_cnewstate,
                     ob_credraw: smallint): smallint;
begin
  objc_change:=aes.objc_change(@ob_ctree[0], ob_cobject, ob_cresvd, ob_xclip, ob_yclip, ob_wclip, ob_hclip, ob_cnewstate, ob_credraw);
end;

function form_do(fo_dotree: AESTreePtr;	fo_dostartob: smallint): smallint;
begin
  form_do:=aes.form_do(@fo_dotree[0], fo_dostartob);
end;

function form_center(fo_ctree: AESTreePtr; out fo_cx, fo_cy, fo_cw, fo_ch: smallint): smallint;
begin
  form_center:=aes.form_center(@fo_ctree[0], fo_cx, fo_cy, fo_cw, fo_ch);
end;

function form_keybd(fo_ktree: AESTreePtr;
                    fo_kobject, fo_kobnext, fo_kchar: smallint;
                    out fo_knxtobject, fo_knxtchar: smallint): smallint;
begin
  form_keybd:=aes.form_keybd(@fo_ktree[0], fo_kobject, fo_kobnext, fo_kchar, fo_knxtobject, fo_knxtchar);
end;

function form_button(fo_btree: AESTreePtr; fo_bobject, fo_bclicks: smallint;
                     out fo_bnxtobj: smallint): smallint;
begin
  form_button:=aes.form_button(@fo_btree[0], fo_bobject, fo_bclicks, fo_bnxtobj);
end;

function rsrc_obfix(re_obtree: AESTreePtr; re_oobject: smallint): smallint;
begin
  rsrc_obfix:=aes.rsrc_obfix(@re_obtree[0], re_oobject);
end;


procedure SetFreeString(tree: PAESTree; obj: smallint; const str: String);
var len: SizeInt;
    p: pchar;
begin
  len:=length(str);
  p:=tree^[obj].ob_spec.free_string;
  move(str[1], p^, len);
  p[len]:=#0;
end;

procedure GetFreeString(tree: PAESTree; obj: smallint; out str: String);
begin
  str := tree^[obj].ob_spec.free_string;
end;

procedure SetPtext(tree: PAESTree; obj: smallint; const str: String);
var len: SizeInt;
    p: pchar;
begin
  len:=length(str);
  p:=tree^[obj].ob_spec.ted_info^.te_ptext;
  if (len >= tree^[obj].ob_spec.ted_info^.te_txtlen) then
    len := tree^[obj].ob_spec.ted_info^.te_txtlen-1;
  move(str[1], p^, len);
  p[len]:=#0;
end;

procedure GetPtext(tree: PAESTree; obj: smallint; out str: String);
begin
  str := tree^[obj].ob_spec.ted_info^.te_ptext;
end;

procedure SetPtmplt(tree: PAESTree; obj: smallint; const str: String);
var len: SizeInt;
    p: pchar;
begin
  len:=length(str);
  p:=tree^[obj].ob_spec.ted_info^.te_ptmplt;
  if (len >= tree^[obj].ob_spec.ted_info^.te_tmplen) then
    len := tree^[obj].ob_spec.ted_info^.te_tmplen-1;
  move(str[1], p^, len);
  p[len]:=#0;
end;

procedure GetPtmplt(tree: PAESTree; obj: smallint; out str: String);
begin
  str := tree^[obj].ob_spec.ted_info^.te_ptmplt;
end;

procedure SetPvalid(tree: PAESTree; obj: smallint; const str: String);
var len: SizeInt;
    p: pchar;
begin
  len:=length(str);
  p:=tree^[obj].ob_spec.ted_info^.te_pvalid;
  move(str[1], p^, len);
  p[len]:=#0;
end;

procedure GetPvalid(tree: PAESTree; obj: smallint; out str: String);
begin
  str := tree^[obj].ob_spec.ted_info^.te_pvalid;
end;

procedure SetIcontext(tree: PAESTree; obj: smallint; const str: String);
var len: SizeInt;
    p: pchar;
begin
  len:=length(str);
  p:=tree^[obj].ob_spec.icon_blk^.ib_ptext;
  move(str[1], p^, len);
  p[len]:=#0;
end;

procedure GetIcontext(tree: PAESTree; obj: smallint; out str: String);
begin
  str := tree^[obj].ob_spec.icon_blk^.ib_ptext;
end;

procedure WindSetTitle(handle: smallint; const str: String; var buf: String);
var len: SizeInt;
    pstr: Pchar;
begin
  pstr := @buf[0];
  len:=length(str);
  move(str[1], pstr^, len);
  pstr[len]:=#0;
  wind_set(handle, WF_NAME, Pointer(pstr));
end;

procedure WindSetInfo(handle: smallint; const str: String; var buf: String);
var len: SizeInt;
    pstr: Pchar;
begin
  pstr := @buf[0];
  len:=length(str);
  move(str[1], pstr^, len);
  pstr[len]:=#0;
  wind_set(handle, WF_INFO, Pointer(pstr));
end;

procedure WindSetNewDesk(tree: PAESTree; firstObj: smallint);
begin
{$PUSH}
{$WARN 4055 OFF} { Conversion between ordinals and pointers is not portable }
  wind_set(0, WF_NEWDESK, hi(ptruint(tree)), lo(ptruint(tree)), firstObj, 0);
{$POP}
end;

end.
