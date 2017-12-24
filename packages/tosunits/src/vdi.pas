{
    Copyright (c) 2017 by Free Pascal development team

    VDI interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit vdi;

interface

{ The API description of this file is based on the information available
  online at: http://toshyp.atari.org }

type
  PCOLOR_RGB = ^TCOLOR_RGB;
  TCOLOR_RGB = record
      reserved: word;     {* Set to 0 or the index of the entry *}
      red: word;          {* Red:   0<->65535 *}
      green: word;        {* Green: 0<->65535 *}
      blue: word;         {* Blue:  0<->65535 *}
  end;

{$WARNING type TCOLOR_ENTRY is incomplete}
type
  TCOLOR_ENTRY = record
    case byte of
      0: ( rgb: TCOLOR_RGB; );
      1: ( cymk: array[0..1] of longint; ); // dummy
  end;

type
  PCOLOR_TAB = ^TCOLOR_TAB;
  TCOLOR_TAB = record             {* Colour table                    *}
      magic: array[0..3] of char; {* 'ctab'                          *}
      length: longint;
      format: longint;            {* Format (0)                      *}
      reserved: longint;          {* Reserved, set to 0              *}
      map_id: longint;            {* Colour table ID                 *}
      color_space: longint;       {* Colour space (at present only
                                     CSPACE_RGB)                     *}
      flags: longint;             {* VDI-internal flags, set to 0    *}
      no_colors: longint;         {* Number of colour entries        *}
      reserved1: longint;         {* Reserved, must be 0             *}
      reserved2: longint;         {* Reserved, must be 0             *}
      reserved3: longint;         {* Reserved, must be 0             *}
      reserved4: longint;         {* Reserved, must be 0             *}
      colors: array[0..0] of TCOLOR_ENTRY; { repeated no_colors times }
  end;

type
  PPOINT16 = ^TPOINT16;
  TPOINT16 = record               {* Point for 16-bit coordinates *}
      x: smallint;
      y: smallint;
  end;

type
  PPOINT32 = ^TPOINT32;
  TPOINT32 = record               {* Point for 32-bit coordinates *}
      x: longint;
      y: longint;
  end;

type
  PRECT16 = ^TRECT16;
  TRECT16 = record                {* Rectangle for 16-bit coordinates *}
      x1: smallint;
      y1: smallint;
      x2: smallint;
      y2: smallint;
  end;

type
  PRECT32 = ^TRECT32;
  TRECT32 = record                {* Rectangle for 32-bit coordinates *}
      x1: longint;
      y1: longint;
      x2: longint;
      y2: longint;
  end;

type
  PMFDB = ^TMFDB;
  TMFDB = record
      fd_addr: pointer;          {* Pointer to the start of the
                                    memory block, e.g. the
                                    screen memory base address  *}
      fd_w: smallint;            {* Width in pixels             *}
      fd_h: smallint;            {* Height in pixels            *}
      fd_wdwidth: smallint;      {* Width of a line in words    *}
      fd_stand: smallint;        {* 0 = Device-specific format  *}
                                 {* 1 = Standard format         *}
      fd_nplanes: smallint;      {* Number of planes            *}
      fd_r1: smallint;           {* Reserved, must be 0         *}
      fd_r2: smallint;           {* Reserved, must be 0         *}
      fd_r3: smallint;           {* Reserved, must be 0         *}
  end;

type
  PVDIContrl = ^TVDIContrl;
  TVDIContrl = array[0..11] of smallint;

  PVDIPtsIn  = ^TVDIPtsIn;
  TVDIPtsIn  = array[0..1023] of smallint;

  PVDIPtsOut = ^TVDIPtsOut;
  TVDIPtsOut = array[0..255] of smallint;

  PVDIIntIn  = ^TVDIIntIn;
  TVDIIntIn  = array[0..1023] of smallint;

  PVDIIntOut = ^TVDIIntOut;
  TVDIIntOut = array[0..511] of smallint;

type
  PVDIPB = ^TVDIPB;
  TVDIPB = record
      contrl: PVDIContrl;        {* Pointer to contrl array *}
      intin: PVDIIntIn;          {* Pointer to intin array  *}
      ptsin: PVDIPtsIn;          {* Pointer to ptsin array  *}
      intout: PVDIIntOut;        {* Pointer to intout array *}
      ptsout: PVDIPtsOut;        {* Pointer to ptsout array *}
  end;

const
  VDI_TRAP_MAGIC = $73;

procedure vdi;

procedure vdi_str_to_pchar(src: psmallint; des: pchar; len: smallint);
function pchar_str_to_vdi(src: pchar; des: psmallint): longint;

procedure v_opnwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
procedure v_clswk(handle: smallint);

procedure v_pline(handle: smallint; count: smallint; pxyarray: psmallint);

procedure v_gtext(handle: smallint; x: smallint; y: smallint; _string: pchar);

procedure v_bar(handle: smallint; pxyarray: psmallint);
procedure v_circle (handle: smallint; x: smallint; y: smallint; radius: smallint);

function vsl_color(handle: smallint; color_index: smallint): smallint;
function vst_color(handle: smallint; color_index: smallint): smallint;
function vsf_color(handle: smallint; color_index: smallint): smallint;

procedure v_opnvwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
procedure v_clsvwk(handle: smallint);

procedure v_get_pixel(handle: smallint; x: smallint; y: smallint;
                      pel: psmallint; index: psmallint);

procedure v_show_c(handle: smallint; reset: smallint);
procedure v_hide_c(handle: smallint);

procedure vs_clip(handle: smallint; clip_flag: smallint; pxyarray: psmallint);

implementation

var
  _contrl: TVDIContrl;
  _intin: TVDIIntIn;
  _intout: TVDIIntOut;
  _ptsin: TVDIPtsIn;
  _ptsout: TVDIPtsOut;

const
  pblock: TVDIPB = (
    contrl: @_contrl;
    intin: @_intin;
    ptsin: @_ptsin;
    intout: @_intout;
    ptsout: @_ptsout;
  );

procedure vdi; assembler;
asm
  lea.l pblock, a0
  move.l a0, d1
  move.w #VDI_TRAP_MAGIC, d0
  trap #2
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
    des[len]:=ord(src[len]);
    inc(len);
  until (src[len-1] = #0);

  pchar_str_to_vdi:=len-1;
end;

procedure v_opnwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
begin
  // _intin[0..15] = work_in[0..15];
  move(work_in^,_intin,16*sizeof(smallint));

  _contrl[0]:=1;
  _contrl[1]:=0;
  _contrl[3]:=16;
  _contrl[6]:=0;

  vdi;

  handle^:=_contrl[6];
  // work_out[0..44] = intout[0..44];
  // work_out[45..56] = ptsout[0..11];
  move(_intout,work_out[0],45*sizeof(smallint));
  move(_ptsout,work_out[45],12*sizeof(smallint));
end;

procedure v_clswk(handle: smallint);
begin
  _contrl[0]:=2;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_pline(handle: smallint; count: smallint; pxyarray: psmallint);
begin
  // _ptsin[0..2*count-1] = pxyarray[0..2*count-1];
  move(pxyarray^,_ptsin,count*2*sizeof(smallint));

  _contrl[0]:=6;
  _contrl[1]:=count;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_gtext(handle: smallint; x: smallint; y: smallint; _string: pchar);
var
  i: smallint;
begin
  _ptsin[0]:=x;
  _ptsin[1]:=y;

  i:=0;
  repeat
    _intin[i]:=byte(_string[i]);
    inc(i);
  until (_string[i-1] = #0);
  dec(i);

  _contrl[0]:=8;
  _contrl[1]:=1;
  _contrl[3]:=-i;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_bar(handle: smallint; pxyarray: psmallint);
begin
  // _ptsin[0..3] = pxyarray[0..3];
  move(pxyarray^,_ptsin,4*sizeof(smallint));
  _contrl[0]:=11;
  _contrl[1]:=2;
  _contrl[3]:=0;
  _contrl[5]:=1;
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

function vsl_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=17;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[6]:=handle;

  vdi;

  vsl_color:=_intout[0];
end;

function vst_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=22;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[6]:=handle;

  vdi;

  vst_color:=_intout[0];
end;

function vsf_color(handle: smallint; color_index: smallint): smallint;
begin
  _intin[0]:=color_index;

  _contrl[0]:=25;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[6]:=handle;

  vdi;

  vsf_color:=_intout[0];
end;


procedure v_opnvwk(work_in: psmallint; handle: psmallint; work_out: psmallint);
begin
  // _intin[0..10] = work_in[0..10];
  move(work_in^,_intin,11*sizeof(smallint));

  _contrl[0]:=100;
  _contrl[1]:=0;
  _contrl[3]:=11;
  _contrl[6]:=handle^;

  vdi;

  handle^:=_contrl[6];
  // work_out[0..44] = intout[0..44];
  // work_out[45..56] = ptsout[0..11];
  move(_intout,work_out[0],45*sizeof(smallint));
  move(_ptsout,work_out[45],12*sizeof(smallint));
end;

procedure v_clsvwk(handle: smallint);
begin
  _contrl[0]:=101;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_get_pixel(handle: smallint; x: smallint; y: smallint;
                      pel: psmallint; index: psmallint);
begin
  _ptsin[0]:=x;
  _ptsin[0]:=x;
  _ptsin[1]:=y;
  _contrl[0]:=105;
  _contrl[1]:=1;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;

  pel^:=_intout[0];
  index^:=_intout[1];
end;

procedure v_show_c(handle: smallint; reset: smallint);
begin
  _intin[0]:=reset;

  _contrl[0]:=122;
  _contrl[1]:=0;
  _contrl[3]:=1;
  _contrl[6]:=handle;

  vdi;
end;

procedure v_hide_c(handle: smallint);
begin
  _contrl[0]:=123;
  _contrl[1]:=0;
  _contrl[3]:=0;
  _contrl[6]:=handle;

  vdi;
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
  _contrl[6]:=handle;

  vdi;
end;


end.
