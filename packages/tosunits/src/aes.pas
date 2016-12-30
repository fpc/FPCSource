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


function appl_exit: smallint;
function appl_find(fname: PChar): smallint;
function appl_init: smallint;

function form_alert(default: smallint; alertstr: PChar): smallint;
function form_error(error: smallint): smallint;

function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
function wind_delete(handle: smallint): smallint;
function wind_open(handle: smallint; x, y, w, h: smallint): smallint;

function crys_if(_opcode: dword): smallint;

implementation

const
  ops_table: array[0..120,0..3] of SmallInt = (
    ( 0, 1, 0, 0 ),    // 10, appl_init
    ( 2, 1, 1, 0 ),    // 11, appl_read
    ( 2, 1, 1, 0 ),    // 12, appl_write
    ( 0, 1, 1, 0 ),    // 13, appl_find
    ( 2, 1, 1, 0 ),    // 14, appl_tplay
    ( 1, 1, 1, 0 ),    // 15, appl_trecord
    ( 0, 0, 0, 0 ),    // 16
    ( 0, 0, 0, 0 ),    // 17
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
    ( 2, 1, 2, 0 ),    // 36, menu_popup (V3.3)
    ( 2, 1, 2, 0 ),    // 37, menu_attach (V3.3)
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
    ( 0, 0, 0, 0 ),    // 115, rsrc_rcfix (V4.0)
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
    ( 1, 5, 0, 0 )     // 130, appl_getinfo (V4.0)
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

function appl_find(fname: PChar): smallint;
begin
  _addrin[0]:=fname;
  appl_find:=crys_if($0d);
end;

function appl_init: smallint;
begin
  appl_init:=crys_if($0a);
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


function wind_create(kind: smallint; x, y, w, h: smallint): smallint;
begin
  _intin[0]:=kind;
  _intin[1]:=x;
  _intin[2]:=y;
  _intin[3]:=w;
  _intin[4]:=h;
  wind_create:=crys_if($64);
end;

function wind_delete(handle: smallint): smallint;
begin
  _intin[0]:=handle;
  wind_delete:=crys_if($67);
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
