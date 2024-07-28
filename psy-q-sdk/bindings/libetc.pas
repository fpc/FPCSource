//  (C) Copyright 1993/1994 Sony Corporation,Tokyo,Japan. All Rights Reserved
// 			libetc.h: Pad Interface
unit libetc;
interface 

var
 	PadIdentifier: integer; external;
// PAD I/O (SIO Pad)
const
	PADLup     = 1 shl 12;
	PADLdown   = 1 shl 14;
	PADLleft   = 1 shl 15;
	PADLright  = 1 shl 13;
	PADRup     = 1 shl  4;
	PADRdown   = 1 shl  6;
	PADRleft   = 1 shl  7;
	PADRright  = 1 shl  5;
	PADi       = 1 shl  9;
	PADj       = 1 shl 10;
	PADk       = 1 shl  8;
	PADl       = 1 shl  3;
	PADm       = 1 shl  1;
	PADn       = 1 shl  2;
	PADo       = 1 shl  0;
	PADh       = 1 shl 11;
	PADL1      = PADn;
	PADL2      = PADo;
	PADR1      = PADl;
	PADR2      = PADm;
	PADstart   = PADh;
	PADselect  = PADk;
			    
	MOUSEleft  = 1 shl 3;
	MOUSEright = 1 shl 2;

// PAD utility macro: _PAD(x,y)
//   x: controller ID (0 or 1)
//   y: PAD assign macro
//
// Example: _PAD(0,PADstart) ... PADstart of controller 1
//          _PAD(1,PADLup)   ... PADLup of controller 2
function _PAD(x, y: longint): longint;

// scratch pad address 0x1f800000 - 0x1f800400 */
function getScratchAddr(offset: dword): dword;


// Video Mode:	NTSC/PAL
const
	MODE_NTSC = 0;
	MODE_PAL  = 1;

function CheckCallback: integer; stdcall; external;
procedure PadInit(mode: integer); stdcall; external;
function ResetCallback: integer; stdcall; external;
function RestartCallback: integer; stdcall; external;
function StopCallback: integer; stdcall; external;
function VSync(mode: integer): integer; stdcall; external;
function VSyncCallback(f: pointer): integer; stdcall; external;
function GetVideoMode: longint; stdcall; external;
function SetVideoMode(mode: longint): longint; stdcall; external;
function PadRead(id: integer): dword; stdcall; external;
procedure PadStop; stdcall; external;


implementation


function _PAD(x, y: longint): longint;
begin
	_PAD:= y shl x shl 4;
end;


function getScratchAddr(offset: dword): dword;
begin
	getScratchAddr:= $1f800000+(offset*4);
end;


begin
end.