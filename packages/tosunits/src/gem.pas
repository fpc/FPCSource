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

unit gem;

interface

uses aes, vdi, sysutils;

const
		LWhite			= DWHITE;
		LBlack			= DBLACK;
		LRed			= DRED;
		LGreen			= DGREEN;
		LBlue			= DBLUE;
		LCyan			= DCYAN;
		LYellow			= DYELLOW;
		LMagenta		= DMAGENTA;

type
	AESPB = TAESPB;
	AESPBPtr = ^AESPB;
	AESOBJECT = TAESOBJECT;
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
	MFDB = TMFDB;
	MFDBPtr = ^MFDB;

procedure SetFreeString(tree: PAESTree; obj: smallint; const str: String);
procedure GetFreeString(tree: PAESTree; obj: smallint; var str: String);
procedure SetPtext(tree: PAESTree; obj: smallint; const str: String);
procedure GetPtext(tree: PAESTree; obj: smallint; var str: String);
procedure SetPtmplt(tree: PAESTree; obj: smallint; const str: String);
procedure GetPtmplt(tree: PAESTree; obj: smallint; var str : String);
procedure SetPvalid(tree: PAESTree; obj: smallint; const str: String);
procedure GetPvalid(tree: PAESTree; obj: smallint; var str: String);
procedure SetIcontext(tree: PAESTree; obj: smallint; const str: String);
procedure GetIcontext(tree: PAESTree; obj: smallint; var str: String);
procedure WindSetTitle(handle: smallint; const str: String; var buf: String);
procedure WindSetInfo(handle: smallint; const str: String; var buf: String);
procedure WindSetNewDesk(tree: PAESTree; firstObj: smallint);

implementation

procedure SetFreeString(tree: PAESTree; obj: smallint; const str: String);
begin
  StrPCopy(tree^[obj].ob_spec.free_string, str);
end;

procedure GetFreeString(tree: PAESTree; obj: smallint; var str: String);
begin
  str := StrPas(tree^[obj].ob_spec.free_string);
end;

procedure SetPtext(tree: PAESTree; obj: smallint; const str: String);
begin
  StrPCopy(tree^[obj].ob_spec.ted_info^.te_ptext, str);
end;

procedure GetPtext(tree: PAESTree; obj: smallint; var str: String);
begin
  str := StrPas(tree^[obj].ob_spec.ted_info^.te_ptext);
end;

procedure SetPtmplt(tree: PAESTree; obj: smallint; const str: String);
begin
  StrPCopy(tree^[obj].ob_spec.ted_info^.te_ptmplt, str);
end;

procedure GetPtmplt(tree: PAESTree; obj: smallint; var str : String);
begin
  str := StrPas(tree^[obj].ob_spec.ted_info^.te_ptmplt);
end;

procedure SetPvalid(tree: PAESTree; obj: smallint; const str: String);
begin
  StrPCopy(tree^[obj].ob_spec.ted_info^.te_pvalid, str);
end;

procedure GetPvalid(tree: PAESTree; obj: smallint; var str: String);
begin
  str := StrPas(tree^[obj].ob_spec.ted_info^.te_pvalid);
end;

procedure SetIcontext(tree: PAESTree; obj: smallint; const str: String);
begin
  StrPCopy(tree^[obj].ob_spec.icon_blk^.ib_ptext, str);
end;

procedure GetIcontext(tree: PAESTree; obj: smallint; var str: String);
begin
  str := StrPas(tree^[obj].ob_spec.icon_blk^.ib_ptext);
end;

procedure WindSetTitle(handle: smallint; const str: String; var buf: String);
var pstr: Pchar;
begin
  pstr := @buf[0];
  StrPCopy(pstr, str);
  wind_set(handle, WF_NAME, Pointer(pstr));
end;

procedure WindSetInfo(handle: smallint; const str: String; var buf: String);
var pstr: Pchar;
begin
  pstr := @buf[0];
  StrPCopy(pstr, str);
  wind_set(handle, WF_INFO, Pointer(pstr));
end;

procedure WindSetNewDesk(tree: PAESTree; firstObj: smallint);
begin
  wind_set(0, WF_NEWDESK, dword(tree) shr 16, word(tree), firstObj, 0);
end;

end.
