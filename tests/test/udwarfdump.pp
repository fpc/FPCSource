unit udwarfdump;
{
    This file is part of the Free Pascal run time library.
    This file is based on lnfodwarf.

    Copyright (c) 2006 by Thomas Schatzl, member of the FreePascal
    Development team
    Parts (c) 2000 Peter Vreman (adapted from original dwarfs line
    reader)
    Dwarf (Symbol) Info reader added by M. Friebe

    Dwarf (Symbol) Info Dumper

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}

{$H+}
{$S-}

interface

uses
  exeinfo, SysUtils, udwarfdata;

type
  TDwarfDumpProc = procedure(AddLine: String);

procedure DumpDwarfInfo(AResProc: TDwarfDumpProc; AFileName:String; AnUnitName: String);

implementation

{$MACRO ON}

  {$define DEBUG_WRITELN := //}
  {$define DEBUG_COMMENT := //}

const
  EBUF_SIZE = 10000;

{$WARNING This code is not thread-safe, and needs improvement}
var
  { the input file to read DWARF debug info from, i.e. paramstr(0) }
  e : TExeFile;
  EBuf: Array [0..EBUF_SIZE-1] of Byte;
  EBufCnt, EBufPos: Integer;
  { the offset and size of the DWARF debug_info section in the file }
  Dwarf_Debug_Info_Section_Offset,
  Dwarf_Debug_Info_Section_Size,
  { the offset and size of the DWARF debug_abbrev section in the file }
  Dwarf_Debug_Abbrev_Section_Offset,
  Dwarf_Debug_Abbrev_Section_Size : longint;

type
  TDebugInfoProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    debug_abbrev_offset : QWord;
    address_size : Byte;
  end;

  TDebugInfoProgramHeader32= packed record
    unit_length : DWord;
    version : Word;
    debug_abbrev_offset : DWord;
    address_size : Byte;
  end;


{---------------------------------------------------------------------------
 I/O utility functions
---------------------------------------------------------------------------}

type
{$ifdef cpui8086}
  TFilePos = LongInt;
{$else cpui8086}
  TFilePos = SizeInt;
{$endif cpui8086}

var
  base, limit : TFilePos;
  index : TFilePos;
  dbgfn : string;


procedure CloseDwarf;
begin
  if e.isopen then
    CloseExeFile(e);
end;

function OpenDwarf(AFileName: String) : boolean;
begin
  // False by default
  OpenDwarf:=false;

  if AFileName = '' then
    AFileName := argv[0];
  if AFileName = '' then
    exit;

  // Close previously opened Dwarf
  CloseDwarf;

  // Open exe file or debug link
  if not OpenExeFile(e,AFileName) then
    exit;
  if ReadDebugLink(e,dbgfn) then
    begin
      CloseExeFile(e);
      if not OpenExeFile(e,dbgfn) then
        exit;
    end;

  // Find debug data section
  if FindExeSection(e,'.debug_info',Dwarf_Debug_Info_Section_offset,dwarf_Debug_Info_Section_size) and
    FindExeSection(e,'.debug_abbrev',Dwarf_Debug_Abbrev_Section_offset,dwarf_Debug_Abbrev_Section_size) then
  begin
    OpenDwarf:=true;
  end
  else
    CloseExeFile(e);
end;




function Init(aBase, aLimit : Int64) : Boolean;
begin
  base := aBase;
  limit := aLimit;
  Init := (aBase + limit) <= e.size;
  seek(e.f, base);
  EBufCnt := 0;
  EBufPos := 0;
  index := 0;
end;

function Pos() : TFilePos;
begin
  Pos := index;
end;

procedure Seek(const newIndex : Int64);
begin
  index := newIndex;
  system.seek(e.f, base + index);
  EBufCnt := 0;
  EBufPos := 0;
end;


{ Returns the next Byte from the input stream, or -1 if there has been
  an error }
function ReadNext() : Longint; inline;
var
  bytesread : SizeInt;
begin
  ReadNext := -1;
  if EBufPos >= EBufCnt then begin
    EBufPos := 0;
    EBufCnt := EBUF_SIZE;
    if EBufCnt > limit - index then
      EBufCnt := limit - index;
    blockread(e.f, EBuf, EBufCnt, bytesread);
    EBufCnt := bytesread;
  end;
  if EBufPos < EBufCnt then begin
    ReadNext := EBuf[EBufPos];
    inc(EBufPos);
    inc(index);
  end
  else
    ReadNext := -1;
end;

{ Reads the next size bytes into dest. Returns true if successful,
  false otherwise. Note that dest may be partially overwritten after
  returning false. }
function ReadNext(var dest; size : SizeInt) : Boolean;
var
  bytesread, totalread : SizeInt;
  r: Boolean;
  d: PByte;
begin
  d := @dest;
  totalread := 0;
  r := True;
  while (totalread < size) and r do begin;
    if EBufPos >= EBufCnt then begin
      EBufPos := 0;
      EBufCnt := EBUF_SIZE;
      if EBufCnt > limit - index then
        EBufCnt := limit - index;
      blockread(e.f, EBuf, EBufCnt, bytesread);
      EBufCnt := bytesread;
      if bytesread <= 0 then
        r := False;
    end;
    if EBufPos < EBufCnt then begin
      bytesread := EBufCnt - EBufPos;
      if bytesread > size - totalread then bytesread := size - totalread;
      System.Move(EBuf[EBufPos], d[totalread], bytesread);
      inc(EBufPos, bytesread);
      inc(index, bytesread);
      inc(totalread, bytesread);
    end;
  end;
  ReadNext := r;
end;


{ Reads an unsigned LEB encoded number from the input stream }
function ReadULEB128() : QWord;
var
  shift : Byte;
  data : PtrInt;
  val : QWord;
begin
  shift := 0;
  ReadULEB128 := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    ReadULEB128 := ReadULEB128 or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
end;

{ Reads a signed LEB encoded number from the input stream }
function ReadLEB128() : Int64;
var
  shift : Byte;
  data : PtrInt;
  val : Int64;
begin
  shift := 0;
  ReadLEB128 := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    ReadLEB128 := ReadLEB128 or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
  { extend sign. Note that we can not use shl/shr since the latter does not
    translate to arithmetic shifting for signed types }
  ReadLEB128 := (not ((ReadLEB128 and (Int64(1) shl (shift-1)))-1)) or ReadLEB128;
end;


//{$ifdef CPUI8086}
//{ Reads an address from the current input stream }
//function ReadAddress(addr_size: smallint) : LongWord;
//begin
//  if addr_size = 4 then
//    ReadNext(ReadAddress, 4)
//  else if addr_size = 2 then begin
//    ReadAddress := 0;
//    ReadNext(ReadAddress, 2);
//  end
//  else
//    ReadAddress := 0;
//end;
//
//{ Reads a segment from the current input stream }
//function ReadSegment() : Word;
//begin
//  ReadNext(ReadSegment, sizeof(ReadSegment));
//end;
//{$else CPUI8086}
//{ Reads an address from the current input stream }
//function ReadAddress(addr_size: smallint) : PtrUInt;
//begin
//  ReadNext(ReadAddress, sizeof(ReadAddress));
//end;
//{$endif CPUI8086}


{ Reads a zero-terminated string from the current input stream. If the
  string is larger than 255 chars (maximum allowed number of elements in
  a ShortString, excess characters will be chopped off. }
function ReadString() : ShortString;
var
  temp : PtrInt;
  i : PtrUInt;
begin
  i := 1;
  temp := ReadNext();
  while (temp > 0) do begin
    ReadString[i] := char(temp);
    if (i = 255) then begin
      { skip remaining characters }
      repeat
        temp := ReadNext();
      until (temp <= 0);
      break;
    end;
    inc(i);
    temp := ReadNext();
  end;
  { unexpected end of file occurred? }
  if (temp = -1) then
    ReadString := ''
  else
    Byte(ReadString[0]) := i-1;
end;


var
  Abbrev_Offsets : array of QWord;
  Abbrev_Tags : array of QWord;
  Abbrev_Children : array of Byte;
  Abbrev_Attrs : array of array of record attr,form : QWord; end;

procedure ReadAbbrevTable;
  var
   i : PtrInt;
   tag,
   nr,
   attr,
   form: int64;
  begin
    DEBUG_WRITELN('Starting to read abbrev. section at $',hexstr(Dwarf_Debug_Abbrev_Section_Offset+Pos,16));
    repeat
      nr:=ReadULEB128;
      if nr=0 then
        break;

      if nr>high(Abbrev_Offsets) then
        begin
          SetLength(Abbrev_Offsets,nr+1024);
          SetLength(Abbrev_Tags,nr+1024);
          SetLength(Abbrev_Attrs,nr+1024);
          SetLength(Abbrev_Children,nr+1024);
        end;

      Abbrev_Offsets[nr]:=Pos;

      { read tag }
      tag:=ReadULEB128;
      Abbrev_Tags[nr]:=tag;
      DEBUG_WRITELN('Abbrev ',nr,' at offset ',Pos,' has tag $',hexstr(tag,4));
      { read flag for children }
      Abbrev_Children[nr]:=ReadNext;
      i:=0;
      { ensure that length(Abbrev_Attrs)=0 if an entry is overwritten (not sure if this will ever happen) and
        the new entry has no attributes }
      Abbrev_Attrs[nr]:=nil;
      repeat
        attr:=ReadULEB128;
        form:=ReadULEB128;
        if attr<>0 then
          begin
            SetLength(Abbrev_Attrs[nr],i+1);
            Abbrev_Attrs[nr][i].attr:=attr;
            Abbrev_Attrs[nr][i].form:=form;
          end;
        inc(i);
      until attr=0;
      DEBUG_WRITELN('Abbrev ',nr,' has ',Length(Abbrev_Attrs[nr]),' attributes');
    until false;
  end;


function DumpCompilationUnitForFunctionName(const file_offset : QWord; AnUnitName: String; AResProc: TDwarfDumpProc) : QWord;
  function ToHex(p: pbyte; l : integer):String; inline;
  begin
    ToHex := '';
    while l > 0 do begin
      ToHex :=  IntToHex(p^,2) + ToHex;
      inc(p);
      dec(l);
    end;
  end;

  function ToHexCommaList(p: pbyte; l : integer):String;
  begin
    ToHexCommaList := '';
    while l > 0 do begin
      if ToHexCommaList <> ''
      then ToHexCommaList :=  ToHexCommaList + ', ' + '$'+IntToHex(p^,2)
      else ToHexCommaList :=  ToHexCommaList + '$'+IntToHex(p^,2);
      inc(p);
      dec(l);
    end;
  end;

var
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TDebugInfoProgramHeader64;
  header32 : TDebugInfoProgramHeader32;
  isdwarf64 : boolean;
  abbrev,
  temp_length : DWord;
  unit_length : QWord;
  level : Integer;

function DecodeLocation(AData: PByte; ASize: QWord): String;
  function MakeAddressString(AData: Pointer; Sz: integer): string;
  begin
    case sz of
      2: MakeAddressString := '$'+IntToHex(PWord(AData)^, 4);
      4: MakeAddressString := '$'+IntToHex(PLongWord(AData)^, 8);
      8: MakeAddressString := '$'+IntToHex(PQWord(AData)^, 16);
    end;
  end;
  function ULEB128toOrdinal(var p: PByte): QWord;
  var
    n: Byte;
    Stop: Boolean;
  begin
    ULEB128toOrdinal := 0;
    n := 0;
    repeat
      Stop := (p^ and $80) = 0;
      ULEB128toOrdinal := ULEB128toOrdinal + QWord(p^ and $7F) shl n;
      Inc(n, 7);
      Inc(p);
    until Stop or (n > 128);
  end;

  function SLEB128toOrdinal(var p: PByte): Int64;
  var
    n: Byte;
    Stop: Boolean;
  begin
    SLEB128toOrdinal := 0;
    n := 0;
    repeat
      Stop := (p^ and $80) = 0;
      SLEB128toOrdinal := SLEB128toOrdinal + Int64(p^ and $7F) shl n;
      Inc(n, 7);
      Inc(p);
    until Stop or (n > 128);

    // sign extend when msbit = 1
    if ((p[-1] and $40) <> 0) and (n < 64) // only supports 64 bit
    then SLEB128toOrdinal := SLEB128toOrdinal or (Int64(-1) shl n);
  end;
var
  MaxData: PByte;
  v: Int64;
begin
  DecodeLocation:='';
  MaxData := AData + ASize - 1;
  while AData <= MaxData do
  begin
    if DecodeLocation <> '' then DecodeLocation := DecodeLocation + ', ';
    case AData^ of
      DW_OP_addr: begin
        DecodeLocation := DecodeLocation + 'DW_OP_addr, AddrB(' + MakeAddressString(@AData[1], header64.address_size) + ')';
        Inc(AData, header64.address_size);
      end;
      DW_OP_deref: begin
        DecodeLocation := DecodeLocation + 'DW_OP_deref';
      end;
      DW_OP_const1u: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const1u, ' + IntToStr(AData[1]);
        Inc(AData, 1);
      end;
      DW_OP_const1s: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const1s, ' + IntToStr(PShortInt(@AData[1])^);
        Inc(AData, 1);
      end;
      DW_OP_const2u: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const2u, NumU(' + IntToStr(PWord(@AData[1])^) + ',2)';
        Inc(AData, 2);
      end;
      DW_OP_const2s: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const2s, NumS(' + IntToStr(PSmallInt(@AData[1])^) + ',2)';
        Inc(AData, 2);
      end;
      DW_OP_const4u: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const4u, NumU(' + IntToStr(PLongWord(@AData[1])^) + ',4)';
        Inc(AData, 4);
      end;
      DW_OP_const4s: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const4s, NumS(' + IntToStr(PLongInt(@AData[1])^) + ',4)';
        Inc(AData, 4);
      end;
      DW_OP_const8u: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const8u, NumU' + IntToStr(PQWord(@AData[1])^) + ',8)';
        Inc(AData, 8);
      end;
      DW_OP_const8s: begin
        DecodeLocation := DecodeLocation + 'DW_OP_const8s, NumS' + IntToStr(PInt64(@AData[1])^) + ',8)';
        Inc(AData, 8);
      end;
      DW_OP_constu: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_constu, ULEB' + IntToStr(ULEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_consts: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_consts, SLEB(' + IntToStr(SLEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_dup: begin
        DecodeLocation := DecodeLocation + 'DW_OP_dup';
      end;
      DW_OP_drop: begin
        DecodeLocation := DecodeLocation + 'DW_OP_drop';
      end;
      DW_OP_over: begin
        DecodeLocation := DecodeLocation + 'DW_OP_over';
      end;
      DW_OP_pick: begin
        DecodeLocation := DecodeLocation + 'DW_OP_pick, ' + IntToStr(AData[1]);
        Inc(AData, 1);
      end;
      DW_OP_swap: begin
        DecodeLocation := DecodeLocation + 'DW_OP_swap';
      end;
      DW_OP_rot: begin
        DecodeLocation := DecodeLocation + 'DW_OP_rot';
      end;
      DW_OP_xderef: begin
        DecodeLocation := DecodeLocation + 'DW_OP_xderef';
      end;
      DW_OP_abs: begin
        DecodeLocation := DecodeLocation + 'DW_OP_abs';
      end;
      DW_OP_and: begin
        DecodeLocation := DecodeLocation + 'DW_OP_and';
      end;
      DW_OP_div: begin
        DecodeLocation := DecodeLocation + 'DW_OP_div';
      end;
      DW_OP_minus: begin
        DecodeLocation := DecodeLocation + 'DW_OP_minus';
      end;
      DW_OP_mod: begin
        DecodeLocation := DecodeLocation + 'DW_OP_mod';
      end;
      DW_OP_mul: begin
        DecodeLocation := DecodeLocation + 'DW_OP_mul';
      end;
      DW_OP_neg: begin
        DecodeLocation := DecodeLocation + 'DW_OP_neg';
      end;
      DW_OP_not: begin
        DecodeLocation := DecodeLocation + 'DW_OP_not';
      end;
      DW_OP_or: begin
        DecodeLocation := DecodeLocation + 'DW_OP_or';
      end;
      DW_OP_plus: begin
        DecodeLocation := DecodeLocation + 'DW_OP_plus';
      end;
      DW_OP_plus_uconst: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_plus_uconst, ULEB(' + IntToStr(ULEB128toOrdinal(AData))+')';
        Dec(AData);
      end;
      DW_OP_shl: begin
        DecodeLocation := DecodeLocation + 'DW_OP_shl';
      end;
      DW_OP_shr: begin
        DecodeLocation := DecodeLocation + 'DW_OP_shr';
      end;
      DW_OP_shra: begin
        DecodeLocation := DecodeLocation + 'DW_OP_shra';
      end;
      DW_OP_xor: begin
        DecodeLocation := DecodeLocation + 'DW_OP_xor';
      end;
      DW_OP_skip: begin
        DecodeLocation := DecodeLocation + 'DW_OP_skip, NumS(' + IntToStr(PSmallInt(@AData[1])^) + ',2)';
        Inc(AData, 2);
      end;
      DW_OP_bra: begin
        DecodeLocation := DecodeLocation + 'DW_OP_bra, NumS(' + IntToStr(PSmallInt(@AData[1])^) + ',2)';
        Inc(AData, 2);
      end;
      DW_OP_eq: begin
        DecodeLocation := DecodeLocation + 'DW_OP_eq';
      end;
      DW_OP_ge: begin
        DecodeLocation := DecodeLocation + 'DW_OP_ge';
      end;
      DW_OP_gt: begin
        DecodeLocation := DecodeLocation + 'DW_OP_gt';
      end;
      DW_OP_le: begin
        DecodeLocation := DecodeLocation + 'DW_OP_le';
      end;
      DW_OP_lt: begin
        DecodeLocation := DecodeLocation + 'DW_OP_lt';
      end;
      DW_OP_ne: begin
        DecodeLocation := DecodeLocation + 'DW_OP_ne';
      end;
      DW_OP_lit0..DW_OP_lit31: begin
        DecodeLocation := DecodeLocation + 'DW_OP_lit' + IntToStr(AData^ - DW_OP_lit0);
      end;
      DW_OP_reg0..DW_OP_reg31: begin
        DecodeLocation := DecodeLocation + 'DW_OP_reg' + IntToStr(AData^ - DW_OP_reg0);
      end;
      DW_OP_breg0..DW_OP_breg31: begin
        DecodeLocation := DecodeLocation + 'DW_OP_breg' + IntToStr(AData^ - DW_OP_breg0);
        Inc(AData);
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        DecodeLocation := DecodeLocation + ', SLEB(';
        DecodeLocation := DecodeLocation + IntToStr(v) + ')';
      end;
      DW_OP_regx: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_regx, ULEB(' + IntToStr(ULEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_fbreg: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_fbreg, SLEB(' + IntToStr(SLEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_bregx: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_bregx, ULEB(' + IntToStr(ULEB128toOrdinal(AData)) + ')';
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        DecodeLocation := DecodeLocation + ', SLEB(';
        DecodeLocation := DecodeLocation + IntToStr(v) + ')';
      end;
      DW_OP_piece: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_piece, ULEB(' + IntToStr(ULEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_deref_size: begin
        DecodeLocation := DecodeLocation + 'DW_OP_deref_size, ' + IntToStr(AData[1]);
        Inc(AData);
      end;
      DW_OP_xderef_size: begin
        DecodeLocation := DecodeLocation + 'DW_OP_xderef_size, ' + IntToStr(AData[1]);
        Inc(AData);
      end;
      DW_OP_nop: begin
        DecodeLocation := DecodeLocation + 'DW_OP_nop';
      end;
      DW_OP_push_object_address: begin
        DecodeLocation := DecodeLocation + 'DW_OP_push_object_address';
      end;
      DW_OP_call2: begin
        DecodeLocation := DecodeLocation + 'DW_OP_call2, ' + IntToStr(PWord(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_call4: begin
        DecodeLocation := DecodeLocation + 'DW_OP_call4, ' + IntToStr(PLongWord(@AData[1])^);
        Inc(AData, 4);
      end;
      DW_OP_call_ref: begin
        if isdwarf64 then
          DecodeLocation := DecodeLocation + 'DW_OP_call_ref, AddrB(' + MakeAddressString(@AData[1], 8) + ')'
        else
          DecodeLocation := DecodeLocation + 'DW_OP_call_ref, AddrB(' + MakeAddressString(@AData[1], 4) + ')';
        Inc(AData, 4);
      end;
      DW_OP_form_tls_address: begin
        DecodeLocation := DecodeLocation + 'DW_OP_form_tls_address';
      end;
      DW_OP_call_frame_cfa: begin
        DecodeLocation := DecodeLocation + 'DW_OP_call_frame_cfa';
      end;
      DW_OP_bit_piece: begin
        Inc(AData);
        DecodeLocation := DecodeLocation + 'DW_OP_bit_piece, ULEB(' + IntToStr(ULEB128toOrdinal(AData)) + '), ULEB(' + IntToStr(ULEB128toOrdinal(AData)) + ')';
        Dec(AData);
      end;
      DW_OP_bit_offset: begin
        DecodeLocation := DecodeLocation + 'DW_OP_bit_offset';
      end;
      DW_OP_offset: begin
        DecodeLocation := DecodeLocation + 'DW_OP_offset';
      end;
      DW_OP_lo_user..DW_OP_hi_user: begin
        DecodeLocation := DecodeLocation + 'DW_OP_user, ' + IntToStr(AData^);
      end;
    else begin
      DecodeLocation := DecodeLocation + 'Unknown DW_OP_' + IntToStr(AData^);
      DecodeLocation := DecodeLocation + ' RemainCnt:' + IntToStr(MaxData - AData);
      break;
    end
    end;
    Inc(AData);
  end;
end;

function DumpAttrib(attr, form : QWord; var Val: int64): string;
  var
    dummy : array[0..7] of byte;
    data : array of byte;
    bl : byte;
    wl : word;
    dl : dword;
    ql : qword;
    il : int64;
    i : PtrUInt;

    procedure DumpFormBlock(sz: integer; LocExpr: Boolean);
    begin
      SetLength(Data, sz);
      ReadNext(data[0], sz);
      DumpAttrib := DumpAttrib + ToHexCommaList(@data[0], sz);
      if LocExpr then
        DumpAttrib := DumpAttrib + ' # ' + DecodeLocation(@data[0], sz);
    end;

  begin
    DumpAttrib := '';
    Val := -1;
    case form of
      DW_FORM_addr:
        begin
          ReadNext(dummy,header64.address_size);
          // TODO: big endian
          DumpAttrib := ToHex(@dummy[0], header64.address_size);
        end;
      DW_FORM_block,
      DW_FORM_exprloc:
        begin
          ql:=ReadULEB128;
          DumpFormBlock(ql, True);
        end;
      DW_FORM_block1:
        begin
          bl:=ReadNext;
          DumpFormBlock(bl, (attr=DW_AT_location) or (attr=DW_AT_data_bit_offset) or (attr=DW_AT_data_member_location));
        end;
      DW_FORM_block2:
        begin
          ReadNext(wl,SizeOf(wl));
          DumpFormBlock(wl, (attr=DW_AT_location) or (attr=DW_AT_data_bit_offset) or (attr=DW_AT_data_member_location));
        end;
      DW_FORM_block4:
        begin
          ReadNext(dl,SizeOf(dl));
          DumpFormBlock(dl, (attr=DW_AT_location) or (attr=DW_AT_data_bit_offset) or (attr=DW_AT_data_member_location));
        end;
      DW_FORM_flag,
      DW_FORM_data1:
        begin
          ReadNext(bl,1);
          Val := bl;
          DumpAttrib := IntToStr(bl);
        end;
      DW_FORM_data2:
        begin
          ReadNext(wl,2);
          Val := wl;
          DumpAttrib := IntToStr(wl);
        end;
      DW_FORM_data4:
        begin
          ReadNext(dl,4);
          Val := dl;
          DumpAttrib := IntToStr(dl);
        end;
      DW_FORM_data8:
        begin
          ReadNext(ql,8);
          Val := int64(ql);
          DumpAttrib := IntToStr(ql);
        end;
      DW_FORM_udata:
        begin
          ql := ReadULEB128;
          Val := int64(ql);
          DumpAttrib := IntToStr(ql);
        end;
      DW_FORM_sdata:
        begin
          il := ReadLEB128;
          Val := il;
          DumpAttrib := IntToStr(il);
        end;
      DW_FORM_ref1:
        begin
          ReadNext(dummy,1);
          DumpAttrib := ToHex(@dummy[0],1);
        end;
      DW_FORM_ref2:
        begin
          ReadNext(dummy,2);
          DumpAttrib := ToHex(@dummy[0],2);
        end;
      DW_FORM_ref4:
        begin
          ReadNext(dummy,4);
          DumpAttrib := ToHex(@dummy[0],4);
        end;
      DW_FORM_ref8:
        begin
          ReadNext(dummy,8);
          DumpAttrib := ToHex(@dummy[0],8);
        end;
      DW_FORM_ref_udata:
        begin
          ql := ReadULEB128;
          DumpAttrib := ToHex(PByte(@ql),8);
        end;
      DW_FORM_ref_addr:
        { the size of DW_FORM_ref_addr changed between DWAWRF2 and later versions:
          in DWARF2 it depends on the architecture address size, in later versions on the DWARF type (32 bit/64 bit)
        }
        begin
          i := 4;
          if header64.version>2 then
            begin
              if isdwarf64 then i := 8;
            end
          else
            begin
              { address size for DW_FORM_ref_addr must be at least 32 bits }
              { this is compatible with Open Watcom on i8086 }
              if header64.address_size >= 4 then i := header64.address_size;
            end;
          ReadNext(dummy,i);
          DumpAttrib := ToHex(@dummy[0],i) + '_R'; // mark as REF-ADDR
        end;
      DW_FORM_string:
        DumpAttrib := ReadString;

      DW_FORM_strp,
      DW_FORM_sec_offset:
        if isdwarf64 then begin
          ReadNext(dummy,8);
          DumpAttrib := ToHex(@dummy[0],8);
        end
        else begin
          ReadNext(dummy,4);
          DumpAttrib := ToHex(@dummy[0],4);
        end;
      DW_FORM_indirect:
        begin
          ql := ReadULEB128;
          DumpAttrib := DwarfAttributeFormToString(ql) + ': ' + DumpAttrib(attr, ql, Val);
        end;
      DW_FORM_flag_present:
        begin
          DumpAttrib := 'TRUE';
          //Val := 1;
        end;
      DW_FORM_ref_sig8:
        begin
          ReadNext(dummy,8);
          DumpAttrib := ToHex(@dummy[0],8);
        end;
      else
        begin
          writeln(stderr,'Internal error: unknown dwarf form: $',hexstr(form,2));
          ReadNext;
          exit;
        end;
    end;
  end;

var
  i : PtrInt;
  val, ref_addr_base: int64;
  prev_base,prev_limit : TFilePos;
  prev_pos : TFilePos;
  s, hdinf: string;

begin
  ReadNext(temp_length, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  DumpCompilationUnitForFunctionName := file_offset + unit_length;

  Init(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then begin
    DEBUG_WRITELN('32 bit DWARF detected');
    hdinf := '// 32bit DWARF-';
    ReadNext(header32, sizeof(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.debug_abbrev_offset := header32.debug_abbrev_offset;
    header64.address_size := header32.address_size;
    isdwarf64:=false;
  end else begin
    DEBUG_WRITELN('64 bit DWARF detected');
    hdinf := '// 64bit DWARF-';
    ReadNext(header64, sizeof(header64));
    isdwarf64:=true;
  end;
  hdinf := hdinf + IntToStr(header64.version);
  hdinf := hdinf + ' AddrSz: ' + IntToStr(header64.address_size);

  DEBUG_WRITELN('debug_abbrev_offset: ',header64.debug_abbrev_offset);
  DEBUG_WRITELN('address_size: ',header64.address_size);

  { not nice, but we have to read the abbrev section after the start of the debug_info section has been read }
  prev_limit:=limit;
  prev_base:=base;
  prev_pos:=Pos;
  Init(Dwarf_Debug_Abbrev_Section_Offset+header64.debug_abbrev_offset,Dwarf_Debug_Abbrev_Section_Size);
  ReadAbbrevTable;

  { restore previous reading state and position }
  Init(prev_base,prev_limit);
  Seek(prev_pos);

  AnUnitName := UpperCase(AnUnitName);
  prev_pos:=Pos;
  ref_addr_base := file_offset - Dwarf_Debug_Info_Section_Offset;
  abbrev:=ReadULEB128;
  level:=0;
  while (abbrev <> 0) do
    begin
      DEBUG_WRITELN('Next abbrev: ',abbrev);
      AResProc(StringOfChar(' ', level*2) +
        IntToHex(prev_pos, 4) + ' ' +
        DwarfTagToString(integer(Abbrev_Tags[abbrev])) +
        ' ' + IntToHex(prev_pos+ref_addr_base,8)+ '_R ' + // the ref-addr
        hdinf
      );
      hdinf := '';

      for i:=0 to high(Abbrev_Attrs[abbrev]) do begin
        s := DumpAttrib(Abbrev_Attrs[abbrev][i].attr, Abbrev_Attrs[abbrev][i].form, val);
        if (AnUnitName <> '') and (Abbrev_Attrs[abbrev][i].attr = DW_AT_name)
        then begin
          if (UpperCase(s) <> AnUnitName) and
             (copy(UpperCase(s), Length(s)-Length(AnUnitName), Length(AnUnitName)+1) <> '/'+AnUnitName) and
             (copy(UpperCase(s), Length(s)-Length(AnUnitName), Length(AnUnitName)+1) <> '\'+AnUnitName)
          then
            exit;
        end;
        if val >= 0 then begin
          case Abbrev_Attrs[abbrev][i].attr of
            DW_AT_language:      s := s + ' ' + DwarfLanguageToString(val);
            DW_AT_encoding:      s := s + ' ' + DwarfBaseTypeEncodingToString(val);
            DW_AT_accessibility: s := s + ' ' + DwarfAccessibilityToString(val);
          end;
        end;
        AResProc(StringOfChar(' ', level*2+4) +
          DwarfAttributeToString(integer(Abbrev_Attrs[abbrev][i].attr)) +
          ' (' + DwarfAttributeFormToString(integer(Abbrev_Attrs[abbrev][i].form)) + '): ' +
          s
        );
      end;

      if Abbrev_Children[abbrev]<>0 then
        inc(level);

      prev_pos:=Pos;
      abbrev:=ReadULEB128;
      { skip entries signaling that no more child entries are following }
      while (level>0) and (abbrev=0) do
        begin
          dec(level);
          prev_pos:=Pos;
          abbrev:=ReadULEB128;
        end;
      AnUnitName := '';
    end;
end;



procedure DumpDwarfInfo(AResProc: TDwarfDumpProc; AFileName: String; AnUnitName: String);
var
  current_offset,
  end_offset: QWord;

begin
  if not OpenDwarf(AFileName) then
    exit;

  current_offset := Dwarf_Debug_Info_Section_Offset;
  end_offset := Dwarf_Debug_Info_Section_Offset + Dwarf_Debug_Info_Section_Size;

  while (current_offset < end_offset) do begin
    DEBUG_WRITELN('Reading .debug_info at section offset $',hexStr(current_offset-Dwarf_Debug_Info_Section_Offset,16));

    Init(current_offset, end_offset - current_offset);
    current_offset := DumpCompilationUnitForFunctionName(current_offset, AnUnitName, AResProc);
  end;

  CloseDwarf;
end;

end.

