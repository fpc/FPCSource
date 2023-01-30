{ Efficient string buffer helper

  Copyright (C) 2006-2008 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lStrBuffer;

{$mode objfpc}{$h+}

interface

type
  PStringBuffer = ^TStringBuffer;
  TStringBuffer = record
    Memory: pansichar;
    Pos: pansichar;
  end;

function  InitStringBuffer(InitialSize: integer): TStringBuffer;
// Assumes UTF8
procedure AppendString(var ABuffer: TStringBuffer; const ASource: unicodestring); overload;
procedure AppendString(var ABuffer: TStringBuffer; const ASource: ansistring); overload;
procedure AppendString(var ABuffer: TStringBuffer; const ASource: shortstring); overload;
procedure AppendString(var ABuffer: TStringBuffer; ASource: pointer; ALength: PtrUInt); overload;
procedure AppendString(var ABuffer: TStringBuffer; ASource: pansichar); overload;
procedure AppendChar(var ABuffer: TStringBuffer; AChar: Ansichar);

implementation

function  InitStringBuffer(InitialSize: integer): TStringBuffer;
begin
  Result.Memory := GetMem(InitialSize);
  Result.Pos := Result.Memory;
end;

procedure AppendString(var ABuffer: TStringBuffer; ASource: pointer; ALength: PtrUInt);
var
  lPos, lSize: PtrUInt;
begin
  if ALength = 0 then exit;
  lPos := PtrUInt(ABuffer.Pos - ABuffer.Memory);
  lSize := PtrUInt(MemSize(ABuffer.Memory));
  { reserve 2 extra spaces }
  if lPos + ALength + 2 >= lSize then
  begin
    ReallocMem(ABuffer.Memory, lPos + ALength + lSize);
    ABuffer.Pos := ABuffer.Memory + lPos;
  end;
  Move(ASource^, ABuffer.Pos^, ALength);
  Inc(ABuffer.Pos, ALength);
end;

procedure AppendString(var ABuffer: TStringBuffer; ASource: pansichar);
begin
  if ASource = nil then exit;
  AppendString(ABuffer, ASource, StrLen(ASource)); 
end;

procedure AppendString(var ABuffer: TStringBuffer; const ASource: shortstring);
begin
  AppendString(ABuffer, @ASource[1], Length(ASource));
end;

procedure AppendString(var ABuffer: TStringBuffer; const ASource: ansistring);
begin
  AppendString(ABuffer, PAnsiChar(ASource), Length(ASource));
end;

procedure AppendChar(var ABuffer: TStringBuffer; AChar: ansichar);
begin
  ABuffer.Pos^ := AChar;
  Inc(ABuffer.Pos);
end;

procedure AppendString(var ABuffer: TStringBuffer; const ASource: unicodestring);

Var
  S : UTF8String;

begin
  S:=UTF8Encode(aSource);
  AppendString(aBuffer,S);
end;

end.
