{
    *********************************************************************
    Copyright (C) 2002 Free Pascal Development Team

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.QueryInterface(IID,Intf)=0);
end;

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
begin
  Result:=(Instance<>nil) and Instance.GetInterface(IID,Intf);
end;

function Supports(const Instance: TObject; const IID: Shortstring; out Intf): Boolean;
begin
  Result:=(Instance<>nil) and Instance.GetInterface(IID,Intf);
end;

function Supports(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result:=Supports(Instance,IID,Temp);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.GetInterfaceEntry(IID)<>nil);
end;

function Supports(const Instance: TObject; const IID: Shortstring): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.GetInterfaceEntryByStr(IID)<>nil);
end;

function Supports(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result:=(AClass<>nil) and (AClass.GetInterfaceEntry(IID)<>nil);
end;

function Supports(const AClass: TClass; const IID: Shortstring): Boolean;
begin
  Result:=(AClass<>nil) and (AClass.GetInterfaceEntryByStr(IID)<>nil);
end;


function StringToGUID(const S: string): TGUID;

  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':
        Result:=Byte(c) - Byte('0');
      'a'..'f':
        Result:=(Byte(c) - Byte('a')) + 10;
      'A'..'F':
        Result:=(Byte(c) - Byte('A')) + 10;
      else
        raise EConvertError.CreateFmt(SInvalidGUID, [s]);
        Result:=0;
    end;
  end;

  function HexByte(p: PChar): Byte;
  begin
    Result:=(HexChar(p[0]) shl 4) + HexChar(p[1]);
  end;

var
  i: integer;
  src: PChar;
  dest: PByte;
begin
  if ((Length(S)<>38) or
      (s[1]<>'{')) then
    raise EConvertError.CreateFmt(SInvalidGUID, [s]);
  dest:=PByte(@Result);
  src:=PChar(s);
  inc(src);
  for i:=0 to 3 do
    dest[i]:=HexByte(src+(3-i)*2);
  inc(src, 8);
  inc(dest, 4);
  if src[0]<>'-' then
    raise EConvertError.CreateFmt(SInvalidGUID, [s]);
  inc(src);
  for i:=0 to 1 do
   begin
     dest^:=HexByte(src+2);
     inc(dest);
     dest^:=HexByte(src);
     inc(dest);
     inc(src, 4);
     if src[0]<>'-' then
       raise EConvertError.CreateFmt(SInvalidGUID, [s]);
     inc(src);
   end;
  dest^:=HexByte(src);
  inc(dest);
  inc(src, 2);
  dest^:=HexByte(src);
  inc(dest);
  inc(src, 2);
  if src[0]<>'-' then
    raise EConvertError.CreateFmt(SInvalidGUID, [s]);
  inc(src);
  for i:=0 to 5 do
   begin
     dest^:=HexByte(src);
     inc(dest);
     inc(src, 2);
   end;
end;


function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a1,a2: PIntegerArray;
begin
  a1:=PIntegerArray(@guid1);
  a2:=PIntegerArray(@guid2);
  Result:=(a1^[0]=a2^[0]) and
          (a1^[1]=a2^[1]) and
          (a1^[2]=a2^[2]) and
          (a1^[3]=a2^[3]);
end;


function GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [
     GUID.D1, GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
end;

