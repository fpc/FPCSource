{
    Copyright (c) 2013 by Yury Sidorov and the FPC Development Team

    JSON output of a PPU File

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

 ****************************************************************************}

unit ppujson;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ppuout;

type
  { TPpuJsonOutput }

  TPpuJsonOutput = class(TPpuOutput)
  private
    FNeedDelim: array of boolean;
    function JsonStr(const s: string): string;
    procedure BeforeWriteElement;
    procedure WriteAttr(const AName, AValue: string);
  protected
    procedure WriteObjectStart(const AName: string; Def: TPpuDef); override;
    procedure WriteObjectEnd(const AName: string; Def: TPpuDef); override;
    procedure WriteArrayStart(const AName: string); override;
    procedure WriteArrayEnd(const AName: string); override;
    procedure WriteStr(const AName, AValue: string); override;
    procedure WriteInt(const AName: string; AValue: Int64; Signed: boolean); override;
    procedure WriteFloat(const AName: string; AValue: extended); override;
    procedure WriteBool(const AName: string; AValue: boolean); override;
    procedure WriteNull(const AName: string); override;
  public
    constructor Create(var OutFile: Text); override;
    procedure IncI; override;
    procedure DecI; override;
  end;

implementation

{ TPpuJsonOutput }

function TPpuJsonOutput.JsonStr(const s: string): string;
var
  ws: widestring;
  ps: PWideChar;
  pd: PAnsiChar;
  i, slen, dlen, dpos: integer;

  procedure _AddChar(c: ansichar);
  begin
    if dpos = dlen then begin
      dlen:=dlen*2;
      SetLength(Result, dlen);
      pd:=PAnsiChar(Result) + dpos;
    end;
    pd^:=c;
    Inc(pd);
    Inc(dpos);
  end;

var
  c: widechar;
  ss: shortstring;
begin
  ws:=UTF8Decode(s);
  ps:=PWideChar(ws);
  slen:=Length(ws);
  dlen:=slen + 2;
  SetLength(Result, dlen);
  pd:=PAnsiChar(Result);
  dpos:=0;
  _AddChar('"');
  while slen > 0 do begin
    c:=ps^;
    case c of
      '"', '\', '/':
        begin
          _AddChar('\');
          _AddChar(c);
        end;
      #8:
        begin
          _AddChar('\');
          _AddChar('b');
        end;
      #9:
        begin
          _AddChar('\');
          _AddChar('t');
        end;
      #10:
        begin
          _AddChar('\');
          _AddChar('n');
        end;
      #13:
        begin
          _AddChar('\');
          _AddChar('r');
        end;
      #12:
        begin
          _AddChar('\');
          _AddChar('f');
        end;
      else
        if (c < #32) or (c > #127) then begin
          _AddChar('\');
          _AddChar('u');
          ss:=hexStr(integer(c), 4);
          for i:=1 to 4 do
            _AddChar(ss[i]);
        end
        else
          _AddChar(c);
    end;
    Inc(ps);
    Dec(slen);
  end;
  _AddChar('"');
  SetLength(Result, dpos);
end;

procedure TPpuJsonOutput.BeforeWriteElement;
begin
  if FNeedDelim[Indent] then
    WriteLn(',');
  FNeedDelim[Indent]:=True;
end;

procedure TPpuJsonOutput.WriteAttr(const AName, AValue: string);
begin
  BeforeWriteElement;
  if AName <> '' then
    Write(Format('"%s": %s', [AName, AValue]))
  else
    Write(AValue);
end;

procedure TPpuJsonOutput.WriteStr(const AName, AValue: string);
begin
  WriteAttr(AName, JsonStr(AValue));
end;

procedure TPpuJsonOutput.WriteInt(const AName: string; AValue: Int64; Signed: boolean);
begin
  if Signed then
    WriteAttr(AName, IntToStr(AValue))
  else
    WriteAttr(AName, IntToStr(QWord(AValue)));
end;

procedure TPpuJsonOutput.WriteFloat(const AName: string; AValue: extended);
var
  s: string;
begin
  Str(AValue, s);
  WriteAttr(AName, s);
end;

procedure TPpuJsonOutput.WriteBool(const AName: string; AValue: boolean);
begin
  if AValue then
    WriteAttr(AName, 'true')
  else
    WriteAttr(AName, 'false');
end;

procedure TPpuJsonOutput.WriteNull(const AName: string);
begin
  WriteAttr(AName, 'null');
end;

procedure TPpuJsonOutput.WriteArrayStart(const AName: string);
begin
  WriteAttr(AName, '[');
  WriteLn;
  inherited;
end;

procedure TPpuJsonOutput.WriteArrayEnd(const AName: string);
begin
  inherited;
  Write(']');
end;

procedure TPpuJsonOutput.WriteObjectStart(const AName: string; Def: TPpuDef);
begin
  WriteAttr(AName, '{');
  WriteLn;
  inherited;
end;

procedure TPpuJsonOutput.WriteObjectEnd(const AName: string; Def: TPpuDef);
begin
  inherited;
  Write('}');
end;

constructor TPpuJsonOutput.Create(var OutFile: Text);
begin
  inherited Create(OutFile);
  SetLength(FNeedDelim, 10);
  FNeedDelim[0]:=False;
end;

procedure TPpuJsonOutput.IncI;
begin
  inherited IncI;
  if Length(FNeedDelim) >= Indent then
    SetLength(FNeedDelim, Indent + 1);
  FNeedDelim[Indent]:=False;
end;

procedure TPpuJsonOutput.DecI;
begin
  if FNeedDelim[Indent] then
    WriteLn;
  inherited DecI;
end;

end.

