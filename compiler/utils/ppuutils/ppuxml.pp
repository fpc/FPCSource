{
    Copyright (c) 2013 by Yury Sidorov and the FPC Development Team

    XML output of a PPU File

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

unit ppuxml;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ppuout;

type
  { TPpuXmlOutput }

  TPpuXmlOutput = class(TPpuOutput)
  private
    function XmlStr(const s: string): string;
    function GetTagName(const n, def: string): string;
  protected
    procedure WriteObjectStart(const AName: string; Def: TPpuDef); override;
    procedure WriteObjectEnd(const AName: string; Def: TPpuDef); override;
    procedure WriteArrayStart(const AName: string); override;
    procedure WriteArrayEnd(const AName: string); override;
    procedure WriteStr(const AName, AValue: string); override;
  public
    constructor Create(var OutFile: Text); override;
    procedure Init; override;
  end;

implementation

{ TPpuXmlOutput }

function TPpuXmlOutput.XmlStr(const s: string): string;
var
  ws: widestring;
  ps: PWideChar;
  pd: PAnsiChar;
  slen, dlen, dpos: integer;

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

  procedure _AddStr(const s: string);
  var
    p: PAnsiChar;
    i: integer;
  begin
    p:=PAnsiChar(s);
    for i:=1 to Length(s) do begin
      _AddChar(p^);
      Inc(p);
    end;
  end;

var
  c: widechar;
begin
  ws:=UTF8Decode(s);
  ps:=PWideChar(ws);
  slen:=Length(ws);
  dlen:=slen + 2;
  SetLength(Result, dlen);
  pd:=PAnsiChar(Result);
  dpos:=0;
  while slen > 0 do begin
    c:=ps^;
    case c of
      '<': _AddStr('&lt;');
      '>': _AddStr('&gt;');
      '&': _AddStr('&amp;');
      '''': _AddStr('&apos;');
      '"': _AddStr('&quot;');
      '\': _AddStr('\\');
      else
        if (c > #127) or (byte(c) in [9, 10, 13]) then
          _AddStr('&#x' + hexStr(word(c), 4) + ';')
        else
          if c < #32 then
            _AddStr('\x' + hexStr(byte(c), 2))
          else
            _AddChar(c);
    end;
    Inc(ps);
    Dec(slen);
  end;
  SetLength(Result, dpos);
end;

function TPpuXmlOutput.GetTagName(const n, def: string): string;
begin
  Result:=LowerCase(n);
  if Result = '' then
    Result:=def;
end;

procedure TPpuXmlOutput.WriteStr(const AName, AValue: string);
begin
  if AName = 'Type' then
    exit;
  WriteLn(Format('<%s>%s</%0:s>', [GetTagName(AName, 'value'), XmlStr(AValue)]));
end;

procedure TPpuXmlOutput.WriteArrayStart(const AName: string);
begin
  if (AName = '') and (Indent = 0) then
    exit;
  WriteLn(Format('<%s>', [GetTagName(AName, 'array')]));
  inherited;
end;

procedure TPpuXmlOutput.WriteArrayEnd(const AName: string);
begin
  if (AName = '') and (Indent = 0) then
    exit;
  inherited;
  WriteLn(Format('</%s>', [GetTagName(AName, 'array')]));
end;

procedure TPpuXmlOutput.WriteObjectStart(const AName: string; Def: TPpuDef);
begin
  if Def = nil then
    WriteLn(Format('<%s>', [GetTagName(AName, 'object')]))
  else
    WriteLn(Format('<%s>', [GetTagName(Def.DefTypeName, 'object')]));
  inherited;
end;

procedure TPpuXmlOutput.WriteObjectEnd(const AName: string; Def: TPpuDef);
begin
  inherited;
  if Def = nil then
    WriteLn(Format('</%s>', [GetTagName(AName, 'object')]))
  else
    WriteLn(Format('</%s>', [GetTagName(Def.DefTypeName, 'object')]));
end;

constructor TPpuXmlOutput.Create(var OutFile: Text);
begin
  inherited Create(OutFile);
end;

procedure TPpuXmlOutput.Init;
begin
  inherited Init;
  WriteLn('<?xml version="1.0" encoding="utf-8"?>');
end;

end.

