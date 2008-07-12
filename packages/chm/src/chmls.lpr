{ Copyright (C) <2005> <Andrew Haines> chmls.lpr

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING, included in this distribution,
  for details about the copyright.
}
program chmls;

{$IFDEF MSWINDOWS}
{$apptype console}
{$ENDIF}

{$mode objfpc}{$H+}

uses
  Classes, chmreader, chmbase, Sysutils
  { add your units here };
type

  { TJunkObject }

  TJunkObject = class
    procedure OnFileEntry(Name: String; Offset, UncompressedSize, ASection: Integer);
  end;
  

var
  ITS: TITSFReader;
  Stream: TFileStream;
  I : Integer;
  Section: Integer = -1;
  JunkObject: TJunkObject;

procedure WriteStr(Str: String; CharWidth: Integer);
  var
    OutString: String;
    Len: Integer;
  begin
    Len := Length(Str);
    SetLength(OutString, CharWidth-Len);
    FillChar(OutString[1], CharWidth-Len, ' ');

    Write(OutString + Str); // to sdtout
  end;

{ TJunkObject }

procedure TJunkObject.OnFileEntry(Name: String; Offset, UncompressedSize,
  ASection: Integer);
begin
  Inc(I);
  if (Section > -1) and (ASection <> Section) then Exit;
  if (I = 1) or (I mod 40 = 0) then
    WriteLn(StdErr, '<Section> <Offset> <UnCompSize>  <Name>');
  Write(' ');
  Write(ASection);
  Write('      ');
  WriteStr(IntToStr(Offset), 10);
  Write('  ');
  WriteStr(IntToStr(UncompressedSize), 11);
  Write('  ');
  WriteLn(Name);
end;

Procedure Usage;

begin
  WriteLn('   Usage:  chmls filename.chm [section number]');
  Halt(1);
end;

// Start of program
begin
  if (Paramcount < 1) or (Paramstr(1)='-h') or (Paramstr(1)='-?') then 
    begin
    usage;
    end;
  if ParamCount > 1 then 
    begin
    Section := StrToIntDef(ParamStr(2),-1);
    If (Section=-1) then
      begin
      Usage;
      Halt(1);
      end;
    end; 
  Stream := TFileStream.Create(ParamStr(1), fmOpenRead);
  JunkObject := TJunkObject.Create;
  ITS:= TITSFReader.Create(Stream, True);
  I := 0;
  ITS.GetCompleteFileList(@JunkObject.OnFileEntry);
  
  WriteLn('Total Files in chm: ', I);
  ITS.Free;
  JunkObject.Free;
end.

