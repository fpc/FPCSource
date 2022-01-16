{ IHX (Intel Hex format) to TZX (ZX Spectrum tape file format) convertor tool.

  This file contains the TZX writer code.

  Copyright (C) 2020 Nikolay Nikolov <nickysn@users.sourceforg.net>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit tzxwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTZXWriter }

  TTZXWriter = class
  private
    FOutStream: TStream;
  public
    constructor Create(OutStream : TStream);
    procedure AppendStandardSpeedDataBlock(const Buffer; Count: Word);
    procedure AppendHeader(FileType: Byte; const FileName: string; DataBlockLength, Parameter1, Parameter2: Word);
    procedure AppendDataBlock(const Buffer; Count: Word);
    procedure AppendProgramFile(const FileName: string; AutostartLine, VarAreaOffset: Word; const Buffer; Count: Word);
    procedure AppendCodeFile(const FileName: string; StartAddress: Word; const Buffer; Count: Word);
  end;

implementation

{ TTZXWriter }

constructor TTZXWriter.Create(OutStream: TStream);
const
  Header: string = 'ZXTape!'#$1A#1#20;
begin
  FOutStream := OutStream;
  FOutStream.Seek(0, soFromBeginning);
  FOutStream.Write(Header[1], Length(Header));
end;

procedure TTZXWriter.AppendStandardSpeedDataBlock(const Buffer; Count: Word);
const
  PauseMilliseconds = 1000;
begin
  FOutStream.WriteByte($10);
  FOutStream.WriteByte(Byte(PauseMilliseconds));
  FOutStream.WriteByte(Byte(PauseMilliseconds shr 8));
  FOutStream.WriteByte(Byte(Count));
  FOutStream.WriteByte(Byte(Count shr 8));
  FOutStream.Write(Buffer, Count);
end;

procedure TTZXWriter.AppendHeader(FileType: Byte; const FileName: string;
  DataBlockLength, Parameter1, Parameter2: Word);
var
  HeaderBlock: array [0..18] of Byte;
  I: Integer;
  Checksum: Byte;
begin
  HeaderBlock[0] := 0;  { header }
  HeaderBlock[1] := FileType;
  { file name }
  for I := 1 to 10 do
    if I <= Length(FileName) then
      HeaderBlock[I + 1] := Ord(FileName[I])
    else
      HeaderBlock[I + 1] := Ord(' ');
  HeaderBlock[12] := Byte(DataBlockLength);
  HeaderBlock[13] := Byte(DataBlockLength shr 8);
  HeaderBlock[14] := Byte(Parameter1);
  HeaderBlock[15] := Byte(Parameter1 shr 8);
  HeaderBlock[16] := Byte(Parameter2);
  HeaderBlock[17] := Byte(Parameter2 shr 8);
  Checksum := 0;
  for I := 0 to 17 do
    Checksum := Checksum xor HeaderBlock[I];
  HeaderBlock[18] := Checksum;
  AppendStandardSpeedDataBlock(HeaderBlock, SizeOf(HeaderBlock));
end;

procedure TTZXWriter.AppendDataBlock(const Buffer; Count: Word);
var
  I: Integer;
  Checksum: Byte;
  DataBlock: array of Byte;
begin
  SetLength(DataBlock, Count + 2);
  Move(Buffer, DataBlock[1], Count);
  DataBlock[0] := $FF;  { data }
  Checksum := 0;
  for I := 0 to High(DataBlock) - 1 do
    Checksum := Checksum xor DataBlock[I];
  DataBlock[High(DataBlock)] := Checksum;
  AppendStandardSpeedDataBlock(DataBlock[0], Length(DataBlock));
end;

procedure TTZXWriter.AppendProgramFile(const FileName: string; AutostartLine,
  VarAreaOffset: Word; const Buffer; Count: Word);
begin
  AppendHeader(0, FileName, Count, AutostartLine, VarAreaOffset);
  AppendDataBlock(Buffer, Count);
end;

procedure TTZXWriter.AppendCodeFile(const FileName: string; StartAddress: Word;
  const Buffer; Count: Word);
begin
  AppendHeader(3, FileName, Count, StartAddress, 32768);
  AppendDataBlock(Buffer, Count);
end;

end.

