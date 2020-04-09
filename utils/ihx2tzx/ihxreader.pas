{ IHX (Intel Hex format) to TZX (ZX Spectrum tape file format) convertor tool.

  This file contains the IHX writer code.

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

unit ihxreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIHXReader }

  TIHXReader = class
  private
    FOrigin: Word;
    FInternalData: array [0..$FFFF] of Byte;
  public
    Data: array of Byte;

    procedure ReadIHXFile(const FileName: string);

    property Origin: Word read FOrigin;
  end;

implementation

{ TIHXReader }

procedure TIHXReader.ReadIHXFile(const FileName: string);
var
  InF: TextFile;
  S: string;
  I: Integer;
  LineByteCount: Byte;
  LineAddress: Word;
  MinAddress, MaxAddress: LongInt;
  RecordType: Byte;
  Checksum, ExpectedChecksum: Byte;
  B: Byte;
begin
  MinAddress := -1;
  MaxAddress := -1;
  FOrigin := 0;
  SetLength(Data, 0);
  AssignFile(InF, FileName);
  Reset(InF);
  try
    while not EoF(InF) do
    begin
      ReadLn(InF, S);
      S:=UpperCase(Trim(S));
      if S='' then
        continue;
      if Length(S)<11 then
        raise Exception.Create('Line too short');
      if S[1]<>':' then
        raise Exception.Create('Line must start with '':''');
      for I:=2 to Length(S) do
        if not (S[I] in ['0'..'9','A'..'F']) then
          raise Exception.Create('Line contains an invalid character');
      LineByteCount:=StrToInt('$'+Copy(S,2,2));
      if (LineByteCount*2+11)<>Length(S) then
        raise Exception.Create('Invalid line length');
      LineAddress:=StrToInt('$'+Copy(S,4,4));
      RecordType:=StrToInt('$'+Copy(S,8,2));
      Checksum:=StrToInt('$'+Copy(S,Length(S)-1,2));
      ExpectedChecksum := Byte(LineByteCount + RecordType + Byte(LineAddress) + Byte(LineAddress shr 8));
      for I:=0 to LineByteCount-1 do
      begin
        B := StrToInt('$' + Copy(S, 10 + 2*I, 2));
        ExpectedChecksum := Byte(ExpectedChecksum + B);
      end;
      ExpectedChecksum := Byte(-ExpectedChecksum);
      if ExpectedChecksum <> Checksum then
        raise Exception.Create('Invalid checksum');
      case RecordType of
        0:
          begin
            if (MinAddress = -1) or (LineAddress < MinAddress) then
              MinAddress := LineAddress;
            if (MaxAddress = -1) or (MaxAddress < (LineAddress + LineByteCount - 1)) then
              MaxAddress := LineAddress + LineByteCount - 1;
            if MaxAddress > High(FInternalData) then
              raise Exception.CreateFmt('Data exceeds %d bytes', [High(FInternalData) + 1]);
            for I:=0 to LineByteCount-1 do
            begin
              B := StrToInt('$' + Copy(S, 10 + 2*I, 2));
              FInternalData[LineAddress + I] := B;
            end;
          end;
        1:
          begin
            { end of file }
            break;
          end;
      end;
    end;
    FOrigin := MinAddress;
    SetLength(Data, MaxAddress - MinAddress + 1);
    Move(FInternalData[MinAddress], Data[0], MaxAddress - MinAddress + 1);
  finally
    CloseFile(InF);
  end;
end;

end.

