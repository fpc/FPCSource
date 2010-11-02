program project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, strutils;

type
  TPad1   = 0..65535;    // 16 bits padding
  TLevel1 = 0..63;       // 6 bits
  TLevel2 = 0..1023;     // 10 bits
  TLevel3 = 0..16777215; // 24 bits
  TLevel4 = 0..255;      // 8 bits

  TLevelsRec = bitpacked record
    level4  : TLevel4;
    level3  : TLevel3;
    level2  : TLevel2;
    level1  : TLevel1;
    pad     : TPad1;     // padding to make record size 64 bits
  end;

var
  id : TLevelsRec;
begin
  writeln('record size: ', sizeof(TLevelsRec));

  writeln(StringOfChar('-', 32));
  FillChar(id, sizeof(id), 0);
  TLevelsRec(id).level1 := 1;
  TLevelsRec(id).level2 := 0;
  TLevelsRec(id).level3 := 3;
  TLevelsRec(id).level4 := 4;
  writeln(TLevelsRec(id).level1, ' (', IntToBin(TLevelsRec(id).level1, 8), ')');
  writeln(TLevelsRec(id).level2, ' (', IntToBin(TLevelsRec(id).level2, 12), ')');
  writeln(TLevelsRec(id).level3, ' (', IntToBin(TLevelsRec(id).level3, 26), ')');
  writeln(TLevelsRec(id).level4, ' (', IntToBin(TLevelsRec(id).level4, 10), ')');
  writeln(IntToBin(int64(id), 64));

  if (TLevelsRec(id).level1 <> 1) then raise Exception.Create('level1 bad');
  if (TLevelsRec(id).level2 <> 0) then raise Exception.Create('level2 bad');
  if (TLevelsRec(id).level3 <> 3) then raise Exception.Create('level3 bad');
  if IntToBin(TLevelsRec(id).level3, 26)<> '00000000000000000000000011' then
    raise Exception.Create('level3 bad in IntToBin call');
  if (TLevelsRec(id).level4 <> 4) then raise Exception.Create('level4 bad');

  writeln(StringOfChar('-', 32));
  FillChar(id, sizeof(id), 0);
  TLevelsRec(id).level1 := 1;
  TLevelsRec(id).level2 := 2;
  TLevelsRec(id).level3 := 3;
  TLevelsRec(id).level4 := 4;
  writeln(TLevelsRec(id).level1, ' (', IntToBin(TLevelsRec(id).level1, 8), ')');
  writeln(TLevelsRec(id).level2, ' (', IntToBin(TLevelsRec(id).level2, 12), ')');
  writeln(TLevelsRec(id).level3, ' (', IntToBin(TLevelsRec(id).level3, 26), ')');
  writeln(TLevelsRec(id).level4, ' (', IntToBin(TLevelsRec(id).level4, 10), ')');
  writeln(IntToBin(int64(id), 64));

  if (TLevelsRec(id).level1 <> 1) then raise Exception.Create('level1 bad');
  if (TLevelsRec(id).level2 <> 2) then raise Exception.Create('level2 bad');
  if (TLevelsRec(id).level3 <> 3) then raise Exception.Create('level3 bad');
  if IntToBin(TLevelsRec(id).level3, 26)<> '00000000000000000000000011' then
    raise Exception.Create('level3 bad in IntToBin call');
  if (TLevelsRec(id).level4 <> 4) then raise Exception.Create('level4 bad');

end.


