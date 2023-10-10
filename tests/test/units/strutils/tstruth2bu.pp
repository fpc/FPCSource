program HexToBin_tests;

{$IFDEF FPC}
  // PChar in Delphi is PWideChar so make sure the tests behave the same
  {$mode DelphiUnicode}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF FPC}
    StrUtils
  {$ELSE}
    Classes
  {$ENDIF};

var
  BinValueBytes: TBytes;
  HexValueBytes: TBytes;
  HexInLen, BinBufLen: Integer;
  ret: Integer;

const
  HexInputA: AnsiString = '1decaf';
  HexInputW: WideString = '1decaf';
  HexCorruptInputW: WideString = '9abcdefg';
  HexOffsetInputW: WideString = '608da975';

begin
  writeln('start testing of HexToBin methods');

  {* test simple methods *}
  // ansistring
  // write 2 bytes into 1 byte
  HexInLen := Length(HexInputA) * SizeOf(AnsiChar) div 2;

(*
  // Delphi: E2251 Ambiguous overloaded call to 'HexToBin'
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(@HexInputA[1], @BinValueBytes[0], BinBufLen);
  if ret <> 3 then halt(1);
  if BinValueBytes[0] <> 29 then halt(1);
  if BinValueBytes[1] <> 236 then halt(1);
  if BinValueBytes[2] <> 175 then halt(1);
*)

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PChar(HexInputA), PChar(BinValueBytes), BinBufLen);
  if ret <> 0 then halt(2);
  if BinValueBytes[0] <> 0 then halt(2);
  if BinValueBytes[1] <> 0 then halt(2);
  if BinValueBytes[2] <> 0 then halt(2);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PAnsiChar(HexInputA), PAnsiChar(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(3);
  if BinValueBytes[0] <> 29 then halt(3);
  if BinValueBytes[1] <> 236 then halt(3);
  if BinValueBytes[2] <> 175 then halt(3);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PAnsiChar(HexInputA), Pointer(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(4);
  if BinValueBytes[0] <> 29 then halt(4);
  if BinValueBytes[1] <> 236 then halt(4);
  if BinValueBytes[2] <> 175 then halt(4);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PAnsiChar(HexInputA), BinValueBytes, BinBufLen);
  if ret <> 3 then halt(5);
  if BinValueBytes[0] <> 29 then halt(5);
  if BinValueBytes[1] <> 236 then halt(5);
  if BinValueBytes[2] <> 175 then halt(5);

  // widestring
  // write 4 bytes into 1 byte
  HexInLen := Length(HexInputW) * SizeOf(WideChar) div 4;

(*
  // Delphi: E2251 Ambiguous overloaded call to 'HexToBin'
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(@HexInputW[1], @BinValueBytes[0], BinBufLen);
  if ret <> 3 then halt(6);
  if BinValueBytes[0] <> 29 then halt(6);
  if BinValueBytes[1] <> 236 then halt(6);
  if BinValueBytes[2] <> 175 then halt(6);
*)

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexInputW), PAnsiChar(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(7);
  if BinValueBytes[0] <> 29 then halt(7);
  if BinValueBytes[1] <> 236 then halt(7);
  if BinValueBytes[2] <> 175 then halt(7);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexInputW), Pointer(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(8);
  if BinValueBytes[0] <> 29 then halt(8);
  if BinValueBytes[1] <> 236 then halt(8);
  if BinValueBytes[2] <> 175 then halt(8);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexInputW), BinValueBytes, BinBufLen);
  if ret <> 3 then halt(9);
  if BinValueBytes[0] <> 29 then halt(9);
  if BinValueBytes[1] <> 236 then halt(9);
  if BinValueBytes[2] <> 175 then halt(9);

  // not fully valid widestring input
  HexInLen := Length(HexCorruptInputW) * SizeOf(WideChar) div 4;
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexCorruptInputW), PAnsiChar(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(10);
  if BinValueBytes[0] <> 154 then halt(10);
  if BinValueBytes[1] <> 188 then halt(10);
  if BinValueBytes[2] <> 222 then halt(10);
  if BinValueBytes[3] <> 0 then halt(10);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexCorruptInputW), Pointer(BinValueBytes), BinBufLen);
  if ret <> 3 then halt(11);
  if BinValueBytes[0] <> 154 then halt(11);
  if BinValueBytes[1] <> 188 then halt(11);
  if BinValueBytes[2] <> 222 then halt(11);
  if BinValueBytes[3] <> 0 then halt(11);

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PWideChar(HexCorruptInputW), BinValueBytes, BinBufLen);
  if ret <> 3 then halt(12);
  if BinValueBytes[0] <> 154 then halt(12);
  if BinValueBytes[1] <> 188 then halt(12);
  if BinValueBytes[2] <> 222 then halt(12);
  if BinValueBytes[3] <> 0 then halt(12);

  {* test complex offset methods *}
  // ansistring
  HexInLen := Length(HexInputA) div 2;

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PChar(HexInputA), 2, BinValueBytes, 2, BinBufLen);
  if ret <> 0 then halt(13);
  if BinValueBytes[0] <> 0 then halt(13);
  if BinValueBytes[1] <> 0 then halt(13);
  if BinValueBytes[2] <> 0 then halt(13);

  HexValueBytes := TEncoding.ASCII.GetBytes(HexInputA);
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(HexValueBytes, 2, BinValueBytes, 2, BinBufLen);
  if ret <> 2 then halt(14);
  if BinValueBytes[0] <> 0 then halt(14);
  if BinValueBytes[1] <> 0 then halt(14);
  if BinValueBytes[2] <> 236 then halt(14);

  // widestring
  HexInLen := Length(HexInputW) div 2;

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PChar(HexInputW), 2, BinValueBytes, 2, BinBufLen);
  if ret <> 2 then halt(15);
  if BinValueBytes[0] <> 0 then halt(15);
  if BinValueBytes[1] <> 0 then halt(15);
  if BinValueBytes[2] <> 236 then halt(15);

  HexValueBytes := TEncoding.ASCII.GetBytes(HexInputW);
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(HexValueBytes, 2, BinValueBytes, 2, BinBufLen);
  if ret <> 2 then halt(16);
  if BinValueBytes[0] <> 0 then halt(16);
  if BinValueBytes[1] <> 0 then halt(16);
  if BinValueBytes[2] <> 236 then halt(16);

  // documentation offset example
  HexInLen := Length(HexOffsetInputW) * SizeOf(WideChar) div 4;

  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(PChar(HexOffsetInputW), 4, BinValueBytes, 0, BinBufLen);
  if ret <> 2 then halt(17);
  if BinValueBytes[0] <> 169 then halt(17);
  if BinValueBytes[1] <> 117 then halt(17);
  if BinValueBytes[2] <> 0 then halt(17);
  if BinValueBytes[3] <> 0 then halt(17);

  HexValueBytes := TEncoding.ASCII.GetBytes(HexOffsetInputW);
  SetLength(BinValueBytes, HexInLen);
  FillChar(BinValueBytes[0], Length(BinValueBytes), 0);
  BinBufLen := Length(BinValueBytes);
  ret := HexToBin(HexValueBytes, 4, BinValueBytes, 0, BinBufLen);
  if ret <> 2 then halt(18);
  if BinValueBytes[0] <> 169 then halt(18);
  if BinValueBytes[1] <> 117 then halt(18);
  if BinValueBytes[2] <> 0 then halt(18);
  if BinValueBytes[3] <> 0 then halt(18);

  writeln('testing of HexToBin methods ended');
end.
