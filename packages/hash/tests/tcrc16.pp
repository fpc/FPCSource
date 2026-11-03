{ CRC-16 variants of the crc unit against the catalogue check values and a bitwise computation }
program tcrc16;

{$mode objfpc}
{$h+}

uses
  sysutils,
  crc;

type
  TCrc16Func = function(aBuf: PByte; aLen: Cardinal): Word;

var
  lErrors: Integer;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Checks the CRC computed by aFunc for a string }
procedure CheckString(const aName: string; aFunc: TCrc16Func; const aData: RawByteString; aExpected: Word);

var
  lCrc: Word;

begin
  lCrc:=aFunc(PByte(PAnsiChar(aData)), Length(aData));
  Check(lCrc=aExpected, aName+' of '''+aData+''' is $'+HexStr(aExpected, 4)+', got $'+HexStr(lCrc, 4));
end;


{ Returns the table entry for aByte, computed bit by bit from aPoly }
function BitEntry(aByte: Byte; aPoly: Word; aReflected: Boolean): Word;

var
  lBit: Integer;

begin
  if aReflected then
    begin
      Result:=aByte;
      for lBit:=0 to 7 do
        if (Result and 1)<>0 then
          Result:=(Result shr 1) xor aPoly
        else
          Result:=Result shr 1;
    end
  else
    begin
      Result:=Word(aByte shl 8);
      for lBit:=0 to 7 do
        if (Result and $8000)<>0 then
          Result:=Word(Result shl 1) xor aPoly
        else
          Result:=Word(Result shl 1);
    end;
end;


{ Checks all entries of aTable against the bitwise computation for aPoly }
procedure CheckTable(const aName: string; const aTable: TCrc16Table; aPoly: Word; aReflected: Boolean);

var
  lByte: Integer;
  lBad: Integer;

begin
  lBad:=0;
  for lByte:=0 to 255 do
    if aTable[lByte]<>BitEntry(lByte, aPoly, aReflected) then
      Inc(lBad);
  Check(lBad=0, aName+' has '+IntToStr(lBad)+' entries that differ from polynomial $'+HexStr(aPoly, 4));
end;


{ Checks that chained updates with the generic routines give the one-call result }
procedure CheckChained;

const
  Data: RawByteString = 'The quick brown fox jumps over the lazy dog';

var
  lBuf: PByte;
  lLen, lSplit: Cardinal;
  lCrc: Word;

begin
  lBuf:=PByte(PAnsiChar(Data));
  lLen:=Length(Data);
  lSplit:=lLen div 3;
  lCrc:=crc16_reflected($FFFF, lBuf, lSplit, crc16_table_a001);
  lCrc:=crc16_reflected(lCrc, lBuf+lSplit, lLen-lSplit, crc16_table_a001);
  Check(lCrc=crc16_modbus(lBuf, lLen), 'chained MODBUS update equals crc16_modbus');
  lCrc:=crc16_normal($FFFF, lBuf, lSplit, crc16_table_1021);
  lCrc:=crc16_normal(lCrc, lBuf+lSplit, lLen-lSplit, crc16_table_1021);
  Check(lCrc=crc16_ccitt_false(lBuf, lLen), 'chained CCITT-FALSE update equals crc16_ccitt_false');
  lCrc:=crc16_reflected(0, lBuf, lSplit, crc16_table_a6bc);
  lCrc:=crc16_reflected(lCrc, lBuf+lSplit, lLen-lSplit, crc16_table_a6bc);
  Check((lCrc xor $FFFF)=crc16_dnp(lBuf, lLen), 'chained DNP update equals crc16_dnp');
  Check(crc16_reflected($1234, nil, 0, crc16_table_a001)=$1234, 'Update without data returns the CRC unchanged');
end;


begin
  lErrors:=0;
  CheckString('CRC-16/ARC', @crc16_arc, '123456789', $BB3D);
  CheckString('CRC-16/MODBUS', @crc16_modbus, '123456789', $4B37);
  CheckString('CRC-16/XMODEM', @crc16_xmodem, '123456789', $31C3);
  CheckString('CRC-16/KERMIT', @crc16_kermit, '123456789', $2189);
  CheckString('CRC-16/CCITT-FALSE', @crc16_ccitt_false, '123456789', $29B1);
  CheckString('CRC-16/AUG-CCITT', @crc16_aug_ccitt, '123456789', $E5CC);
  CheckString('CRC-16/DNP', @crc16_dnp, '123456789', $EA82);
  CheckString('CRC-16/ARC', @crc16_arc, 'The quick brown fox jumps over the lazy dog', $FCDF);
  CheckString('CRC-16/XMODEM', @crc16_xmodem, 'The quick brown fox jumps over the lazy dog', $F0C8);
  CheckString('CRC-16/DNP', @crc16_dnp, 'The quick brown fox jumps over the lazy dog', $667C);
  CheckString('CRC-16/ARC', @crc16_arc, '', $0000);
  CheckString('CRC-16/MODBUS', @crc16_modbus, '', $FFFF);
  CheckString('CRC-16/DNP', @crc16_dnp, '', $FFFF);
  CheckTable('crc16_table_a001', crc16_table_a001, $A001, True);
  CheckTable('crc16_table_1021', crc16_table_1021, $1021, False);
  CheckTable('crc16_table_8408', crc16_table_8408, $8408, True);
  CheckTable('crc16_table_a6bc', crc16_table_a6bc, $A6BC, True);
  CheckChained;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
