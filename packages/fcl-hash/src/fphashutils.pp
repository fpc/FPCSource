{ This file is part of the Free Component Library.

  Hash unit utilities.
  
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fphashutils;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
interface

uses
  SysUtils;

Type
  EHashUtil = Class(Exception);

Procedure BytesFromVar(out aBytes : TBytes; aLocation : Pointer; aSize : Integer);
Function BytesFromVar(aLocation : Pointer; aSize : Integer) : TBytes;
Procedure BytesToVar(const aBytes : TBytes; out aLocation; aSize : Integer);
Procedure BytesToVar(const aBytes : TBytes; Out aLocation : Pointer);

Procedure HexStrToBytes(Const aHexStr : AnsiString; out aBytes : TBytes); overload;
Function HexStrToBytes(Const aHexStr : AnsiString) : TBytes; overload;

Function BytesToHexStr(Const aSource : AnsiString) : Ansistring; overload;
Procedure BytesToHexStr(out aHexStr : AnsiString;Const aSource : AnsiString); overload;
Procedure BytesToHexStr(out aHexStr : AnsiString; aBytes : PByte; aSize : Integer); overload;
Procedure BytesToHexStr(out aHexStr : AnsiString; aBytes : TBytes); overload;
Function BytesToHexStr(aBytes : TBytes) : AnsiString; overload;
Procedure BytesToHexStrAppend(aBytes : TBytes;var aHexStr : AnsiString);

procedure BytesEncodeBase64(Source: Tbytes; out Dest: AnsiString; const IsURL, MultiLines, Padding: Boolean);
Function BytesEncodeBase64(Source: Tbytes; const IsURL, MultiLines, Padding: Boolean) : AnsiString;

function CryptoGetRandomBytes(Buffer: PByte; const Count: Integer): Boolean;
Function ExtractBetween(const ASource,aStart,aEnd : String) : String;

Type
  TGetRandomBytes = function(aBytes : PByte; aCount: Integer): Boolean;

var
  GetRandomBytes : TGetRandomBytes;


implementation

Procedure BytesFromVar(out aBytes : TBytes; aLocation : Pointer; aSize : Integer);

begin
  aBytes:=[];
  SetLength(aBytes,aSize);
  if aSize>0 then
    Move(aLocation^,aBytes[0],aSize);
end;

Function BytesFromVar(aLocation : Pointer; aSize : Integer) : TBytes;

begin
  BytesFromVar(Result,aLocation,aSize);
end;

Procedure BytesToVar(const aBytes : TBytes; out aLocation; aSize : Integer);

begin
  if aSize>Length(aBytes) then
    aSize:=Length(aBytes);
  Move(aBytes[0],aLocation,aSize);
end;

Procedure BytesToVar(const aBytes : TBytes; out aLocation : Pointer);

begin
  BytesToVar(aBytes,aLocation,Length(aBytes));
end;

Function HexStrToBytes(Const aHexStr : AnsiString) : TBytes;

begin
  Result:=[];
  HexStrToBytes(ahexStr,Result);
end;

Procedure HexStrToBytes(Const aHexStr : AnsiString; out aBytes : TBytes);

const
  Convert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

Var
  Len : LongInt;
  P: PAnsiChar;
  PResult: PByte;

begin
  Len:=Length(aHexStr);
  aBytes:=[];
  SetLength(aBytes, Len div 2);
  if Len=0 then Exit;
  P := PAnsiChar(aHexStr);
  PResult := PByte(aBytes);
  while Len > 0 do
  begin
    PResult^ := (Convert[P[0]] shl 4) + Convert[P[1]];
    Inc(PResult);
    Inc(P, 2);
    Dec(Len, 2);
  end;
end;

Function BytesToHexStr(Const aSource : AnsiString) : Ansistring;

begin
  BytesToHexStr(Result,aSource);
end;

Procedure BytesToHexStr(out aHexStr : AnsiString; Const aSource : AnsiString);

begin
  BytesToHexStr(aHexStr,PByte(PChar(aSource)),Length(aSource))
end;

procedure BytesToHexStr(out aHexStr : AnsiString; aBytes : PByte; aSize : Integer);

Const
  Digits: Array[0..15] of AnsiChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

var
  I: Integer;
  PB : Pbyte;
  PC : PAnsiChar;

begin
  aHexStr:='';
  SetLength(aHexStr,aSize*2);
  if aSize=0 then
    exit;
  PB:=aBytes;
  PC:=PChar(aHexStr);
  for I:=0 to aSize-1 do
    begin
    PC^:=Digits[(PB^ shr 4) and $0f];
    Inc(PC);
    PC^:=Digits[PB^ and $0f];
    Inc(PC);
    Inc(PB);
    end;
end;

Procedure BytesToHexStr(out aHexStr: AnsiString; aBytes: TBytes);

begin
  BytesToHexStr(aHexStr,PByte(aBytes),Length(aBytes));
end;

function BytesToHexStr(aBytes: TBytes): AnsiString;

begin
  BytesToHexStr(Result,aBytes);
end;

Procedure BytesToHexStrAppend(aBytes : TBytes;var aHexStr : AnsiString);

begin
  aHexStr:=aHexStr+BytesToHexStr(aBytes);
end;

function GetBase64EncodedSize(const SourceSize: Int32; const MultiLines: Boolean): Int32;
var
  Lines: Int32;
begin
  Result := (SourceSize div 3) * 4;
  if SourceSize mod 3 > 0 then
    Inc(Result, 4);
  if MultiLines then
  begin
    Lines := Result div 76;
    Inc(Result, Lines*2); // #13#10 for each lines
  end;
end;

procedure BytesEncodeBase64(Source: Tbytes; out Dest: AnsiString; const IsURL, MultiLines, Padding: Boolean);

const
  FCodingTable: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  FCodingTableURL: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

  procedure XBufferEncode64_1(var DestBuf: PByte; const AIn1: Byte; const IsURL: Boolean); inline;
  begin
    if IsURL then
    begin
      DestBuf[0] := Ord(FCodingTableURL[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTableURL[((AIn1 shl 4) and 63) + 1]);
    end else begin
      DestBuf[0] := Ord(FCodingTable[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTable[((AIn1 shl 4) and 63) + 1]);
    end;
    Inc(DestBuf,2);
  end;

  procedure XBufferEncode64_2(var DestBuf: PByte; const AIn1, AIn2: Byte; const IsURL: Boolean); inline;
  begin
    if IsURL then
    begin
      DestBuf[0] := Ord(FCodingTableURL[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTableURL[(((AIn1 shl 4) or (AIn2 shr 4)) and 63) + 1]);
      DestBuf[2] := Ord(FCodingTableURL[((AIn2 shl 2) and 63) + 1]);
    end else begin
      DestBuf[0] := Ord(FCodingTable[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTable[(((AIn1 shl 4) or (AIn2 shr 4)) and 63) + 1]);
      DestBuf[2] := Ord(FCodingTable[((AIn2 shl 2) and 63) + 1]);
    end;
    Inc(DestBuf,3);
  end;

  procedure XBufferEncode64_3(var DestBuf: PByte; const AIn1, AIn2, AIn3: Byte; const IsURL: Boolean); inline;
  begin
    if IsURL then
    begin
      DestBuf[0] := Ord(FCodingTableURL[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTableURL[(((AIn1 shl 4) or (AIn2 shr 4)) and 63) + 1]);
      DestBuf[2] := Ord(FCodingTableURL[(((AIn2 shl 2) or (AIn3 shr 6)) and 63) + 1]);
      DestBuf[3] := Ord(FCodingTableURL[(Ord(AIn3) and 63) + 1]);
    end else begin
      DestBuf[0] := Ord(FCodingTable[((AIn1 shr 2) and 63) + 1]);
      DestBuf[1] := Ord(FCodingTable[(((AIn1 shl 4) or (AIn2 shr 4)) and 63) + 1]);
      DestBuf[2] := Ord(FCodingTable[(((AIn2 shl 2) or (AIn3 shr 6)) and 63) + 1]);
      DestBuf[3] := Ord(FCodingTable[(Ord(AIn3) and 63) + 1]);
    end;
    Inc(DestBuf,4);
  end;

var
  BufSize, BufSize3: Int32;
  Ch1, Ch2, Ch3: Byte;
  DestBuf, SourceBuf: PByte;
  DestCapacity, DestSize: Int32;
  Index, IndexCRLF : Int32;
begin
  BufSize := Length(Source);
  if BufSize = 0 then
    Exit;
  DestCapacity := GetBase64EncodedSize(BufSize, MultiLines);
  DestSize := 0;
  Dest:='';
  SetLength(Dest, DestCapacity);
  SourceBuf := PByte(Source);
  DestBuf := PByte(Dest);
  IndexCRLF := 0;
  Index := 0;
  BufSize3 := (BufSize div 3)*3;
  while Index < BufSize3 do
  begin // Process the buffer up to the trailing 2 chars
    Ch1 := SourceBuf[0];
    Ch2 := SourceBuf[1];
    Ch3 := SourceBuf[2];
    SourceBuf := Pointer(PtrUInt(SourceBuf)+3);
    Inc(Index, 3);
    XBufferEncode64_3(DestBuf, Ch1, Ch2, Ch3, IsURL);
    Inc(DestSize, 4);
    if MultiLines then
    begin
      if (IndexCRLF = 18) and (Index < BufSize3) then // KW 20170405 BufSize -> BufSize3
      begin
        DestBuf[0] := Ord(#13);
        DestBuf[1] := Ord(#10);
        DestBuf := Pointer(PtrUInt(DestBuf)+2);
        Inc(DestSize, 2);
        IndexCRLF := 0;
      end else
        Inc(IndexCRLF);
    end;
  end;
  if MultiLines and (IndexCRLF = 19) and (Index < BufSize) then  // KW 20170405 IndexCRLF=18 -> 19
  begin
    DestBuf[0] := Ord(#13);
    DestBuf[1] := Ord(#10);
    DestBuf := Pointer(PtrUInt(DestBuf)+2);
    Inc(DestSize, 2);
  end;
  if Index = BufSize-2 then // Last remaining 2 chars
  begin
    Ch1 := SourceBuf[0];
    Ch2 := SourceBuf[1];
    XBufferEncode64_2(DestBuf, Ch1, Ch2, IsURL);
    Inc(DestSize, 3);
    if Padding then
    begin
      DestBuf[0] := Ord('=');
      Inc(DestSize);
    end;
  end else if Index = BufSize-1 then // Last remaining char
  begin
    Ch1 := Source[Index];
    XBufferEncode64_1(DestBuf, Ch1, IsURL);
    Inc(DestSize, 2);
    if Padding then
    begin
      DestBuf[0] := Ord('=');
      DestBuf[1] := Ord('=');
      Inc(DestSize, 2);
    end;
  end;
  SetLength(Dest,DestSize);
end;

Function BytesEncodeBase64(Source: Tbytes; const IsURL, MultiLines, Padding: Boolean) : AnsiString;

begin
  BytesEncodeBase64(Source,Result,IsURL, MultiLines, Padding);
end;

type
  TUInt32 = Cardinal;
  PUInt32Array = ^TUInt32;
  TLecuyer = record
    rs1, rs2, rs3: UInt32;
    SeedCount: UInt32;
    procedure Seed;
    function Next: UInt32;
  end;


procedure TLecuyer.Seed;
var
  VLI: Array[0..2] of byte;
  I : Integer;

begin
  I:=0;
  Repeat
    Inc(I);
    if (Pointer(GetRandomBytes)=Nil) or not GetRandomBytes(@VLI,Sizeof(VLI)) then
       Raise EHashUtil.Create('Cannot seed Lecuyer: no random bytes');
    rs1 := VLI[0];
    rs2 := VLI[1];
    rs3 := VLI[2];
  Until ((RS1>1) and (rs2>7) and (RS3>15)) or (I>100);
  if I>100 then
    Raise EHashUtil.Create('Cannot seed Lecuyer: no suitable random bytes');
  SeedCount := 1;
end;

function TLecuyer.Next: UInt32;
begin
  if SeedCount and $FFFF = 0 then // reseed after 256KB of output
    Seed
  else
    Inc(SeedCount);
  Result := rs1;
  rs1 := ((Result and -2) shl 12) xor (((Result shl 13) xor Result) shr 19);
  Result := rs2;
  rs2 := ((Result and -8) shl 4) xor (((Result shl 2) xor Result) shr 25);
  Result := rs3;
  rs3 := ((Result and -16) shl 17) xor (((Result shl 3) xor Result) shr 11);
  Result := rs1 xor rs2 xor result;
end;


function CryptoGetRandomBytes(Buffer: PByte; const Count: Integer): Boolean;

var
  I, Remainder, Rounds: Integer;
  Lecuyer: TLecuyer;
  R: UInt32;
begin
  Result := True;
  Lecuyer.Seed;
  Rounds := Count div SizeOf(UInt32);
  for I := 0 to Rounds-1 do
    PUInt32Array(Buffer)[I] := Lecuyer.Next;
  Remainder := Count mod SizeOf(UInt32);
  if Remainder > 0 then
  begin
    R := Lecuyer.Next;
    Move(R, PByteArray(Buffer)[Rounds*SizeOf(UInt32)], Remainder);
    R:=R+R;// Silence compiler warning
  end;
end;

function ExtractBetween(const ASource, aStart, aEnd: String): String;

Var
  P1,P2 : Integer;

begin
  Result:='';
  P1:=Pos(aStart,ASource);
  if P1<=0 then exit;
  Inc(P1,Length(aStart));
  P2:=Pos(aEnd,ASource,P1);
  if P2<=0 then exit;
  Result:=Copy(aSource,P1,P2-P1);

end;

function IntGetRandomNumber(aBytes : PByte; aCount: Integer): Boolean;

Var
  i : Integer;
  P : PByte;

begin
  P:=aBytes;
  For I:=0 to aCount-1 do
    begin
    P^:=Random(256);
    Inc(P);
    end;
  Result:=True;
end;

begin
  GetRandomBytes:=@IntGetRandomNumber;
end.

