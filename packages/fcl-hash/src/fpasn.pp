{
  This file is part of the Free Component Library.
  Copyright (c) 2023 by the Free Pascal team.

  ASN routines.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpasn;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Fcl.BaseNEnc, System.Classes, System.SysUtils, System.Hash.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Basenenc, Classes, SysUtils, fphashutils;
{$ENDIF FPC_DOTTEDUNITS}

const
  ASN1_BOOL       = $01;
  ASN1_INT        = $02;
  ASN1_BITSTR     = $03;
  ASN1_OCTSTR     = $04;
  ASN1_NULL       = $05;
  ASN1_OBJID      = $06;
  ASN1_ENUM       = $0A;
  ASN1_UTF8STRING = $0C;
  ASN1_PRINTABLESTRING = $13;
  ASN1_IA5STRING  = $16;
  ASN1_UTCTIME    = $17;
  ASN1_SEQ        = $30;
  ASN1_SETOF      = $31;
  ASN1_IPADDR     = $40;
  ASN1_COUNTER    = $41;
  ASN1_GAUGE      = $42;
  ASN1_TIMETICKS  = $43;
  ASN1_OPAQUE     = $44;
  ASN1_COUNTER64  = $46;

  ASN_emailAddress = '1.2.840.113549.1.9.1';
  ASN_commonName = '2.5.4.3';
  ASN_subjectAltName = '2.5.29.17';
  // ASN_organizationName = '2.5.4.10';
  // ASN_organizationalUnitName = '2.5.4.11';
  // ASN_countryName = '2.5.4.6';
  // ASN_stateOrProvince Name = '2.5.4.8';
  // ASN_localityName = '2.5.4.7';
  ASN_ecPublicKey = '1.2.840.10045.2.1';
  // ASN_prime256v1 = '1.2.840.10045.3.1.7';
  ASN_secp256r1 = '1.2.840.10045.3.1.7';
  ASN_ecdsa_with_SHA256 = '1.2.840.10045.4.3.2';
  ASN_ecdsa_with_SHA512 = '1.2.840.10045.4.3.4';
  ASN_ecdsa_with_SHA384 = '1.2.840.10045.4.3.3';
  ASN_ecdsa_with_SHA224 = '1.2.840.10045.4.3.1';

  ASN_MaxOIDSize = 1000;

//------------------------------------------------------------------------------
// ASN
//------------------------------------------------------------------------------
procedure ASNEncodeOID(const Value: Int64; var Result: AnsiString);
function ASNDecodeOID(var Start: Integer; const S: AnsiString): Int64; overload;
function ASNDecodeOID(var Buffer: PByte; BufferEnd: PByte): Int64; overload;
function ASNGetEncodedLen(const Len: Integer): Integer;
procedure ASNEncodeLen(const Len: Integer; var Buffer: TBytes);
function ASNReadLen(var Buffer: PByte; BufferEnd: PByte): Int32;
procedure ASNEncodeInt(Value: Int64; var Result: TBytes);
procedure ASNEncodeUInt(Value: Integer; var Result: TBytes);

procedure ASNWriteNull(s: TStream);
procedure ASNWriteInt(Value: Int64; s: TStream);
procedure ASNWriteBigInt(Value: TBytes; s: TStream);
procedure ASNWriteObjID(const ObjID: string; s: TStream);
function ASNWriteSequenceBegin(s: TMemoryStream): int64;
procedure ASNWriteSequenceEnd(SeqBegin: int64; s: TMemoryStream);
function ASNWriteBitStrBegin(s: TMemoryStream): int64;
procedure ASNWriteBitStrEnd(BitStrBegin: int64; s: TMemoryStream);

// Encodes ASN.1 object to binary form
procedure ASNObject(const Data: AnsiString; const ASNType: Integer; var Buffer: TBytes);
// Encodes an MIB OID String to binary form
procedure MibToId(Mib: AnsiString; var Result: AnsiString);
// Decodes MIB OID from binary form to String form.
procedure IdToMib(const Id: AnsiString; var Result: AnsiString); overload;
function IdToMib(Buffer, BufferEnd: PByte): string; overload;

procedure ASNDebug(const Buffer: TBytes; var Output: TBytes);
procedure ASNDebugList(const Prefix: string; List: TStrings);
procedure ASNParse(const Buffer: TBytes; List: TStrings);
procedure ASNParse_GetItem(List: TStrings; Index: integer; out ASNType, ASNSize: integer);
function ASNParse_GetIntBytes(List: TStrings; ListIndex: integer; ID: int64): TBytes;
function ASNFetch(var Buffer: PByte; BufferEnd: PByte; Out ASNType, ASNSize: Int32): Boolean; overload;
function ASNFetchOID(var Buffer: PByte; BufferEnd: PByte; out OID: UnicodeString): Boolean; overload;
function ASNFetchOID(var Buffer: PByte; BufferEnd: PByte; out OID: AnsiString): Boolean; overload;

implementation

//------------------------------------------------------------------------------
// ASN
//------------------------------------------------------------------------------

procedure ASNEncodeOID(const Value: Int64; var Result: AnsiString);
var
  B: Boolean;
  I: Integer;
  x: Int64;
  Modulo: Byte;
  S: AnsiString;

begin
  S:='';
  X := Value;
  B := False;
  repeat
    Modulo := X mod 128;
    X := X div 128;
    if B then
      Modulo := Modulo or $80;
    if x > 0 then
      B := True;
    S:=S+AnsiChar(Modulo);
  until x = 0;
  for I:=Length(S) downto 1 do
    Result:=Result+S[I];
end;

// @Start=0
function ASNDecodeOID(var Start: Integer; const S: AnsiString): Int64;
var
  x: Integer;
begin
  Result := 0;
  repeat
    x := Ord(S[Start]);
    Inc(Start);
    Result := (Result shl 7) + (x and $7F);
  until (x and $80) = 0;
end;

function ASNDecodeOID(var Buffer: PByte; BufferEnd: PByte): Int64;
var
  x: Byte;
begin
  Result := 0;
  repeat
    if Buffer>=BufferEnd then
      exit(-1);
    x := Buffer^;
    Inc(Buffer);
    Result := (Result shl 7) + (x and $7F);
    if Result>high(dword) then
      exit(-1);
  until (x and $80) = 0;
end;

procedure ASNEncodeLen(const Len: Integer; var Buffer: TBytes);
var
  x, y: Integer;
  S: String;

begin
  if Len < $80 then
  begin
    Buffer:=Concat(Buffer,[Len]);
    Exit;
  end;
  S:='';
  x := Len;
  repeat
    y := x mod 256;
    x := x div 256;
    S:=S+AnsiChar(y);
  until x = 0;
  y := Length(S);
  y := y or $80;
  S:=S+AnsiChar(y);
  for x := Length(S) downto 1 do
    Buffer:=Concat(Buffer,[Ord(S[x])]);
end;

function ASNGetEncodedLen(const Len: Integer): Integer;
var
  x: Integer;
begin
  Result := 1;
  if Len < $80 then
    Exit;
  x := Len;
  while x > 0 do
  begin
    x := x div 256;
    Inc(Result);
  end;
end;

function ASNReadLen(var Buffer: PByte; BufferEnd: PByte): Int32;
var
  Len: Integer;
begin
  if Buffer>BufferEnd then
    raise Exception.Create('20220428135218');
  Result := Buffer^;
  Inc(Buffer);
  if Result < $80 then
    Exit;
  Len := Result and $7F;
  if (Len>4) or (BufferEnd-Buffer < Len) then
    raise Exception.Create('20220428135333');
  Result := 0;
  while Len > 0 do
  begin
    Result := Result*256 + Buffer^;
    Inc(Buffer);
    Dec(Len);
  end;
end;

procedure ASNEncodeInt(Value: Int64; var Result: TBytes);

var
  x: Int64;
  y: byte;
  neg: Boolean;
  S : AnsiString;
begin
  S:='';
  neg := Value < 0;
  x := Abs(Value);
  if neg then
    x := x - 1;
  repeat
    y := x mod 256;
    x := x div 256;
    if neg then
      y := not y;
    S:=S+AnsiChar(y);
  until x = 0;
  if (not neg) and (S[Length(S)] > #$7F) then
    S:=S+#0
  else if neg and (S[Length(S)] < #$80) then
    S:=S+#$FF;
  for y := S.Length downto 1 do
    Result:=Concat(Result,[Ord(S[y])]);
end;

procedure ASNEncodeUInt(Value: Integer; var Result: TBytes);
var
  x, y: Integer;
  neg: Boolean;
  S : String;

begin
  neg := Value < 0;
  x := Value;
  if neg then
    x := x and $7FFFFFFF;
  S:='';
  repeat
    y := x mod 256;
    x := x div 256;
    S:=AnsiChar(y);
  until x = 0;
  if neg then
    S[Length(S)]:=AnsiChar(Ord(S[Length(S)]) or $80);
  for y := Length(S) downto 1 do
    Result:=Concat(Result,[Ord(S[y])]);
end;

procedure ASNWriteNull(s: TStream);
begin
  s.WriteByte(ASN1_NULL);
  s.WriteByte(0);
end;

procedure ASNWriteInt(Value: Int64; s: TStream);
var
  aBytes, aLen: TBytes;
begin
  aBytes:=[];
  ASNEncodeInt(Value,aBytes);
  aLen:=[];
  ASNEncodeLen(length(aBytes),aLen);
  s.WriteByte(ASN1_INT);
  s.Write(aLen[0],length(aLen));
  s.Write(aBytes[0],length(aBytes));
end;

procedure ASNWriteBigInt(Value: TBytes; s: TStream);
var
  EndIndex: SizeInt;
  aLen: TBytes;
  StartIndex: Integer;
begin
  EndIndex:=length(Value);
  if EndIndex=0 then
    raise Exception.Create('20220501115642');
  StartIndex:=0;
  while (StartIndex<EndIndex) and (Value[StartIndex]=0) do
    inc(StartIndex);
  if StartIndex=EndIndex then
  begin
    ASNWriteInt(0,s);
    exit;
  end;
  if Value[StartIndex]>=$80 then
    dec(StartIndex);
  aLen:=[];
  ASNEncodeLen(EndIndex-StartIndex,aLen);
  s.WriteByte(ASN1_INT);
  s.Write(aLen[0],length(aLen));
  if StartIndex<0 then
  begin
    s.WriteByte(0);
    StartIndex:=0;
  end;
  s.Write(Value[StartIndex],EndIndex-StartIndex);
end;

procedure ASNWriteObjID(const ObjID: string; s: TStream);
var
  Mib: Ansistring;
  aLen: TBytes;
begin
  Mib:='';
  MibToId(ObjID,Mib);
  aLen:=[];
  ASNEncodeLen(length(Mib),aLen);

  s.WriteByte(ASN1_OBJID);
  s.Write(aLen[0],length(aLen));
  s.Write(Mib[1],length(Mib));
end;

function ASNWriteSequenceBegin(s: TMemoryStream): int64;
begin
  s.WriteByte(ASN1_SEQ);
  s.WriteByte(0);
  Result:=s.Position;
end;

procedure ASNWriteSequenceEnd(SeqBegin: int64; s: TMemoryStream);
var
  SeqLen: Int64;
  aLen: TBytes;
  l: SizeInt;
  p: PByte;
begin
  SeqLen:=s.Position-SeqBegin;
  aLen:=[];
  ASNEncodeLen(SeqLen,aLen);
  l:=length(aLen);
  if l>1 then
  begin
    s.Write(aLen[1],l-1);
    p:=PByte(s.Memory);
    System.Move(p[SeqBegin],p[SeqBegin+l-1],SeqLen);
    System.Move(aLen[0],p[SeqBegin-1],l);
  end else
    PByte(s.Memory)[SeqBegin-1]:=aLen[0];
end;

function ASNWriteBitStrBegin(s: TMemoryStream): int64;
begin
  s.WriteByte(ASN1_BITSTR);
  s.WriteByte(0); // length
  Result:=s.Position;
  s.WriteByte(0); // trailing bit length
end;

procedure ASNWriteBitStrEnd(BitStrBegin: int64; s: TMemoryStream);
begin
  ASNWriteSequenceEnd(BitStrBegin,s);
end;

Procedure AppendStringToBuffer(var Buffer: TBytes; const aString : AnsiString);
Var
  Buflen,sLen : integer;
begin
  bufLen:=Length(Buffer);
  sLen:=Length(aString);
  SetLength(Buffer,BufLen+sLen);
  If (sLen>0) then
    Move(aString[1],Buffer[Buflen],sLen);
end;

procedure ASNObject(const Data: AnsiString; const ASNType: Integer; var Buffer: TBytes);
begin
  Buffer:=Concat(Buffer,[ASNType]);
  ASNEncodeLen(Length(Data), Buffer);
  AppendStringToBuffer(Buffer,Data);
end;

procedure DumpExStr(const S: String; var Output: TBytes);
var
  I: Integer;
  x: Byte;
begin
  for I := 1 to Length(S) do
  begin
    x := Ord(S[I]);
    if x in [65..90, 97..122] then
    begin
      AppendStringToBuffer(Output, ' +''');
      AppendStringToBuffer(Output, AnsiChar(x)+'''');
    end else
    begin
      AppendStringToBuffer(Output, ' +#$');
      AppendStringToBuffer(Output, HexStr(X,2));
    end;
  end;
end;

procedure OutputHexa(var Output: TBytes; const S: AnsiString);

var
  I: Integer;
  P: PByte;

begin
  P := PByte(PAnsiChar(S));
  for I := 1 to Length(S) do
  begin
    AppendStringToBuffer(Output, HexStr(P^,2));
    Inc(P);
  end;
end;

procedure MibToId(Mib: AnsiString; var Result: AnsiString);

  function WalkInt(var S: AnsiString): Integer;
  var
    P : Integer;
  begin
    P:=Pos('.',S);
    If P=0 then
      P:=Length(S)+1;
    Result:=StrToIntDef(Copy(S,1,P-1),0);
    S:=Copy(S,P+1,Length(S));
  end;

var
  x: Integer;
begin
  x := WalkInt(Mib);
  x := x*40 + WalkInt(Mib);
  ASNEncodeOID(x, Result);
  while (Mib<>'') do
  begin
    x := WalkInt(Mib);
    ASNEncodeOID(x, Result);
  end;
end;

procedure IdToMib(const Id: AnsiString; var Result: AnsiString);
var
  x, y, Index: Integer;
begin
  Index := 1;
  while Index <= Length(ID) do
  begin
    x := ASNDecodeOID(Index, ID);
    if Index = 2 then
    begin
      y := x div 40;
      x := x mod 40;
      Result:=IntToStr(y);
    end;
    Result:=Result+'.';
    Result:=Result+IntToStr(x);
  end;
end;

function ASNParseInt(var Buffer: PByte; BufferEnd: PByte; const ASNSize: Integer): Int64;
var
  I: Integer;
  Negative: Boolean;
  X: Byte;
begin
  Result := 0;
  Negative := False;
  for I := 1 to ASNSize do
  begin
    if Buffer>=BufferEnd then
      raise Exception.Create('20220428134948');
    X := Buffer^;
    if (I = 1) and (X > $7F) then
      Negative := True;
    if Negative then
      X := not X;
    Result := Result*256 + X;
    if Result>high(longint) then
      raise Exception.Create('20220428135614');
    Inc(Buffer);
  end;
  if Negative then
    Result := -(Result + 1);
end;

function ASNParseUInt(var Buffer: PByte; BufferEnd: PByte; const ASNSize: Integer): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to ASNSize do
  begin
    if Buffer>=BufferEnd then
      raise Exception.Create('20220428135002');
    Result := Result*256 + Buffer^;
    if Result>high(dword) then
      raise Exception.Create('20220428135614');
    Inc(Buffer);
  end;
end;

// Decode the ASN.1 item of the next element in @Buffer. Type of item is stored in @ASNType
procedure ASNDebugItem(var Buffer: PByte; BufferEnd: PByte; Out ASNType, ASNSize: Integer; var Output: TBytes);

  procedure BufToString(out S : AnsiString);
  
  var
    SA : AnsiString;

  begin
    SetLength(SA,ASNSize);
    if ASNSize>0 then
    begin
      Move(Buffer^,SA[1],ASNSize);
      inc(Buffer,ASNSize);
    end;
    S:=SA;
  end;

var
  n: Integer;
  S, S2: AnsiString;
  y: Int64;
  OldBuffer: PByte;
begin
  S:='';
  S2:='';
  ASNType := ASN1_NULL;
  if Buffer>=BufferEnd then
    Exit;
  ASNType := Buffer^;
  Inc(Buffer);
  ASNSize := ASNReadLen(Buffer, BufferEnd);
  if BufferEnd-Buffer < ASNSize then
    Exit;
  AppendStringToBuffer(Output,'$');
  AppendStringToBuffer(Output, HexStr(ASNType,2));
  if (ASNType and $20) > 0 then
  begin
    if ASNType = ASN1_SEQ then
      AppendStringToBuffer(Output, ' SEQUENCE: length ')
    else if ASNType = ASN1_SETOF then
      AppendStringToBuffer(Output, ' SET: length ')
    else
      AppendStringToBuffer(Output, ' constructed: length ');
    AppendStringToBuffer(Output, IntToStr(ASNSize));
    Exit;
  end;
  case ASNType of
    ASN1_INT, ASN1_ENUM, ASN1_BOOL:
      begin
        if ASNType = ASN1_BOOL then
          AppendStringToBuffer(Output, ' BOOL: ')
        else if ASNType = ASN1_INT then
          AppendStringToBuffer(Output, ' INT: ')
        else if ASNType = ASN1_ENUM then
          AppendStringToBuffer(Output, ' ENUM: ');
        if ASNSize < 8 then
        begin
          y := ASNParseInt(Buffer, BufferEnd, ASNSize);
          AppendStringToBuffer(Output, IntToStr(y));
        end else
        begin
          BufToString(S);
          if S[1] = Char(#00) then
          begin
            Delete(S,1,1);
          end;
          AppendStringToBuffer(Output, '$');
          OutputHexa(Output, S);
        end;
      end;
    ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS, ASN1_COUNTER64:
      begin
        if ASNType = ASN1_COUNTER then
          AppendStringToBuffer(Output, ' COUNTER: ')
        else if ASNType = ASN1_GAUGE then
          AppendStringToBuffer(Output, ' GAUGE: ')
        else if ASNType = ASN1_TIMETICKS then
          AppendStringToBuffer(Output, ' TIMETICKS: ')
        else if ASNType = ASN1_COUNTER64 then
          AppendStringToBuffer(Output, ' COUNTER64: ');
        if ASNSize < 8 then
        begin
          y := ASNParseUInt(Buffer, BufferEnd, ASNSize);
          AppendStringToBuffer(Output, IntToStr(y));
        end else
        begin
          BufToString(S);
          AppendStringToBuffer(Output, '$');
          OutputHexa(Output, S);
        end;
      end;
    ASN1_OCTSTR, ASN1_OPAQUE:
      begin
        if ASNType = ASN1_OCTSTR then
          AppendStringToBuffer(Output, ' OCTSTR: ')
        else if ASNType = ASN1_OPAQUE then
          AppendStringToBuffer(Output, ' OPAQUE: ');
        BufToString(S);
        OutputHexa(Output, S);
      end;
    ASN1_UTCTIME:
      begin // 180131123456Z -> 2018-01-31 12:34:56
        AppendStringToBuffer(Output, ' UTCTIME: ');
        BufToString(S);
        AppendStringToBuffer(Output, S);
      end;
    ASN1_BITSTR:
      begin
        AppendStringToBuffer(Output, ' BITSTR: len='+IntToStr(ASNSize)+' TrailBits='+IntToStr(Ord(Buffer^))+' ');
        Inc(Buffer); // this is the Trailing Length in bits
        Dec(ASNSize);
        OldBuffer:=Buffer;
        BufToString(S);
        OutputHexa(Output, S);
        if (ASNType = ASN1_BITSTR) and (OldBuffer^ = ASN1_SEQ) then
        begin
          // continue to decode the bitstring as ASN.1 formatted content
          Buffer:=OldBuffer;
        end;
      end;
    ASN1_UTF8STRING, ASN1_PRINTABLESTRING, ASN1_IA5STRING:
      begin
        if ASNType = ASN1_UTF8STRING then
          AppendStringToBuffer(Output, ' UTF8STRING: ')
        else if ASNType = ASN1_PRINTABLESTRING then
          AppendStringToBuffer(Output, ' PRINTABLESTRING: ')
        else if ASNType = ASN1_IA5STRING then
          AppendStringToBuffer(Output, ' IA5STRING: ');
        BufToString(S);
        AppendStringToBuffer(Output, S);
      end;
    ASN1_OBJID:
      begin
        AppendStringToBuffer(Output, ' OBJID: ');
        BufToString(S2);
        S:='';
        IdToMib(S2, S);
        AppendStringToBuffer(Output, S);
      end;
    ASN1_IPADDR:
      begin
        AppendStringToBuffer(Output, ' IPADDR: ');
        for n := 1 to ASNSize do
        begin
          if n > 1 then
            AppendStringToBuffer(Output, '.');
          y := Buffer^;
          Inc(Buffer);
          AppendStringToBuffer(Output, IntToStr(y));
        end;
      end;
    ASN1_NULL:
      begin
        AppendStringToBuffer(Output, ' NULL: ');
        Inc(Buffer, ASNSize);
      end;
  else // unknown
    begin
      AppendStringToBuffer(Output, ' unknown: ');
      BufToString(S);
      OutputHexa(Output, S);
    end;
  end;
end;

function IdToMib(Buffer, BufferEnd: PByte): string;
var
  x: Int64;
begin
  Result:='';
  while Buffer<BufferEnd do
  begin
    x := ASNDecodeOID(Buffer, BufferEnd);
    if x<0 then
      raise Exception.Create('20220427114808');
    if Result='' then
    begin
      Result:=IntToStr(x div 40);
      x := x mod 40;
    end;
    Result:=Result+'.'+IntToStr(x);
  end;
end;

// Convert ASN.1 DER encoded buffer to human readable form for debugging
procedure ASNDebug(const Buffer: TBytes; var Output: TBytes);

const
  SSpaces: AnsiString = '                                                                     ';

var
  ASNSize, ASNType, n: Integer;
  Indent: Integer;
  IndentList: Array of Integer;
  StartP, p, EndP: PByte;

begin
  if length(Buffer)=0 then exit;
  IndentList:=[];
  Indent:=0;
  StartP:=@Buffer[0];
  p:=StartP;
  EndP:=StartP+length(Buffer);
  while p<EndP do
  begin
    writeln('ASNDebug p=',p-StartP,' Type=',hexstr(p^,2),' Indent=',length(IndentList));
    // check if any sequence/set has ended and unindent
    for n := Length(IndentList)-1 downto 0 do
    begin
      ASNSize := IndentList[n];
      if p-StartP >= ASNSize then
      begin
        Delete(IndentList,n,1);
        Dec(Indent, 2);
      end;
    end;
    AppendStringToBuffer(Output, Copy(SSpaces,1,Indent));
    ASNDebugItem(p, EndP, ASNType, ASNSize, Output);
    if (ASNType and $20) > 0 then
    begin
      // sequence/set -> indent
      Inc(Indent, 2);
      IndentList:=Concat(IndentList,[ASNSize+integer(p-StartP)]);
    end;
    AppendStringToBuffer(Output, #13#10);
  end;
end;

procedure ASNParseAdd(List: TStrings; const S: String; const ASNType, ASNSize: Integer);
begin
  if ASNSize>high(word) then
    raise Exception.Create('20220428160845');
  if ASNType>high(word) then
    raise Exception.Create('20220428160853');
  List.AddObject(S, TObject(PtrInt (ASNType shl 16) or (ASNSize)));
end;

procedure ASNParseAddInt(var Buffer: PByte; BufferEnd: PByte; List: TStrings; const ASNType, ASNSize: Integer; Signed: boolean);

  procedure BufToString(var S : AnsiString);

  begin
    SetLength(S,ASNSize);
    if ASNSize=0 then
      exit;
    Move(Buffer^,S[1],ASNSize);
    inc(Buffer, ASNSize);
  end;

var
  S, S2: AnsiString;
  y:  Int64;
begin
  S:='';
  S2:='';
  if ASNSize < 8 then
  begin
    if Signed then
      y := ASNParseInt(Buffer, BufferEnd, ASNSize)
    else
      y := ASNParseUInt(Buffer, BufferEnd, ASNSize);
    S:=IntToStr(y);
  end else
  begin
    BufToString(S2);
    if S2[1] = AnsiChar(#00) then
      Delete(S2,1,1);
    BytesToHexStr(S,GetRawStringBytes(S2));
  end;
  ASNParseAdd(List, S, ASNType, ASNSize);
end;

function ASNFetch(var Buffer: PByte; BufferEnd: PByte; out ASNType,
  ASNSize: Int32): Boolean;
var
  Len: byte;
begin
  Result:=false;
  if Buffer>=BufferEnd then exit;
  ASNType := Buffer^;
  inc(Buffer);
  if Buffer>=BufferEnd then exit;
  ASNSize := Buffer^;
  Inc(Buffer);
  if ASNSize < $80 then
    Exit(true);
  Len := ASNSize and $7F;
  if (Len>4) or ((BufferEnd-Buffer)<Len) then
    exit;
  ASNSize := 0;
  while Len > 0 do
  begin
    ASNSize := ASNSize*256 + Buffer^;
    Inc(Buffer);
    Dec(Len);
  end;
  Result:=true;
end;

function ASNFetchOID(var Buffer: PByte; BufferEnd: PByte; out OID: AnsiString): Boolean; overload;

Var
  OIDS : String;

begin
  Result:=ASNFetchOID(Buffer,BufferEnd,OIDS);
  OID:=OIDS;
end;

function ASNFetchOID(var Buffer: PByte; BufferEnd: PByte; out OID: UnicodeString): Boolean;
var
  ASNType, ASNSize: Int32;
  OIDEnd: PByte;
begin
  OID:='';
  Result := ASNFetch(Buffer, BufferEnd, ASNType, ASNSize);
  if not Result then
    Exit;
  Result := ASNType = ASN1_OBJID;
  if not Result then
    Exit;
  if ASNSize=0 then
    Exit;
  if ASNSize>ASN_MaxOIDSize then
    Exit;
  if (BufferEnd-Buffer)<ASNSize then
    Exit;
  OIDEnd:=Buffer+ASNSize;
  OID:=IdToMib(Buffer, OIDEnd);
  Buffer:=OIDEnd;
  Result := OID<>'';
end;

// Beginning with the @Start position, decode the ASN.1 item of the next element in @Buffer. Type of item is stored in @ASNType
// @Offset starts at 0

function ASNParseItem(var Buffer: PByte; BufferEnd: PByte; List: TStrings): boolean;

  function BufToString(Len : Integer): AnsiString;
  begin
    SetLength(Result{%H-},Len);
    if Len=0 then exit;
      Move(Buffer^,Result[1],Len);
    inc(Buffer, Len);
  end;

var
  ASNType, ASNSize: Integer;
  n: Integer;
  S,S2: AnsiString;
  y: Int64;
  OldBuffer: PByte;

begin
  Result:=false;
  if not ASNFetch(Buffer, BufferEnd, ASNType, ASNSize) then
    Exit;
  if (ASNType and $20) > 0 then
  begin // constructed
    ASNParseAdd(List, '', ASNType, ASNSize);
    Exit;
  end;
  if (BufferEnd-Buffer) < ASNSize then
    Exit;
  S:='';
  S2:='';
  case ASNType of
    ASN1_INT, ASN1_ENUM, ASN1_BOOL:
      begin
        ASNParseAddInt(Buffer, BufferEnd, List, ASNType, ASNSize, true);
      end;
    ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS, ASN1_COUNTER64:
      begin
        ASNParseAddInt(Buffer, BufferEnd, List, ASNType, ASNSize, false);
      end;
    ASN1_BITSTR, ASN1_OCTSTR, ASN1_OPAQUE:
      begin
        if ASNType = ASN1_BITSTR then
        begin // this is the Trailing Length in bits
          Inc(Buffer);
          Dec(ASNSize);
        end;
        OldBuffer:=Buffer;
        S2 := BufToString(ASNSize);
        S:=BytesToHexStr(S2);
        ASNParseAdd(List, S, ASNType, ASNSize);
        if (ASNType = ASN1_BITSTR) and (OldBuffer^ = ASN1_SEQ) then
        begin
          // continue to decode the bitstring as ASN.1 formatted content
          Buffer:=OldBuffer;
        end;
      end;
    ASN1_UTF8STRING, ASN1_PRINTABLESTRING, ASN1_IA5STRING:
      begin
        S2 := BufToString(ASNSize);
        ASNParseAdd(List, S2, ASNType, ASNSize);
      end;
    ASN1_UTCTIME:
      begin // 180131123456Z -> 2018-01-31 12:34:56
        S2 := BufToString(ASNSize);
        ASNParseAdd(List, S2, ASNType, ASNSize);
      end;
    ASN1_OBJID:
      begin
        S2 := BufToString(ASNSize);
        IdToMib(S2, S);
        ASNParseAdd(List, S, ASNType, ASNSize);
      end;
    ASN1_IPADDR:
      begin
        for n := 1 to ASNSize do
        begin
          if n <> 1 then
            S:=S+'.';
          y := Buffer^;
          Inc(Buffer);
          S:=S+IntToStr(y);
        end;
        ASNParseAdd(List, S, ASNType, ASNSize);
      end;
    ASN1_NULL:
      begin
        ASNParseAdd(List, '', ASNType, ASNSize);
        Inc(Buffer, ASNSize);
      end;
  else // unknown
    begin
      S2 := BufToString(ASNSize);
      S:=BytesToHexStr(S2);
      ASNParseAdd(List, S, ASNType, ASNSize);
    end;
  end;
end;

procedure ASNDebugList(const Prefix: string; List: TStrings);
var
  i, ASNType, ASNSize: Integer;
begin
  for i:=0 to List.Count-1 do begin
    ASNParse_GetItem(List,i,ASNType,ASNSize);
    writeln(Prefix,' ',i,'/',List.Count,' ASNType=',hexstr(ASNType,2),' ASNSize=',ASNSize,' S="',List[i],'"');
  end;
end;

procedure ASNParse(const Buffer: TBytes; List: TStrings);
var
  P, EndP: PByte;
  O : Tbytes;
begin
  {$IFDEF ASN1_DEBUG}
  ASNDebug(Buffer,O);
  Writeln(TEncoding.UTF8.GetAnsiString(O));
  {$ENDIF}
  if length(Buffer)=0 then exit;
  P:=@Buffer[0];
  EndP:=P+length(Buffer);
  while P < EndP do
    ASNParseItem(p, EndP, List);
end;

procedure ASNParse_GetItem(List: TStrings; Index: integer; out ASNType,
  ASNSize: integer);
var
  h: PtrUInt;
begin
  h:=PtrUInt(List.Objects[Index]);
  ASNType:=h shr 16;
  ASNSize:=h and $ffff;
end;

function ASNParse_GetIntBytes(List: TStrings; ListIndex: integer; ID: int64
  ): TBytes;
var
  ASNType, ASNSize, i: Integer;
  Value: Int64;
begin
  ASNParse_GetItem(List,ListIndex,ASNType,ASNSize);
  if ASNType<>ASN1_INT then
    raise Exception.Create(IntToStr(Id));
  if ASNSize<8 then
  begin
    SetLength(Result{%H-},ASNSize);
    Value:=StrToInt64Def(List[ListIndex],0);
    for i:=ASNSize-1 downto 0 do
    begin
      Result[i]:=Value and $ff;
      Value:=Value shr 8;
    end;
  end else
    Result:=HexStrToBytes(List[ListIndex]);
  if length(Result)<1 then
    raise Exception.Create(IntToStr(Id));
end;

end.

