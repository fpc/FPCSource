unit fpasn;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
interface

uses
 Basenenc, Classes, SysUtils, fphashutils;

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
  // ASN_stateOrProvinceName = '2.5.4.8';
  // ASN_localityName = '2.5.4.7';
  ASN_ecPublicKey = '1.2.840.10045.2.1';
  // ASN_prime256v1 = '1.2.840.10045.3.1.7';
  ASN_secp256r1 = '1.2.840.10045.3.1.7';
  ASN_ecdsa_with_SHA256 = '1.2.840.10045.4.3.2';
  ASN_ecdsa_with_SHA512 = '1.2.840.10045.4.3.4';
  ASN_ecdsa_with_SHA384 = '1.2.840.10045.4.3.3';
  ASN_ecdsa_with_SHA224 = '1.2.840.10045.4.3.1';

//------------------------------------------------------------------------------
// ASN
//------------------------------------------------------------------------------
procedure ASNEncodeOID(const Value: Int64; var Result: AnsiString);
function ASNDecodeOID(var Start: Integer; const S: AnsiString): Int64;
function ASNGetEncodedLen(const Len: Integer): Integer;
procedure ASNEncodeLen(const Len: Integer; var Buffer: TBytes);
function ASNReadLen(const Buffer: TBytes; var Offset: Int32): Int32;
procedure ASNEncodeInt(Value: Int64; var Result: TBytes);
procedure ASNEncodeUInt(Value: Integer; var Result: TBytes);
// Encodes ASN.1 object to binary form
procedure ASNObject(const Data: Ansistring; const ASNType: Integer; var Buffer: TBytes);
// Encodes an MIB OID Ansistring to binary form
procedure MibToId(Mib: Ansistring; var Result: AnsiString);
// Decodes MIB OID from binary form to Ansistring form.
procedure IdToMib(const Id: Ansistring; var Result: Ansistring);
procedure ASNDebug(const Buffer: TBytes; var Output: TBytes);
procedure ASNParse(const Buffer: TBytes; List: TStrings);
procedure PemToDER(const PEM: AnsiString; const BeginTag, EndTag: Ansistring; Out DER: TBytes); overload;
procedure PemToDER(PEM: TBytes; const BeginTag, EndTag: Ansistring; Out DER: TBytes); overload;
procedure ASNParsePemSection(const PEM: TBytes; List: TStrings; const BeginTag, EndTag: Ansistring);
procedure ASNParsePemSection(const PEM: AnsiString; List: TStrings; const BeginTag, EndTag: Ansistring);


implementation

//------------------------------------------------------------------------------
// ASN
//------------------------------------------------------------------------------

procedure ASNEncodeOID(const Value: Int64; var Result: Ansistring);
var
  B: Boolean;
  I: Integer;
  x: Int64;
  Modulo: Byte;
  S: Ansistring;

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
    S:=S+Char(Modulo);
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

procedure ASNEncodeLen(const Len: Integer; var Buffer: TBytes);
var
  x, y: Integer;
  S: AnsiString;

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
    S:=S+Char(y);
  until x = 0;
  y := Length(S);
  y := y or $80;
  S:=S+Char(y);
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

function ASNReadLen(const Buffer: TBytes; var Offset: Int32): Int32;
var
  Len: Integer;
begin
  Result := Buffer[Offset];
  Inc(Offset);
  if Result < $80 then
    Exit;
  Len := Result and $7F;
  Result := 0;
  while Len > 0 do
  begin
    Result := Result*256 + Buffer[Offset];
    Inc(Offset);
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
  S : AnsiString;

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

procedure DumpExStr(const S: AnsiString; var Output: TBytes);
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
  P := PByte(PChar(S));
  for I := 1 to Length(S) do
  begin
    AppendStringToBuffer(Output, HexStr(P^,2));
    Inc(P);
  end;
end;

// @Result[256]
procedure MibToId(Mib: AnsiString; var Result: AnsiString);

  function WalkInt(var S: AnsiString): Integer;
  var
    P : Integer;

  begin
    P:=Pos('.',S);
    If P=0 then
      P:=Length(S)+1;
    Result:=StrToIntDef(Copy(S,1,P-1),0);
    S:=Copy(S,Pos('.',S)+1,Length(S));
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

// @Result[256]
procedure IdToMib(const ID: AnsiString; var Result: AnsiString);
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

function ASNParseInt(const Buffer: TBytes; var Start: Integer; const ASNSize: Integer): Int64;
var
  I: Integer;
  Negative: Boolean;
  X: Byte;
begin
  Result := 0;
  Negative := False;
  for I := 1 to ASNSize do
  begin
    X := Buffer[Start];
    if (I = 1) and (X > $7F) then
      Negative := True;
    if Negative then
      X := not X;
    Result := Result*256 + X;
    Inc(Start);
  end;
  if Negative then
    Result := -(Result + 1);
end;

function ASNParseUInt(const Buffer: TBytes; var Start: Integer; const ASNSize: Integer): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to ASNSize do
  begin
    Result := Result*256 + Buffer[Start];
    Inc(Start);
  end;
end;

// Beginning with the @Start position, decode the ASN.1 item of the next element in @Buffer. Type of item is stored in @ASNType
procedure ASNDebugItem(const Buffer: TBytes; var Start: Integer; Out ASNType, ASNSize: Integer; var Output: TBytes);

  procedure BufToString(out S : AnsiString);

  begin
    S:='';
    SetLength(S,ASNSize);
    if ASNSize>0 then
      Move(Buffer[Start],S[1],ASNSize);
  end;


var
  l, n: Integer;
  S, S2: AnsiString;
  y: Int64;
begin
  S:='';
  S2:='';
  ASNType := ASN1_NULL;
  l := Length(Buffer);
  if Start > l then
    Exit;
  ASNType := Buffer[Start];
  Inc(Start);
  ASNSize := ASNReadLen(Buffer, Start);
  if (Start + ASNSize) > l then
    Exit;
  AppendStringToBuffer(Output,'$');
  AppendStringToBuffer(Output, HexStr(ASNType,2));
  if (ASNType and $20) > 0 then
  begin
//    XBufferAppend(Output, Buffer, Start, ASNSize)
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
          y := ASNParseInt(Buffer, Start, ASNSize);
          AppendStringToBuffer(Output, IntToStr(y));
        end else
        begin
          BufToString(S);
          if S[1] = Char(#00) then
            Delete(S,1,1);
          AppendStringToBuffer(Output, '$');
          OutputHexa(Output, S);
          Inc(Start, ASNSize);
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
          y := ASNParseUInt(Buffer, Start, ASNSize);
          AppendStringToBuffer(Output, IntToStr(y));
        end else
        begin
          BufToString(S);
          AppendStringToBuffer(Output, '$');
          OutputHexa(Output, S);
          Inc(Start, ASNSize);
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
        Inc(Start, ASNSize);
      end;
    ASN1_UTCTIME:
      begin // 180131123456Z -> 2018-01-31 12:34:56
        AppendStringToBuffer(Output, ' UTCTIME: ');
        BufToString(S);
        AppendStringToBuffer(Output, S);
        Inc(Start, ASNSize);
      end;
    ASN1_BITSTR:
      begin
        AppendStringToBuffer(Output, ' BITSTR: ');
        Inc(Start); // this is the Trailing Length in bits
        Dec(ASNSize);
        BufToString(S);
        OutputHexa(Output, S);
        if (ASNType = ASN1_BITSTR) and (Buffer[Start] = ASN1_SEQ) then
        begin
          // continue to decode the bitstring as ASN.1 formatted content
        end else
          Inc(Start, ASNSize);
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
        Inc(Start, ASNSize);
      end;
    ASN1_OBJID:
      begin
        AppendStringToBuffer(Output, ' OBJID: ');
        BufToString(S2);
        S:='';
        IdToMib(S2, S);
        AppendStringToBuffer(Output, S);
        Inc(Start, ASNSize);
      end;
    ASN1_IPADDR:
      begin
        AppendStringToBuffer(Output, ' IPADDR: ');
        for n := 1 to ASNSize do
        begin
          if n <> 1 then
            AppendStringToBuffer(Output, '.');
          y := Buffer[Start];
          Inc(Start);
          AppendStringToBuffer(Output, IntToStr(y));
        end;
      end;
    ASN1_NULL:
      begin
        AppendStringToBuffer(Output, ' NULL: ');
        Inc(Start, ASNSize);
      end;
  else // unknown
    begin
      AppendStringToBuffer(Output, ' unknown: ');
      BufToString(S);
      OutputHexa(Output, S);
      Inc(Start, ASNSize);
    end;
  end;
end;

// Convert ASN.1 DER encoded buffer to human readable form for debugging

procedure ASNDebug(const Buffer: TBytes; var Output: TBytes);

const
  SSpaces: AnsiString = '                                                                     ';

var
  ASNSize, ASNType, Index, n: Integer;
  Indent: Integer;
  IndentList: Array of Integer;

begin
  IndentList:=[];
  Indent:=0;
  Index := 0;
  while Index < Length(Buffer) do
  begin
    for n := Length(IndentList)-1 downto 0 do
    begin
      ASNSize := IndentList[n];
      if ASNSize <= Index then
      begin
        Delete(IndentList,n,1);
        Dec(Indent, 2);
      end;
    end;
    AppendStringToBuffer(Output, Copy(SSpaces,1,Indent));
    ASNDebugItem(Buffer, Index, ASNType, ASNSize, Output);
    if (ASNType and $20) > 0 then
    begin
      Inc(Indent, 2);
      IndentList:=Concat(IndentList,[ASNSize+Index-1]);
    end;
    AppendStringToBuffer(Output, #13#10);
  end;
end;

procedure ASNParseAdd(List: TStrings; const S: AnsiString; const ASNType, ASNSize: Integer);
begin
  List.AddObject(S, TObject(PtrInt (ASNType shl 16) or (ASNSize)));
end;

procedure ASNParseAddInt(const Buffer: TBytes; var Start: Integer; List: TStrings; const ASNType, ASNSize: Integer);

  procedure BufToString(var S : AnsiString);

  begin
    SetLength(S,ASNSize);
    if ASNSize>0 then
      Move(Buffer[Start],S[1],ASNSize);
  end;

var
  S, S2: AnsiString;
  y: Int64;
begin
  S:='';
  S2:='';
  if ASNSize < 8 then
  begin
    y := ASNParseInt(Buffer, Start, ASNSize);
    S:=IntToStr(y);
  end else
  begin
    BufToString(S2);
    if S2[1] = Char(#00) then
      Delete(S2,1,1);
    BytesToHexStr(S,TEncoding.UTF8.GetAnsiBytes(S2));
    Inc(Start, ASNSize);
  end;
  ASNParseAdd(List, S, ASNType, ASNSize);
end;

procedure ASNParseAddUInt(const Buffer: TBytes; var Start: Integer; List: TStrings; const ASNType, ASNSize: Integer);

  procedure BufToString(out S : AnsiString);

  begin
    S:='';
    SetLength(S,ASNSize);
    if ASNSize>0 then
      Move(Buffer[Start],S[1],ASNSize);
  end;
var
  S, S2: AnsiString;
  y: Int64;
begin
  S:='';
  S2:='';
  if ASNSize < 8 then
  begin
    y := ASNParseUInt(Buffer, Start, ASNSize);
    S:=IntToStr(y);
  end else
  begin
    BufToString(S2);
    if S2[1] = Char(#00) then
      Delete(S2,1,1);
    BytesToHexStr(S,TEncoding.UTF8.GetAnsiBytes(S2));
    Inc(Start, ASNSize);
  end;
  ASNParseAdd(List, S, ASNType, ASNSize);
end;

function ASNFetch(const Buffer: TBytes; var Offset: Int32; Out ASNType, ASNSize: Int32): Boolean;
var
  Len: Int32;
begin
  Result := False;
  Len := Length(Buffer);
  if Offset > Len then
    Exit;
  ASNType := Buffer[Offset];
  Inc(Offset);
  ASNSize := ASNReadLen(Buffer, Offset);
  if (Offset + ASNSize) > Len then
    Exit;
  Result := True;
end;

// Beginning with the @Start position, decode the ASN.1 item of the next element in @Buffer. Type of item is stored in @ASNType
// @Offset starts at 0

procedure ASNParseItem(const Buffer: TBytes; var Offset: Int32; List: TStrings);

  procedure BufToString(var S : AnsiString; P : PByte; Len : Integer);

  begin
    SetLength(S,Len);
    if Len>0 then
      Move(P^,S[1],Len);
  end;

var
  ASNType, ASNSize: Integer;
  n: Integer;
  S, S2: AnsiString;
  y: Int64;

begin
  if not ASNFetch(Buffer, Offset, ASNType, ASNSize) then
    Exit;
  if (ASNType and $20) > 0 then
  begin // constructed
    ASNParseAdd(List, '', ASNType, ASNSize);
    Exit;
  end;
  S:='';
  S2:='';
  case ASNType of
    ASN1_INT, ASN1_ENUM, ASN1_BOOL:
      begin
        ASNParseAddInt(Buffer, Offset, List, ASNType, ASNSize);
      end;
    ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS, ASN1_COUNTER64:
      begin
        ASNParseAddUInt(Buffer, Offset, List, ASNType, ASNSize);
      end;
    ASN1_BITSTR, ASN1_OCTSTR, ASN1_OPAQUE:
      begin
        if ASNType = ASN1_BITSTR then
        begin // this is the Trailing Length in bits
          Inc(Offset);
          Dec(ASNSize);
        end;
        BufToString(S2, @Buffer[Offset], ASNSize);
        S:=BytesToHexStr(S2);
        ASNParseAdd(List, S, ASNType, ASNSize);
        if (ASNType = ASN1_BITSTR) and (Buffer[Offset] = ASN1_SEQ) then
        begin
          // continue to decode the bitstring as ASN.1 formatted content
        end else
          Inc(Offset, ASNSize);
      end;
    ASN1_UTF8STRING, ASN1_PRINTABLESTRING, ASN1_IA5STRING:
      begin
        BufToString(S2, @Buffer[Offset], ASNSize);
        ASNParseAdd(List, S2, ASNType, ASNSize);
        Inc(Offset, ASNSize);
      end;
    ASN1_UTCTIME:
      begin // 180131123456Z -> 2018-01-31 12:34:56
        BufToString(S2, @Buffer[Offset], ASNSize);
        ASNParseAdd(List, S2, ASNType, ASNSize);
        Inc(Offset, ASNSize);
      end;
    ASN1_OBJID:
      begin
        BufToString(S2, @Buffer[Offset], ASNSize);
        IdToMib(S2, S);
        ASNParseAdd(List, S, ASNType, ASNSize);
        Inc(Offset, ASNSize);
      end;
    ASN1_IPADDR:
      begin
        for n := 1 to ASNSize do
        begin
          if n <> 1 then
            S:=S+'.';
          y := Buffer[Offset];
          Inc(Offset);
          S:=S+IntToStr(y);
        end;
        ASNParseAdd(List, S, ASNType, ASNSize);
      end;
    ASN1_NULL:
      begin
        ASNParseAdd(List, '', ASNType, ASNSize);
        Inc(Offset, ASNSize);
      end;
  else // unknown
    begin
      BufToString(S2, @Buffer[Offset], ASNSize);
      S:=BytesToHexStr(S2);
      ASNParseAdd(List, S, ASNType, ASNSize);
      Inc(Offset, ASNSize);
    end;
  end;
end;

// Convert ASN.1 DER encoded buffer to human readable form for debugging
procedure ASNParse(const Buffer: TBytes; List: TStrings);
var
  Index: integer;
begin
  Index := 0;
  while Index < Length(Buffer) do
    ASNParseItem(Buffer, Index, List);
end;

procedure PemToDER(PEM: TBytes; const BeginTag, EndTag: AnsiString; out DER: TBytes);

begin
  PemToDER(TEncoding.UTF8.GetAnsiString(PEM),BeginTag,EndTag,DER);
end;

procedure PemToDER(Const PEM: AnsiString; const BeginTag, EndTag: AnsiString; Out DER: TBytes);

var
  Content: AnsiString;

begin
  DER:=[];
  Content:=ExtractBetween(Pem, BeginTag, EndTag);
  Content:=Trim(Content);
  if Length(Content) = 0 then
    Exit;
  DER:=Base64.Decode(Content,True);
end;

procedure ASNParsePemSection(const PEM: TBytes; List: TStrings; const BeginTag, EndTag: AnsiString);

begin
  ASNParsePemSection(TEncoding.UTF8.GetAnsiString(PEM),List,BeginTag,EndTag);
end;

procedure ASNParsePemSection(const PEM: AnsiString; List: TStrings; const BeginTag, EndTag: AnsiString);

var
  BufferSection,res: TBytes;
//  S : AnsiString;

begin
  List.Clear;
  PemToDER(PEM, BeginTag, EndTag, BufferSection);
  {ASNDebug(BufferSection,Res);
  S:=TEncoding.UTF8.GetAnsiString(Res);
  Writeln('ASN Debug: ',S);}
  ASNParse(BufferSection, List);
end;


end.

