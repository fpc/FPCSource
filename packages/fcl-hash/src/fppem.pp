{
  This file is part of the Free Component Library.
  Copyright (c) 2023 by the Free Pascal team.

  PEM key management

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit fppem;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Fcl.BaseNEnc, System.Hash.Sha256, System.Hash.Asn, System.Hash.Utils, System.Hash.Ecc;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, basenenc, fpsha256, fpasn, fphashutils, fpecc;
{$ENDIF FPC_DOTTEDUNITS}

const
  _BEGIN_CERTIFICATE = '-----BEGIN CERTIFICATE-----';
  _END_CERTIFICATE = '-----END CERTIFICATE-----';
  _BEGIN_EC_PARAMETERS = '-----BEGIN EC PARAMETERS-----';
  _END_EC_PARAMETERS = '-----END EC PARAMETERS-----';
  _BEGIN_EC_PRIVATE_KEY = '-----BEGIN EC PRIVATE KEY-----';
  _END_EC_PRIVATE_KEY = '-----END EC PRIVATE KEY-----';
  _BEGIN_RSA_PRIVATE_KEY = '-----BEGIN RSA PRIVATE KEY-----';
  _END_RSA_PRIVATE_KEY = '-----END RSA PRIVATE KEY-----';
  _BEGIN_PRIVATE_KEY = '-----BEGIN PRIVATE KEY-----';
  _END_PRIVATE_KEY = '-----END PRIVATE KEY-----';
  _BEGIN_PUBLIC_KEY = '-----BEGIN PUBLIC KEY-----';
  _END_PUBLIC_KEY = '-----END PUBLIC KEY-----';

type
  TPrivateKeyType = (pktNone, pktPKCS8, pktRSA, pktEC);

function PemIsECDSA(const aStream : TStream; List: TStrings): Boolean;
function PemIsECDSA(const FileName: String; List: TStrings): Boolean;

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString);
procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString);

function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;
function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString): Boolean;
function PemLoadECDSA(const aStream : TStream; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;
function PemLoadECDSA(const aStream : TStream; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString): Boolean;

procedure PemLoadPrivateKeyAsDER(const PEM: String; out PrivateKey, ECParams: TBytes; out PrivateKeyType: TPrivateKeyType);
// Expiry date as YYYYMMDD from a certificate parsed with ASNParse: the second of the first two consecutive
// UTCTime or GeneralizedTime items (notBefore, notAfter). Empty when there are no such items.
function PemParseValidUntil(List: TStrings): String;

function PemLineIsTag(const Line, Tag: string): boolean; // true if Line contains Tag, ignoring leading and trailing spaces
function PemLineIsBase64(const Line: string): boolean; // true if Line contains only spaces and base64 chars
// Line indexes of the first complete BeginTag..EndTag block without another BeginTag inside;
// False and both indexes 0 when there is none.
function PemFindTags(List: TStrings; const BeginTag, EndTag: string; out StartIndex, EndIndex: integer): boolean;
function PemExtractBetween(const PEM, BeginTag, EndTag: String): String;
function PemToDER(const PEM, BeginTag, EndTag: String): TBytes; overload;
procedure PemToDER(PEM: TBytes; const BeginTag, EndTag: String; out DER: TBytes); overload;

procedure ASNParsePemSection(const PEM: TBytes; List: TStrings; const BeginTag, EndTag: String);
procedure ASNParsePemSection(const PEM: String; List: TStrings; const BeginTag, EndTag: String);

implementation

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString);

Var
  S : String;

begin
  PemLoadPublicKey64FromList(List,PrivateKey,PublicKey,PublicKeyX64,PublicKeyY64);
  // Thumbprint
  S:='{"crv":"P-256","kty":"EC","x":"' + PublicKeyX64 + '","y":"' + PublicKeyY64 + '"}';
  TSHA256.DigestBase64(GetRawStringBytes(S),True,ThumbPrint);
end;

// True when List, from ASNParse, has the shape of an SEC1 EC private key with a public key:
// INTEGER version, OCTET STRING private key, [0] curve OID, [1] BIT STRING public key.
function PemIsECPrivateKeyList(List: TStrings): Boolean;

  function ItemType(aIndex: Integer): Integer;
  var
    lSize: Integer;
  begin
    ASNParse_GetItem(List, aIndex, Result, lSize);
  end;

begin
  Result := (List.Count >= 7)
            and (ItemType(1) = ASN1_INT)
            and (ItemType(2) = ASN1_OCTSTR)
            and (ItemType(4) = ASN1_OBJID)
            and (ItemType(6) = ASN1_BITSTR);
end;

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString);

var
  SPrivateKeyHexa: AnsiString;
  SPrivateKey : TBytes;
  Uncompressed: array[0..2*ECC_BYTES] of Byte;

begin
  if not PemIsECPrivateKeyList(List) then
    Raise EHashUtil.Create('Not an EC private key');
  SPrivateKeyHexa := List.Strings[2];
  HexStrToBytes(SPrivateKeyHexa, SPrivateKey);
  if Length(SPrivateKey)>=SizeOf(PrivateKey) then
    BytesToVar(SPrivateKey,Privatekey,SizeOf(Privatekey))
  else
    Raise EHashUtil.CreateFmt('Wrong private key length, got %d, expected %d',[Length(SPrivateKey),SizeOf(TPrivateKey)]);
  // Compressed (02/03 x) or uncompressed (04 x y) public key; raises EHashUtil for a point not on the curve
  EccPublicKeyFromHexa(PublicKey, List.Strings[6]);
  if not EccExportX963(PublicKey, @Uncompressed, SizeOf(Uncompressed)) then
    Raise EHashUtil.Create('Invalid EC public key');
  PublicKeyX64:=base64URL.Encode(BytesFromVar(@Uncompressed[1],ECC_BYTES),False);
  PublicKeyY64:=base64URL.Encode(BytesFromVar(@Uncompressed[1+ECC_BYTES],ECC_BYTES),False);
end;

function PemIsECDSA(const FileName: String; List: TStrings): Boolean;

var
  F : TFileStream;
begin
  F:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  try
    Result:=PemIsECDSA(F,List);
  finally
    F.Free;
  end;
end;


function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey;
  out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString
  ): Boolean;

var
  List: TStrings;

begin
  List := TStringList.Create;
  try
    Result:=FileExists(FileName) and PemIsECDSA(FileName, List);
    if Result then
      PemLoadPublicKey64FromList(List, PrivateKey, PublicKey, PublicKeyX64, PublicKeyY64);
  finally
    List.Free;
  end;
end;


function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey;
  out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64,
  ThumbPrint: AnsiString): Boolean;

var
  List: TStrings;

begin
  List := TStringList.Create;
  try
    Result:=FileExists(FileName) and PemIsECDSA(FileName, List);
    if Result then
      PemLoadPublicKey64FromList(List, PrivateKey, PublicKey, PublicKeyX64, PublicKeyY64,ThumbPrint);
  finally
    List.Free;
  end;
end;

function PemIsECDSA(const aStream : TStream; List: TStrings): Boolean;

var
  Buffer: TBytes;
  CurveOID: String;

begin
  Result := False;
  Buffer:=[];
  SetLength(Buffer,aStream.Size);
  aStream.ReadBuffer(Buffer,aStream.Size);
  ASNParsePemSection(Buffer, List, _BEGIN_EC_PRIVATE_KEY, _END_EC_PRIVATE_KEY);
  if not PemIsECPrivateKeyList(List) then
    Exit;
  CurveOID := List.Strings[4];
  Result := (CurveOID=ASN_secp256r1);
end;

function PemLoadECDSA(const aStream: TStream; out PrivateKey: TEccPrivateKey;
  out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64,
  ThumbPrint: AnsiString): Boolean;

var
  List: TStrings;

begin
  List := TStringList.Create;
  try
    Result:=PemIsECDSA(aStream, List);
    if Result then
      PemLoadPublicKey64FromList(List, PrivateKey, PublicKey, PublicKeyX64, PublicKeyY64, ThumbPrint);
  finally
    List.Free;
  end;
end;

function PemLoadECDSA(const aStream: TStream; out PrivateKey: TEccPrivateKey;
  out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString
  ): Boolean;

var
  List: TStrings;

begin
  List := TStringList.Create;
  try
    Result:=PemIsECDSA(aStream, List);
    if Result then
      PemLoadPublicKey64FromList(List, PrivateKey, PublicKey, PublicKeyX64, PublicKeyY64);
  finally
    List.Free;
  end;
end;

procedure PemLoadPrivateKeyAsDER(const PEM: String; out PrivateKey,
  ECParams: TBytes; out PrivateKeyType: TPrivateKeyType);
begin
  PrivateKeyType := pktNone;
  ECParams:=[];
  PrivateKey := PemToDER(PEM, _BEGIN_EC_PRIVATE_KEY, _END_EC_PRIVATE_KEY);
  if length(PrivateKey)>0 then
  begin
    PrivateKeyType := pktEC;
    ECParams := PemToDER(PEM, _BEGIN_EC_PARAMETERS, _END_EC_PARAMETERS);
    Exit;
  end;
  PrivateKey := PemToDER(PEM, _BEGIN_RSA_PRIVATE_KEY, _END_RSA_PRIVATE_KEY);
  if length(PrivateKey)>0 then
  begin
    PrivateKeyType := pktRSA;
    Exit;
  end;
  PrivateKey := PemToDER(PEM, _BEGIN_PRIVATE_KEY, _END_PRIVATE_KEY);
  if length(PrivateKey)>0 then
  begin
    PrivateKeyType := pktPKCS8;
    Exit;
  end;
end;

// YYYYMMDD from a UTCTime (YYMMDD...) or GeneralizedTime (YYYYMMDD...) value, empty when too short.
function PemTimeToDate(const aTime: String; aASNType: Integer): String;
begin
  Result:='';
  if aASNType=ASN1_GENTIME then
  begin
    if Length(aTime)>=8 then
      Result:=Copy(aTime,1,8);
  end else if Length(aTime)>=6 then
  begin
    // RFC 5280: YY >= 50 is 19YY, otherwise 20YY
    if aTime[1]>='5' then
      Result:='19'+Copy(aTime,1,6)
    else
      Result:='20'+Copy(aTime,1,6);
  end;
end;

function PemParseValidUntil(List: TStrings): String;
var
  Type1, Type2, Size1, Size2: Integer;
  i: Integer;
begin
  Result:='';
  for i := 0 to List.Count-2 do
  begin
    ASNParse_GetItem(List, i, Type1, Size1);
    ASNParse_GetItem(List, i+1, Type2, Size2);
    if (Type1 in [ASN1_UTCTIME, ASN1_GENTIME]) and (Type2 in [ASN1_UTCTIME, ASN1_GENTIME]) then
      Exit(PemTimeToDate(List[i+1], Type2));
  end;
end;

function PemLineIsTag(const Line, Tag: string): boolean;
var
  LineLen, TagLen: SizeInt;
  p: Integer;
  LineP, TagP: PChar;
begin
  Result:=false;
  LineLen:=length(Line);
  TagLen:=length(Tag);
  if LineLen<TagLen then exit;
  LineP:=PChar(Line);
  TagP:=PChar(Tag);
  for p:=0 to LineLen-TagLen do
  begin
    if CompareMem(LineP,TagP,TagLen) then
    begin
      inc(LineP,TagLen);
      while LineP^ in [' ',#9] do inc(LineP);
      exit(LineP^ in [#0,#10,#13]);
    end else if LineP^ in [' ',#9] then
      inc(LineP)
    else
      exit;
  end;
end;

function PemFindTags(List: TStrings; const BeginTag, EndTag: string; out
  StartIndex, EndIndex: integer): boolean;
begin
  Result:=false;
  StartIndex:=0;
  while (StartIndex<List.Count) do
  begin
    if PemLineIsTag(List[StartIndex],BeginTag) then
    begin
      EndIndex:=StartIndex+1;
      while (EndIndex<List.Count) do
        if PemLineIsTag(List[EndIndex],EndTag) then
          exit(true)
        else
        begin
          // A later BeginTag starts a new block
          if PemLineIsTag(List[EndIndex],BeginTag) then
            StartIndex:=EndIndex;
          inc(EndIndex);
        end;
      // No EndTag after this BeginTag
      break;
    end else
      inc(StartIndex);
  end;
  // BeginTag or EndTag missing
  StartIndex:=0;
  EndIndex:=0;
end;

function PemExtractBetween(const PEM, BeginTag, EndTag: String): String;
var
  StartP, EndP: SizeInt;
begin
  Result:='';
  StartP:=Pos(BeginTag,PEM);
  if StartP<1 then exit;
  inc(StartP,length(BeginTag));
  // skip trailing spaces
  while (StartP<=length(PEM)) and (PEM[StartP] in [' ',#9]) do inc(StartP);
  // skip line end and empty lines
  while (StartP<=length(PEM)) and (PEM[StartP] in [#10,#13]) do inc(StartP);
  EndP:=Pos(EndTag,PEM,StartP);
  if EndP<1 then exit;
  // skip leading spaces
  while (EndP>StartP) and (PEM[EndP-1] in [' ',#9]) do dec(EndP);
  Result:=copy(PEM,StartP,EndP-StartP);
end;

function PemLineIsBase64(const Line: string): boolean;
const
  Alphabet = ['a'..'z','A'..'Z','0'..'9','+','/','=', ' ', #9];
var
  i: Integer;
begin
  for i:=1 to length(Line) do
    if not (Line[i] in Alphabet) then
      exit(false);
  Result:=true;
end;

function PemToDER(const PEM, BeginTag, EndTag: String): TBytes;
var
  sl: TStringList;
  Line :  AnsiString;
  TxtBase64: AnsiString;
  StartIndex, EndIndex, i: Integer;
begin
  Result:=[];
  sl:=TStringList.Create;
  try
    sl.Text:=PEM;
    if not PemFindTags(sl,BeginTag,EndTag,StartIndex,EndIndex) then
      exit;
    // todo: encryption
    TxtBase64:='';
    for i:=StartIndex+1 to EndIndex-1 do
    begin
      Line:=sl[i];
      if not PemLineIsBase64(Line) then
        raise EHashUtil.Create('20220428220523');
      TxtBase64:=TxtBase64+Line;
    end;
    Result:={$ifdef FPC_DOTTEDUNITS}Fcl.{$ENDIF}basenenc.Base64.Decode(TxtBase64,True);
  finally
    sl.Free;
  end;
end;

procedure PemToDER(PEM: TBytes; const BeginTag, EndTag: String; out DER: TBytes);
begin
  DER:=PemToDER(GetRawStringFromBytes(PEM),BeginTag,EndTag);
end;

procedure ASNParsePemSection(const PEM: TBytes; List: TStrings; const BeginTag, EndTag: String);
begin
  ASNParsePemSection(GetRawStringFromBytes(PEM),List,BeginTag,EndTag);
end;

procedure ASNParsePemSection(const PEM: String; List: TStrings; const BeginTag, EndTag: String);
var
  DER: TBytes;
begin
  List.Clear;
  DER:=PemToDER(PEM, BeginTag, EndTag);
  ASNParse(DER, List);
end;

end.

