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
function PemParseValidUntil(List: TStrings): String;

function PemLineIsTag(const Line, Tag: string): boolean; // true if Line contains Tag, ignoring leading and trailing spaces
function PemLineIsBase64(const Line: string): boolean; // true if Line contains only spaces and base64 chars
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

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString);

var
  SPrivateKeyHexa, SPublicKeyHexa: AnsiString;
  DecompressedPublicKey, SPrivateKey : TBytes;

begin
  SPrivateKeyHexa := List.Strings[2];
  HexStrToBytes(SPrivateKeyHexa, SPrivateKey);
  if Length(SPrivateKey)>=SizeOf(PrivateKey) then
    BytesToVar(SPrivateKey,Privatekey,SizeOf(Privatekey))
  else
    Raise Exception.CreateFmt('Wrong private key length, got %d, expected %d',[Length(SPrivateKey),SizeOf(TPrivateKey)]);
  SPublicKeyHexa := List.Strings[6]; // 132 chars
  if Length(SPublicKeyHexa) = 2+2*(2*ECC_BYTES) then
    Delete(SPublicKeyHexa,1,2); // skip the leading $04 for uncompressed publickey
  DecompressedPublicKey:=HexStrToBytes(SPublicKeyHexa);
  EccPublicKeyFromHexa(PublicKey, SPublicKeyHexa);
  PublicKeyX64:=base64URL.Encode(Copy(DecompressedPublicKey,0,32),False);
  PublicKeyY64:=base64URL.Encode(Copy(DecompressedPublicKey, 32, 32),False);
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
  if List.Count < 7 then
    Exit;
//  Writeln(List.Text);
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

function PemParseValidUntil(List: TStrings): String;
var
  ASNType: LongInt;
  S: String;
  i: Integer;
begin
  for i := 0 to List.Count-2 do
  begin
    ASNType := StrToIntDef(List[i],0) shr 16;
    if ASNType <> ASN1_UTCTIME then
      Continue;
    S := List.Strings[i+1];
    ASNType := StrToIntDef(S,0) shr 16;
    if ASNType <> ASN1_UTCTIME then
      Continue;
    if (S.Length > 6) and (S[length(s)] = 'Z') then
      SetLength(S,6); // 181231
    Result:='20'+S; // 20181231
    Break;
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
      Result:=LineP^ in [#0,#10,#13];
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
          inc(EndIndex);
    end else
      inc(StartIndex);
  end;
  // BeginTag missing
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
        raise Exception.Create('20220428220523');
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

