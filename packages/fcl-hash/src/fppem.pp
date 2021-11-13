unit fppem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpecc;

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


Function PemIsECDSA(const aStream : TStream; List: TStrings): Boolean;
Function PemIsECDSA(const FileName: String; List: TStrings): Boolean;

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString);
procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString);

Function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;
Function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString): Boolean;
Function PemLoadECDSA(const aStream : TStream; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;
Function PemLoadECDSA(const aStream : TStream; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString): Boolean;


implementation

uses basenenc, fpsha256, fpasn, fphashutils;

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString);

Var
  S : String;

begin
  PemLoadPublicKey64FromList(List,PrivateKey,PublicKey,PublicKeyX64,PublicKeyY64);
  // Thumbprint
  S:='{"crv":"P-256","kty":"EC","x":"' + PublicKeyX64 + '","y":"' + PublicKeyY64 + '"}';
  TSHA256.DigestBase64(TEncoding.UTF8.GetAnsiBytes(S),True,ThumbPrint);
end;

procedure PemLoadPublicKey64FromList(List: TStrings; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64: AnsiString);

var
  SPrivateKeyHexa, SPublicKeyHexa: String;
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


Function PemLoadECDSA(const FileName: String; Out PrivateKey: TEccPrivateKey; Out PublicKey: TEccPublicKey; Out PublicKeyX64, PublicKeyY64: AnsiString) : Boolean;

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


Function PemLoadECDSA(const FileName: String; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;

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
  CurveOID := List.Strings[4];
  Result := (CurveOID=ASN_secp256r1);
end;

Function PemLoadECDSA(const aStream : TStream; out PrivateKey: TEccPrivateKey; out PublicKey: TEccPublicKey; out PublicKeyX64, PublicKeyY64, ThumbPrint: AnsiString) : Boolean;

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

Function PemLoadECDSA(const aStream: Tstream; Out PrivateKey: TEccPrivateKey; Out PublicKey: TEccPublicKey; Out PublicKeyX64, PublicKeyY64: AnsiString) : Boolean;

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


end.

