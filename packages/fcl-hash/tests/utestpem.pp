unit utestpem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, basenenc,
  fpTLSBigInt, fppem, fprsa, fphashutils, fpecc;

Type

  { TTestPEM }

  TTestPEM = Class(TTestCase)
  protected
    procedure GetPEM_PrivateKeyRSA2048bit(out PrivatePEM, PublicPEM: string);
  Published
    Procedure TestECC_Load;
    Procedure TestRSA_RS256Verify;
    Procedure TestRSA_PrivatePublicPEM_NoPassphrase;
  end;

implementation

function LinesToText(Lines: array of string): string;
var
  i: Integer;
begin
  Result:='';
  for i:=low(Lines) to High(Lines) do
    Result:=Result+Lines[i]+sLineBreak;
end;

{ TTestPEM }
Const
  PrivateKeyFile = 'private-key.pem';

procedure TTestPEM.GetPEM_PrivateKeyRSA2048bit(out PrivatePEM, PublicPEM: string
  );
begin
  // generated with
  //   openssl genrsa -out private.pem 2048
  PrivatePEM:=LinesToText([
    '-----BEGIN RSA PRIVATE KEY-----',
    'MIIEpQIBAAKCAQEAvkRfGW8psCZ3G4+hBA6W/CR/FHhBLB3k3QLypamPbRFlFBxL',
    'tOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAlTqhtbTWLy/0K3qbG09vLm8V40kEK',
    '8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6K5JGK1VTsMuDCS7DCYk6Vqr3zjX7',
    'qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG/PN2GUQ0oZpa16DDtfgGu7wT3X3Q',
    'EZFWLJYQTvGc82NpachBIUvqNdIt1npbK38MXU4IPHVrSN/HdK2nQPSMLdKnTV+E',
    'h/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg8wIDAQABAoIBAQCZxVwujB7fFFdS',
    '2QPC6Z+w7DYgbwgNBaP/0vAUXzNhbJuKY0v0Rv4H8U9wHGm9EDyvrdG8JHZqPBX+',
    'dJNQ97aPGaRGjO4M0NdGFve+JXcqz6/UDWkywYnV3V1A0NhmdPQK2et3DSjqN7qQ',
    'OoAoVWzR5gf74Zwf2Hpwo3BRdqzFeUYVDOH7e7q1SOf2QeU54kVUG21saJR0wsyH',
    'oSX8BMU2kmg1Un8ET4FM5xEwhdTZzgFTJVZhc6EfOKVbQt6cKmW3aER3c9vR7M3l',
    'N6Oq73vqrfmy+jFMwz1SoPObQQ7UAnr7YUowaX0AzxHpYm/afyVm+Toym0qWGrrY',
    'MY/l+vNRAoGBAOsi72pJj30ApfVbSpx8/8QIpweLbEgAD+Ssd41Kgc4O/N7azB61',
    'RjzSOs1BGhpAZNU6muAAbucm9EssfG5WTAjIM2W2LVuZXXEVXqEGkIymPz9NGugf',
    'JaCWLaoibmwHkKa+ZV9kDwasmx/VkbAfAbRWaz49ejdrMmkpCW77lYjHAoGBAM8m',
    'PVJWvFhQrB21xQGSWKd5iSUn2V92gICeDoORqfVtt/UPOaDT915KzXPh4bJeOwg6',
    'Kkx5wX6UwaNSRH39loDSY1rsBYioV8bxW0BpBvEJG7KXRbBvxzr0+TJkCHgmGMns',
    'dhePYUcriCaqpQi1yzf201oLTZ6PlJxkmHQobXJ1AoGBAIgWPg576InmWCa64WHU',
    'joq8nz8kmFTLhGdK0h56IspJrlyksUKMk8wbuGCW7y6GWlV2h7BhT86Eoxrm8lVB',
    'qNvkUqrpVzMOfiA2x//WNs7QYQaX75ysejCI+oDfUJ1Be5yl0TH2TSQFvfoctycB',
    'qxDee08YcaWlaxWl5InRHeh9AoGABm3XZWDPw6XtUZa8oIncOoZpHUAZXP8eid9d',
    '7/NrZPScyvxH+5fYi5Kiwb/280Q9bMnxWiJFQRp40ArTmV1veFwPPVkp6s3eu4vu',
    'GxenYX+43lgXj5xIgKntugSkxqXYCxxNpfmLOVw+g4S0Torl3bzJXngPVqZ6JEhy',
    '+tfuXakCgYEA19/JCD/5pVPJtwyDDAYnUUESK+JfBPq1cTbsxcOq01mp5ntsqR4y',
    'dtOAmxMASvsqud3XIM5fO5m3Jpl1phiGhCw4nvVLcYzVWxYY+oWoeCSyECgu5tmT',
    'Fo8vn4EEXCkEAA2YPiEuVcrcYsWkLivCTC19lJDfUNMmpwSdiGz/tDU=',
    '-----END RSA PRIVATE KEY-----' ]);

  // public key extracted with
  //   openssl rsa -in private.pem -outform PEM -pubout -out public.pem
  PublicPEM:=LinesToText([
    '-----BEGIN PUBLIC KEY-----',
    'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvkRfGW8psCZ3G4+hBA6W',
    '/CR/FHhBLB3k3QLypamPbRFlFBxLtOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAl',
    'TqhtbTWLy/0K3qbG09vLm8V40kEK8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6',
    'K5JGK1VTsMuDCS7DCYk6Vqr3zjX7qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG',
    '/PN2GUQ0oZpa16DDtfgGu7wT3X3QEZFWLJYQTvGc82NpachBIUvqNdIt1npbK38M',
    'XU4IPHVrSN/HdK2nQPSMLdKnTV+Eh/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg',
    '8wIDAQAB',
    '-----END PUBLIC KEY-----' ]);
end;

procedure TTestPEM.TestECC_Load;

Const
  // Hex encoded keys, Obtained using XMLRAD
  // Private key matched openssl
  // openssl ec -text -in private-key.pem -noout
  resprivatekey = '8D8DC61A006BF4E8C508ACFDEB331E385E763B6C4CD52B918C438D5B1804A5A9';
  respublickey  = '03675CA1BD5D466F5EE30CEC92FDECCB0834A6467E7013CAF05C7B68648F7DB93C';
  ResX = 'Z1yhvV1Gb17jDOyS_ezLCDSmRn5wE8rwXHtoZI99uTw';
  ResY = 'kBzplHWhOlp71lYjOaoK6Bfo9crVT3mbCEAWqK-1FVs';
  // Obtained through OpenSSL: public key as X&Y (DER 04 prefix removed)
  ResXY = {04}
          '675ca1bd5d466f5ee30cec92fdec'+
          'cb0834a6467e7013caf05c7b68648f'+
          '7db93c901ce99475a13a5a7bd65623'+
          '39aa0ae817e8f5cad54f799b084016'+
          'a8afb5155b';

var
  List: TStrings;
  PrivateKey  : TEccPrivateKey;
  PublicKey : TEccPublicKey;
  XHex,YHex,PublicKeyX64, PublicKeyY64 : Ansistring;
  Res : Boolean;

begin
  List := TStringList.Create;
  try
    Res:=FileExists(PrivateKeyFile) and PemIsECDSA(PrivateKeyFile, List);
    if Res then
      PemLoadPublicKey64FromList(List, PrivateKey, PublicKey, PublicKeyX64, PublicKeyY64);
    AssertEquals('Private key',resprivatekey,BytesToHexStr(BytesFromVar(@PrivateKey,Sizeof(PrivateKey))));
    AssertEquals('Public  key',respublickey,BytesToHexStr(BytesFromVar(@PublicKey,Sizeof(PublicKey))));
    AssertEquals('X',resX,PublicKeyX64);
    AssertEquals('Y',resY,PublicKeyY64);
    XHex:=base16.Encode(base64url.Decode(PublicKeyX64),False);
    YHex:=base16.Encode(base64url.Decode(PublicKeyY64),False);
    AssertEquals('Public as X,Y',ResXY,LowerCase(XHex+YHex));

//    Writeln('X ', PublicKeyX64,' -> ',XHex);
//    Writeln('Y ', PublicKeyY64,' -> ',YHex);
  finally
    List.Free;
  end;
end;

procedure TTestPEM.TestRSA_RS256Verify;
begin
  if not fprsa.TestRS256Verify then
    Fail('TestRS256Verify');
end;

procedure TTestPEM.TestRSA_PrivatePublicPEM_NoPassphrase;
const
  SecretMsg = 'FreePascal RSA Test';
var
  PrivatePEM, PublicPEM, Original, Encrypted, Decrypted: string;
  DER: TBytes;
  PrivateRSA, PublicRSA: TRSA;
  EncryptedLen, DecryptedLen: Integer;
begin
  GetPEM_PrivateKeyRSA2048bit(PrivatePEM, PublicPEM);

  RSACreate(PublicRSA);
  RSACreate(PrivateRSA);
  try
    // load public key
    DER:=PemToDER(PublicPEM,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY);
    if length(DER)=0 then
      Fail('PemToDER public key');
    RSAInitFromPublicKeyDER(PublicRSA,DER);

    // load private key
    DER:=PemToDER(PrivatePEM,_BEGIN_RSA_PRIVATE_KEY,_END_RSA_PRIVATE_KEY);
    if length(DER)=0 then
      Fail('PemToDER private rsa key');
    RSAInitFromPrivateKeyDER(PrivateRSA,DER);

    AssertEquals('PublicRSA.ModulusLen=PrivateRSA.ModulusLen',PublicRSA.ModulusLen,PrivateRSA.ModulusLen);
    if BICompare(PublicRSA.M,PrivateRSA.M)<>0 then
      Fail('PublicRSA.M = PrivateRSA.M');
    if BICompare(PublicRSA.E,PrivateRSA.E)<>0 then
      Fail('PublicRSA.E = PrivateRSA.E');

    Original:=SecretMsg;

    // encrypt with public key
    SetLength(Encrypted{%H-},PublicRSA.ModulusLen);
    EncryptedLen:=RSAEncryptSign(PublicRSA,@Original[1],length(Original),@Encrypted[1],false);
    if EncryptedLen<PublicRSA.ModulusLen then
      AssertEquals('EncryptedLen = ModulusLen',EncryptedLen,PublicRSA.ModulusLen);

    // decrypt with private key
    SetLength(Decrypted{%H-},EncryptedLen);
    DecryptedLen:=RSADecryptVerify(PrivateRSA,@Encrypted[1],@Decrypted[1],EncryptedLen,false);

    if DecryptedLen<>length(Original) then
      AssertEquals('DecryptedLen = length(Original)',DecryptedLen,length(Original));
    SetLength(Decrypted,DecryptedLen);
    if Decrypted<>Original then
      Fail('Decrypted = Original');

  finally
    RSAFree(PublicRSA);
    RSAFree(PrivateRSA);
  end;
end;

initialization
  RegisterTest(TTestPEM);
end.

