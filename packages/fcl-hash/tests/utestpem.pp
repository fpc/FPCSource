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
    procedure GetPEM_PrivateKeyRSA2048bit(out PrivatePEM, PublicPEM: Ansistring);
    // PKCS#1 v1.5 signature of aData, used as is as the DigestInfo, with the 512-bit test key
    function SignWithSmallKey(const aData: TBytes): AnsiString;
  Published
    Procedure TestECC_Load;
    Procedure TestRSA_RS256Verify;
    Procedure TestRSA_PrivatePublicPEM_NoPassphrase;
    Procedure TestRSA_SignCRTPLessThanQ;
    Procedure TestRSA_VerifyDigestInfo;
    Procedure TestRSA_VerifyRejectsE3Forgery;
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

procedure TTestPEM.GetPEM_PrivateKeyRSA2048bit(out PrivatePEM, PublicPEM: Ansistring
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
    PrivateKey:=Default(TEccPrivateKey);
    PublicKey:=Default(TEccPublicKey);
    Res:=FileExists(PrivateKeyFile) and PemIsECDSA(PrivateKeyFile, List);
    AssertTrue('Have file with Key',Res);
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
  PrivatePEM, PublicPEM, Original, Encrypted, Decrypted: Ansistring;
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

const
  // 512-bit key with p < q
  SmallKeyN = 'E00B841F439034CF534B24D368201E765357E140C626AAD330337F8F709F586B3E1F59C766B8B65365A233641E2E42E826E206B970E9C9539D2020DE642D3A5B';
  SmallKeyE = '010001';
  SmallKeyD = 'BA0275A82B44EFC36DC906830A8118296F1A98265A1D4258623D6F5B9CCA7EF15DFEC4DC44D1EBD7B4CD6E52004A7EA4A52DD2C159AE793B0FE65A7788C1CDE9';
  SmallKeyP = '764DDABF9C746153C5444912D29CE4CB78B57FA0364C947423A8CA31DCD4F2A5';
  SmallKeyQ = '01E4D06EFEEFDA8D7A70DFE8426CBB068037FE47501FC124A974E07CC2C962E8FF';
  SmallKeyDP = '564590300B381381B36988A0615668C2B8D422AE02EAF5977C29AAC991F0F8C1';
  SmallKeyDQ = '9C14CA9742CAFEDF5B758A85D0118A2F3F579AC7DDE1459754AC6D1E1CCDBE2B';
  SmallKeyQI = '224C778FE22D9A52A77769AFA3EF080E672BECBD687CAA4A6D81CE2A7A99F33D';

function TTestPEM.SignWithSmallKey(const aData: TBytes): AnsiString;
var
  Key: TX509RSAPrivateKey;
  PrivateRSA: TRSA;
begin
  Key.InitWithHexStrings(SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI);
  RSACreate(PrivateRSA);
  try
    RSAInitFromX509PrivateKey(PrivateRSA,Key);
    SetLength(Result{%H-},PrivateRSA.ModulusLen);
    AssertEquals('Signature length',PrivateRSA.ModulusLen,
                 RSAEncryptSign(PrivateRSA,PByte(aData),Length(aData),PByte(Result),True));
  finally
    RSAFree(PrivateRSA);
  end;
end;

procedure TTestPEM.TestRSA_SignCRTPLessThanQ;
const
  PayloadLen = 20;
var
  Key: TX509RSAPrivateKey;
  PrivateRSA, PublicRSA: TRSA;
  Payload, Signature, Verified: TBytes;
  I, J, VerifiedLen: Integer;
begin
  Key.InitWithHexStrings(SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI);
  RSACreate(PrivateRSA);
  RSACreate(PublicRSA);
  try
    RSAInitFromX509PrivateKey(PrivateRSA,Key);
    RsaInitFromPublicKey(PublicRSA,HexStrToString(SmallKeyN),HexStrToString(SmallKeyE));
    SetLength(Payload,PayloadLen);
    SetLength(Signature,PrivateRSA.ModulusLen);
    SetLength(Verified,PrivateRSA.ModulusLen);
    for I:=1 to 50 do
      begin
      for J:=0 to PayloadLen-1 do
        Payload[J]:=(I*31+J*7) and $FF;
      AssertEquals('Signature '+IntToStr(I)+' length',PrivateRSA.ModulusLen,
                   RSAEncryptSign(PrivateRSA,@Payload[0],PayloadLen,@Signature[0],True));
      VerifiedLen:=RSADecryptVerify(PublicRSA,@Signature[0],@Verified[0],Length(Verified),True);
      AssertEquals('Signature '+IntToStr(I)+' verifies with the public key',PayloadLen,VerifiedLen);
      AssertTrue('Signature '+IntToStr(I)+' recovers the payload',CompareMem(@Verified[0],@Payload[0],PayloadLen));
      end;
  finally
    RSAFree(PrivateRSA);
    RSAFree(PublicRSA);
  end;
end;

procedure TTestPEM.TestRSA_VerifyDigestInfo;
var
  Hash: AnsiString;
  HashBytes, DigestInfo, Data: TBytes;
  I: Integer;

  function Verify(const aData: TBytes): Boolean;
  begin
    Result:=RS256VerifyFromPublicKeyHexa(SmallKeyE+SmallKeyN,Hash,SignWithSmallKey(aData));
  end;

begin
  SetLength(Hash{%H-},32);
  for I:=1 to 32 do
    Hash[I]:=AnsiChar(I*5);
  HashBytes:=BytesFromVar(@Hash[1],32);
  DigestInfo:=EncodeDigestInfoSHA(RSADigestInfoSHA256,32);

  AssertTrue('SHA-256 DigestInfo with NULL parameters',Verify(Concat(DigestInfo,HashBytes)));
  AssertTrue('SHA-256 DigestInfo without parameters',
             Verify(Concat(TBytes([$30,47,$30,11,$06,9,$60,$86,$48,1,101,3,4,2,1,$04,32]),HashBytes)));
  AssertFalse('Byte after the hash',Verify(Concat(DigestInfo,HashBytes,TBytes([0]))));
  AssertFalse('OID of a hash with another length',Verify(Concat(EncodeDigestInfoSHA(RSADigestInfoSHA384,32),HashBytes)));
  AssertFalse('Unknown OID',Verify(Concat(EncodeDigestInfoSHA(11,32),HashBytes)));
  AssertFalse('Hash too short',Verify(Copy(Concat(DigestInfo,HashBytes),0,50)));
  Data:=Concat(DigestInfo,HashBytes);
  Data[Length(Data)-1]:=Data[Length(Data)-1] xor 1;
  AssertFalse('Other hash',Verify(Data));
end;

procedure TTestPEM.TestRSA_VerifyRejectsE3Forgery;
const
  // e=3 public key, hash and a signature forged without the private key
  ForgeKeyHexa = '000003ACC5D7CA65F7D22FEFAD479268BCC4A614117EDEABE129360B099C5414FB6965E116993B8C3EF065996CC05D4E25F7078C7F8D166A8C6046E55E161C05'
                 +'0BC1955538C06202CF7E0ACD6CDBFB51E80147AB878C107E1556570D03DC6205AB80A7D549C47032C0A390F86A51C80F89EB200B8F3892C2337F9755AB934F6B'
                 +'0833340CCC96FF6011AE7C0AB6D9416CFF14CC3EEE977879825E36699EECE1F9BEC9D803A8122EF5794DAE009D1257DF158903DDCE0EC6CCE9090180C49A590C'
                 +'8C491868247A837012128E8A588AE705D62FEB4FBC5C7F036C73BB3BADC0EE38729B31C2FB6847CB86F12A728E0D6CA94B62B1CB1F1161A30335ADAF866CA95B'
                 +'EBD523';
  ForgeHash = 'E457C93DCCD27F3B2142F65AFE309B5262D1AF6D7C6AE9139AB6820C277909A8';
  ForgeSignature = '00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
                   +'00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
                   +'0000000000000000000000000000000000000000000000000000000000000000000000000000000000000032CBFD4A7ADC7905583D767520F51640759176D378'
                   +'26F2EF63AE3DC7ADAB162566D64AD984AE82F08CBE5ADB31FDC3FDE3A3E1351D3963296BBA64599EAD8865392D2F04F74EBA434887CFB984C821896FFCDF3609';
begin
  AssertFalse('Forged signature rejected',
              RS256VerifyFromPublicKeyHexa(ForgeKeyHexa,HexStrToString(ForgeHash),HexStrToString(ForgeSignature)));
end;

initialization
  RegisterTest(TTestPEM);
end.

