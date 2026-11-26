unit utestpem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, basenenc,
  fpTLSBigInt, fppem, fprsa, fphashutils, fpecc, fpasn;

Type

  { TTestPEM }

  TTestPEM = Class(TTestCase)
  protected
    procedure GetPEM_PrivateKeyRSA2048bit(out PrivatePEM, PublicPEM: Ansistring);
    // PKCS#1 v1.5 signature of aData, used as is as the DigestInfo, with the 512-bit test key
    function SignWithSmallKey(const aData: TBytes): AnsiString;
    // Sign and verify 20 SHA-256 PSS signatures with salt length aSaltLen for the given key
    procedure CheckPSSSignVerify(const aName, aN, aE, aD, aP, aQ, aDP, aDQ, aQI: String; aSaltLen: Integer);
  Published
    Procedure TestECC_Load;
    Procedure TestRSA_RS256Verify;
    Procedure TestRSA_PrivatePublicPEM_NoPassphrase;
    Procedure TestRSA_SignCRTPLessThanQ;
    Procedure TestRSA_VerifyDigestInfo;
    Procedure TestRSA_VerifyRejectsE3Forgery;
    Procedure TestPemFindTags;
    Procedure TestRSA_DecryptPadding;
    Procedure TestRSA_PSSModulusSizes;
    Procedure TestRSA_PSSNoPadding;
    Procedure TestRSA_PrivateKeyNoRandomBytes;
    Procedure TestRSA_BlindedSignatureDeterministic;
    Procedure TestPemParseValidUntil;
    Procedure TestECC_LoadPublicKeyForms;
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

procedure TTestPEM.TestRSA_DecryptPadding;
const
  BlockSize = 64;
var
  Key: TX509RSAPrivateKey;
  PrivateRSA, PublicRSA: TRSA;

  // 00 02, aPadding bytes $11, 00 if there is room, then message bytes (I*3+1) up to BlockSize
  function MakeBlock(aPadding: Integer): TBytes;
  var
    I: Integer;
  begin
    Result:=[];
    SetLength(Result,BlockSize);
    Result[0]:=0;
    Result[1]:=2;
    for I:=2 to 1+aPadding do
      Result[I]:=$11;
    if 2+aPadding<BlockSize then
      Result[2+aPadding]:=0;
    for I:=3+aPadding to BlockSize-1 do
      Result[I]:=(I*3+1) and $FF;
  end;

  // Decrypted length of the raw RSA encryption of aBlock; aMessage receives the decrypted bytes
  function Decrypt(const aBlock: TBytes; out aMessage: TBytes): Integer;
  var
    Cipher: TBytes;
  begin
    SetLength(Cipher,BlockSize);
    PublicRSA.Context.ModOffset:=BIGINT_M_OFFSET;
    BIExport(PublicRSA.Context,BIModPower(PublicRSA.Context,BIImport(PublicRSA.Context,aBlock),PublicRSA.E),@Cipher[0],BlockSize);
    SetLength(aMessage,BlockSize);
    Result:=RSADecryptVerify(PrivateRSA,@Cipher[0],@aMessage[0],BlockSize,False);
    if Result>=0 then
      SetLength(aMessage,Result);
  end;

var
  Block, Msg: TBytes;
begin
  Key.InitWithHexStrings(SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI);
  RSACreate(PrivateRSA);
  RSACreate(PublicRSA);
  try
    RSAInitFromX509PrivateKey(PrivateRSA,Key);
    RsaInitFromPublicKey(PublicRSA,HexStrToString(SmallKeyN),HexStrToString(SmallKeyE));

    Block:=MakeBlock(10);
    AssertEquals('10 padding bytes',BlockSize-13,Decrypt(Block,Msg));
    AssertTrue('10 padding bytes: message',CompareMem(@Msg[0],@Block[13],Length(Msg)));

    Block:=MakeBlock(8);
    AssertEquals('8 padding bytes',BlockSize-11,Decrypt(Block,Msg));
    AssertTrue('8 padding bytes: message',CompareMem(@Msg[0],@Block[11],Length(Msg)));

    Block:=MakeBlock(8);
    Block[12]:=0;
    Block[14]:=0;
    AssertEquals('Zero bytes inside the message',BlockSize-11,Decrypt(Block,Msg));
    AssertTrue('Zero bytes inside the message: message',CompareMem(@Msg[0],@Block[11],Length(Msg)));

    AssertEquals('Separator in the last byte gives an empty message',0,Decrypt(MakeBlock(BlockSize-3),Msg));

    AssertEquals('7 padding bytes',-1,Decrypt(MakeBlock(7),Msg));
    AssertEquals('No separator',-1,Decrypt(MakeBlock(BlockSize-2),Msg));
    Block:=MakeBlock(10);
    Block[1]:=1;
    AssertEquals('Block type 1',-1,Decrypt(Block,Msg));
    Block:=MakeBlock(10);
    Block[0]:=1;
    AssertEquals('First byte not zero',-1,Decrypt(Block,Msg));
  finally
    RSAFree(PrivateRSA);
    RSAFree(PublicRSA);
  end;
end;

const
  // 529-bit key: emBits 528 is a multiple of 8, emLen 66 = 2*32+2
  Key529N = '0161BAAF8F719C03EABB548C95A81548D36D861649B90C2F4F3AF62E1AA9774525DD6221C7FBC537C80068C4AF70F32A870425D1596F7C03B5897180334EF2A6FEB349';
  Key529E = '010001';
  Key529D = '1F3EDB552366F19DA170857E8EBB5A947A7F36A6CA76BE1314953C5B548ED987CF7FF4E105201A1412222D0C599D44DE23EC23B874E073EEBEB426AB30EB8509C835';
  Key529P = 'E0D9E7AC2B01587A4D177F2EBDBE675420F590637BB82C849C31961C5713F8477B';
  Key529Q = '0192BB4BD05EF2483EDB1ACA84E978E2C25BD07B7FE916488F40874A90B97858930B';
  Key529DP = '5231B5CBAD08D7E4154C5294AE8DECCDC7229D3E909A59C3BC29303B79E575EC03';
  Key529DQ = '2AA5A2D5A1F84E2639D590749AB4AE6E9E826951964141D5C9A38A05E15E192319';
  Key529QI = 'A141405E80DFDE8211F36831DF3F12ADA04573335A49CD7FE88F0984C46FCD0642';

procedure TTestPEM.CheckPSSSignVerify(const aName, aN, aE, aD, aP, aQ, aDP, aDQ, aQI: String; aSaltLen: Integer);
var
  Key: TX509RSAPrivateKey;
  PrivateRSA, PublicRSA: TRSA;
  HashFunc: TRSAHashFuncInfo;
  Msg: AnsiString;
  Signature: TBytes;
  I: Integer;
begin
  HashFunc.UseSHA256;
  Key.InitWithHexStrings(aN,aE,aD,aP,aQ,aDP,aDQ,aQI);
  RSACreate(PrivateRSA);
  RSACreate(PublicRSA);
  try
    RSAInitFromX509PrivateKey(PrivateRSA,Key);
    RsaInitFromPublicKey(PublicRSA,HexStrToString(aN),HexStrToString(aE));
    Signature:=[];
    SetLength(Signature,PrivateRSA.ModulusLen);
    for I:=1 to 20 do
      begin
      Msg:='PSS message '+IntToStr(I);
      AssertEquals(aName+' signature '+IntToStr(I),PrivateRSA.ModulusLen,
                   RSASSA_PSS_Sign(PrivateRSA,PByte(Msg),Length(Msg),@HashFunc,@Signature[0],aSaltLen));
      AssertEquals(aName+' signature '+IntToStr(I)+' verifies with its salt length',0,
                   RSASSA_PSS_Verify(PublicRSA,PByte(Msg),Length(Msg),@HashFunc,@Signature[0],aSaltLen));
      AssertEquals(aName+' signature '+IntToStr(I)+' verifies with automatic salt length',0,
                   RSASSA_PSS_Verify(PublicRSA,PByte(Msg),Length(Msg),@HashFunc,@Signature[0],RSA_PSS_SaltLen_Auto));
      Msg[1]:='p';
      AssertTrue(aName+' signature '+IntToStr(I)+' does not verify another message',
                 RSASSA_PSS_Verify(PublicRSA,PByte(Msg),Length(Msg),@HashFunc,@Signature[0],aSaltLen)<>0);
      end;
  finally
    RSAFree(PrivateRSA);
    RSAFree(PublicRSA);
  end;
end;

procedure TTestPEM.TestRSA_PSSModulusSizes;
begin
  CheckPSSSignVerify('529-bit key',Key529N,Key529E,Key529D,Key529P,Key529Q,Key529DP,Key529DQ,Key529QI,16);
  CheckPSSSignVerify('512-bit key',SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI,16);
end;

procedure TTestPEM.TestRSA_PSSNoPadding;
begin
  CheckPSSSignVerify('529-bit key, salt of hash length',Key529N,Key529E,Key529D,Key529P,Key529Q,Key529DP,Key529DQ,Key529QI,
                     RSA_PSS_SaltLen_HashLen);
  CheckPSSSignVerify('529-bit key, maximum salt',Key529N,Key529E,Key529D,Key529P,Key529Q,Key529DP,Key529DQ,Key529QI,
                     RSA_PSS_SaltLen_Max);
  CheckPSSSignVerify('512-bit key, maximum salt',SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI,
                     RSA_PSS_SaltLen_Max);
end;

// Random source that always fails
function FailingRandomBytes(aBytes: PByte; aCount: Integer): Boolean;
begin
  Result:=False;
end;

procedure TTestPEM.TestRSA_PrivateKeyNoRandomBytes;
var
  Key: TX509RSAPrivateKey;
  PrivateRSA, PublicRSA: TRSA;
  HashFunc: TRSAHashFuncInfo;
  Saved: TGetRandomBytes;
  Payload, Cipher, Output: TBytes;
  Raised: Boolean;
begin
  HashFunc.UseSHA256;
  Key.InitWithHexStrings(SmallKeyN,SmallKeyE,SmallKeyD,SmallKeyP,SmallKeyQ,SmallKeyDP,SmallKeyDQ,SmallKeyQI);
  RSACreate(PrivateRSA);
  RSACreate(PublicRSA);
  Saved:=GetRandomBytes;
  try
    RSAInitFromX509PrivateKey(PrivateRSA,Key);
    RsaInitFromPublicKey(PublicRSA,HexStrToString(SmallKeyN),HexStrToString(SmallKeyE));
    Payload:=TBytes([1,2,3,4,5]);
    SetLength(Cipher,PrivateRSA.ModulusLen);
    SetLength(Output,PrivateRSA.ModulusLen);
    AssertEquals('Encrypted',PrivateRSA.ModulusLen,RSAEncryptSign(PublicRSA,@Payload[0],Length(Payload),@Cipher[0],False));
    GetRandomBytes:=@FailingRandomBytes;
    AssertEquals('No signature without random bytes',-1,RSAEncryptSign(PrivateRSA,@Payload[0],Length(Payload),@Output[0],True));
    AssertEquals('No decryption without random bytes',-1,RSADecryptVerify(PrivateRSA,@Cipher[0],@Output[0],Length(Output),False));
    Raised:=False;
    try
      RSASSA_PSS_Sign(PrivateRSA,@Payload[0],Length(Payload),@HashFunc,@Output[0],16);
    except
      on EHashUtil do
        Raised:=True;
    end;
    AssertTrue('No PSS signature without random bytes',Raised);
    GetRandomBytes:=Saved;
    AssertEquals('Decrypted with random bytes',Length(Payload),RSADecryptVerify(PrivateRSA,@Cipher[0],@Output[0],Length(Output),False));
    AssertTrue('Decrypted payload',CompareMem(@Output[0],@Payload[0],Length(Payload)));
  finally
    GetRandomBytes:=Saved;
    RSAFree(PrivateRSA);
    RSAFree(PublicRSA);
  end;
end;

procedure TTestPEM.TestRSA_BlindedSignatureDeterministic;
var
  Payload: TBytes;
  First: AnsiString;
  I: Integer;
begin
  Payload:=TBytes([9,8,7,6,5,4,3,2,1]);
  First:=SignWithSmallKey(Payload);
  for I:=1 to 10 do
    AssertEquals('PKCS#1 v1.5 signature '+IntToStr(I)+' equals the first one',BytesToHexStr(First),BytesToHexStr(SignWithSmallKey(Payload)));
end;

procedure TTestPEM.TestPemParseValidUntil;
const
  // Self-signed EC certificates made with OpenSSL; notAfter 2036-11-23 (UTCTime) and 2054-04-13 (GeneralizedTime)
  Cert2036 =
    '-----BEGIN CERTIFICATE-----'+sLineBreak+
    'MIIBbTCCAROgAwIBAgIUPV5OcacRKn/CGGKUhGBrLGrzeJswCgYIKoZIzj0EAwIw'+sLineBreak+
    'DDEKMAgGA1UEAwwBdDAeFw0yNjExMjYxODA4NDFaFw0zNjExMjMxODA4NDFaMAwx'+sLineBreak+
    'CjAIBgNVBAMMAXQwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAATRHwIhq2rJGX3B'+sLineBreak+
    'lNqHLU9ex8BHC4/41doOJo0akDxPqJZLYlANysZTjqXTR2CZIljvltwkiAzdYvCO'+sLineBreak+
    'cUcyB4w+o1MwUTAdBgNVHQ4EFgQUG42d60yx2z2P3FsJK/KBTidIQgswHwYDVR0j'+sLineBreak+
    'BBgwFoAUG42d60yx2z2P3FsJK/KBTidIQgswDwYDVR0TAQH/BAUwAwEB/zAKBggq'+sLineBreak+
    'hkjOPQQDAgNIADBFAiBMwsewNEKYrht/adLnvj3kxqmuSJAmJZ+tu4q3TSmtwgIh'+sLineBreak+
    'AJQH/4O+0oT9TA+l53nDFCJzffnjhmcftWTAgwHBuQxB'+sLineBreak+
    '-----END CERTIFICATE-----';
  Cert2054 =
    '-----BEGIN CERTIFICATE-----'+sLineBreak+
    'MIIBbzCCARWgAwIBAgIUGFoHIF3euzcokkfzTo2V6OSScOUwCgYIKoZIzj0EAwIw'+sLineBreak+
    'DDEKMAgGA1UEAwwBdDAgFw0yNjExMjYxODA4NDFaGA8yMDU0MDQxMzE4MDg0MVow'+sLineBreak+
    'DDEKMAgGA1UEAwwBdDBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABB8fz7M4jK9H'+sLineBreak+
    'zokWqgCxcxTtxiGusE7EaFje16YpYLtIeiu3a2lA3hc41L5piKpbGo53sCPR/A5j'+sLineBreak+
    'HzpEm1iVJXajUzBRMB0GA1UdDgQWBBTNSBMANS7Ej4Dr7uxVVjQSf3CSeDAfBgNV'+sLineBreak+
    'HSMEGDAWgBTNSBMANS7Ej4Dr7uxVVjQSf3CSeDAPBgNVHRMBAf8EBTADAQH/MAoG'+sLineBreak+
    'CCqGSM49BAMCA0gAMEUCIQDnZYCRjiOnlVHlfEAlILdUAdgSWcH9hGdEYaj8uTSs'+sLineBreak+
    '2gIgWNCP4XIaFlMAORC95WGsWXeZAeb+W4QjBwlnaQoiioo='+sLineBreak+
    '-----END CERTIFICATE-----';
var
  List: TStringList;

  // Validity sequence with the two given UTCTime values
  procedure ParseValidity(const aNotBefore, aNotAfter: AnsiString);
  var
    Data: TBytes;
  begin
    Data:=Concat(TBytes([$30,2+Length(aNotBefore)+2+Length(aNotAfter),$17,Length(aNotBefore)]),
                 BytesFromVar(@aNotBefore[1],Length(aNotBefore)),
                 TBytes([$17,Length(aNotAfter)]),BytesFromVar(@aNotAfter[1],Length(aNotAfter)));
    List.Clear;
    ASNParse(Data,List);
  end;

begin
  List:=TStringList.Create;
  try
    ASNParsePemSection(Cert2036,List,_BEGIN_CERTIFICATE,_END_CERTIFICATE);
    AssertEquals('UTCTime notAfter','20361123',PemParseValidUntil(List));
    List.Clear;
    ASNParsePemSection(Cert2054,List,_BEGIN_CERTIFICATE,_END_CERTIFICATE);
    AssertEquals('GeneralizedTime notAfter','20540413',PemParseValidUntil(List));
    ParseValidity('990131123456Z','491231235959Z');
    AssertEquals('UTCTime year 49','20491231',PemParseValidUntil(List));
    ParseValidity('400131123456Z','500101000000Z');
    AssertEquals('UTCTime year 50','19500101',PemParseValidUntil(List));
    List.Clear;
    ASNParse(TBytes([$02,$01,$05]),List);
    AssertEquals('No time values','',PemParseValidUntil(List));
  finally
    List.Free;
  end;
end;

procedure TTestPEM.TestECC_LoadPublicKeyForms;
const
  // One P-256 key made with OpenSSL: uncompressed, -conv_form compressed and -no_public
  KeyUncompressed =
    '-----BEGIN EC PRIVATE KEY-----'+sLineBreak+
    'MHcCAQEEIJ6O/ZNuzzd6eq5NUjoxile7fx2goOeUnnyTH0io9MUloAoGCCqGSM49'+sLineBreak+
    'AwEHoUQDQgAEYDpmnlYZO8I1RRgXsGoXVCZrebufbrzINIYLAUdNHnP8BijXHFbU'+sLineBreak+
    'W3HaDlXk+V+fMdfl7WFLlPzOLFL+JA6Xcw=='+sLineBreak+
    '-----END EC PRIVATE KEY-----';
  KeyCompressed =
    '-----BEGIN EC PRIVATE KEY-----'+sLineBreak+
    'MFcCAQEEIJ6O/ZNuzzd6eq5NUjoxile7fx2goOeUnnyTH0io9MUloAoGCCqGSM49'+sLineBreak+
    'AwEHoSQDIgADYDpmnlYZO8I1RRgXsGoXVCZrebufbrzINIYLAUdNHnM='+sLineBreak+
    '-----END EC PRIVATE KEY-----';
  KeyNoPublic =
    '-----BEGIN EC PRIVATE KEY-----'+sLineBreak+
    'MDECAQEEIJ6O/ZNuzzd6eq5NUjoxile7fx2goOeUnnyTH0io9MUloAoGCCqGSM49'+sLineBreak+
    'AwEH'+sLineBreak+
    '-----END EC PRIVATE KEY-----';
  PointX = '603A669E56193BC235451817B06A1754266B79BB9F6EBCC834860B01474D1E73';
  PointY = 'FC0628D71C56D45B71DA0E55E4F95F9F31D7E5ED614B94FCCE2C52FE240E9773';

type
  TLoaded = record
    Ok: Boolean;
    PrivateKey: TEccPrivateKey;
    PublicKey: TEccPublicKey;
    X64, Y64, ThumbPrint: AnsiString;
  end;

  function Load(const aPEM: String): TLoaded;
  var
    S: TStringStream;
  begin
    Result:=Default(TLoaded);
    S:=TStringStream.Create(aPEM);
    try
      Result.Ok:=PemLoadECDSA(S,Result.PrivateKey,Result.PublicKey,Result.X64,Result.Y64,Result.ThumbPrint);
    finally
      S.Free;
    end;
  end;

var
  Plain, Comp: TLoaded;
begin
  Plain:=Load(KeyUncompressed);
  AssertTrue('Uncompressed key loaded',Plain.Ok);
  AssertEquals('X of the uncompressed key',PointX,UpperCase(BytesToHexStr(Base64URL.Decode(Plain.X64))));
  AssertEquals('Y of the uncompressed key',PointY,UpperCase(BytesToHexStr(Base64URL.Decode(Plain.Y64))));
  Comp:=Load(KeyCompressed);
  AssertTrue('Compressed key loaded',Comp.Ok);
  AssertTrue('Same private key',CompareMem(@Plain.PrivateKey,@Comp.PrivateKey,SizeOf(Plain.PrivateKey)));
  AssertTrue('Same public key',CompareMem(@Plain.PublicKey,@Comp.PublicKey,SizeOf(Plain.PublicKey)));
  AssertEquals('X of the compressed key',PointX,UpperCase(BytesToHexStr(Base64URL.Decode(Comp.X64))));
  AssertEquals('Y of the compressed key',PointY,UpperCase(BytesToHexStr(Base64URL.Decode(Comp.Y64))));
  AssertEquals('Same thumbprint',Plain.ThumbPrint,Comp.ThumbPrint);
  AssertFalse('Key without public key not loaded',Load(KeyNoPublic).Ok);
end;

procedure TTestPEM.TestPemFindTags;
var
  List: TStringList;
  StartIndex, EndIndex: Integer;
begin
  List:=TStringList.Create;
  try
    List.Text:='Comment'+sLineBreak+_BEGIN_PUBLIC_KEY+sLineBreak+'AAAA'+sLineBreak+_END_PUBLIC_KEY;
    AssertTrue('Complete block found',PemFindTags(List,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY,StartIndex,EndIndex));
    AssertEquals('Begin line',1,StartIndex);
    AssertEquals('End line',3,EndIndex);

    List.Text:='Comment'+sLineBreak+_BEGIN_PUBLIC_KEY+sLineBreak+'AAAA';
    AssertFalse('Begin without end',PemFindTags(List,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY,StartIndex,EndIndex));
    AssertEquals('Start index without end',0,StartIndex);
    AssertEquals('End index without end',0,EndIndex);

    List.Text:='AAAA'+sLineBreak+_END_PUBLIC_KEY;
    AssertFalse('End without begin',PemFindTags(List,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY,StartIndex,EndIndex));
  finally
    List.Free;
  end;
  AssertEquals('PemToDER without end line',0,
               Length(PemToDER(_BEGIN_PUBLIC_KEY+sLineBreak+'AAAA'+sLineBreak,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY)));
end;

initialization
  RegisterTest(TTestPEM);
end.

