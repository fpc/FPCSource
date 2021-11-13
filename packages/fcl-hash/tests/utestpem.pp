unit utestpem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpasn, fppem;

Type

  { TTestPEM }

  TTestPEM = Class(TTestCase)
  Published
    Procedure TestLoad;
  end;

implementation

uses basenenc, fphashutils, fpecc;


{ TTestPEM }
Const
  PrivateKeyFile = 'private-key.pem';

procedure TTestPEM.TestLoad;

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

initialization
  RegisterTest(TTestPEM);
end.

