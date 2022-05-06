{$mode objfpc}
{$h+}
uses jsonparser, dateutils, sysutils, fppem, fpjwt, fprsa, fphashutils, fpjwarsa;

Type
 // The claims we want to sign in our JWT. 
 // You can skip this class and just use TClaims if the standard claims are sufficient
 
  TMyClaims = Class(TClaims)
  private
    FAdmin: Boolean;
    FName: string;
  Published
    Property Name : string Read FName Write FName;
    Property admin : Boolean Read FAdmin Write FAdmin;
  end;
  
  // A JWT that uses our claims
  TMyJWT = Class(TJWT)
    Function CreateClaims : TClaims; override;
  end;      

  
function TMyJWT.CreateClaims: TClaims;
begin
  Result:=TMyClaims.Create;
end;

// Read the private/public key data
// In reality you will read this from a file.

procedure GetTestPEM(out aPrivateKeyPEM, aPublicKeyPEM: string);

const
  // generated with
  //   openssl genrsa -out private.pem 2048
  PrivateKeyPem =
    '-----BEGIN RSA PRIVATE KEY-----'#10+
    'MIIEpQIBAAKCAQEAvkRfGW8psCZ3G4+hBA6W/CR/FHhBLB3k3QLypamPbRFlFBxL'#10+
    'tOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAlTqhtbTWLy/0K3qbG09vLm8V40kEK'#10+
    '8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6K5JGK1VTsMuDCS7DCYk6Vqr3zjX7'#10+
    'qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG/PN2GUQ0oZpa16DDtfgGu7wT3X3Q'#10+
    'EZFWLJYQTvGc82NpachBIUvqNdIt1npbK38MXU4IPHVrSN/HdK2nQPSMLdKnTV+E'#10+
    'h/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg8wIDAQABAoIBAQCZxVwujB7fFFdS'#10+
    '2QPC6Z+w7DYgbwgNBaP/0vAUXzNhbJuKY0v0Rv4H8U9wHGm9EDyvrdG8JHZqPBX+'#10+
    'dJNQ97aPGaRGjO4M0NdGFve+JXcqz6/UDWkywYnV3V1A0NhmdPQK2et3DSjqN7qQ'#10+
    'OoAoVWzR5gf74Zwf2Hpwo3BRdqzFeUYVDOH7e7q1SOf2QeU54kVUG21saJR0wsyH'#10+
    'oSX8BMU2kmg1Un8ET4FM5xEwhdTZzgFTJVZhc6EfOKVbQt6cKmW3aER3c9vR7M3l'#10+
    'N6Oq73vqrfmy+jFMwz1SoPObQQ7UAnr7YUowaX0AzxHpYm/afyVm+Toym0qWGrrY'#10+
    'MY/l+vNRAoGBAOsi72pJj30ApfVbSpx8/8QIpweLbEgAD+Ssd41Kgc4O/N7azB61'#10+
    'RjzSOs1BGhpAZNU6muAAbucm9EssfG5WTAjIM2W2LVuZXXEVXqEGkIymPz9NGugf'#10+
    'JaCWLaoibmwHkKa+ZV9kDwasmx/VkbAfAbRWaz49ejdrMmkpCW77lYjHAoGBAM8m'#10+
    'PVJWvFhQrB21xQGSWKd5iSUn2V92gICeDoORqfVtt/UPOaDT915KzXPh4bJeOwg6'#10+
    'Kkx5wX6UwaNSRH39loDSY1rsBYioV8bxW0BpBvEJG7KXRbBvxzr0+TJkCHgmGMns'#10+
    'dhePYUcriCaqpQi1yzf201oLTZ6PlJxkmHQobXJ1AoGBAIgWPg576InmWCa64WHU'#10+
    'joq8nz8kmFTLhGdK0h56IspJrlyksUKMk8wbuGCW7y6GWlV2h7BhT86Eoxrm8lVB'#10+
    'qNvkUqrpVzMOfiA2x//WNs7QYQaX75ysejCI+oDfUJ1Be5yl0TH2TSQFvfoctycB'#10+
    'qxDee08YcaWlaxWl5InRHeh9AoGABm3XZWDPw6XtUZa8oIncOoZpHUAZXP8eid9d'#10+
    '7/NrZPScyvxH+5fYi5Kiwb/280Q9bMnxWiJFQRp40ArTmV1veFwPPVkp6s3eu4vu'#10+
    'GxenYX+43lgXj5xIgKntugSkxqXYCxxNpfmLOVw+g4S0Torl3bzJXngPVqZ6JEhy'#10+
    '+tfuXakCgYEA19/JCD/5pVPJtwyDDAYnUUESK+JfBPq1cTbsxcOq01mp5ntsqR4y'#10+
    'dtOAmxMASvsqud3XIM5fO5m3Jpl1phiGhCw4nvVLcYzVWxYY+oWoeCSyECgu5tmT'#10+
    'Fo8vn4EEXCkEAA2YPiEuVcrcYsWkLivCTC19lJDfUNMmpwSdiGz/tDU='#10+
    '-----END RSA PRIVATE KEY-----'#10;

  PublicKeyPem =
    '-----BEGIN PUBLIC KEY-----'#10+
    'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvkRfGW8psCZ3G4+hBA6W'#10+
    '/CR/FHhBLB3k3QLypamPbRFlFBxLtOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAl'#10+
    'TqhtbTWLy/0K3qbG09vLm8V40kEK8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6'#10+
    'K5JGK1VTsMuDCS7DCYk6Vqr3zjX7qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG'#10+
    '/PN2GUQ0oZpa16DDtfgGu7wT3X3QEZFWLJYQTvGc82NpachBIUvqNdIt1npbK38M'#10+
    'XU4IPHVrSN/HdK2nQPSMLdKnTV+Eh/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg'#10+
    '8wIDAQAB'#10+
    '-----END PUBLIC KEY-----';
    
begin
  aPrivateKeyPEM:=PrivateKeyPem;
  aPublicKeyPEM:=PublicKeyPem;
end;

Procedure CreateKeyAndJWT(out Key : TJWTKey; Out JWT : TJWT);

begin
  Key:=TJWTKey.Create('mysecretkey');
  JWT:=TMyJWT.Create;
  JWT.JOSE.alg:='none';
  JWT.JOSE.typ:='JWT';
  JWT.Claims.sub:='1234567890';
  JWT.Claims.iat:=1516239022;
  (JWT.Claims as TMyClaims).Name:='John Doe';
end;

var
  aInput, aPrivateKeyPEM, aPublicKeyPEM: String;
  Key: TJWTKey;
  VerifyResult,JWT : TJWT;
  Signer: TJWTSigner;
  NewDER: TBytes;
  RSAPublic: TX509RSAPublicKey;
  RSAPrivate: TX509RSAPrivateKey;
  
begin
  CreateKeyAndJWT(Key,JWT);
  GetTestPEM(aPrivateKeyPEM, aPublicKeyPEM);
  
  // header
  jwt.JOSE.alg:=TJWTSignerRS256.AlgorithmName;

  // claims
  jwt.Claims.exp:=DateTimeToUnix(Now+10);
  jwt.Claims.iss:='FPC JWT';

  // load private key from pem
  Key.AsBytes:=PemToDER(APrivateKeyPem,_BEGIN_RSA_PRIVATE_KEY,_END_RSA_PRIVATE_KEY);
  X509RsaPrivateKeyInitFromDER(RSAPrivate,Key.AsBytes);
  NewDER:=RSAPrivate.AsDER;
  if (length(Key.AsBytes)<>length(NewDER)) or
      not CompareMem(@Key.AsBytes[0],@NewDER[0],length(NewDER)) then
    Raise exception.Create('TX509RSAPrivateKey.AsDER does not match!');

  // sign
  Signer:=TJWTSignerRS256.Create;
  try
    aInput:=Signer.AppendSignature(JWT,Key);
  finally
    Signer.Free;
  end;

  Writeln('Our JWT Token: ',aInput);

  // load public key from pem
  Key.AsBytes:=PemToDER(APublicKeyPem,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY);
  X509RsaPublicKeyInitFromDER(RSAPublic,Key.AsBytes);
  NewDER:=RSAPublic.AsDER;
  if (length(Key.AsBytes)<>length(NewDER)) or
      not CompareMem(@Key.AsBytes[0],@NewDER[0],length(NewDER)) then
    Raise exception.Create('TX509RSAPublicKey.AsDER does not match');

  // verify our generated token. This generates a new JWT
  VerifyResult:=TMyJWT.ValidateJWT(aInput,Key);
  if VerifyResult=Nil then
    Raise Exception.Create('No verify resultn, verification failed!');
end.