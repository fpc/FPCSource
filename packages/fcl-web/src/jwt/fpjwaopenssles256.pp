{
    This file is part of the Free Component Library.
    ES256 (ECDSA P-256 with SHA-256) JWT signer using OpenSSL.
    See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit FpJWAOpenSSLES256;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, Jwt.Types, Api.Openssl;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, fpjwt, openssl;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TJWTOpenSSLSignerES256 }

  // ES256 signer using OpenSSL. The key is a PEM encoded EC private key; Verify also accepts a PEM public key.
  // Not registered automatically: call TJWTOpenSSLSignerES256.Register to use it for ES256.
  TJWTOpenSSLSignerES256 = Class(TJWTSigner)
  Private
    class var FDefaultOutputP1363 : Boolean;
  Private
    FOutputP1363 : Boolean;
  Protected
    // Load the private key in aKey, nil if aKey holds none
    function LoadPrivateKey(aKey : TJWTKey) : PEVP_PKEY;
    // Load the public key in aKey, or else its private key
    function LoadVerifyKey(aKey : TJWTKey) : PEVP_PKEY;
  Public
    // Create a signer with OutputP1363 set to DefaultOutputP1363
    Constructor Create; override;
    // Return ES256
    Class function AlgorithmName : String; override;
    // Return the base64url encoded signature of aJWT made with the private key aKey
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    // Check the signature of aJWT, in P1363 or DER format, with the key aKey
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override;
    // Initial value of OutputP1363 for new signers, True by default
    class property DefaultOutputP1363 : Boolean read FDefaultOutputP1363 write FDefaultOutputP1363;
    // Emit IEEE P1363 (r||s) signatures as RFC 7518 requires; False emits DER
    property OutputP1363 : Boolean read FOutputP1363 write FOutputP1363;
  end;

// Convert a DER encoded ECDSA P-256 signature to 64-byte IEEE P1363 (r||s)
function DerToP1363(const aDer : TBytes) : TBytes;
// Convert a 64-byte IEEE P1363 (r||s) ECDSA P-256 signature to DER
function P1363ToDer(const aSig : TBytes) : TBytes;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, Fcl.BaseNEnc;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, basenenc;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SErrOpenSSLNotLoaded = 'Could not load the OpenSSL library';
  SErrInvalidECKey = 'Could not read an EC private key from the JWT key';
  SErrSignFailed = 'OpenSSL could not sign the JWT';
  SErrInvalidDER = 'Invalid DER encoded ECDSA signature';
  SErrInvalidP1363 = 'An ES256 P1363 signature has %d bytes, not %d';

const
  ES256IntSize = 32;
  ES256SigSize = 2 * ES256IntSize;

// Raise an exception if OpenSSL cannot be loaded
procedure CheckOpenSSL;

begin
  if not InitSSLInterface then
    Raise EJWT.Create(SErrOpenSSLNotLoaded);
end;


// Read the DER INTEGER at aPos: its start and length in aStart, aLen. aPos moves past it.
function ReadDerInteger(const aDer : TBytes; var aPos : Integer; out aStart, aLen : Integer) : Boolean;

begin
  aStart:=aPos+2;
  aLen:=0;
  Result:=(aStart<=Length(aDer)) and (aDer[aPos]=$02);
  if not Result then
    exit;
  aLen:=aDer[aPos+1];
  Result:=(aLen>0) and (aLen<$80) and (aStart+aLen<=Length(aDer));
  aPos:=aStart+aLen;
end;


// Convert DER signature aDer to P1363 in aSig, False if aDer is not a valid ES256 DER signature
function TryDerToP1363(const aDer : TBytes; out aSig : TBytes) : Boolean;

var
  lPos, lRStart, lRLen, lSStart, lSLen : Integer;

begin
  aSig:=Nil;
  Result:=(Length(aDer)>=8) and (aDer[0]=$30) and (aDer[1]<$80) and (aDer[1]+2=Length(aDer));
  if not Result then
    exit;
  lPos:=2;
  Result:=ReadDerInteger(aDer,lPos,lRStart,lRLen)
          and ReadDerInteger(aDer,lPos,lSStart,lSLen)
          and (lPos=Length(aDer));
  if not Result then
    exit;
  while (lRLen>ES256IntSize) and (aDer[lRStart]=0) do
    begin
    Inc(lRStart);
    Dec(lRLen);
    end;
  while (lSLen>ES256IntSize) and (aDer[lSStart]=0) do
    begin
    Inc(lSStart);
    Dec(lSLen);
    end;
  Result:=(lRLen<=ES256IntSize) and (lSLen<=ES256IntSize);
  if not Result then
    exit;
  SetLength(aSig,ES256SigSize);
  FillChar(aSig[0],ES256SigSize,0);
  Move(aDer[lRStart],aSig[ES256IntSize-lRLen],lRLen);
  Move(aDer[lSStart],aSig[ES256SigSize-lSLen],lSLen);
end;


// Append the aLen bytes of aData at aStart to aDer at aPos as a minimal DER INTEGER
procedure AddDerInteger(var aDer : TBytes; var aPos : Integer; const aData : TBytes; aStart, aLen : Integer);

begin
  while (aLen>1) and (aData[aStart]=0) do
    begin
    Inc(aStart);
    Dec(aLen);
    end;
  aDer[aPos]:=$02;
  if aData[aStart]>=$80 then
    begin
    aDer[aPos+1]:=aLen+1;
    aDer[aPos+2]:=0;
    Inc(aPos,3);
    end
  else
    begin
    aDer[aPos+1]:=aLen;
    Inc(aPos,2);
    end;
  Move(aData[aStart],aDer[aPos],aLen);
  Inc(aPos,aLen);
end;


function DerToP1363(const aDer : TBytes) : TBytes;

begin
  if not TryDerToP1363(aDer,Result) then
    Raise EJWT.Create(SErrInvalidDER);
end;


function P1363ToDer(const aSig : TBytes) : TBytes;

var
  lPos : Integer;

begin
  if Length(aSig)<>ES256SigSize then
    Raise EJWT.CreateFmt(SErrInvalidP1363,[Length(aSig),ES256SigSize]);
  Result:=Nil;
  SetLength(Result,2+2*(3+ES256IntSize));
  lPos:=2;
  AddDerInteger(Result,lPos,aSig,0,ES256IntSize);
  AddDerInteger(Result,lPos,aSig,ES256IntSize,ES256IntSize);
  Result[0]:=$30;
  Result[1]:=lPos-2;
  SetLength(Result,lPos);
end;


{ TJWTOpenSSLSignerES256 }

constructor TJWTOpenSSLSignerES256.Create;

begin
  Inherited Create;
  FOutputP1363:=FDefaultOutputP1363;
end;


class function TJWTOpenSSLSignerES256.AlgorithmName : String;

begin
  Result:='ES256';
end;


function TJWTOpenSSLSignerES256.LoadPrivateKey(aKey : TJWTKey) : PEVP_PKEY;

var
  lBio : PBIO;

begin
  Result:=Nil;
  if aKey.Length=0 then
    exit;
  lBio:=BIO_new_mem_buf(aKey.AsPointer,aKey.Length);
  if lBio=Nil then
    exit;
  try
    Result:=PEM_read_bio_PrivateKey(lBio,Nil,Nil,Nil);
  finally
    BioFreeAll(lBio);
  end;
end;


function TJWTOpenSSLSignerES256.LoadVerifyKey(aKey : TJWTKey) : PEVP_PKEY;

var
  lBio : PBIO;
  lKey : PEVP_PKEY;

begin
  Result:=Nil;
  if aKey.Length=0 then
    exit;
  lBio:=BIO_new_mem_buf(aKey.AsPointer,aKey.Length);
  if lBio=Nil then
    exit;
  try
    lKey:=Nil;
    Result:=PEM_read_bio_PUBKEY(lBio,lKey,Nil,Nil);
  finally
    BioFreeAll(lBio);
  end;
  if Result=Nil then
    Result:=LoadPrivateKey(aKey);
end;


function TJWTOpenSSLSignerES256.CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String;

var
  lKey : PEVP_PKEY;
  lCtx : PEVP_MD_CTX;
  lInput, lSig : TBytes;
  lLen : csize_t;

begin
  Result:='';
  CheckOpenSSL;
  lKey:=LoadPrivateKey(aKey);
  if lKey=Nil then
    Raise EJWT.Create(SErrInvalidECKey);
  lCtx:=Nil;
  try
    lCtx:=EVP_MD_CTX_new;
    if lCtx=Nil then
      Raise EJWT.Create(SErrSignFailed);
    lInput:=GetSignInput(aJWT);
    lLen:=0;
    if (EVP_DigestSignInit(lCtx,Nil,EVP_sha256,Nil,lKey)<>1)
       or (EVP_DigestSignUpdate(lCtx,PByte(lInput),Length(lInput))<>1)
       or (EVP_DigestSignFinal(lCtx,Nil,@lLen)<>1) then
      Raise EJWT.Create(SErrSignFailed);
    lSig:=Nil;
    SetLength(lSig,lLen);
    if EVP_DigestSignFinal(lCtx,PByte(lSig),@lLen)<>1 then
      Raise EJWT.Create(SErrSignFailed);
    SetLength(lSig,lLen);
    if FOutputP1363 then
      lSig:=DerToP1363(lSig);
    Result:=Base64URL.Encode(lSig,False);
  finally
    if lCtx<>Nil then
      EVP_MD_CTX_free(lCtx);
    EVP_PKEY_free(lKey);
  end;
end;


function TJWTOpenSSLSignerES256.Verify(const aJWT : String; aKey : TJWTKey) : Boolean;

var
  J, C, S : String;
  lInput : AnsiString;
  lSig, lP1363, lDer : TBytes;
  lKey : PEVP_PKEY;
  lCtx : PEVP_MD_CTX;

begin
  Result:=GetParts(aJWT,J,C,S) and (S<>'');
  if not Result then
    exit;
  lSig:=Base64URL.Decode(S);
  if Length(lSig)=ES256SigSize then
    lDer:=P1363ToDer(lSig)
  else if TryDerToP1363(lSig,lP1363) then
    lDer:=P1363ToDer(lP1363)
  else
    Exit(False);
  CheckOpenSSL;
  lKey:=LoadVerifyKey(aKey);
  if lKey=Nil then
    Exit(False);
  lInput:=J+'.'+C;
  lCtx:=EVP_MD_CTX_new;
  try
    Result:=(lCtx<>Nil)
            and (EVP_DigestVerifyInit(lCtx,Nil,EVP_sha256,Nil,lKey)=1)
            and (EVP_DigestVerifyUpdate(lCtx,PAnsiChar(lInput),Length(lInput))=1)
            and (EVP_DigestVerifyFinal(lCtx,PByte(lDer),Length(lDer))=1);
  finally
    if lCtx<>Nil then
      EVP_MD_CTX_free(lCtx);
    EVP_PKEY_free(lKey);
  end;
end;


initialization
  TJWTOpenSSLSignerES256.FDefaultOutputP1363:=True;
end.
