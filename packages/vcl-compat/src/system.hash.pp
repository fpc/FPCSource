{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2023 the Free Pascal team

    Delphi-compatible hash unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.Hash;

{$mode objfpc}
{$modeswitch advancedrecords}
{$macro on}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Hash.Md5, System.Hash.Sha1, System.Hash.Fnv, System.Hash.Sha256, System.Hash.Sha512;
{$ELSE}
  Classes, SysUtils, md5, sha1, fnvhash, fpsha256, fpsha512;
{$ENDIF}

const
  HashReadBufferSize = 4096; // Use 4k buffer.

type
  EHashException = class(Exception);

  { THash }

  THash = record
    class function DigestAsInteger(const aDigest: TBytes): Integer; static;
    class function DigestAsString(const aDigest: TBytes; UpperCase : Boolean = false): UnicodeString; static;
    class function DigestAsStringGUID(const aDigest: TBytes): UnicodeString; static;
    class function GetRandomString(const aLen: Integer = 10): UnicodeString; static;
    class function ToBigEndian(aValue: Cardinal): Cardinal; overload; static; inline;
    class function ToBigEndian(aValue: UInt64): UInt64; overload; static; inline;
  end;

  { THashMD5 }

  THashMD5 = record
  Private
    _MD5 : TMD5Context;
    _Digest : TMD5Digest;
    _DidFinal : Boolean;
  public
    class function Create: THashMD5; static; inline;
    class function GetHashBytes(const aData: UnicodeString): TBytes; overload; static;
    class function GetHashString(const aData: UnicodeString): UnicodeString; overload; static; inline;
    class function GetHashBytes(const aStream: TStream): TBytes; overload; static;
    class function GetHashString(const aStream: TStream): UnicodeString; overload; static; inline;
    class function GetHashBytesFromFile(const aFileName: TFileName): TBytes; static;
    class function GetHashStringFromFile(const aFileName: TFileName): UnicodeString; static; inline;
    class function GetHMAC(const aData,aKey: UnicodeString): UnicodeString; static; inline;
    class function GetHMACAsBytes(const aData,aKey: UnicodeString): TBytes; overload; static;
    class function GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes): TBytes; overload; static;
    class function GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString): TBytes; overload; static;
    class function GetHMACAsBytes(const aData,aKey: TBytes): TBytes; overload; static;
    procedure Reset;
    procedure Update(var aData; aLength: Cardinal); overload;
    procedure Update(const aData: TBytes; aLength: Cardinal = 0); overload; {inline;}
    procedure Update(const aData: UnicodeString); overload; {inline;}
    function GetDigest: TBytes;
    function GetBlockSize: Integer; inline;
    function GetHashSize: Integer; inline;
    function HashAsBytes: TBytes; inline;
    function HashAsString: UnicodeString; // inline;
  end;

  { THashSHA1 }

  THashSHA1 = record
  private
    _SHA1 : TSHA1Context;
    _Digest : TSHA1Digest;
    _DidFinal : Boolean;
  public
    class function Create: THashSHA1; static; // inline;
    class function GetHashBytes(const aData: UnicodeString): TBytes; overload; static;
    class function GetHashString(const aData: UnicodeString): UnicodeString; overload; static; // inline;
    class function GetHashBytes(const aStream: TStream): TBytes; overload; static;
    class function GetHashString(const aStream: TStream): UnicodeString; overload; static; // inline;
    class function GetHashBytesFromFile(const aFileName: TFileName): TBytes; static;
    class function GetHashStringFromFile(const aFileName: TFileName): UnicodeString; static; // inline;
    class function GetHMAC(const aData, aKey: UnicodeString): UnicodeString; static; // inline;
    class function GetHMACAsBytes(const aData, aKey: UnicodeString): TBytes; overload;  static;
    class function GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes): TBytes; overload; static;
    class function GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString): TBytes; overload; static;
    class function GetHMACAsBytes(const aData, aKey: TBytes): TBytes; overload; static;
    procedure Reset; inline;
    procedure Update(var aData; aLength: Cardinal); overload;
    procedure Update(const aData: TBytes; aLength: Cardinal = 0); overload;
    procedure Update(const aData: UnicodeString); overload; // inline;
    function GetDigest: TBytes;
    function GetBlockSize: Integer; inline;
    function GetHashSize: Integer; inline;
    function HashAsBytes: TBytes; inline;
    function HashAsString: UnicodeString; // inline;
  end;

  { THashSHA2 }

  THashSHA2 = record
  public type
    TSHA2Version = (SHA224, SHA256, SHA384, SHA512, SHA512_224, SHA512_256);
  public
    class function Create(aHashVersion: TSHA2Version = TSHA2Version.SHA256): THashSHA2; static; inline;
    class function GetHashBytes(const aData: UnicodeString; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload; static;
    class function GetHashString(const aData: UnicodeString; aHashVersion: TSHA2Version = TSHA2Version.SHA256): UnicodeString; overload; static; inline;
    class function GetHashBytes(const aStream: TStream; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload; static;
    class function GetHashString(const aStream: TStream; aHashVersion: TSHA2Version = TSHA2Version.SHA256): UnicodeString; overload; static; inline;
    class function GetHashBytesFromFile(const aFileName: TFileName; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; static;
    class function GetHashStringFromFile(const aFileName: TFileName; aHashVersion: TSHA2Version = TSHA2Version.SHA256): UnicodeString; static; inline;
    class function GetHMAC(const aData, aKey: UnicodeString; aHashVersion: TSHA2Version = TSHA2Version.SHA256): UnicodeString; static; inline;
    class function GetHMACAsBytes(const aData, aKey: UnicodeString; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload;  static;
    class function GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload; static;
    class function GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload; static;
    class function GetHMACAsBytes(const aData, aKey: TBytes; aHashVersion: TSHA2Version = TSHA2Version.SHA256): TBytes; overload; static;
    procedure Reset; inline;
    procedure Update(var aData; aLength: Cardinal); overload;
    procedure Update(const aData : PByte; aLength: Cardinal); overload;
    procedure Update(const aData: TBytes; aLength: Cardinal = 0); overload;
    procedure Update(const aData: UnicodeString); overload;
    procedure Update(const aData: RawByteString); overload;
    function GetDigest: TBytes;
    function GetBlockSize: Integer; inline;
    function GetHashSize: Integer;  inline;
    function HashAsBytes: TBytes; // inline;
    function HashAsString: UnicodeString; // inline;
  Private
    procedure DoFinal;
  private
    FDidFinal : Boolean;
    case FHashVersion: TSHA2Version of
      Sha224 : (_S224 : TSHA224);
      Sha256 : (_S256 : TSHA256);
      Sha384 : (_S384 : TSHA384);
      Sha512 : (_S512 : TSHA512);
  end;

  { THashBobJenkins }

  THashBobJenkins = record
  Private
    FCurrent : Cardinal;
  public
    class function Create: THashBobJenkins; static;
    class function GetHashBytes(const aData: UnicodeString): TBytes; static;
    class function GetHashString(const aData: UnicodeString): UnicodeString; static;
    class function GetHashString(const aData: RawByteString): UnicodeString; static;
    class function GetHashValue(const aData: UnicodeString): Integer; overload; static;
    class function GetHashValue(const aData: RawByteString): Integer; overload; static;
    class function GetHashValue(var aData; aLength: Integer; aInitialValue: Integer = 0): Integer; overload; static;
    class function GetHashValue(const aData : PByte; aLength: Integer; aInitialValue: Integer = 0): Integer; overload; static;
    procedure Reset(aInitialValue: Integer = 0);
    procedure Update(var aData; aLength: Cardinal); overload;
    procedure Update(aData : PByte; aLength: Cardinal); overload;
    procedure Update(const aData: TBytes; aLength: Cardinal = 0); overload;
    procedure Update(const aData: UnicodeString); overload;
    procedure Update(const aData: RawByteString); overload;
    function HashAsBytes: TBytes;
    function HashAsInteger: Integer;
    function HashAsString: UnicodeString;
  end;

  { THashFNV1a32 }

  THashFNV1a32 = record
  public const
    FNV_PRIME = FNV_32_PRIME;
    FNV_SEED  = FNV1_32_INIT;
  private
    FCurrent : Fnv32_t;
  public
    class function Create: THashFNV1a32; static;
    class function GetHashBytes(const aData: UnicodeString): TBytes; static;
    class function GetHashString(const aData: UnicodeString): UnicodeString; overload; static;
    class function GetHashString(const aData: RawByteString): UnicodeString; overload; static;
    class function GetHashValue(const aData: UnicodeString): Integer; overload; static; // inline;
    class function GetHashValue(const aData: RawByteString): Integer; overload; static; // inline;
    class function GetHashValue(const aData; aLength: Cardinal; aInitialValue: Cardinal = FNV_SEED): Integer; overload; static; // inline;
    procedure Reset(aInitialValue: Cardinal = FNV_SEED);
    procedure Update(const aData; aLength: Cardinal); overload; // inline;
    procedure Update(const aData: TBytes; aLength: Cardinal = 0); overload; // inline;
    procedure Update(const aData: UnicodeString); overload; // inline;
    procedure Update(const aData: RawByteString); overload; // inline;
    function GetDigest : TBytes;
    function HashAsBytes: TBytes;
    function HashAsInteger: Integer;
    function HashAsString: UnicodeString;
  end;

const
  SHashCanNotUpdateMD5 = 'MD5: Cannot update a finalized hash';
  SHashCanNotUpdateSHA1 = 'SHA1: Cannot update a finalized hash';
  SHashCanNotUpdateSHA2 = 'SHA2: Cannot update a finalized hash';
  RandomStringChars =  UnicodeString('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-/*_');
  RandomStringCharCount = Length(RandomStringChars);


implementation

Uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysConst, System.Generics.Hashes;
{$ELSE}
  Types, SysConst, Generics.Hashes;
{$ENDIF}

Resourcestring
  SErrDigestSizeMustBe4 = 'Digest size must be 4, got %d instead.';


{ THash }

class function THash.ToBigEndian(aValue: Cardinal): Cardinal;
begin
  Result:=NtoBE(aValue);
end;

class function THash.ToBigEndian(aValue: UInt64): UInt64;
begin
  Result:=NtoBE(aValue);
end;


class function THash.DigestAsInteger(const aDigest: TBytes): Integer;
begin
  if Length(aDigest) <> 4 then
    raise EHashException.CreateFmt(SErrDigestSizeMustBe4,[Length(aDigest)]);
  Result:= PLongInt(@ADigest[0])^;
end;

class function THash.DigestAsString(const aDigest: TBytes; UpperCase: Boolean): UnicodeString;

const
  HexDigitsWL: array[0..15] of widechar = '0123456789abcdef';


var
  S : UnicodeString;
  I,Len: Integer;
  H,Res : PWideChar;
  PB : PByte;
  B : Byte;

begin
  S:='';
  if Uppercase then
    H:=@HexDigitsW
  else
    H:=@HexDigitsWL;
  Len:=Length(aDigest);
  SetLength(S,2*Len);
  Res:=PWideChar(S);
  PB:=PByte(aDigest);
  for I:=1 to Len do
    begin
    B:=PB^;
    Res^:=H[B shr 4];
    inc(Res);
    Res^:=H[B and 15];
    Inc(Res);
    Inc(PB);
    end;
  Result:=S;
end;

class function THash.DigestAsStringGUID(const aDigest: TBytes): UnicodeString;


begin
  With TGUID.Create(aDigest,TEndian.Little) do
    begin
    D1:=ToBigEndian(D1);
    D2:=Swap(D2);
    D3:=Swap(D3);
{$IF SIZEOF(Char)=2}
    Result:=ToString;
{$ELSE}
    Result:=Utf8Decode(ToString);
{$ENDIF}
    end;
end;

class function THash.GetRandomString(const aLen: Integer): UnicodeString;

var
  I : Integer;
  Res: PWideChar;

begin
  Result:='';
  SetLength(Result,aLen);
  Res:=PWideChar(Result);
  for I:=1 to ALen do
    begin
    Res^:=RandomStringChars[1+Random(RandomStringCharCount)];
    Inc(Res);
    end;
end;

{ THashMD5 }

class function THashMD5.Create: THashMD5;
begin
  Result:=Default(THashMD5);
  Result.Reset;
end;

function THashMD5.GetBlockSize: Integer;
begin
  Result:=64;
end;

function THashMD5.GetHashSize: Integer;
begin
  Result:=SizeOf(TMD5Digest);
end;

class function THashMD5.GetHashBytes(const aData: UnicodeString): TBytes;

begin
  With THashMD5.Create do
    begin
    Update(aData);
    Result:=GetDigest;
    end;
end;

class function THashMD5.GetHashString(const aData: UnicodeString): UnicodeString;

begin
  With THashMD5.Create do
    begin
    Update(aData);
    Result:=HashAsString;
    end;
end;

class function THashMD5.GetHashBytes(const aStream: TStream): TBytes;

var
  Buf: TBytes;
  Len,Count: Longint;

begin
  Buf:=Default(TBytes);
  Len:=HashReadBufferSize;
  SetLength(Buf,Len);
  With THashMD5.Create do
    begin
    Count:=aStream.Read(Buf,Len);
    While (Count>0) do
      begin
      Update(Buf,Count);
      Count:=aStream.Read(Buf,Len);
      end;
    Result:=GetDigest;
    end;
end;

class function THashMD5.GetHashString(const aStream: TStream): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytes(aStream));
end;

class function THashMD5.GetHashBytesFromFile(const aFileName: TFileName): TBytes;

var
  F: TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=GetHashBytes(F);
  finally
    F.Free;
  end;
end;

class function THashMD5.GetHashStringFromFile(const aFileName: TFileName): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytesFromFile(aFileName));
end;

class function THashMD5.GetHMAC(const aData,aKey: UnicodeString): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHMACAsBytes(aData,aKey));
end;

class function THashMD5.GetHMACAsBytes(const aData, aKey: UnicodeString): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),GetBytes(aKey));
end;

class function THashMD5.GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),aKey);
end;

class function THashMD5.GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(aData,GetBytes(aKey));
end;

class function THashMD5.GetHMACAsBytes(const aData, aKey: TBytes): TBytes;

var
  I: Byte;
  MD5_BLOCK_SIZE : Integer;
  VLength: PtrUInt;
  PKey, POPad, PIPad: PByte;
  VKey, VOPad, VIPad: TBytes;
  MD5  : THashMD5;

begin
  VKey:=Default(TBytes);
  VOPad:=Default(TBytes);
  VIPad:=Default(TBytes);
  MD5:=THashMD5.Create;
  MD5_BLOCK_SIZE:=MD5.GetBlockSize;
  VLength:=Length(aKey);
  if VLength > MD5_BLOCK_SIZE then
    begin
    SetLength(VKey,MD5_BLOCK_SIZE);
    FillChar(VKey[0],MD5_BLOCK_SIZE, #0);
    MD5.Update(aKey);
    VKey:=Concat(MD5.GetDigest,VKey);
    end
  else
    begin
    SetLength(VKey,MD5_BLOCK_SIZE-VLength);
    FillChar(VKey[0],MD5_BLOCK_SIZE-VLength, #0);
    VKey:=Concat(aKey,VKey);
    end;
  SetLength(VOPad,MD5_BLOCK_SIZE);
  POPad:=PByte(VOPad);
  FillChar(POPad^, MD5_BLOCK_SIZE, $5c);
  SetLength(VIPad, MD5_BLOCK_SIZE);
  PIPad := PByte(VIPad);
  FillChar(PIPad^, MD5_BLOCK_SIZE, $36);
  PKey := PByte(VKey);
  for I:=1 to VLength do
    begin
    POPad^:=(POPad^ xor PKey^);
    PIPad^:=(PIPad^ xor PKey^);
    Inc(POPad);
    Inc(PIPad);
    Inc(PKey);
    end;
  VIPad:=Concat(VIPad,aData);
  MD5.Reset;
  MD5.Update(VIPad);
  Result:=Concat(VOPad,MD5.GetDigest);
end;

procedure THashMD5.Reset;
begin
  MD5Init(_MD5);
end;

procedure THashMD5.Update(var aData; aLength: Cardinal);
begin
  MD5Update(_MD5,aData,aLength);
end;

procedure THashMD5.Update(const aData: TBytes; aLength: Cardinal);
begin
  if aLength=0 then
    aLength:=Length(aData);
  MD5Update(_MD5,aData[0],aLength);
end;

procedure THashMD5.Update(const aData: UnicodeString);
begin
  Update(TEncoding.UTF8.GetBytes(aData));
end;

function THashMD5.GetDigest: TBytes;

begin
  Result:=[];
  if not _DidFinal then
    begin
    _DidFinal:=True;
    MD5Final(_MD5,_Digest);
    end;
  SetLength(Result,Length(_Digest));
  Move(_Digest,Result[0],Length(_Digest));
end;

function THashMD5.HashAsBytes: TBytes;
begin
  Result:=GetDigest;
end;

function THashMD5.HashAsString: UnicodeString;
begin
   Result:=THash.DigestAsString(GetDigest);
end;

{ THashSHA1 }

procedure THashSHA1.Reset;
begin
  SHA1Init(_SHA1);
end;

class function THashSHA1.Create: THashSHA1;
begin
  Result:=Default(THashSHA1);
  Result.Reset;
end;

procedure THashSHA1.Update(var aData; aLength: Cardinal);

begin
  SHA1Update(_SHA1,aData,aLength);
end;

procedure THashSHA1.Update(const aData: TBytes; aLength: Cardinal);
begin
  if ALength=0 then
    ALength:=Length(aData);
  Update(aData[0],aLength);
end;

procedure THashSHA1.Update(const aData: UnicodeString);
begin
  Update(TEncoding.UTF8.GetBytes(aData));
end;

function THashSHA1.GetBlockSize: Integer;
begin
  Result:=64;
end;

function THashSHA1.GetHashSize: Integer;
begin
  Result:=20;
end;

function THashSHA1.HashAsBytes: TBytes;
begin
  Result:=GetDigest;
end;

function THashSHA1.HashAsString: UnicodeString;
begin
  Result:=THash.DigestAsString(GetDigest);
end;

class function THashSHA1.GetHashBytes(const aData: UnicodeString): TBytes;
begin
  With THashSha1.Create do
    begin
    Update(aData);
    Result:=GetDigest;
    end;
end;

class function THashSHA1.GetHashString(const aData: UnicodeString): UnicodeString;
begin
  With THashSha1.Create do
    begin
    Update(aData);
    Result:=HashAsString;
    end;
end;

class function THashSHA1.GetHashBytes(const aStream: TStream): TBytes;

var
  Buf: TBytes;
  Len,Count: Longint;

begin
  Buf:=Default(TBytes);
  Len:=HashReadBufferSize;
  SetLength(Buf,Len);
  With THashSha1.Create do
    begin
    Count:=aStream.Read(Buf,Len);
    While (Count>0) do
      begin
      Update(Buf,Count);
      Count:=aStream.Read(Buf,Len);
      end;
    Result:=GetDigest;
    end;
end;

class function THashSHA1.GetHashString(const aStream: TStream): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytes(aStream));
end;

class function THashSHA1.GetHashBytesFromFile(const aFileName: TFileName): TBytes;

var
  F: TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=GetHashBytes(F);
  finally
    F.Free;
  end;
end;

class function THashSHA1.GetHashStringFromFile(const aFileName: TFileName): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytesFromFile(aFileName));
end;

class function THashSHA1.GetHMAC(const aData, aKey: UnicodeString): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHMACAsBytes(aData,aKey));
end;

class function THashSHA1.GetHMACAsBytes(const aData, aKey: UnicodeString): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),GetBytes(aKey));
end;

class function THashSHA1.GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),aKey);
end;

class function THashSHA1.GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(aData,GetBytes(aKey));
end;

class function THashSHA1.GetHMACAsBytes(const aData, aKey: TBytes): TBytes;

var
  I: Byte;
  SHA1_BLOCK_SIZE : Integer;
  VLength: PtrUInt;
  PKey, POPad, PIPad: PByte;
  VKey, VOPad, VIPad: TBytes;
  Sha1  : THashSha1;

begin
  VKey:=Default(TBytes);
  VOPad:=Default(TBytes);
  VIPad:=Default(TBytes);
  Sha1:=THashSha1.Create;
  SHA1_BLOCK_SIZE:=Sha1.GetBlockSize;
  VLength:=Length(aKey);
  if VLength > SHA1_BLOCK_SIZE then
    begin
    SetLength(VKey,SHA1_BLOCK_SIZE);
    FillChar(VKey[0],SHA1_BLOCK_SIZE, #0);
    Sha1.Update(aKey);
    VKey:=Concat(Sha1.GetDigest,VKey);
    end
  else
    begin
    SetLength(VKey,SHA1_BLOCK_SIZE-VLength);
    FillChar(VKey[0],SHA1_BLOCK_SIZE-VLength, #0);
    VKey:=Concat(aKey,VKey);
    end;
  SetLength(VOPad,SHA1_BLOCK_SIZE);
  POPad:=PByte(VOPad);
  FillChar(POPad^, SHA1_BLOCK_SIZE, $5c);
  SetLength(VIPad, SHA1_BLOCK_SIZE);
  PIPad := PByte(VIPad);
  FillChar(PIPad^, SHA1_BLOCK_SIZE, $36);
  PKey := PByte(VKey);
  for I:=1 to VLength do
    begin
    POPad^:=(POPad^ xor PKey^);
    PIPad^:=(PIPad^ xor PKey^);
    Inc(POPad);
    Inc(PIPad);
    Inc(PKey);
    end;
  VIPad:=Concat(VIPad,aData);
  Sha1.Reset;
  Sha1.Update(VIPad);
  Result:=Concat(VOPad,Sha1.GetDigest);
end;

function THashSHA1.GetDigest: TBytes;

begin
  Result:=[];
  if not _DidFinal then
    begin
    _DidFinal:=True;
    SHA1Final(_SHA1,_Digest);
    end;
  SetLength(Result,Length(_Digest));
  Move(_Digest,Result[0],Length(_Digest));
end;


{ THashSHA2 }

Procedure NotSupportedVersion(aHashVersion : THashSHA2.TSHA2Version);

var
  S : String;

begin
  WriteStr(S,aHashversion);
  Raise EHashException.CreateFmt('SHA2 - %s not yet supported',[S]);
end;

class function THashSHA2.Create(aHashVersion: TSHA2Version): THashSHA2;

begin
  if aHashVersion in [SHA512_224, SHA512_256] then
    NotSupportedVersion(aHashVersion);
  Result.FHashVersion:=aHashVersion;
  Result.Reset;
end;

class function THashSHA2.GetHashBytes(const aData: UnicodeString; aHashVersion: TSHA2Version): TBytes;

var
  H : THashSHA2;

begin
  H:=THashSHA2.Create(aHashVersion);
  H.Update(AData);
  Result:=H.GetDigest;
end;

class function THashSHA2.GetHashString(const aData: UnicodeString; aHashVersion: TSHA2Version): UnicodeString;

var
  H: THashSHA2;

begin
  H:=THashSHA2.Create(aHashVersion);
  H.Update(aData);
  Result:=H.HashAsString;
end;

class function THashSHA2.GetHashBytes(const aStream: TStream; aHashVersion: TSHA2Version): TBytes;

var
  Buf: TBytes;
  Len,Count: Longint;

begin
  Buf:=Default(TBytes);
  Len:=HashReadBufferSize;
  SetLength(Buf,Len);
  With THashSha2.Create(aHashVersion) do
    begin
    Count:=aStream.Read(Buf,Len);
    While (Count>0) do
      begin
      Update(Buf,Count);
      Count:=aStream.Read(Buf,Len);
      end;
    Result:=GetDigest;
    end;
end;

class function THashSHA2.GetHashString(const aStream: TStream; aHashVersion: TSHA2Version): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytes(aStream,aHashVersion));
end;

class function THashSHA2.GetHashBytesFromFile(const aFileName: TFileName; aHashVersion: TSHA2Version): TBytes;

var
  F: TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=GetHashBytes(F,aHashVersion);
  finally
    F.Free;
  end;
end;

class function THashSHA2.GetHashStringFromFile(const aFileName: TFileName; aHashVersion: TSHA2Version): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHashBytesFromFile(aFileName,aHashVersion));
end;

class function THashSHA2.GetHMAC(const aData, aKey: UnicodeString; aHashVersion: TSHA2Version): UnicodeString;
begin
  Result:=THash.DigestAsString(GetHMACAsBytes(aData,aKey,aHashVersion));
end;

class function THashSHA2.GetHMACAsBytes(const aData, aKey: UnicodeString; aHashVersion: TSHA2Version): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),GetBytes(aKey),aHashVersion);
end;

class function THashSHA2.GetHMACAsBytes(const aData: UnicodeString; const aKey: TBytes; aHashVersion: TSHA2Version): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(GetBytes(aData),aKey,aHashVersion);
end;

class function THashSHA2.GetHMACAsBytes(const aData: TBytes; const aKey: UnicodeString; aHashVersion: TSHA2Version): TBytes;
begin
  With TEncoding.UTF8 do
    Result:=GetHMACAsBytes(aData,GetBytes(aKey),aHashVersion);
end;

class function THashSHA2.GetHMACAsBytes(const aData, aKey: TBytes; aHashVersion: TSHA2Version): TBytes;

var
  Count: UInt32;
  KeySize,DataSize,BufSize : Integer;
  aDigest,KeyBuffer, PadBuffer: TBytes;
  SHA2,SHA2_ : THashSHA2;

begin
  Result:=[];
  KeySize:=Length(akey);
  DataSize:=Length(aData);
  if aKey = nil then
    Exit;
  if aData = nil then
    Exit;
  SHA2:=THashSHA2.Create(aHashversion);
  BufSize:=SHA2.GetBlockSize;
  SetLength(KeyBuffer,BufSize);
  SetLength(PadBuffer,BufSize);
  if KeySize>BufSize then
  begin
    SHA2.Update(aKey);
    aDigest:=SHA2.GetDigest;
    System.Move(aDigest[0],KeyBuffer[0],SHA2.GetHashSize);
  end else
    System.Move(aKey[0], KeyBuffer[0], KeySize);
  // XOR the key buffer with the iPad value
  for Count := 0 to BufSize do
    PadBuffer[Count] := KeyBuffer[Count] xor $36;
  SHA2.Reset;
  SHA2.Update(PadBuffer);
  SHA2.Update(aData);
  aDigest:=SHA2.GetDigest;
  // XOR the key buffer with the oPad value
  for Count := 0 to 63 do
    PadBuffer[Count] := KeyBuffer[Count] xor $5C;
  // SHA256 the key buffer and the result of the inner SHA256 (Outer)
  SHA2.Reset;
  SHA2.Update(PadBuffer);
  SHA2.Update(aDigest);
  Result:=SHA2_.GetDigest;
end;

procedure THashSHA2.Reset;
begin
  case FHashVersion of
    Sha224 : _S224.Init;
    Sha256 : _S256.Init;
    Sha384 : _S384.Init;
    Sha512 : _S512.Init;
  end;
  FDidFinal:=False;
end;

procedure THashSHA2.Update(var aData; aLength: Cardinal);
begin
  Update(PByte(@aData),aLength);
end;

procedure THashSHA2.Update(const aData: PByte; aLength: Cardinal);
begin
  case FHashVersion of
    Sha224 : _S224.Update(aData,aLength);
    Sha256 : _S256.Update(aData,aLength);
    Sha384 : _S384.Update(aData,aLength);
    Sha512 : _S512.Update(aData,aLength);
  end;
end;

procedure THashSHA2.Update(const aData: TBytes; aLength: Cardinal);
begin
  if aLength=0 then
    aLength:=Length(aData);
  Update(PByte(aData),aLength);
end;

procedure THashSHA2.Update(const aData: UnicodeString);
begin
  Update(TEncoding.UTF8.GetBytes(aData));
end;

procedure THashSHA2.Update(const aData: RawByteString);
begin
  Update(PByte(aData),Length(aData)*SizeOf(AnsiChar));
end;

procedure THashSHA2.DoFinal;

begin
  case FHashVersion of
    Sha224 : _S224.Final;
    Sha256 : _S256.Final;
    Sha384 : _S384.Final;
    Sha512 : _S512.Final;
  end;
  FDidFinal:=True;
end;

function THashSHA2.GetDigest: TBytes;

Var
  P : PByte;
  L : Integer;


begin
  if Not FDidFinal then
    DoFinal;
  // These should normally all be the same...
  case FHashVersion of
    Sha224 : P:=@_S224.Digest;
    Sha256 : P:=@_S256.Digest;
    Sha384 : P:=@_S384.Digest;
    Sha512 : P:=@_S512.Digest;
  end;
  L:=GetHashSize;
  SetLength(Result,L);
  Move(P^,Result[0],L);
end;

function THashSHA2.GetBlockSize: Integer;
Const
  Sizes : Array[TSHA2Version] of integer
        = (64,64,128,128,128,128);
begin
  Result:=Sizes[FHashVersion];
end;

function THashSHA2.GetHashSize: Integer;

Const
  Sizes : Array[TSHA2Version] of integer
        = (28,32,48,64,28,32);
begin
  Result:=Sizes[FHashVersion];
end;

function THashSHA2.HashAsBytes: TBytes;
begin
  Result:=GetDigest;
end;

function THashSHA2.HashAsString: UnicodeString;
begin
  Result:=THash.DigestAsString(GetDigest);
end;

{ THashBobJenkins }

class function THashBobJenkins.Create: THashBobJenkins;
begin
  Result.Reset;
end;

class function THashBobJenkins.GetHashBytes(const aData: UnicodeString): TBytes;
begin
  Result:=Default(TBytes);
  SetLength(Result,SizeOf(Cardinal));
  PCardinal(Result)^:=GetHashValue(aData)
end;

class function THashBobJenkins.GetHashString(const aData: UnicodeString): UnicodeString;
begin
  Result:=HexStr(GetHashValue(aData),8);
end;

class function THashBobJenkins.GetHashString(const aData: RawByteString): UnicodeString;
begin
  Result:=HexStr(GetHashValue(aData),8);
end;


class function THashBobJenkins.GetHashValue(var aData; aLength: Integer; aInitialValue: Integer): Integer;
begin
  Result:=Integer(DelphiHashLittle(PByte(@AData),aLength,aInitialValue));
end;

class function THashBobJenkins.GetHashValue(const aData: PByte; aLength: Integer; aInitialValue: Integer): Integer;
begin
  Result:=DelphiHashLittle(AData,aLength,aInitialValue);
end;

class function THashBobJenkins.GetHashValue(const aData: UnicodeString): Integer;
begin
  Result:=GetHashValue(PByte(aData),Length(aData)*SizeOf(UnicodeChar),0);
end;

class function THashBobJenkins.GetHashValue(const aData: RawByteString): Integer;
begin
  Result:=GetHashValue(PByte(aData),Length(aData)*SizeOf(AnsiChar),0);
end;

procedure THashBobJenkins.Reset(aInitialValue: Integer);
begin
  FCurrent:=aInitialValue;
end;

procedure THashBobJenkins.Update(aData: PByte; aLength: Cardinal);
begin
  FCurrent:=DelphiHashLittle(AData,aLength,FCurrent);
end;

procedure THashBobJenkins.Update(var aData; aLength: Cardinal);
begin
  Update(PByte(@AData),aLength);
end;

procedure THashBobJenkins.Update(const aData: TBytes; aLength: Cardinal);
begin
  if aLength=0 then
    aLength:=Length(aData);
  Update(PByte(aData),aLength);
end;

procedure THashBobJenkins.Update(const aData: UnicodeString);
begin
  Update(PByte(aData),Length(aData)*SizeOf(UnicodeChar));
end;

procedure THashBobJenkins.Update(const aData: RawByteString);
begin
  Update(PByte(aData),Length(aData)*SizeOf(AnsiChar));
end;

function THashBobJenkins.HashAsBytes: TBytes;
begin
  Result:=[];
  SetLength(Result,Sizeof(Cardinal));
  PCardinal(Result)^:=FCurrent;
end;

function THashBobJenkins.HashAsInteger: Integer;
begin
  Result:=FCurrent;
end;

function THashBobJenkins.HashAsString: UnicodeString;
begin
  Result:=HexStr(HashAsInteger,8);
end;

{ THashFNV1a32 }

class function THashFNV1a32.Create: THashFNV1a32;
begin
  Result.Reset;
end;

class function THashFNV1a32.GetHashBytes(const aData: UnicodeString): TBytes;

var
  C : Cardinal;

begin
  Result:=Default(TBytes);
  SetLength(Result,SizeOf(Fnv32_t));
  C:=Cardinal(GetHashValue(aData));
  PFnv32_t(@Result[0])^:=C;
end;

class function THashFNV1a32.GetHashString(const aData: UnicodeString): UnicodeString;
begin
  Result:=HexStr(FNV1_32a(Pointer(aData)^,Length(aData)*SizeOf(UnicodeChar),FNV_SEED),8);
end;

class function THashFNV1a32.GetHashString(const aData: RawByteString): UnicodeString;
begin
  Result:=HexStr(FNV1_32a(Pointer(aData)^,Length(aData),FNV_SEED),8);
end;

class function THashFNV1a32.GetHashValue(const aData: UnicodeString): Integer;

var
  C : Cardinal;

begin
  C:=FNV1_32a(PByte(aData),Length(aData)*SizeOf(UnicodeChar),FNV_SEED);
  Result:=Integer(C);
end;

class function THashFNV1a32.GetHashValue(const aData: RawByteString): Integer;
begin
  Result:=Integer(FNV1_32a(Pointer(aData)^,Length(aData), FNV_SEED));
end;

class function THashFNV1a32.GetHashValue(const aData; aLength: Cardinal; aInitialValue: Cardinal): Integer;
begin
  Result:=Integer(FNV1_32a(aData,aLength,aInitialValue));
end;

procedure THashFNV1a32.Reset(aInitialValue: Cardinal);
begin
  FCurrent:=aInitialValue;
end;

procedure THashFNV1a32.Update(const aData; aLength: Cardinal);
begin
  FCurrent:=FNV1_32a(aData,aLength,FCurrent);
end;

procedure THashFNV1a32.Update(const aData: TBytes; aLength: Cardinal);
begin
  if aLength=0 then
    aLength:=Length(aData);
  Update(aData[0],aLength);
end;

procedure THashFNV1a32.Update(const aData: UnicodeString);
begin
  FCurrent:=FNV1_32a(PByte(aData),Length(aData)*SizeOf(UnicodeChar),FCurrent);
end;

procedure THashFNV1a32.Update(const aData: RawByteString);
begin
  FCurrent:=FNV1_32a(PByte(aData),Length(aData)*SizeOf(AnsiChar),FCurrent);
end;

function THashFNV1a32.GetDigest: TBytes;
begin
  Result:=Default(TBytes);
  SetLength(Result,SizeOf(Fnv32_t));
  PFnv32_t(@Result[0])^:=FCurrent;
end;

function THashFNV1a32.HashAsBytes: TBytes;
begin
  Result:=GetDigest;
end;

function THashFNV1a32.HashAsInteger: Integer;
begin
  Result:=Integer(FCurrent);
end;

function THashFNV1a32.HashAsString: UnicodeString;
begin
  Result:=HexStr(FCurrent,8);
end;

end.
