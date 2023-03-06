{
  This file is part of the Free Component Library.

  Hash-based supporting HMAC-MD5 and HMAC-SHA-1.
  Copyright (c) 2013 by Silvio Clecio silvioprog@gmail.com

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFNDEF FPC_DOTTEDUNITS}
unit HMAC;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Hash.Md5, System.Hash.Sha1;
{$ELSE FPC_DOTTEDUNITS}
uses
  MD5, SHA1;
{$ENDIF FPC_DOTTEDUNITS}

type
  THMACMD5Digest = TMD5Digest;
  THMACSHA1Digest = TSHA1Digest;

function HMACMD5Digest(const AKey, AMessage: RawByteString): THMACMD5Digest;
function HMACMD5Print(const ADigest: THMACMD5Digest): string; inline;
function HMACMD5Match(const ADigest1, ADigest2: THMACMD5Digest): boolean; inline;
function HMACMD5(const AKey, AMessage: RawByteString): string; inline;

function HMACSHA1Digest(const AKey, AMessage: RawByteString): THMACSHA1Digest;
function HMACSHA1Print(const ADigest: THMACSHA1Digest): string; inline;
function HMACSHA1Match(const ADigest1, ADigest2: THMACSHA1Digest): boolean; inline;
function HMACSHA1(const AKey, AMessage: RawByteString): string; inline;

implementation

const
  MD5_BLOCK_SIZE = 64;
  MD5_BLOCK_COUNT = 16;
  SHA1_BLOCK_SIZE = 64;
  SHA1_BLOCK_COUNT = 20;

function MD5Raw(var ABuffer; const ABufferLength: PtrUInt): RawByteString;
var
  I: Byte;
  VDest: PAnsiChar;
  VDigest: TMD5Digest;
  VContext: TMD5Context;
begin
  MD5Init(VContext);
  MD5Update(VContext, ABuffer, ABufferLength);
  MD5Final(VContext, VDigest);
  SetLength(Result, MD5_BLOCK_COUNT);
  VDest := Pointer(Result);
  for I := 0 to MD5_BLOCK_COUNT - 1 do
  begin
    VDest^ := AnsiChar(VDigest[I]);
    Inc(VDest);
  end;
end;

function HMACMD5Digest(const AKey, AMessage: RawByteString): THMACMD5Digest;
var
  I: Byte;
  VLength: PtrUInt;
  PKey, POPad, PIPad: PAnsiChar;
  VKey, VOPad, VIPad: RawByteString;
begin
  VLength := Length(AKey);
  if VLength > MD5_BLOCK_SIZE then
  begin
    SetLength(VKey, MD5_BLOCK_SIZE);
    FillChar(Pointer(VKey)^, MD5_BLOCK_SIZE, #0);
    VKey := MD5Raw(Pointer(AKey)^, VLength) + VKey;
  end
  else
  begin
    SetLength(VKey, MD5_BLOCK_SIZE - VLength);
    FillChar(Pointer(VKey)^, MD5_BLOCK_SIZE - VLength, #0);
    VKey := AKey + VKey;
  end;
  SetLength(VOPad, MD5_BLOCK_SIZE);
  POPad := PAnsiChar(VOPad);
  FillChar(POPad^, MD5_BLOCK_SIZE, $5c);
  SetLength(VIPad, MD5_BLOCK_SIZE);
  PIPad := PAnsiChar(VIPad);
  FillChar(PIPad^, MD5_BLOCK_SIZE, $36);
  PKey := PAnsiChar(VKey);
  for I := 1 to VLength do
  begin
    POPad^ := AnsiChar(Ord(POPad^) xor Ord(PKey^));
    PIPad^ := AnsiChar(Ord(PIPad^) xor Ord(PKey^));
    Inc(POPad);
    Inc(PIPad);
    Inc(PKey);
  end;
  VIPad := VIPad + AMessage;
  Result := MD5String(VOPad + MD5Raw(Pointer(VIPad)^, Length(VIPad)));
end;

function HMACMD5Print(const ADigest: THMACMD5Digest): string;
begin
  Result := MD5Print(ADigest);
end;

function HMACMD5Match(const ADigest1, ADigest2: THMACMD5Digest): boolean;
begin
  Result := MD5Match(ADigest1, ADigest2);
end;

function HMACMD5(const AKey, AMessage: RawByteString): string;
begin
  Result := HMACMD5Print(HMACMD5Digest(AKey, AMessage));
end;

function SHA1Raw(const ABuffer; const ABufferLength: PtrUInt): RawByteString;
var
  I: Byte;
  VDest: PAnsiChar;
  VDigest: TSHA1Digest;
  VContext: TSHA1Context;
begin
  SHA1Init(VContext);
  SHA1Update(VContext, ABuffer, ABufferLength);
  SHA1Final(VContext, VDigest);
  SetLength(Result, SHA1_BLOCK_COUNT);
  VDest := Pointer(Result);
  for I := 0 to SHA1_BLOCK_COUNT - 1 do
  begin
    VDest^ := AnsiChar(VDigest[I]);
    Inc(VDest);
  end;
end;

function HMACSHA1Digest(const AKey, AMessage: RawByteString): THMACSHA1Digest;
var
  I: Byte;
  VLength: PtrUInt;
  PKey, POPad, PIPad: PAnsiChar;
  VKey, VOPad, VIPad: RawBytestring;
begin
  // Set up masking block from key.
  VLength := Length(AKey);
  if VLength > SHA1_BLOCK_SIZE then
  begin
    SetLength(VKey, SHA1_BLOCK_SIZE);
    FillChar(Pointer(VKey)^, SHA1_BLOCK_SIZE, #0);
    VKey := SHA1Raw(Pointer(AKey)^, VLength) + VKey;
  end
  else
  begin
    SetLength(VKey, SHA1_BLOCK_SIZE - VLength);
    FillChar(Pointer(VKey)^, SHA1_BLOCK_SIZE - VLength, #0);
    VKey := AKey + VKey; // VKEY now has length SHA1_BLOCK_SIZE
  end;
  PKey := PAnsiChar(VKey);
  // Padding blocks
  SetLength(VOPad, SHA1_BLOCK_SIZE);
  POPad := PAnsiChar(VOPad);
  FillChar(POPad^, SHA1_BLOCK_SIZE, $5c);
  SetLength(VIPad, SHA1_BLOCK_SIZE);
  PIPad := PAnsiChar(VIPad);
  FillChar(PIPad^, SHA1_BLOCK_SIZE, $36);
  for I := 1 to SHA1_BLOCK_SIZE do
  begin
    POPad^ := AnsiChar(Ord(POPad^) xor Ord(PKey^));
    PIPad^ := AnsiChar(Ord(PIPad^) xor Ord(PKey^));
    Inc(POPad);
    Inc(PIPad);
    Inc(PKey);
  end;
  VIPad := VIPad + AMessage;
  Result := SHA1String(VOPad + SHA1Raw(Pointer(VIPad)^, Length(VIPad)));
end;

function HMACSHA1Print(const ADigest: THMACSHA1Digest): string;
begin
  Result := SHA1Print(ADigest);
end;

function HMACSHA1Match(const ADigest1, ADigest2: THMACSHA1Digest): boolean;
begin
  Result := SHA1Match(ADigest1, ADigest2);
end;

function HMACSHA1(const AKey, AMessage: RawByteString): string;
begin
  Result := HMACSHA1Print(HMACSHA1Digest(AKey, AMessage));
end;

end.
