{
  This file is part of the Free Component Library.
  Copyright (c) 2021 by the Free Pascal team.

  HOTP and TOTP One-time password algorithms. Compatible with the Google Authenticator.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFNDEF FPC_DOTTEDUNITS}
unit onetimepass;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils , Fcl.BaseNEnc, System.Types, System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils , basenenc, types, DateUtils;
{$ENDIF FPC_DOTTEDUNITS}

const
  TOTP_Mod = 1000000;
  TOTP_KeyRegeneration = 30; // Time step for TOTP generation. Google Authenticator uses 30 seconds.
  // Default shared secret length in bytes (160 bits, RFC 6238 for HMAC-SHA1).
  TOTP_SecretLength = 20;
  // Minimum shared secret length in bytes (128 bits, RFC 4226 R6).
  TOTP_MinSecretLength = 16;

Type
  // Fills all bytes of aBytes with cryptographically secure random data.
  TRandomBytes = Procedure (aBytes : TByteDynArray);
  EOTPError = Class(Exception);

function HOTPCalculateToken(const aSecret: AnsiString; const Counter: LongInt): LongInt;
function TOTPCalculateToken(const aSecret: AnsiString): LongInt;
function TOTPGenerateToken(const aSecret: AnsiString): LongInt;
function TOTPValidate(const aSecret: AnsiString; const Token: LongInt; const WindowSize: LongInt; var Counter: LongInt): Boolean;
// Return a new Base32 encoded shared secret of aLength random bytes (at least TOTP_MinSecretLength).
// aRandom supplies the bytes; when it is Nil, fphashutils.GetRandomBytes is used.
// Raises EOTPError when aLength is too small or no random bytes can be obtained.
Function TOTPSharedSecret(aRandom : TRandomBytes = Nil; aLength : Integer = TOTP_SecretLength) : AnsiString;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Sha1, System.Hash.Hmac, System.Hash.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses sha1, hmac, fphashutils;
{$ENDIF FPC_DOTTEDUNITS}

resourcestring
  SErrSecretTooShort = 'Shared secret length %d is below the minimum of %d bytes';
  SErrNoRandomBytes = 'Cannot obtain random bytes for the shared secret';

// @Result[8]
Function Int64ToRawString(const Value: Int64) : AnsiString;

var
  B: array[0..7] of Byte;
  I: Int32;
begin
  PInt64(@B)^ := Value;
  Result:='';
  for I := 7 downto 0 do
    Result:=Result+AnsiChar(B[I]);
end;

function TOTPCalculateToken(const aSecret: AnsiString): Longint;

begin
  Result:=HOTPCalculateToken(aSecret,-1);
end;

function HOTPCalculateToken(const aSecret: AnsiString; const Counter: Longint): Longint;

var
  Digest: TSHA1Digest;
  Key: UInt32;
  Offset: Longint;
  Part1, Part2, Part3, Part4: UInt32;
  SecretBinBuf: TBytes;
  STime, SSecretBin: RawbyteString;
  Time: Longint;

begin
  Time := Counter;
  if Time=-1 then
    Time := DateTimeToUnix(Now,False) div TOTP_KeyRegeneration;
  SSecretBin:='';
  SecretBinBuf:=Base32.Decode(aSecret);
  STime:=Int64ToRawString(Time);
  SetLength(SSecretBin,length(SecretBinBuf));
  Move(SecretBinBuf[0],SSecretBin[1],length(SecretBinBuf));
  Digest:=HMACSHA1Digest(SSecretBin, STime);
  Offset := Digest[19] and $0F;
  Part1 := (Digest[Offset + 0] and $7F);
  Part2 := (Digest[Offset + 1] and $FF);
  Part3 := (Digest[Offset + 2] and $FF);
  Part4 := (Digest[Offset + 3] and $FF);
  Key := (Part1 shl 24) or (Part2 shl 16) or (Part3 shl 8) or Part4;
  Result := Key mod TOTP_Mod; // mod 1000000 in case of otpLength of 6 digits
end;

function TOTPGenerateToken(const aSecret: AnsiString): LongInt;
begin
  Result := HOTPCalculateToken(aSecret, -1);
end;

Function TOTPSharedSecret(aRandom : TRandomBytes = Nil; aLength : Integer = TOTP_SecretLength) : AnsiString;

var
  RandomKey: TByteDynArray;

begin
  if aLength<TOTP_MinSecretLength then
    Raise EOTPError.CreateFmt(SErrSecretTooShort,[aLength,TOTP_MinSecretLength]);
  RandomKey:=[];
  SetLength(RandomKey,aLength);
  if aRandom <> Nil then
    aRandom(RandomKey)
  else if (Pointer(GetRandomBytes)=Nil) or not GetRandomBytes(PByte(RandomKey),aLength) then
    Raise EOTPError.Create(SErrNoRandomBytes);
  Result:=Base32.Encode(RandomKey);
end;

// @Secret Base32 encoded, @WindowSize=1
function TOTPValidate(const aSecret: AnsiString; const Token: LongInt; const WindowSize: LongInt; var Counter: LongInt): Boolean;
var
  TimeStamp: Longint;
  UnixTime: Longint;
begin
  Result := False;
  UnixTime := DateTimeToUnix(Now,False);
  TimeStamp := UnixTime div TOTP_KeyRegeneration;
  Counter := Timestamp-WindowSize;
  while Counter <= TimeStamp+WindowSize do
  begin
    Result := HOTPCalculateToken(aSecret, Counter) = Token;
    if Result then
      Exit;
    Inc(Counter);
  end;
end;

end.

