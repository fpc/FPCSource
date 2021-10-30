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

unit onetimepass;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils , basenenc, types, DateUtils;

const
  TOTP_Mod = 1000000;
  TOTP_KeyRegeneration = 30; // Time step for TOTP generation. Google Authenticator uses 30 seconds.

Type
  TRandomBytes = Procedure (aBytes : TByteDynArray);

function HOTPCalculateToken(const aSecret: AnsiString; const Counter: LongInt): LongInt;
function TOTPCalculateToken(const aSecret: AnsiString): LongInt;
function TOTPGenerateToken(const aSecret: AnsiString): LongInt;
function TOTPValidate(const aSecret: AnsiString; const Token: LongInt; const WindowSize: LongInt; var Counter: LongInt): Boolean;
Function TOTPSharedSecret(aRandom : TRandomBytes = Nil) : String;

implementation

uses sha1, hmac;

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

function TOTPCalculateToken(const aSecret: String): Longint;

begin
  Result:=HOTPCalculateToken(aSecret,-1);
end;

function HOTPCalculateToken(const aSecret: String; const Counter: Longint): Longint;

var
  Digest: TSHA1Digest;
  Key: UInt32;
  Offset: Longint;
  Part1, Part2, Part3, Part4: UInt32;
  SecretBinBuf: TBytes;
  STime: String;
  Time: Longint;

begin
  Time := Counter;
  if Time=-1 then
    Time := DateTimeToUnix(Now,False) div TOTP_KeyRegeneration;
  SecretBinBuf:=Base32.Decode(aSecret);
  STime:=Int64ToRawString(Time);
  Digest:=HMACSHA1Digest(TEncoding.UTF8.GetAnsiString(SecretBinBuf), STime);
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

Function TOTPSharedSecret(aRandom : TRandomBytes = Nil) : String;

var
  RandomKey: TByteDynArray;
  I : Integer;

begin
  RandomKey:=[];
  SetLength(RandomKey,10);
  if aRandom <> Nil then
    aRandom(RandomKey)
  else
    For I:=0 to 9 do
      RandomKey[I]:=Random(256);
  Result:=Base32.Encode(RandomKey);
end;

// @Secret Base32 encoded, @WindowSize=1
function TOTPValidate(const aSecret: String; const Token: LongInt; const WindowSize: LongInt; var Counter: LongInt): Boolean;
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

