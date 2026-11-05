{ Creates and validates a UUID version 7 (RFC 9562) with random bits from CNG. Based on issue #41246. }
program bcryptuuid7;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, BCrypt;

// Return a version 7 UUID: 48-bit Unix time in milliseconds, then 74 random bits
function CreateUUIDv7: TGUID;

var
  lTime: QWord;
  lRandom: array[0..9] of Byte;
  lStatus: NTSTATUS;

begin
  lTime:=MilliSecondsBetween(NowUTC, UnixEpoch);
  lStatus:=BCryptGenRandom(Nil, @lRandom[0], SizeOf(lRandom), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if not BCRYPT_SUCCESS(lStatus) then
    raise Exception.CreateFmt('BCryptGenRandom failed: $%.8x', [lStatus]);
  Result.D1:=LongWord(lTime shr 16);
  Result.D2:=Word(lTime);
  Result.D3:=$7000 or ((lRandom[0] shl 8) or lRandom[1]) and $0FFF;
  Result.D4[0]:=$80 or (lRandom[2] and $3F);
  Move(lRandom[3], Result.D4[1], 7);
end;


// Return True if aGUID has version 7 and the RFC 9562 variant
function IsUUIDv7(const aGUID: TGUID): Boolean;

begin
  Result:=((aGUID.D3 shr 12)=7) and ((aGUID.D4[0] and $C0)=$80);
end;


var
  lUUID: TGUID;

begin
  lUUID:=CreateUUIDv7;
  Writeln('UUIDv7: ', GUIDToString(lUUID));
  Writeln('Valid:  ', IsUUIDv7(lUUID));
end.
