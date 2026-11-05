{ Prints 32 random bytes from the CNG random number generator. Based on issue #41246. }
program bcryptrandom;

{$mode objfpc}{$H+}

uses
  SysUtils, BCrypt;

// Return aCount random bytes, raise an exception if bcrypt fails
function GenerateRandomBytes(aCount: Integer): TBytes;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lStatus: NTSTATUS;

begin
  Result:=Nil;
  lAlg:=Nil;
  lStatus:=BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_RNG_ALGORITHM, Nil, 0);
  if not BCRYPT_SUCCESS(lStatus) then
    raise Exception.CreateFmt('BCryptOpenAlgorithmProvider failed: $%.8x', [lStatus]);
  try
    SetLength(Result, aCount);
    lStatus:=BCryptGenRandom(lAlg, PByte(Result), aCount, 0);
    if not BCRYPT_SUCCESS(lStatus) then
      raise Exception.CreateFmt('BCryptGenRandom failed: $%.8x', [lStatus]);
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


var
  lBytes: TBytes;
  I: Integer;

begin
  lBytes:=GenerateRandomBytes(32);
  Writeln('Random data:');
  for I:=0 to High(lBytes) do
    Write(IntToHex(lBytes[I], 2), ' ');
  Writeln;
end.
