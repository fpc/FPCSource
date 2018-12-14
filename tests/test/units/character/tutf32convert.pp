program tutf32convert;

{$mode objfpc}

{$apptype console}

uses
  SysUtils, Character;

var
  U4: UCS4Char;
  U2: UnicodeString;
begin
  U4 := $1D52D;
  U2 := TCharacter.ConvertFromUtf32(U4);
  if not TCharacter.IsHighSurrogate(U2[1]) then
    halt(1);
  if not TCharacter.IsLowSurrogate(U2[2]) then
    halt(2);
  if TCharacter.ConvertToUtf32(U2, 1) <> U4 then
    halt(3);
  SetLength(U2, 1);
  try
    TCharacter.ConvertToUtf32(U2, 1);
    halt(4);
  except
    on E: EArgumentException do
      WriteLn(E.Message);
    on Exception do
      halt(5);
  end;
  SetLength(U2, 0);
  try
    TCharacter.ConvertToUtf32(U2, 1);
    halt(6);
  except
    on E: EArgumentOutOfRangeException do
      WriteLn(E.Message);
    on Exception do
      halt(7);
  end;
  try
    TCharacter.ConvertToUtf32(#1, #2);
    halt(8);
  except
    on E: EArgumentOutOfRangeException do
      WriteLn(E.Message);
    on Exception do
      halt(9);
  end;
  try
    TCharacter.ConvertToUtf32(#$D800, #2);
    halt(10);
  except
    on E: EArgumentOutOfRangeException do
      WriteLn(E.Message);
    on Exception do
      halt(11);
  end;
  WriteLn('ok');
end.
