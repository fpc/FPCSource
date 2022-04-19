program tw39664;

{$mode Delphi}

uses
  SysUtils;

var
  i8: Int8;
  i16: Int16;
  i32: Int32;
  u32: UInt32;
  s: String;

begin
  i8 := -42;
  s := IntToHex(i8);
  writeln(s);
  if s <> 'D6' then halt(1);

  i16 := -42;
  s := IntToHex(i16);
  writeln(s);
  if s <> 'FFD6' then halt(2);

  i32 := -42;
  s := IntToHex(i32);
  writeln(s);
  if s <> 'FFFFFFD6' then halt(3);

  s := i8.ToHexString;
  writeln(s);
  if s <> 'D6' then halt(4);

  s := i16.ToHexString;
  writeln(s);
  if s <> 'FFD6' then halt(5);

  s := i32.ToHexString;
  writeln(s);
  if s <> 'FFFFFFD6' then halt(6);
end.

