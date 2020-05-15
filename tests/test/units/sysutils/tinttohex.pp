program tinttohex;

{$mode Delphi}

uses
  SysUtils;

var
  i8: Int8;
  u8: UInt8;
  i16: Int16;
  u32: UInt32;
  u64: Uint64;
  i: Integer;
  s: AnsiString;

begin
  i8 := 15;
  s := IntToHex(i8);
  writeln(s);
  if s <> '0F' then halt(1);

  u8 := 224;
  s := IntToHex(u8);
  writeln(s);
  if s <> 'E0' then halt(2);

  i16 := 224;
  s := IntToHex(i16);
  writeln(s);
  if s <> '00E0' then halt(3);

  u32 := 224;
  s := IntToHex(u32);
  writeln(s);
  if s <> '000000E0' then halt(4);

  u64 := 224;
  s := IntToHex(u64);
  writeln(s);
  if s <> '00000000000000E0' then halt(5);

  i := 224;
  s := IntToHex(i);
  writeln(s);
  if s <> '000000E0' then halt(6);

  s := i8.ToHexString;
  writeln(s);
  if s <> '0F' then halt(7);

  s := u8.ToHexString;
  writeln(s);
  if s <> 'E0' then halt(8);

  s := i16.ToHexString;
  writeln(s);
  if s <> '00E0' then halt(9);

  s := u32.ToHexString;
  writeln(s);
  if s <> '000000E0' then halt(10);

  s := u64.ToHexString;
  writeln(s);
  if s <> '00000000000000E0' then halt(11);

  s := i.ToHexString;
  writeln(s);
  if s <> '000000E0' then halt(12);

  writeln('ok');
  //readln;
end.
