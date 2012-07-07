{ %fail}
program tcpstr21a;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
{$ENDIF}

// Test that ansistring types has the same overload precedence when passing an UnicodeString constant

procedure TestStrConst(const S: AnsiString); overload;
begin
  halt(1);
end;
procedure TestStrConst(const S: UTF8String); overload;
begin
  halt(1);
end;
begin
  TestStrConst('Test');
end.

