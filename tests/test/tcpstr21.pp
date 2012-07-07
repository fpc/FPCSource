program tcpstr21;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
{$ENDIF}

// Test overload precedence for string constant when default string type is UnicodeString

// ---- all string types ----

procedure TestStrConst1(const S: UnicodeString); overload;
begin
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure TestStrConst1(const S: WideString); overload;
begin
  halt(1);
end;
{$endif}
procedure TestStrConst1(const S: PWideChar); overload;
begin
  halt(1);
end;
procedure TestStrConst1(const S: PAnsiChar); overload;
begin
  halt(1);
end;
procedure TestStrConst1(const S: AnsiString); overload;
begin
  halt(1);
end;
procedure TestStrConst1(const S: ShortString); overload;
begin
  halt(1);
end;
// ---- no UnicodeString ----
procedure TestStrConst2(const S: WideString); overload;
begin
end;
procedure TestStrConst2(const S: PWideChar); overload;
begin
  halt(2);
end;
procedure TestStrConst2(const S: PAnsiChar); overload;
begin
  halt(2);
end;
procedure TestStrConst2(const S: AnsiString); overload;
begin
  halt(2);
end;
procedure TestStrConst2(const S: ShortString); overload;
begin
  halt(2);
end;
// ---- no UnicodeString, WideString ----
procedure TestStrConst3(const S: PWideChar); overload;
begin
end;
procedure TestStrConst3(const S: PAnsiChar); overload;
begin
  halt(3);
end;
procedure TestStrConst3(const S: AnsiString); overload;
begin
  halt(3);
end;
procedure TestStrConst3(const S: ShortString); overload;
begin
  halt(3);
end;
// ---- no UnicodeString, WideString, PWideChar ----
procedure TestStrConst4(const S: PAnsiChar); overload;
begin
end;
procedure TestStrConst4(const S: AnsiString); overload;
begin
  halt(4);
end;
procedure TestStrConst4(const S: ShortString); overload;
begin
  halt(4);
end;
// ---- no UnicodeString, WideString, PWideChar, PAnsiChar ----
procedure TestStrConst5(const S: AnsiString); overload;
begin
end;
procedure TestStrConst5(const S: ShortString); overload;
begin
  halt(5);
end;
begin
  TestStrConst1('Test');
  TestStrConst2('Test');
  TestStrConst3('Test');
  TestStrConst4('Test');
  TestStrConst5('Test');
end.

