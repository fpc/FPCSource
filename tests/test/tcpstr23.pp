program tcpstr23;

{$MODE DELPHI}

type
  cp1251string = type AnsiString(1251);

// --- all string types ---
procedure test_overload1(const s: ShortString); overload;
begin
end;
procedure test_overload1(const s: UTF8String); overload;
begin
  halt(1);
end;
procedure test_overload1(const s: AnsiString); overload;
begin
  halt(1);
end;
procedure test_overload1(const s: cp1251string); overload;
begin
  halt(1);
end;
procedure test_overload1(const s: unicodestring); overload;
begin
  halt(1);
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure test_overload1(const s: widestring); overload;
begin
  halt(1);
end;
{$endif}
// --- no ShortString ---
procedure test_overload2(const s: UTF8String); overload;
begin
end;
procedure test_overload2(const s: AnsiString); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: cp1251string); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: unicodestring); overload;
begin
  halt(2);
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure test_overload2(const s: widestring); overload;
begin
  halt(2);
end;
{$endif}
// --- no ShortString, UTF8String ---
procedure test_overload3(const s: AnsiString); overload;
begin
end;
procedure test_overload3(const s: cp1251string); overload;
begin
  halt(3);
end;
procedure test_overload3(const s: unicodestring); overload;
begin
  halt(3);
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure test_overload3(const s: widestring); overload;
begin
  halt(3);
end;
{$endif}
// --- no ShortString, UTF8String, AnsiString ---
procedure test_overload4(const s: cp1251string); overload;
begin
end;
procedure test_overload4(const s: unicodestring); overload;
begin
  halt(4);
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure test_overload4(const s: widestring); overload;
begin
  halt(4);
end;
// --- no ShortString, UTF8String, AnsiString, AnsiString(codepage) ---
procedure test_overload5(const s: unicodestring); overload;
begin
end;
procedure test_overload5(const s: widestring); overload;
begin
  halt(5);
end;
{$endif}

var
  S: ShortString;
begin
  test_overload1(S);
  test_overload2(S);
  test_overload3(S);
  test_overload4(S);
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
  test_overload5(S);
{$endif}
end.
