program tcpstr22;

{$MODE DELPHI}

type
  cp1251string = type AnsiString(1251);

// --- all string types ---
procedure test_overload1(const s: AnsiString); overload;
begin
end;
procedure test_overload1(const s: UTF8String); overload;
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
procedure test_overload1(const s: widestring); overload;
begin
  halt(1);
end;
procedure test_overload1(const s: ShortString); overload;
begin
  halt(1);
end;
// --- no AnsiString ---
procedure test_overload2(const s: UTF8String); overload;
begin
end;
procedure test_overload2(const s: cp1251string); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: unicodestring); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: widestring); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: ShortString); overload;
begin
  halt(2);
end;
// --- no AnsiString, UTF8String ---
procedure test_overload3(const s: cp1251string); overload;
begin
end;
procedure test_overload3(const s: unicodestring); overload;
begin
  halt(3);
end;
procedure test_overload3(const s: widestring); overload;
begin
  halt(3);
end;
procedure test_overload3(const s: ShortString); overload;
begin
  halt(3);
end;
// --- no AnsiString, UTF8String, AnsiString(codepage) ---
procedure test_overload4(const s: unicodestring); overload;
begin
end;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
procedure test_overload4(const s: widestring); overload;
begin
  halt(4);
end;
{$endif}
procedure test_overload4(const s: ShortString); overload;
begin
  halt(4);
end;
// --- no AnsiString, UTF8String, AnsiString(codepage), UnicodeString ---
procedure test_overload5(const s: widestring); overload;
begin
end;
procedure test_overload5(const s: ShortString); overload;
begin
  halt(5);
end;

var
  A: AnsiString;
begin
  test_overload1(A);
  test_overload2(A);
  test_overload3(A);
  test_overload4(A);
  test_overload5(A);
end.
