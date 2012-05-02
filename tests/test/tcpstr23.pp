program tcpstr23;

{$MODE DELPHI}

type
  cp1253string = type AnsiString(1253);

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
procedure test_overload1(const s: cp1253string); overload;
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
// --- no ShortString ---
procedure test_overload2(const s: UTF8String); overload;
begin
end;
procedure test_overload2(const s: AnsiString); overload;
begin
  halt(2);
end;
procedure test_overload2(const s: cp1253string); overload;
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
// --- no ShortString, UTF8String ---
procedure test_overload3(const s: AnsiString); overload;
begin
end;
procedure test_overload3(const s: cp1253string); overload;
begin
  halt(3);
end;
procedure test_overload3(const s: unicodestring); overload;
begin
  halt(3);
end;
procedure test_overload3(const s: widestring); overload;
begin
  halt(3);
end;
// --- no ShortString, UTF8String, AnsiString ---
procedure test_overload4(const s: cp1253string); overload;
begin
end;
procedure test_overload4(const s: unicodestring); overload;
begin
  halt(4);
end;
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

var
  S: ShortString;
begin
  test_overload1(S);
  test_overload2(S);
  test_overload3(S);
  test_overload4(S);
  test_overload5(S);
end.
