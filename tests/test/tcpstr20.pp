program tcpstr20;

{$APPTYPE CONSOLE}
{$MODE Delphi}

// Test checks that preferred string type arguments 
// for AnsiChar are: ShortString, AnsiString, UnicodeString, WideString
// for WideChar are: UnicodeString, WideString, AnsiString, ShortString

const
  AC = AnsiChar(13);
  WC = WideChar(13);

procedure Test(const I, Compare, ExitCode: Integer);
begin
  if I <> Compare then
  begin
    WriteLn(I, ' <> ', Compare);
    halt(ExitCode);
  end;
end;

{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
function OverAll(const S: WideString): Integer; overload;
begin
  Result := 1;
end;
{$endif}

function OverAll(const S: UnicodeString): Integer; overload;
begin
  Result := 2;
end;

function OverAll(const S: RawByteString): Integer; overload;
begin
  Result := 3;
end;

function OverAll(const S: ShortString): Integer; overload;
begin
  Result := 4;
end;

{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
function OverWide(const S: WideString): Integer; overload;
begin
  Result := 1;
end;
{$endif}

function OverWide(const S: UnicodeString): Integer; overload;
begin
  Result := 2;
end;

function OverNonWide(const S: RawByteString): Integer; overload;
begin
  Result := 3;
end;

function OverNonWide(const S: ShortString): Integer; overload;
begin
  Result := 4;
end;

{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
function OverAllNoUni(const S: WideString): Integer; overload;
begin
  Result := 1;
end;

function OverAllNoUni(const S: RawByteString): Integer; overload;
begin
  Result := 3;
end;

function OverAllNoUni(const S: ShortString): Integer; overload;
begin
  Result := 4;
end;
{$endif}

{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
function OverAllNoShort(const S: WideString): Integer; overload;
begin
  Result := 1;
end;
{$endif}

function OverAllNoShort(const S: UnicodeString): Integer; overload;
begin
  Result := 2;
end;

function OverAllNoShort(const S: RawByteString): Integer; overload;
begin
  Result := 3;
end;

begin
  Test(OverAll(AC), 4, 1);
  Test(OverAll(WC), 2, 2);
  Test(OverWide(AC), 2, 3);
  Test(OverNonWide(WC), 3, 4);
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
  Test(OverAllNoUni(WC), 1, 5);
  {$endif}
  Test(OverAllNoShort(AC), 3, 6);
end.
