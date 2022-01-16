program app_test_core;

{$H+}
{$inline on}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF !MSWINDOWS}

uses
  Classes, SysUtils;

const
  EMPTY_STRING: Char = #0;

  function StrToPChar(const Value: string): PChar; inline;
  begin
    if Pointer(Value) <> nil then
      StrToPChar := Pointer(Value)
    else
      StrToPChar := @EMPTY_STRING;
  end;

  procedure LogTextA(const TextPtr: PChar; const TextLen: Integer);
  var
    T: string;
  begin
    SetString(T, TextPtr, TextLen);
    writeln('"', T, '"');
  end;

  procedure LogTextB(const Text: string); inline;
  begin
    LogTextA(StrToPChar(Text), Length(Text));
  end;

begin
  LogTextB('');
end.
