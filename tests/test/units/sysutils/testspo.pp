{$mode delphi}
program testspo;

{$APPTYPE CONSOLE}

uses
  SysUtils;

{$IFNDEF FPC}
type
  TStringArray = TArray<string>;
{$ENDIF}

procedure Test(TestString: String; const Expected: TStringArray; Options: TStringSplitOptions;
  UseChar: Boolean);
var
  sa: TStringArray;
  i: Integer;
begin
  Write('Testing "', TestString, '"...');

  {$IFDEF FPC}
  if UseChar then
    sa := TestString.Split(',', Options)
  else
  {$ENDIF}
    sa := TestString.Split([','], Options);

  if Length(sa) <> Length(Expected) then
  begin
    WriteLn('  -->  Length difference');
    Halt(1);
  end;
  for i := Low(sa) to High(sa) do
    if sa[i] <> Expected[i] then
    begin
      WriteLn('  --> Difference found at pos ', i, ': "', sa[i], '" vs "', Expected[i], '"');
      Halt(1);
    end;
  WriteLn('  --> ok');
end;

begin
  {$IFDEF FPC}
  WriteLn('Testing ExcludeLastEmpty with individual separator');
  WriteLn('--------------------------------------------------');
  Test('a,b,c', ['a', 'b', 'c'], TStringSplitOptions.ExcludeLastEmpty, true);
  Test('a,b,', ['a', 'b'], TStringSplitOptions.ExcludeLastEmpty, true);
  Test('a,,c', ['a', '', 'c'], TStringSplitOptions.ExcludeLastEmpty, true);
  Test(',b,c', ['', 'b', 'c'], TStringSplitOptions.ExcludeLastEmpty, true);
  Test('a,,', ['a', ''], TStringSplitOptions.ExcludeLastEmpty, true);
  Test(',b,', ['','b'], TStringSplitOptions.ExcludeLastEmpty, true);
  Test(',,', ['', ''], TStringSplitOptions.ExcludeLastEmpty, true);

  WriteLn;
  WriteLn('Testing ExcludeEmpty with individual separator');
  WriteLn('----------------------------------------------');
  Test('a,b,c', ['a', 'b', 'c'], TStringSplitOptions.ExcludeEmpty, true);
  Test('a,b,,', ['a', 'b'], TStringSplitOptions.ExcludeEmpty, true);
  Test('a,,c', ['a', 'c'], TStringSplitOptions.ExcludeEmpty, true);
  Test(',b,c', ['b', 'c'], TStringSplitOptions.ExcludeEmpty, true);
  Test('a,,', ['a'], TStringSplitOptions.ExcludeEmpty, true);
  Test(',b,', ['b'], TStringSplitOptions.ExcludeEmpty, true);
  Test(',,', [], TStringSplitOptions.ExcludeEmpty, true);
  {$ENDIF}

  WriteLn('Testing ExcludeLastEmpty with set separators');
  WriteLn('--------------------------------------------');
  Test('a,b,c', ['a', 'b', 'c'], TStringSplitOptions.ExcludeLastEmpty, false);
  Test('a,b,', ['a', 'b'], TStringSplitOptions.ExcludeLastEmpty, false);
  Test('a,,c', ['a', '', 'c'], TStringSplitOptions.ExcludeLastEmpty, false);
  Test(',b,c', ['', 'b', 'c'], TStringSplitOptions.ExcludeLastEmpty, false);
  Test('a,,', ['a', ''], TStringSplitOptions.ExcludeLastEmpty, false);
  Test(',b,', ['','b'], TStringSplitOptions.ExcludeLastEmpty, false);
  Test(',,', ['', ''], TStringSplitOptions.ExcludeLastEmpty, false);

  WriteLn;
  WriteLn('Testing ExcludeEmpty with set separators');
  WriteLn('----------------------------------------');
  Test('a,b,c', ['a', 'b', 'c'], TStringSplitOptions.ExcludeEmpty, false);
  Test('a,b,,', ['a', 'b'], TStringSplitOptions.ExcludeEmpty, false);
  Test('a,,c', ['a', 'c'], TStringSplitOptions.ExcludeEmpty, false);
  Test(',b,c', ['b', 'c'], TStringSplitOptions.ExcludeEmpty, false);
  Test('a,,', ['a'], TStringSplitOptions.ExcludeEmpty, false);
  Test(',b,', ['b'], TStringSplitOptions.ExcludeEmpty, false);
  Test(',,', [], TStringSplitOptions.ExcludeEmpty, false);

{$IFNDEF FPC}
  WriteLn;
  WriteLn('Press ENTER to quit...');
  ReadLn;
{$ENDIF}
end.

