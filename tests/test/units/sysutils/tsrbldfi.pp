program strbld_forin_tests;

{$mode Delphi}

uses
  SysUtils;

var
  c: Char;
  sb: TStringBuilder;
  i: Integer;

begin
  sb := TStringBuilder.Create;
  try
    sb.Append('Hello');
    sb.Append(' ');
    sb.Append('World!');
    sb.Append(' ');
    sb.Append('16052020');
    i := 0;
    for c in sb do
    begin
      writeln(c);
      if sb[i] <> c then halt(i);
      Inc(i);
    end;

    // test empty
    sb.clear;
    i := 0;
    for c in sb do
    begin
      writeln(c);
      if sb[i] <> c then halt(i);
      Inc(i);
    end;

    sb.Append('A');
    i := 0;
    for c in sb do
    begin
      writeln(c);
      if sb[i] <> c then halt(i);
      Inc(i);
    end;

  finally
    sb.Free;
  end;

end.