{$ifdef fpc}
{$mode delphi}
{$endif}

procedure test(p: pchar); overload;
begin
  writeln('pchar');
end;

procedure test(p: pwidechar); overload;
begin
  writeln('pwidechar');
  halt(1);
end;

begin
  test('a');
end.
