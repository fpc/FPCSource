{$ifdef fpc}
{$mode delphi}
{$endif}

procedure test(p: pansichar); overload;
begin
  writeln('pchar');
end;

procedure test(p: pwidechar); overload;
begin
  writeln('pwidechar');
  halt(1);
end;

begin
  test(#$1234);
end.
