{$ifdef fpc}
{$mode delphiunicode}
{$endif}

procedure test(p: pansichar); overload;
begin
  writeln('pchar');
  halt(1);
end;

procedure test(p: pwidechar); overload;
begin
  writeln('pwidechar');
end;

begin
  test('abcdef');
end.
