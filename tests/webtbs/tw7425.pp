procedure a(r: double); overload;
begin
end;

procedure a(r: real); overload;
begin
end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure a(r: extended); overload;
begin
end;
{$endif}

begin
  writeln(sizeof(real));
end.
