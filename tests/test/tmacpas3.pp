{$mode macpas}

{Test of anonymous procedure parameters}

function AA1 (i: Integer; function BB (a: Integer): Integer ): Integer;
begin
  AA1:= BB(i);
end;

function CC1 (a: Integer): Integer;
begin
  Writeln('Inside CC1, param is ', a);
  CC1:= a+a;
end;

var
  res: Integer;

procedure AA2 (i: Integer; procedure BB (a: Integer));
begin
  BB(i);
end;

procedure CC2 (a: Integer);
begin
  Writeln('Inside CC2, param is ', a);
  res:= a+a;
end;

begin
  res:= 0;
  res:= AA1(42, CC1);
  if res <> 84 then
    Halt(1);

  AA2(27, CC2);
  if res <> 54 then
    Halt(1);
end.
