{$mode objfpc}
{$modeswitch arrayoperators}
procedure p1;
  var
    A: array of Integer;
    i: integer;
  begin
    A := [];
    A := A + A;
    A := Concat(A,[123456789]);
    A := A + [6];
    A := A + A;

    if A[0]<>123456789 then
      Halt(1);
    if A[High(A)]<>6 then
      Halt(1);
  end;

procedure p2;
  var
    A, B, C: array of Integer;

    i: integer;
  begin
    A := [];
    A := A + A + A;
    A := Concat(A,[123456789],[8]);
    A := A + [6] + A;
    A := A + A + A;
    B:=copy(A);
    C:=B+A;

    if C[0]<>123456789 then
      Halt(1);
    if C[High(C)]<>8 then
      Halt(1);
    if C[High(C)-1]<>123456789 then
      Halt(1);
  end;

begin
  p1;
  p2;
  writeln('ok');
end.

