{ overload selection with float if-expressions }
{$Mode Delphi}

var
  Called: Char;

procedure Fly(d: double); overload;
begin
  Called:='D';
end;

procedure Fly(s: single); overload;
begin
  Called:='S';
end;

var
  b: boolean;
  Expected: Char;
begin
  b:=true;
  Called:=' ';
  Fly(if b then single(1.5) else single(2.5));
  WriteLn(Called);
  if Called<>'S' then
    Halt(1);

  { the if-expression must choose the same overload as the plain constant }
  Called:=' ';
  Fly(1.5);
  Expected:=Called;
  Called:=' ';
  Fly(if b then 1.5 else 2.5);
  WriteLn(Called);
  if Called<>Expected then
    Halt(2);

  Called:=' ';
  Fly(if b then single(1.5) else 2);
  WriteLn(Called);
  if Called<>'S' then
    Halt(3);

  { with 64 bit float constants, like Delphi: double }
{$MINFPCONSTPREC 64}
  Called:=' ';
  Fly(if b then 1.5 else 2.5);
  WriteLn(Called);
  if Called<>'D' then
    Halt(4);

  Called:=' ';
  Fly(if b then single(1.5) else single(2.5));
  WriteLn(Called);
  if Called<>'S' then
    Halt(5);
end.
