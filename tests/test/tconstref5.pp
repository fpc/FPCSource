{ Delphi's [Ref] parameter attribute passes the parameter by reference, as
  constref does: before or after const, on a list of names, on a parameter
  without const, in a procedural type and in a method; it is part of the
  signature }
program tconstref5;

{$mode delphi}

type
  TSmall = record
    X: Integer;
  end;

  TProcRef = procedure(const [Ref] a: TSmall);

  TC = class
    procedure M(const [Ref] a: TSmall);
  end;

var
  gS: TSmall;
  gI: Integer;
  gOk: Boolean;

procedure P1(const [Ref] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure P2([Ref] const a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure P3(const [Ref] a, b: TSmall; c: Integer);
begin
  gOk := (@a = @gS) and (@b = @gS) and (c = 3);
end;

procedure P4(const [ref] a: Integer);
begin
  gOk := @a = @gI;
end;

procedure P5([Ref] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure P6(var [Ref] a: TSmall);
begin
  gOk := @a = @gS;
end;

procedure TC.M(const [Ref] a: TSmall);
begin
  gOk := @a = @gS;
end;

var
  c: TC;
  p: TProcRef;
begin
  gS.X := 1;
  gI := 2;
  gOk := False;
  P1(gS);
  if not gOk then
    Halt(1);
  gOk := False;
  P2(gS);
  if not gOk then
    Halt(2);
  gOk := False;
  P3(gS, gS, 3);
  if not gOk then
    Halt(3);
  gOk := False;
  P4(gI);
  if not gOk then
    Halt(4);
  gOk := False;
  P5(gS);
  if not gOk then
    Halt(5);
  gOk := False;
  P6(gS);
  if not gOk then
    Halt(6);
  gOk := False;
  p := P1;
  p(gS);
  if not gOk then
    Halt(7);
  gOk := False;
  c := TC.Create;
  c.M(gS);
  c.Free;
  if not gOk then
    Halt(8);
  WriteLn('ok');
end.
