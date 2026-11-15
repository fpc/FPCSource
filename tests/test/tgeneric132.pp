{ an inline specialization in mode Delphi is one operand wherever it stands
  in an expression: as the operand of "not" or of a unary minus, as the right
  operand of a binary operator, or followed by another operator }
program tgeneric132;

{$mode delphi}

type
  TBase<T: class> = class
  end;

  TFraming<T: class> = class(TBase<T>)
    function Emit: Boolean;
    function Val: Integer;
    class function Check(aP: TBase<T>): Integer; static;
  end;

  TPair<A, B> = record
    X: Integer;
  end;

  TUtil = class
    class function Twice<T>(aX: Integer): Integer; static;
    class function IsIt<T>(aX: Integer): Boolean; static;
    class function Cmp: Integer; overload; static;
    class function Cmp<T>: Integer; overload; static;
  end;

function TFraming<T>.Emit: Boolean;
begin
  Result := False;
end;

function TFraming<T>.Val: Integer;
begin
  Result := 7;
end;

class function TUtil.Twice<T>(aX: Integer): Integer;
begin
  Result := 2 * aX;
end;

class function TUtil.IsIt<T>(aX: Integer): Boolean;
begin
  Result := aX = 3;
end;

class function TUtil.Cmp: Integer;
begin
  Result := 2;
end;

class function TUtil.Cmp<T>: Integer;
begin
  Result := 5;
end;

{ returns the number of the first failing check, 0 if all pass }
class function TFraming<T>.Check(aP: TBase<T>): Integer;
var
  b: Boolean;
  i: Integer;
begin
  Result := 0;
  b := False;
  if not TFraming<T>(aP).Emit then
    b := True;
  if not b then
    Exit(1);
  b := True and TFraming<T>(aP).Emit;
  if b then
    Exit(2);
  b := TFraming<T>(aP).Emit or True;
  if not b then
    Exit(3);
  b := 7 = TFraming<T>(aP).Val;
  if not b then
    Exit(4);
  i := 1 + TFraming<T>(aP).Val;
  if i <> 8 then
    Exit(5);
  i := -TFraming<T>(aP).Val;
  if i <> -7 then
    Exit(6);
  i := TFraming<T>(aP).Val + 1;
  if i <> 8 then
    Exit(7);
  b := not TFraming<T>(aP).Emit and True;
  if not b then
    Exit(8);
  i := 2 * TFraming<T>(aP).Val;
  if i <> 14 then
    Exit(9);
  b := 6 < TFraming<T>(aP).Val;
  if not b then
    Exit(10);
  { "not" binds tighter than "=" }
  b := not TFraming<T>(aP).Emit = True;
  if not b then
    Exit(11);
end;

var
  f: TFraming<TObject>;
  p: TPair<Integer, string>;
  b: Boolean;
  i: Integer;
begin
  f := TFraming<TObject>.Create;
  i := TFraming<TObject>.Check(f);
  if i <> 0 then
    Halt(i);
  b := not TFraming<TObject>(f).Emit;
  if not b then
    Halt(21);
  i := 1 + TFraming<TObject>(f).Val;
  if i <> 8 then
    Halt(22);
  b := not TUtil.IsIt<Integer>(3);
  if b then
    Halt(23);
  i := 1 + TUtil.Twice<Integer>(3);
  if i <> 7 then
    Halt(24);
  b := TUtil.IsIt<Integer>(3) and True;
  if not b then
    Halt(25);
  i := TUtil.Twice<Integer>(3) + 1;
  if i <> 7 then
    Halt(26);
  p.X := 3;
  i := 1 + TPair<Integer, string>(p).X;
  if i <> 4 then
    Halt(27);
  i := 1 + SizeOf(TPair<TPair<Integer, string>, string>);
  if i <> 1 + SizeOf(Integer) then
    Halt(28);
  { a routine that also has a non-generic overload keeps its specialization
    as the leftmost operand }
  i := TUtil.Cmp<Integer>;
  if i <> 5 then
    Halt(32);
  f.Free;
  WriteLn('ok');
end.
