{ Tests "is not" in various expression contexts }
program tisnot2;

{$mode objfpc}{$H+}

type
  TFoo = class(TObject)
  end;

  TBar = class(TFoo)
  end;

  generic TChecker<T> = class
    class function IsNotABar(Obj: TObject): boolean;
  end;

class function TChecker.IsNotABar(Obj: TObject): boolean;
begin
  Result:=Obj is not TBar;
end;

type
  TIntChecker = specialize TChecker<longint>;

var
  Obj: TObject;
  B: boolean;
  Count: longint;
begin
  Obj:=TFoo.Create;

  { plain assignment }
  B:=Obj is not TBar;
  if not B then
    halt(1);
  B:=Obj is not TFoo;
  if B then
    halt(2);

  { same result as the long form }
  if (Obj is not TBar)<>(not (Obj is TBar)) then
    halt(3);
  if (Obj is not TFoo)<>(not (Obj is TFoo)) then
    halt(4);

  { combined with "and"/"or", which bind tighter and thus need parentheses,
    exactly like for the plain "is" operator }
  if not ((Obj is not TBar) and (Obj is TFoo)) then
    halt(5);
  if (Obj is not TFoo) or (Obj is not TObject) then
    halt(6);

  { while condition }
  Count:=0;
  while (Obj is not TBar) and (Count<3) do
    inc(Count);
  if Count<>3 then
    halt(7);

  { repeat..until condition }
  Count:=0;
  repeat
    inc(Count);
  until Obj is not TBar;
  if Count<>1 then
    halt(8);

  { inside a generic, whose body is parsed from a replayed token stream }
  if not TIntChecker.IsNotABar(Obj) then
    halt(9);

  Obj.Free;
  writeln('ok');
end.
