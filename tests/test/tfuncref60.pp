program tfuncref60;

{ conversions of methods to function references that come from one macro have the same
  source position and are still separate conversions; function references without
  anonymous functions }

{$mode delphi}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions-}
{$macro on}

type
  TProcRef = reference to procedure(aSize: LongInt);

  TFirst = class
    Count: LongInt;
    procedure M(aSize: LongInt);
  end;

  TSecond = class
    Count: LongInt;
    procedure M(aSize: LongInt);
  end;

{$define CONVERT:=R := M}
{$define CONVERTOF:=R := aObj.M}

var
  gFirst: TFirst;
  gSecond: TSecond;
  gFailed: LongInt = 0;

procedure TFirst.M(aSize: LongInt);
begin
  Inc(Count, aSize);
end;

procedure TSecond.M(aSize: LongInt);
begin
  Inc(Count, aSize);
end;

procedure MakeGlobals(out aFirst, aSecond: TProcRef);
var
  R: TProcRef;
begin
  with gFirst do
    CONVERT;
  aFirst := R;
  with gSecond do
    CONVERT;
  aSecond := R;
end;

function MakeOf(aObj: TFirst): TProcRef; overload;
var
  R: TProcRef;
begin
  CONVERTOF;
  Result := R;
end;

function MakeOf(aObj: TSecond): TProcRef; overload;
var
  R: TProcRef;
begin
  CONVERTOF;
  Result := R;
end;

procedure MakeSame(aObj, aOther: TFirst; out aFirst, aSecond: TProcRef);
var
  R: TProcRef;
begin
  CONVERTOF;
  aFirst := R;
  with aOther do
    CONVERT;
  aSecond := R;
end;

procedure Check(const aRow: String; aGot, aWant: LongInt);
begin
  if aGot <> aWant then
    begin
      Writeln(aRow, ': got ', aGot, ', expected ', aWant);
      Inc(gFailed);
    end;
end;

var
  lA, lB: TProcRef;
  lOther: TFirst;
begin
  gFirst := TFirst.Create;
  gSecond := TSecond.Create;
  lOther := TFirst.Create;

  MakeGlobals(lA, lB); lA(1); lB(2);
  Check('two classes, global objects: first', gFirst.Count, 1);
  Check('two classes, global objects: second', gSecond.Count, 2);

  lA := MakeOf(gFirst); lB := MakeOf(gSecond); lA(4); lB(8);
  Check('two routines: first', gFirst.Count, 5);
  Check('two routines: second', gSecond.Count, 10);

  MakeSame(gFirst, lOther, lA, lB); lA(16); lB(32);
  Check('one class, two objects: first', gFirst.Count, 21);
  Check('one class, two objects: second', lOther.Count, 32);

  lA := nil;
  lB := nil;
  lOther.Free;
  gSecond.Free;
  gFirst.Free;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
