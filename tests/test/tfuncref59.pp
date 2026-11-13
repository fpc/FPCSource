program tfuncref59;

{ two method pointers converted to function references in one routine are two references:
  each calls the method for its own object, and each keeps the value the method pointer
  had where it was converted }

{$mode objfpc}
{$modeswitch functionreferences}

type
  TProcRef = reference to procedure(aSize: LongInt);
  TMeth = procedure(aSize: LongInt) of object;

  TSink = class
    Count: LongInt;
    procedure OnRef(aSize: LongInt);
    procedure MakeTwoFromSelf(aOther: TSink; out aFirst, aSecond: TProcRef);
  end;

var
  gFailed: LongInt = 0;

procedure TSink.OnRef(aSize: LongInt);
begin
  Inc(Count, aSize);
end;

procedure TSink.MakeTwoFromSelf(aOther: TSink; out aFirst, aSecond: TProcRef);
begin
  aFirst := @OnRef;
  aSecond := @aOther.OnRef;
end;

function MakeTwo(aSinkA, aSinkB: TSink; out aSecond: TProcRef): TProcRef;
begin
  Result := @aSinkA.OnRef;
  aSecond := @aSinkB.OnRef;
end;

function MakeTwoFromVariable(aSinkA, aSinkB: TSink; out aSecond: TProcRef): TProcRef;
var
  lMeth: TMeth;
begin
  lMeth := @aSinkA.OnRef;
  Result := lMeth;
  lMeth := @aSinkB.OnRef;
  aSecond := lMeth;
end;

function MakeThenChange(aSinkA, aSinkB: TSink): TProcRef;
var
  lObj: TSink;
begin
  lObj := aSinkA;
  Result := @lObj.OnRef;
  lObj := aSinkB;
end;

procedure Check(const aRow: String; aA, aB: TSink; aWantA, aWantB: LongInt);
begin
  if (aA.Count <> aWantA) or (aB.Count <> aWantB) then
    begin
      Writeln(aRow, ': got ', aA.Count, ' and ', aB.Count, ', expected ', aWantA, ' and ', aWantB);
      Inc(gFailed);
    end;
  aA.Count := 0;
  aB.Count := 0;
end;

var
  lA, lB: TSink;
  lRef, lRef2: TProcRef;
begin
  lA := TSink.Create;
  lB := TSink.Create;

  lRef := MakeTwo(lA, lB, lRef2); lRef(1); lRef2(2);
  Check('two methods', lA, lB, 1, 2);

  lRef := MakeTwoFromVariable(lA, lB, lRef2); lRef(1); lRef2(2);
  Check('one method pointer variable at two places', lA, lB, 1, 2);

  lA.MakeTwoFromSelf(lB, lRef, lRef2); lRef(1); lRef2(2);
  Check('Self''s method and another object''s', lA, lB, 1, 2);

  lRef := MakeThenChange(lA, lB); lRef(1);
  Check('the object variable changed after the conversion', lA, lB, 1, 0);

  lRef := nil;
  lRef2 := nil;
  lB.Free;
  lA.Free;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
