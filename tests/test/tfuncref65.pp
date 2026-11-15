program tfuncref65;

{ a method converted to a function reference captures the variables of its object
  expression, as in Delphi: a later change of the variable, of an index or of the
  object a function returns is seen, and the expression is evaluated at every call }

{$ifdef FPC}
{$mode delphi}
{$modeswitch functionreferences}
{$endif}

type
  TRef = reference to function: LongInt;

  TObj = class
    FId: LongInt;
    constructor Create(aId: LongInt);
    function M: LongInt;
  end;

  TRec = record
    FId: LongInt;
    function M: LongInt;
  end;

var
  gFailed: LongInt = 0;
  gArr: array[0..2] of TObj;
  gGetObjCalls: LongInt;
  gObj: TObj;

constructor TObj.Create(aId: LongInt);
begin
  FId := aId;
end;

function TObj.M: LongInt;
begin
  Result := FId;
end;

function TRec.M: LongInt;
begin
  Result := FId;
end;

procedure Check(const aRow: String; aGot, aWant: LongInt);
begin
  if aGot <> aWant then
    begin
      Writeln(aRow, ': got ', aGot, ', expected ', aWant);
      Inc(gFailed);
    end;
end;

function GetObj: TObj;
begin
  Inc(gGetObjCalls);
  Result := gObj;
end;

procedure TestLocal;
var
  lO: TObj;
  lRef: TRef;
begin
  lO := gArr[0];
  lRef := lO.M;
  lO := gArr[1];
  Check('local variable reassigned', lRef(), 11);
end;

procedure TestParam(aO: TObj);
var
  lRef: TRef;
begin
  lRef := aO.M;
  aO := gArr[2];
  Check('parameter reassigned', lRef(), 12);
end;

procedure TestRecord;
var
  lR: TRec;
  lRef: TRef;
begin
  lR.FId := 1;
  lRef := lR.M;
  lR.FId := 2;
  Check('record changed', lRef(), 2);
end;

procedure TestCall;
var
  lRef: TRef;
begin
  gGetObjCalls := 0;
  gObj := gArr[0];
  lRef := GetObj.M;
  Check('calls of the object function at the conversion', gGetObjCalls, 0);
  gObj := gArr[1];
  Check('first call, object function result changed', lRef(), 11);
  Check('second call, object function result changed', lRef(), 11);
  Check('calls of the object function after two calls', gGetObjCalls, 2);
end;

procedure TestIndex;
var
  i: LongInt;
  lRef: TRef;
begin
  i := 0;
  lRef := gArr[i].M;
  i := 2;
  Check('index changed', lRef(), 12);
end;

procedure TestLoop;
var
  i: LongInt;
  lRefs: array[0..1] of TRef;
begin
  i := 0;
  while i < 2 do
    begin
      lRefs[i] := gArr[i].M;
      Inc(i);
    end;
  Check('first reference made in a loop', lRefs[0](), 12);
  Check('second reference made in a loop', lRefs[1](), 12);
end;

begin
  gArr[0] := TObj.Create(10);
  gArr[1] := TObj.Create(11);
  gArr[2] := TObj.Create(12);
  TestLocal;
  TestParam(gArr[0]);
  TestRecord;
  TestCall;
  TestIndex;
  TestLoop;
  gArr[0].Free;
  gArr[1].Free;
  gArr[2].Free;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
