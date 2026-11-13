program tfuncref57;

{ a method of an object that is a parameter or a local variable of a routine, assigned to a
  function reference in that routine: the reference keeps working after the routine
  returned, and it calls the method for what the variable holds at the time of the call }

{$mode delphi}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

type
  TProcRef = reference to procedure(aSize: LongInt);
  TFuncRef = reference to function(aSize: LongInt): LongInt;
  TMeth = procedure(aSize: LongInt) of object;

  TSink = class
    Count: LongInt;
    Tag: LongInt;
    Items: array[0..1] of TSink;
    procedure OnRef(aSize: LongInt);
    function OnFunc(aSize: LongInt): LongInt;
    function MakeFromSelf: TProcRef;
    function MakeFromSelfImplicit: TProcRef;
    function MakeFromItem(aIndex: LongInt): TProcRef;
    function MakeFromOther(aOther: TSink): TProcRef;
  end;

  THolder = class
    Ref: TProcRef;
  end;

var
  gRef: TProcRef;
  gFailed: LongInt = 0;

procedure TSink.OnRef(aSize: LongInt);
begin
  Inc(Count, aSize);
end;

function TSink.OnFunc(aSize: LongInt): LongInt;
begin
  Inc(Count, aSize);
  Result := Tag + aSize;
end;

function TSink.MakeFromSelf: TProcRef;
begin
  Result := Self.OnRef;
end;

function TSink.MakeFromSelfImplicit: TProcRef;
begin
  Result := OnRef;
end;

function TSink.MakeFromItem(aIndex: LongInt): TProcRef;
begin
  { needs both Self and a parameter }
  Result := Items[aIndex].OnRef;
end;

function TSink.MakeFromOther(aOther: TSink): TProcRef;
begin
  { a method of this class, but not for Self }
  Result := aOther.OnRef;
end;

function MakeResult(aSink: TSink): TProcRef;
begin
  Result := aSink.OnRef;
end;

function MakeFunc(aSink: TSink): TFuncRef;
begin
  Result := aSink.OnFunc;
end;

procedure StoreField(aHolder: THolder; aSink: TSink);
begin
  aHolder.Ref := aSink.OnRef;
end;

procedure StoreGlobal(aSink: TSink);
begin
  gRef := aSink.OnRef;
end;

procedure StoreVar(var aRef: TProcRef; aSink: TSink);
begin
  aRef := aSink.OnRef;
end;

procedure StoreOut(out aRef: TProcRef; aSink: TSink);
begin
  aRef := aSink.OnRef;
end;

function MakeViaLocal(aSink: TSink): TProcRef;
var
  lRef: TProcRef;
  lSink: TSink;
begin
  lSink := aSink;
  lRef := lSink.OnRef;
  Result := lRef;
end;

function MakeConst(const aSink: TSink): TProcRef;
begin
  Result := aSink.OnRef;
end;

function MakeTwo(aSinkA, aSinkB: TSink; out aSecond: TProcRef): TProcRef;
begin
  Result := aSinkA.OnRef;
  aSecond := aSinkB.OnRef;
end;

function MakeFromMethodPointer(aSinkA, aSinkB: TSink; out aSecond: TProcRef): TProcRef;
var
  lMeth: TMeth;
begin
  { the method pointer's value at each place }
  lMeth := aSinkB.OnRef;
  aSecond := lMeth;
  lMeth := aSinkA.OnRef;
  Result := lMeth;
end;

function MakeNested(aSink: TSink): TProcRef;

  function Inner: TProcRef;
  begin
    Result := aSink.OnRef;
  end;

begin
  Result := Inner();
end;

function MakeInNested(aSink: TSink): TProcRef;

  function Inner(aInner: TSink): TProcRef;
  begin
    Result := aInner.OnRef;
  end;

begin
  Result := Inner(aSink);
end;

function MakeBeside(aSink, aSinkB: TSink; out aSecond: TProcRef): TProcRef;
begin
  { beside an anonymous function that captures another parameter }
  aSecond := procedure(aSize: LongInt)
    begin
      Inc(aSinkB.Count, aSize);
    end;
  Result := aSink.OnRef;
end;

type
  TRec = record
    Sink: TSink;
  end;

var
  gPicks: LongInt = 0;

function Pick(aSink: TSink): TSink;
begin
  Inc(gPicks);
  Result := aSink;
end;

function MakeWith(aSink: TSink): TProcRef;
begin
  with aSink do
    Result := OnRef;
end;

function MakeCall(aSink: TSink): TProcRef;
begin
  { the expression is the reference's: evaluated at each call of it }
  Result := Pick(aSink).OnRef;
end;

function MakeRecord(aSink: TSink): TProcRef;
var
  lRec: TRec;
begin
  lRec.Sink := aSink;
  Result := lRec.Sink.OnRef;
end;

function MakeElement(aSink: TSink): TProcRef;
var
  lArr: array[0..1] of TSink;
begin
  lArr[0] := nil;
  lArr[1] := aSink;
  Result := lArr[1].OnRef;
end;

function MakeCast(aObj: TObject): TProcRef;
begin
  Result := TSink(aObj).OnRef;
end;

procedure Smash;
var
  lFill: array[0..255] of PtrInt;
  lIdx: LongInt;
begin
  { overwrite the stack the routines above used }
  for lIdx := 0 to High(lFill) do
    lFill[lIdx] := -1;
  if lFill[3] <> -1 then
    Halt(3);
end;

procedure Check(const aRow: String; aGot, aWant: LongInt);
begin
  if aGot <> aWant then
    begin
      Writeln(aRow, ': got ', aGot, ', expected ', aWant);
      Inc(gFailed);
    end;
end;

procedure Run;
var
  lSink, lSinkB, lObj: TSink;
  lRef, lRef2: TProcRef;
  lFunc: TFuncRef;
  lHolder: THolder;
begin
  lSink := TSink.Create;
  lSinkB := TSink.Create;
  lHolder := THolder.Create;

  lRef := MakeResult(lSink); Smash; lRef(1);
  Check('function result', lSink.Count, 1);

  lSink.Tag := 40;
  lFunc := MakeFunc(lSink); Smash;
  Check('function reference with a result: value', lFunc(2), 42);
  Check('function reference with a result', lSink.Count, 3);

  StoreField(lHolder, lSink); Smash; lHolder.Ref(4);
  Check('field', lSink.Count, 7);

  StoreGlobal(lSink); Smash; gRef(8);
  Check('global', lSink.Count, 15);

  StoreVar(lRef, lSink); Smash; lRef(16);
  Check('var parameter', lSink.Count, 31);

  StoreOut(lRef, lSink); Smash; lRef(32);
  Check('out parameter', lSink.Count, 63);

  lRef := MakeViaLocal(lSink); Smash; lRef(64);
  Check('a local variable''s object, through a local reference', lSink.Count, 127);

  lRef := MakeConst(lSink); Smash; lRef(128);
  Check('const parameter', lSink.Count, 255);

  lRef := lSink.MakeFromSelf(); Smash; lRef(256);
  Check('Self.Method', lSink.Count, 511);

  lRef := lSink.MakeFromSelfImplicit(); Smash; lRef(512);
  Check('Method', lSink.Count, 1023);

  lSinkB.Items[1] := lSink;
  lRef := lSinkB.MakeFromItem(1); Smash; lRef(1024);
  Check('Self''s field and a parameter', lSink.Count, 2047);
  Check('Self''s field and a parameter: Self', lSinkB.Count, 0);

  lRef := lSinkB.MakeFromOther(lSink); Smash; lRef(2048);
  Check('a method of the class for another instance', lSink.Count, 4095);
  Check('a method of the class for another instance: Self', lSinkB.Count, 0);

  lRef := MakeTwo(lSink, lSinkB, lRef2); Smash; lRef(4096); lRef2(1);
  Check('two in one routine: first', lSink.Count, 8191);
  Check('two in one routine: second', lSinkB.Count, 1);

  lRef := MakeFromMethodPointer(lSink, lSinkB, lRef2); Smash; lRef(8192); lRef2(2);
  Check('a method pointer variable at two places: first', lSink.Count, 16383);
  Check('a method pointer variable at two places: second', lSinkB.Count, 3);

  lRef := MakeNested(lSink); Smash; lRef(16384);
  Check('in a nested routine, the outer routine''s parameter', lSink.Count, 32767);

  lRef := MakeInNested(lSink); Smash; lRef(32768);
  Check('in a nested routine, its own parameter', lSink.Count, 65535);

  lRef := MakeBeside(lSink, lSinkB, lRef2); Smash; lRef(65536); lRef2(4);
  Check('beside an anonymous function: the method', lSink.Count, 131071);
  Check('beside an anonymous function: the anonymous function', lSinkB.Count, 7);

  { in the routine that calls it; the variable, not its value, is what the reference holds }
  lObj := lSink;
  lRef := lObj.OnRef;
  lRef(131072);
  Check('called in the same routine', lSink.Count, 262143);
  lObj := lSinkB;
  lRef(8);
  Check('the variable changed: the old object', lSink.Count, 262143);
  Check('the variable changed: the new object', lSinkB.Count, 15);

  lSink.Count := 0;
  lRef := MakeWith(lSink); Smash; lRef(1);
  Check('with', lSink.Count, 1);

  lRef := MakeCall(lSink); Smash;
  Check('a function call''s result: not called at the conversion', gPicks, 0);
  lRef(2); lRef(4);
  Check('a function call''s result', lSink.Count, 7);
  Check('a function call''s result: called at each call', gPicks, 2);

  lRef := MakeRecord(lSink); Smash; lRef(8);
  Check('a local record''s field', lSink.Count, 15);

  lRef := MakeElement(lSink); Smash; lRef(16);
  Check('a local array''s element', lSink.Count, 31);

  lRef := MakeCast(lSink); Smash; lRef(32);
  Check('a typecast parameter', lSink.Count, 63);

  lRef := nil;
  lRef2 := nil;
  lFunc := nil;
  gRef := nil;
  lHolder.Free;
  lSinkB.Free;
  lSink.Free;
end;

begin
  Run;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
