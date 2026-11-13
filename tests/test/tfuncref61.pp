program tfuncref61;

{ a method assigned to a function reference inside an anonymous function, and a class
  method of a class reference parameter }

{$mode delphi}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

type
  TProcRef = reference to procedure(aSize: LongInt);
  TMaker = reference to function: TProcRef;
  TNameRef = reference to function: String;

  TSink = class
    Count: LongInt;
    procedure OnRef(aSize: LongInt);
    class function Name: String; virtual;
    function MakerOfSelf: TMaker;
    function MakerOfOther(aOther: TSink): TMaker;
  end;
  TSinkClass = class of TSink;

  TSub = class(TSink)
    class function Name: String; override;
  end;

var
  gFailed: LongInt = 0;

procedure TSink.OnRef(aSize: LongInt);
begin
  Inc(Count, aSize);
end;

class function TSink.Name: String;
begin
  Result := 'sink';
end;

class function TSub.Name: String;
begin
  Result := 'sub';
end;

function TSink.MakerOfSelf: TMaker;
begin
  Result := function: TProcRef
    begin
      Result := OnRef;
    end;
end;

function TSink.MakerOfOther(aOther: TSink): TMaker;
begin
  Result := function: TProcRef
    begin
      Result := aOther.OnRef;
    end;
end;

function MakerOf(aSink: TSink): TMaker;
begin
  Result := function: TProcRef
    var
      lSink: TSink;
    begin
      lSink := aSink;
      Result := lSink.OnRef;
    end;
end;

function NameOf(aClass: TSinkClass): TNameRef;
begin
  Result := aClass.Name;
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
  lA, lB: TSink;
  lMaker: TMaker;
  lRef: TProcRef;
  lSinkName, lSubName: TNameRef;
begin
  lA := TSink.Create;
  lB := TSink.Create;

  lMaker := lA.MakerOfSelf();
  lRef := lMaker();
  lMaker := nil;
  lRef(1);
  Check('in an anonymous function of a method: Self', lA.Count, 1);

  lMaker := lB.MakerOfOther(lA);
  lRef := lMaker();
  lMaker := nil;
  lRef(2);
  Check('in an anonymous function of a method: a parameter', lA.Count, 3);
  Check('in an anonymous function of a method: a parameter, Self', lB.Count, 0);

  lMaker := MakerOf(lA);
  lRef := lMaker();
  lMaker := nil;
  lRef(4);
  Check('in an anonymous function: its own local variable', lA.Count, 7);

  lSinkName := NameOf(TSink);
  lSubName := NameOf(TSub);
  if lSinkName() <> 'sink' then
    Check('a class reference parameter: the class', 0, 1);
  if lSubName() <> 'sub' then
    Check('a class reference parameter: its descendant', 0, 1);

  lRef := nil;
  lSinkName := nil;
  lSubName := nil;
  lB.Free;
  lA.Free;
  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
