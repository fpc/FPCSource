{%OPT=-gh}
program tdel1;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}
{ A test for correct refcounting when using different methods of casting
  object to delegated COM interface. The requirement is no memleaks.
  Delphi output: 3, 4, 3, 3, 3, 3
  FPC output:    3, 4, 4, 4, 3, 3
 }

const
  STestInterface = '{3FB19775-F5FA-464C-B10C-D8137D742088}';

type
  ITest = interface[STestInterface]
    procedure DoSomething;
  end;
  
  TImpl=class(TInterfacedObject,ITest)
    procedure DoSomething;
  end;

  TC1=class(TInterfacedObject,ITest)
  private
    FImpl: ITest;
  public
    constructor Create;
    property impl: ITest read FImpl implements ITest;
  end;

  TC2=class(TInterfacedObject,ITest)
  private
    FImpl: ITest;
    function GetImpl: ITest;
  public
    constructor Create;
    property impl: ITest read GetImpl implements ITest;
  end;

procedure TImpl.DoSomething;
begin
  writeln('Doing something');
end;

function TC2.GetImpl: ITest;
begin
  result:=FImpl;
end;

constructor TC1.Create;
begin
  FImpl := TImpl.Create;
end;

constructor TC2.Create;
begin
  FImpl := TImpl.Create;
end;

var
  C1: TC1;
  C2: TC2;
  I: ITest;
  ref: Integer;

begin
  C1 := TC1.Create;
  C2 := TC2.Create;
  writeln('Testing typecasting...');
  
  I := ITest(C1);
  ref := I._Addref;
  I._Release;
  writeln('When delegating by field, refcount=', ref);
  
  I := ITest(C2);
  ref := I._Addref;
  I._Release;
  writeln('When delegating by function, refcount=', ref);
  {clean up}
  I := nil;
  C1.Free;
  C2.Free;
  
  writeln('Testing ''as'' operator...');
  C1 := TC1.Create;
  C2 := TC2.Create;
  
  I := C1 as ITest;
  ref := I._Addref;
  I._Release;
  writeln('When delegating by field, refcount=', ref);
  
  I := C2 as ITest;
  ref := I._Addref;
  I._Release;
  writeln('When delegating by function, refcount=', ref);
  {clean up}
  I := nil;
  C1.Free;
  C2.Free;

  writeln('Testing GetInteface()...');
  C1 := TC1.Create;
  C2 := TC2.Create;
  
  C1.GetInterface(ITest, I);
  ref := I._Addref;
  I._Release;
  writeln('When delegating by field, refcount=', ref);
  
  C2.GetInterface(ITest, I);
  ref := I._Addref;
  I._Release;
  writeln('When delegating by function, refcount=', ref);

  {clean up}
  I := nil;
  C1.Free;
  C2.Free;
  
end.
