
{$mode objfpc}
unit ulib2a;

interface

type
  ITest=interface(IInterface)['{1C37883B-2909-4A74-A10B-D929D0443B1F}']
    procedure DoSomething;
  end;
  
implementation

// must be declared in implementation, so DoSomething is not global
type
  TObj=class(TInterfacedObject,ITest)
    procedure DoSomething;
  end;

// this is located at the start of .text section. If relocation offset is lost,
// calling DoSomething will likely transfer control here.  
procedure DoSomethingElse;
begin
  writeln('wrong!!!');
  halt(1);
end;

procedure TObj.DoSomething;
begin
  writeln('correct method called');
end;

var t: ITest;

initialization
  t := TObj.Create;
  t.DoSomething;
end.
