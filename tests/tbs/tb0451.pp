{ %version=1.1 }

{$mode delphi}

{ taken from fpc-devel mailing list, posted by }
{ "Morten Juel Skovrup" <ms@mek.dtu.dk>        }
program tb0451;

procedure error(l : longint);
  begin
    writeln('Error: ',l);
    halt(1);
  end;

type
  TDoubleArray = array of Double;
  TTestProp =
    record
      TestItem : Double;
    end;
  TTestPropArray = array of TTestProp;

  TTestClass =
    class
      private
        FTestProp: TTestPropArray;
      public
        constructor Create;
        destructor Destroy; override;
        property TestProp : TTestPropArray read FTestProp;
    end;

procedure Init(var AnArray : array of Double);
var
  i : Integer;
begin
  for i:=0 to High(AnArray) do
    AnArray[i] := 1;
end;

var
  Test      : TDoubleArray;
  i         : Integer;
  TestClass : TTestClass;

constructor TTestClass.Create;
begin
  inherited Create;
  SetLength(FTestProp,2);
end;

destructor TTestClass.Destroy;
begin
  Finalize(FTestProp);
  inherited Destroy;
end;

begin
  SetLength(Test,5);
  Init(Test);                       //!!! FPC compile error - Delphi compiles fine...
  for i:=0 to High(Test) do
    if test[i]<>1 then
      error(1);
  Finalize(Test);

  TestClass := TTestClass.Create;
  with TestClass.TestProp[1] do     //!!! FPC stops with runtime-error 201
    TestItem := 2;
  if TestClass.TestProp[0].TestItem<>0 then
    error(2);
  if TestClass.TestProp[1].TestItem<>2 then
    error(3);
  TestClass.Free;
end.
