program testmethodpointer;

{$mode objfpc}{$H+}

type
  TOnIdentifierFound = function(): integer of object;
  TTest=class
    OnIdentifierFound: TOnIdentifierFound;
    FoundProc: pointer;
    function testm():integer;
  end;
  TTest2=class
    function testmm(Params:TTest;var c,d,e:integer):boolean;
  end;

function TTest.testm():integer;
  begin

  end;

function TTest2.testmm(Params:TTest;var c,d,e:integer):boolean;
var k,l:integer;

  function testm2(Params1:TTest;var m,n:integer):boolean;
  var a,b:integer;
  begin
    if (Params.OnIdentifierFound<>@Params.testm) then halt(1);
    if (Params.FoundProc<>pointer($deadbeef)) then halt(1);
  end;

begin
  testm2(Params,k,l);
end;

var
  Test : TTest;
  Test2 : TTest2;
  c,d,e : integer;
begin
  Test:=TTest.Create;
  Test.OnIdentifierFound:=@Test.testm;
  Test.FoundProc:=pointer($deadbeef);
  Test2:=TTest2.Create;
  Test2.testmm(Test,c,d,e);
  Test.Free;
  Test2.Free;
  writeln('ok');
end.
