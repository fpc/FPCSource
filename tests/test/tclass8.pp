{ %version=1.1 }

{$mode objfpc}

uses
  classes;

type

{$M+}
  TTestCaseTest = class(TObject)
  published
    procedure TestSetUp;
    procedure TestAsString;
  end;

procedure TTestCaseTest.TestSetup;
begin
  writeln('TestSetup');
end;

procedure TTestCaseTest.TestAsString;
begin
  writeln('TestAsString');
end;

function GetMethodNameTableAddress(AClass: TClass): Pointer;
type
  TMethodNameRec = packed record
    name : pshortstring;
    addr : pointer;
  end;

  TMethodNameTable = packed record
    count : dword;
    entries : packed array[0..0] of TMethodNameRec;
  end;

  pMethodNameTable =  ^TMethodNameTable;

var
  methodTable : pMethodNameTable;
  vmt: TClass;
begin
  vmt := aClass;
  if assigned(vmt) then
  begin
    methodTable := pMethodNameTable((Pointer(vmt) + vmtMethodTable)^);
    Result := methodTable;
  end;
end;

begin
  writeln('Address of TestSetUp : ',ptrint(TTestCaseTest.MethodAddress('TestSetUp')));
  writeln('Address of TestAsString : ',ptrint(TTestCaseTest.MethodAddress('TestAsString')));
  if not (Assigned(GetMethodNameTableAddress(TTestCaseTest))) then
    halt(1);
end.
