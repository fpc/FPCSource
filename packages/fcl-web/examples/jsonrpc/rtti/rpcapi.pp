unit rpcapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprpcrtti, myapi;

Type

  { TIntfImpl }

  TIntfImpl = class(TInterfacedObject, IMyInterface)
  public
    procedure SayHello;
    Function Echo(args : Array of string) : String;
    function DoSum(a,b : Integer) : integer;
    function Split(aLine,aSep : string) : TStringArray;
    function DoVarTest(var aArg: String): Boolean;
  end;

  { TIntf2Impl }

  TIntf2Impl = class(TInterfacedObject, IMyOtherInterface)

  public
    function Echo(args: TStringArray): String;
    function SayHello: string;
  end;


Implementation

{ TIntf2Impl }

function TIntf2Impl.Echo(args: TStringArray): String;

var
  S : String;

begin
  Result:='';
  For S in Args do
    begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+S;
    end
end;

function TIntf2Impl.SayHello: string;
begin
  Result:='Hello, World!';
end;

procedure TIntfImpl.SayHello;
begin
  Writeln('Hello, World!');
end;

function TIntfImpl.Echo(args: array of string): String;

var
  S : String;

begin
  Result:='';
  For S in Args do
    begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+S;
    end
end;

function TIntfImpl.DoSum(a,b : Integer) : integer;
begin
  Result := a + b;
end;

function TIntfImpl.Split(aLine,aSep : string) : TStringArray;
begin
  Result := aLine.Split(aSep);
end;

function TIntfImpl.DoVarTest(var aArg: String): Boolean;
begin
  if aArg = 'Test' then begin
    aArg := 'Foo';
    Result := True;
  end else
    Result := False;
end;

Function GetMyInterface(Const aName : string) : IInterface;

begin
  Result:=TIntfImpl.Create as IInterface;
end;

Function GetMyOtherInterface(Const aName : string) : IInterface;

begin
  Result:=TIntf2Impl.Create as IInterface;
end;

initialization
  RTTIJSONRPCRegistry.Add(TypeInfo(IMyInterface),@GetMyInterface);
  RTTIJSONRPCRegistry.Add(TypeInfo(IMyOtherInterface),@GetMyOtherInterface,'Service2');
end.

