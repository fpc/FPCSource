program WebmoduleCrash;

{$mode objfpc}

uses
  Classes, SysUtils;

type
  TGetActionEvent = Procedure (Sender : TObject) of object;
  TGetMethodProc=function(): TMethod of object;

type

{ TTestObject }

  TTestObject = class(TObject)
    function GetOnGetAction: TGetActionEvent;
    procedure DataModuleGetAction(Sender: TObject);
  end;

function TTestObject.GetOnGetAction: TGetActionEvent;
begin
  Result := @DataModuleGetAction;
end;

procedure TTestObject.DataModuleGetAction(Sender: TObject);
begin
  writeln('is');
end;

var AMethod : TMethod;
    ATestObject : TTestObject;

begin
  ATestObject := TTestObject.create;

// uncomment the next line and the exception wil occur on the line after the 'this' writeln,
// else the crash will occur in TTestObject.GetOnGetAction
  ATestObject.GetOnGetAction;

  AMethod := TGetMethodProc(@ATestObject.GetOnGetAction)();
  WriteLn('this');
  TGetActionEvent(AMethod)(nil);
  WriteLn('a test');

  ATestObject.Free;
end.
