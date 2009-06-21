{ %opt=-gh }

program RefCountBug;

{$ifdef fpc}
  //{$mode objfpc}{$H+}
  {$mode delphi}
{$endif}

{$ifdef mswindows}
  {$apptype console}
{$endif}

uses
  Classes,
  SysUtils;

type
  ITest = interface
    function SomeMethod(): ITest;
    function GetValue(): AnsiString;
  end;

  TTest = class(TInterfacedObject, ITest)
  private
    fValue: AnsiString;
  public
    constructor Create(Value: AnsiString);
    destructor Destroy(); override;
    function SomeMethod(): ITest;
    function GetValue(): AnsiString;
  end;

constructor TTest.Create(Value: AnsiString);
begin
  inherited Create();
  fValue := Value;
  Writeln('TTest.Create('+Value+')');
end;

destructor TTest.Destroy();
begin
  Writeln('TTest.Destroy('+fValue+')');
  inherited;
end;

function TTest.SomeMethod(): ITest;
begin
  if (FRefCount <> 1) then
    halt(1);
  Writeln('SomeMethod: ' + fValue, ' ', FRefCount);
  Result := TTest.Create(fValue + ',MethodCall');
end;

function TTest.GetValue(): AnsiString;
begin
  Result := fValue;
end;

var
  t: ITest;
begin
  HaltOnNotReleased := true;
  t := TTest.Create('Create');
  Writeln('Result: ' + t.SomeMethod().SomeMethod().GetValue);
end.
