program project1;
{$mode objfpc}{$H+}

uses Classes, SysUtils;

type IHelpSystem = interface(IInterface) end;
     THelpManager = class(TInterfacedObject, IHelpSystem) end;

var HelpManager : THelpManager = nil;
function GetHelpSystem(out H: IHelpSystem) : Integer;
begin
  if HelpManager = nil then HelpManager := THelpManager.Create; // if help manager is not created here, it works
  H := HelpManager;  // <-- remove this and it works
  result := 0;
end;

procedure FreeHelpSystem;
begin
  HelpManager := nil;
end;

var h : IHelpSystem;
begin
  GetHelpSystem(h);
  FreeHelpSystem;
end.

