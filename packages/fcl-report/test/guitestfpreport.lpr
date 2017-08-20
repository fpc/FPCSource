program guitestfpreport;

{$mode objfpc}{$H+}

uses
  Classes
  ,Interfaces
  ,Forms
  ,GuiTestRunner
  ,regtests
  ;




procedure MainProc;
begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, TestRunner);
  Application.Run;
end;


begin
  MainProc;
end.
