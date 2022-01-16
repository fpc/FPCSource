program unittests_gui;

{$mode objfpc}{$H+}

uses
  Classes
  ,Interfaces
  ,Forms
  ,GuiTestRunner
  {$i testunits.inc}
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
