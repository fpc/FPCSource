program guirunner;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, main;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, GuiTestRunner);
  Application.Run;
end.

