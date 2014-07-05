program testsqlscanner_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  tcsqlscanner,
  fpsqltree, fpsqlscanner, fpsqlparser,
  tcparser, tcgensql;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

