{ demo_lazarus - Lazarus design-time OPF demo
  =============================================
  This demo shows how to use fcl-opf components at design time
  in the Lazarus IDE. Unlike the console demos, this is an LCL
  application with a data module holding TOPManager, TOPRegistry,
  and TOPSQLBuilder components wired via the Object Inspector.

  Open demo_lazarus.lpi in the Lazarus IDE (with fcl_opf and
  fcl_opf_design packages installed) and press F9, or compile
  from the command line:

    lazbuild demo_lazarus.lpi
}
program demo_lazarus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mainform,
  opfdatamodule;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TOPFDataModule, OPFDataModuleInstance);
  Application.CreateForm(TMainForm, MainFormInstance);
  Application.Run;
end.
