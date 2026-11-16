{ demo_film_library - Lazarus design-time OPF demo with Film Library domain
  ============================================================================
  Demonstrates fcl-opf with a multi-entity domain model including:
  - Many-to-many relationships (Film↔Director, Film↔Actor, Film↔Genre)
  - One-to-many relationships (Film→Loan)
  - Nullable fields, set types, auto-increment PKs

  Compile:  lazbuild demo_film_library.lpi
}
program demo_film_library;

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
