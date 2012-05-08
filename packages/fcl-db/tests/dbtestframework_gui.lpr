program dbtestframework_gui;

{$mode objfpc}{$H+}

// Note that this Lazarus project by default re-compiles all DB-units! This eases
// developing, but asks some attention from the developer.
// If you want to use the default, installed db-units, simply clear the search path
// in the compiler-options.
// It could also be that after compiling this project, you have to manually clean
// the .ppu files before you can build fcl-db in the regular way. (Using fpmake)

uses
  Interfaces, Forms, GuiTestRunner,
  // Generic DB-testframework units
  ToolsUnit,
  // Connecors for different database-types
  sqldbtoolsunit,
  dbftoolsunit,
  bufdatasettoolsunit,
  memdstoolsunit,
  SdfDSToolsUnit,
  // DB unittest
  testbasics,
  TestFieldTypes,
  TestDBBasics,
  TestDatasources,
  TestBufDatasetStreams,
  TestSpecificTBufDataset;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

