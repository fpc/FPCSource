program dbtestframework_gui;

{$mode objfpc}{$H+}

// Note that this Lazarus project by default re-compiles all DB units! This eases
// developing, but requires some attention from the developer.
// It could very well be that after compiling this project, you have to manually clean
// the .ppu files before you can build fcl-db in the regular way. (Using fpmake)

// If you want to use the default installed db units, use the
// Default_no_local_ppus build mode which clears the search path in the compiler
// options.

uses
  Interfaces, Forms,
  // GUI:
  StdCtrls {to extend GuiTestRunner},
  DBGuiTestRunner, inieditor,
  // Generic DB test framework units
  ToolsUnit,
  // Connectors for different database types
  sqldbtoolsunit,
  dbftoolsunit,
  bufdatasettoolsunit,
  memdstoolsunit,
  SdfDSToolsUnit,
  tcsdfdata,
  // DB unittest
  TestBasics,
  TestDBBasics,
  TestFieldTypes,
  TestDatasources,
  TestBufDatasetStreams,
  TestSQLDB,
  TestSpecificTBufDataset,
  TestSpecificTDBF,
  TestSpecificTMemDataset,
  TestDBExport, tccsvdataset;

{$R *.res}

begin
  Application.Title:='DBTestFramework';
  Application.Initialize;
  Application.CreateForm(TDBGuiTestRunnerForm, DBGuiTestRunnerForm);
  Application.Run;
end.

