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
  GuiTestRunner, inieditor,
  // Generic DB test framework units
  ToolsUnit,
  // Connectors for different database-types
  sqldbtoolsunit,
  dbftoolsunit,
  bufdatasettoolsunit,
  memdstoolsunit,
  SdfDSToolsUnit,
  tcsdfdata,
  // DB unittest
  testbasics,
  TestFieldTypes,
  TestDBBasics,
  TestDatasources,
  TestBufDatasetStreams,
  TestSpecificTBufDataset,
  TestSpecificTDBF,
  TestDBExport;

{$R *.res}

var
  DBSelectForm: TFormIniEditor;
  TestRunForm: TGUITestRunner;
begin
  Application.Initialize;
  DBSelectForm:=TFormIniEditor.Create(nil);
  try
    DBSelectForm.INIFile:='database.ini';
    DBSelectForm.ProfileSelectSection:='Database';
    DBSelectForm.ProfileSelectKey:='type';
    // We can ignore resulting db selection as the file is saved already:
    DBSelectForm.ShowModal;
  finally
    DBSelectForm.Free;
  end;
  // Manually run this form because autocreation could have loaded an old
  // database.ini file (if the user changed it using DBSelectForm)
  TestRunForm:=TGUITestRunner.Create(nil);
  try
    TestRunForm.Show;
    Application.Run;
  finally
    TestRunForm.Free;
  end;
end.

