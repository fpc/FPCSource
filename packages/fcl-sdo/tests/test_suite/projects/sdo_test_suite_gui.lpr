program sdo_test_suite_gui;

{$mode objfpc}{$H+}

uses HeapTrc, SysUtils,
  Interfaces, Forms, GuiTestRunner, test_xsdhelper,
  test_changesummary, test_convert_helper, test_copyhelper, test_dataobject,
  test_dataobjectlist, test_equalityhelper, test_field_imp, test_property,
  test_serializer, test_suite_utils, test_type, test_utils, test_xpathhelper,
  types;

begin
  DeleteFile('heaptrace.txt');
  SetHeapTraceOutput('heaptrace.txt');
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

