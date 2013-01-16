program dunit_tests;

{%File '..\..\..\sdo_global.inc'}

uses
  ActiveX,
  Forms,
  TestFrameWork,
  GUITestRunner,
  test_type in '..\test_type.pas',
  sdo_type in '..\..\..\sdo_type.pas',
  sdo in '..\..\..\sdo.pas',
  sdo_property in '..\..\..\sdo_property.pas',
  sdo_imp_utils in '..\..\..\sdo_imp_utils.pas',
  sdo_datafactory in '..\..\..\sdo_datafactory.pas',
  sdo_dataobject in '..\..\..\sdo_dataobject.pas',
  sdo_field_imp in '..\..\..\sdo_field_imp.pas',
  test_field_imp in '..\test_field_imp.pas',
  test_dataobject in '..\test_dataobject.pas',
  sdo_linked_list in '..\..\..\sdo_linked_list.pas',
  sdo_types in '..\..\..\sdo_types.pas',
  test_utils in '..\test_utils.pas',
  sdo_dataobjectlist in '..\..\..\sdo_dataobjectlist.pas',
  test_dataobjectlist in '..\test_dataobjectlist.pas',
  sdo_xsd_helper in '..\..\..\sdo_xsd_helper.pas',
  test_xsdhelper in '..\test_xsdhelper.pas',
  sdo_serialization in '..\..\..\sdo_serialization.pas',
  sdo_consts in '..\..\..\sdo_consts.pas',
  test_serializer in '..\test_serializer.pas',
  sdo_serialization_utils in '..\..\..\sdo_serialization_utils.pas',
  sdo_serialization_xml in '..\..\..\sdo_serialization_xml.pas',
  sdo_equalityhelper in '..\..\..\sdo_equalityhelper.pas',
  test_equalityhelper in '..\test_equalityhelper.pas',
  test_changesummary in '..\test_changesummary.pas',
  sdo_changesummary in '..\..\..\sdo_changesummary.pas',
  sdo_copyhelper in '..\..\..\sdo_copyhelper.pas',
  test_copyhelper in '..\test_copyhelper.pas',
  sdo_xpath_helper in '..\..\..\sdo_xpath_helper.pas',
  test_xpathhelper in '..\test_xpathhelper.pas',
  test_suite_utils in '..\test_suite_utils.pas',
  sdo_convert_helper in '..\..\..\sdo_convert_helper.pas',
  test_convert_helper in '..\test_convert_helper.pas',
  test_property in '..\test_property.pas',
  sdo_utils in '..\..\..\sdo_utils.pas',
  sdo_serialization_binary in '..\..\..\sdo_serialization_binary.pas',
  test_xsdparser in '..\test_xsdparser.pas';

{$R *.res}

begin
  Application.Initialize;  
  GUITestRunner.RunRegisteredTests();
end.
