program sdo_test_suite;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, sdo, sdo_datafactory, sdo_dataobject,
  sdo_dataobjectlist, sdo_field_imp, sdo_imp_utils, sdo_linked_list,
  sdo_property, sdo_type, test_type, test_field_imp, test_dataobjectlist,
  test_dataobject, test_utils, test_xsdhelper, test_serializer,
  test_equalityhelper, test_changesummary, test_suite_utils, sdo_xsd_helper,
  test_xpathhelper, sdo_xpath_helper, sdo_copyhelper, test_copyhelper,
  sdo_consts, sdo_equalityhelper, sdo_serialization, sdo_serialization_utils,
  sdo_types, sdo_utils, sdo_changesummary, sdo_convert_helper,
  sdo_serialization_xml, test_convert_helper, test_property;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.