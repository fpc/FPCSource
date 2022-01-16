{
This unit has been produced by ws_helper.
  Input unit name : "System_Data_Resources_CodeGenerationSchema".
  This unit name  : "cgs".
  Date            : "12-5-16 15:37:59".
}
unit cgs;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://schemas.microsoft.com/ado/2006/04/codegeneration';
  sUNIT_NAME = 'System_Data_Resources_CodeGenerationSchema';

type


  TAccess = ( 
    TAccess_Public
    ,Internal
    ,TAccess_Protected
    ,TAccess_Private
  );

  TPublicOrInternalAccess = ( 
    TPublicOrInternalAccess_Public
    ,TPublicOrInternalAccess_Internal
  );

  SetterAccess_Type = TAccess;

  GetterAccess_Type = TAccess;

  TypeAccess_Type = TPublicOrInternalAccess;

  MethodAccess_Type = TAccess;

Implementation
uses metadata_repository, record_rtti;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAccess),'TAccess');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAccess)].RegisterExternalPropertyName('TAccess_Public','Public');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAccess)].RegisterExternalPropertyName('TAccess_Protected','Protected');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAccess)].RegisterExternalPropertyName('TAccess_Private','Private');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPublicOrInternalAccess),'TPublicOrInternalAccess');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPublicOrInternalAccess)].RegisterExternalPropertyName('TPublicOrInternalAccess_Public','Public');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPublicOrInternalAccess)].RegisterExternalPropertyName('TPublicOrInternalAccess_Internal','Internal');



End.
