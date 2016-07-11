{
This unit has been produced by ws_helper.
  Input unit name : "System_Data_Resources_AnnotationSchema".
  This unit name  : "ras".
  Date            : "12-5-16 15:37:59".
}
unit ras;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://schemas.microsoft.com/ado/2009/02/edm/annotation';
  sUNIT_NAME = 'System_Data_Resources_AnnotationSchema';

type


  TStoreGeneratedPattern = ( 
    None
    ,Identity
    ,Computed
  );

  StoreGeneratedPattern_Type = TStoreGeneratedPattern;

  LazyLoadingEnabled_Type = boolean;

Implementation
uses metadata_repository, record_rtti;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TStoreGeneratedPattern),'TStoreGeneratedPattern');



End.
