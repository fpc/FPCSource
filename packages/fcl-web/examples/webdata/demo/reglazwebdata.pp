unit reglazwebdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebdata, sqldbwebdata, LazIDEIntf, ProjectIntf, fpextjs, extjsjson, extjsxml;

Type

  { TFileDescWebProviderDataModule }

  TFileDescWebProviderDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

Procedure Register;

Var
   FileDescriptorWebProviderDataModule: TFileDescWebProviderDataModule;

implementation

uses FormEditingIntf;

constructor TFileDescWebProviderDataModule.Create;
begin
  inherited Create;
  Name:='Web DataProvider Module';
  ResourceClass:=TFPWebProviderDataModule;
  UseCreateFormStatements:=False;
end;

function TFileDescWebProviderDataModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', HTTPDefs, websession, fpHTTP, fpWeb, fpwebdata';
end;

function TFileDescWebProviderDataModule.GetLocalizedName: string;
begin
  Result:='Web DataProvider Module';
end;

function TFileDescWebProviderDataModule.GetLocalizedDescription: string;
begin
  Result:='WEB DataProvider Module'#13
         +'A datamodule to handle data requests for WEB (HTTP) applications using WebDataProvider components.';
end;

function TFileDescWebProviderDataModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  Result:=Result+'  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');'+LineEnding;
end;


Procedure Register;

begin
   RegisterComponents('fpWeb',[TWebdataInputAdaptor,TFPWebDataProvider, TSQLDBWebDataProvider,
                               TExtJSJSonWebdataInputAdaptor,TExtJSJSONDataFormatter,
                               TExtJSXMLWebdataInputAdaptor,TExtJSXMLDataFormatter]);
   FileDescriptorWebProviderDataModule:=TFileDescWebProviderDataModule.Create;
   RegisterProjectFileDescriptor(FileDescriptorWebProviderDataModule);
   FormEditingHook.RegisterDesignerBaseClass(TFPCustomWebProviderDataModule);
   FormEditingHook.RegisterDesignerBaseClass(TFPWebProviderDataModule);
end;

end.

