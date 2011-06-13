unit wmusers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, websession, fpHTTP, fpWeb, 
  db, dbf, fpwebdata, extjsjson, extjsxml;

type

  { TFPWebProviderDataModule2 }

  TFPWebProviderDataModule2 = class(TFPWebProviderDataModule)
    ExtJSJSONDataFormatter1: TExtJSJSONDataFormatter;
    ExtJSJSonWebdataInputAdaptor1: TExtJSJSonWebdataInputAdaptor;
    ExtJSXMLDataFormatter1: TExtJSXMLDataFormatter;
    ExtJSXMLWebdataInputAdaptor1: TExtJSXMLWebdataInputAdaptor;
    procedure DataModuleGetContentProducer(Sender: TObject;
      var AContentProducer: TCustomHTTPDataContentProducer);
    procedure DataModuleGetInputAdaptor(Sender: TObject;
      var AInputAdaptor: TCustomWebdataInputAdaptor);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebProviderDataModule2: TFPWebProviderDataModule2; 

implementation

{$R *.lfm}

Type

    { TDBFWebDataProvider }

    TDBFWebDataProvider = Class(TFPCustomWebDataProvider)
      FDBF : TDBF;
    Public
      Constructor Create(AOwner : TComponent); override;
      Function GetDataset : TDataset; override;
    end;

{ TDBFWebDataProvider }

constructor TDBFWebDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBF:=TDBF.Create(Self);
  FDBF.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
  FDBF.Open;
  With FDBF.FieldByname('ID') do
    ProviderFlags:=ProviderFlags+[pfInKey];
end;

function TDBFWebDataProvider.GetDataset: TDataset;
begin
  Result:=FDBF;
end;

{ TFPWebProviderDataModule2 }

procedure TFPWebProviderDataModule2.DataModuleGetInputAdaptor(Sender: TObject;
  var AInputAdaptor: TCustomWebdataInputAdaptor);
begin
  If Request.QueryFields.values['format']='xml' then
    AInputAdaptor:=ExtJSXMLWebdataInputAdaptor1;
end;

procedure TFPWebProviderDataModule2.DataModuleGetContentProducer(
  Sender: TObject; var AContentProducer: TCustomHTTPDataContentProducer);
begin
  If Request.QueryFields.values['format']='xml' then
    AContentProducer:=ExtJSXMLDataFormatter1;
end;

initialization
  RegisterHTTPModule('Provider', TFPWebProviderDataModule2);
  WebDataProviderManager.RegisterProvider('AName',TDBFWebDataProvider);
end.

