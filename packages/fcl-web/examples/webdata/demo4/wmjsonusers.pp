unit wmjsonusers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb, 
    fpwebdata, extjsjson;

type

  { TFPWebProviderDataModule1 }

  TFPWebProviderDataModule1 = class(TFPWebProviderDataModule)
    ExtJSJSONDataFormatter1: TExtJSJSONDataFormatter;
    ExtJSJSonWebdataInputAdaptor1: TExtJSJSonWebdataInputAdaptor;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebProviderDataModule1: TFPWebProviderDataModule1; 

implementation

initialization
  {$I wmjsonusers.lrs}

  RegisterHTTPModule('JSONProvider', TFPWebProviderDataModule1);
end.

