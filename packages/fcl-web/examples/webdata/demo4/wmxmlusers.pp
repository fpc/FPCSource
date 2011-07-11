unit wmxmlusers; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, websession, fpHTTP, fpWeb, 
    fpwebdata, extjsxml;

type

  { TFPWebProviderDataModule2 }

  TFPWebProviderDataModule2 = class(TFPWebProviderDataModule)
    ExtJSXMLDataFormatter1: TExtJSXMLDataFormatter;
    ExtJSXMLWebdataInputAdaptor1: TExtJSXMLWebdataInputAdaptor;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebProviderDataModule2: TFPWebProviderDataModule2; 

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule('XMLProvider', TFPWebProviderDataModule2);
end.

