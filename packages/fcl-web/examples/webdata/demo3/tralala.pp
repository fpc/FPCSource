unit tralala; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb; 

type
  TFPWebModule1 = class(TFPWebModule)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

initialization
  {$I tralala.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.

