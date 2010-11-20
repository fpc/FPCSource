{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazwebdata; 

interface

uses
    reglazwebdata, extjsjson, extjsxml, fpextjs, fpwebdata, sqldbwebdata, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('reglazwebdata', @reglazwebdata.Register); 
end; 

initialization
  RegisterPackage('lazwebdata', @Register); 
end.
