{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit fpvectorial_pkg; 

interface

uses
    fpvectorial, pdfvectorialreader, pdfvrlexico, pdfvrsemantico, 
  pdfvrsintatico, avisozlib, avisocncgcodewriter, avisocncgcodereader, 
  fpvtocanvas, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('fpvectorial_pkg', @Register); 
end.
