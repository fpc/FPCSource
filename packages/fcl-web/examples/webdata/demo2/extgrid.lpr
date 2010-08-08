program extgrid;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cwstring,xmliconv,{$endif}
  fpCGI, wmusers;

{$IFDEF WINDOWS}{$R extgrid.rc}{$ENDIF}

begin
  Application.Title:='ExtJS Grid demo for fpwebdata';
  Application.Initialize;
  Application.Run;
end.

