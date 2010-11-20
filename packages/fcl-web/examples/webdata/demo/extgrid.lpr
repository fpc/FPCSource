program extgrid;

{$mode objfpc}{$H+}

uses
  fpCGI, wmusers;

{$IFDEF WINDOWS}{$R extgrid.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.Run;
end.

