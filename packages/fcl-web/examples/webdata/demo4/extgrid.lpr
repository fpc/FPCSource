program extgrid;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cwstring,xmliconv,{$endif}
  fpCGI, wmjsonusers, wmxmlusers, dmusers;

{$IFDEF WINDOWS}{$R extgrid.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.Run;
end.

