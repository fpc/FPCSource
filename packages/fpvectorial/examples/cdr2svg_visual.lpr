program cdr2svg_visual;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, cdr2svg_mainform
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformCDR2SVG, formCDR2SVG);
  Application.Run;
end.

