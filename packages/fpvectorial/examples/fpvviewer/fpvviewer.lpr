{
FPVectorial example application for viewing vectorial images

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvviewer;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, fpvv_mainform;

//{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFPVViewer, frmFPVViewer);
  Application.Run;
end.

