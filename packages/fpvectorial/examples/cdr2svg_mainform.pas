unit cdr2svg_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls;

type

  { TformCDR2SVG }

  TformCDR2SVG = class(TForm)
    buttonVisualize: TButton;
    buttonConvert: TButton;
    buttonQuit: TButton;
    editInput: TFileNameEdit;
    editOutput: TFileNameEdit;
    imagePreview: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure buttonConvertClick(Sender: TObject);
    procedure buttonQuitClick(Sender: TObject);
    procedure buttonVisualizeClick(Sender: TObject);
  private
    { private declarations }
    function CheckInput(): Boolean;
  public
    { public declarations }
  end; 

var
  formCDR2SVG: TformCDR2SVG;

implementation

uses
  fpvectorial, cdrvectorialreader, svgvectorialwriter, pdfvectorialreader,
  fpvtocanvas;

{$R *.lfm}

{ TformCDR2SVG }

procedure TformCDR2SVG.buttonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TformCDR2SVG.buttonVisualizeClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  if not CheckInput() then Exit;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editInput.FileName, vfPDF);
    imagePreview.Canvas.Brush.Color := clWhite;
    imagePreview.Canvas.FillRect(0, 0, imagePreview.Width, imagePreview.Height);
    DrawFPVectorialToCanvas(Vec, imagePreview.Canvas, 0, 0, 0.25, 0.25);
  finally
    Vec.Free;
  end;
end;

function TformCDR2SVG.CheckInput(): Boolean;
begin
  // todo...
end;

procedure TformCDR2SVG.buttonConvertClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  if not CheckInput() then Exit;

  // Now convert
  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editInput.FileName, vfPDF);
    Vec.WriteToFile(editOutPut.FileName, vfSVG);
  finally
    Vec.Free;
  end;
end;

end.

