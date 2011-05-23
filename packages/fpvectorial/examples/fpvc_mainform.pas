unit fpvc_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls;

type

  { TformVectorialConverter }

  TformVectorialConverter = class(TForm)
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
  formVectorialConverter: TformVectorialConverter;

implementation

uses
  fpvectorial, cdrvectorialreader, svgvectorialwriter, pdfvectorialreader,
  fpvtocanvas;

{$R *.lfm}

{ TformVectorialConverter }

procedure TformVectorialConverter.buttonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TformVectorialConverter.buttonVisualizeClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  if not CheckInput() then Exit;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editInput.FileName);
    imagePreview.Canvas.Brush.Color := clWhite;
    imagePreview.Canvas.FillRect(0, 0, imagePreview.Width, imagePreview.Height);
    DrawFPVectorialToCanvas(Vec, imagePreview.Canvas);
  finally
    Vec.Free;
  end;
end;

function TformVectorialConverter.CheckInput(): Boolean;
begin
  // todo...
end;

procedure TformVectorialConverter.buttonConvertClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  if not CheckInput() then Exit;

  // Now convert
  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editInput.FileName);
    Vec.WriteToFile(editOutPut.FileName);
  finally
    Vec.Free;
  end;
end;

end.

