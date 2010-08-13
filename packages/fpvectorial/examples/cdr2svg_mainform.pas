unit cdr2svg_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonConvert: TButton;
    buttonQuit: TButton;
    editInput: TFileNameEdit;
    editOutput: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure buttonConvertClick(Sender: TObject);
    procedure buttonQuitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  fpvectorial, cdrvectorialreader, svgvectorialwriter;

{$R *.lfm}

{ TForm1 }

procedure TForm1.buttonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.buttonConvertClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  // todo...

  // Now convert
  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editInput.FileName, vfPDF);
    Vec.WriteToFile(editOutPut.FileName, vfGCodeAvisoCNCPrototipoV5);
  finally
    Vec.Free;
  end;
end;

end.

