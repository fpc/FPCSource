unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  fpvectorial, svgvectorialwriter, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnView: TButton;
    editFileName: TFileNameEdit;
    procedure btnViewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Vec: TvVectorialDocument;
  end;

var
  frmFPVViewer: TfrmFPVViewer;

implementation

{$R *.lfm}

{ TfrmFPVViewer }

procedure TfrmFPVViewer.btnViewClick(Sender: TObject);
begin
  Vec.ReadFromFile(editFileName.FileName);
end;

procedure TfrmFPVViewer.FormCreate(Sender: TObject);
begin
  Vec := TvVectorialDocument.Create;
end;

procedure TfrmFPVViewer.FormDestroy(Sender: TObject);
begin
  Vec.Free;
end;

end.

