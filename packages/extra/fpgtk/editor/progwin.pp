{$mode objfpc}{$h+}
unit ProgWin;

interface

uses FPgtk, gtk, classes;

type

  TProgressWindow = class (TFPgtkWindow)
  private
    Bar : TFPgtkProgressbar;
    procedure ComposeWindow;
  public
    procedure StepIt;
    procedure SetMax (max : integer);
    constructor create;
  end;

implementation

uses gtkDefTexts;

procedure TProgressWindow.ComposeWindow;
begin
  Title := ProgressWinTitle;
  border := 20;

  bar := TFPgtkProgressBar.Create (nil);
  bar.FormatString := '- %p%% -';
  add (bar);

end;

procedure TProgressWindow.StepIt;
begin
  with bar do
    CurrentValue := CurrentValue + 1.0;
end;

procedure TProgressWindow.SetMax (max : integer);
begin
  bar.Configure (0.0,0.0,max);
end;

constructor TProgressWindow.create;
begin
  inherited create (gtk_window_dialog);
  ComposeWindow;
end;

end.
