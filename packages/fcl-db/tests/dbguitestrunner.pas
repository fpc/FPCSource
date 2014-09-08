unit DBGuiTestRunner;
// Adds database.ini editing facilities to regular GuiTestRunner form

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Interfaces, Forms,
  StdCtrls,
  GuiTestRunner, inieditor;

type

  { TDBGuiTestRunnerForm }

  TDBGuiTestRunnerForm=class(TGUITestRunner)
  private
    DBEditButton: TButton;
  public
    procedure DBEditButtonClick(ASender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

var
  DBGuiTestRunnerForm: TDBGuiTestRunnerForm;


implementation


{ TDBGuiTestRunnerForm }

procedure TDBGuiTestRunnerForm.DBEditButtonClick(ASender: TObject);
var
  DBSelectForm: TFormIniEditor;
begin
  DBSelectForm:=TFormIniEditor.Create(nil);
  try
    DBSelectForm.INIFile:='database.ini';
    DBSelectForm.ProfileSelectSection:='Database';
    DBSelectForm.ProfileSelectKey:='type';
    // We can ignore resulting db selection as the file is saved already:
    DBSelectForm.ShowModal;
  finally
    DBSelectForm.Free;
  end;
end;

constructor TDBGuiTestRunnerForm.Create(AOwner: TComponent);
// Add our database.ini edit button to the existing GUI
begin
  inherited Create(AOwner);
  DBEditButton:=TButton.Create(Self);
  DBEditButton.Top:=7;
  DBEditButton.Left:=340; //to the left of the close button
  DBEditButton.Height:=32;
  DBEditButton.Width:=100;
  DBEditButton.Caption:='Edit database.ini...';
  DBEditButton.Hint:='Edit database selection settings (effective for next start)';
  DBEditButton.OnClick:=@DBEditButtonClick;
  // Set this last; now all properties take effect
  DBEditButton.Parent:=Self.Panel1;
end;

end.

