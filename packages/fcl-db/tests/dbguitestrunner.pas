unit DBGuiTestRunner;
// Adds database.ini editing facilities to regular GuiTestRunner form

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Interfaces, Forms, StdCtrls, GuiTestRunner, Menus,inieditor;

type

  { TDBGuiTestRunnerForm }

  TDBGuiTestRunnerForm=class(TGUITestRunner)
  private
    MEditIni: TMenuItem;
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
  MEditIni:=TMenuItem.Create(Self);
  MEditIni.Caption:='Edit database.ini...';
  MEditIni.Hint:='Edit database selection settings (effective for next start)';
  MEditIni.OnClick:=@DBEditButtonClick;
  MenuItemEdit.Add(MEditIni);
end;

end.

