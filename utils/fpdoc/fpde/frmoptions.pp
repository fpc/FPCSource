{$mode objfpc}
{$h+}

unit frmoptions;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TOptionsForm = Class (TFPGtkWindow)
    FTable : TFPGtkTable;
    FLCreateBackup,
    FLSkipEmptyNodes,
    FLConfirmDelete,
    FLMaxRecentUsed,
    FLbackupExtension,
    FLDefaultExtension : TFPGtkLabel;
    FCreateBackup,
    FSkipEmptyNodes,
    FConfirmDelete : TFPGtkToggleButton;
    FBackupExtension,
    FDefaultExtension : TFPGtkEntry;
    FMaxRecentUsed : TFPGtkSpinButton;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FHBox : TFPgtkHBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
    Procedure OnShow(Sender : TFpGtkObject;Data : Pointer);
    Procedure SaveResult(Sender : TFpGtkObject;Data : Pointer);
    Procedure OptionsToForm;
    Procedure FormToOptions;
  end;

Implementation

uses fpdemsg,fpdeopts;

Function MakeLabel(Caption : String) : TFPgtkLabel;

begin
  Result:=TFPGtkLabel.Create(Caption);
  Result.Justify:=GTK_JUSTIFY_RIGHT;
end;

Function MakeCheck : TFPgtkToggleButton;

begin
  Result:=TFPgtkToggleButton.create;
  Result.SetUsize(14,14);
end;


Constructor TOptionsForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Function PackBox(W : TFpGtkWidget) : TFpGtkHbox;

begin
  Result:=TFPGtkHBox.Create;
  Result.PackStart(W,True,False,0);
end;


Procedure ToptionsForm.CreateWindow;

Var
  OH,OV : TgtkAttachOPtions;
  B : TfpgtkHbox;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // Table area
  FTable:=TFPGtkTable.Create(2,6);
  FLCreateBackup:=MakeLabel(SOptCreateBackup);
  FLSkipEmptyNodes:=MakeLabel(SOptSkipEmptyNodes);
  FLConfirmDelete:=MakeLabel(SOptConfirmDelete);
  FLbackupExtension:=MakeLabel(SOptBackupExtension);
  FLDefaultExtension:=MakeLabel(SOptDefaultExtension);
  FLMaxRecentUsed:=MakeLabel(SOptMaxRecentUsed);
  FCreateBackup:=MakeCheck;
  FSkipEmptyNodes:=MakeCheck;
  FConfirmDelete:=MakeCheck;
  FBackupExtension:=TFpGtkEntry.Create;
  FDefaultExtension:=TFpGtkEntry.Create;
  FMaxRecentUsed:=TFPGtkSpinButton.Create;
  FMaxRecentUsed.Adjustment.Upper:=32;
  OH:=GTK_EXPAND or GTK_FILL;
  FTable.Attach(FLConfirmDelete    ,0,1,0,1,GTK_FILL,0,4,4);
  FTable.Attach(FLSkipEmptyNodes   ,0,1,1,2,GTK_FILL,0,4,4);
  FTable.Attach(FLCreatebackup     ,0,1,2,3,GTK_FILL,0,4,4);
  FTable.Attach(FLBackupExtension  ,0,1,3,4,GTK_FILL,0,4,4);
  FTable.Attach(FLDefaultExtension ,0,1,4,5,GTK_FILL,0,4,4);
  FTable.Attach(FLMaxrecentUSed    ,0,1,5,6,GTK_FILL,0,4,4);

  FTable.Attach(PackBox(FConfirmDelete)  ,1,2,0,1,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FSkipEmptyNodes) ,1,2,1,2,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FCreatebackup)   ,1,2,2,3,0,GTK_FILL,4,4);
  FTable.Attach(FBackupExtension         ,1,2,3,4,0,GTK_FILL,4,4);
  FTable.Attach(FDefaultExtension        ,1,2,4,5,0,GTK_FILL,4,4);
  FTable.Attach(FMaxRecentUsed           ,1,2,5,6,0,GTK_FILL,4,4);

  // button area
  FOK:=TFpGtkButton.CreateWithLabel(SOK);
  FOK.ConnectClicked(@SaveResult,Nil);
  FCancel:=TFPgtkButton.CreateWithLabel(SCancel);
  FCancel.ConnectCLicked(@CloseWithResult,IntToPointer(drCancel));
  FSeparator:=TFPgtkHSeparator.Create;
  FButtonBox:=TfpGtkHBox.Create;
  FButtonBox.Spacing:=4;
  FButtonBox.PackEnd(FOK,false,false,4);
  FButtonBox.PackEnd(FCancel,false,false,4);
  // Add to window
  FVBox.PackStart(FTable,False,False,0);
  FVBox.PackStart(FSeparator,False,False,4);
  FVBox.PackStart(FButtonBox,false,false,0);
  // Some events;
  ConnectShow(@OnShow,Nil);
end;

Procedure TOptionsForm.OnShow(Sender : TFpgtkObject; Data : Pointer);

begin
  OptionsToForm;
  FocusedWidget(FConfirmDelete);
end;

Procedure TOptionsForm.SaveResult(Sender : TFpgtkObject; Data : Pointer);

begin
  FormToOptions;
  CloseWithResult(Sender,IntToPointer(drOK));
end;

Procedure TOptionsForm.OptionsToForm;

begin
  FCreateBackup.Active:=CreateBackup;
  FSkipEmptyNodes.Active:=SkipEmptyNodes;
  FConfirmDelete.Active:=ConfirmDelete;
  FBackupExtension.Text:=BackupExtension;
  FDefaultExtension.Text:=DefaultExtension;
  FMaxRecentUsed.AsInteger:=MaxRecentUsed;
end;

Procedure TOptionsForm.FormToOptions;

begin
  CreateBackup:=FCreateBackup.Active;
  SkipEmptyNodes:=FSkipEmptyNodes.Active;
  ConfirmDelete:=FConfirmDelete.Active;
  BackupExtension:=FBackupExtension.Text;
  DefaultExtension:=FDefaultExtension.Text;
  MaxRecentUsed:=FMaxRecentUsed.AsInteger;
  SaveOptions;
end;

end.
