{$mode objfpc}
{$h+}

unit frmlink;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TLinkForm = Class (TFPGtkWindow)
    FTable : TFPGtkTable;
    FLLinkTarget,
    FLLinkText : TFPGtkLabel;
    FLinkText : TFPGtkEntry;
    FLinkTarget : TFPGtkCombo;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
    Procedure OnShow(Sender : TFpGtkObject;Data : Pointer);
  end;

Implementation

uses fpdemsg;

Constructor TLinkForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TLinkForm.CreateWindow;

Var
  OH,OV : TgtkAttachOPtions;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // Table area
  FTable:=TFPGtkTable.Create(2,2);
  FLLinktarget:=TFPGtkLabel.Create(SLinkTarget);
  FLLinktarget.Justify:=GTK_JUSTIFY_RIGHT;
  FLLinkText:=TFPGtkLabel.Create(SLinkText);
  FLLinktext.Justify:=GTK_JUSTIFY_RIGHT;
  FLinkText:=TFPgtkEntry.Create;
  FlinkTarget:=TFPGtkCombo.Create;
  OH:=GTK_EXPAND or GTK_FILL;
  FTable.Attach(FLLinkTarget,0,1,0,1,0,GTK_FILL,4,4);
  FTable.Attach(FLLinkText,0,1,1,2,0,GTK_FILL,4,4);
  FTable.Attach(FLinkTarget,1,2,0,1,OH,0,4,4);
  FTable.Attach(FLinkText,1,2,1,2,OH,0,4,4);
  // button area
  FOK:=TFpGtkButton.CreateWithLabel(SOK);
  FOK.ConnectClicked(@CloseWithResult,IntToPointer(drOK));
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

Procedure TLinkForm.OnShow(Sender : TFpgtkObject; Data : Pointer);

begin
  FocusedWidget(FLinkTarget.entry);
end;


end.  
