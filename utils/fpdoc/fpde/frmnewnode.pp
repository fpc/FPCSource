{$mode objfpc}
{$h+}

unit frmnewnode;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TNewNodeForm = Class (TFPGtkWindow)
    FTable : TFPGtkTable;
    FLENodeName : TFPGtkLabel;
    FENodeName : TFPGtkEntry;
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

uses
  fpdemsg;

Constructor TNewNodeForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TNewNodeForm.CreateWindow;

Var
  OH,OV : TgtkAttachOPtions;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // Table area
  FTable:=TFPGtkTable.Create(1,1);
  FLENodeName:=TFPGtkLabel.Create(SName);
  FLENodeName.Justify:=GTK_JUSTIFY_RIGHT;
  FENodeName:=TFPgtkEntry.Create;
  FENodeName.GrabFocus;
  OH:=GTK_EXPAND or GTK_FILL;
  FTable.Attach(FLENodeName,0,1,0,1,0,GTK_FILL,4,4);
  FTable.Attach(FENodeName,1,2,0,1,OH,0,4,4);
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

Procedure TNewNodeForm.OnShow(Sender : TFpgtkObject; Data : Pointer);

begin
  FocusedWidget(FENodeName);
end;


end.  
