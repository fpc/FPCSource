{$mode objfpc}
{$h+}

unit frmtable;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TTableForm = Class (TFPGtkWindow)
    FTable : TFPGtkTable;
    FLTableRows,
    FLTableCols,
    FLUseHeader : TFPGtkLabel;
    FTableRows,
    FTableCols : TFPGtkSpinButton;
    FUseHeader : TFPGtkToggleButton;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FHBox : TFPgtkHBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
    Procedure OnShow(Sender : TFpGtkObject;Data : Pointer);
  end;

Implementation

uses fpdemsg;

Constructor TTableForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TTableForm.CreateWindow;

Var
  OH,OV : TgtkAttachOPtions;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // Table area
  FTable:=TFPGtkTable.Create(2,3);
  FLTableRows:=TFPGtkLabel.Create(STableRows);
  FLTableRows.Justify:=GTK_JUSTIFY_RIGHT;
  FLTableCols:=TFPGtkLabel.Create(STableCols);
  FLTableCols.Justify:=GTK_JUSTIFY_RIGHT;
  FLUseHeader:=TFPGtkLabel.Create(STableHeader);
  FLUseHeader.Justify:=GTK_JUSTIFY_RIGHT;
  FTableRows:=TFPGtkSpinButton.Create;
  FTableCols:=TFPGtkSpinButton.Create;
  FUSeHeader:=TFPgtkToggleButton.Create;
  FUseHeader.SetUSize(14,14);
  FHBox:=TFPgtkHBox.Create;
  FHBox.PackStart(FuseHeader,True,False,0);
  OH:=GTK_EXPAND or GTK_FILL;
  FTable.Attach(FLTableRows,0,1,0,1,0,GTK_FILL,4,4);
  FTable.Attach(FLTableCols,0,1,1,2,0,GTK_FILL,4,4);
  FTable.Attach(FLUseHeader,0,1,2,3,0,GTK_FILL,4,4);
  FTable.Attach(FTableRows,1,2,0,1,OH,0,4,4);
  FTable.Attach(FTableCols,1,2,1,2,OH,0,4,4);
  FTable.Attach(FHBox,1,2,2,3,0,GTK_FILL,4,4);
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

Procedure TTableForm.OnShow(Sender : TFpgtkObject; Data : Pointer);

begin
  FocusedWidget(FTableRows);
end;


end.  
