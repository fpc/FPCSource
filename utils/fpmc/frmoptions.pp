{$mode objfpc}
{$h+}

unit frmoptions;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TOptionsForm = Class (TFPGtkWindow)
  Private
    FTable : TFPGtkTable;
    FLVerbose,
    FLCreateMsgFile,
    FLCreateRCFile,
    FLCreatePasFile,
    FLEscapePath,
    FLLocale,
    FLSubLocale,
    FLUnitName : TFPGtkLabel;
    FVerbose,
    FCreateMsgFile,
    FCreatePasFile,
    FCreateRCFile,
    FEscapePath : TFPGtkToggleButton;
    FUnitName,
    FLocale,
    FSubLocale : TFPGtkEntry;
    FMaxRecentUsed : TFPGtkSpinButton;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FHBox : TFPgtkHBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
  Public
    Constructor Create;
    Procedure CreateWindow;
    Procedure OnShow(Sender : TFpGtkObject;Data : Pointer);
    Procedure SaveResult(Sender : TFpGtkObject;Data : Pointer);
    Function  GetBoolProp (Index : Integer) : Boolean;
    Procedure SetBoolProp (Index : Integer; Value : Boolean);
    Function GetStringProp (Index : Integer) : String;
    Procedure SetStringProp (Index : Integer; Value : String);
    Property  CreateMsgFile : Boolean Index 1 Read GetBoolProp Write SetBoolProp;
    Property  CreatePasFile : Boolean Index 2 Read GetBoolProp Write SetBoolProp;
    Property  CreateRCFile  : Boolean Index 3 Read GetBoolProp Write SetBoolProp;
    Property  EscapePath    : Boolean Index 4 Read GetBoolProp Write SetBoolProp;
    Property  Verbose       : Boolean Index 5 Read GetBoolProp Write SetBoolProp;
    Property  Locale    : String Index 1 Read GetStringProp Write SetStringProp;
    Property  SubLocale : String Index 2 Read GetStringProp Write SetStringProp;
    Property  UnitName  : String Index 3 Read GetStringProp Write SetStringProp;
  end;

Implementation

ResourceString
  SOptCreateMsgFile = 'Create message file';
  SOptCreateRCFile  = 'Create RC file';
  SOptCreatePasFile = 'Create pascal file';
  SOptEscapePath    = 'Escape path delimiters';
  SOptLocale        = 'Locale ID';
  SOptSubLocale     = 'Sublocale ID';
  SOptUnitName      = 'Unit name';
  SOK               = 'OK';
  SCancel           = 'Cancel';
  SOptVerbose       = 'Be verbose';

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
  FTable:=TFPGtkTable.Create(2,8);

  FLVerbose:=MakeLabel(SOptVerbose);
  FLCreateMsgFile:=MakeLabel(SOptCreateMsgFile);
  FLCreateRCFile:=MakeLabel(SOptCreateRCFile);
  FLCreatePasFile:=MakeLabel(SOptCreatePasFile);
  FLEscapePath:=MakeLabel(SOptEscapePath);
  FLLocale:=MakeLabel(SOptLocale);
  FLSubLocale:=MakeLabel(SOptSubLocale);
  FLUnitName:=MakeLabel(SOptUnitName);

  FVerbose:=MakeCheck;
  FEscapePath:=MakeCheck;
  FCreateMsgFile:=MakeCheck;
  FCreateRCFile:=MakeCheck;
  FCreatePasFile:=MakeCheck;
  FUnitName:=TFpGtkEntry.Create;
  FLocale:=TFpGtkEntry.Create;
  FSubLocale:=TFpGtkEntry.Create;

  OH:=GTK_EXPAND or GTK_FILL;
  FTable.Attach(FLVerbose          ,0,1,0,1,GTK_FILL,0,4,4);
  FTable.Attach(FLCreateMsgFile    ,0,1,1,2,GTK_FILL,0,4,4);
  FTable.Attach(FLCreatePasFile    ,0,1,2,3,GTK_FILL,0,4,4);
  FTable.Attach(FLCreateRCFile     ,0,1,3,4,GTK_FILL,0,4,4);
  FTable.Attach(FLEscapePath       ,0,1,4,5,GTK_FILL,0,4,4);
  FTable.Attach(FLUnitName         ,0,1,5,6,GTK_FILL,0,4,4);
  FTable.Attach(FLLocale           ,0,1,6,7,GTK_FILL,0,4,4);
  FTable.Attach(FLSubLocale        ,0,1,7,8,GTK_FILL,0,4,4);

  FTable.Attach(PackBox(FVerbose)        ,1,2,0,1,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FCreateMsgFile)  ,1,2,1,2,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FCreatePasFile)  ,1,2,2,3,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FCreateRCFile)   ,1,2,3,4,0,GTK_FILL,4,4);
  FTable.Attach(PackBox(FEscapePath)     ,1,2,4,5,0,GTK_FILL,4,4);
  FTable.Attach(FUnitName                ,1,2,5,6,0,GTK_FILL,4,4);
  FTable.Attach(FLocale                  ,1,2,6,7,0,GTK_FILL,4,4);
  FTable.Attach(FSubLocale               ,1,2,7,8,0,GTK_FILL,4,4);

  // button area
  FOK:=TFpGtkButton.CreateWithLabel(SOK);
  FOK.ConnectClicked(@SaveResult,Nil);
  FCancel:=TFPgtkButton.CreateWithLabel(SCancel);
  FCancel.ConnectCLicked(@CloseWindow,Nil);
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
  FocusedWidget(FCreateMsgFile);
end;

Procedure TOptionsForm.SaveResult(Sender : TFpgtkObject; Data : Pointer);

begin
  CloseWithResult(Sender,IntToPointer(drOK));
end;

Function  TOptionsForm.GetBoolProp (Index : Integer) : Boolean;

begin
  Result:=False;
  Case Index of
    1 : Result:=FCreateMsgFile.Active;
    2 : Result:=FCreatePasFile.Active;
    3 : Result:=FCreateRCFile.Active;
    4 : Result:=FEscapePath.Active;
    5 : Result:=FVerbose.Active;
  end;
end;

Procedure TOptionsForm.SetBoolProp (Index : Integer; Value : Boolean);

begin
  Case Index of
    1 : FCreateMsgFile.Active:=Value;
    2 : FCreatePasFile.Active:=Value;
    3 : FCreateRCFile.Active:=Value;
    4 : FEscapePath.Active:=Value;
    5 : FVerbose.Active:=Value;
  end;
end;

Function  TOptionsForm.GetStringProp (Index : Integer) : String;

begin
  Result:='';
  Case Index of
    1 : Result:=Flocale.Text;
    2 : Result:=FSublocale.Text;
    3 : Result:=FUnitName.Text;
  end;
end;

Procedure TOptionsForm.SetStringProp (Index : Integer; Value : String);

begin
  Case Index of
    1 : Flocale.Text:=Value;
    2 : FSublocale.Text:=Value;
    3 : FUnitName.Text:=Value;
  end;
end;

end.
