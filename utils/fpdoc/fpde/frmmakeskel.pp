{$mode objfpc}
{$h+}

unit frmmakeskel;

interface

uses fpgtk,fpgtkext,gtk,classes,sysutils;

Type
  TMakeSkelForm = Class (TFPGtkWindow)
    FTable : TFPGtkTable;
    FInputFile,
    FOutputFile : TFPgtkFileEntry;
    FPackageName,
    FAdditionalOptions : TFPgtkEntry;
    FDisableArguments,
    FDisableResults,
    FDisableSeeAlso,
    FDisableProtected,
    FDisablePrivate,
    FDisableErrors : TFPGtkToggleButton;
    FLInputFile,
    FLOutPutfile,
    FLPackageName,
    FLAdditionalOptions,
    FLDisableArguments,
    FLDisableResults,
    FLDisableSeeAlso,
    FLDisableProtected,
    FLDisablePrivate,
    FLDisableErrors : TFPGtkLabel;
    FDisableFrame : TFPgtkFrame;
    FDisableTable : TFPgtkTable;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
    Procedure OnShow(Sender : TFpGtkObject;Data : Pointer);
    Procedure OnOKClick(Sender : TFpGtkObject;Data : Pointer);
  end;

Implementation

resourcestring
  SDisableCode = 'Do not generate nodes for';
  SArguments = 'Function arguments';
  SResults = 'Function results';
  SProtected = 'Protected class members';
  SPrivate = 'Private class members';
  SSeeAlso = 'See also section';
  SErrors = 'Errors section';
  SInputfile = 'Input file';
  SOutputFile = 'Output file';
  SPackageName = 'Package name';
  SAdditionalOptions = 'Additional options';
  SOK = 'OK';
  SCancel = 'Cancel';
  SNeedInputFileName = 'An input filename is required.';
  SNeedOutputFileName = 'An output filename is required.';
  SNeedPackageName  = 'A package name is required.';

Constructor TMakeSkelForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TMakeSkelForm.CreateWindow;

Var
  OH,OV : TgtkAttachOPtions;

  Function CreateToggle : TFPgtkToggleButton;

  begin
    Result:=TFPgtkToggleButton.Create;
    Result.SetUsize(14,14);
  end;

  Function CreateLLabel(S : String) : TFPgtkLabel;

  begin
    Result:=TFPgtkLabel.Create(S);
    Result.Justify:=GTK_JUSTIFY_LEFT;
  end;

  Function CreateRLabel(S : String) : TFPgtkLabel;

  begin
    Result:=TFPgtkLabel.Create(S);
    Result.Justify:=GTK_JUSTIFY_RIGHT;
  end;

begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // input options table area
  FTable:=TFPGtkTable.Create(2,4);
  FLInputFile:=CreateRLabel(SInputFile);
  FInputFile:=TFPgtkFileEntry.Create;
  FLOutputFile:=CreateRLabel(SOutputFile);
  FOutputFile:=TFPgtkFileEntry.Create;
  FLAdditionalOptions:=CreateRLabel(SAdditionalOptions);
  FAdditionalOptions:=TFPgtkEntry.Create;
  FLPackageName:=CreateRLabel(SPackageName);
  FPackageName:=TFPgtkEntry.Create;
  // Pack in table.
  OH:=GTK_EXPAND or GTK_FILL;
  With FTable do
    begin
    Attach(FLInputFile,0,1,0,1,GTK_FILL,0,4,4);
    Attach(FLOutputFile,0,1,1,2,GTK_FILL,0,4,4);
    Attach(FLPackageName,0,1,2,3,GTK_FILL,0,4,4);
    Attach(FLAdditionalOptions,0,1,3,4,GTK_FILL,0,4,4);
    Attach(FInputFile,1,2,0,1,OH,0,4,4);
    Attach(FOutputFile,1,2,1,2,OH,0,4,4);
    Attach(FPackageName,1,2,2,3,OH,0,4,4);
    Attach(FAdditionalOptions,1,2,3,4,OH,0,4,4);
    end;
  FDisableArguments:=CreateToggle;
  FDisableResults:=CreateToggle;
  FDisableSeeAlso:=CreateToggle;
  FDisableProtected:=CreateToggle;
  FDisablePrivate:=CreateToggle;
  FDisableErrors:=CreateToggle;
  FLDisableArguments:=CreateLLabel(SArguments);
  FLDisableResults:=CreateLLabel(SResults);
  FLDisableSeeAlso:=CreateLLabel(SSeeAlso);
  FLDisableProtected:=CreateLLabel(SProtected);
  FLDisablePrivate:=CreateLLabel(SPrivate);
  FLDisableErrors:=CreateLLabel(SErrors);
  FDisableTable:=TFPgtkTable.Create(2,6);
  With FDisableTable do
    begin
    // Checks
    Attach(FDisableArguments   ,0,1,0,1,0,GTK_FILL,4,4);
    Attach(FDisableResults     ,0,1,1,2,0,GTK_FILL,4,4);
    Attach(FDisableProtected   ,0,1,2,3,0,GTK_FILL,4,4);
    Attach(FDisablePrivate     ,0,1,3,4,0,GTK_FILL,4,4);
    Attach(FDisableErrors      ,0,1,4,5,0,GTK_FILL,4,4);
    Attach(FDisableSeeAlso     ,0,1,5,6,0,GTK_FILL,4,4);
    // Labels
    Attach(FLDisableArguments  ,1,2,0,1,GTK_FILL,0,4,4);
    Attach(FLDisableResults    ,1,2,1,2,GTK_FILL,0,4,4);
    Attach(FLDisableProtected  ,1,2,2,3,GTK_FILL,0,4,4);
    Attach(FLDisablePrivate    ,1,2,3,4,GTK_FILL,0,4,4);
    Attach(FLDisableErrors     ,1,2,4,5,GTK_FILL,0,4,4);
    Attach(FLDisableSeeAlso    ,1,2,5,6,GTK_FILL,0,4,4);
    end;
  FDisableFrame:=TFpgtkFrame.Create;
  FDisableFrame.Text:=SDisableCode;
  FDisableFrame.Add(FDisableTable);
  // button area
  FOK:=TFpGtkButton.CreateWithLabel(SOK);
  FOK.ConnectClicked(@OnOkCLick,Nil);
  FCancel:=TFPgtkButton.CreateWithLabel(SCancel);
  FCancel.ConnectCLicked(@CloseWithResult,IntToPointer(drCancel));
  FSeparator:=TFPgtkHSeparator.Create;
  FButtonBox:=TfpGtkHBox.Create;
  FButtonBox.Spacing:=4;
  FButtonBox.PackEnd(FOK,false,false,4);
  FButtonBox.PackEnd(FCancel,false,false,4);
  // Add to window
  FVBox.PackStart(FTable,False,False,0);
  FVBox.PackStart(FDisableFrame,False,False,4);
  FVBox.PackStart(FSeparator,False,False,4);
  FVBox.PackStart(FButtonBox,false,false,0);
  // Some events;
  ConnectShow(@OnShow,Nil);
end;

Procedure TMakeSkelForm.OnShow(Sender : TFpgtkObject; Data : Pointer);

begin
  FocusedWidget(FInputFile.Edit);
end;

Procedure TMakeSkelForm.OnOkClick(Sender : TFpgtkObject; Data : Pointer);

begin
  If (FInputFile.FileName='') then
    MessageDlg(SNeedInputFileName,mtError,[mbOk],0)
  else If (FOutPutFile.FileName='') then
    MessageDlg(SNeedOutPutFileName,mtError,[mbOk],0)
  Else if (FPackageName.Text='') then
    MessageDlg(SNeedPackageName,mtError,[mbOk],0)
  else
    CloseWithResult(Sender,IntToPointer(drOK));
end;

end.
