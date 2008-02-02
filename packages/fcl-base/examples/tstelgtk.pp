{$mode objfpc}
{$H+}
{$apptype gui}

program tstelgtk;

uses gdk,gtk,fpgtk,fpgtkext,classes,sysutils,eventlog;

{ ---------------------------------------------------------------------
    Main form class
  ---------------------------------------------------------------------}


Type
  TMainForm = Class(TFPGtkWindow)
    FEventLog : TEventLog;
    RGFrame : TFPgtkFrame;
    FHBox : TFPgtkHBox;
    RGBox,
    FVBox : TFPgtkVBox;
    BSend : TFPgtkButton;
    RGMsgType : TFPgtkRadioButtonGroup;
    FLMsg : TFPGtkLabel;
    FMsg : TFPGtkEntry;
    Procedure BSendClicked(Sender : TFPgtkObject; Data : Pointer);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure CreateWindow;
    Procedure SendEvent;
  end;

ResourceString
  SCaption        = 'Free Pascal Event Log Demo';
  SEventlogDemo   = 'TestEventlogClass';
  SMessage        = 'Message text:';
  SMsgType        = 'Message type:';
  SSend           = 'Send message';
  SInformation    = 'Information';
  SWarning        = 'Warning';
  SError          = 'Error';
  SDebug          = 'Debug';

{ ---------------------------------------------------------------------
    Form Creation
  ---------------------------------------------------------------------}

Constructor TMainForm.Create;

begin
  Inherited create (gtk_window_dialog);
  Createwindow;
end;

Procedure TMainForm.CreateWindow;

  Procedure AddRG(C : String);

  Var
    RB : TFPgtkRadioButton;

  begin
    RB:= TFPgtkRadioButton.CreateWithLabel(RGmsgType,C);
    RGBox.Packstart(RB,False,False,2);
    rb.TheLabel.Justify:=GTK_JUSTIFY_LEFT;
  end;

Var
  S : TStrings;

begin
  BSend:=TFPGtkButton.CreateWithlabel(SSend);
  BSend.ConnectCLicked(@BSendClicked,Nil);
  RGFrame:=TFpgtkFrame.Create;
  RGFrame.Text:=SMsgType;
  RGBox:=TFPgtkVBox.Create;
  RGFRame.Add(RGBox);
  S:=TstringList.Create;
  try
    With S do
      begin
      Add(SInformation);
      Add(SWarning);
      Add(SError);
      Add(SDebug);
      end;
     RGMsgType:=RadioButtonGroupCreateFromStrings(S,Nil);
     RGMsgType.PackInBox(RGBox,True,False,False,2);
  Finally
    S.Free;
  end;
  FLMsg:=TfpGtkLabel.Create(SMessage);
  FMsg:=TfpGtkEntry.Create;
  FHBox:=TFPgtkHbox.Create;
  FHBox.PackStart(FLMsg,False,False,2);
  FHBox.PackStart(FMsg,True,True,2);
  Title:=SCaption;
  FVBox:=TFPgtkVBox.Create;
  FVBox.Homogeneous:=False;
  FVBox.PackStart(FHBox,False,False,2);
  FVBox.PackStart(RGFrame,False,False,2);
  FVBox.PackStart(BSend,true,false,2);
  Add(FVBox);
  FMsg.GrabFocus;
  FEventLog:=TEventlog.Create(Nil);
  FEventLog.Identification:=SEventLogDemo;
  FEventLog.RegisterMessagefile('');
  FEventLog.Active:=True;
end;

Destructor TMainForm.Destroy;

begin
  FEventLog.Active:=False;
  FEventLog.Free;
  Inherited;
end;

{ ---------------------------------------------------------------------
    Callback events
  ---------------------------------------------------------------------}

Procedure TMainForm.BSendClicked(Sender : TFPgtkObject; Data : Pointer);

begin
  SendEvent;
end;


Procedure TMainForm.SendEvent;

Var
  E : TEventType;

begin
  Case RGMsgType.ActiveButtonIndex of
    0 : E:=etinfo;
    1 : E:=etWarning;
    2 : E:=etError;
    3 : E:=etDebug;
  end;
  FEventLog.log(E,FMsg.Text);
end;

{ ---------------------------------------------------------------------
    Program.
  ---------------------------------------------------------------------}

begin
  application := TFPgtkApplication.Create;
  application.MainWindow := TMainForm.Create;
  application.Run;
  application.Free;
end.
