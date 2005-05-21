{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    About form for debug server

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit frmabout;

interface

uses fpgtk,gtk,classes,sysutils;

Type
  TAboutForm = Class (TFPGtkWindow)
    FAboutText : TFPGtkLabel;
    FSeparator : TFPGtkHSeparator;
    FVBox : TFPgtkVBox;
    FOK,
    FCancel : TFPGtkButton;
    FButtonBox: TFPgtkHBox;
    Constructor Create;
    Procedure CreateWindow;
  end;

Implementation

Resourcestring
  SAbout1     = 'Free Pascal message compiler - GTK version.';
  SAbout2     = '(c) 2003, Michael Van Canneyt';
  SOK         = 'OK';
  SCancel     = 'Cancel';

Constructor TAboutForm.Create;

begin
  Inherited Create(GTK_WINDOW_DIALOG);
  CreateWindow;
end;

Procedure TAboutForm.CreateWindow;

Var
  S : String;


begin
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  // About text
  S:=SAbout1+LineEnding+SAbout2;
  FAboutText:=TFPgtkLabel.Create(S);
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
  FVBox.PackStart(FAboutText,True,True,0);
  FVBox.PackStart(FSeparator,False,False,4);
  FVBox.PackStart(FButtonBox,false,false,0);
end;

end.
