{
    This file is part of the fpgtk package
    Copyright (c) 1999-2000 by Michael van Canney, Sebastian Guenther
 
    Find dialogs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Finddlgs;

interface

{$IFDEF FPC_DOTTEDUNITS}
uses Api.Gtk1.Gtk, Fpgtk;
{$ELSE FPC_DOTTEDUNITS}
uses gtk, FPgtk;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFindDialog = class (TFPgtkWindow)
  private
    FSearchString : AnsiString;
    EditSearch : TFPgtkEntry;
    procedure SetSearchString (Value:AnsiString);
    procedure ChangeText (Sender:TFPgtkObject; data:pointer);
  public
    constructor create (WindowType : TGtkWindowType); override;
    procedure DoDialogInit (InitData : pointer); override;
    property SearchString : AnsiString read FSearchString write SetSearchString;
  end;

  PFindDialogData = ^TFindDialogData;
  TFindDialogData = record
    Text : AnsiString;
  end;

implementation

resourcestring
  rsSearch = 'Search';

constructor TFindDialog.create (WindowType : TGtkWindowType);
var b : TFPgtkButton;
    t : TFPgtkTable;
begin

  inherited Create (WindowType);
  border := 2;

  t := TFPgtkTable.create (2,3);
  Add (t);

  t.attach (TFPgtkLabel.create('Give text to search (case sensitive)'), 0,2, 0,1);

  b := TFPgtkButton.CreateWithLabel ('Ok');
  b.ConnectClicked ( CloseWithResult, inttopointer (drOk) );
  t.attach (b, 0,1, 2,3);
  b.Candefault := True;
  b.GrabDefault;

  b := TFPgtkButton.CreateWithLabel ('Cancel');
  b.ConnectClicked ( CloseWindow, inttopointer (drCancel) );
  t.attach (b, 1,2, 2,3);
  b.Candefault := True;

  EditSearch := TFpGtkEntry.Create;
  EditSearch.ConnectChanged (ChangeText, nil);
  t.attach (EditSearch, 0,2, 1,2);
  EditSearch.GrabFocus;

end;

procedure TFindDialog.DoDialogInit (InitData : pointer);
begin
  EditSearch.Text := PFindDialogData(InitData).Text;
  Title := rsSearch;
  inherited;
end;

procedure TFindDialog.SetSearchString (Value : AnsiString);
begin
  EditSearch.Text := Value;
end;

procedure TFindDialog.ChangeText (Sender : TFPGtkObject; data : pointer);
begin
  FSearchString := EditSearch.Text;
end;

end.
