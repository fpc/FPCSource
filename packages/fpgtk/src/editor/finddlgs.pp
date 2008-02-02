unit Finddlgs;

interface

uses gtk, FPgtk;

type
  TFindDialog = class (TFPgtkWindow)
  private
    FSearchString : string;
    EditSearch : TFPgtkEntry;
    procedure SetSearchString (Value:string);
    procedure ChangeText (Sender:TFPgtkObject; data:pointer);
  public
    constructor create (WindowType : TGtkWindowType); override;
    procedure DoDialogInit (InitData : pointer); override;
    property SearchString : string read FSearchString write SetSearchString;
  end;

  PFindDialogData = ^TFindDialogData;
  TFindDialogData = record
    Text : string;
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

procedure TFindDialog.SetSearchString (Value : string);
begin
  EditSearch.Text := Value;
end;

procedure TFindDialog.ChangeText (Sender : TFPGtkObject; data : pointer);
begin
  FSearchString := EditSearch.Text;
end;

end.
