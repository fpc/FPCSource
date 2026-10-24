program ListDemo;

uses Objects, Views, Dialogs, App, Drivers;

type
  PListItems = ^TListItems;
  TListItems = object(TStringCollection)
    constructor Init;
  end;
  PPickLine = ^TPickLine;
  TPickLine = object(TInputLine)
    procedure HandleEvent(var Event: TEvent); virtual;
  end;
  PPickWindow = ^TPickWindow;
  TPickWindow = object(TDialog)
    constructor Init;
  end;
  TDoublClickApp = object(TApplication)
    PickWindow: PPickWindow;
    constructor Init;
  end;

constructor TListItems.Init;
begin
  inherited Init(10, 10);
  Insert(NewStr('One'));
  Insert(NewStr('Two'));
  Insert(NewStr('Three'));
  Insert(NewStr('Four'));
  Insert(NewStr('Five'));
  Insert(NewStr('Six'));
  Insert(NewStr('Seven'));
  Insert(NewStr('Eight'));
  Insert(NewStr('Nine'));
end;

procedure TPickLine.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if Event.What = evBroadcast then
    if Event.Command = cmListItemSelected then
    begin
      with PListBox(Event.InfoPtr)^ do
      begin
        Data^ := GetText(Focused, 30);
      end;
      DrawView;
      ClearEvent(Event);
    end;
end;

constructor TPickWindow.Init;
var
  R: TRect;
  Control: PView;
  ListBox:PListBox;

  ScrollBar: PScrollBar;
begin
  R.Assign(0, 0, 46, 18);
  inherited Init(R, 'Double click example');
  Options := Options or ofCentered;
  R.Assign(4, 2, 41, 3);
  Insert(New(PLabel, Init(R, 'To pick List Item double click on it', nil)));
  R.Assign(40, 5, 41, 11);
  New(ScrollBar, Init(R));
  Insert(ScrollBar);
  R.Assign(5, 5, 40, 11);
  ListBox := New(PListBox, Init(R, 1, ScrollBar));
  Insert(ListBox);
  ListBox^.NewList(New(PListItems, Init));
  R.Assign(4, 4, 12, 5);
  Insert(New(PLabel, Init(R, 'Items:', Control)));
  R.Assign(5, 13, 41, 14);
  Control := New(PPickLine, Init(R, 30));
  Control^.EventMask := Control^.EventMask or evBroadcast;
  Insert(Control);
  R.Assign(4, 12, 13, 13);
  Insert(New(PLabel, Init(R, 'Picked:', Control)));
  R.Assign(18, 15, 28, 16);
  Insert(New(PButton, Init(R, '~Q~uit', cmQuit, bfDefault)));
  ListBox^.Select();
end;

constructor TDoublClickApp.Init;
begin
  inherited Init;
  PickWindow := New(PPickWindow, Init);
  InsertWindow(PickWindow);
end;

var
  DoublClickApp: TDoublClickApp;
begin
  DoublClickApp.Init;
  DoublClickApp.Run;
  DoublClickApp.Done;
end.