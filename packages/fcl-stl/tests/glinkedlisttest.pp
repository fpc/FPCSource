program LLTest;

{$apptype console}

uses
  glinkedlist;

type
  IMyIntf = interface
    function GetName: string;
    property Name: string read GetName;
  end;

  { TMyClass }

  TMyClass = class(TInterfacedObject, IMyIntf)
  protected
    FName: string;
  public
    constructor Create(const AName: string);
    function GetName: string;
  end;

  TIntfLL = specialize TLinkedList<IMyIntf>;

  { TTest }

  TTest = class
    FList: TIntfLL;
    procedure Notification(Sender: TObject; const Item: IMyIntf; Action: TCollectionNotification);
    procedure SetupItems;
    procedure PrintList;
    function Main: TTest;
  end;

operator :=(const AValue: string): IMyIntf;
begin
  Result := TMyClass.Create(AValue);
end;

{ TTest }

procedure TTest.Notification(Sender: TObject; const Item: IMyIntf;
  Action: TCollectionNotification);
var
  LL: TIntfLL;
begin
  LL := (Sender as TIntfLL);
  case Action of
    cnAdded:
      write('added');
    cnRemoved:
      write('removed');
  end;
  write(' "', Item.GetName, '"; ');
  write('count=', LL.Count, '; ');

  write('first=');
  if LL.First = nil then
    write('nil')
  else
    write('"' + LL.First^.Data.Name, '"');

  write(' ');

  write('last=');
  if LL.Last = nil then
    write('nil')
  else
    write('"' + LL.Last^.Data.Name, '" ');

  writeln;
end;

procedure TTest.SetupItems;
begin
  // add items "1" to "8"
  FList.InsertLast('4')^.InsertAfter('5')^.InsertAfter('6');
  FList.InsertFirst('3')^.InsertBefore('2')^.InsertBefore('1');
  FList.Last^.InsertAfter('7')^.InsertAfter('8');
end;

procedure TTest.PrintList;
var
  i: IMyIntf;
begin
  write('"');
  for i in FList do
    write(i.GetName, ' ');
  writeln('"');
end;

function TTest.Main: TTest;
var
  i: integer;
  item: TIntfLL.PItem;
begin
  FList := TIntfLL.Create;
  try
    FList.OnNotify := @Notification;

    // setup and print items
    SetupItems;
    PrintList;
    WriteLn;
    // print ROL
    for i := 1 to 8 do
    begin
      FList.RotateLeft;
      PrintList;
    end;
    WriteLn;
    // print ROR
    for i := 1 to 8 do
    begin
      FList.RotateRight;
      PrintList;
    end;
    WriteLn;
    // print deleting first item
    for i := 1 to 8 do
    begin
      FList.Delete(FList.First);
      PrintList;
    end;
    WriteLn;
    // print deleting last item
    SetupItems;
    for i := 1 to 8 do
    begin
      FList.Delete(FList.Last);
      PrintList;
    end;
    WriteLn;

    // delete some item from middle
    SetupItems;
    PrintList;
    item := FList.First^.Next^.Next^.Next;
    WriteLn(item^.data.GetName);
    FList.Delete(item);
    PrintList;
    WriteLn;

    // clear all items
    FList.Clear;
    PrintList;
    WriteLn;
  finally
    FList.Free;
  end;
  Result:=Self;
end;

{ TMyClass }

constructor TMyClass.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TMyClass.GetName: string;
begin
  Result := FName;
end;

begin
  With TTest.Create do
    try
      Main;
    finally
      Free;
    end;
end.

