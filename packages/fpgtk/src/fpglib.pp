{$mode objfpc}{$h+}
unit FPGlib;

interface

uses classes, glib;

type

  TLukForEachProcedure = procedure (item : pointer; data : pointer) of object;

  TLList = class (TList)
  private
    FGSList : PGSList;
    FGList : PGList;
    FNotUpdating,
    FClassesChanged,FSlistChanged,FListChanged : Boolean;
    procedure FreeList;
    procedure FreeSList;
    function CreateGList : PGList;
    function GetTheGtkList : PGList;
    procedure SetGtkList (Value : PGList);
    function CreateGSList : PGSList;
    function GetTheGtkSList : PGSlist;
    procedure SetGtkSList (Value : PGSList);
    procedure BuildFromGtkList;
    procedure BuildFromGtkSList;
  protected
    procedure Notify (Ptr: Pointer; Action: TListNotification); override;
    function GetData (index : integer) : pointer; dynamic;
    function UngetData (data : pointer) : pointer; dynamic;
    // GetData needs to give the pointer to the data in the List or SList of GTK
    // UngetData needs to give the item in this list from the datapointer of GTK
  public
    constructor create;
    destructor destroy; override;
    function GetGtkList (buffered : boolean) : PGList;
    function GetGtkSList (buffered : boolean) : PGSlist;
    procedure BeginUpdate;  // Currently only changes in 1 list are possible
    procedure EndUpdate;    // without memory leaks and/or errors in the list
    procedure ForEach (Proc : TLukForEachprocedure; data : pointer);
    property GtkList : PGList read GetTheGtkList write SetGtkList;
    property GtkSList : PGSList read GetTheGtkSList write SetGtkSList;
  end;

implementation

{ TLList }

procedure TLList.FreeList;
begin
  if FGList <> null then
    begin
    g_list_free (FGList);
    FGList := null;
    end;
end;

procedure TLList.FreeSList;
begin
  if FGSList <> null then
    begin
    g_slist_free (FGSList);
    FGSlist := null;
    end;
end;

procedure TLList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  FClassesChanged := True;
end;

constructor TLList.create;
begin
  inherited create;
  FClassesChanged := False;
  FListChanged := false;
  FSListChanged := False;
  FGList := null;
  FGSList := null;
  FNotUpdating := True;
end;

destructor TLList.destroy;
begin
  FreeList;
  FreeSList;
  inherited Destroy;
end;

function TLList.GetGtkList (buffered : boolean) : PGList;
begin
  if buffered then
    if FClasseschanged then
      result := CreateGList
    else if FSListChanged then
      begin
      BuildFromGtkSList;
      result := CreateGList;
      end
    else
      result := FGlist
  else
    result := CreateGList;
end;

function TLList.GetGtkSList (buffered : boolean) : PGSList;
begin
  if buffered then
    if FClassesChanged then
      result := CreateGSList
    else if FListChanged then
      begin
      BuildFromGtkList;
      result := CreateGSList;
      end
    else
      result := FGSlist
  else
    result := CreateGSList;
end;

function TLList.CreateGList : PGList;
var r : integer;
begin
  FreeList;
  result := null;
  for r := pred(count) downto 0 do
    result := g_list_prepend (result, GetData(r));
  FGList := result;
end;

function TLList.CreateGSList : PGSList;
var r : integer;
begin
  FreeSList;
  result := null;
  for r := pred(count) downto 0 do
    result := g_slist_prepend (result, GetData(r));
  FGSList := result;
end;

function TLList.GetData (index : integer) : pointer;
begin
  result := items[index];
end;

function TLList.UngetData (data : pointer) : pointer;
begin
  result := data
end;

function TLList.GetTheGtkList : PGList;
begin
  result := GetGtkList (True);
end;

procedure TLList.SetGtkList (Value : PGList);
begin
  FGList := Value;
  if FNotUpdating then
    BuildFromGtkList
  else
    FListChanged := True;
end;

function TLList.GetTheGtkSList : PGSlist;
begin
  result := GetGtkSList (True);
end;

procedure TLList.SetGtkSList (Value : PGSList);
begin
  FGSlist := Value;
  if FNotUpdating then
    BuildFromGtkSList
  else
    FSListChanged := True;
end;

procedure TLList.BuildFromGtkList;
var p : PGList;
begin
  clear;
  p := FGList;
  while p <> null do
    begin
    add (UngetData(p^.data));
    p := p^.Next;
    end;
  FListChanged := False;
  FSListChanged := False;
  FClassesChanged := False;
  FreeSList;
end;

procedure TLList.BuildFromGtkSList;
var p :PGSList;
begin
  clear;
  p := FGSList;
  while p <> null do
    begin
    add (UngetData(p^.data));
    p := p^.Next;
    end;
  FListChanged := False;
  FSListChanged := False;
  FClassesChanged := False;
  FreeList;
end;

procedure TLList.BeginUpdate;
begin
  FNotUpdating := False;
end;

procedure TLList.EndUpdate;
begin
  FNotUpdating := True;
  if FlistChanged then
    BuildFromGtkSList
  else if FSListChanged then
    BuildFromGtkSList
  else if FClassesChanged then
    begin
    FreeSList;
    FreeList;
    end;
end;

procedure TLList.ForEach (Proc : TLukForEachProcedure; data : pointer);
var r: integer;
begin
  for r := 0 to pred(count) do
    Proc (items[r], data);
end;

end.
