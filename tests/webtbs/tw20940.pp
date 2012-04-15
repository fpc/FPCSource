program tw20940;

{$mode objfpc}{$H+}

uses
  Classes, uw20940;

Type
  TMyEvent=procedure(var Items:Storage.Folders.TItem) of Object;
  TMyClass=class
  private
    FFolders : uw20940.Storage.Folders.TItems;
    FOnChange : uw20940.Storage.Folders.TItemsEvent;
    FMyEvent : TMyEvent;
  public
    property OnChange:Storage.Folders.TItemsEvent read FOnChange write FOnChange;
    //property MyEvent:TMyEvent read FOnMyEvent write FOnMyEvent;
  end;

  TOtherClass=class
    function  SomeMethod(Var ItemP:uw20940.Storage.Folders.PItem):uw20940.Storage.Folders.PItem;
    procedure MyEvent(var Item:uw20940.Storage.Resources.TItem); overload;
    {remove comment} procedure MyEvent(var Item:uw20940.Storage.Folders.TItem); overload;
    {remove comment} procedure MyEvent(var Item:uw20940.Storage.Files.TItem); overload;
  end;

  function TOtherClass.SomeMethod(Var ItemP:uw20940.Storage.Folders.PItem):uw20940.Storage.Folders.PItem;
  begin

  end;

  procedure TOtherClass.MyEvent(var Item:uw20940.Storage.Resources.TItem);
  begin

  end;
  procedure TOtherClass.MyEvent(var Item:uw20940.Storage.Folders.TItem);
  begin

  end;


  procedure TOtherClass.MyEvent(var Item:uw20940.Storage.Files.TItem);
  begin

  end;

begin


end.

