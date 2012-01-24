unit uw20909a;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uw20909b;

type

  TMyEvent = procedure(var Items: Storage.Folders.TItem) of object;

  TMyClass = class
  private
    FOnChange: uw20909b.Storage.Folders.TItemsEvent;
    FMyEvent: TMyEvent;
  public
    property OnChange: Storage.Folders.TItemsEvent read FOnChange write FOnChange;
    property MyEvent: TMyEvent read FMyEvent write FMyEvent;
  end;

implementation

end.

