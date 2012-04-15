unit uw20909b;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Storage = class
  public
    type
      Folders = class
      public
        const
          FLAG_REFRESH = 1;
          FLAG_DELETE = 2;
        type
          TItem = record
            ID: int64;
            Path: string;
          end;
          PItem = ^TItem;
          TItems = array of PItem;
          PItems = ^TItems;

          TItemsEvent = procedure(var Items: TItems) of object;
      end;
  end;

implementation

end.

