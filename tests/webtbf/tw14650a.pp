{ %fail }

{$mode objfpc}

uses
  classes;

type
  TPublicQueueList = class(tfplist)
  public
    property List: PPointerList read FList;
  end;

begin
end.
