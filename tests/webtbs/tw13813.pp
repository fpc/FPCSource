program test_collection;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}
{$apptype console}

uses Classes;

type
  titem = class(TCollectionItem)
  public
    procedure do_something;
  end;

  tcoll = class(TCollection)
  public
    procedure Update(Item: TCollectionItem); override;
  end;

var
  c: tcoll;
  item: titem;
  i: Integer;
  update_counter: Integer;

procedure titem.do_something;
begin
  Changed(False);
end;

procedure tcoll.Update(Item: TCollectionItem);
begin
  Inc(update_counter);
  inherited;
end;


begin
  c := tcoll.Create(titem);
  item := titem(c.Add);
  update_counter := 0;
  c.BeginUpdate;
  try
    for i := 0 to 9 do
      item.do_something;
  finally
    c.EndUpdate;
  end;
  writeln('updates: ', update_counter);
  if update_counter<>1 then
    begin
      c.Free;
      Halt(1);
    end;
  c.Free;
  writeln('ok');
end.
