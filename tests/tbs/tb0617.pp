{ %NORUN }

program tb0617;

{$mode objfpc}
{ We don't want this warning to occur again }
{$warn 5059 error}

type
  generic TGenericStructList<T> = class
  public
    function Remove(const Item: T): Integer;
    procedure Delete(Index: Integer);
    function IndexOf(const Item: T): Integer;
  end;

function TGenericStructList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  { for some reason, FPC 3.1.1 makes here incorrect warning:
    castlegenericlists.pas(254,13) Warning: Function result variable does not seem to be initialized }
  if Result >= 0 then
    Delete(Result);
end;

function TGenericStructList.IndexOf(const Item: T): Integer;
begin
  Result := 0;
end;

procedure TGenericStructList.Delete(Index: Integer);
begin

end;


begin

end.
