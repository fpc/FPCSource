{ %fail }
{$mode objfpc}
type generic TListNode<_T> = class(TObject)
       Data: _T;
     end;
type t = specialize TListNode<TListNode>;
begin
end.
