{$mode objfpc}
type generic PListNode<_T> = ^specialize TListNode;
     generic TListNode<_T> = class(TObject)
         Data: _T;
         Next,Prev: PListNode;
     end;
     generic TList<_T> = class(TObject)
         First,Last: PListNode;
         procedure add(item: _T);
     end;

procedure TList.add(item: _T);
begin
end;

type TMyList = specialize TList<real>;

begin
end.