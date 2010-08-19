{ %fail }
{$MODE OBJFPC}
program Test;

type
   generic IList<IItem, IItemList> = interface end;
   IPointerList = specialize IList<Pointer, IPointerList>;

type
   generic TList<TItem, TItemList> = class end;
   TPointerList = specialize TList<Pointer, TPointerList>;

begin
end.
