Unit MySortC;

Interface

Uses Objects;

Type
  PMySortedCollection = ^TMySortedCollection;
  TMySortedCollection = Object(TSortedCollection)
       Function Compare (Key1,Key2 : Pointer): Sw_integer; virtual;
       end;

Implementation

Uses MyObject;

Function TMySortedCollection.Compare (Key1,Key2 : Pointer) :sw_integer;

begin
  Compare:=PMyobject(Key1)^.GetField - PMyObject(Key2)^.GetField;
end;

end.