{ %fail }

{$mode objfpc}
unit tw7752;
interface
type generic TMapNode<_Key,_Value> = class(TObject)
       key: _Key;
       value: _Value;
     end;
     generic TMap<_Key,_Value> = class(TObject)
     private
     public
       procedure Insert(x: TMapNode);
     end;

implementation

procedure TMap.Insert(x: TMapNode);
	procedure TreeInsert(z: TMapNode);
	var x,y: TMapNode;
	begin
	end;
begin
end;

end.
