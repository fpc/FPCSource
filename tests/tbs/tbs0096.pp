type
   TParent = object
   end;

   PParent = ^TParent;

   TChild = object(TParent)
   end;

procedure aProc(const x : TParent );
begin
end;

procedure anotherProc(var x : TParent );
begin
end;

var
   y : TChild;

   begin
      aProc(y);
      anotherProc(y);
   end.
