type
   tobject = object
      procedure x;
      constructor c;
   end;

procedure a;

  begin
  end;

procedure tobject.x;

  begin
  end;

constructor tobject.c;

  begin
  end;

var
   p : pointer;

begin
   p:=@a;
   p:=@tobject.x;
   p:=@tobject.c;
end.
