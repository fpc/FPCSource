type
  TA = object
  end;

var
   P: Pointer;

begin
   P := pointer(TypeOf(TA));
end.
