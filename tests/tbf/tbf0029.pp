type
  TA = object
  end;

var
   P: Pointer;

begin
   { must fail on compilation because
     TA has no VMT }
   P := pointer(TypeOf(TA));
end.
