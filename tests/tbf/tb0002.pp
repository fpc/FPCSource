{ %FAIL }
{ Old file: tbf0029.pp }
{  tests typeof(object type)                         OK 0.99.1 (FK) }

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
