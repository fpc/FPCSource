{ Old file: tbs0029.pp }
{  tests typeof(object type)                         OK 0.99.1 (FK) }

type
  TA = object
    constructor init;
    procedure test;virtual;
  end;

  constructor TA.init;
    begin
    end;

  procedure TA.test;
    begin
    end;

var
   P: Pointer;

begin
   P := pointer(TypeOf(TA));
end.
