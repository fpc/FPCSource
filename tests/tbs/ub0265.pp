{ Old file: tbs0308a.pp }
{ problem with objects that don't have VMT nor variable fields OK 0.99.13 (FK) }

unit ub0265;

interface

type
  tcourses = object
    function index(cName: string): integer;
    function name(cIndex: integer): string;
  end;

var coursedb: tcourses;
    l: longint;

implementation

function tcourses.index(cName: string): integer;
begin
  index := byte(cName[0]);
end;

function tcourses.name(cIndex: integer): string;
begin
  name := char(byte(cIndex));
end;

end.
