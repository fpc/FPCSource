{ %FAIL }

unit tw1316;
interface

type
  searchrec=record
    l : longint;
  end;

operator [] (a,b:searchrec) r:boolean;

implementation

operator [] (a,b:searchrec) r:boolean;
begin
end;

end.
