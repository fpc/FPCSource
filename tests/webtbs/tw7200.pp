{$mode delphi}

var i : integer;

function GetInt : integer;
begin
 Result := 10;
end;


 var myfunc : function : integer;


begin

 myfunc := GetInt;

 //i := integer(myfunc) div 2; //works
 //i := myfunc; i := i div 2; //works
 i := myfunc div 2; //does not work
 if (i <> 5) then
   halt(1);

 i := myfunc shr 2;
 if i <> 2 then
   halt(2);

 i := not myfunc;
 if i <> not(integer(10)) then
   halt(3);

end.
