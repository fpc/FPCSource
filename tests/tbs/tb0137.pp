{ Old file: tbs0162.pp }
{ continue in repeat ... until loop doesn't work correct OK 0.99.8 (PFV) }

var
   i : longint;

begin
   i:=1;
   repeat
     continue;
   until i=1;
end.
