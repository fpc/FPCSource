{ Old file: tbs0163.pp }
{ missing <= and >= operators for sets.                 OK 0.99.11 (JM) }

Program test;

{ shows missing <= and >= for sets }

Type
     Days = (Monday,tuesday,wednesday,thursday,friday,saturday,sunday);

Var
     FreeDays,Weekend : set of days;

begin
   Weekend := [saturday, sunday];
   FreeDays := [friday, saturday, sunday];
   If (Weekend <= Freedays) then
       Writeln ('Free in weekend !');
end.
