{$mode delphi}

Procedure test;

Var
  V : Variant;
  SS : String [80];

Begin
   V := 'Hello';
   SS := V;
   if (ss<>'Hello') then
     halt(1);
End;


begin
  test;
end.
