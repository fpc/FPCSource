Program Example51;

{ Program to demonstrate the Reset function. }

Function FileExists (Name : String) : boolean;

Var F : File;

begin
  {$i-}
  Assign (F,Name);
  Reset (F);
  {$I+}
  FileExists:=(IoResult=0) and (Name<>'');
  Close (f);
end;

begin
  If FileExists (Paramstr(1)) then
    Writeln ('File found')
  else
    Writeln ('File NOT found');
end.
