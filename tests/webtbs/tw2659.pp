{ Source provided for Free Pascal Bug Report 2659 }
{ Submitted by "Tomas Hajny" on  2003-08-31 }
{ e-mail: hajny@freepascal.org }
{ If the X- is removed then it compiles OK }

 {$X-}

procedure Test (S: string);
begin
end;

begin
 Test (Copy ('A', 1, 1));
end.
