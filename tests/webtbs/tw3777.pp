{ Source provided for Free Pascal Bug Report 3777 }
{ Submitted by "David Fuchs" on  2005-03-12 }
{ e-mail: drfuchs@yahoo.com }
{$mode delphi}
{$T+}
type
  mytype = record
   myfield: ^PChar;
   end;

procedure fyl_use_ptrs(var f: mytype; var myparm);
begin
  f.myfield := addr(myparm);
end;

begin
end.
