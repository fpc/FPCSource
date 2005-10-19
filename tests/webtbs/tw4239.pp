{ Source provided for Free Pascal Bug Report 4239 }
{ Submitted by "Lars" on  2005-07-30 }
{ e-mail: L@z505.com }
program Project1;

{$mode objfpc}{$H+}

var
  MyProc: array of procedure(s:string);

procedure testing(s:string);
begin
  writeln(s);
end;

begin
  setlength(myproc,1);
  MyProc[0]:=@testing;
  MyProc[0]('Test me');
end.
