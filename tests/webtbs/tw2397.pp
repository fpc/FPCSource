{ Source provided for Free Pascal Bug Report 2397 }
{ Submitted by "Yakov Sudeikin" on  2003-02-23 }
{ e-mail: yashka@exebook.com }

{$mode delphi}

var
 a: array of string = nil; //Only works in Delphi
begin
 a := nil; //works in FPC and Delphi
end.
