{ Source provided for Free Pascal Bug Report 2072 }
{ Submitted by "Bill Rayer" on  2002-08-09 }
{ e-mail: lingolanguage@hotmail.com }
program tw2072;



(*$ifdef FPC*)
const name = 'FPC';
(*$else *)(*Hello*)
const name = 'Delphi';
(*$endif *)



begin
  writeln ('Hello ', name);
end.
