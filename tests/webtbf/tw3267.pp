{ %fail }

{ Source provided for Free Pascal Bug Report 3267 }
{ Submitted by "Karoly Balogh" on  2004-08-22 }
{ e-mail: charlie@scenergy.dfmk.hu }
program test;

const VALUE : cardinal = 1;

procedure bug(var p: array of cardinal);
begin
end;

begin
 bug([VALUE]);
end.
