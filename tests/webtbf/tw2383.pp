{ %fail }

{ Source provided for Free Pascal Bug Report 2383 }
{ Submitted by "Anton Roolaid" on  2003-02-18 }
{ e-mail: Anton.Roolaid@mail.ee }
program VarParameterLoop;

const
  Elements = 3; { Array size }
  Arr: array [0 .. Elements - 1] of char = ('B', 'U', 'G');

function FindIndex(chElement: char; var iIndex: integer): boolean;
begin
  FindIndex := false; { Not found }
  { Using a variable parameter should not be allowed }
  for iIndex := 0 to Elements - 1 do
    if Arr[iIndex] = chElement then exit(true) { Found }
end;

var
 i: integer;
begin
  if FindIndex('U', i) then
    writeln('The index is ', i)
  else
    writeln('Not found')
end.
