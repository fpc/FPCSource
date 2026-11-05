{ %OPT=-O3 }
{ global variables kept in registers must be initialized with zero if they are
  live at the start of the program, also if they are read only by the condition
  of a repeat loop or after it and the loop body is entered by a goto }
program tb0734;

{$goto on}

label
  again;

var
  i, x : longint;
  second : boolean;

begin
  { occupy registers }
  i:=paramcount+$12345;
  if i=0 then
    halt(2);
  i:=0;
  repeat
  again:
    inc(i);
  until (i>=3) or (x<>0);
  if not(second) then
    begin
      second:=true;
      goto again;
    end;
  if i<>4 then
    halt(1);
  writeln('ok');
end.
