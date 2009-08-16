

var
  S : string;
  i : longint;
  err : word;
begin
  S:='';
  val(S,i,err);
  if err=0 then
    begin
      Writeln('Error: empty string is a valid input for val function');
      Halt(1);
    end
  else
    begin
      Writeln('Correct: empty string is a not valid input for val function');
    end;
  S:=#0;
  val(S,i,err);
  if err=0 then
    begin
      Writeln('Error: #0 string is a valid input for val function');
      Halt(1);
    end
  else
    begin
      Writeln('Correct: #0 string is a not valid input for val function');
    end;
end.