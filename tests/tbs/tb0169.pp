{ Old file: tbs0202.pp }
{ flag results not supported with case                  OK 0.99.11 (PFV) }

program silly;

var greater : boolean;

procedure error;
begin
   Writeln('Error in tbs0202');
   Halt(1);
end;

procedure compare(i,j : integer);
begin
   case (i>j) of
     true : begin
                greater:=true;
            end;
     false : begin
                greater:=false;
             end;
   end;
end;

begin
  compare(45,2);
  if not greater then
    error;
  compare(-5,26);
  if greater then
    error;
end.
