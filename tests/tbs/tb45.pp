{ Old file: tbs0050.pp }
{  can't set a function result in a nested procedure of a function OK 0.99.7 (PM) }

function Append : Boolean;

      procedure DoAppend;
        begin
           Append := true;
        end;

begin
   Append:=False;
   DoAppend;
end;

begin
  If not Append then
    begin
       Writeln('TBS0050 fails');
       Halt(1);
    end;
end.
