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
