var vlCnt:longint;
begin
   vlCnt := 10;
   case vlCnt of
   7: writeln(7);
   12,13: begin
            writeln('Case codegeneration error!');
            halt(1);
          end;
   11:writeln(11);
   end;
end.
