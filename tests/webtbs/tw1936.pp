var
   X,Y,X1,Y1,X2,Y2,Style: byte;

begin
   X := 4;
   Y := 5;
   X1 := 3;
   X2 := 5;
   Y1 := 4;
   Y2 := 6;
   Style := 7;

   if ((Style=0) and (X in [X1..X2]) and (Y in [Y1..Y2]))
   or ((Style=6) and (X in [succ(X1)..pred(X2)]) and (Y in [Y1+3..pred(Y2)]))
   or ((Style <> 0) and (Style <> 6) and  (X in [succ(X1)..pred(X2)]) and (Y in [succ(Y1)..pred(Y2)]))
   then
   begin
      writeln ('OK');
   end
   else
   begin
     writeln('ERROR');
     halt(1);
   end;
end.
