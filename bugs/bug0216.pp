type rec = record
         a : Longint;
         b : Longint;
         c : Longint;
         d : record
           e : Longint;
           f : Word;
         end;
         g : Longint;
     end;

const r : rec = (
        a : 100; b : 200; c : 300; d : (e : 20; f : 30); g : 10);


begin
     with r do begin
          Writeln('A : ', a);
          Writeln('B : ', b);
          Writeln('C : ', c);
          Writeln('D');
          with d do begin
               Writeln('E : ', e);
               Writeln('F : ', f);
          end;
          Writeln('G : ', g);
     end;
end.
