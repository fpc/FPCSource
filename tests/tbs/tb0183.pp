{ Old file: tbs0216.pp }
{ problem with with fields as function args            OK 0.99.11 (PM) }

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
          if a<>100 then halt(1);
          Writeln('B : ', b);
          if b<>200 then halt(1);
          Writeln('C : ', c);
          if c<>300 then halt(1);
          Writeln('D');
          with d do begin
               Writeln('E : ', e);
               if e<>20 then halt(1);
               Writeln('F : ', f);
               if f<>30 then halt(1);
          end;
          Writeln('G : ', g);
          if g<>10 then halt(1);
     end;
end.
