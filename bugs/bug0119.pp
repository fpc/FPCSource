program ObjTest;
   uses crt;

   type
     ObjectA = object
       procedure Greetings;
       procedure DoIt;
     end;
     ObjectB = object (ObjectA)
       procedure Greetings;
       procedure DoIt;
     end;

   procedure ObjectA.Greetings;
   begin
     writeln('  A');
   end;
   procedure ObjectA.DoIt;
   begin
     writeln('A ');
     Greetings;
   end;

   procedure ObjectB.Greetings;
   begin
     writeln('  B');
   end;
   procedure ObjectB.DoIt;
   begin
     writeln('B');
     Greetings;
   end;

   var
     A: ObjectA;
     B: ObjectB;

   begin
     A.DoIt;
     B.DoIt;
     writeln; writeln('Now doing it directly:');
     A.Greetings;
     B.Greetings;
   end.