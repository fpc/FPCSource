{ %OPT=-gl -OG1 -S2cgi }
program Project1;

{$mode objfpc}{$H+}

procedure my_very_looooooooong_idenfier_procedure_1;

   procedure my_very_looooooooong_idenfier_procedure_2;

     procedure my_very_looooooooong_idenfier_procedure_3;
     begin
        // bug
     end;

   begin
     my_very_looooooooong_idenfier_procedure_3;
   end;

begin
   my_very_looooooooong_idenfier_procedure_2;
end;

begin
   my_very_looooooooong_idenfier_procedure_1;
end.
