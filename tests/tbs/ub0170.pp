{ Old file: tbs0203a.pp }
{  }

unit ub0170;

interface

   procedure a;external name '_assembler_a';
   procedure c;

   const is_called : boolean = false;

implementation

   procedure c;
     begin
        a;
     end;

   procedure b;[public, alias : '_assembler_a'];
     begin
        Writeln('b called');
        Is_called:=true;
     end;

end.
