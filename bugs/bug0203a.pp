unit bug0203a;

interface
   procedure a;
   procedure c;

   const is_called : boolean = false;

implementation

   procedure c;
     begin
        a;
     end;

   procedure b;
     begin
        { call to a }
        asm
	   .globl _assembler_a
_assembler_a:
        end;
        Writeln('b called'); 
        Is_called:=true;
     end;

   procedure a;external name '_assembler_a';

end.

