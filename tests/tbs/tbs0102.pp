{ assembler reader of m68k for register ranges }

unit tbs0102;
  interface

  implementation

{$ifdef M68K}
    procedure int_help_constructor;

      begin
         asm
            movem.l d0-a7,-(sp)
         end;
      end;
{$endif M68K}


  end.
