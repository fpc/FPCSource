{ %CPU=m68k }

{ Old file: tbs0102.pp }
{ page fault when trying to compile under ppcm68k       OK 0.99.1 }

{ assembler reader of m68k for register ranges }

unit tb0086;
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
