{ $OPT= -Tamiga }

unit tbs0102;
  interface

  implementation

    procedure int_help_constructor;

      begin
         asm
            movem.l d0-a7,-(sp)
         end;
      end;


  end.
