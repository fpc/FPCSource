    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';noreturn;
      asm
        cli
      .Lhalt:
{$ifdef RELBRANCHES}
        rjmp .Lhalt
{$else RELBRANCHES}
        jmp .Lhalt
{$endif RELBRANCHES}
      end;

    procedure Default_IRQ_handler; assembler; nostackframe; public name '_Default_IRQ_handler';noreturn;
      asm
      .Lloop:
{$ifdef RELBRANCHES}
        rjmp .Lloop
{$else RELBRANCHES}
        jmp .Lloop
{$endif RELBRANCHES}
      end;

    var
      _data: record end; external name '__data_start';
      _edata: record end; external name '__data_end';
      _etext: record end; external name '_etext';
      _bss_start: record end; external name '__bss_start';
      _bss_end: record end; external name '__bss_end';
      _stack_top: record end; external name '_stack_top';
