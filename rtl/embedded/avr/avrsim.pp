{******************************************************************************
Startup code for an avr simulator

******************************************************************************}
unit avrsim;

  interface
    var
      OUTPUTREG   : byte absolute $20;
      EXITCODEREG : byte absolute $21;
      HALTREQUEST : byte absolute $22;
      EXCEPTIONJMPZERO : boolean absolute 52;

  implementation

    uses
      consoleio,heapmgr;

    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; public name '_haltproc'; noreturn;
      begin
        EXITCODEREG:=exitcode;
        HALTREQUEST:=1;
        { really stop }
        while true do
          ;
      end;

    var
      _data: record end; external name '__data_start';
      _edata: record end; external name '__data_end';
      _etext: record end; external name '_etext';
      _bss_start: record end; external name '__bss_start';
      _bss_end: record end; external name '__bss_end';
      _stack_top: record end; external name '_stack_top';
      __dtors_end: record end; external name '__dtors_end';

    procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
      asm
        jmp __dtors_end // Jump to last linker symbol before .init0
        // Interrupt vectors to be added here
      end;

    procedure _FPC_init_zeroreg_SP; assembler; nostackframe; noreturn; public name '_init_zeroreg_SP'; section '.init2';
      asm
{$ifdef CPUAVR_16_REGS}
        clr r17
{$else CPUAVR_16_REGS}
        clr r1
{$endif CPUAVR_16_REGS}
{$ifndef CPUAVR1}
        // load stack pointer
        ldi r30,lo8(_stack_top)
        out 0x3d,r30
        ldi r30,hi8(_stack_top)
        out 0x3e,r30
{$endif CPUAVR1}
      end;


    procedure _FPC_copy_data; assembler; nostackframe; noreturn; section '.init4';
      asm
        // Initialize .data section
        ldi XL,lo8(_data)
        ldi XH,hi8(_data)
        ldi YH,hi8(_edata)
        ldi ZL,lo8(_etext)
{$ifdef CPUAVR_16_REGS}
        ldi ZH,hi8(_etext)+(0x40) // program memory mapped to $4000 in data space
{$else CPUAVR_16_REGS}
        ldi ZH,hi8(_etext)
{$endif CPUAVR_16_REGS}

        rjmp .LCopyDataLoopEntry
.LCopyDataLoop:
{$ifdef CPUAVR_16_REGS}
        ld r16, Z+
{$else CPUAVR_16_REGS}
        lpm r16, Z+
{$endif CPUAVR_16_REGS}
        st X+, r16

.LCopyDataLoopEntry:
        cpi XL, lo8(_edata)
        cpc XH, YH
        brne .LCopyDataLoop

        // Zero .bss section
        ldi XL,lo8(_bss_start)
        ldi XH,hi8(_bss_start)
        ldi YH,hi8(_bss_end)

{$ifdef RELBRANCHES}
        rjmp .LZeroBssLoopEntry
{$else RELBRANCHES}
        jmp .LZeroBssLoopEntry
{$endif RELBRANCHES}
.LZeroBssLoop:
{$ifdef CPUAVR_16_REGS}
        st X+, r17
{$else CPUAVR_16_REGS}
        st X+, r1
{$endif CPUAVR_16_REGS}

.LZeroBssLoopEntry:
        cpi XL, lo8(_bss_end)
        cpc XH, YH
        brne .LZeroBssLoop
      end;

    procedure _FPC_jmp_main; noreturn; assembler; nostackframe; section '.init9';
      asm
{$ifdef RELBRANCHES}
        rjmp PASCALMAIN
{$else RELBRANCHES}
        jmp PASCALMAIN
{$endif RELBRANCHES}
      end;


    function WriteChar(ACh: char; AUserData: pointer): boolean;
      begin
        WriteChar:=true;
        OUTPUTREG:=ord(ACh);
      end;


    function ReadChar(var ACh: char; AUserData: pointer): boolean;
      begin
        ReadChar:=true;
        ACh:=#0;
      end;


begin
  EXCEPTIONJMPZERO:=true;
  OpenIO(Input, @WriteChar, @ReadChar, fmInput, nil);
  OpenIO(Output, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(ErrOutput, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(StdOut, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(StdErr, @WriteChar, @ReadChar, fmOutput, nil);
end.
