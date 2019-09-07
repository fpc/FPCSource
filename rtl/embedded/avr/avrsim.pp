{******************************************************************************
Startup code for an avr simulator

******************************************************************************}
unit avrsim;

{$goto on}
{$macro on}

  interface
    var
      OUTPUTREG   : byte absolute $20;
      EXITCODEREG : byte absolute $21;
      HALTREQUEST : byte absolute $22;
      EXCEPTIONJMPZERO : boolean absolute 52;

    {$define DOCALL:=call}
    {$define DOJMP:=jmp}

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

    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        .init
        .globl _start
        jmp _start

        {
          all ATMEL MCUs use the same startup code, the details are
          governed by defines
        }
        {$i start.inc}
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
