{******************************************************************************
Startup code for xtensa-esp32 using idf

******************************************************************************}
unit esp32;

{$goto on}
{$macro on}

  interface

{$linklib esp32,static}
{$linklib soc,static}
{$linklib driver,static}
{$linklib freertos,static}
{$linklib log,static}
{$linklib esp_common,static}
{$linklib heap,static}
{$linklib newlib,static}
{$linklib vfs,static}
{$linklib esp_ringbuf,static}
{$linklib spi_flash,static}
{$linklib app_update,static}
{$linklib xtensa,static}
{$linklib bootloader_support,static}
{$linklib pthread,static}
{$linklib hal,static}
{$linklib libm,static}
{$linklib libg,static}
{$linklib c,static}
{$linklib esp_event,static}

  implementation

    uses
      consoleio,heapmgr;

    var
      _stack_top: record end; public name '_stack_top';
      operatingsystem_result: longint; external name 'operatingsystem_result';

    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure esp_deep_sleep_start;external;
    procedure putchar(c : char);external;
    function getchar : char;external;
    function __getreent : pointer;external;
    procedure fflush(f : pointer);external;
    procedure vTaskDelay(xTicksToDelay: uint32); external;

    procedure flushOutput(var t : TextRec);
      begin
        fflush(ppointer(__getreent+8)^);
      end;


    procedure _FPC_haltproc; public name '_haltproc';noreturn;
      begin
        if operatingsystem_result <> 0 then
          writeln('Runtime error ', operatingsystem_result);

        writeln('_haltproc called, exit code: ',operatingsystem_result);
        flushOutput(TextRec(Output));
        repeat
          // Allow other tasks to run
          // Do not enter deep sleep, can lead to problems with flashing
          vTaskDelay(1000);
        until false;
      end;


    procedure app_main;public name 'app_main';noreturn;
      begin
        PASCALMAIN;
        _FPC_haltproc;
      end;


    function WriteChar(ACh: char; AUserData: pointer): boolean;
      begin
        WriteChar:=true;
        putchar(ACh);
      end;


    function ReadChar(var ACh: char; AUserData: pointer): boolean;
      begin
        ReadChar:=true;
        ACh:=getchar;
      end;

begin
  OpenIO(Input, @WriteChar, @ReadChar, fmInput, nil);
  OpenIO(Output, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(ErrOutput, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(StdOut, @WriteChar, @ReadChar, fmOutput, nil);
  OpenIO(StdErr, @WriteChar, @ReadChar, fmOutput, nil);
end.
