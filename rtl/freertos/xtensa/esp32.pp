{******************************************************************************
Startup code for xtensa-esp32 using idf

******************************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit esp32;
{$ENDIF FPC_DOTTEDUNITS}

{$goto on}
{$macro on}

  interface

  implementation

{$IFDEF FPC_DOTTEDUNITS}
    uses
      EmbeddedApi.ConsoleIO,EmbeddedApi.HeapMGR;
{$ELSE FPC_DOTTEDUNITS}
    uses
      consoleio,heapmgr;
{$ENDIF FPC_DOTTEDUNITS}

    var
      _stack_top: record end; public name '_stack_top';
      operatingsystem_result: longint; external name 'operatingsystem_result';

    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure esp_deep_sleep_start;external;
    procedure putchar(c : AnsiChar);external;
    function getchar : AnsiChar;external;
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


    function WriteChar(ACh: AnsiChar; AUserData: pointer): boolean;
      begin
        WriteChar:=true;
        putchar(ACh);
      end;


    function ReadChar(var ACh: AnsiChar; AUserData: pointer): boolean;
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
