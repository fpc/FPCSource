{******************************************************************************
Startup code for xtensa-esp32 using idf

******************************************************************************}
unit esp32;

{$goto on}
{$macro on}

  interface

  implementation

    uses
      consoleio,heapmgr;

    var
      _stack_top: record end; public name '_stack_top';

    procedure PASCALMAIN; external name 'PASCALMAIN';
    
    procedure esp_deep_sleep_start;external;
    procedure putchar(c : char);external;
    function getchar : char;external;
    
    procedure _FPC_haltproc; public name '_haltproc';noreturn;
      begin
        while true do
           esp_deep_sleep_start;
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
