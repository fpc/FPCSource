
{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{$define newsignal}

{$I systemh.inc}
{$I heaph.inc}

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  sLineBreak : string[2] = #10;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;
  
