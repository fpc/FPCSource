{$ifndef __G_BACKTRACE_H__}
{$define __G_BACKTRACE_H__}

//{$include gtypes.inc}

{ Fatal error handlers.
   g_on_error_query() will prompt the user to either
   [E]xit, [H]alt, [P]roceed or show [S]tack trace.
   g_on_error_stack_trace() invokes gdb, which attaches to the current
   process and shows a stack trace.
   These function may cause different actions on non-unix platforms.
   The prg_name arg is required by gdb to find the executable, if it is
   passed as NULL, g_on_error_query() will try g_get_prgname().
}

  procedure g_on_error_query (prg_name:Pgchar);cdecl;external gliblib name 'g_on_error_query';
  procedure g_on_error_stack_trace(prg_name:Pgchar);cdecl;external gliblib name 'g_on_error_stack_trace';

{ Hacker macro to place breakpoints for elected machines.
  Actual use is strongly discouraged of course ;)

}

{*  Not implemented *}


{$endif} { __G_BACKTRACE_H__  }

