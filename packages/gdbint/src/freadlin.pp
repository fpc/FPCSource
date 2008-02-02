unit freadlin;
{**********************************************************************
    Copyright (c) 2007 by Daniel Mantione

    A fake read line library which allows us to use libgdb in the IDE
    without linking the bloated and unused GNU readline library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$calling cdecl}

interface

implementation

uses ctypes;

var rl_end:cint;public name 'rl_end';               {The number of characters in the readline buffer.}
    rl_point:cint;public name 'rl_point';           {The cursor position in the readline buffer.}
    rl_prompt:Pchar;public name 'rl_prompt';        {The prompt readline should use.}
    rl_instream:pointer;public name 'rl_instream';  {The FILE* for input.}
    rl_outstream:pointer;public name 'rl_outstream';{The FILE* for output.}
    rl_terminal_name:pointer;public name 'rl_terminal_name'; {The terminal (set by TERM) readline thinks it is using.}
    rl_prep_term_function:pointer;public name 'rl_prep_term_function'; {Procedure to initialize terminal.}
    rl_getc_function:pointer;public name 'rl_getc_function'; {The function to get a char from input.}
    rl_line_buffer:Pchar;public name 'rl_line_buffer'; {The buffer readline is currently reading into.}
    rl_completer_word_break_characters:Pchar;public name 'rl_completer_word_break_characters';
    rl_completer_quote_characters:Pchar;public name 'rl_completer_quote_characters';
    rl_already_prompted:cint;public name 'rl_already_prompted';
    readline_echoing_p:cint;public name 'readline_echoing_p';
    rl_startup_hook:pointer;public name 'rl_startup_hook';
    emacs_ctlx_keymap:pointer;public name 'emacs_ctlx_keymap';
    rl_readline_name:Pchar;public name 'rl_readline_name';
    rl_deprep_term_function:pointer;public name 'rl_deprep_term_function';
    rl_redisplay_function:pointer;public name 'rl_redisplay_function';
    rl_pre_input_hook:pointer;public name 'rl_pre_input_hook';
    rl_completion_entry_function:pointer;public name 'rl_completion_entry_function';
    rl_filename_completion_desired:cint;public name 'rl_filename_completion_desired';
    rl_completion_display_matches_hook:pointer;public name 'rl_completion_display_matches_hook';
    rl_completion_query_items:cint;public name 'rl_completion_query_items';
    rl_ignore_completion_duplicates:cint;public name 'rl_ignore_completion_duplicates';
    rl_print_completions_horizontally:cint;public name '_rl_print_completions_horizontally';

function rl_initialize:cint;public;alias:'rl_initialize';

{Should initialize readline and return 0 if successfull.}

begin
  runerror(254);
end;

function rl_reset_terminal(terminal:Pchar):cint;public;alias:'rl_reset_terminal';

begin
  {Called by gdb, do nothing.}
end;

function rl_tilde_expand(s:Pchar):Pchar;public;alias:'tilde_expand';

begin
  {Called by gdb, don't expand, return original string.}
  rl_tilde_expand:=s;
end;

function rl_newline(count,key:cint):cint;public;alias:'rl_newline';

begin
  runerror(254);
end;

procedure rl_get_screen_size(var rows,cols:cint);public;alias:'rl_get_screen_size';

begin
  {Called by gdb. Fake a 80x25 screen.}
  {Gdb can call using nil pointers.}
  if @rows<>nil then
    rows:=25;
  if @cols<>nil then
    cols:=80;
end;

procedure rl_set_screen_size(rows,cols:cint);public;alias:'rl_set_screen_size';

begin
  {Called by gdb, do nothing.}
end;

function rl_bind_key_in_map(key:cint;rl_command_func_t:pointer;map:pointer):cint;public;alias:'rl_bind_key_in_map';

begin
  runerror(254);
end;

procedure rl_set_keymap(keymap:pointer);public;alias:'rl_set_keymap';

begin
  runerror(254);
end;

function rl_get_keymap:pointer;public;alias:'rl_get_keymap';

begin
  runerror(254);
end;

function rl_make_bare_keymap:pointer;public;alias:'rl_make_bare_keymap';

begin
  runerror(254);
end;

function rl_add_defun(name:Pchar;rl_command_func_t:pointer;key:cint):cint;public;alias:'rl_add_defun';

begin
  {Called by gdb, do nothing.}
end;

function rl_insert(count,c:cint):cint;public;alias:'rl_insert';

begin
  runerror(254);
end;

function rl_kill_text(start,stop:cint):cint;public;alias:'rl_kill_text';

begin
  runerror(254);
end;

procedure rl_prep_terminal(meta_flag:cint);public;alias:'rl_prep_terminal';

begin
  runerror(254);
end;

procedure rl_deprep_terminal;public;alias:'rl_deprep_terminal';

begin
  runerror(254);
end;

procedure rl_callback_handler_install(prompt:Pchar;lhandler:pointer);public;alias:'rl_callback_handler_install';

begin
  runerror(254);
end;

procedure rl_callback_handler_remove;public;alias:'rl_callback_handler_remove';

begin
  runerror(254);
end;

function rl_filename_completion_function(text:Pchar;state:cint):Pchar;public;alias:'rl_filename_completion_function';

begin
  runerror(254);
end;

procedure rl_callback_read_char;public;alias:'rl_callback_read_char';

begin
  runerror(254);
end;

procedure rl_redisplay;public;alias:'rl_redisplay';

begin
  runerror(254);
end;

function rl_generic_bind(_type:cint;keyseq,data:Pchar;map:pointer):cint;public;alias:'rl_generic_bind';

begin
  runerror(254);
end;

function rl_get_previous_history(count,key:cint):cint;public;alias:'rl_get_previous_history';

begin
  runerror(254);
end;

function rl_read_key:cint;public;alias:'rl_read_key';

begin
  runerror(254);
end;

function rl_abort_internal:cint;public;alias:'_rl_abort_internal';

begin
  runerror(254);
end;

function readline(prompt:Pchar):Pchar;public;alias:'readline';

begin
  runerror(254);
end;

function rl_qsort_string_compare(s1,s2:Pchar):cint;public;alias:'_rl_qsort_string_compare';

begin
  runerror(254);
end;

begin
  rl_end:=0;
  rl_point:=0;
  rl_prompt:=nil;
  rl_instream:=nil;
  rl_outstream:=nil;
  rl_terminal_name:=nil;
  rl_already_prompted:=0;
  rl_completer_word_break_characters:=nil;
  rl_completer_quote_characters:=nil;
  rl_line_buffer:=nil;
  rl_getc_function:=nil;
  rl_prep_term_function:=nil;
  rl_startup_hook:=nil;
  readline_echoing_p:=0;
  emacs_ctlx_keymap:=nil;
  rl_readline_name:=nil;
  rl_deprep_term_function:=nil;
  rl_redisplay_function:=nil;
  rl_completion_entry_function:=nil;
  rl_filename_completion_desired:=0;
  rl_completion_display_matches_hook:=nil;
  rl_ignore_completion_duplicates:=0;
  rl_print_completions_horizontally:=0;
end.