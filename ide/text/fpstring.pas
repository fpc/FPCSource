{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Florian Klaempfl

    Strings for menus, dialogs etc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit fpstring;

  interface

    uses
       fpconst;

{$ifdef FPC}
    resourcestring
{$else}
    const
{$endif}
      { menu entries }
      menu_file              = '~F~ile';
      menu_file_new          = '~N~ew';
      menu_file_template     = 'New from ~t~emplate...';
      menu_file_open         = '~O~pen...';
      menu_file_save         = '~S~ave';
      menu_file_saveas       = 'Save ~a~s...';
      menu_file_saveall      = 'Save a~l~l';
      menu_file_changedir    = '~C~hange dir...';
      menu_file_dosshell     = '~D~OS shell';
      menu_file_exit         = 'E~x~it';

      menu_edit              = '~E~dit';
      menu_edit_copywin      = 'Cop~y~ to Windows';
      menu_edit_pastewin     = 'Paste from ~W~indows';
      menu_edit_undo         = '~U~ndo';
      menu_edit_redo         = '~R~edo';
      menu_edit_cut          = 'Cu~t~';
      menu_edit_copy         = '~C~opy';
      menu_edit_paste        = '~P~aste';
      menu_edit_clear        = 'C~l~ear';
      menu_edit_showclipboard= '~S~how clipboard';

      menu_search            = '~S~earch';
      menu_search_find       = '~F~ind...';
      menu_search_replace    = '~R~eplace...';
      menu_search_searchagain= '~S~earch again';
      menu_search_jumpline   = '~G~o to line number...';
      menu_search_findproc   = 'Find ~p~rocedure...';
      menu_search_objects    = '~O~bjects';
      menu_search_modules    = 'Mod~u~les';
      menu_search_globals    = 'G~l~obals';
      menu_search_symbol     = 'S~y~mbol';

      menu_run               = '~R~un';
      menu_run_run           = '~R~un';
      menu_run_stepover      = '~S~tep over';
      menu_run_traceinto     = '~T~race into';
      menu_run_conttocursor  = '~G~oto Cursor';
      menu_run_untilreturn   = '~U~ntil return';
      menu_run_parameters    = 'P~a~rameters...';
      menu_run_resetdebugger = '~P~rogram reset';

      menu_compile           = '~C~ompile';
      menu_compile_compile   = '~C~ompile';
      menu_compile_make      = '~M~ake';
      menu_compile_build     = '~B~uild';
      menu_compile_target    = '~T~arget...';
      menu_compile_primaryfile = '~P~rimary file...';
      menu_compile_clearprimaryfile = 'C~l~ear primary file';
      menu_compile_information = '~I~nformation...';
      menu_compile_compilermessages = 'C~o~mpiler messages';

      menu_debug             = '~D~ebug';
      menu_debug_output      = '~O~utput';
      menu_debug_userscreen  = '~U~ser screen';
      menu_debug_breakpoint  = '~B~reakpoint';
      menu_debug_callstack   = '~C~all stack';
      menu_debug_registers   = '~R~egisters';
      menu_debug_addwatch    = '~A~dd Watch';
      menu_debug_watches     = '~W~atches';
      menu_debug_breakpointlist = 'Breakpoint ~L~ist';
      menu_debug_gdbwindow   = '~G~DB window';

      menu_tools             = '~T~ools';
      menu_tools_messages    = '~M~essages';
      menu_tools_msgnext     = 'Goto ~n~ext';
      menu_tools_msgprev     = 'Goto ~p~revious';
      menu_tools_grep        = '~G~rep';
      menu_tools_calculator  = '~C~alculator';
      menu_tools_asciitable  = 'Ascii ~t~able';

      menu_options           = '~O~ptions';
      menu_options_mode      = 'Mode~.~..';
      menu_options_compiler  = '~C~ompiler...';
      menu_options_memory    = '~M~emory sizes...';
      menu_options_linker    = '~L~inker...';
      menu_options_debugger  = 'De~b~ugger...';
      menu_options_directories = '~D~irectories...';
      menu_options_browser   = 'Bro~w~ser...';
      menu_options_tools     = '~T~ools...';
      menu_options_env       = '~E~nvironment';
      menu_options_env_preferences = '~P~references...';
      menu_options_env_editor= '~E~ditor...';
      menu_options_env_desktop = '~D~esktop...';
      menu_options_env_mouse = '~M~ouse...';
      menu_options_env_startup = '~S~tartup...';
      menu_options_env_colors= '~C~olors';
      menu_options_open      = '~O~pen...';
      menu_options_save      = '~S~ave';
      menu_options_saveas    = 'Save ~a~s...';

      menu_window            = '~W~indow';
      menu_window_tile       = '~T~ile';
      menu_window_cascade    = 'C~a~scade';
      menu_window_closeall   = 'Cl~o~se all';
      menu_window_resize     = '~S~ize/Move';
      menu_window_zoom       = '~Z~oom';
      menu_window_next       = '~N~ext';
      menu_window_previous   = '~P~revious';
      menu_window_close      = '~C~lose';
      menu_window_list       = '~L~ist...';
      menu_window_update     = '~R~efresh display';

      menu_help              = '~H~elp';
      menu_help_contents     = '~C~ontents';
      menu_help_index        = '~I~ndex';
      menu_help_topicsearch  = '~T~opic search';
      menu_help_prevtopic    = '~P~revious topic';
      menu_help_using        = '~U~sing help';
      menu_help_files        = '~F~iles...';
      menu_help_about        = '~A~bout...';

      { short cut entries in menu }
      menu_key_file_open     = 'F3';
      menu_key_file_save     = 'F2';
      menu_key_file_exit     = 'Alt+X';

      menu_key_edit_undo     = 'Alt+BkSp';
      menu_key_edit_cut      = 'Shift+Del';
      menu_key_edit_copy     = 'Ctrl+Ins';
      menu_key_edit_paste    = 'Shift+Ins';
      menu_key_edit_clear    = 'Ctrl+Del';

      menu_key_run_run       = 'Ctrl+F9';
      menu_key_run_stepover  = 'F8';
      menu_key_run_traceinto = 'F7';
      menu_key_run_conttocursor = 'F4';
      menu_key_run_resetdebugger = 'Ctrl+F2';

      menu_key_compile_compile = 'Alt+F9';
      menu_key_compile_make = 'F9';
      menu_key_compile_compilermessages = 'F12';

      menu_key_debug_userscreen = 'Alt+F5';
      menu_key_debug_breakpoint = 'Ctrl+F8';
      menu_key_debug_callstack = 'Ctrl+F3';
      menu_key_debug_addwatch = 'Ctrl+F7';

      menu_key_tools_messages= 'F11';
      menu_key_tools_msgnext = 'Alt+F8';
      menu_key_tools_msgprev = 'Alt+F7';
      menu_key_tools_grep    = 'Shift+F2';

      menu_key_window_resize = 'Ctrl+F5';
      menu_key_window_zoom   = 'F5';
      menu_key_window_next   = 'F6';
      menu_key_window_previous = 'Shift+F6';
      menu_key_window_close  = 'Alt+F3';
      menu_key_window_list   = 'Alt+0';

      menu_key_help_helpindex= 'Shift+F1';
      menu_key_help_topicsearch = 'Ctrl+F1';
      menu_key_help_prevtopic= 'Alt+F1';

      { status line entries }
      status_help            = '~F1~ Help';
      status_help_on_help    = '~F1~ Help on help';
      status_help_previoustopic = '~Alt+F1~ Previous topic';
      status_help_index      = '~Shift+F1~ Help index';
      status_help_close      = '~Esc~ Close help';
      status_save            = '~F2~ Save';
      status_open            = '~F3~ Open';
      status_compile         = '~Alt+F9~ Compile';
      status_make            = '~F9~ Make';
      status_localmenu       = '~Alt+F10~ Local menu';
      status_transferchar    = '~Ctrl+Enter~ Transfer char';
      status_msggotosource   = '~'+EnterSign+'~ Goto source';
      status_msgtracksource  = '~Space~ Track source';
      status_close           = '~Esc~ Close';
      status_calculatorpaste = '~Ctrl+Enter~ Transfer result';

      { error messages }
      error_saving_cfg_file  = 'Error saving configuration.';
      error_saving_dsk_file  = 'Error saving desktop file.'#13+
                               'Desktop layout could not be stored.';
      error_user_screen_not_avail = 'Sorry, user screen not available.';


  implementation

end.
{
  $Log$
  Revision 1.1  2000-01-23 21:25:17  florian
    + start of internationalization support

}
