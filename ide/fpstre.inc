
{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Florian Klaempfl & Berczi Gabor

    Strings for menus, dialogs etc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
      menu_local_gotosource = '~G~oto source';
      menu_local_tracksource = '~T~rack source';
      menu_local_options = '~O~ptions...';
      menu_local_clear = '~C~lear';
      menu_local_saveas = 'Save ~a~s';

      menu_common_contents     = '~C~ontents';
      menu_common_index        = '~I~ndex';
      menu_common_topicsearch  = '~T~opic search';
      menu_common_prevtopic    = '~P~revious topic';
      menu_common_copy = '~C~opy';

      menu_key_common_helpindex = 'Shift+F1';
      menu_key_common_topicsearch = 'Ctrl+F1';
      menu_key_common_prevtopic = 'Alt+F1';
      menu_key_common_copy = 'Ctrl+Ins';

      { Symbol browser tabs }
      label_browsertab_scope = 'S';
      label_browsertab_reference = 'R';
      label_browsertab_inheritance = 'I';
      label_browsertab_memory = 'M';
      label_browsertab_unit = 'U';

{$ifdef USERESSTRINGS}
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
      menu_file_reload       = '~R~eload';
      menu_file_print        = '~P~rint';
      menu_file_printsetup   = 'Print s~e~tup';
      menu_file_changedir    = '~C~hange dir...';
      menu_file_dosshell     = 'Comman~d~ shell';
      menu_file_exit         = 'E~x~it';

      menu_edit              = '~E~dit';
      menu_edit_copywin      = 'Cop~y~ to Windows';
      menu_edit_pastewin     = 'Paste from ~W~indows';
      menu_edit_undo         = '~U~ndo';
      menu_edit_redo         = '~R~edo';
      menu_edit_cut          = 'Cu~t~';
      menu_edit_copy         = menu_common_copy;
      menu_edit_paste        = '~P~aste';
      menu_edit_clear        = 'C~l~ear';
      menu_edit_showclipboard= '~S~how clipboard';
      menu_edit_selectall    = 'Select ~A~ll';
      menu_edit_unselect     = 'U~n~select';

      menu_search            = '~S~earch';
      menu_search_find       = '~F~ind...';
      menu_search_replace    = '~R~eplace...';
      menu_search_searchagain= '~S~earch again';
      menu_search_jumpline   = '~G~o to line number...';
      menu_search_findproc   = 'Find ~p~rocedure...';
      menu_search_objects    = '~O~bjects';
      menu_search_modules    = 'Mod~u~les';
      menu_search_globals    = 'G~l~obals';
      menu_search_symbol     = 'S~y~mbol...';

      menu_run               = '~R~un';
      menu_run_run           = '~R~un';
      menu_run_continue      = '~C~ontinue';
      menu_run_stepover      = '~S~tep over';
      menu_run_traceinto     = '~T~race into';
      menu_run_conttocursor  = '~G~oto Cursor';
      menu_run_untilreturn   = '~U~ntil return';
      menu_run_rundir        = 'Run ~D~irectory...';
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
      menu_debug_remote      = '~S~end to remote';
      menu_debug_registers   = '~R~egisters';
      menu_debug_fpu_registers   = '~F~loating Point Unit';
      menu_debug_vector_registers   = '~V~ector Unit';
      menu_debug_addwatch    = '~A~dd Watch';
      menu_debug_watches     = '~W~atches';
      menu_debug_breakpointlist = 'Breakpoint ~L~ist';
      menu_debug_gdbwindow   = '~G~DB window';
      menu_debug_disassemble = '~D~isassemble';

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
      menu_options_remote    = '~R~emote...';
      menu_options_directories = '~D~irectories...';
      menu_options_browser   = 'Bro~w~ser...';
      menu_options_tools     = '~T~ools...';
      menu_options_env       = '~E~nvironment';
      menu_options_env_preferences = '~P~references...';
      menu_options_env_editor= '~E~ditor...';
      menu_options_env_codecomplete = 'Code~C~omplete...';
      menu_options_env_codetemplates = 'Code~T~emplates...';
      menu_options_env_desktop = '~D~esktop...';
      menu_options_env_mouse = '~M~ouse...';
      menu_options_env_startup = '~S~tartup...';
      menu_options_env_colors= '~C~olors';
      menu_options_learn_keys= 'Learn ~K~eys';
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
      menu_window_hide       = '~H~ide';
      menu_window_close      = '~C~lose';
      menu_window_list       = '~L~ist...';
      menu_window_update     = '~R~efresh display';

      menu_help              = '~H~elp';
      menu_help_contents     = menu_common_contents;
      menu_help_index        = menu_common_index;
      menu_help_topicsearch  = menu_common_topicsearch;
      menu_help_prevtopic    = menu_common_prevtopic;
      menu_help_using        = '~U~sing help';
      menu_help_files        = '~F~iles...';
      menu_help_about        = '~A~bout...';

      { Breakpoints window local menu items }
      menu_bplocal_gotosource  = menu_local_gotosource;
      menu_bplocal_editbreakpoint = '~E~dit breakpoint';
      menu_bplocal_newbreakpoint = '~N~ew breakpoint';
      menu_bplocal_deletebreakpoint = '~D~elete breakpoint';
      menu_bplocal_togglestate = '~T~oggle state';

      { Watches local menu items }
      menu_watchlocal_edit = '~E~dit watch';
      menu_watchlocal_new = '~N~ew watch';
      menu_watchlocal_delete = '~D~elete watch';

      { Symbol view local menu items }
      menu_symlocal_browse = '~B~rowse';
      menu_symlocal_gotosource = menu_local_gotosource;
      menu_symlocal_tracksource = menu_local_tracksource;
      menu_symlocal_options = menu_local_options;

      { Source editor local menu items }
      menu_srclocal_openfileatcursor = 'Open ~f~ile at cursor';
      menu_srclocal_browseatcursor = '~B~rowse symbol at cursor';
      menu_srclocal_topicsearch = 'Topic ~s~earch';
      menu_srclocal_options = menu_local_options;
      menu_srclocal_reload = '~R~eload modified file';
      { Help viewer local menu items }
      menu_hlplocal_contents = menu_common_contents;
      menu_hlplocal_index = menu_common_index;
      menu_hlplocal_topicsearch = menu_common_topicsearch;
      menu_hlplocal_prevtopic = menu_common_prevtopic;
      menu_hlplocal_copy = menu_common_copy;

      { Messages local menu items }
      menu_msglocal_clear = menu_local_clear;
      menu_msglocal_gotosource = menu_local_gotosource;
      menu_msglocal_tracksource = menu_local_tracksource;
      menu_msglocal_saveas = menu_local_saveas;

      { short cut entries in menu }
      menu_key_file_open     = 'F3';
      menu_key_file_save     = 'F2';
      menu_key_file_exit     = 'Alt+X';

      menu_key_edit_undo     = 'Alt+BkSp';
      menu_key_edit_cut      = 'Shift+Del';
      menu_key_edit_copy     = menu_key_common_copy;
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
      menu_key_window_hide   = 'Ctrl+F6';

      menu_key_help_helpindex= menu_key_common_helpindex;
      menu_key_help_topicsearch = menu_key_common_topicsearch;
      menu_key_help_prevtopic= menu_key_common_prevtopic;

      menu_key_hlplocal_index = menu_key_common_helpindex;
      menu_key_hlplocal_topicsearch = menu_key_common_topicsearch;
      menu_key_hlplocal_prevtopic = menu_key_common_prevtopic;
      menu_key_hlplocal_copy = menu_key_common_copy;

      dialog_openafile        = 'Open a file';
      label_filetoopen        = 'File to ope~n~';
      label_lookingfor        = 'Looking for %s';

      msg_confirmsourcediradd = 'Directory %s is not in search path for source files. '+
                                'Should we add it ?';

      dialog_fillintemplateparameter = 'Fill in template parameter';

      dialog_calculator       = 'Calculator';

      msg_quitconfirm         = 'Do You really want to quit?';

      msg_printerror = 'Error while printing';

      { CodeComplete dialog }
      dialog_codecomplete     = 'CodeComplete';
      label_codecomplete_keywords = '~K~eywords';

      dialog_codecomplete_add = 'Add new keyword';
      label_codecomplete_add_keyword = 'Keyword';

      dialog_codecomplete_edit = 'Edit keyword';
      label_codecomplete_edit_keyword = 'Keyword';

      { CodeTemplates dialog }
      dialog_codetemplates_add = 'Add new template';
      dialog_codetemplates_edit = 'Edit template';

      msg_codecomplete_alreadyinlist = '"%s" is already in the list';

      { File|New from template dialog }
      msg_notemplatesavailable = 'No templates available.';
      dialog_newfromtemplate   = 'New from template';
      label_availabletemplates = 'Available ~t~emplates';

      label_codetemplate_shortcut = '~S~hortcut';
      label_codetemplate_content = '~T~emplate content';
      label_codetemplate_templates = '~T~emplates';

      dialog_newtemplate = 'New template';
      dialog_modifytemplate = 'Modify template';
      msg_codetemplate_alreadyinlist = 'A template named "%s" is already in the list';

      dialog_compilermessages = 'Compiler Messages';

      dialog_compilingwithmode = 'Compiling  (%s mode)';

      { Breakpoints window }
      dialog_breakpointlist = 'Breakpoint list';
      label_breakpointpropheader = ' Type      | State   | Position          | Ignore | Conditions ';

      dialog_modifynewbreakpoint = 'Modify/New Breakpoint';
      label_breakpoint_name = '~N~ame';
      label_breakpoint_line = '~L~ine';
      label_breakpoint_conditions = '~C~onditions';
      label_breakpoint_ignorecount = '~I~gnore count';
      label_breakpoint_type = '~T~ype';

      { Watches window }
      dialog_watches = 'Watches';

      label_watch_expressiontowatch = '~E~xpression to watch';
      label_watch_values = 'Watch values';

      msg_watch_currentvalue = 'Current value: '+#13+
                               '%s';
      msg_watch_currentandpreviousvalue = 'Current value: '+#13+
                                          '%s'+#13+
                                          'Previous value: '+#13+
                                          '%s';

      dialog_registers = 'Register View';

      dialog_fpu = 'FPU View';
      dialog_vector = 'Vector Unit View';

      dialog_callstack = 'Call Stack';

      dialog_help = 'Help';
      msg_modalhelpnotimplemented = 'Sorry, modal help not yet implemented.';

      dialog_browse = 'Browse: %s';
      dialog_units = 'Units';

      dialog_browsesymbol = 'Browse Symbol';
      label_entersymboltobrowse = 'Enter S~y~mbol to browse';

      dialog_proceduredialog = 'Find Procedure';
      label_enterproceduretofind = 'Enter ~m~atching expr.';

      dialog_gdbwindow = 'GDB window';

      dialog_disaswindow = 'Disassembly window';

      dialog_clipboard = 'Clipboard';

      dialog_userscreen = 'User screen';

      dialog_messages = 'Messages';

      dialog_memorysizes = 'Memory sizes';

      dialog_directories = 'Directories';

      dialog_programparameters = 'Program parameters';
      label_parameters_parameter = '~P~arameter';

      dialog_setupprinter = 'Setup printer';
      label_setupprinter_device = '~D~evice';

      dialog_openoptions = 'Open Options';
      msg_cantopenconfigfile = 'Can''t open config file.';
      msg_errorsavingconfigfile = 'Error saving config file.';

      dialog_saveoptions = 'Save Options';
      dialog_ini_filename = 'Name of INI file';

      dialog_preferences = 'Preferences';
      label_preferences_videomode = 'Video mode';
      label_preferences_currentdirectory = 'C~u~rrent directory';
      label_preferences_configdirectory = 'Conf~i~g file directory';
      label_preferences_desktopfile = 'Desktop file';
      label_preferences_editorfiles = 'Editor ~f~iles';
      label_preferences_environment = '~E~nvironment';
      label_preferences_desktop = '~D~esktop';
      label_preferences_autosave = 'Auto save';
      label_preferences_autotracksource = '~A~uto track source';
      label_preferences_closeongotosource = 'C~l~ose on go to source';
      label_preferences_changedironopen = 'C~h~ange dir on open';
      label_preferences_options = 'Options';

      dialog_defaulteditoroptions = 'Default Editor Options';
      dialog_editoroptions = 'Editor Options';
      label_editor_backupfiles = 'Create backup ~f~iles';
      label_editor_insertmode = '~I~nsert mode';
      label_editor_autoindentmode = '~A~uto indent mode';
      label_editor_usetabcharacters = '~U~se tab characters';
      label_editor_backspaceunindents = '~B~ackspace unindents';
      label_editor_persistentblocks = '~P~ersistent blocks';
      label_editor_syntaxhighlight = '~S~yntax highlight';
      label_editor_blockinsertcursor = 'B~l~ock insert cursor';
      label_editor_verticalblocks = '~V~ertical blocks';
      label_editor_highlightcolumn = 'Highlight ~c~olumn';
      label_editor_highlightrow = 'Highlight ~r~ow';
      label_editor_autoclosingbrackets = 'Aut~o~-closing brackets';
      label_editor_keeptrailingspaces = '~K~eep trailing spaces';
      label_editor_codecomplete = 'Co~d~eComplete enabled';
      label_editor_folds = 'E~n~able folds';
      label_editor_editoroptions = '~E~ditor options';
      label_editor_tabsize = '~T~ab size';
      label_editor_indentsize = 'Indent si~z~e';
      label_editor_highlightextensions = '~H~ighlight extensions';
      label_editor_filepatternsneedingtabs = 'File ~p~atterns needing tabs';

      dialog_browseroptions = 'Browser Options';
      dialog_localbrowseroptions = 'Local Browser Options';
      label_browser_labels = '~L~abels';
      label_browser_constants = '~C~onstants';
      label_browser_types = '~T~ypes';
      label_browser_variables = '~V~ariables';
      label_browser_procedures = '~P~rocedures';
      label_browser_inherited = '~I~nherited';
      label_browser_symbols = 'Symbols';
      label_browser_newbrowser = '~N~ew browser';
      label_browser_currentbrowser = '~R~eplace current';
      label_browser_subbrowsing = 'Sub-browsing';
      label_browser_scope = '~S~cope';
      label_browser_reference = 'R~e~ference';
      label_browser_preferredpane = 'Preferred pane';
      label_browser_qualifiedsymbols = '~Q~ualified symbols';
      label_browser_sortsymbols = 'S~o~rt always';
      label_browser_display = 'Display';

      dialog_desktoppreferences = 'Desktop Preferences';
      label_desktop_historylists = '~H~istory lists';
      label_desktop_clipboard = '~C~lipboard content';
      label_desktop_watches = '~W~atch expressions';
      label_desktop_breakpoints = '~B~reakpoints';
      label_desktop_openwindow = '~O~pen windows';
      label_desktop_symbolinfo = '~S~ymbol information';
      label_desktop_codecompletewords = 'Co~d~eComplete wordlist';
      label_desktop_codetemplates = 'Code~T~emplates';
      label_desktop_preservedacrosssessions = '~P~reserved across sessions';

      dialog_mouseoptions = 'Mouse Options';
      label_mouse_speedbar = 'Fast       Medium      Slow';
      label_mouse_doubleclickspeed = '~M~ouse double click';
      label_mouse_reversebuttons = '~R~everse mouse buttons';
      label_mouse_crtlrightmousebuttonaction = 'Ctrl+Right mouse button';
      label_mouse_altrightmousebuttonaction = 'Alt+Right mouse button';
      label_mouse_act_nothing = '~N~othing';
      label_mouse_act_topicsearch = '~T~opic search';
      label_mouse_act_gotocursor = '~G~o to cursor';
      label_mouse_act_breakpoint = '~B~reakpoint';
      label_mouse_act_evaluate = '~E~valuate';
      label_mouse_act_addwatch = '~A~dd watch';
      label_mouse_act_browsesymbol = 'Browse ~s~ymbol';

      label_colors_grp_browser = 'Browser';
      label_colors_framepassive = 'Frame passive';
      label_colors_frameactive = 'Frame active';
      label_colors_frameicon = 'Frame icon';
      label_colors_scrollbarpage = 'Scroll bar page';
      label_colors_scrollbaricons = 'Scroll bar icons';
      label_colors_normaltext = 'Normal text';
      label_colors_selectedtext = 'Selected text';
      label_colors_activeitem = 'Active item';
      label_colors_inactiveitem = 'Inactive item';
      label_colors_focuseditem = 'Focused item';
      label_colors_selecteditem = 'Selected item';
      label_colors_divider = 'Divider';
      label_colors_grp_clock = 'Clock';
      label_colors_clockview = 'clock view';
      label_colors_grp_desktop = 'Desktop';
      label_colors_grp_dialogs = 'Dialogs';
      label_colors_grp_editor = 'Editor';
      label_colors_highlighcolumn = 'Highlight column';
      label_colors_highlightrow = 'Highlight row';
      label_colors_errormessages = 'Error messages';
      label_colors_grp_help = 'Help';
      label_colors_helptext = 'Help text';
      label_colors_helplinks = 'Help links';
      label_colors_selectedlink = 'Selected link';
      label_colors_grp_menus = 'Menus';
      label_colors_grp_syntax = 'Syntax';
      label_colors_whitespace = 'Whitespace';
      label_colors_comments = 'Comments';
      label_colors_reservedwords = 'Reserved words';
      label_colors_identifiers = 'Identifiers';
      label_colors_strings = 'Strings';
      label_colors_numbers = 'Numbers';
      label_colors_hexnumbers = 'Hex numbers';
      label_colors_assembler = 'Assembler';
      label_colors_symbols = 'Symbols';
      label_colors_directives = 'Directives';
      label_colors_tabs = 'Tabs';
      label_colors_html_heading1 = 'HTML Heading 1';
      label_colors_html_heading2 = 'HTML Heading 2';
      label_colors_html_heading3 = 'HTML Heading 3';
      label_colors_html_heading4 = 'HTML Heading 4';
      label_colors_html_heading5 = 'HTML Heading 5';
      label_colors_html_heading6 = 'HTML Heading 6';

      dialog_linker = 'Linker';
      label_linker_preferredlibtype = 'Preferred library type';

      dialog_remote = 'Remote machine configuration';
      dialog_debugger = 'Browsing/Debugging/Profiling';
      label_debugger_debuginfo = 'Debugging information';
      label_debugger_profileswitches = 'Profiling Switches';
      label_debugger_compilerargs = 'Additional ~c~ompiler args';
      label_debugger_useanotherconsole = '~U~se another console';
      label_debugger_redirection = 'Debuggee ~R~edirection';
      label_debugger_useanothertty = '~U~se Another tty for Debuggee';

      label_remote_machine = 'Remote ~m~achine name';
      label_remote_port = 'Remote ~p~ort number';
      label_remote_config = 'Remote con~f~ig file';
      label_remote_ident = 'Remote ~i~dent file';
      label_remote_dir = 'Remote ~d~irectory';
      label_remote_command = 'Remote ~c~ommand';

      dialog_helpfiles = 'Install Help Files';
      label_helpfiles_helpfiles = '~H~elp files';

      dialog_installhelpfile = 'Install a help file';
      label_installhelpfile_filename = '~H~elp file name';

      msg_createkeywordindexforhelpfile = 'Create keyword index from help file?';

      dialog_topictitle = 'Topic title';
      label_topictitle_title = 'Title';

      msg_pleasewaitwhilecreatingindex = 'Please wait while creating index...';
      msg_buildingindexfile = 'Building index file %s';
      msg_filedoesnotcontainanylinks = '%s doesn''t contain any links, thus it isn''t suitable for indexing.';
      msg_filealreadyexistsoverwrite = 'File %s already exists. Overwrite?';
      msg_storinghtmlindexinfile = 'Storing HTML index in %s';
      msg_errorstoringindexdata = 'Error storing index data (%d)';

      dialog_switchesmode = 'SwitchesMode';
      static_switchesmode_switchesmode = 'Switches Mode';

      dialog_compilerswitches = 'Compiler Switches';
      label_compiler_syntaxswitches = 'Syntax Switches';
      label_compiler_runtimechecks = 'Run-time checks';
      label_compiler_optimizations = 'Optimizations';
      label_compiler_targetprocessor = 'Target processor';
      label_compiler_linkafter = 'Linking stage';
      label_compiler_verboseswitches = 'Verbose Switches';
      label_compiler_browser = 'Browser';
      label_compiler_assemblerreader = 'Assembler reader';
      label_compiler_assemblerinfo = 'Assembler info';
      label_compiler_assembleroutput = 'Assembler output';
      page_compiler_syntax = 'S~y~ntax';
      page_compiler_codegeneration = 'Code ~g~eneration';
      page_compiler_verbose = '~V~erbose';
      page_compiler_browser = '~B~rowser';
      page_compiler_assembler = '~A~ssembler';

      dialog_target = 'Target';
      label_target_platform = 'Target platform';

      label_primaryfile_primaryfile = 'Primary file';

      { About window }
      dialog_about = 'About';
      label_about_compilerversion = 'Compiler Version';
      label_about_debugger = 'Debugger';

      { Program info window }
      dialog_programinformation = 'Program Information';
      label_proginfo_currentmodule = 'Current module';
      label_proginfo_lastexitcode = 'Last exit code';
      label_proginfo_availablememory = 'Available memory';

      { Tools }
      dialog_tools = 'Tools';
      label_tools_programtitles = '~P~rogram titles';
      dialog_modifynewtool = 'Modify/New Tool';
      label_toolprop_title = '~T~itle';
      label_toolprop_programpath = 'Program ~p~ath';
      label_toolprop_commandline = 'Command ~l~ine';
      msg_errorparsingparametersatpos = ^C'Error parsing parameters line at line position %d.';
      msg_cantinstallmoretools = ^C'Can''t install more tools...';
      msg_requiredparametermissingin = 'Required parameter missing in [%s]';
      msg_requiredpropertymissingin = 'Required property missing in [%s]';
      msg_unknowntypein = 'Unknown type in [%s]';
      msg_propertymissingin = '%s property missing in [%s]';
      msg_invaliditemsin = 'Invalid number of items in [%s]';
      msg_errorparsingtoolparams = ^C'Error parsing tool params.';
      msg_executingtool = 'Executing tool %s ...';
      msg_errorreadingoutput = 'Error reading output.';
      msg_executingfilterfor = 'Executing filter for %s ...';
      msg_cantfindfilteredoutput = 'Can''t find filtered output.';
      msg_errorprocessingfilteredoutput = 'Error processing filtered output.';
      msg_errorexecutingfilter = 'Error executing filter %s';
      msg_errorexecutingtool = 'Error executing tool %s';
      msg_errorexecutingshell = 'Error cannot run shell';
      msg_filterexecutionsuccessfulexitcodeis = 'Filter execution successful. Exit code = %d';
      msg_toolexecutionsuccessfulexitcodeis = 'Tool execution successful. Exit code = %d';

      dialog_greparguments = 'Grep arguments';
      msg_grepprogramnotfound = 'Grep program not found';
      label_grep_texttofind = '~T~ext to find';
      label_grep_greparguments = '~G~rep arguments';
      msg_runninggrepwithargs = 'Running Grep -n %s';
      msg_errorrunninggrep = #3'Error running Grep'#13#3'DosError = %d'#13#3'Exit code = %d';
      msg_errorreadinggrepoutput = #3'Error reading Grep output'#13#3'In line %d of %s';

      dialog_windowlist = 'Window List';
      label_wndlist_windows = '~W~indows';

      msg_windowlist_hidden = 'hidden';

      msg_userscreennotavailable = 'Sorry, user screen not available.';
      msg_cantsetscreenmode = #3'Impossible to set'#13#3'%dx%d mode';
      msg_confirmnewscreenmode = 'Please, confirm that new mode'#13 +
                                 'is displayed correctly';
      { Tools $PROMPT() dialog }
      dialog_programarguments = 'Program Arguments';
      label_enterprogramargument = '~E~nter program argument';

      { Debugger confirmations and messages }
      msg_nodebuginfoavailable = 'No debug info available.';
      msg_nodebuggersupportavailable = 'No debugger support available.';
      msg_cantdebugchangetargetto = #3'Sorry, can not debug'#13+
                                    #3'programs compiled for %s.'#13+
                                    #3'Change target to %s?';
      msg_compiledwithoutdebuginforecompile =
                                 #3'Warning, the program'#13+
                                 #3'was compiled without'#13+
                                 #3'debugging info.'#13+
                                 #3'Recompile it?';
      msg_nothingtodebug = 'Oooops, nothing to debug.';
      msg_startingdebugger = 'Starting debugger';

      msg_doyouwanttocreatelocalconfigfile =
        'The Free Pascal IDE was never started in this directory before. '+
         'Do you want to create a new config file in this directory? '{#13+
         '(If you answer with "No", the IDE will use '+
         'the config file located in "%s")'};
      msg_configcopyexistingorcreatenew =
        'Do you want to copy the existing configuration or '+
        'create a new one from scratch?';
      btn_config_copyexisting = 'Copy ~e~xisting';
      btn_config_createnew = ' Create ~n~ew  ';

      { Desktop file messages }
      msg_readingdesktopfile = 'Reading desktop file...';
      msg_writingdesktopfile = 'Writing desktop file...';
      msg_readingdesktopcontents = 'Reading desktop contents...';
      msg_storingdesktopcontents = 'Storing desktop contents...';
      msg_readinghistory = 'Reading history...';
      msg_storinghistory = 'Storing history...';
      msg_readingwatches = 'Reading watches...';
      msg_storingwatches = 'Storing watches...';
      msg_readingbreakpoints = 'Reading breakpoints...';
      msg_storingbreakpoints = 'Storing breakpoints...';
      msg_readingcodecompletewordlist = 'Reading CodeComplete wordlist...';
      msg_storingcodecompletewordlist = 'Writing CodeComplete wordlist...';
      msg_readingcodetemplates = 'Reading CodeTemplates...';
      msg_storingcodetemplates = 'Writing CodeTemplates...';
      msg_readingsymbolinformation = 'Reading symbol information...';
      msg_storingsymbolinformation = 'Storing symbol information...';
      msg_failedtoreplacedesktopfile = 'Failed to replace desktop file.';
      msg_errorloadinghistory = 'Error loading history';
      msg_errorstoringhistory = 'Error storing history';
      msg_errorloadingkeys = 'Error loading custom keys';
      msg_errorstoringkeys = 'Error storing custom keys';
      msg_errorloadingwatches = 'Error loading watches';
      msg_errorstoringwatches = 'Error storing watches';
      msg_errorloadingbreakpoints = 'Error loading breakpoints';
      msg_errorstoringbreakpoints = 'Error storing breakpoints';
      msg_errorloadingdesktop = 'Error loading desktop';
      msg_errorstoringdesktop = 'Error storing desktop';
      msg_errorreadingflags = 'Error loading flags';
      msg_errorwritingflags = 'Error writing flags';
      msg_errorreadingvideomode = 'Error reading video mode';
      msg_errorstoringvideomode = 'Error storing video mode';
      msg_errorloadingcodetemplates = 'Error loading CodeTemplates';
      msg_errorstoringcodetemplates = 'Error writing CodeTemplates';
      msg_errorloadingsymbolinformation = 'Error loading symbol information';
      msg_errorstoringsymbolinformation = 'Error storing symbol information';
      msg_errorloadingcodecompletewordlist = 'Error loading CodeComplete wordlist';
      msg_errorstoringcodecompletewordlist = 'Error writing CodeComplete wordlist';
      msg_invaliddesktopversionlayoutlost = 'Invalid desktop version. Desktop layout lost.';
      msg_saveansifile = 'Save previous screen as Ansi File';
      msg_click_upper_left = 'Click to select upper left corner; Escape to cancel; Enter to select (0,0)';
      msg_click_lower_right = 'Click to select lower right corner; Escape to cancel; Enter to select (maxX,maxY)';

      msg_cantopenfile = 'Can''t open %s';
      msg_cantcreatefile = 'Can''t create %s';
      msg_cantfindfile = 'Can''t find %s';
      msg_errorreadingfile = 'Error reading file %s';
      msg_loadingfile = 'Loading %s';
      msg_storingfile = 'Storing %s';
      msg_closingfile = 'Closing %s';
      msg_invalidfilename = 'Invalid filename %s';

      msg_openingsourcefile = 'Opening source file... (%s)';
      msg_readingfileineditor = 'Reading %s into editor...';

      label_sym_objects = 'Objects';
      label_sym_globalscope = 'Global scope';
      label_sym_globals = 'Globals';
      label_sym_findprocedure = 'Procedures';
      label_sym_findprocedure2 = 'Matching ';

      { Symbol browser meminfo page }
      msg_sizeinmemory = 'Size in memory';
      msg_sizeonstack = 'Size on stack';

      msg_usedfirstin = 'Used first in';
      msg_mainsource = 'Main source';
      msg_sourcefiles = 'Source files';

      { Help messages }
      msg_indexingfile = 'Indexing file %s';
      msg_loadinghelpfiles = 'Loading help files...';
      msg_loadinghelpfile = 'Loading help file...';
      msg_buildinghelpindex = 'Building Help Index...';
      msg_locatingtopic = 'Locating topic...';
      msg_failedtoloadhelpfile = 'Failed to load help file %s';

      { Browser messages }
      msg_symbolnotfound = #3'Symbol %s not found';
      msg_nobrowserinfoavailable = 'No Browser info available';
      msg_toomanysymbolscantdisplayall= 'Too many symbols. Can''t display all of them.';

      { Status hints during compilation }
      msg_hint_pressesctocancel = 'Press ESC to cancel';
      msg_hint_compilesuccessfulpressenter = 'Compile successful: ~Press any key~';
      msg_hint_compilefailed = 'Compile failed';
      msg_hint_compileaborted = 'Compile aborted';
      msg_hint_pleasewait = 'Please wait...';

      { Compile status dialog texts }
      msg_compilingfile      = 'Compiling %s';
      msg_loadingunit        = 'Loading %s unit';
      msg_linkingfile        = 'Linking %s';
      msg_compiledone        = 'Done.';
      msg_failedtocompile    = 'Failed to compile...';
      msg_compilationaborted = 'Compilation aborted...';

      msg_nothingtocompile = 'Oooops, nothing to compile.';
      msg_cantcompileunsavedfile = 'Can''t compile unsaved file.';

      msg_couldnotcreatefile = 'could not create %s';
      msg_therearemoreerrorsinfile = 'There are more errors in file %s';
      msg_firstcompilationof = 'First compilation of %s';
      msg_recompilingbecauseof = 'Recompiling because of %s';

      msg_errorinexternalcompilation = 'Error in external compilation';
      msg_iostatusis = 'IOStatus = %d';
      msg_executeresultis = 'ExecuteResult = %d';

      msg_nothingtorun = 'Oooops, nothing to run.';
      msg_cannotrununit = 'Can''t run a unit';
      msg_cannotrunlibrary = 'Can''t run a library';

      msg_impossibletoreachcursor = 'Impossible to reach current cursor';
      msg_impossibletosetbreakpoint = 'Impossible to set breakpoints here';

      msg_programnotrundoserroris = #3'Program %s'#13#3'not run'#13#3'DosError = %d';
      msg_programfileexitedwithexitcode = #3'Program %s'#13#3'exited with '#13#3'exitcode = %d';

      msg_automaticallycreateddontedit = 'Automaticly created file, don''t edit.';

      { Compiler options }
      opt_objectpascal = '~O~bject pascal support';
      opt_clikeoperators = '~C~-like operators';
      opt_stopafterfirsterror = 'S~t~op after first error';
      opt_allowlabelandgoto = 'Allo~w~ LABEL and GOTO';
      opt_cplusplusstyledinline = 'C++ styled ~i~nline';
      opt_globalcmacros = 'Global C ~m~acros';
      opt_tp7compatibility = 'TP/BP ~7~.0 compatibility';
      opt_delphicompatibility = 'Del~p~hi compatibility';
      opt_allowstaticinobjects = 'A~l~low STATIC in objects';
      opt_strictvarstrings = 'Strict ~v~ar-strings';
      opt_extendedsyntax = 'E~x~tended syntax';
      opt_allowmmxoperations = 'Allow MMX op~e~rations';
      { Verbose options }
      opt_warnings = '~W~arnings';
      opt_notes = 'N~o~tes';
      opt_hints = '~H~ints';
      opt_generalinfo = 'General ~I~nfo';
      opt_usedtriedinfo = '~U~sed,tried info';
      opt_all = '~A~ll';
      opt_showallprocsonerror = 'Show all ~P~rocedures if error';
      { Checking options }
      opt_rangechecking = '~R~ange checking';
      opt_stackchecking = '~S~tack checking';
      opt_iochecking = '~I~/O checking';
      opt_overflowchecking = 'Integer ~o~verflow checking';
      { Code options }
      opt_generatefastercode = 'Generate ~f~aster code';
      opt_generatesmallercode = 'Generate s~m~aller code';
      opt_useregistervariables = 'Use regis~t~er-variables';
      opt_uncertainoptimizations = '~U~ncertain optimizations';
      opt_level1optimizations = 'Level ~1~ optimizations';
      opt_level2optimizations = 'Level ~2~ optimizations';
      opt_i386486 = 'i~3~86/i486';
      opt_pentiumandmmx = 'Pentium/PentiumMM~X~ (tm)';
      opt_pentiumpro = '~P~entium2/PentiumM/AMD';
      opt_pentiumiv = 'Pentium~4~';
      opt_m68000 = 'm~6~8000';
      opt_m68020 = 'm680~2~0';
      { Assembler options }
      opt_directassembler = '~D~irect assembler';
      opt_attassembler = '~A~T&T style assembler';
      opt_intelassembler = '~I~ntel style assembler';
      opt_motassembler = '~M~otorola style assembler';
      opt_listsource = '~L~ist source';
      opt_listregisterallocation = 'list ~r~egister allocation';
      opt_listtempallocation = 'list ~t~emp allocation';
      opt_usedefaultas = 'Use ~d~efault output';
      opt_usegnuas = 'Use ~G~NU as';
      opt_usenasmcoff = 'Use ~N~ASM coff';
      opt_usenasmelf = 'Use NASM ~e~lf';
      opt_usenasmobj = 'Use NASM ~o~bj';
      opt_usemasm = 'Use ~M~ASM';
      opt_usetasm = 'Use ~T~ASM';
      opt_usecoff = 'Use ~c~off';
      opt_usepecoff = 'Use ~p~ecoff';
      { Browser options }
      opt_nobrowser = 'N~o~ browser';
      opt_globalonlybrowser = 'Only Glob~a~l browser';
      opt_localglobalbrowser = '~L~ocal and global browser';
      { Conditional defines }
      opt_conditionaldefines = 'Conditio~n~al defines';
      { Memory sizes }
      opt_stacksize = '~S~tack size';
      opt_heapsize = '~H~eap size';
      { Directory options }
      opt_unitdirectories = '~U~nit directories';
      opt_includedirectories = '~I~nclude directories';
      opt_librarydirectories = '~L~ibrary directories';
      opt_objectdirectories = '~O~bject directories';
      opt_exeppudirectories = '~E~XE output directory';
      opt_ppuoutputdirectory = '~P~PU output directory';
      opt_cross_tools_directory = '~C~ross tools directory';
      { Library options }
      opt_librariesdefault = '~T~arget default';
      opt_dynamiclibraries = 'Link to ~D~ynamic libraries';
      opt_staticlibraries = 'Link to ~S~tatic libraries';
      opt_smartlibraries = 'Link to S~m~art libraries';
      opt_forcestaticlibs = 'Only link to st~a~tic libraries';
      { Symbol info options }
      opt_stripalldebugsymbols = '~S~trip all debug symbols from executable';
      opt_nogendebugsymbolinfo = 'Skip ~d~ebug information generation';
      opt_gendebugsymbolinfo = 'Generate ~d~ebug symbol information';
      opt_gensymbolandbacktraceinfo = 'Generate also backtrace ~l~ine information';
      opt_valgrindinfo = 'Generate ~v~algrind compatible debug info';
      { Link after options }
      opt_linkafter = 'Call ~l~inker after';
      { Profiling options }
      opt_noprofileinfo = '~N~o profile information';
      opt_gprofinfo = 'Generate profile code for g~p~rof';

      msg_xmustbesettoyforz_doyouwanttochangethis =
        '%s must be set to "%s" for %s. '+
        'Do you want to change this option automatically?';

      { Debugger messages and status hints }
      msg_programexitedwithcodeandsteps = #3'Program exited with '#13+
                                          #3'exitcode = %d'#13+
                                          #3'hidden steps = %d';

      msg_programexitedwithexitcode = #3'Program exited with '#13+
                                      #3'exitcode = %d';

      msg_programsignal             = #3'Program recieved signal %s'#13+
                                      #3'%s';

      msg_runningprogram  = 'Running...';
      msg_runningremotely = 'Executable running remotely on ';
      msg_connectingto    = 'Connecting to ';
      msg_getting_info_on = 'Getting infos from ';
      msg_runninginanotherwindow = 'Executable running in another window..';
      msg_couldnotsetbreakpointat = #3'Could not set Breakpoint'#13+
                                    #3+'%s:%d';
      msg_couldnotsetbreakpointtype = #3'Could not set Breakpoint'#13+
                                      #3+'%s %s';

      { standard button texts }
      button_OK          = 'O~K~';
      button_Cancel      = 'Cancel';
      button_New         = '~N~ew';
      button_Edit        = '~E~dit';
      button_Delete      = '~D~elete';
      button_Close       = '~C~lose';
      button_Show        = '~S~how';
      button_Hide        = '~H~ide';
      button_DisableAllBreakpoints = 'Dis. ~a~ll invalid';

      { Specific Button }
      button_ToggleButton = '~T~oggle';

      { Compiler message classes }
      msg_class_normal   = '';
      msg_class_fatal    = 'Fatal';
      msg_class_error    = 'Error';
      msg_class_warning  = 'Warning';
      msg_class_note     = 'Note';
      msg_class_hint     = 'Hint';
      msg_class_macro    = 'Macro';
      msg_class_procedure= 'Procedure';
      msg_class_conditional = 'Conditional';
      msg_class_info     = 'Info';
      msg_class_status   = 'Status';
      msg_class_used     = 'Used';
      msg_class_tried    = 'Tried';
      msg_class_debug    = 'Debug';

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
      status_disassemble     = '~Alt+I~ Disassemble';

      { Menu hints }
      hint_systemmenu        = 'System menu';
      hint_update            = 'Refresh and redraw display';
      hint_about             = 'Show version and copyright information';
      hint_filemenu          = 'File managment commands (Open, New, Save, etc.)';
      hint_filenew           = 'Create a new file in a new edit window';
      hint_filenewfromtemplate='Create a new file using a code template';
      hint_fileopen          = 'Locate and open a file in an edit window';
      hint_filesave          = 'Save the file in the active edit window';
      hint_filesaveas        = 'Save the current file under a different name, directory or drive';
      hint_filesaveall       = 'Save all modified files';
      hint_print             = 'Print current file';
      hint_printersetup      = 'Setup printer output device';
      hint_changedir         = 'Choose a new default directory';
      hint_dosshell          = 'Temporarily exit to shell';
      hint_exit              = 'Exit the IDE';
      hint_openrecentfile    = 'Open ';
      hint_editmenu          = 'Clipboard editing commands';
      hint_editundo          = 'Undo the previous editor operation';
      hint_editredo          = 'Redo the previously undone editor operation';
      hint_editcut           = 'Remove the selected text and put it in the clipboard';
      hint_editcopy          = 'Copy the selected text in the clipboard';
      hint_editpaste         = 'Insert selected text from the clipboard at the cursor position';
      hint_editcopywin       = 'Copy the selected text in windows clipboard';
      hint_editpastewin      = 'Insert selected text from windows clipboard at the cursor position';
      hint_editclear         = 'Delete the selected text';
      hint_editselectall     = 'Select the whole text';
      hint_editunselect      = 'Unselect everything';
      hint_showclipboard     = 'Open then clipboard window';
      hint_searchmenu        = 'Text and symbols search commands';
      hint_searchfind        = 'Search for text';
      hint_searchreplace     = 'Search for text and replace it with new text';
      hint_searchagain       = 'Repeat the last Search or Replace command';
      hint_gotoline          = 'Move the cursor to a specified line number';
      hint_objects           = 'Open a browser displaying all objects in the program';
      hint_modules           = 'Open a browser displaying all modules of the program';
      hint_globals           = 'Open a browser displaying all global symbols in the program';
      hint_symbol            = 'Open a browser a current word (not yet scope sensitive)';
      hint_runmenu           = 'Execution and parameters';
      hint_run               = 'Run the current program';
      hint_rundir            = 'Set directory that will be used as current working directory at execution';
      hint_runparameters     = 'Set command-line parameters passed to program at execution';
      hint_resetprogram      = 'Reset Program';
      hint_rununtilcursor    = 'Go on until Cursor position';
      hint_rununtilreturn    = 'Go on until end of current function';
      hint_userscreen        = 'Switch to the full-screen user output';
      hint_compilemenu       = 'Compile, build & make';
      hint_compile           = 'Compile the current source file';
      hint_make              = 'Rebuild source file and all other files that have been modified';
      hint_build             = 'Rebuild program and all available source files';
      hint_target            = 'Select target platform to compile for';
      hint_primaryfile       = 'Define the file that is the focus of Make and Build';
      hint_clearprimaryfile  = 'Clear the file previously set to Primary';
      hint_information       = 'Show compiler messages and program information';
      hint_showmessages      = 'Show compiler messages window';
      hint_debugmenu         = 'Debug Program';
      hint_togglebreakpoint  = 'Toggles Breakpoint';
      hint_createnewbreakpoint = 'Create a new breakpoint';
      hint_editbreakpoint    = 'Edit focused breakpoint';
      hint_deletebreakpoint  = 'Delete focused breakpoint';
      hint_opengdbwindow     = 'Open direct window to GDB';
      hint_addwatch          = 'Add a new expression to watch';
      hint_watches           = 'Open the Watches Window';
      hint_callstack         = 'Show call stack';
      hint_editbreakpoints   = 'Edit breakpoints';
      hint_toolsmenu         = 'User installed tools';
      hint_calculator        = 'Show calculator';
      hint_grep              = 'Run grep';
      hint_gotosource        = 'Edit source';
      hint_registers         = 'Open the Registers Window';
      hint_fpuregisters      = 'Open the FPU Registers Window';
      hint_vectorregisters   = 'Open the Vector Registers Window';
      hint_messageswindow    = 'Open the message window';
      hint_gotonextmsg       = 'Jumps to the next message in the Message Window';
      hint_gotoprevmsg       = 'Jumps to the previous message in the Message Window';
      hint_usertool          = 'User installed tool';
      hint_asciitable        = 'Show ASCII table';
      hint_optionsmenu       = 'Setting for compiler, editor, mouse, etc.';
      hint_switchesmode      = 'Select settings for normal, debug or release version';
      hint_compiler          = 'Set default compiler directives and conditional defines';
      hint_memorysizes       = 'Set default stack and heap sizes for generated programs';
      hint_linkeroptions     = 'Set linker options';
      hint_debugoptions      = 'Set debug information options';
      hint_remotedialog      = 'Set remote protocol parameters';
      hint_transferremote    = 'Transfer executable to remote target';
      hint_directories       = 'Set paths for units, include, object and generated files';
      hint_browser           = 'Specify global browser settings';
      hint_reloadmodifiedfile= 'Reload file modified on disk';
      hint_tools             = 'Create or change tools';
      hint_environmentmenu   = 'Specify environment settins';
      hint_preferences       = 'Specify desktop settings';
      hint_editoroptions     = 'Specify default editor settings';
      hint_codecomplete      = 'Specify CodeComplete keywords';
      hint_codetemplates     = 'Specify CodeTemplates';
      hint_mouseoptions      = 'Specify mouse settings';
      hint_desktopoptions    = 'Specify desktop settings';
      hint_startup           = 'Permanently change default startup options';
      hint_colors            = 'Customize IDE colors for windows, menus, editors, etc.';
      hint_openini           = 'Load a previously saved options file';
      hint_saveini           = 'Save all the changes made in the options menu';
      hint_saveasini         = 'Save all the changes made under a different name';
      hint_windowmenu        = 'Windows managment commands';
      hint_tile              = 'Arrange windows on desktop by tiling';
      hint_cascade           = 'Arrange windows on desktop by cascading';
      hint_closeall          = 'Close all windows on the desktop';
      hint_resize            = 'Change the size/postion of the active window';
      hint_zoom              = 'Enlarge or restore the size of the active window';
      hint_next              = 'Make the next window active';
      hint_prev              = 'Make the previous window active';
      hint_hide              = 'Hide the current window';
      hint_closewindow       = 'Close the active window';
      hint_windowlist        = 'Show a list of all open windows';
      hint_userscreenwindow  = 'Show contents of user screen in a window';
      hint_helpmenu          = 'Get online help';
      hint_helpcontents      = 'Show table of contents for Online Help';
      hint_helpindex         = 'Show index for Online Help';
      hint_helptopicsearch   = 'Display help on the word at cursor';
      hint_helpprevtopic     = 'Redisplay the last-viewed Online Help screen';
      hint_helphowtouse      = 'How to use Online Help';
      hint_helpfiles         = 'Install or remove installed help files';
      hint_openatcursor      = 'Attempt to open the file indicated by the word at cursor';
      hint_browseatcursor    = 'Attempt to browse the symbol at cursor';
      hint_editoroptionscur  = 'Specify editor settings';
      hint_rawgdbwindow      = 'Raw GDB communication window';
      hint_disassemblywindow = 'Show mixed Assembly/Source window';
      hint_allbreakpoints    = 'All current breakpoints';

      { error messages }
      error_saving_cfg_file  = 'Error saving configuration.';
      error_saving_dsk_file  = 'Error saving desktop file.'#13+
                               'Desktop layout could not be stored.';
      error_user_screen_not_avail = 'Sorry, user screen not available.';

      { caught signals or abnormal exits }
            { Debugger messages and status hints }
      error_programexitedwitherror = #3'Program generated a RTE %d'#13+
                                     #3'at address $%s.'#13+
                                     #3'Save your sources and restart the IDE.';
      error_programexitedwithsignal = #3'Program generated a signal %d.'#13+
                                      #3'Save your sources and restart the IDE.';

      continue_despite_error = #3'The IDE generated an internal error'#13+
                            #3'Do you really want to continue?'#13+
                            #3'The IDE could be in an unstable state.';

      leaving_after_error = #3'The IDE generated an internal error'#13+
                            #3'and will now be closed.';

