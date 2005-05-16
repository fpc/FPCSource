{
    $Id: fpstrh.inc,v 1.4 2005/02/14 17:13:18 peter Exp $
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

      menu_common_copy = '~M~†sol†s';
      menu_common_contents = '~T~artalom';
      menu_common_index = '~I~ndex';
      menu_common_topicsearch = 'TÇma ~k~eresÇse';
      menu_common_prevtopic = '~E~lãzã tÇma';

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
      menu_file              = '~F~†jl';
      menu_file_new          = 'È~j~';
      menu_file_template     = 'Èj min~t~a alapj†n...';
      menu_file_open         = 'Meg~n~yit†s...';
      menu_file_save         = '~M~entÇs';
      menu_file_saveas       = 'MentÇ~s~ m†skÇnt...';
      menu_file_saveall      = 'Mindent e~l~ment';
      menu_file_changedir    = 'Kînyvt†r~v~†lt†s...';
      { menu_file_dosshell     = 'Comman~d~ shell';
        should be changed to reflect the above change in english version PM }
      menu_file_dosshell     = '~D~OS parancssor';
      menu_file_exit         = '~K~ilÇpÇs';

      menu_edit              = 'S~z~erk.';
      menu_edit_copywin      = 'M†~s~ol†s Windows-b¢l';
      menu_edit_pastewin     = 'B~e~illesztÇs ~W~indows-ba';
      menu_edit_undo         = '~V~isszavon†s';
      menu_edit_redo         = 'MÇ~g~is';
      menu_edit_cut          = '~K~iv†g†s';
      menu_edit_copy         = menu_common_copy;
      menu_edit_paste        = '~B~eillesztÇs';
      menu_edit_clear        = '~T~îrlÇse';
      menu_edit_showclipboard= 'V†g¢l~a~p megjelen°tÇse';

      menu_search            = '~K~eresÇs';
      menu_search_find       = '~K~eresÇs...';
      menu_search_replace    = 'C~s~ere...';
      menu_search_searchagain= '~I~smÇtelt keresÇs';
      menu_search_jumpline   = 'U~g~r†s sorra...';
      menu_search_findproc   = '~E~lj†r†s keresÇse...';
      menu_search_objects    = '~O~bjektumok';
      menu_search_modules    = 'Mod~u~lok';
      menu_search_globals    = 'G~l~ob†lis azonos°t¢k';
      menu_search_symbol     = 'S~z~imb¢lum...';

      menu_run               = 'F~u~t†s';
      menu_run_run           = '~F~uttat†s';
      menu_run_stepover      = 'µ~t~lÇpÇs';
      menu_run_traceinto     = '~L~ÇpÇsenkÇnti futtat†s';
      menu_run_conttocursor  = 'Futtat†s a ~k~urzorig';
      menu_run_untilreturn   = 'Futtat†s ~v~isszatÇrÇsig';
      menu_run_parameters    = 'P~a~ramÇterek...';
      menu_run_resetdebugger = '~P~rogram alap†llapotba';

      menu_compile           = 'Fo~r~d°t†s';
      menu_compile_compile   = '~F~ord°t†s';
      menu_compile_make      = '~K~Çsz°tÇs';
      menu_compile_build     = 'ê~p~°tÇs';
      menu_compile_target    = '~C~Çl...';
      menu_compile_primaryfile = '~E~lsãdleges f†jl...';
      menu_compile_clearprimaryfile = 'E~l~sãdleges f†jl tîrlÇse';
      menu_compile_information = '~I~nform†ci¢...';
      menu_compile_compilermessages = 'F~o~rd°t†si Åzenetek';

      menu_debug             = '~D~ebug';
      menu_debug_output      = '~K~imenet';
      menu_debug_userscreen  = '~F~elhaszn†l¢i kÇpernyã';
      menu_debug_breakpoint  = '~T~îrÇspont';
      menu_debug_callstack   = '~H~°v†si verem';
      menu_debug_registers   = '~R~egiszterek';
      menu_debug_addwatch    = 'NÇzãke hozz†~a~d†sa';
      menu_debug_watches     = '~N~ÇzãkÇk';
      menu_debug_breakpointlist = 'TîrÇspontok ~l~ist†ja';
      menu_debug_gdbwindow   = '~G~DB ablak';

      menu_tools             = '~E~szkîz';
      menu_tools_messages    = 'ö~z~enetek';
      menu_tools_msgnext     = 'Ugr†s a ~k~îvetkezãre';
      menu_tools_msgprev     = 'Ugr†s az ~e~lãzãre';
      menu_tools_grep        = '~G~rep';
      menu_tools_calculator  = '~S~z†mol¢gÇp';
      menu_tools_asciitable  = 'Ascii ~t~†bla';

      menu_options           = '~O~pci¢k';
      menu_options_mode      = 'M¢d~.~..';
      menu_options_compiler  = '~F~ord°t¢...';
      menu_options_memory    = '~M~em¢ria-mÇretek...';
      menu_options_linker    = '~L~inker...';
      menu_options_debugger  = 'De~b~ugger...';
      menu_options_directories = '~K~înyvt†rak...';
      menu_options_browser   = '~T~all¢z¢...';
      menu_options_tools     = '~E~szkîzîk...';
      menu_options_env       = 'Kî~r~nyezet';
      menu_options_env_preferences = '~B~e†ll°t†sok...';
      menu_options_env_editor= 'S~z~erkesztã...';
      menu_options_env_codecomplete = 'Code~C~omplete...';
      menu_options_env_codetemplates = 'Code~T~emplates...';
      menu_options_env_desktop = '~A~sztal...';
      menu_options_env_mouse = 'E~g~Çr...';
      menu_options_env_startup = '~I~ndul†s...';
      menu_options_env_colors= 'Sz°~n~ek';
      menu_options_learn_keys= 'Learn ~K~eys';{ NOT TRANSLATED }
      menu_options_open      = 'Megn~y~it†s...';
      menu_options_save      = 'MentÇ~s~';
      menu_options_saveas    = 'MentÇs m~†~skÇnt...';

      menu_window            = '~A~blak';
      menu_window_tile       = '~M~ozaikszer˚ elrendezÇs';
      menu_window_cascade    = 'µ~t~lapol†s';
      menu_window_closeall   = 'Mindent ~b~ez†r';
      menu_window_resize     = 'MÇ~r~etezÇs/Mozgat†s';
      menu_window_zoom       = '~N~agy°t†s';
      menu_window_next       = '~K~îvetkezã';
      menu_window_previous   = '~E~lãzã';
      menu_window_hide       = '~R~ejtÇs';
      menu_window_close      = 'Be~z~†r†s';
      menu_window_list       = '~L~ista...';
      menu_window_update     = 'Megjelen°tã ~f~riss°tÇse';

      menu_help              = '~S~£g¢';
      menu_help_contents     = menu_common_contents;
      menu_help_index        = menu_common_index;
      menu_help_topicsearch  = menu_common_topicsearch;
      menu_help_prevtopic    = menu_common_prevtopic;
      menu_help_using        = 'S£g¢ ~h~aszn†lata';
      menu_help_files        = '~F~†jlok...';
      menu_help_about        = '~N~Çvjegy...';

      { Breakpoints window local menu items }
      menu_bplocal_gotosource  = menu_local_gotosource;
      menu_bplocal_editbreakpoint = 'TîrÇspont s~z~erkesztÇse';
      menu_bplocal_newbreakpoint = 'È~j~ tîrÇspont';
      menu_bplocal_deletebreakpoint = 'TîrÇspont ~t~îrlÇse';
      menu_bplocal_togglestate = 'µllapot ~v~†lt†sa';

      { Watches local menu items }
      menu_watchlocal_edit = 'NÇzãke s~z~erkesztÇse';
      menu_watchlocal_new = 'È~j~ nÇzãke';
      menu_watchlocal_delete = 'NÇzãke ~t~îrlÇse';

      { Symbol view local menu items }
      menu_symlocal_browse = '~T~alloz†s';
      menu_symlocal_gotosource = menu_local_gotosource;
      menu_symlocal_tracksource = menu_local_tracksource;
      menu_symlocal_options = menu_local_options;

      { Source editor local menu items }
      menu_srclocal_openfileatcursor = 'Kurzor alatti ~f~†jl megnyit†sa';
      menu_srclocal_browseatcursor = 'Kurzor alatt ~s~zimb¢lum tall¢z†sa';
      menu_srclocal_topicsearch = 'TÇma ~k~eresÇse';
      menu_srclocal_options = menu_local_options;

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
      menu_msglocal_saveas = menu_file_saveas;

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

      dialog_openafile        = 'F†jl megnyit†sa';
      label_filetoopen        = 'Meg~n~yitand¢ f†jl';
      label_lookingfor        = '%s felkutat†sa';

      msg_confirmsourcediradd = 'A %s kînyvt†r nincs a keresÇsi £tvonalban. '+
                                'Hozz†adja ?';

      dialog_fillintemplateparameter = 'Minta paramÇtereinek megad†sa';

      dialog_calculator       = 'Sz†mol¢gÇp';

      msg_quitconfirm         = 'Biztosan ki akar lÇpni?';

      { CodeComplete dialog }
      dialog_codecomplete     = 'CodeComplete';
      label_codecomplete_keywords = '~K~ulcszavak';

      dialog_codecomplete_add = 'Èj kulcssz¢ hozz†ad†sa';
      label_codecomplete_add_keyword = 'Kulcssz¢';

      dialog_codecomplete_edit = 'Kulcssz¢ szerkesztÇse';
      label_codecomplete_edit_keyword = 'Kulcssz¢';

      { CodeTemplates dialog }
      dialog_codetemplates_add = 'Èj minta hozz†ad†sa';
      dialog_codetemplates_edit = 'Minta szerkesztÇse';

      msg_codecomplete_alreadyinlist = '"%s" m†r van a list†ban';

      { File|New from template dialog }
      msg_notemplatesavailable = 'Nincsenek mint†k.';
      dialog_newfromtemplate   = 'Èj dokumentum minta alapj†n';
      label_availabletemplates = 'Felhaszn†lhat¢ ~m~int†k';

      label_codetemplate_shortcut = '~R~îvid°tÇs';
      label_codetemplate_content = 'Minta t~a~rtalm';
      label_codetemplate_templates = '~M~int†k';

      dialog_newtemplate = 'Èj minta';
      dialog_modifytemplate = 'Minta szerkesztÇse';
      msg_codetemplate_alreadyinlist = '"%s" nev˚ minta m†r van a list†ban';

      dialog_compilermessages = 'A ford°t¢ Åzenetei';

      dialog_compilingwithmode = 'Ford°t†s  (%s m¢d)';

      { Breakpoints window }
      dialog_breakpointlist = 'TîrÇspontok list†ja';
      label_breakpointpropheader = ' T°pus     | µllapot | Poz°ci¢           | Ignor. | FeltÇtelek ';

      dialog_modifynewbreakpoint = 'TîrÇspont szerkesztÇse/felvÇtele';
      label_breakpoint_name = '~N~Çv';
      label_breakpoint_line = '~S~or';
      label_breakpoint_conditions = 'FeltÇtelek';
      label_breakpoint_ignorecount = '~I~gnor†l†sok sz†ma';

      { Watches window }
      dialog_watches = 'NÇzãkÇk';

      label_watch_expressiontowatch = '~K~ifejezÇs';
      label_watch_values = 'NÇzãke ÇrtÇke';

      msg_watch_currentvalue = 'Aktu†lis ÇrtÇk: '+#13+
                               '%s';
      msg_watch_currentandpreviousvalue = 'Aktu†lis ÇrtÇk: '+#13+
                                          '%s'+#13+
                                          'Elãzã ÇrtÇk: '+#13+
                                          '%s';

      dialog_registers = 'Regiszterek';

      dialog_fpu = 'FPU †llapota';

      dialog_callstack = 'H°v†si verem';

      dialog_help = 'S£g¢';
      msg_modalhelpnotimplemented = 'A mod†lis s£g¢ mÇg nincs implement†lva.';

      dialog_browse = 'Tall¢z†s: %s';
      dialog_units = 'Modulok';

      dialog_browsesymbol = 'Szimb¢lum tall¢z†sa';
      label_entersymboltobrowse = 'Tall¢zand¢ ~s~zimb¢lum';

      dialog_gdbwindow = 'GDB ablak';

      dialog_clipboard = 'V†g¢lap';

      dialog_userscreen = 'Felhaszn†l¢i kÇpernyã';

      dialog_messages = 'özenetek';

      dialog_memorysizes = 'Mem¢ria-mÇretek';

      dialog_directories = 'Kînyvt†rak';

      dialog_programparameters = 'Program paramÇterei';
      label_parameters_parameter = '~P~aramÇterek';

      dialog_openoptions = 'Opci¢k betîltÇse';
      msg_cantopenconfigfile = 'Nem tudom megnyitni a konfigur†ci¢s f†jlt.';
      msg_errorsavingconfigfile = 'Hiba a konfigur†ci¢s f†jl mentÇse kîzben.';

      dialog_saveoptions = 'Opci¢k mentÇse';
      dialog_ini_filename = 'Name of INI file'; { NOT TRANSLATED }

      dialog_preferences = 'Be†ll°t†sok';
      label_preferences_videomode = 'Vide¢-m¢d';
      label_preferences_currentdirectory = 'Akt~u~†lis kînyvt†r';
      label_preferences_configdirectory = 'Konf~i~g. f†jl kînyvt†ra';
      label_preferences_desktopfile = 'Asztal-f†jl';
      label_preferences_editorfiles = 'Szerkesztett ~f~†jlok';
      label_preferences_environment = 'Kîrny~e~zet';
      label_preferences_desktop = 'As~z~tal';
      label_preferences_autosave = 'Automatikus mentÇs';
      label_preferences_autotracksource = 'Forr†s ~a~utom. kîvetÇse';
      label_preferences_closeongotosource = 'Bez~†~r†s ugr†skor';
      label_preferences_changedironopen = 'Kînyvt†r~v~†lt†s megyit†skor';
      label_preferences_options = 'Opci¢k';

      dialog_defaulteditoroptions = 'Szerkesztã alapÇrtÇkeinek be†ll°t†sa';
      dialog_editoroptions = 'Szerkesztã be†ll°t†sa';
      label_editor_backupfiles = 'Biztons†gi ~f~†jlok';
      label_editor_insertmode = 'Besz£r†s~i~ m¢d';
      label_editor_autoindentmode = '~A~uto-bekezdÇs m¢d';
      label_editor_usetabcharacters = 'Tab-karakterek has~z~n†lata';
      label_editor_backspaceunindents = '~B~acksp. tîrli a beh£z†st';
      label_editor_persistentblocks = 'Ta~r~t¢s blokkok';
      label_editor_syntaxhighlight = '~S~zintakszis-kiemelÇs';
      label_editor_blockinsertcursor = 'B~l~okk alak£ besz£r¢ kurzor';
      label_editor_verticalblocks = '~F~Åggãleges blokkok';
      label_editor_highlightcolumn = 'Oszlop kiemel~Ç~se';
      label_editor_highlightrow = 'So~r~ kiemelÇse';
      label_editor_autoclosingbrackets = 'Aut~o~matikus z†r¢jelezÇse';
      label_editor_keeptrailingspaces = 'Z†r¢ szî~k~îzîk megtart†sa';
      label_editor_codecomplete = 'Co~d~eComplete engedÇlyezve';
      label_editor_folds = '"~H~ajtogat†s" engedÇlyezve';
      label_editor_editoroptions = 'Sz~e~rkesztã opci¢k';
      label_editor_tabsize = '~T~ab mÇrete';
      label_editor_indentsize = 'Beh£~z~†s mÇrete';
      label_editor_highlightextensions = 'Kieme~l~Çs a kîvetkezã kiterjesztÇsek esetÇn';
      label_editor_filepatternsneedingtabs = 'File ~p~atterns needing tabs';

      dialog_browseroptions = 'Tall¢z¢ be†ll°t†sai';
      dialog_localbrowseroptions = 'Helyi tall¢z¢ be†ll°t†sai';
      label_browser_labels = '~C~°mkÇk';
      label_browser_constants = '~K~onstansok';
      label_browser_types = '~T~°pusok';
      label_browser_variables = '~V~†ltoz¢k';
      label_browser_procedures = '~E~lj†r†sok';
      label_browser_inherited = 'ô~r~îklÇsek';
      label_browser_symbols = 'Szimb¢lumok';
      label_browser_newbrowser = 'È~j~ tall¢z¢';
      label_browser_currentbrowser = 'Aktu†lis cse~r~Çje';
      label_browser_subbrowsing = 'Al-tall¢z†s';
      label_browser_scope = '~H~at¢kîr';
      label_browser_reference = 'H~i~vatkoz†sok';
      label_browser_preferredpane = 'Alap lap';
      label_browser_qualifiedsymbols = '~Q~ualified symbols';
      label_browser_sortsymbols = '~M~indig rendez';
      label_browser_display = 'Megjelen°tÇs';

      dialog_desktoppreferences = 'Asztal be†ll°t†sai';
      label_desktop_historylists = '~E~lãzmÇnyek';
      label_desktop_clipboard = '~V~†g¢lap tartalma';
      label_desktop_watches = '~N~ÇzãkÇk';
      label_desktop_breakpoints = '~T~îrÇspontok';
      label_desktop_openwindow = 'N~y~itott ablakok';
      label_desktop_symbolinfo = '~S~zimb¢lum-inform†ci¢';
      label_desktop_codecompletewords = 'Co~d~eComplete sz¢lista';
      label_desktop_codetemplates = 'Code~T~emplates';
      label_desktop_preservedacrosssessions = 'Menete~k~ kîzîtt megãrzÇsre kerÅl';

      dialog_mouseoptions = 'EgÇr be†ll°t†sai';
      label_mouse_speedbar = 'Gyors     Kîzepes     Lass£';
      label_mouse_doubleclickspeed = '~E~gÇr dupla-kattint†sok';
      label_mouse_reversebuttons = 'FelcserÇlt egÇ~r~gombok';
      label_mouse_crtlrightmousebuttonaction = 'Ctrl+Jobb egÇrgomb';
      label_mouse_altrightmousebuttonaction = 'Alt+Jobb egÇrgomb';
      label_mouse_act_nothing = 'S~e~mmi';
      label_mouse_act_topicsearch = '~T~Çma keresÇse';
      label_mouse_act_gotocursor = '~G~o to cursor';
      label_mouse_act_breakpoint = 'T~î~rÇspont';
      label_mouse_act_evaluate = '~K~iÇrtÇkelÇs';
      label_mouse_act_addwatch = 'NÇzãke hozz†~a~d†sa';
      label_mouse_act_browsesymbol = '~S~zimb¢lum tall¢z†sa';

      label_colors_grp_browser = 'Tall¢z¢';
      label_colors_framepassive = 'Passz°v keret';
      label_colors_frameactive = 'Akt°v keret';
      label_colors_frameicon = 'Keret ikon';
      label_colors_scrollbarpage = 'Gîrgetãs†v oldal';
      label_colors_scrollbaricons = 'Gîrgetãs†v ikon';
      label_colors_normaltext = 'Norm†l szîveg';
      label_colors_selectedtext = 'Kiv†lasztott szîveg';
      label_colors_activeitem = 'Akt°v elem';
      label_colors_inactiveitem = 'Inakt°v elem';
      label_colors_focuseditem = 'F¢kusz†lt elem';
      label_colors_selecteditem = 'Kiv†lasztott elem';
      label_colors_divider = 'Oszt¢';
      label_colors_grp_clock = '‡ra';
      label_colors_clockview = '‡ra elem';
      label_colors_grp_desktop = 'Asztal';
      label_colors_grp_dialogs = 'P†rbeszÇdablakok';
      label_colors_grp_editor = 'Szerkesztã';
      label_colors_highlighcolumn = 'Oszlop kiemelÇse';
      label_colors_highlightrow = 'Sor kiemelÇse';
      label_colors_errormessages = 'HibaÅzenetek';
      label_colors_grp_help = 'S£g¢';
      label_colors_helptext = 'S£g¢ szîveg';
      label_colors_helplinks = 'S£g¢ kapcsok';
      label_colors_selectedlink = 'Kiv†lasztott kapocs';
      label_colors_grp_menus = 'MenÅk';
      label_colors_grp_syntax = 'Szintakszis';
      label_colors_whitespace = 'Sz¢kîz';
      label_colors_comments = 'MegjegyzÇsek';
      label_colors_reservedwords = 'Fenntartott szavak';
      label_colors_identifiers = 'Azonos°t¢k';
      label_colors_strings = 'Karakterl†ncok';
      label_colors_numbers = 'Sz†mok';
      label_colors_hexnumbers = 'Hexa sz†mok';
      label_colors_assembler = 'Assembler';
      label_colors_symbols = 'Szimb¢lumok';
      label_colors_directives = 'Direkt°v†k';
      label_colors_tabs = 'Tabul†torok';
      label_colors_html_heading1 = 'HTML Heading 1';
      label_colors_html_heading2 = 'HTML Heading 2';
      label_colors_html_heading3 = 'HTML Heading 3';
      label_colors_html_heading4 = 'HTML Heading 4';
      label_colors_html_heading5 = 'HTML Heading 5';
      label_colors_html_heading6 = 'HTML Heading 6';

      dialog_linker = 'Linker';
      label_linker_preferredlibtype = 'Preferr†lt kînyvt†r-t°pus';

      dialog_debugger = 'Tall¢z†s/Debugging/Profiling';
      label_debugger_debuginfo = 'Debugging inform†ci¢';
      label_debugger_profileswitches = 'Profiling kapcsol¢k';
      label_debugger_compilerargs = 'Tov~†~bbi ford°t†si kapcsol¢k';
      label_debugger_useanotherconsole = 'M†sik konzol h~a~szn†lata';
      label_debugger_redirection = 'Debuggolt prog. †ti~r~†nyt°s†sa';
      label_debugger_useanothertty = 'M†sik tty h~a~szn†lata';

      dialog_helpfiles = 'S£g¢-f†jlok telep°tÇse';
      label_helpfiles_helpfiles = '~S~£g¢-f†jlok';

      dialog_installhelpfile = 'S£g¢-f†jl telep°tÇse';
      label_installhelpfile_filename = '~S~£g¢-f†jl neve';

      msg_createkeywordindexforhelpfile = 'Indexeljem a s£g¢ f†jlt?';

      dialog_topictitle = 'TÇma c°me';
      label_topictitle_title = 'C°m';

      msg_pleasewaitwhilecreatingindex = 'KÇrem v†rjon am°g az index elkÇszÅl...';
      msg_buildingindexfile = 'A %s index-f†jl kÇsz°tÇse folyamatban';
      msg_filedoesnotcontainanylinks = 'a %s nem tartalmaz kapcsokat, °gy nem alkalmas indexelÇsre.';
      msg_filealreadyexistsoverwrite = 'A %s f†jl m†r lÇtezik. FelÅl°rja?';
      msg_storinghtmlindexinfile = 'HTML index t†rol†sa a %s f†jlban';
      msg_errorstoringindexdata = 'Hiba az index adatok t†rol†sa kîzben (%d)';

      dialog_switchesmode = 'SwitchesMode';
      static_switchesmode_switchesmode = 'Switches Mode';

      dialog_compilerswitches = 'Ford°t¢ kapcsol¢k';
      label_compiler_syntaxswitches = 'Szintaktikai kapcsol¢k';
      label_compiler_runtimechecks = 'Fut†s-idej˚ ellenãrzÇsek';
      label_compiler_optimizations = 'Optimaliz†ci¢k';
      label_compiler_targetprocessor = 'CÇl-processzor';
      label_compiler_linkafter = 'Linking stage'; { NOT TRANSLATED }
      label_compiler_verboseswitches = 'Verbose kapcsol¢k';
      label_compiler_browser = 'Tall¢z¢';
      label_compiler_assemblerreader = 'Assembler olvas¢';
      label_compiler_assemblerinfo = 'Assembler inf¢';
      label_compiler_assembleroutput = 'Assembler kimenet';
      page_compiler_syntax = 'Sz~i~ntaxis';
      page_compiler_codegeneration = 'K¢d-~g~ener†ci¢';
      page_compiler_verbose = '~V~erbose';
      page_compiler_browser = '~T~all¢z¢';
      page_compiler_assembler = '~A~ssembler';

      dialog_target = 'CÇl';
      label_target_platform = 'CÇl-platform';

      label_primaryfile_primaryfile = 'Elsãdleges f†jl';

      { About window }
      dialog_about = 'NÇvjegy';
      label_about_compilerversion = 'Ford°t¢ verzi¢';
      label_about_debugger = 'Debugger';

      { Program info window }
      dialog_programinformation = 'Program inform†ci¢';
      label_proginfo_currentmodule = 'Aktu†lis modul';
      label_proginfo_lastexitcode = 'Utols¢ kilÇpÇsi k¢d';
      label_proginfo_availablememory = 'Szabad mem¢ria';

      { Tools }
      dialog_tools = 'Eszkîzîk';
      label_tools_programtitles = '~P~rogram c°mek';
      dialog_modifynewtool = 'Eszkîz szerkesztÇse/felvÇtele';
      label_toolprop_title = '~C~°m';
      label_toolprop_programpath = '~P~rogram elÇrÇsi £tja';
      label_toolprop_commandline = 'Parancs~s~or';
      msg_errorparsingparametersatpos = ^C'Hiba a paramÇterek ÇrtelmezÇse kîzen a %d. karakternÇl.';
      msg_cantinstallmoretools = ^C'Tîbb eszkîz nem vehetã fel...';
      msg_requiredparametermissingin = 'Egy kîtelezã paramÇter hi†nyzik [%s] szekci¢ban';
      msg_requiredpropertymissingin = 'Egy kîtelezã jellemzã hi†nyzik a [%s] szekci¢ban';
      msg_unknowntypein = 'Ismeretlen t°pus a [%s] szekci¢ban';
      msg_propertymissingin = 'A %s jellemzã hi†nyzik a [%s] szekci¢b¢l';
      msg_invaliditemsin = 'êrvÇnytelen elemsz†m a [%s] szekci¢ban';
      msg_errorparsingtoolparams = ^C'Hiba a paramÇterek ÇrtelmezÇse kîzben.';
      msg_executingtool = 'A %s eszkîz futtat†sa...';
      msg_errorreadingoutput = 'Hiba a kimenet olvas†sa kîzben.';
      msg_executingfilterfor = 'Sz˚rã futtat†sa a %s eszkîzhîz...';
      msg_cantfindfilteredoutput = 'Nem tal†lom a sz˚rã kimenetÇt.';
      msg_errorprocessingfilteredoutput = 'Hiba a sz˚rt kimenet feldolgoz†sa kîzben.';
      msg_errorexecutingfilter = 'Hiba a sz˚rã futtat†sa kîzben'; {%s}
      msg_errorexecutingtool = 'Hiba a %s eszkîz futtat†sa kîzben';
      msg_errorexecutingshell = 'Nem tudom futtatni a shell-t';

      msg_filterexecutionsuccessfulexitcodeis = 'Sz˚rã futtat†sa sikeres. KilÇpÇsi k¢d = %d';
      msg_toolexecutionsuccessfulexitcodeis = 'Eszkîz futtat†sa sikeres. KilÇpÇsi k¢d = %d';

      dialog_greparguments = 'Grep paramÇterei';
      msg_grepprogramnotfound = 'Nem tal†lom a Grep programot';
      label_grep_texttofind = 'K~e~resendã szîveg';
      label_grep_greparguments = '~G~rep paramÇterei';
      msg_runninggrepwithargs = '"Grep -n %s" futtat†sa';
      msg_errorrunninggrep = #3'Hiba a Grep futtat†sa kîzben'#13#3'DosError = %d'#13#3'KilÇpÇsi k¢d = %d';
      msg_errorreadinggrepoutput = #3'Hiba Grep kimenet olvas†sa'#13#3'kîzben a %d. sorban. (%s)';

      dialog_windowlist = 'Ablakok list†ja';
      label_wndlist_windows = '~A~blakok';

      msg_windowlist_hidden = 'rejtett';

      msg_userscreennotavailable = 'A felhaszn†l¢i kÇpernyã nem elÇrhetã.';

      { Tools $PROMPT() dialog }
      dialog_programarguments = 'Program paramÇterei';
      label_enterprogramargument = 'írja b~e~ a program paramÇtereit';

      { Debugger confirmations and messages }
      msg_nodebuginfoavailable = 'Nincs debug inform†ci¢.';
      msg_nodebuggersupportavailable = 'Debugger t†mogat†s nem elÇrhetã.';
      msg_cantdebugchangetargetto = #3'A %s platformra ford°tott'#13+
                                    #3'programok nem debuggolhat¢k.'#13+
                                    #3'A cÇlt ink†bb %s-ra v†ltoztatja?';
      msg_compiledwithoutdebuginforecompile =
                                 #3'Figyelem, a program'#13+
                                 #3'debug inform†ci¢ nÇlkÅl'#13+
                                 #3'lett ford°tva.'#13+
                                 #3'Èjraford°tja?';
      msg_nothingtodebug = 'Hopp†, nincs mit debuggolni.';
      msg_startingdebugger = 'Debugger ind°t†sa';

      msg_doyouwanttocreatelocalconfigfile =
        'A Free Pascal IDE mÇg sosem lett ind°tva ebben a kînyvt†rban. '+
         'LÇtre akar hozni egy £j konfigur†ci¢s f†jlt itt? '{#13+
         '(If you answer with "No", the IDE will use '+
         'the config file located in "%s")'};
      msg_configcopyexistingorcreatenew =
        'A m†r lÇtezã konfigur†ci¢t m†soljam le, vagy hozzak '+
        'lÇtre egy teljesen £jat?';
      btn_config_copyexisting = '~M~†sold le!';
      btn_config_createnew = '~H~ozd lÇtre!';

      { Desktop file messages }
      msg_readingdesktopfile = 'Asztal-f†jl olvas†sa...';
      msg_writingdesktopfile = 'Asztal-f†jl °r†sa...';
      msg_readingdesktopcontents = 'Asztal tartalm†nak olvas†sa...';
      msg_storingdesktopcontents = 'Asztal tartalm†nak °r†sa...';
      msg_readinghistory = 'ElãzmÇnyek olvas†sa...';
      msg_storinghistory = 'ElãzmÇnyek t†rol†sa...';
      msg_readingwatches = 'NÇzãkÇk olvas†sa...';
      msg_storingwatches = 'NÇzãkÇk t†rol†sa...';
      msg_readingbreakpoints = 'TîrÇspontok olvas†sa...';
      msg_storingbreakpoints = 'TîrÇspontok t†rol†sa...';
      msg_readingcodecompletewordlist = 'CodeComplete sz¢lista olvas†sa...';
      msg_storingcodecompletewordlist = 'CodeComplete sz¢lista t†rol†sa...';
      msg_readingcodetemplates = 'CodeTemplate-ek olvas†sa...';
      msg_storingcodetemplates = 'CodeTemplate-ek t†rol†sa...';
      msg_readingsymbolinformation = 'Szimb¢lum-inform†ci¢ olvas†sa...';
      msg_storingsymbolinformation = 'Szimb¢lum-inform†ci¢ t†rol†sa...';
      msg_failedtoreplacedesktopfile = 'Nem sikerÅlt az asztal-f†jlt lecserÇlni.';
      msg_errorloadinghistory = 'Hiba az elãzmÇnyek betîltÇse kîzben';
      msg_errorstoringhistory = 'Hiba az elãzmÇnyek t†rol†sa kîzben';
      msg_errorloadingwatches = 'Hiba a nÇzãkÇk betîltÇse kîzben';
      msg_errorstoringwatches = 'Hiba a nÇzãkÇk t†rol†sa kîzben';
      msg_errorloadingbreakpoints = 'Hiba a tîrÇspontok olvas†sa kîzben';
      msg_errorstoringbreakpoints = 'Hiba a tîrÇspontok t†rol†sa kîzben';
      msg_errorloadingdesktop = 'Hiba az asztal betîltÇse kîzben';
      msg_errorstoringdesktop = 'Hiba az asztal t†rol†sa kîzben';
      msg_errorreadingflags = 'Hiba a flagek betîltÇse kîzben';
      msg_errorwritingflags = 'Hiba a flagek °r†sa kîzben';
      msg_errorreadingvideomode = 'Hiba a vide¢-m¢d olvas†sa kîzben';
      msg_errorstoringvideomode = 'Hiba a vide¢-m¢d °r†sa kîzben';
      msg_errorloadingcodetemplates = 'Hiba a CodeTemplate-ek olvas†sa kîzben';
      msg_errorstoringcodetemplates = 'Hiba a CodeTemplate-ek °r†sa kîzben';
      msg_errorloadingsymbolinformation = 'Hiba a szimb¢lum-inform†ci¢ olvas†sa kîzben';
      msg_errorstoringsymbolinformation = 'Hiba a szimb¢lum-inform†ci¢ °r†sa kîzben';
      msg_errorloadingcodecompletewordlist = 'Hiba a CodeComplete sz¢lista olvas†sa kîzben';
      msg_errorstoringcodecompletewordlist = 'Hiba a CodeComplete sz¢lista °r†sa kîzben';
      msg_invaliddesktopversionlayoutlost = 'êrvÇnytelen asztal verzi¢. Az asztal tartalma elveszett.';
      { NOT TRANSLATED }
      msg_saveansifile = 'Save previous screen as Ansi File';
      msg_click_upper_left = 'Click to select upper left corner; Escape to cancel; Enter to select (0,0)';
      msg_click_lower_right = 'Click to select lower right corner; Escape to cancel; Enter to select (maxX,maxY)';

      msg_cantopenfile = 'Nem tudom megnyitni a %s f†jlt';
      msg_cantcreatefile = 'Nem tudom lÇtrehozni a %s f†jlt';
      msg_cantfindfile = 'Nem tal†lom a %s f†jlt';
      msg_errorreadingfile = 'Hiba a %s f†jl olvas†sa kîzben';
      msg_loadingfile = '%s betîltÇse';
      msg_storingfile = '%s t†rol†sa';
      msg_closingfile = '%s bez†r†sa';
      msg_invalidfilename = 'A %s ÇrvÇnytelen f†jlnÇv';

      msg_openingsourcefile = 'Forr†sf†jl megnyit†sa... (%s)';
      msg_readingfileineditor = '%s beolvas†sa a szerkesztãbe...';

      label_sym_objects = 'Objektumok';
      label_sym_globalscope = 'Glob†lis hat¢kîr';
      label_sym_globals = 'Glob†lis elemek';

      { Symbol browser meminfo page }
      msg_sizeinmemory = 'MÇret a mem¢ri†ban';
      msg_sizeonstack = 'MÇret a vermen';

      msg_usedfirstin = 'Elãszîr haszn†lva';
      msg_mainsource = 'Fã forr†s';
      msg_sourcefiles = 'Forr†s-f†jlok';

      { Help messages }
      msg_indexingfile = '%s f†jl indexelÇse';
      msg_loadinghelpfiles = 'S£g¢ f†jlok betîltÇse...';
      msg_loadinghelpfile = 'S£g¢ f†jl betîltÇse...';
      msg_buildinghelpindex = 'S£g¢index kÇsz°tÇse...';
      msg_locatingtopic = 'TÇma betîltÇse...';
      msg_failedtoloadhelpfile = 'Nem sikerÅlt betîlteni a %s s£g¢-f†jlt';

      { Browser messages }
      msg_symbolnotfound = #3'Nem tal†lom a %s szimb¢lumot';
      msg_nobrowserinfoavailable = 'Nincs tall¢z†si inform†ci¢';
      msg_toomanysymbolscantdisplayall= 'T£l sok szimb¢lum. Nem tudom mindet megjelen°teni.';

      { Status hints during compilation }
      msg_hint_pressesctocancel = 'Nyomjon ESC-et a megszak°t†shoz';
      { NOT TRANSLATED should be changed to Press any key }
      msg_hint_compilesuccessfulpressenter = 'A ford°t†s sikeres: ~Nyomjon Enter-t~';
      msg_hint_compilefailed = 'A ford°t†s sikertelen';
      msg_hint_compileaborted = 'A ford°t†s megszak°tva';
      msg_hint_pleasewait = 'KÇrem v†rjon...';

      { Compile status dialog texts }
      msg_compilingfile      = 'Ford°t†s alatt %s';
      msg_loadingunit        = 'Loading %s unit'; { NOT TRANSLATED }
      msg_linkingfile        = 'LinkelÇs alatt %s';
      msg_compiledone        = 'KÇsz.';
      msg_failedtocompile    = 'Nem sikerÅlt leford°tani...';
      msg_compilationaborted = 'A ford°t†s megszak°tva...';

      msg_nothingtocompile = 'Hopp†, nincs mit ford°tani.';
      msg_cantcompileunsavedfile = 'El nem mentett f†jlt nem tudok ford°tani.';

      msg_couldnotcreatefile = 'A %s f†jl lÇtrehoz†sa nem sikerÅlt';
      msg_therearemoreerrorsinfile = 'A %s f†jlban tîbb hiba is tal†lhat¢';
      msg_firstcompilationof = 'A %s elsã ford°t†sa';
      msg_recompilingbecauseof = 'Èjraford°t†sa %s miatt';

      msg_errorinexternalcompilation = 'Hiba a kÅlsã ford°t†s kîzben';
      msg_iostatusis = 'IOStatus = %d';
      msg_executeresultis = 'ExecuteResult = %d';

      msg_nothingtorun = 'Hopp†, nincs mit futtatni.';

      msg_impossibletoreachcursor = 'A kurzor-poz°ci¢ra sosem kerÅl a vezÇrlÇs';
      msg_impossibletosetbreakpoint = 'Itt nem helyezhetã el tîrÇspont';

      msg_programnotrundoserroris = #3'A %s programot'#13#3'nem sikerÅlt futtatni'#13#3'DosError = %d';
      msg_programfileexitedwithexitcode = #3'A %s program'#13#3'kilÇpett '#13#3'exitcode = %d';

      msg_automaticallycreateddontedit = 'Automatikusan lÇtrehozott f†jl, ne szerkessze!';

      { Compiler options }
      opt_objectpascal = '~D~elphi 2 bãv°tÇsek be';
      opt_clikeoperators = '~C~-szer˚ oper†torok';
      opt_stopafterfirsterror = 'S~t~op az elsã hiba ut†n';
      opt_allowlabelandgoto = 'LABEL, GOTO engedÇl~y~ezve';
      opt_cplusplusstyledinline = 'C++ st~°~lus£ inline';
      opt_globalcmacros = 'Glob†lis C ~m~akr¢k';
      opt_tp7compatibility = 'TP/BP ~7~.0 kompatibilit†s';
      opt_delphicompatibility = 'Del~p~hi kompatibilit†s';
      opt_allowstaticinobjects = 'STATIC engedÇ~l~yezÇse az objektumokban';
      opt_strictvarstrings = 'Szigor£an vett ~v~ar-sztringek';
      opt_extendedsyntax = 'Ki~t~erjesztett szintakszis';
      opt_allowmmxoperations = 'MMX m˚v~e~letek engedÇlyezÇse';
      { Verbose options }
      opt_warnings = '~F~igyelmeztetÇsek';
      opt_notes = 'Meg~j~egyzÇsek';
      opt_hints = '~T~ippek';
      opt_generalinfo = 'µltal†nos ~i~nf¢';
      opt_usedtriedinfo = '~H~aszn†lt, pr¢b†lt inf¢';
      opt_all = '~M~inden';
      opt_showallprocsonerror = 'Hiba esetÇn az ~î~sszes elj†r†s megmutat†sa';
      { Checking options }
      opt_rangechecking = 'Hat†~r~-ellenãrzÇs';
      opt_stackchecking = 'Veremviz~s~g†lat';
      opt_iochecking = '~I~/O ellenãrzÇs';
      opt_overflowchecking = 'EgÇszek t£lcs~o~rdul†s-vizsg†lata';
      { Code options }
      opt_generatefastercode = '~G~yorsabb k¢d gener†l†sa';
      opt_generatesmallercode = '~K~isebb k¢d gener†l†sa';
      opt_useregistervariables = 'Regisz~t~er-v†ltoz¢k haszn†lata';
      opt_uncertainoptimizations = '~B~izonztalan optimiz†ci¢k';
      opt_level1optimizations = '~1~.szint˚ optimiz†ci¢k';
      opt_level2optimizations = '~2~.szint˚ optimiz†ci¢k';
      opt_i386486 = 'i~3~86/i486';
      opt_pentiumandmmx = 'Pentium/PentiumMM~X~ (tm)';
      opt_pentiumpro = 'P~P~ro/PII/c6x86/K6 (tm)';
      { Assembler options }
      opt_directassembler = '~D~irekt assembler';
      opt_attassembler = '~A~T&T st°lus£ assembler';
      opt_intelassembler = '~I~ntel st°lus£ assembler';
      opt_listsource = 'Forr†s ~l~ist†z†sa';
      opt_listregisterallocation = '~r~egister-foglal†s list.';
      opt_listtempallocation = '†~t~meneti foglal†s list.';
      opt_usedefaultas = '~D~efault kimeneti form†tum haszn†lata';
      opt_usegnuas = '~G~NU as haszn†lata';
      opt_usenasmcoff = '~N~ASM coff haszn†lata';
      opt_usenasmelf = 'Use NASM ~e~lf haszn†lata';
      opt_usenasmobj = 'Use NASM ~o~bj haszn†lata';
      opt_usemasm = '~M~ASM haszn†lata';
      opt_usetasm = '~T~ASM haszn†lata';
      opt_usecoff = '~c~off haszn†lata';
      opt_usepecoff = '~p~ecoff haszn†lata';
      { Browser options }
      opt_nobrowser = 'Nincs tall~¢~z¢';
      opt_globalonlybrowser = 'Cs~a~k glob†lis tall¢z¢';
      opt_localglobalbrowser = '~L~ok†lis Çs glob†lis tall¢z¢';
      { Conditional defines }
      opt_conditionaldefines = 'FeltÇteles defi~n~°ci¢k';
      { Memory sizes }
      opt_stacksize = '~V~erem-mÇret';
      opt_heapsize = '~H~eap-mÇret';
      { Directory options }
      opt_unitdirectories = '~U~nit kînyvt†rak';
      opt_includedirectories = '~I~nclude kînyvt†rak';
      opt_librarydirectories = '~L~ibrary kînyvt†rak';
      opt_objectdirectories = '~O~bject kînyvt†rak';
      opt_exeppudirectories = '~E~XE & PPU kînyvt†rak';
      { Library options }
      opt_librariesdefault = '~T~arget default'; { NOT TRANSLATED }
      opt_dynamiclibraries = '~D~inamikus kînyvt†rak';
      opt_staticlibraries = '~S~tatikus kînyvt†rak';
      opt_smartlibraries = 'S~m~art libraries'; { NOT TRANSLATED }
      { Symbol info options }
      opt_stripalldebugsymbols = 'Debug szimb¢lumok elt†vol°t†~s~a';
      opt_gendebugsymbolinfo = '~D~ebug szimb¢lum info gener†l†sa';
      opt_gensymbolandbacktraceinfo = 'Backtrace sorinfom†ci¢ gener†~l~†sa is';
      { Link after options }
      opt_linkafter = 'Call ~l~inker after'; { NOT TRANSLATED }
      { Profiling options }
      opt_noprofileinfo = '~N~incs profile inform†ci¢';
      opt_gprofinfo = 'Profile inf¢ gener†l†sa g~p~rof-hoz';

      msg_xmustbesettoyforz_doyouwanttochangethis =
        'Az %s ÇrtÇkÇt "%s"-re kell †ll°tani a %s-hez. '+
        'K°v†nja, hogy ezt a be†ll°t†st automatikusan megtegyem?';


      { Debugger messages and status hints }
      msg_programexitedwithcodeandsteps = #3'A program kilÇpett '#13+
                                          #3'exitcode = %d'#13+
                                          #3'rejtett lÇpÇsek = %d';

      msg_programexitedwithexitcode = #3'A program kilÇpett '#13+
                                      #3'exitcode = %d';

      msg_runningprogram = 'Futtat†s...';
      msg_runninginanotherwindow = 'Program futtat†sa m†sik ablakban...';
      msg_couldnotsetbreakpointat = #3'Nem sikerÅlt a tîrÇspont felvÇtele'#13+
                                    #3+'%s:%d';
      msg_couldnotsetbreakpointtype = #3'Nem sikerÅlt a tîrÇspont felvÇtele'#13+
                                      #3+'%s %s';

      { standard button texts }
      button_OK          = 'O~K~';
      button_Cancel      = 'MÇgsem';
      button_New         = 'È~j~';
      button_Edit        = 'S~z~erk.';
      button_Delete      = '~T~îrlÇs';
      button_Close       = 'Be~z~†r';
      button_Show        = '~M~utat';
      button_Hide        = '~R~ejt';

      { Compiler message classes }
      msg_class_normal   = '';
      msg_class_fatal    = 'Fat†lis';
      msg_class_error    = 'Hiba';
      msg_class_warning  = 'FigyelmeztetÇs';
      msg_class_note     = 'MegjegyzÇs';
      msg_class_hint     = 'Tipp';
      msg_class_macro    = 'Makro';
      msg_class_procedure= 'Elj†r†s';
      msg_class_conditional = 'Conditional';
      msg_class_info     = 'Info';
      msg_class_status   = 'St†tusz';
      msg_class_used     = 'Haszn†lva';
      msg_class_tried    = 'Pr¢b†lva';
      msg_class_debug    = 'Debug';

      { status line entries }
      status_help            = '~F1~ S£g¢';
      status_help_on_help    = '~F1~ Seg°tsÇg a s£g¢hoz';
      status_help_previoustopic = '~Alt+F1~ Elãzã tÇma';
      status_help_index      = '~Shift+F1~ S£g¢ index';
      status_help_close      = '~Esc~ S£g¢ bez†r†sa';
      status_save            = '~F2~ Ment';
      status_open            = '~F3~ Megnyit';
      status_compile         = '~Alt+F9~ Ford°t';
      status_make            = '~F9~ KÇsz°t';
      status_localmenu       = '~Alt+F10~ Helyi menÅ';
      status_transferchar    = '~Ctrl+Enter~ Karakter †tvitele';
      status_msggotosource   = '~'+EnterSign+'~ Ugr†s forr†sba';
      status_msgtracksource  = '~Space~ Forr†s kîvetÇse';
      status_close           = '~Esc~ Bez†r';
      status_calculatorpaste = '~Ctrl+Enter~ EredmÇny †tvitele';

      { Menu hints }
      hint_systemmenu        = 'RendszermenÅ';
      hint_update            = 'Kijelzã friss°tÇse Çs £jrarajzol†sa';
      hint_about             = 'Verzi¢- Çs copyright inform†ci¢k megjelen°tÇse';
      hint_filemenu          = 'F†jl-kezelÇsi parancsok (Megnyit†s, Èj, MentÇs, stb.)';
      hint_filenew           = 'Èj f†jl lÇtrehoz†sa egy £j szerkesztã-ablakban';
      hint_filenewfromtemplate='Èj f†jl lÇtrehoz†sa minta alapj†n';
      hint_fileopen          = 'F†jl felkutat†sa Çs megnyit†sa szerkesztã-ablakban';
      hint_filesave          = 'Az akt°v szerkesztã-ablak mentÇse f†jlba';
      hint_filesaveas        = 'Az aktu†lis f†jl mentÇse m†sik nÇven, kînyvt†rba vagy meghajt¢ra';
      hint_filesaveall       = 'Minden m¢dos°tott f†jl mentÇse';
      hint_changedir         = 'Èj alapÇrtelmezett kînyvt†r kiv†laszt†sa';
      hint_dosshell          = 'µtmeneti kilÇpÇs a DOS-ba';
      hint_exit              = 'KilÇpÇs az IDE-bãl';
      hint_openrecentfile    = 'Megjelîlt f†jlt megnyit†sa egy £j szerkesztã-ablakban';
      hint_editmenu          = 'V†g¢lap-szerkesztã parancsok';
      hint_editundo          = 'Utols¢ szerkesztÇsi m˚velet visszavon†sa';
      hint_editredo          = 'Utols¢ vissavont szerkesztÇsi m˚velet £jb¢li elvÇgzÇse';
      hint_editcut           = 'Kiv†lasztott szîveg elt†vol°t†sa Çs elhelyezÇse a v†g¢lapon';
      hint_editcopy          = 'Kiv†lasztott szîveg m†sol†sa a v†g¢lapra';
      hint_editpaste         = 'Kiv†lasztott szîveg beillesztÇse a v†g¢lapr¢l';
      hint_editcopywin       = 'Kiv†lasztott szîveg m†sol†sa a Windows v†g¢lapra';
      hint_editpastewin      = 'Kiv†lasztott szîveg beillesztÇse a Windows v†g¢lapr¢l';
      hint_editclear         = 'Kiv†lasztott szîveg tîrlÇse';
      hint_showclipboard     = 'V†g¢lap ablak†nak megnyit†sa';
      hint_searchmenu        = 'Szîveg- Çs szimb¢lum-keresã parancsok';
      hint_searchfind        = 'Szîveg keresÇse';
      hint_searchreplace     = 'Szîveg keresÇse Çs cserÇje m†sik szîvegre';
      hint_searchagain       = 'Repeat the last Search or Replace command';
      hint_gotoline          = 'Kurzor mozgat†sa a megadott sz†m£ sorra';
      hint_objects           = 'A program objektumait tartalmaz¢ tall¢z¢ megjelen°tÇse';
      hint_modules           = 'A program moduljait tartalmaz¢ tall¢z¢ megjelen°tÇse';
      hint_globals           = 'A program gl¢b†lis szimb¢lumait tartalmaz¢ tall¢z¢ megjelen°tÇse';
      hint_symbol            = 'Tall¢z¢ megjelen°tÇse az aktu†lis sz¢hoz';
      hint_runmenu           = 'Futtat†s Çs paramÇterek';
      hint_run               = 'Az aktu†lis program futtat†sa';
      hint_runparameters     = 'A programnak futtat†skor †tadott parancssori paramÇterek megad†sa';
      hint_resetprogram      = 'Program alaphelyzetbe';
      hint_rununtilcursor    = 'Futtat†s a kurzor-poz°ci¢ig';
      hint_rununtilreturn    = 'Futtat†s az aktu†lis elj†r†s vÇgÇig';
      hint_userscreen        = 'µtv†lt†s a teljes-kÇpernyãs kiementre';
      hint_compilemenu       = 'Ford°t†s, Çp°tÇs & kÇsz°tÇse';
      hint_compile           = 'Aktu†lis forr†sf†jl ford°t†sa';
      hint_make              = 'Forr†s Çs minden m¢dos°tott f†jl £jraÇp°tÇse';
      hint_build             = 'A program- Çs minden elÇrhetã forr†s-f†jl £jraÇp°tÇse';
      hint_target            = 'Ford°t†si cÇlplatform kiv†laszt†sa';
      hint_primaryfile       = 'A kÇsz°tÇs Çs Çp°tÇs cÇlf†jlj†nak megad†sa';
      hint_clearprimaryfile  = 'Az elãzãleg megadott elsãdleges f†jl tîrlÇse';
      hint_information       = 'Ford°t†si Åzenetek Çs program-inform†ci¢ megjelen°tÇse';
      hint_showmessages      = 'Ford°t†si Åzenetek ablak megjelen°tÇse';
      hint_debugmenu         = 'Program debugol†sa';
      hint_togglebreakpoint  = 'TîrÇspont v†lt†sa';
      hint_createnewbreakpoint = 'Èj tîrÇspont lÇtrehoz†sa';
      hint_editbreakpoint    = 'F¢kusz†lt tîrÇspont szerkesztÇse';
      hint_deletebreakpoint  = 'F¢kusz†lt tîrÇspont tîrlÇse';
      hint_opengdbwindow     = 'Kîzvetlen GDB ablak megnyit†sa';
      hint_addwatch          = 'Èj nÇzãke-kifejezÇs felvÇtele';
      hint_watches           = 'NÇzãkÇk ablak megjelen°tÇse';
      hint_callstack         = 'H°v†si verem megjelen°tÇse';
      hint_editbreakpoints   = 'TîrÇspontok szerkesztÇse';
      hint_toolsmenu         = 'Felhaszn†l¢ †ltal telep°tett eszkîzîk';
      hint_calculator        = 'Sz†mol¢gÇp megjelen°tÇse';
      hint_grep              = 'Grep futtat†sa';
      hint_gotosource        = 'Forr†s szerkesztÇse';
      hint_registers         = 'Regiszterek ablak megnyit†sa';
      hint_messageswindow    = 'özenetek ablak megnyit†sa';
      hint_usertool          = 'Felhaszn†l¢ †ltal telep°tett eszkîz';
      hint_asciitable        = 'Show ASCII table';
      hint_optionsmenu       = 'Ford°t¢, szerkesztã, egÇr, stb. be†ll°t†sai';
      hint_switchesmode      = 'Norm†l, debug vagy release verzi¢ kiv†laszt†sa';
      hint_compiler          = 'AlapÇrtelmezett ford°t†si Çs feltÇteles direkt°v†k be†ll°t†sa';
      hint_memorysizes       = 'AlapÇrtelmezett verem-Çs heap-mÇret be†ll°t†sa';
      hint_linkeroptions     = 'Linker opci¢k be†ll°t†sa';
      hint_debugoptions      = 'Debug inform†ci¢ opci¢k';
      hint_directories       = 'ElÇrÇsi utak be†ll°t†sa (unit, include, object f†jlokhoz)';
      hint_browser           = 'Glob†lis tall¢z†si opci¢k be†ll°t†sa';
      hint_tools             = 'Eszkîzîk felvÇtele vagy m¢dos°t†sa';
      hint_environmentmenu   = 'Kîrnyezeti be†ll°t†sok megad†sa';
      hint_preferences       = 'Be†ll°t†sok megad†sa';
      hint_editoroptions     = 'Szerkesztã be†ll°t†sainak megad†sa';
      hint_codecomplete      = 'CodeComplete kulcsszavak megad†sa';
      hint_codetemplates     = 'CodeTemplate-ek megad†sa';
      hint_mouseoptions      = 'EgÇr-be†ll°t†sok megad†sa';
      hint_desktopoptions    = 'Asztal be†ll°t†sainak megad†sa';
      hint_startup           = 'Ind°t†si opci¢k permanens megv†ltoztat†sa';
      hint_colors            = 'Az IDE †ltal haszn†lt sz°nek megv†ltoztat†sa';
      hint_openini           = 'Elãzãleg elmentett konfigur†ci¢s f†jl megnyit†sa';
      hint_saveini           = 'Minden v†ltoztat†s mentÇse a konfigur†ci¢s f†jlba';
      hint_saveasini         = 'V†ltoztat†sok mentÇse m†s nÇven';
      hint_windowmenu        = 'Ablak-kezelã parancsok';
      hint_tile              = 'Ablakok mozaikszer˚ elrendezÇse az asztalon';
      hint_cascade           = 'Ablakok †tlapolt elrendezÇse az asztalon';
      hint_closeall          = 'Az asztal îsszes ablak†nak bez†r†sa';
      hint_resize            = 'Akt°v ablak mÇretÇnek/poz°ci¢j†nak v†ltozat†sa';
      hint_zoom              = 'Akt°v ablak megnagyobb°t†sa vagy mÇretÇnek vissz†ll°t†sa';
      hint_next              = 'A kîvetkezã ablak aktiv†l†sa';
      hint_prev              = 'Az elãzã ablak aktiv†l†sa';
      hint_closewindow       = 'Akt°v ablak bez†r†sa';
      hint_windowlist        = 'Az îsszes nyitott ablak list†j†nak megjelen°tÇse';
      hint_userscreenwindow  = 'Felhaszn†l¢i kÇpernyã tartalm†nak megjelen°tÇse ablakban';
      hint_helpmenu          = 'Seg°tsÇg kÇrÇse';
      hint_helpcontents      = 'Online S£g¢ tartlomjegyzÇkÇnek megjelen°tÇse';
      hint_helpindex         = 'Online S£g¢ indexÇnek megjelen°tÇse';
      hint_helptopicsearch   = 'A kurzor alatti sz¢val kapcsolatos s£g¢ megjelen°tÇse';
      hint_helpprevtopic     = 'Az utols¢nak megtekintett S£g¢ oldal megjelen°tÇse';
      hint_helphowtouse      = 'Az Online S£g¢ haszn†lata';
      hint_helpfiles         = 'S£g¢ f†jlok telep°tÇse Çs elt†vol°t†sa';
      hint_openatcursor      = 'A kurzor alatti f†jl megnyit†sa';
      hint_browseatcursor    = 'A kurzor alatti szimb¢lum tall¢z†sa';
      hint_editoroptionscur  = 'Szerkesztã be†ll°t†sainak megad†sa';
      hint_rawgdbwindow      = 'Nyers GDB kommunik†ci¢s ablak';
      hint_allbreakpoints    = 'ôsszes tîrÇspont';

      { error messages }
      error_saving_cfg_file  = 'Hiba a konfigur†ci¢ mentÇse kîzben.';
      error_saving_dsk_file  = 'Hiba az asztal f†jl t†rol†sa kîzben.'#13+
                               'Az asztal tartalma nem kerÅlt t†rol†sra.';
      error_user_screen_not_avail = 'A felhaszn†l¢i kÇpernyã nem elÇrhetã.';

{
  $Log: fpstrh.inc,v $
  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
