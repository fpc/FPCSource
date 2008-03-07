#
#   German (alternative, LATIN-US DOS) Language File for Free Pascal
#   Latest updates contributed by Karl-Michael Schindler aka mischi
#   <karl-michael.schindler at physik.uni-halle.de>
#
#   Based on errore.msg of SVN revision 10422
#
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998-2000 by the Free Pascal Development team
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#
# The constants are build in the following order:
# <part>_<type>_<txtidentifier>
#
# <part> is the part of the compiler the message is used
#   asmr_     assembler parsing
#   asmw_     assembler writing/binary writers
#   unit_     unit handling
#   scan_     scanner
#   parser_   parser
#   type_     type checking
#   general_  general info
#   exec_     calls to assembler, linker, binder
#
# <type> the type of the message it should normally used for
#   f_   fatal error
#   e_   error
#   w_   warning
#   n_   note
#   h_   hint
#   i_   info
#   l_   add linenumber
#   u_   used
#   t_   tried
#   c_   conditional
#   d_   debug message
#   x_   executable informations
#

#
# General
#
# 01023 is the last used one
#
# BeginOfTeX
% \section{General compiler messages}
% This section gives the compiler messages which are not fatal, but which
% display useful information. The number of such messages can be
% controlled with the various verbosity level \var{-v} switches.
% \begin{description}
general_t_compilername=01000_T_Compiler: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=01001_D_Quellbetriebssystem: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=01002_I_Zielbetriebssystem: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Der Pfad der ausfÅhrbaren Datei ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its binaries.
general_t_unitpath=01004_T_Der Unitpfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Der Includepfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Der Bibliothekspfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Der Objektdateienpfad: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 Zeilen Åbersetzt, $2 Sekunden$3
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_Speicher voll
% The compiler doesn't have enough memory to compile your program. There are
% several remedies for this:
% \begin{itemize}
% \item If you're using the build option of the compiler, try compiling the
% different units manually.
% \item If you're compiling a huge program, split it up in units, and compile
% these separately.
% \item If the previous two don't work, recompile the compiler with a bigger
% heap (you can use the \var{-Ch} option for this, \seeo{Ch})
% \end{itemize}
general_i_writingresourcefile=01010_I_Resourcestringtabellendatei $1 wird geschrieben
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Fehler beim Schreiben der Resourcestringtabellendatei: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
general_i_fatal=01012_I_Fataler Fehler:
% Prefix for Fatal Errors
general_i_error=01013_I_Fehler:
% Prefix for Errors
general_i_warning=01014_I_Warnung:
% Prefix for Warnings
general_i_note=01015_I_Anmerkung:
% Prefix for Notes
general_i_hint=01016_I_Hinweis:
% Prefix for Hints
general_e_path_does_not_exist=01017_E_Der Pfad "$1" existiert nicht
% The specified path does not exist.
general_f_compilation_aborted=01018_F_öbersetzen abgebrochen
% Compilation was aborted.
general_text_bytes_code=01019_bytes Code
general_text_bytes_data=01020_bytes Daten
general_i_number_of_warnings=01021_I_$1 Warnung(en) ausgegeben
% Total number of warnings issued during compilation.
general_i_number_of_hints=01022_I_$1 Hinweis(e) ausgegeben
% Total number of hints issued during compilation.
general_i_number_of_notes=01023_I_$1 Anmerkung(en) ausgegeben
% Total number of notes issued during compilation.%
% \end{description}
#
# Scanner
#
# 02086 is the last used one
#
% \section{Scanner messages.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=02000_F_Unerwartetes Dateiende
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=02001_F_Zeichenkette geht Åber Zeilenende hinaus
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_UnzulÑssiges Zeichen "$1" ($2)
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Syntaxfehler, "$1" erwartet, aber "$2" vorgefunden
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_TL_Die Include-Datei $1 wird jetzt gelesen
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_Kommentarschachtelungstiefe $1 gefunden
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_ignored_switch=02008_N_Compilerschalter $1 wurde ignoriert
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Compilerschalter $1 ist ungÅltig
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=02010_W_Dieser Compilerschalter hat globale Auswirkung
% The compiler switch is misplaced, and should be located at
% the start of the unit or program.
scan_e_illegal_char_const=02011_E_UngÅltige Char-Konstante
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range.
scan_f_cannot_open_input=02012_F_Datei $1 kann nicht geîffnet werden
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_Includedatei $1 kann nicht geîffnet werden
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_illegal_pack_records=02015_E_UngÅltige Record Ausrichtung "$1"
% You are specifying the \var{\{\$PACKRECORDS n\} } or \var{\{\$ALIGN n\} }
% with an illegal value for \var{n}. For \$PACKRECORDS valid alignments are 1, 2, 4, 8, 16, 32, C,
% NORMAL, DEFAULT, and for \$ALIGN valid alignment are 1, 2, 4, 8, 16, 32, ON,
% OFF. Under mode MacPas \$ALIGN also supports MAC68K, POWER and RESET.
scan_e_illegal_pack_enum=02016_E_UngÅltige minimale Grîsse der AufzÑhlung "$1"
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 NORMAL or DEFAULT are valid in this case.
scan_e_endif_expected=02017_E_$ENDIF erwartet fÅr $1 $2 definiert in $3 Zeile $4
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Syntaxfehler im Argument einer $if Direktive
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_e_error_in_preproc_expr=02019_E_Syntaxfehler im Kontext einer $if Direktive
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_w_macro_cut_after_255_chars=02020_W_Inhalt des Makros wurde nach der Auswertung bei 255 Zeichen abgeschnitten
% The contents of macros cannot be longer than 255 characters.
scan_e_endif_without_if=02021_E_ENDIF ohne IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_Benutzerdefiniert: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_Benutzerdefiniert: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_Benutzerdefiniert: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_Benutzerdefiniert: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_Benutzerdefiniert: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_Benutzerdefiniert: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_Ein Makro, welches den selben Namen wie ein SchlÅsselwort hat, wird ignoriert
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_MakropufferÅberlauf wÑhrend des Lesens oder Expandierens eines Makros
% Your macro or it's result was too long for the compiler.
scan_w_macro_too_deep=02030_W_Expansion des Makros Åberschreitet eine Makroschachtelungstiefe von 16
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_w_wrong_styled_switch=02031_W_Compilerschalter innerhalb von Kommentaren der Form // werden nicht untersÅtzt
% Compiler switches should be in normal pascal style comments.
scan_d_handling_switch=02032_DL_Bearbeite Schalter "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_CL_ENDIF $1 bearbeitet
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_CL_IFDEF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_CL_IFOPT $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_CL_IF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_CL_IFNDEF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_CL_ELSE $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_CL_Quelltext wird ignoriert bis...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_DrÅcken Sie <Return> um fortzusetzen
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Nicht unterstÅtzter Schalter $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_UngÅltige Compilerdirektive $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_TL_Wieder zurÅck in $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Nicht unterstÅtzter Anwendungstyp: $1
% You get this warning, if you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_APPTYPE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$APPTYPE\}} directive is supported by win32 applications only.
scan_w_description_not_support=02046_W_Der Compilerschalter DESCRIPTION wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$DESCRIPTION\}} directive is not supported on this target OS
scan_n_version_not_support=02047_N_VERSION wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$VERSION\}} directive is only supported by win32 target.
scan_n_only_exe_version=02048_N_VERSION kann in Units nicht verwendet werden
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_Falsches Format fÅr VERSION-Direktive $1
% The \var{\{\$VERSION\}} directive format is major_version.minor_version
% where major_version and minor_version are words.
scan_e_illegal_asmmode_specifier=02050_E_Unbekannter Assemblermodusname: "$1"
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_ASM-Leser-Schalter ist innerhalb einer Asm-Sequenz nicht mîglich, $1 wird erst in der nÑchsten Sequenz wirksam
% It is not possible to switch from one assembler reader to another
% inside an assembler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=02052_E_Parameter fÅr Schalter falsch, verwenden Sie ON/OFF oder +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_Resourcedateien werden vom aktuellen Zielbetriebssystem nicht unterstÅtzt
% The target you are compiling for doesn't support resource files.
scan_w_include_env_not_found=02054_W_$1 ist keine Umgebungsvariable
% The included environment variable can't be found in the environment, it'll
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Nicht erlaubter Wert fÅr MAXFPUREGISTER-Direktive
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Nur ein Resourcedatei wird vom aktuellen Zielbetriebssystem unterstÅtzt
% Only one resource file can be supported for this target - this is the case of
% OS/2 (EMX) currently. The first one found is used, the others are discarded.
scan_w_macro_support_turned_off=02057_W_MakrounterstÅtzung ist ausgeschaltet
% A macro declaration has been found, but macro support is currently off,
% so the declaration will be ignored. To turn macro support on compile with
% -Sm on the commandline or add {$MACRO ON} in the source
scan_e_invalid_interface_type=02058_E_Unbekannter Interfacetyp. UnterstÅtzt wird COM, CORBA und DEFAULT
% The interface type that was specified is not supported
scan_w_appid_not_support=02059_W_APPID wird nur von PalmOS unterstÅtzt
% The \var{\{\$APPID\}} directive is only supported for the PalmOS target.
scan_w_appname_not_support=02060_W_APPNAME wird nur von PalmOS unterstÅtzt
% The \var{\{\$APPNAME\}} directive is only supported for the PalmOS target.
scan_e_string_exceeds_255_chars=02061_E_Zeichenkettenkonstanten kînnen maximal 255 Zeichen lang sein
% A single string constant can contain at most 255 chars. Try splitting up the
% string in multiple smaller parts and concatenate them with a + operator.
scan_f_include_deep_ten=02062_F_Include-Dateien kînnen nur bis zu einer Tiefe von 16 geschachtelt werden
% When including include files the files have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_too_many_push=02063_F_Zu viele Ebenen von PUSH
% A maximum of 20 levels is allowed. This error occurs only in mode MacPas.
scan_e_too_many_pop=02064_E_POP ohne ein vorhergehendes PUSH
% This error occurs only in mode MacPas.
scan_e_error_macro_lacks_value=02065_E_Das Makro "$1" besitzt keinen Wert
% Thus the conditional compiling expression cannot be evaluated.
scan_e_wrong_switch_toggle_default=02066_E_Zum Modusumschalten kann nur ON/OFF/DEFAULT oder +/-/* verwendet werden
% You need to use ON or OFF or DEFAULT or a + or - or * to toggle the switch
scan_e_mode_switch_not_allowed=02067_E_Der Modus Umschalter "$1" ist hier nicht erlaubt
% A mode switch has already been encountered, or, in case of option -Mmacpas,
% a mode switch occur after UNIT.
scan_e_error_macro_undefined=02068_E_Die Compile time Variable oder das Makro "$1" ist nicht definiert.
% Thus the conditional compile time expression cannot be evaluated. Only in mode MacPas.
scan_e_utf8_bigger_than_65535=02069_E_Der UTF-8 Kode ist grîsser als 65535
% \fpc handles utf-8 strings internally as widestrings e.g. the char codes are limited to 65535
scan_e_utf8_malformed=02070_E_UngÅltige UTF-8 Zeichenkette
% The given string isn't a valid UTF-8 string
scan_c_switching_to_utf8=02071_C_UTF-8 Signatur gefunden, verwende UTF-8 Kode
% The compiler found an UTF-8 encoding signature (\$ef, \$bb, \$bf) at the beginning of a file,
% so it interprets it as an UTF-8 file
scan_e_compile_time_typeerror=02072_E_Compile time Ausdruck: Erwartete $1 aber erhielt $2 bei $3
% Type check of a compile time expression failed.
scan_n_app_type_not_support=02073_N_APPTYPE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_e_illegal_optimization_specifier=02074_E_"$1" ist eine ungÅltige Optimierung 
% When you specify an optimization with the \var{\{\$OPTIMIZATION xxx\}}
% the compiler didn't recognize the optimization you specified.
scan_w_setpeflags_not_support=02075_W_SETPEFLAGS wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$SETPEFLAGS\}} directive is not supported by the target OS
scan_w_imagebase_not_support=02076_W_IMAGEBASE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$IMAGEBASE\}} directive is not supported by the target OS
scan_w_minstacksize_not_support=02077_W_MINSTACKSIZE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$MINSTACKSIZE\}} directive is not supported by the target OS
scan_w_maxstacksize_not_support=02078_W_MAXSTACKSIZE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$MAXSTACKSIZE\}} directive is not supported by the target OS
scanner_e_illegal_warn_state=02079_E_UngÅltiger Wert fÅr die $WARN Direktive
% Only ON and OFF can be used as state with a \$warn compiler directive
scan_e_only_packset=02080_E_UngÅltiger Wert fÅr das set packing
% Only 0, 1, 2, 4, 8, DEFAULT and NORMAL are allowed as packset parameter
scan_w_pic_ignored=02081_W_PIC Direktive oder Schalter wird ignoriert
% Several targets like windows do not support neither need PIC so the PIC directive and switch are
% ignored.
scan_w_unsupported_switch_by_target=02082_W_Der Schalter "$1" wird vom derzeit ausgewÑhlten Zielbetriebssystem nicht unterstÅtzt
% Some compiler switches like \$E are not supported by all targets.
scan_w_frameworks_darwin_only=02084_W_Framework-bezogene Optionen werden nur fÅr Darwin/Mac OS X unterstÅtzt
% Frameworks are not a known concept, or at least not supported by FPC, on operating systems other than Darwin/Mac OS X.
scan_e_illegal_minfpconstprec=02085_E_"$1" ist eine ungÅltige minimale PrÑzision von Fliesskommakonstanten
% Valid minimal precisions for floating point constants are default, 32 and 64, which mean respectively minimal (usually 32 bit), 32 bit and 64 bit precision.
scan_w_multiple_main_name_overrides=02086_W_Der Name der "main" Prozedur wird mehrfach Åberschrieben. Es war bisher auf "$1" gesetzt
% \end{description}
#
# Parser
#
# 03244 is the last used one
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=03000_E_Parser - Syntaxfehler
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_e_dont_nest_interrupt=03004_E_INTERRUPT-Prozeduren dÅrfen nicht verschachtelt sein
% An \VAR{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Prozedurtyp $1 wird ignoriert
% The specified procedure directive is ignored by FPC programs.
parser_e_no_overload_for_all_procs=03006_E_Nicht alle Deklarationen von "$1" sind mit OVERLOAD deklariert
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_export_name_double=03008_E_Doppelter Name fÅr exportierte Funktion $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Doppelter Index fÅr exportierte Funktion $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_UngÅltiger Index for exportierte Funktion
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_FÅr relozierbare DLL oder ausfÅhrbare Datei $1 funktionieren keine Debug-Information, deaktiviert.
parser_w_parser_win32_debug_needs_WN=03012_W_Um Win32-Code debuggen zu kînnen mÅssen die Relozierungen mit -WN option abgeschaltet werden.
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_Konstruktor-Name muss INIT sein
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_Destruktor-Name muss DONE sein
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_proc_inline_not_supported=03016_E_SchlÅsselwort INLINE nicht unterstÅtzt
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_constructor_should_be_public=03018_W_Konstruktor muss PUBLIC sein
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_Destruktor muss PUBLIC sein
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Klasse darf nur einen Destruktor besitzen
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Lokale Klassendefinitionen sind nicht zulÑssig
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Anonyme Klassendefinitionen sind nicht zulÑssig
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_n_object_has_no_vmt=03023_N_Das Objekt "$1" besitzt keine VMT
% This is a note indicating that the declared object has no
% virtual method table.
parser_e_illegal_parameter_list=03024_E_UngÅltige Parameterliste
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_size=03026_E_Falsche Anzahl an Parameter im Aufruf von "$1" angegeben
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_Bezeichner "$1" ist keine Funktion, overload nicht mîglich
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_Overload-Funktion darf keine identische Parameterliste aufweisen
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_Funktionskopf unterscheidet sich von voriger Deklaration "$1"
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=03030_E_Funktionskopf von "$1" passt nicht zur Forward-Deklaration, Variablename Ñndert sich: $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_Werte in AufzÑhlungen mÅssen aufsteigend sein
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_e_no_with_for_variable_in_other_segments=03033_E_WITH kann nicht auf Variablen in anderen Segmenten angewendet werden
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_Funktionsverschachtelung grîsser als 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_BereichsprÅfungsfehler bei Konstantenbestimmung
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_BereichsprÅfungsfehler bei Konstantenbestimmung
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_doppelter CASE-Wert
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_Obere Grenze der CASE-Bereichsangabe ist kleiner als untere Grenze
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_Klassen oder Interfaces sind als typisierte Konstanten unzulÑssig
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_Funktionsvariablen von Overload-Funktionen sind unzulÑssig
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=03041_E_StringlÑnge muss ein Wert zwischen 1 und 255 sein
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
parser_w_use_extended_syntax_for_objects=03042_W_Benutzen Sie die erweiterte Syntax von NEW und DISPOSE fÅr Objekt-Instanzen
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=03043_W_Verwendung von NEW oder DISPOSE mit untypisierten Pointern ist ohne Aussage
parser_e_no_new_dispose_on_void_pointers=03044_E_Verwendung von NEW oder DISPOSE mit untypisierten Pointern ist nicht mîglich
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_Klassenbezeichner erwartet
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_Typbezeichner ist hier nicht zulÑssig
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_Methodenbezeichner erwartet
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_Funktionskopf passt zu keiner Methode der Klasse $1
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_d_procedure_start=03049_DL_Procedure/Function $1
% When using the \var{-vd} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_UngÅltige Fliesskommakonstante
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL darf nur in Konstruktoren verwendet werden
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Destruktoren kînnen keine Parameter haben
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Nur Klassenmethoden kînnen Åber den Klassennamen angesprochen werden
% This error occurs in a situation like the following:
% \begin{verbatim}
% Type :
%    Tclass = Class of Tobject;
%
% Var C : TClass;
%
% begin
% ...
% C.free
% \end{verbatim}
% \var{Free} is not a class method and hence cannot be called with a class
% reference.
parser_e_only_class_methods=03054_E_Nur Klassenmethoden kînnen in einer Klassenmethode angesprochen werden
% This is related to the previous error. You cannot call a method of an object
% from a inside a class method. The following code would produce this error:
% \begin{verbatim}
% class procedure tobject.x;
%
% begin
%   free
% \end{verbatim}
% Because free is a normal method of a class it cannot be called from a class
% method.
parser_e_case_mismatch=03055_E_Konstante und CASE-Typ passen nicht zueinander
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_Das Symbol kann nicht aus einer Bibliothek exportiert werden
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_Eine Inherit-Methode wird durch $1 verdeckt
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_Es gibt keine VorgÑngerklasse mit einer Methode, die damit Åberschrieben werden kînnte: $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=03059_E_Es gibt keine Member-Funktion um auf diese Property zuzugreifen
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_Die Stored Property - Direktive ist noch nicht implementiert
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_UngÅltiges Symbol fÅr den Zugriff auf die Property
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code would cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_Kann an dieser Stelle nicht auf das geschÅtzte Feld des Objekts zugreifen
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module where the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_Kann an dieser Stelle nicht auf das private Feld des Objekts zugreifen
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_e_overridden_methods_not_same_ret=03066_E_öberschriebene, virtuelle Methoden mÅssen den gleichen Ergebnistyp haben: "$2" wird Åberschrieben von "$1"
% If you declare overridden methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_Mit EXPORT deklarierte Funktionen dÅrfen nicht verschachtelt sein
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_Methoden dÅrfen nicht EXPORTiert werden
% You cannot declare a procedure that is a method for an object as
% \var{export}ed. That is, your methods cannot be called from a C program.
parser_e_call_by_ref_without_typeconv=03069_E_Aufrufe mit VAR-Parametern fÅr Argument-Nr. $1 mÅssen exakt stimmen: "$2" gefunden, "$3" erwartet
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_Klasse ist keine Elternklasse zur aktuellen Klasse
% When calling inherited methods, you are trying to call a method of a strange
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_SELF ist nur in Methoden erlaubt
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_Methoden dÅrfen nur in anderen Methoden direkt mit dem Klassen-Typbezeichner aufgerufen werden
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=03073_E_UnzulÑssige Verwendung von ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_BereichsprÅfungsfehler im SET-Konstruktor oder doppeltes Set-Element
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Zeiger auf Objekt erwartet
% You specified an illegal type in a \var{New} statement.
% The extended synax of \var{New} needs an object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_Ausdruck muss ein Konstruktor-Aufruf sein
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_Ausdruck muss ein Destruktor-Aufruf sein
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_UnzulÑssige Reihenfolge der Record-Elemente
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_Typ des Ausdrucks muss eine Klasse oder ein Recordtyp sein
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Prozeduren kînnen keinen Wert zurÅckliefern
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures cannot return a value.
parser_e_constructors_always_objects=03081_E_Konstruktoren und Destruktoren mÅssen Methoden sein
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_Operator besitzt kein Overload
% You're trying to use an overloaded operator when it isn't overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Es ist nicht mîglich, die Zuweisung fÅr gleiche Typen zu Åberladen
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Unmîgliche Operator-.berladung
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_Auslîsen einer Exception an dieser Stelle nicht mîglich
% You are trying to raise an exception where it isn't allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_Die erweiterte Syntax von New oder Dispose ist fÅr Klassen unzulÑssig
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{Dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_procedure_overloading_is_off=03088_E_Das öberladen von Prozeduren ist ausgeschaltet
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_Es ist nicht mîglich, dieesen Operator zu Åberladen. Verwandte Åberladbare Operatoren sind "$1"
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Vergleichsoperator muss einen booleschen Wert zurÅckgeben
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Nur virtuelle Methoden kînnen abstrakt sein
% You are declaring a method as abstract, when it isn't declared to be
% virtual.
parser_f_unsupported_feature=03092_F_Benutzung einer nicht unterstÅtzten Erweiterung!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_Das Mischen von Klassen und Objekten ist unzulÑssig
% You cannot derive \var{objects} and \var{classes} intertwined . That is,
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Unbekannte Prozedur, Direktive $1 wurde ignoriert
% The procedure direcive you secified is unknown.
parser_e_absolute_only_one_var=03095_E_ABSOLUTE kann nur auf eine alleinstehende Variable angewendet werden
% You cannot specify more than one variable before the \var{absolute} directive.
% Thus, the following construct will provide this error:
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
% \item [ absolute can only be associated a var or const ]
% The address of a \var{absolute} directive can only point to a variable or a
% typed constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_absolute_only_to_var_or_const=03096_E_ABSOLUTE kann nur auf Variablen und Konstanten angewendet werden
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Es kann nur EINE Variable initialisiert werden
% You cannot specify more than one variable with a initial value
% in Delphi syntax.
parser_e_abstract_no_definition=03098_E_Abstrakte Methoden dÅrfen keine Definition (mit Rumpf) haben
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Diese Åberladene Funktion darf nicht lokal sein (muss Exportiert werden)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_Virtuelle Methoden wurden ohne Konstruktor verwendet in $1
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_c_macro_defined=03101_CL_Makro definiert: $1
% When \var{-vc} is used, the compiler tells you when it defines macros.
parser_c_macro_undefined=03102_CL_Makrodefinition gelîscht: $1
% When \var{-vc} is used, the compiler tells you when it undefines macros.
parser_c_macro_set_to=03103_CL_Wert des Makros $1 auf $2 gesetzt
% When \var{-vc} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_öbersetze $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_UL_Parse Interface von Unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_UL_Parse Implementation von $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_DL_öbersetze $1 zum zweiten Mal
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_property_found_to_override=03109_E_Es gibt keine Property diesen Namens, die Åberschrieben werden kînnte
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Es ist nur eine einzelne Default-Property erlaubt, vererbte Default-Property in Klasse $1 gefunden
% You specified a property as \var{Default}, but a parent class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_Die Default-Property muss eine Array-Property sein
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Virtuelle Konstruktoren werden nur im Klassen-Objektmodell unterstÅtzt
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_Keine Default-Property verfÅgbar
% You try to access a default property of a class, but this class (or one of
% its ancestors) doesn't have a default property.
parser_e_cant_have_published=03114_E_Die Klasse kann keinen PUBLISHED Bereich haben, benutzen Sie den {$M+} Schalter
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_Forward-Deklaration der Klasse $1 muss hier aufgelîst werden, wenn sie als Elternklasse benutzt werden soll
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Lokale Operatoren werden nicht unterstÅtzt
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_Prozedur-Direktive $1 unzulÑssig im Interface-Bereich
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_Prozedur Direktive $1 unzulÑssig im Implementation-Bereich
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_Prozedur-Direktive $1 unzulÑssig in einer Procvar-Deklaration
% This procedure directive cannot be part of a procedural of function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_Funktion ist bereits als Public oder Forward deklariert: $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_Kann nicht EXPORT und EXTERNAL gleichzeitig benutzen
% These two procedure directives are mutually exclusive
parser_w_not_supported_for_inline=03123_W_$1 noch nicht innerhalb von inline Prozeduren/Funktionen unterstÅtzt
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=03124_W_Inlining deaktiviert
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Schreibe Browser-Log $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+\}} switch).
parser_h_maybe_deref_caret_missing=03126_H_Mîglicherweise fehlt eine Zeiger-Dereferenzierung
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_GewÑhlter Assembler-Leser nicht unterstÅtzt
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=03128_E_Prozedur-Direktive $1 steht in Konflikt mit anderen Direktiven
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_Aufrufkonvention passt nicht zur Forward-Deklaration
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_property_cant_have_a_default_value=03131_E_Property kann keinen Vorgabewert haben
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_Der Vorgabewert einer Property muss eine Konstante sein
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_Symbol darf nicht PUBLISHED sein, dies kann nur eine Klasse sein
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Diese Property-Art kann nicht PUBLISHED sein
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_e_empty_import_name=03136_E_Leerer Importname angegeben
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_division_by_zero=03138_E_Division durch Null
% There is a division by zero encounted
parser_e_invalid_float_operation=03139_E_UngÅltige Fliesskomma-Operation
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_Obere Grenze des Bereichs ist kleiner als die untere Grenze
% The upper bound of a \var{case} label is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_String "$1" ist lÑnger als $2
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_StringlÑnge ist grîsser als die LÑnge des "array of char"
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_UngÅltiger Ausdruck nach der 'Message'-Direktive
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Message-Handler akzeptieren nur einen "Call by Reference" Parameter
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Doppeltes Message-Label: $1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_SELF darf nur in Message-Handlern ein expliziter Parameter sein
% The self parameter can be passed only explicit if it is a method which
% is declared as message method handler
parser_e_threadvars_only_sg=03147_E_Threadvariablen kînnen nur statisch oder global sein
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has its own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Direkter Assembler wird f+r binÑres Ausgabeformat nicht unterstÅtzt
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=03149_W_Laden Sie die OBJPAS Unit nicht manuell, benutzen Sie statdessen {$mode objfpc} oder {$mode delphi}
% You're trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea to do, you can better use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automaticly
parser_e_no_object_override=03150_E_OVERRIDE darf in Objekten nicht verwendet werden
% Override isn't support for objects, use VIRTUAL instead to override
% a method of an anchestor object
parser_e_cant_use_inittable_here=03151_E_Datentypen, die ein Initialiserung oder Finalisierung benîtigen, kînnen in varianten Records nicht verwendet werden
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Resourcestrings dÅrfen nur statisch oder global sein
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_EXIT mit Argument darf hier nicht verwendet werden
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_Der Typ des STORED-Symbols muss boolesch sein
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_Dieses Symbol ist als Speichersymbol unzulÑssig
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can_be_published=03156_E_Nur Klassen, die im "$M+"-Modus Åbersetzt wurden, dÅrfen published sein
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Prozedurdirektive erwartet
% This error is triggered when you have a \var{\{\$Calling\}} directive without
% a calling convention specified.
% It also happens when declaring a procedure in a const block and you
% used a ; after a procedure declaration which must be followed by a
% procedure directive.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_Der Wert fÅr einen Property-Index muss ordinalen Typs sein
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Prozedurname zu kurz um exportiert zu werden
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_Es kann kein DEFFILE-Eintrag fÅr unit-globale Variablen erzeugt werden
parser_e_dlltool_unit_var_problem2=03161_E_öbersetze ohne "-WD"-Option
% You need to compile this file without the -WD switch on the
% commandline
parser_f_need_objfpc_or_delphi_mode=03162_F_Es muss der Modus ObjFPC (-S2) oder Delphi (-Sd) aktiv sein, um dieses Modul zu Åbersetzen
% You need to use {$mode objfpc} or {$mode delphi} to compile this file.
% Or use the equivalent commandline switches -S2 or -Sd.
parser_e_no_export_with_index_for_target=03163_E_Der Index darf nicht kleiner als $1 sein
% Exporting of functions or procedures with a specified index is not
% supported on this target.
parser_e_no_export_of_variables_for_target=03164_E_Das Ziel $1 unterstÅtzt das Exportieren von Variablen nicht
% Exporting of variables is not supported on this target.
parser_e_improper_guid_syntax=03165_E_Falscher GUID-Syntax
% The GUID indication does not have the proper syntax. It should be of the form
% \begin{verbatim}
% {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}
% \end{verbatim}
% Where each \var{X} represents a hexadecimal digit.
parser_w_interface_mapping_notfound=03168_W_Eine Prozedur mit dem Namen "$1", die $2.$3 implementieren kînnte, kann nicht gefunden werden
% The compiler cannot find a suitable procedure which implements the given method of an interface.
% A procedure with the same name is found, but the arguments do not match.
parser_e_interface_id_expected=03169_E_Interface-Bezeichner erwartet
% This happens when the compiler scans a \var{class} declaration that contains
% \var{interface} function name mapping code like this:
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% and the \var{interface} before the dot not listed in the inheritance list.
parser_e_type_cant_be_used_in_array_index=03170_E_Der Typ "$1" kann nicht als Array-Index verwendet werden
% Types like \var{qword} or \var{int64} aren't allowed as array index type
parser_e_no_con_des_in_interfaces=03171_E_Kon- und Destruktoren sind in Interfaces nicht erlaubt
% Constructor and destructor declarations aren't allowed in interface
% In the most cases the method \var{QueryInterface} of \var{IUnknown} can
% be used to create a new interface.
parser_e_no_access_specifier_in_interfaces=03172_E_Zugriffsbezeichner kînnen in Interfaces nicht benutzt werden
% The access specifiers \var{public}, \var{private}, \var{protected} and
% \var{pusblished} can't be used in interfaces because all methods
% of an interfaces must be public.
parser_e_no_vars_in_interfaces=03173_E_Ein Interface darf keine Felder enthalten
% Declarations of fields aren't allowed in interfaces. An interface
% can contain only methods
parser_e_no_local_proc_external=03174_E_Eine lokale Prozedur kann nicht als EXTERNAL deklariert werden
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high
parser_w_skipped_fields_before=03175_W_Einige Felder vor dem Feld "$1" sind nicht initialisiert
% In Delphi mode, not all fields of a typed constant record have to be
% initialized, but the compiler warns you when it detects such situations.
parser_e_skipped_fields_before=03176_E_Einige Felder vor dem Feld "$1" sind nicht initialisiert
% In all syntax modes but Delphi mode, you can't leave some fields uninitialized
% in the middle of a typed constant record
parser_w_skipped_fields_after=03177_W_Einige Felder nach dem Feld "$1" sind nicht initialisiert
% You can leave some fields at the end of a type constant record uninitialized
% (the compiler will initialize them to zero automatically). This may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_Verwendung von VarArgs (oder '...' in MacPas) ohne CDecl/CPPDecl/MWPascal oder External nicht mîglich
% The varargs directive (or the ``...'' varargs parameter in MacPas mode) can only be used with procedures or functions
% that are declared with \var{external} and one of \var{cdecl}, \var{cppdecl} and \var{mwpascal}. This functionality
% is only supported to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_Self muss ein Call-By-Value-Parameter sein
% You can't declare self as a const or var parameter, it must always be
% a call-by-value parameter
parser_e_interface_has_no_guid=03180_E_Interface "$1" hat keine Interface-Identifikation
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Klassenfeld oder Methoden-Bezeichner "$1" unbekannt
% Properties must refer to a field or method in the same class.
parser_w_proc_overriding_calling=03182_W_öberschreibe die Calling Convention "$1" mit "$2"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used
parser_e_no_procvarobj_const=03183_E_Die Konstante des Typs "procedure of object" kann nur mit NIL initialisiert werden
% You can't assign the address of a method to a typed constant which has a
% 'procedure of object' type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates on (which can not be known at compile time).
parser_e_default_value_only_one_para=03184_E_Der Default Value kann nur einem Parameter zugewiesen werden
% It is not possible to specify a default value for several parameters at once.
% The following is invalid:
% \begin{verbatim}
% Procedure MyProcedure (A,B : Integer = 0);
% \end{verbatim}
% Instead, this should be declared as
% \begin{verbatim}
% Procedure MyProcedure (A : Integer = 0; B : Integer = 0);
% \end{verbatim}
parser_e_default_value_expected_for_para=03185_E_Standard Parameter fÅr "$1" benîtigt
% The specified parameter requires a default value.
parser_w_unsupported_feature=03186_W_Verwendung eines nicht unterstÅtzten Features!
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_C Arrays werden "by reference" Åbergeben
% Any array passed to a C functions is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_C array of const muss letztes Argument sein
% You can not add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Erneute Definition des Typs "$1"
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a cause for errors.
parser_w_cdecl_has_no_high=03190_W_cdecl'ared Functionen haben keinen high Parameter
% Functions declared with cdecl modifier do not pass an extra implicit parameter.
parser_w_cdecl_no_openstring=03191_W_cdecl'ared Functionen unterstÅtzen keine open strings
% Openstring is not supported for cdecl'ared functions.
parser_e_initialized_not_for_threadvar=03192_E_Als threadvar deklarierte Variable kann nicht initialisiert werden
% Variables declared as threadvar can not be initialized with a default value.
% The variables will always be filled with zero at the start of a new thread.
parser_e_msg_only_for_classes=03193_E_Message directive ist nur in Klassen erlaubt
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_Procedure oder Function erwartet
% A class method can only be specified for procedures and functions.
parser_e_illegal_calling_convention=03195_W_Calling convention directive ignoriert: "$1"
% Some calling conventions are supported only by certain CPUs. I.e. most non-i386 ports support
% only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE kann in Objekten nicht benutzt werden
% \var{reintroduce} is not supported for objects.
parser_e_paraloc_only_one_para=03197_E_Jedes Argument muss seine eigene "location" haben
% If locations for arguments are specified explicitly as it is required by
% some syscall conventions, each argument must have it's only location, things
% like \var{procedure p(i,j : longint 'r1');} aren't allowed
parser_e_paraloc_all_paras=03198_E_Jedes Argument muss seine explizite "location" haben
% If one argument has an explicit argument location, all arguments of a procedure
% must have one.
parser_e_illegal_explicit_paraloc=03199_E_"location" des Arguments unbekannt
% The location specified for an argument isn't recognized by the compiler
parser_e_32bitint_or_pointer_variable_expected=03200_E_32 Bit-Integer oder Zeiger-Variable erwartet
% The libbase for MorphOS/AmigaOS can be give only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_Goto Statements zwischen verschiedenen Prozeduren sind nicht erlaubt
% It isn't allowed to use the \var{goto} statements referencing labels outside the
% current procedure. The following example shows the problem:
% \begin{verbatim}
% ...
%   procedure p1;
%   label
%     l1;
%
%     procedure p2;
%     begin
%       goto l1; // This goto ISN'T allowed
%     end;
%
%   begin
%     p2
%   l1:
%   end;
% ...
%
% \end{verbatim}
parser_f_too_complex_proc=03202_F_Prozedur zu komplex, sie erfordert zu viele Register
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_UngÅltiger Ausdruck
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
parser_e_invalid_integer=03204_E_UngÅltiger integer Ausdruck
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_UngÅltiger Qualifier
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_High Range Limit < low Range Limit
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
parser_e_macpas_exit_wrong_param=03207_E_Exits Parameter muss der Name der Prozedur sein, in der es benutzt wird
% Non local exit is not allowed. This error occurs only in mode MacPas.
parser_e_illegal_assignment_to_count_var=03208_E_UngÅltige Zuweisung zur for-loop Variable "$1"
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings. You can also not assign values to
% loop variables inside the loop (except in Delphi and TP modes). Use a while or
% repeat loop instead if you need to do something like that, since those
% constructs were built for that.
parser_e_no_local_var_external=03209_E_Lokale Variable kann nicht als EXTERNAL deklariert werden
% Declaring local variables as external is not allowed. Only global variables can reference
% to external variables.
parser_e_proc_already_external=03210_E_Prozedur ist bereits als EXTERNAL deklariert
% The procedure is already declared with the EXTERNAL directive in an interface or
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Implizite Verwendung einer Variant Unit
% The Variant type is used in the unit without any used unit using the Variants unit. The
% compiler has implicitly added the Variants unit to the uses list. To remove this warning
% the Variants unit needs to be added to the uses statement.
parser_e_no_static_method_in_interfaces=03212_E_Class und statische Methoden kînnen nicht in INTERFACES verwendet werden
% The specifier \var{class} and directive \var{static} can't be used in interfaces
% because all methods of an interfaces must be public.
parser_e_arithmetic_operation_overflow=03213_E_öberlauf in arithmetischer Operation
% An operation on two integers values produced an overflow
parser_e_protected_or_private_expected=03214_E_Protected oder private erwartet
% \var{strict} can be only used together with \var{protected} or \var{private}.
parser_e_illegal_slice=03215_E_SLICE kann nicht ausserhalb der Parameterliste benutzt werden
% \var{slice} can be used only for arguments accepting an open array parameter
parser_e_dispinterface_cant_have_parent=03216_E_Ein DISPINTERFACE kann keine Elternklasse haben.
% A DISPINMTERFACE is a special type of interface which can't have a parent class
parser_e_dispinterface_needs_a_guid=03217_E_Ein DISPINTERFACE benîtigt einen GUID
% A DISPINMTERFACE needs always an interface identification
parser_w_overridden_methods_not_same_ret=03218_W_öberschriebene Methoden mÅssen einen entsprechenden RÅckgabetyp haben. Dieser Code kann abstÅrzen, weil er von einem Delphi Parser Bug abhÑngt (Methode "$2" wird durch "$1" Åberschrieben, die einen anderen RÅckgabetyp hat).
% If you declare overridden methods in a class definition, they must
% have the same return type. Some versions of Delphi allow you to change the
% return type of interface methods, and even to change procedures into
% functions, but the resulting code may crash depending on the types used
% and the way the methods are called.
parser_e_dispid_must_be_ord_const=03219_E_Dispatch IDs mÅssen ganzzahlige Konstanten sein
parser_e_array_range_out_of_bounds=03220_E_Der Bereich des Array ist zu gross
% Regardless of the size taken up by its elements, an array cannot have more
% than high(ptrint) elements. Additionally, the range type must be a subrange
% of ptrint.
parser_e_packed_element_no_var_addr=03221_E_Bit packed Array-Elemente und Record-Felder haben keine Adressen
% If you declare an array or record as \var{packed} in Mac Pascal mode (or as \var{packed} in any mode with \var{\{\$bitpacking on\}}), it will
% be packed at the bit level. This means it becomes impossible to take addresses
% of individual array elements or record fields. The only exception to this rule is in case of packed arrays elements
% whose packed size is a multple of 8 bits.
parser_e_packed_dynamic_open_array=03222_E_Dynamische Arrays kînnen nicht packed sein
% Only regular and open arrays can be packed
parser_e_packed_element_no_loop=03223_E_Bit packed Array-Elemente und Record-Felder kînnen nicht als Loop-Variable verwerndet werden
% If you declare an array or record as \var{packed} in Mac Pascal mode (or as \var{packed} in any mode with \var{\{\$bitpacking on\}}), it will
% be packed at the bit level. For performance reasons, they cannot be
% used as loop variables.
parser_e_type_and_var_only_in_generics=03224_E_VAR und TYPE sind nur innerhalb "generics" erlaubt
% The usage of VAR and TYPE to declare new types inside an object is allowed only inside
% generics.
parser_e_cant_create_generics_of_this_type=03225_E_Dieser Typ kann nicht "generic" sein
% Only Classes, Objects, Interfaces and Records are allowed to be used as generic
parser_w_no_lineinfo_use_switch=03226_W_Die LINEINFO Unit nicht manuell laden. Verwende statt dessen den Compilerschalter -gl
% Do not use the LINEINFO unit directly, Use the \var{-gl} switch which automatically adds the
% unit for reading the selected type of debugging information instead.
parser_e_no_funcret_specified=03227_E_Kein Funktionsergebnistyp fÅr Funktion "$1" angegeben
% The first time you declare a function you have to declare it completely,
% including all parameters and the result type.
parser_e_special_onlygenerics=03228_E_Spezialisierung wird nur fÅr generische Typen unterstÅtzt
% Types not being generics can't be specialized
parser_e_no_generics_as_params=03229_E_Generische Typen kînnen bei der Spezialisierung generischer Typen nicht als Parameter benutzt werden
% When specializing a generic, only non-generic types can be used as parameters.
parser_e_type_object_constants=03230_E_Konstanten eines Objekts, das ein VMT enthÑlt, sind unzulÑssig
% If an object requires a VMT either because it contains a constructor or virtual methods,
% it's not allowed to create constants of it. In TP and Delphi mode this is allowed
% for compatibility reasons.
parser_e_label_outside_proc=03231_E_Die Address von Labels, die ausserhalb des aktuellen Scopes definiert wurden, kînnen nicht verwendet werden
% It isn't allowed to take the addresss of labels outside the
% current procedure.
parser_e_initialized_not_for_external=03233_E_Extern deklarierte Variablen kînnen nicht intialisiert werden 
% Variables declared as external can not be initialized with a default value.
parser_e_illegal_function_result=03234_E_UngÅltiger Funktionsergebnistyp
% Some types like file types can not be used as function result.
parser_e_no_common_type=03235_E_"$1" und "$2" haben keinen gemeinsamen Typ
% To perform an operation beween integers, the compiler converts both operands
% to their common type, which appears to be an invalid type. To determine the
% common type of the operands, the compiler takes the minimum of the minimal values
% of both types, and the maximum of the maximal values of both types. The common
% type is then minimum..maximum.
parser_e_no_generics_as_types=03236_E_Generische Typen kînnen nicht ohne Spezialisierung als Typ fÅr eine Variable verwendet werden
% Generics must be always specialized before being used as variable type
parser_w_register_list_ignored=03237_W_Registerliste wird in reinen Assemblerroutinen ignoriert
% When using pure assembler routines, the list with modified registers is ignored.
parser_e_implements_must_be_class_or_interface=03238_E_Die Implements-Eigenschaft muss einen Interface-Typ haben
parser_e_implements_must_have_correct_type=03239_E_Die Implements-Eigenschaft muss ein Interface mit dem richtigen Type implementieren; Es wurde Typ "$1" gefunden, aber Typ "$2" erwartet.
parser_e_implements_must_read_specifier=03240_E_Die Implements-Eigenschaft muss einen Lesen-Bezeichner haben
parser_e_implements_must_not_have_write_specifier=03241_E_Die Implements-Eigenschaft darf keinen Schreib-Bezeichner haben
parser_e_implements_must_not_have_stored_specifier=03242_E_Die Implements-Eigenschaft darf keinen Gespeichert-Bezeichner haben
parser_e_implements_uses_non_implemented_interface=03243_E_Die Implements-Eigenschaft benutzt das nicht implementierte Interface "$1"
parser_e_unsupported_real=03244_E_Fliesskommavariablen werden fÅr dieses Ziel nicht unterstÅtzt
% The compiler parsed a floating point expression, but it is not supported.
% \end{description}
#
# Type Checking
#
# 04083 is the last used one
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Typen passen nicht zusammen
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Inkompatible Typen: Habe "$1" erhalten, aber "$2" erwartet
% There is no conversion possible between the two types
% Another possiblity is that they are declared in different
% declarations:
% \begin{verbatim}
% Var
%    A1 : Array[1..10] Of Integer;
%    A2 : Array[1..10] Of Integer;
%
% Begin
%    A1:=A2; { This statement gives also this error, it
%              is due the strict type checking of pascal }
% End.
% \end{verbatim}
type_e_not_equal_types=04002_E_Typen sind verschieden bei $1 und $2
% The types are not equal
type_e_type_id_expected=04003_E_Typbezeichner erwartet
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Variablenbezeichner erwartet
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_Integer-Ausdruck erwartet, aber "$1" erhalten
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Booleschen Ausdruck erwartet, aber "$1" erhalten
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Ganzzahliger Ausdruck erwartet
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_Zeigertyp erwartet, aber "$1" erhalten
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_Klassentyp erwartet, aber "$1" erhalten
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_Kann konstanten Ausdruck nicht auswerten
% This error can occur when the bounds of an array you declared does
% not evaluate to ordinal constants
type_e_set_element_are_not_comp=04012_E_Set-Elemente sind nicht kompatibel
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Operation fÅr Sets nicht implementiert
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Automatische Typumwandlung von Fliesskommatyp nach COMP (=integer mit 64 bit)
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{Comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_Verwenden sie DIV um ein Integer-Ergebnis zu erhalten
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_Stringtypen passen nicht zueinander, da in "$V+"-Modus
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_Succ oder Pred kann nicht auf AufzÑhlungen mit Zuweisungen angewendet werden
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_Kann Variablen dieses Typs nicht lesen oder schreiben
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% reals, pchars and strings can be read from/written to a text file.
% Booleans can only be written to text files.
type_e_no_readln_writeln_for_typed_file=04019_E_Kann ReadLn und WriteLn nicht bei typisierten Dateien verwenden
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_Kann Read und Write nicht bei untypisierten Dateien verwenden
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Typkonflikt zwischen den Elementen des Sets
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(dword/qword) gibt oberes/unteres Word/DWord zurÅck
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Integer- oder Real-Ausdruck erwartet
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=04024_E_Falscher Typ "$1" im Array-Konstruktor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Inkompatible Typen fÅr Argument Nr. #$1: habe $2 erhalten, aber $3 erwartet
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=04026_E_Methode (Variable) und Prozedur (Variable) sind nicht kompatibel
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_UnzulÑssige Konstante an interne Algebrafunktion Åbergeben
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_Von Konstanten kann keine Adresse bestimmt werden
% It's not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant. This error
% can also be displayed if you try to pass a property to a var parameter.
type_e_argument_cant_be_assigned=04029_E_An das Argument kann nichts zugewiesen werden
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they can't be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_Kann lokale Prozedur/Funktion nicht an Prozedurvariable zuweisen
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Kann einer Adresse keine Werte zuweisen
% It's not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Kann einer Konstanten keine Werte zuweisen
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing make the parameter value or var.
type_e_array_required=04033_E_Array Typ notwendig
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode also a pointer is allowed.
type_e_interface_type_expected=04034_E_Interface Typ erwartet, aber "$1" erhalten
% The compiler expected to encounter an interface type name, but got something else.
% The following code would provoke this error:
% \begin{verbatim}
% Type
%   TMyStream = Class(TStream,Integer)
% \end{verbatim}
type_w_mixed_signed_unsigned=04035_W_Mischen von signed AusdrÅcken und Longwords ergibt ein 64bit Ergebnis
% If you divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64bit which is slower than normal
% 32bit arithmetic. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_w_mixed_signed_unsigned2=04036_W_Mischen von signed AusdrÅcken und kardinalen Typen hier kann einen BereichsprÅfungsfehler verursachen
% If you use a binary operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% carried out. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_Typecast hat verschiedene Grîssen ($1 -> $2) in der Zuweisung
% Type casting to a type with a different size is not allowed when the variable is
% used for assigning.
type_e_array_index_enums_with_assign_not_possible=04038_E_enums mit Zuweisungen kînnen nicht als Array-Index verwendet werden
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use it as index of an array.
type_e_classes_not_related=04039_E_Klassen- oder Objekttypen "$1" and "$2" sind nicht verwandt
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_w_classes_not_related=04040_W_Klassentypen "$1" und "$2" sind nicht verwandt
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_e_class_or_interface_type_expected=04041_E_Klassen- oder Interface-Typ erwartet, aber "$1" erhalten
% The compiler expected a class or interface name, but got another type or identifier.
type_e_type_is_not_completly_defined=04042_E_Typ "$1" ist nicht vollstÑndig definiert
% This error occurs when a type is not complete: i.e. a pointer type which points to
% an undefined type.
type_w_string_too_long=04043_W_String literal hat mehr Zeichen als short string length
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring
type_w_signed_unsigned_always_false=04044_W_Wegen des Bereichs der Werte ist das Vergleichsergebnis immer falsch
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% false. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_signed_unsigned_always_true=04045_W_Wegen des Bereichs der Werte ist das Vergleichsergebnis immer richtig
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% true. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_instance_with_abstract=04046_W_Konstruktion der Klasse "$1" mit abstrakten Methoden
% An instance of a class is created which contains non-implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overriden.
type_h_in_range_check=04047_H_Der linke Operand des IN Operators sollte byte Grîsse haben
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8-bits, this may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_Unpassende Typgrîssen, Gefahr des Datenverlusts oder BereichsprÅfungsfehlers
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_h_smaller_possible_range_check=04049_H_Unpassende Typgrîssen, Gefahr des Datenverlusts oder BereichsprÅfungsfehlers
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_e_cant_take_address_of_abstract_method=04050_E_Die Adresse einer abstrakten Methode kann nicht verwendet werden
% An abstract method has no body, so the address of an abstract method can't be taken.
type_e_assignment_not_allowed=04051_E_Zuweisungen auf formale Parameter und offene Arrays sind nicht mîglich
% You are trying to assign a value to a formal (untyped var, const or out)
% parameter, or to an open array.
type_e_constant_expr_expected=04052_E_Konstanter Ausdruck erwartet
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_Operation "$1" wird fÅr die Typen "$2" und "$3" nicht unterstÅtzt
% The operation is not allowed for the supplied types
type_e_illegal_type_conversion=04054_E_Illegale Typ-Konversion: "$1" nach "$2"
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
type_h_pointer_to_longint_conv_not_portable=04055_H_Konversion zwischen ordinalen Typen und Zeigern ist nicht portierbar
% If you typecast a pointer to a longint (or vice-versa), this code will not compile
% on a machine using 64-bit for pointer storage.
type_w_pointer_to_longint_conv_not_portable=04056_W_Konversion zwischen ordinalen Typen und Zeigern ist nicht portierbar
% If you typecast a pointer to a ordinal type of a different size (or vice-versa), this can
% cause problems. This is a warning to help finding the 32bit specific code where cardinal/longint is used
% to typecast pointers to ordinals. A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_Kann nicht bestimmen, welche der Åberladenen Funktionen aufgerufen werden soll
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Loop-Variable muss einen ordinalen Typ haben
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
type_w_double_c_varargs=04059_W_Der konstante Wert vom Typ real wird fÅr ein C Variablen-Argument zu double konvertiert. ErgÑnze eine explizite Typ-Konversion (typecast), um das zu verhindern
% In C, constant real values are double by default. For this reason, if you
% pass a constant real value to a variable argument part of a C function, FPC
% by default converts this constant to double as well. If you want to prevent
% this from happening, add an explicit typecast around the constant.
type_e_class_or_cominterface_type_expected=04060_E_Class oder COM interface Typ erwartet, statt dessen "$1" erhalten
% Some operators like the AS operator are only appliable to classes or COM interfaces.
type_e_no_const_packed_array=04061_E_Konstante packed Arrays werden noch nicht unterstÅtzt
% You cannot declare a (bit)packed array as a typed constant.
type_e_got_expected_packed_array=04062_E_Inkompatibler Typ fÅr Argument $1: Erhielt "$2" erwartete "(Bit)Packed Array"
% The compiler expects a (bit)packed array as the specified parameter
type_e_got_expected_unpacked_array=04063_E_Inkompatibler Typ fÅr Argument $1: Erhielt "$2" erwartete "(not packed) Array"
% The compiler expects a regular (i.e., not packed) array as the specified parameter
type_e_no_packed_inittable=04064_E_Elemente von packed Arrays kînnen nicht von einem Typ sein, der initialisiert werden muss
% Support for packed arrays of types that need initialization (such as ansistrings, or records which contain ansistrings) is not yet implemented.
type_e_no_const_packed_record=04065_E_Konstante packed Records und Objekte werden noch nicht unterstÅtzt
% You cannot declare a (bit)packed array as a typed constant at this time.
type_w_untyped_arithmetic_unportable=04066_W_Arithmetik "$1" mit typenlosem Pointer ist nicht portierbar nach {$T+}, schlage typecast vor
% Addition/subtraction from untyped pointer may work differently in \var{\{\$T+\}}, use typecast to typed pointer
type_e_cant_take_address_of_local_subroutine=04076_E_Die Address einer Subroutine, die als local markiert ist, kann nicht verwendet werden
% The address of a subroutine marked as local can't be taken.
type_e_cant_export_local=04077_E_Eine Subroutine, die aus einer Unit als local markiert ist, kann nicht exportiert werden
% A subroutine marked as local can't be export from a unit.
type_e_not_automatable=04078_E_Typ "$1" ist nicht "automatable"
% Only byte, integer, longint, smallint, currency, single, double, ansistring,
% widestring, tdatetime, variant, olevariant, wordbool and all interfaces are automatable.
type_h_convert_add_operands_to_prevent_overflow=04079_H_Konvertierung des Operanden "$1" vor der Addition kînnte öberlauf Fehler verhindern
% Adding two types can cause overflow errors. Since you are converting the result to a larger type, you
% could prevent such errors by converting the operands to this type before doing the addition.
type_h_convert_sub_operands_to_prevent_overflow=04080_H_Konvertierung des Operanden "$1" vor der Subtraktion kînnte öberlauf Fehler verhindern
% Subtracting two types can cause overflow errors. Since you are converting the result to a larger type, you
% could prevent such errors by converting the operands to this type before doing the subtraction.
type_h_convert_mul_operands_to_prevent_overflow=04081_H_Konvertierung des Operanden "$1" vor der Multiplikation kînnte öberlauf Fehler verhindern
% Multiplying two types can cause overflow errors. Since you are converting the result to a larger type, you
% could prevent such errors by converting the operands to this type before doing the multiplication.
type_w_pointer_to_signed=04082_W_Die Konvertierung von Pointern in einen Integertyp mit Vorzeichen kann zu falschen Ergebnissen bei Vergleichen und zu BereichsÅberschreitungen fÅhren; verwenden sie statt dessen besser einen Typ ohne Vorzeichen
% The virtual address space on 32-bit machines runs from \$00000000 to \$ffffffff. Many operating systems allow you to
% allocate memory above \$80000000, for example both Windows and Linux allow pointers in the range \$0000000 to \$bfffffff.
% If you convert pointers to signed types, this can cause overflow and range check errors, but also \$80000000 < \$7fffffff.
% This can cause random errors in code like "if p>q".
type_interface_has_no_guid=04083_E_Interface Typ $1 hat keine gÅltige GUID
% When applying the as-operator to an interface or class, the desired interface, i.e. the right operand of the
% as-operator must have a valid GUID
% \end{description}
#
# Symtable
#
# 05062 is the last used one
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Bezeichner "$1" nicht gefunden
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Interner Fehler in SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Doppelter Bezeichner "$1"
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_Bezeichner ist bereits definiert in $1 in Zeile $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Unbekannter Bezeichner "$1"
% The identifier encountered hasn't been declared, or is used outside the
% scope where it's defined.
sym_e_forward_not_resolved=05005_E_Forward-Deklaration "$1" nicht gefunden
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Fehler in Typdefinition
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_Forward-Typ "$1" nicht gefunden
% The compiler encountered an unknown type.
sym_e_only_static_in_static=05010_E_Nur statische Variablen kînnen in statischen oder Ñusseren Methoden verwendet werden
% A static method of an object can only access static variables.
sym_f_type_must_be_rec_or_class=05012_F_Record- oder Klassen-Typ erwartet
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Instanzen von Klassen oder Objekten mit abstrakten Methoden sind unzulÑssig
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Label "$1" nicht definiert
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Label "$1" benutzt aber nicht definiert
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_UngÅltige Label-Deklaration
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO und LABEL werden nicht unterstÅtzt (verwenden Sie den Schalter -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Label nicht gefunden
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_Bezeichner ist kein Label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_Label ist bereits definiert
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_UngÅltige Typdeklaration von Set-Elementen
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Forward-Klassendefinition nicht gefunden: $1
% You declared a class, but you didn't implement it.
sym_n_unit_not_used=05023_H_Unit "$1" wird von "$2" nicht verwendet
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Parameter nicht verwendet: $1
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=05025_N_Lokale Variable nicht verwendet: $1
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_Werte-Parameter "$1" wurde zugewiesen, aber nie verwendet
% This is a warning. The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_Lokale Variable "$1" wurde zugewiesen, aber nicht verwendet
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Lokales $1-Element "$2" wird nicht verwendet
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_Privates Feld $1.$2 wird nie verwendet
% The indicated private field is defined, but is never used in the code.
sym_n_private_identifier_only_set=05030_N_Privates Feld $1.$2 wurde zugewiesen, aber nie verwendet
% The indicated private field is declared, assigned but never read.
sym_n_private_method_not_used=05031_N_Private Methode $1.$2 wird nie verwendet
% The indicated private method is declared but is never used in the code.
sym_e_set_expected=05032_E_Mengentyp erwartet
% The variable or expression isn't of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_Funktionsergebnis scheint nicht gesetzt zu sein
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_Typ "$1" ist im aktuellen Record mit C-Packing nicht korrekt ausgerichtet
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Unbekannter Record-Feldbezeichner $1
% The field doesn't exist in the record definition.
sym_w_uninitialized_local_variable=05036_W_Lokale Variable "$1" wird verwendet, bevor ihr ein Wert zugewiesen wurde
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_uninitialized_variable=05037_W_Variable "$1" scheint nicht initialisiert worden zu sein
% These messages are displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_Bezeichener verweist nicht auf ein Element: $1
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the class you are trying to create. The procedure you specified
% does not exist.
sym_h_param_list=05039_H_Deklaration gefunden: $1
% You get this when you use the \var{-vh} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Datensegment zu gross (max. 2GB)
% You get this when you declare an array whose size exceeds the 2GB limit.
sym_e_no_matching_implementation_found=05042_E_Keine passende Implementation der Interface Methode "$1" gefunden
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_Symbol "$1" ist veraltet
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Usage of this symbol
% should be avoided as much as possible.
sym_w_non_portable_symbol=05044_W_Symbol "$1" ist nicht portierbar
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, usage
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_Symbol "$1" ist nicht implementiert
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_Von diesem Typ kann kein eindeutiger Typ deklariert werden
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}.
sym_h_uninitialized_local_variable=05057_H_Die Variable "$1" scheint nicht initialisiert zu sein
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_h_uninitialized_variable=05058_H_Die Variable "$1" scheint nicht initialisiert zu sein
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_function_result_uninitialized=05059_W_Die Ergebnisvariable der Funktion scheint nicht initialisiert zu sein
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_h_function_result_uninitialized=05060_H_Die Ergebnisvariable der Funktion scheint nicht initialisiert zu sein
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_w_identifier_only_read=05061_W_Die Variable "$1" wird gelesen, obwohl ihr aber noch kein Wert zugewiesen wurde
% You have read the value of a variable, but nowhere assigned a value to
% it.
sym_h_abstract_method_list=05062_H_Abstrakte Method "$1" gefunden 
% When getting a warning about constructing a class/object with abstract methods
% you get this hint to find the affected method.
sym_w_experimental_symbol=05063_W_Symbol "$1" ist experimentell
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{experimental} is used. Experimental symbols
% might disappear or change semantics in future versions. Usage of this symbol
% should be avoided as much as possible.
% \end{description}
#
# Codegenerator
#
# 06049 is the last used one
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_Grîsse der Parameterliste Åbersteigt 65535 Bytes
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_file_must_call_by_reference=06012_E_Dateitypen mÅssen VAR Parameter sein
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_Die Verwendung eines FAR-Zeigers ist in dieser Art nicht erlaubt
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_Mit EXPORT deklarierte Funktionen kînnen nicht aufgerufen werden
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Mîglicherweise unzulÑssiger Aufruf eines Konstruktors oder Destruktors (passt nicht in diesen Kontext)
% The compiler detected that a constructor or destructor is called within a
% a method. This will probably lead to problems, since constructors / destructors
% require parameters on entry.
cg_n_inefficient_code=06017_N_Uneffiziente Programmierung
% You construction seems dubious to the compiler.
cg_w_unreachable_code=06018_W_Code wird niemals ausgefÅhrt
% You specified a loop which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Abstrakte Methoden kînnen nicht direkt aufgerufen werden
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Register $1 Gewichtung $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_Stackframe wird ausgelassen
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=06031_E_Objektmethoden kînnen nicht Inline sein
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Procvar-Aufrufe kînnen nicht Inline sein
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_Kein Code fÅr Inline-Prozedur gespeichert
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Auf Element Null von Ansi/Wide- oder Longstring kann nicht zugegriffen werden, benutzen Sie stattdessen (Set)Length
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such kinf of string
cg_e_cannot_call_cons_dest_inside_with=06037_E_Konstruktoren und Destruktoren kînnen in diesem Kontext nicht aufgerufen werden
% Inside a \var{With} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_Kann Messagehandler-Methode nicht direkt aufrufen
% A message method handler method can't be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=06039_E_Sprung in- oder aus dem Exceptionblock heraus
% It isn't allowed to jump in or outside of an exception block like \var{try..finally..end;}:
% \begin{verbatim}
% label 1;
%
% ...
%
% try
%    if not(final) then
%      goto 1;   // this line will cause an error
% finally
%   ...
% end;
% 1:
% ...
% \end{verbatim}
cg_e_control_flow_outside_finally=06040_E_Kontrollfluss-Anweisungen sind in einem Finally-Block nicht erlaubt
% It isn't allowed to use the control flow statements \var{break},
% \var{continue} and \var{exit}
% inside a finally statement. The following example shows the problem:
% \begin{verbatim}
% ...
%   try
%      p;
%   finally
%      ...
%      exit;  // This exit ISN'T allowed
%   end;
% ...
%
% \end{verbatim}
% If the procedure \var{p} raises an exception the finally block is
% executed. If the execution reaches the exit, it's unclear what to do:
% exiting the procedure or searching for another exception handler
cg_w_parasize_too_big=06041_W_Grîsse der Parameter Åberschreitet die Grenze fÅr bestimmte CPUs
% This indicates that you are declaring more than 64K of parameters, which
% might not be supported on other processor targets.
cg_w_localsize_too_big=06042_W_Grîsse der lokalen Variablen Åberschreitet die Grenze fÅr bestimmte CPUs
% This indicates that you are declaring more than 32K of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_Grîsse der lokalen Variablen Åberschreitet die unterstÅtzte Grenze
% This indicates that you are declaring more than 32K of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK nicht zulÑssig
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE nicht zulÑssig
% You're trying to use \var{continue} outside a loop construction.
cg_f_unknown_compilerproc=06046_F_Unbekannte Compiler-Prozedur "$1". öberprÅfe, ob die korrekte Laufzeit-Bibliothek verwendet wird.
% The compiler expects that the runtime library contains certain subroutines. If you see this error
% and you didn't change the runtime library code, it's very likely that the runtime library
% you're using doesn't match the used compiler. If you changed the runtime library this error means
% that you removed a subroutine which the compiler needs for internal use.
cg_f_unknown_system_type=06047_F_Systemtyp "$1" konnte nicht gefunden werden. öberprÅfe ob die korrekte Laufzeit-Bibliothek verwendet wird.
% The compiler expects that the runtime library contains certain type definitions. If you see this error
% and you didn't change the runtime library code, it's very likely that the runtime library
% you're using doesn't match the used compiler. If you changed the runtime library this error means
% that you removed a type which the compiler needs for internal use.
cg_h_inherited_ignored=06048_H_Geerbter Aufruf einer abstrakten Methode ignoriert
% This messages appears only in Delphi mode when you call an abstract method
% of a parent class via \var{inherited;}. The call is then ignored.
cg_e_goto_label_not_found=06049_E_Goto Label "$1": Das Label ist nicht definiert oder wurde bei der Optimierung entfernt
% The label used in the goto definition is not defined or optimized away by the
% unreachable code elemination.
% \end{description}
# EndOfTeX

#
# Assembler reader
#
# 07107 is the last used one
#
asmr_d_start_reading=07000_DL_Starte $1 Stil Assembler Parsen
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_DL_Beende $1 Stil Assembler Parsen
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Nicht-Label Bezeichner enthÑlt @
% A identifier which isn't a label can't contain a @.
asmr_e_building_record_offset=07004_E_Fehler beim Ermitteln des Recordoffsets
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET ohne Bezeicner verwendet
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE verwendet ohne Bezeichner
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_Lokale Variable oder Parameter kînnen hier nicht verwendet werden
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the %ebp register so the
% address can't be get directly.
asmr_e_need_offset=07008_E_Hier muss OFFSET verwendet werden
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_Hier muss "$" verwendet werden
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_Verwendung von mehreren verschiebbaren Symbolen nicht mîglich
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Verschiebbares Symbol kann nur addiert werden
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_UngÅltiger Konstantenausdruck
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Verschiebbares Symbol ist nicht zulÑssig
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_UngÅltige Verweis-Syntax
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_Sie kînnen "$1" von diesem Code aus nicht erreichen
% You can not read directly the value of a local variable or parameter
% of a higher level procedure in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Lokale Symbole/Labels sind nicht als Referenz zulÑssig
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_UngÅltige Verwendung von Basis- und Index-Registern
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Mîglicher Fehler bei Objektfeld-Behandlung
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Falscher Skalierungsfaktor angegeben
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Mehrfache Verwendung fon Index-Registern
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_UngÅltiger Operandentyp
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_UngÅlitge Zeichenkette als Opcode-Operand: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE und @DATA werden nicht unterstÅtzt
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Null-Label-Bezug nicht mîglich
asmr_e_expr_zero_divide=07025_E_Division durch Null in Assembler-Ausdruck
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_UngÅltiger Ausdruck
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_Escape-Sequenz ignoriert: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_UngÅltige Symbolverwendung
asmr_w_fwait_emu_prob=07029_W_FWAIT kann Emulationsprobleme mit emu387 verursachen
asmr_w_fadd_to_faddp=07030_W_$1 ohne Operand wurde in $1P Åbersetzt
asmr_w_enter_not_supported_by_linux=07031_W_Der ENTER-Befehl wird vom Linux-Kernel nicht unterstÅtzt
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Aufruf einer Åberladenen Funktion in Assembler
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_Nicht unterstÅtzter Symboltyp fÅr Operand
asmr_e_constant_out_of_bounds=07034_E_Wert der Konstante ausserhalb des zulÑssigen Bereichs
asmr_e_error_converting_decimal=07035_E_Fehler beim Umwandeln in Dezimal $1
% A constant decimal value does not have the correct syntax
asmr_e_error_converting_octal=07036_E_Fehler beim Umwandeln in Oktal $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Fehler beim Umwandeln in BinÑr $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Fehler beim Umwandeln in Hexadezimal $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 Åbersetzt nach $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 ist einer Åberladenen Funktion zugeordnet
asmr_e_cannot_use_SELF_outside_a_method=07041_E_Kann SELF nicht ausserhalb einer Methode verwenden
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_Kann OLDEBP ausserhalb einer verschachtelten Prozedur nicht verwenden
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Prozeduren kînnen keinen Wert im Assembler-Code zurÅckliefern
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG nicht unterstÅtzt
asmr_e_size_suffix_and_dest_dont_match=07045_E_Grîssensuffix und Ziel- oder Quellgrîsse passen nicht zusammen
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_Grîssensuffix und Ziel- oder Quellgrîsse passen nicht zusammen
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Assembler Syntaxfehler
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_UngÅltige Kombination von Opcode und Operanden
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Assembler Syntaxfehler im Operanden
asmr_e_syn_constant=07050_E_Assembler Syntaxfehler in Konstanten
asmr_e_invalid_string_expression=07051_E_UngÅltiger Stringausdruck
asmr_w_const32bit_for_address=07052_W_Konstante mit Symbol $1 fÅr Adresse erzeugt, die kein Pointer ist
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Unbekannter Opcode $1
% This opcode is not known
asmr_e_invalid_or_missing_opcode=07054_E_UngÅltiger oder fehlender Opcode
asmr_e_invalid_prefix_and_opcode=07055_E_UngÅltige Kombination von Prefix und Opcode: $1
asmr_e_invalid_override_and_opcode=07056_E_UngÅltige Kombination von Override und Opcode: $1
asmr_e_too_many_operands=07057_E_Zu viele Operanden in der Zeile
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR ignoriert
asmr_w_far_ignored=07059_W_FAR ignoriert
asmr_e_dup_local_sym=07060_E_Doppelters lokales Symbol $1
asmr_e_unknown_local_sym=07061_E_Undefiniertes lokales Symbol $1
asmr_e_unknown_label_identifier=07062_E_Unbekannter Label-Bezeichner $1
asmr_e_invalid_register=07063_E_UngÅltiger Registername
% There is an unknown register name used as operand.
asmr_e_invalid_fpu_register=07064_E_UngÅltiger Name fÅr Fliesskommaregister
% There is an unknown register name used as operand.
asmr_w_modulo_not_supported=07066_W_Modulo nicht unterstÅtzt
asmr_e_invalid_float_const=07067_E_UngÅltige Fliesskommakonstante $1
% The floating point constant declared in an assembler block is
% invalid.
asmr_e_invalid_float_expr=07068_E_UngÅltiger Fliesskommaausdruck
% The floating point expression declared in an assembler block is
% invalid.
asmr_e_wrong_sym_type=07069_E_Falscher Symboltyp
asmr_e_cannot_index_relative_var=07070_E_Kann lokale Variable oder Parameter nicht mit Register indizieren
% Trying to index using a base register a symbol which is already relative
% to a register. This is not possible, and will probably lead to crashes.
asmr_e_invalid_seg_override=07071_E_UngÅltiger Segmentoverride-Ausdruck
asmr_w_id_supposed_external=07072_W_Bezeichner $1 ist vermutlich External
% There is a reference to an undefined symbol. This will not result
% in an error, since the symbol might be external, but may cause
% problems at link time if the symbol is not defined anywhere.
asmr_e_string_not_allowed_as_const=07073_E_Strings sind als Konstanten unzulÑssig
% Character strings are not allowed as constants.
asmr_e_no_var_type_specified=07074_Typ der Variablen nicht angegeben
% The syntax expects a type idenfitifer after the dot, but
% none was found.
asmr_w_assembler_code_not_returned_to_text=07075_E_Assemblercode kehrt nicht zum Text zurÅck
% There was a directive in the assembler block to change sections,
% but there is a missing return to the text section at the end
% of the assembler block. This might cause errors during link time.
asmr_e_not_directive_or_local_symbol=07076_E_Keine Direktive oder lokales Symbol $1
% This symbol is unknown.
asmr_w_using_defined_as_local=07077_E_Verwendung eines definierten Namens als lokales Label
asmr_e_dollar_without_identifier=07078_E_Dollarzeichen wird ohne Bezeichner verwendet
% A constant expression has an identifier which does not start with
% the $ symbol.
asmr_w_32bit_const_for_address=07079_W_32-Bit-Konstante fÅr Adresse erzeugt
% A constant was used as an address. This is probably an error,
% since using absolute addresses will probably not work.
asmr_n_align_is_target_specific=07080_N_.align ist abhÑngig von Zielplattform, verwende .balign oder .p2align
% Using the .align directive is platform specific, and its meaning will vary
% from one platform to another.
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Kann fÅr Parameter nicht direkt auf Felder zugreifen
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_Kann auf Felder von Objekten/Klassen nicht direkt zugreifen
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_Grîsse nicht spezifiziert und auch keine Mîglichkeit die Grîsse der Operanden zu bestimmen
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=07084_E_RESULT kann in dieser Funktion nicht verwendet werden
% Some functions which return complex types cannot use the \var{result}
% keyword.
asmr_w_adding_explicit_args_fXX=07086_W_"$1" ohne Operand Åbersetzt in "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" Åbersetzt in "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" Åbersetzt in "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Das Zeichen < ist hier nicht erlaubt
% The shift operator requires the << characters. Only one
% of those characters was found.
asmr_e_invalid_char_greater=07090_E_Das Zeichen > ist hier nicht erlaubt
% The shift operator requires the >> characters. Only one
% of those characters was found.
asmr_w_align_not_supported=07093_W_ALIGN wird nicht unterstÅtzt
asmr_e_no_inc_and_dec_together=07094_E_Inc und Dec kînnen nicht gemeinsam vorkommen
% Trying to use an increment and a decrement within the same
% opcode on the 680x0. This is impossible.
asmr_e_invalid_reg_list_in_movem=07095_E_UngÅltige Registerliste fÅr movem
% Trying to use the \var{movem} opcode with invalid registers
% to save or restore.
asmr_e_invalid_reg_list_for_opcode=07096_E_UngÅltige Registerliste fÅr diesen opcode
asmr_e_higher_cpu_mode_required=07097_E_Hîherer cpu Modus notwendig ($1)
% Trying to use an instruction which is not supported in the current
% cpu mode. Use a higher cpu generation to be able to use this
% opcode in your assembler block
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_Grîsse nicht spezifiziert und auch keine Mîglichkeit die Grîsse der Operanden zu bestimmen. Es wird DWORD als Voreinstellung verwendet
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Syntax Fehler beim Analysieren eines shifter Operanden
% ARM only; ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
asmr_e_packed_element=07100_E_Die Adresse der packed Komponente ist nicht an einer Byte Grenze
% Packed components (record fields and array elements) may start at an arbitrary
% bit inside a byte. On CPU which do not support bit-addressable memory (which
% includes all currently supported CPUs by FPC) you will therefore get an error
% message when trying to index arrays with elements whose size is not a multiple
% of 8 bits. The same goes for accessing record fields with such an address.
% multiple of 8 bits.
asmr_w_unable_to_determine_reference_size_using_byte=07101_W_Grîsse nicht angegeben und kann auch nicht aus der Grîsse der Operanden bestimmt werden. Verwende BYTE als Voreinstellung
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use BYTE as default.
asmr_w_no_direct_ebp_for_parameter=07102_W_Die Verwendung von +offset(%ebp) fÅr Parameter ist hier ungÅltig
% Using direct 8(%ebp) reference for function/procedure parameters is invalid
% if parameters are in registers.
asmr_w_direct_ebp_for_parameter_regcall=07103_W_Die Verwendung von +offset(%ebp) ist nicht mit der regcall Konvention kompatibel
% Using direct 8(%ebp) reference for function/procedure parameters is invalid
% if parameters are in registers.
asmr_w_direct_ebp_neg_offset=07104_W_Die Verwendung von -offset(%ebp) wird fÅr den Zugriff auf lokale Variablen nicht empfohlen
% Using -8(%ebp) to access a local variable is not recommended
asmr_w_direct_esp_neg_offset=07105_W_Verwendung von -offset(%esp); Zugriff kann einen Crash oder Datenverlust auslîsen
% Using -8(%esp) to access a local stack is not recommended, as
% this stack portion can be overwritten by any function calls or interrupts.
asmr_e_no_vmtoffset_possible=07106_E_VMTOffset muss in Kombination mit einer virtuellen Methode verwendet werden; "$1" ist aber nicht virtuell
% Only virtual methods have VMT offsets
asmr_e_need_pic_ref=07107_E_Erzeuge eigentlich PIC, aber die Referenz ist nicht PIC-sicher
% The compiler has been configured to generate position-independent code
% (PIC), but there are position-dependent references in the current
% handwritten assembler instruction.
#
# Assembler/binary writers
#
# 08020 is the last used one
#
asmw_f_too_many_asm_files=08000_F_Zu viele Assembler-Dateien
% With smartlinking enabled, there are too many assembler
% files generated. Disable smartlinking.
asmw_f_assembler_output_not_supported=08001_F_GewÑhlte Assemblerausgabe wird nicht unterstÅtzt
asmw_f_comp_not_supported=08002_F_Comp nicht unterstÅtzt
asmw_f_direct_not_supported=08003_F_Direct nicht unterstÅtzt fÅr binÑres Schreiben
% Direct assembler mode is not supported for binary writers.
asmw_e_alloc_data_only_in_bss=08004_E_Allozieren von Daten ist nur in "bss"-Abschnitten zulÑssig
asmw_f_no_binary_writer_selected=08005_F_Kein BinÑrschreiber ausgewÑhlt
asmw_e_opcode_not_in_table=08006_E_Asm: Opcode $1 nicht in Tabelle enthalten
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 ungÅltige Kombination von Opcode und Operanden
asmw_e_16bit_not_supported=08008_E_Asm: 16-Bit-Verweise werden nicht unterstÅtzt
asmw_e_invalid_effective_address=08009_E_Asm: UngÅltige effektive Adresse
asmw_e_immediate_or_reference_expected=08010_E_Asm: Konstanter Ausdruck oder Referenz erwartet
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 Wert Åberschreitet Grenzen $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: "Short jump" ist ausserhalb des Bereichs $1
asmw_e_undefined_label=08013_E_Asm: Undefiniertes Label: $1
asmw_e_comp_not_supported=08014_E_Asm: Comp wird fÅr dieses Ziel nicht unterstÅtzt
asmw_e_extended_not_supported=08015_E_Asm: Extended Typ wird fÅr dieses Ziel nicht unterstÅtzt
asmw_e_duplicate_label=08016_E_Asm: Doppeltes Label $1
asmw_e_redefined_label=08017_E_Asm: Neu definiertes Label $1
asmw_e_first_defined_label=08018_E_Asm: First beginnt hier
asmw_e_invalid_register=08019_E_Asm: UngÅltiges Register $1
asmw_e_16bit_32bit_not_supported=08020_E_Asm: 16 oder 32 Bit Referenzen werden nicht unterstÅtzt
asmw_e_64bit_not_supported=08021_E_Asm: 64 Bit Operanden werden nicht unterstÅtzt

#
# Executing linker/assembler
#
# 09028 is the last used one
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Quellbetriebssystem wurde neu definiert
% The source operating system is redefined.
exec_i_assembling_pipe=09001_I_Assembliere (pipe) $1
% Assembling using a pipe to an external assembler.
exec_d_cant_create_asmfile=09002_E_Kann Assemblerdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_objectfile=09003_E_Kann Objektdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_archivefile=09004_E_Kann Archivdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_assembler_not_found=09005_E_Assembler $1 nicht gefunden, schalte um zu externem Assemblieren
% The assembler program was not found. The compiler will produce a script that
% can be used to assemble and link the program.
exec_t_using_assembler=09006_T_Benutze Assembler: $1
% Information message saying which assembler is being used.
exec_e_error_while_assembling=09007_E_Fehler beim Assemblieren des Exitcodes $1
% There was an error while assembling the file using an external assembler.
% Consult the documentation of the assembler tool to find out more information
% on this error.
exec_e_cant_call_assembler=09008_E_Assembler kann nicht aufgerufen werden, Fehler $1 schalte um zu externem Assemblieren
% An error occurred when calling an external assembler, The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_assembling=09009_I_Assembliere $1
% An informational message stating which file is being assembled.
exec_i_assembling_smart=09010_I_Assembliere mit Smartlinking $1
% An informational message stating which file is being assembled using smartlinking.
exec_w_objfile_not_found=09011_W_Objekt $1 nicht gefunden, Linken kann fehlschlagen!
% One of the object file is missing, and linking will probably fail.
% Check your paths.
exec_w_libfile_not_found=09012_W_Bibliothek $1 nicht gefunden, Linken kann fehlschlagen!
% One of the library file is missing, and linking will probably fail.
% Check your paths.
exec_e_error_while_linking=09013_E_Fehler beim Linken
% Generic error while linking.
exec_e_cant_call_linker=09014_E_Linker kann nicht aufgerufen werden, schalte um zu externem Linken
% An error occurred when calling an external linker, The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_linking=09015_I_Linke $1
% An informational message, showing which program or library is being linked.
exec_e_util_not_found=09016_E_Hilfsprogramm $1 nicht gefunden, schalte um zu externem Linken
% An external tool was not found, the compiler will produce a script that
% can be used to assemble and link or postprocess the program.
exec_t_using_util=09017_T_Benutze Hilfsprogramm $1
% An informational message, showing which external program (usually a postprocessor) is being used.
exec_e_exe_not_supported=09018_E_Erzeugen von ausfÅhrbaren Dateien nicht unterstÅtzt
% Creating executable programs is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_e_dll_not_supported=09019_E_Dynamische Bibliotheken nicht unterstÅtzt
% Creating dynamically loadable libraries is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_i_closing_script=09020_I_Schliesse Skript $1
% Informational message showing when the external assembling an linking script is finished.
exec_e_res_not_found=09021_E_Resourcen Compiler nicht gefunden, schalte um zu externem Modus
% An external resource compiler was not found, the compiler will produce a script that
% can be used to assemble, compile resources and link or postprocess the program.
exec_i_compilingresource=09022_I_öbersetze Resource $1
% An informational message, showing which resource is being compiled.
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_Unit $1 kann nicht statisch gelinkt werden, schalte um zu smart Linken
% Statical linking was requested, but a unit which is not statically linkable was used.
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_Unit $1 kann nicht smart gelinkt werden, schalte um zu statischem Linken
% Smart linking was requested, but a unit which is not smart-linkable was used.
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_Unit $1 kann nicht shared gelinkt werden, schalte um zu statischem Linken
% Shared linking was requested, but a unit which is not shared-linkable  was used.
exec_e_unit_not_smart_or_static_linkable=09026_E_Unit $1 kann weder smart noch statisch gelinkt werden
% Smart or static linking was requested, but a unit which cannot be used for either  was used.
exec_e_unit_not_shared_or_static_linkable=09027_E_Unit $1 kann weder shared noch statisch gelinkt werden
% Shared or static linking was requested, but a unit which cannot be used for either  was used.
exec_d_resbin_params=09028_D_Resource Compiler "$1" wird mit "$2" als Kommandozeile aufgerufen
% An informational message showing which command-line is used for the resource compiler.
%\end{description}
# EndOfTeX

#
# Executable information
#
# 09134 is the last used one
#
# BeginOfTeX
% \section{Executable information messages.}
% This section lists all messages that the compiler emits when an executable program is produced,
% and only when the internal linker is used.
% \begin{description}
execinfo_f_cant_process_executable=09128_F_Kann ausfÅhrbare Datei nicht nachbearbeiten: $1
% Fatal error when the compiler is unable to post-process an executable.
execinfo_f_cant_open_executable=09129_F_Kann ausfÅhrbare Datei nicht îffnen: $1
% Fatal error when the compiler cannot open the file for the executable.
execinfo_x_codesize=09130_X_Grîsse des Codes: $1 Bytes
% Informational message showing the size of the produced code section.
execinfo_x_initdatasize=09131_X_Grîsse der initialisierten Daten: $1 Bytes
% Informational message showing the size of the initialized data section.
execinfo_x_uninitdatasize=09132_X_Grîsse der nicht initialisierten Daten: $1 Bytes
% Informational message showing the size of the uninitialized data section.
execinfo_x_stackreserve=09133_X_Stack Bereich "reserved": $1 Bytes
% Informational message showing the stack size that the compiler reserved for the executable.
execinfo_x_stackcommit=09134_X_Stack Bereich "committed": $1 Bytes
% Informational message showing the stack size that the compiler committed for the executable.
%\end{description}
# EndOfTeX

#
# Unit loading
#
# 10059 is the last used one
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=10000_T_Suche Unit: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_Lade PPU: $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_PPU-Name: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_PPU-Flags: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_PPU-CRC: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_PPU-Zeit: $1
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=10006_U_PPU-Datei zu kurz
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_invalid_header=10007_U_PPU UngÅltiger Header (kein PPU am Anfang)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_PPU UngÅltige Version $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_PPU ist fÅr einen anderen Prozessor Åbersetzt
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=10010_U_PPU ist fÅr ein anderes Zielsystem Åbersetzt
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=10011_U_PPU Quelle: $1
% When you use the \var{-vu} flag, the unit source file name is shown.
unit_u_ppu_write=10012_U_Schreibe $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_Kann PPU-Datei nicht schreiben
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_Kann PPU-Datei nicht lesen
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=10015_F_Unerwartetes Ende der PPU-Datei
% Unexpected end of file. This may mean that the PPU file is
% corrupted.
unit_f_ppu_invalid_entry=10016_F_UngÅltiger Eintrag in PPU-Datei: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_PPU Dbx ZÑhler-Problem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_UngÅltiger Unitname: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=10019_F_Zu viele Units
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Gegenseitige AbhÑngigkeit von Units zwischen $1 und $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_Kann Unit "$1" nicht Åbersetzen, keine Quellen vorhanden
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_Kann Unit "$1", die von "$2" benutzt wird, nicht finden
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_unit_name_error=10023_W_Unit "$1" wurde nicht gefunden, aber "$2" existiert
% This error message is no longer used.
unit_f_unit_name_error=10024_F_Unit "$1" gesucht, aber "$2" gefunden
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_öbersetzen der Systemunit erfordert den Schalter -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_Es traten $1 Fehler beim öbersetzen des Moduls auf, halte an
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Lade aus $1 ($2) die Unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_öbersetze $1 erneut, PrÅfsumme fÅr $2 hat sich geÑndert
% The unit is recompiled because the checksum of a unit it depends on has
% changed.
unit_u_recompile_source_found_alone=10029_U_öbersetze "$1", nur Quellcode gefunden
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_öbersetze Unit erneut, statische Biblothek ist Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_öbersetze Unit erneut, gemeinsame Bibliothek ist Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_öbersetze Unit erneut, obj und asm sind Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the assembler of
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_öbersetze Unit erneut, obj ist Ñlter als asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_Analysiere Interface von $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_parsing_implementation=10035_U_Analysiere Implementation von $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_Zweites Laden fÅr Unit "$1"
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_PPU prÅfe Datei $1 Zeit $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
### The following two error msgs is currently disabled.
#unit_h_cond_not_set_in_last_compile=10038_H_Conditional $1 wurde beim Start beim letzten öbersetzen von $2 nicht gesetzt
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that currently is defined, but was not used the last
#% time the unit was compiled.
#unit_h_cond_set_in_last_compile=10039_H_Conditional $1 wurde beim Start beim letzten öbersetzen von $2 gesetzt
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that was used the last time the unit was compiled, but
#% the conditional is currently not defined.
unit_w_cant_compile_unit_with_changed_incfile=10040_W_Kann Unit $1 nicht erneut Åbersetzen, aber geÑnderte Include-Datei gefunden
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_u_source_modified=10041_U_Datei $1 ist neuer als die PPU Datei $2
% A modified source file for a compiler unit was found.
unit_u_ppu_invalid_fpumode=10042_U_Versuch eine Unit zu verwenden, die in einem anderen FPU Mode Åbersetzt wurde
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_Interface Units werden von $1 geladen
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the interface part of the unit.
unit_u_loading_implementation_units=10044_U_Implementation Units werden von $1 geladen
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the implementation part of the unit.
unit_u_interface_crc_changed=10045_U_GeÑnderte Interface CRC fÅr Unit $1
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated for the interface has been changed after the implementation
% has been parsed.
unit_u_implementation_crc_changed=10046_U_GeÑnderte Implementation CRC fÅr Unit $1
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated has been changed after the implementation
% has been parsed.
unit_u_finished_compiling=10047_U_öbersetzen der Unit $1 beendet
% When you use the \var{-vu} flag, the compiler warns that it
% has finished compiling the unit.
unit_u_add_depend_to=10048_U_FÅge die AbhÑngigkeit von $1 von $2 dazu
% When you use the \var{-vu} flag, the compiler warns that it
% has added a dependency between the two units.
unit_u_no_reload_is_caller=10049_U_Kein erneutes Laden, Unit $1 ist die Aufrufende
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is the unit that wants
% to load this unit
unit_u_no_reload_in_second_compile=10050_U_Kein erneutes Laden der Unit, bereits beim zweiten öbersetzen: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is already in a second recompile
unit_u_flag_for_reload=10051_U_Flag fÅr erneutes Laden: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to reload the unit
unit_u_forced_reload=10052_U_Erzwungenes erneutes Laden
% When you use the \var{-vu} flag, the compiler warns that it
% has is reloading the unit because it was required
unit_u_previous_state=10053_U_Vorhergehender Status von $1: $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit
unit_u_second_compile_unit=10054_U_$1 wird bereits Åbersetzt, zweites öbersetzen gesetzt
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_loading_unit=10055_U_Unit $1 wird geladen
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the unit.
unit_u_finished_loading_unit=10056_U_Laden der Unit $1 beendet
% When you use the \var{-vu} flag, the compiler warns that it finished
% loading the unit.
unit_u_registering_new_unit=10057_U_Registrierung der neuen Unit $1
% When you use the \var{-vu} flag, the compiler warns that it has
% found a new unit and registers it in the internal lists.
unit_u_reresolving_unit=10058_U_Erneutes resolving der Unit $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to recalculate the internal data of the unit
unit_u_skipping_reresolving_unit=10059_U_Erneutes Resolving der Unit $1 wird Åbersprungen, benutzte Units werden noch geladen
% When you use the \var{-vu} flag, the compiler warns that it
% skips to recalculate the internal data of the unit because there
% is no data to recalculate
% \end{description}
# EndOfTeX

#
#  Options
#
# 11041 is the last used one
#
option_usage=11000_O_$1 [Optionen] <Eingabedatei> [Optionen]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Nur eine Quelldatei unterstÅtzt
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_DEF-Datei kann nur fÅr OS/2 erzeugt werden
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_Verschachtelte Response-Dateien werden nicht unterstÅtzt
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_Kein Name fÅr Quelldatei auf der Kommandzeile
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_Keine Angaben in Konfigurationsdatei "$1" gefunden
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_UngÅltiger Parameter: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? zeigt Hilfetext an
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Zu viele verschachtelte Konfigurationsdateien
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Kann Datei nicht îffnen $1
% The option file cannot be found.
option_reading_further_from=11010_D_Lese weitere Optionen aus $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Zielsystem ist bereits gesetzt: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Gemeinsame Bibliotheken sind auf der DOS Platform nicht verfÅgbar, verwende stattdessen statische Bibliotheken
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_Zu viele IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_Zu viele ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_Offene Bedingung am Dateiende
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_Erzeugung von Debug-Informationen wird von dieser ausfÅhrbaren Datei nicht unterstÅtzt
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Versuchen Sie mit -dGDB erneut zu Åbersetzen
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_W_Sie verwenden den nun Åberholten Schalter $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_W_Sie benutzen den nun Åberholten Schalter $1, bitte benutzen Sie $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_Schalte Assembler auf den Standard-Assembler-Quellcodeschreiber
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_Das gewÑhlte Assembler-Ausgabeformat "$1" ist nicht mit "$2" kompatibel
option_asm_forced=11022_W_Verwendung des Assemblers "$1" erzwungen
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_Optionen werden aus der Datei $1 gelesen
% Options are also read from this file
option_using_env=11027_T_Optionen werden aus dem environment $1 gelesen
% Options are also read from this environment string
option_handling_option=11028_D_Handling der Option "$1"
% Debug info that an option is found and will be handled
option_help_press_enter=11029__*** DrÅcken Sie die ENTER-Taste ***
option_start_reading_configfile=11030_H_Beginn des Lesens der Konfigurationsdatei $1
% Starting of config file parsing.
option_end_reading_configfile=11031_H_Ende des Lesens der Konfigurationsdatei $1
% End of config file parsing.
option_interpreting_option=11032_D_Option "$1" interpretieren
option_interpreting_firstpass_option=11036_D_firstpass Option "$1" interpretieren
option_interpreting_file_option=11033_D_Datei Option "$1" interpretieren
option_read_config_file=11034_D_Konfigurationsdatei "$1" lesen
option_found_file=11035_D_Name der Quelldatei "$1" gefunden
% Additional infos about options, displayed
% when you have debug option turned on.
option_code_page_not_available=11039_E_Unbekannte code page
option_config_is_dir=11040_F_Konfigurationsdatei $1 ist ein Verzeichnis
% Directories can not be used as configuration files.
option_confict_asm_debug=11041_W_Die gewÑhlte Assembler-Ausgabe "$1" kann kein Debug-Info erzeugen, Debugging ist ausgeschaltet
% The assembler output selected can not generate
% debugging information, debugging option is therefore disabled.
%\end{description}
# EndOfTeX

#
# Logo (option -l)
#
option_logo=11023_[
Free Pascal Compiler Version $FPCFULLVERSION [$FPCDATE] fÅr $FPCTARGET
Copyright (c) 1993-2007 Florian KlÑmpfl
]

#
# Info (option -i)
#
option_info=11024_[
Free Pascal Compiler Version $FPCVERSION

Compiler Datum:      $FPCDATE
Compiler Zielsystem: $FPCCPU

UnterstÅtzte Zielbetriebssysteme:
  $OSTARGETS

UnterstÅtzte CPU Instruktionen:
  $INSTRUCTIONSETS

UnterstÅtzte FPU Instruktionen:
  $FPUINSTRUCTIONSETS

UnterstÅtzte Optimierungen:
  $OPTIMIZATIONS

Dieses Programm unterliegt der GNU General Public Licence
Weitere Informationen sind in COPYING.FPC zu finden

Fehlerberichte, VorschlÑge usw. bitte senden an:
             bugs@freepascal.org
]

#
# Help pages (option -? and -h)
#
# The first character on the line indicates who will display this
# line, the current possibilities are :
#    * = every target
#    3 = 80x86 targets
#    6 = 680x0 targets
#    e = in extended debug mode only
#    P = PowerPC targets
#    S = Sparc targets
#    V = Virtual machine targets
# The second character also indicates who will display this line,
# (if the above character was TRUE) the current possibilities are :
#    * = everyone
#    g = with GDB info supported by the compiler
#    O = OS/2
#    L = UNIX systems
#  The third character represents the indentation level.
#
option_help_pages=11025_[
**0*_Nach booleschen Optionen geben Sie + zum Ein- bzw. - zum Ausschalten an
**1a_Lîsche die erzeugte Assembler-Datei nicht
**2al_Liste Quellcode-Zeilen in der Assembler-Datei
**2an_Liste "node info" in der Assembler-Datei
*L2ap_Benutze Pipes anstelle temporÑrer Assembler-Dateien
**2ar_Liste Registerbelegungsinformation in Assembler-Datei
**2at_Liste Temp. Variablenbelegungsinfo in Assembler-Datei
**1A<x>_Ausgabe Format:
**2Adefault_Benutze den "default" Assembler
3*2Aas_Assembliere mit Hilfe von GNU AS
3*2Anasmcoff_COFF (Go32v2) Datei mit Hilfe von Nasm
3*2Anasmelf_ELF32 (Linux) Datei mit Hilfe von Nasm
3*2Anasmwin32_Win32 Object Datei mit Hilfe von Nasm
3*2Anasmwdosx_Win32/WDOSX Object Datei mit Hilfe von Nasm
3*2Awasm_obj Datei mit Hilfe von Wasm (Watcom)
3*2Anasmobj_Obj Datei mit Hilfe von Nasm
3*2Amasm_Obj Datei mit Hilfe von Masm (Microsoft)
3*2Atasm_Obj Datei mit Hilfe von Tasm (Borland)
3*2Aelf_ELF32 (Linux) mit Hilfe von internen Schreiber
3*2Acoff_COFF (Go32v2) mit Hilfe von internen Schreiber
3*2Apecoff_PE_COFF (Win32) mit Hilfe von internen Schreiber
4*2Aas_Assembliere mit Hilfe von GNU AS
6*2Aas_Unix o-file mit Hilfe von GNU AS
6*2Agas_GNU Motorola Assembler
6*2Amit_MIT Syntax (old GAS)
6*2Amot_Standard Motorola Assembler
A*2Aas_Assembliere mit Hilfe von GNU AS
P*2Aas_Assembliere mit Hilfe von GNU AS
S*2Aas_Assembliere mit Hilfe von GNU AS
**1b_Erzeuge Browser-Info
**2bl_Erzeuge Info zu lokalen Symbolen
**1B_Erzeuge alle Module (Build)
**1C<x>_Optionen fÅr Code-Erzeugung:
**2Cc<x>_Setze "default calling convention" zu <x>
**2CD_Erzeuge auch eine dynamische Bibliothek (nicht unterstÅtzt)
**2Ce_öbersetze mit emulierten Fliesskomma opcodes
**2Cf<x>_WÑhle den Fliesskomma instruction set aus, siehe fpc -i wegen mîglicher Werte
**2CF<x>_Minimale PrÑzission von Fliesskommakonstanten (default, 32, 64)
**2Cg_Erzeuge PIC code
**2Ch<n>_<n> Bytes Heap (zwischen 1023 und 67107840)
**2Ci_I/O-PrÅfung
**2Cn_Lasse die Linkstufe aus
**2Co_PrÅfe auf öberlauf von Integer-Operationen
**2CO_PrÅfe auf mîglichen öberlauf von Integer-Operationen
**2Cp<x>_WÑhle instruction set aus, siehe fpc -i wegen mîglicher Werte
**2CP<x>=<y>_ Einstellungen fÅr packing
**3CPPACKSET=<y>_ <y> Belegung von Sets: 0, 1 oder DEFAULT oder NORMAL, 2, 4 und 8
**2Cr_FÅhre BereichsprÅfung durch
**2CR_Verifiziere die GÅltigkiet des Aufrufs der Objektmethoden
**2Cs<n>_Setze Grîsse des Stacks auf <n>
**2Ct_FÅhre StackprÅfung durch
**2Cx_Benutze Smartlinking
**1d<x>_Definiere das Symbol <x>
**1D_Erzeuge eine DEF-Datei
**2Dd<x>_Setze Beschreibung zu <x>
**2Dv<x>_Setze DLL Version zu <x>
*O2Dw_Erzeuge PM-Anwendung
**1e<x>_Setze Pfad zur ausfÅhrbaren Datei
**1E_Genau wie -Cn
**1fPIC_Genau wie -Cg
**1F<x>_Dateinamen und Pfade:
**2Fa<x>[,y]_Ein Program soll die Units <x> und [y] laden, bevor uses analysiert wird
**2Fc<x>_Setze die Eingabe-Codepage zu <x>
**2FC<x>_Setze den Namen des RC Compiler-BinÑrprograms auf <x>
**2FD<x>_Setze das Verzeichnis fÅr die Compiler-Hilfsprogramme
**2Fe<x>_Leite die Fehlerausgabe um nach <x>
**2Ff<x>_ErgÑnze <x> zum Framework-Pfad (nur Darwin)
**2FE<x>_Setze den Pfad fÅr Exe/Unit-Dateien auf <x>
**2Fi<x>_ErgÑnze <x> zum Include-Pfad
**2Fl<x>_ErgÑnze <x> zum Bibliotheks-Pfad
**2FL<x>_Benutze <x> als dynamischen Linker
**2Fm<x>_Lade die Unicode-Konversionstabelle aus <x>.txt im Compiler-Verzeichnis
**2Fo<x>_ErgÑnze <x> zum Objektdatei-Pfad
**2Fr<x>_Lade die Fehler-Meldungs Datei <x>
**2FR<x>_Setze den Resource (.res) Linker auf <x>
**2Fu<x>_ErgÑnze <x> zum Unit-Pfad
**2FU<x>_Units werden nach <x> ausgegeben, hat Vorrang vor -FE
*g1g<x>_Erzeuge Informationen zur Fehlersuche:
*g2gc_ZeigerÅberprÅfung
*g2gh_Heaptrace-Unit einbinden
*g2gl_Line info Unit einbinden, um mehr backtrace Informationen anzuzeigen
*g2go<x>_Setze Optionen fÅr die Debug Informationen
*g3godwarfsets_Schalte Dwarf Debug Informationen fÅr Mengen (sets) ein (verhindert debugging mit gdb < 6.5)
*g2gp_Erhalte Gross/Kleinschreibung in Stabs-Symbolnamen
*g2gt_Lîsche lokale Variablen (um eine Verwendung ohne Initialisierung zu finden)
*g2gv_Erzeuge ein mit valgrind verfolgbares (traceable) Programm
*g2gw_Erzeuge dwarf Informationen
**1i_Zeige alle Information Åber den Compiler
**2iD_Zeige Compilerdatum
**2iV_Zeige Compilerversion
**2iW_Zeige vollstÑndige Compilerversion
**2iSO_Zeige Compilerbetriebssystem
**2iSP_Zeige Compilerprozessor
**2iTO_Zeige Zielbetriebssystem
**2iTP_Zeige Zielprozessor
**1I<x>_ErgÑnze <x> zum Include-Pfad
**1k<x>_öbergebe <x> an den Linker
**1l_Zeige Logo
**1M<x>_Setze Sprachmodus zu <x>
**2Mfpc_Free Pascal Dialekt (Voreinstellung)
**2Mobjfpc_Schalte einige Delphi 2 Erweiterungen ein
**2Mdelphi_Versuche zu Delphi kompatibel zu sein
**2Mtp_Versuche zu TP/BP 7.0 kompatibel zu sein
**2Mmacpas_Versuche zu den MacIntosh Pascal-Dialekten kompatibel zu sein
**1n_Standard-Konfigurationsdatei ignorieren
**1N<x>_Node tree Optimierung
**2Nu_Unroll loops
**1o<x>_Die erzeugte, ausfÅhrbare Datei bekommt den Namen <x>
**1O<x>_Optimierungen:
**2O-_Optimierungen ausschalten
**2O1_Level 1 Optimierung (schnell und Debugger freundlich)
**2O2_Level 2 Optimierung (-O1 + schnelle Optimierungen)
**2O3_Level 3 Optimierung (-O2 + langsame Optimierungen)
**2Oa<x>=<y>_Ausrichtung (alignment) von Mengen
**2Oo[NO]<x>_Optimierungen ein- oder ausschalten, siehe fpc -i fÅr mîgliche Werte
**2Op<x>_Setze Zielprozessor fÅr die Optimierung, siehe fpc -i fÅr mîgliche Werte
**2Os_Erzeuge kÅrzeren Code
**1pg_Erzeuge Profiler-Code fÅr gprof
**1R<x>_Assembler Code Format:
**2Rdefault_Benutze den default Assembler
3*2Ratt_Lese Assembler Code im AT&T Format
3*2Rintel_Lese Assembler im Intel Format
6*2RMOT_Lese Assembler im Motorola Format
**1S<x>_Syntax-Optionen:
**2S2_Schalte einige der Delphi 2 Erweiterungen ein (wie -Mobjfpc)
**2Sc_UnterstÅtze spezielle C Operatoren (*=,+=,/= and -=)
**2Sa_Erlaube assertion code.
**2Sd_Sei Delphi-kompatibel (wie -Mdelphi)
**2Se<x>_Fehler Optionen. <x> ist eine der folgenden Kombinationen:
**3*_<n> : Compiler hÑlt nach <n> Fehlern (Voreinstellung ist 1)
**3*_w : Compiler hÑlt auch nach Warnungen
**3*_n : Compiler hÑlt auch nach Anmerkungen
**3*_h : Compiler hÑlt auch nach Hinweisen
**2Sg_Erlaube LABEL und GOTO
**2Sh_Benutze ANSI-Strings
**2Si_Benutze C++ artige INLINE
**2Sk_Lade fpcylix Unit
**2SI<x>_Setze den Stil des Interface zu <x>
**3SIcom_COM kompatibles Interface (Voreinstellung)
**3SIcorba_CORBA kompatibles Interface
**2Sm_UnterstÅtze Makros wie in C (global)
**2So_Sei TP/BP 7.0 kompatibel (wie -Mtp)
**2Sp_Sei gpc-kompatibel (wie -Mgpc)
**2Ss_Konstruktor- und Destruktorname mÅssen "Init" und "Done" sein
**2St_Erlaube SchlÅsselwort static in Objekten
**2Sx_Exception SchlÅsselwîrter einschalten (Voreinstellung in Delphi/ObjFPC Moden)
**1s_Rufe weder Assembler noch Linker auf (nur mit -a)
**2sh_Erzeuge Script um auf dem Host zu linken
**2st_Erzeuge Script um auf dem Zielsystem zu linken
**2sr_öberspringe die Phase der "register allocation" (mit -alr benutzen)
**1T<x>_Zielbetriebssystem::
3*2Temx_OS/2 via EMX (einschliesslich EMX/RSX extender)
3*2Tfreebsd_FreeBSD
3*2Tgo32v2_Version 2 of DJ Delorie DOS extender
3*2Tlinux_Linux
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware Module (clib)
3*2Tnetwlibc_Novell Netware Module (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 / eComStation
3*2Tsunos_SunOS/Solaris
3*2Tsymbian_Symbian OS
3*2Twatcom_Watcom compatible DOS extender
3*2Twdosx_WDOSX DOS extender
3*2Twin32_Windows 32 Bit
3*2Twince_Windows CE
4*2Tlinux_Linux
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tlinux_Linux-68k
6*2Tmacos_Macintosh m68k (nicht unterstÅtzt)
6*2Tpalmos_PalmOS
A*2Tlinux_Linux
A*2Twince_Windows CE
P*2Tamiga_AmigaOS on PowerPC
P*2Tdarwin_Darwin und Mac OS X on PowerPC
P*2Tlinux_Linux auf PowerPC
P*2Tmacos_Mac OS (classic) auf PowerPC
P*2Tmorphos_MorphOS
S*2Tlinux_Linux
**1u<x>_Entferne die Definition fÅr das Symbol <x>
**1U<x>_Unit-Optionen:
**2Un_PrÅfe den Unitnamen nicht
**2Ur_Erzeuge release unit Dateien
**2Us_Erzeuge eine Systemunit
**1v<x>_Meldungen, <x> ist eine Kombination der folgenden Zeichen:
**2*_e : Fehler (Standard)             0 : Nichts (ausser Fehlern)
**2*_w : Warnungen                     u : Unit Informationen
**2*_n : Anmerkungen                   t : Angesprochene/benutzte Dateien
**2*_h : Hinweise                      c : Preprozessordirektive
**2*_i : Allgemeine Informationen      d : Debug Informationen
**2*_l : Zeilennummern                 r : Rhide/GCC kompatibler Modus
**2*_a : Alles                         x : Exe-Datei Informationen (nur Win32)
**2*_b : Schreibe bei Meldungen mit Dateinamen den vollstÑndigem Pfad
**2*_v : Schreibe fpcdebug.txt mit     p : Schreibe tree.log mit Analysenbaum (parse tree)
**2*_   ganz viel Information
3*1W<x>_Win32-Ñhnliche Ziel-Optionen
3*2Wb_Erzeuge statt einer Bibliothek ein Bundle (Darwin)
P*2Wb_Erzeuge statt einer Bibliothek ein Bundle (Darwin)
p*2Wb_Erzeuge statt einer Bibliothek ein Bundle (Darwin)
3*2WB_Erzeuge ein relozierbares Image
3*2WB<x>_Setze die Image base auf den Hexadezimal Wert <x>
3*2WC_Spezifiziere "console type application"
3*2WD_Benutze DEFFILE um Funktionen der DLL oder EXE zu exportieren
3*2WF_Spezifiziere "full-screen type application" (nur OS/2)
3*2WG_Spezifiziere "graphic type application"
3*2WN_Erzeuge keinen "relocation code" (notwendig fÅr debugging)
3*2WR_Erzeuge "relocation code"
P*2WC_Spezifiziere "console type application" (nur Mac OS)
P*2WG_Spezifiziere "graphic type application" (nur Mac OS)
P*2WT_Spezifiziere "tool type application"    (MPW tool, nur Mac OS)
**1X_Programm-Optionen:
**2Xc_öbergebe --shared an den Linker (nur Unix)
**2Xd_Den Standard Bibliotheks-Suchpfad NICHT nutzen (benîtigt fÅr cross compile)
**2Xe_Verwende den externen Linker
**2Xg_Erstelle die Debug-Informationen in einer separaten Datei und erstelle eine Debug-Link Sektion im ausfÅhrbaren Programm
**2XD_Versuche Units dynamisch zu linken             (definiert FPC_LINK_DYNAMIC)
**2Xi_Verwende den internen Linker
**2Xm_Erzeuge die "link map"
**2XM<x>_Setze den Namen der 'main' program Routine   (default ist 'main')
**2XP<x>_Stelle den Namen der Compiler-Hilfsprogrammen den Prefix <x> voran
**2Xr<x>_Setze den Bibliotheks-Suchpfad zu <x>       (benîtigt fÅr cross compile)
**2XR<x>_Stelle allen Linker-Suchpfaden den Namen <x> voran (BeOS, Darwin, FreeBSD, Linux, Mac OS, Solaris)
**2Xs_Entferne alle Symbole von ausfÅhrbarer Datei
**2XS_Versuche Units statisch zu linken (default)    (definiert FPC_LINK_STATIC)
**2Xt_Linke mit statischen Bibliotheken              (-static wird an den Linker Åbergeben)
**2XX_Versuche Units smart zu linken                 (definiert FPC_LINK_SMART)
**1*_
**1?_Zeigt diese Hilfe an
**1h_Zeigt diese Hilfe ohne Warten an
]

#
# The End...
#
