#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998-2000 by the Free Pascal Development team
#
#   German (alternative) Language File for Free Pascal
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
#   l_   linenumber
#   u_   used
#   t_   tried
#   m_   macro
#   p_   procedure
#   c_   conditional
#   d_   debug message
#   b_   display overloaded procedures
#   x_   executable informations
#

#
# General
#
# BeginOfTeX
% \section{General compiler messages}
% This section gives the compiler messages which are not fatal, but which
% display useful information. The number of such messages can be
% controlled with the various verbosity level \var{-v} switches.
% \begin{description}
general_t_compilername=T_Compiler: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=D_Hostbetriebssystem: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=I_Zielbetriebssystem: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=T_Der Pfad der ausfÅhrbaren Datei ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=T_Der Unitpfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=T_Der Includepfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=T_Der Bibliothekspfad ist: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=T_Der Objektdateienpfad: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=I_$1 Zeilen kompiliert, $2 Sekunden
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=F_Speicher voll
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
general_i_writingresourcefile=I_Resourcestringtabellendatei $1 wird geschrieben
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=E_Fehler beim Schreiben der Resourcestringtabellendatei: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
% \end{description}
#
# Scanner
#
% \section{Scanner messages.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=F_Unerwartetes Dateiende
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=F_Zeichenkette geht Åber Zeilenende hinaus
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=F_UnzulÑssiges Zeichen
% An illegal character was encountered in the input file.
scan_f_syn_expected=F_Syntaxfehler, $1 erwartet aber $2 vorgefunden
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=T_Die Include-Datei $1 wird jetzt gelesen
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=W_Kommentarschachtelungstiefe $1 gefunden
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_far_directive_ignored=N_$F Direktive (FAR) ignoriert
% The \var{FAR} directive is a 16-bit construction which is recorgnised
% but ignored by the compiler, since it produces 32 bit code.
scan_n_stack_check_global_under_linux=N_StackprÅfung ist unter Linux global
% Stack checking with the \var{-Cs} switch is ignored under \linux, since
% \linux does this for you. Only displayed when \var{-vn} is used.
scan_n_ignored_switch=N_Compilerschalter $1 wurde ignoriert
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=W_Compilerschalter $1 ist ungÅltig
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=W_Dieser Compilerschalter hat globale Auswirkung
% When \var{-vw} is used, the compiler warns if a switch is global.
scan_e_illegal_char_const=E_UngÅltige Char-Konstantante
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range. The range
% is 1-255.
scan_f_cannot_open_input=F_Datei $1 kann nicht geîffnet werden
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=F_Includedatei $1 kann nicht geîffnet werden
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_too_much_endifs=E_Zu viele $ENDIF oder $ELSE Direktiven
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_w_only_pack_records=W_Record-Elemente kînnen nur im Raster 1,2,4 oder 16 Bytes ausgerichtet werden
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=W_FÅr AufzÑhlungen kînnen nur die Grî·en 1,2 or 4 Bytes angegeben werden
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=E_$1 erwartet fÅr $2 definiert in Zeile $3
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=E_Syntaxfehler im Argument einer $if Direktive
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_e_error_in_preproc_expr=E_Syntaxfehler im Kontext einer $if Direktive
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_w_marco_cut_after_255_chars=W_Inhalt des Makros wurde nach der Auswertung bei 255 Zeichen abgeschnitten
% The contents of macros canno be longer than 255 characters. This is a
% safety in the compiler, to prevent buffer overflows. This is shown as a
% warning, i.e. when the \var{-vw} switch is used.
scan_e_endif_without_if=E_ENDIF ohne IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=F_Benutzerdefiniert: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=E_Benutzerdefiniert: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=W_Benutzerdefiniert: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=N_Benutzerdefiniert: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=H_Benutzerdefiniert: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=I_Benutzerdefiniert: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=E_Ein Makro, welches den selben Namen wie ein SchlÅsselwort hat, wird ignoriert
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=F_MakropufferÅberlauf wÑhrend des Lesens oder Expandierens eines Makros
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=W_Expansion des Makros Åberschreitet eine Makroschachtelungstiefe von 16
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_wrong_styled_switch=E_Compilerschalter in (* ... *)-Kommentaren sind nicht erlaubt
% Compiler switches should always be between \var{\{ \}} comment delimiters.
scan_d_handling_switch=D_Bearbeite Schalter "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=C_ENDIF $1 bearbeitet
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=C_IFDEF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=C_IFOPT $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=C_IF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=C_IFNDEF $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=C_ELSE $1 bearbeitet, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=C_Quelltext wird ignoriert bis...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=I_DrÅcken Sie <Return> um fortzusetzen
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=W_Nicht unterstÅtzter Schalter $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=W_UngÅltiges Compilerdirektive $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=T_Wieder zurÅck in $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=W_Nicht unterstÅtzter Anwendungstyp: $1
% You get this warning, ff you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=W_APPTYPE wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$APPTYPE\}} directive is supported by win32 applications only.
scan_w_decription_not_support=W_DESCRIPTION kann nur bei OS/2- und Windows-Programmen verwendet werden
% The \var{\{\$DESCRIPTION\}} directive is only supported for OS2 and Win32 targets.
scan_n_version_not_support=N_VERSION wird vom Zielbetriebssystem nicht unterstÅtzt
% The \var{\{\$VERSION\}} directive is only supported by win32 target.
scan_n_only_exe_version=N_VERSION kann in Units nicht verwendet werden
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=W_Falsches Format fÅr VERSION-Directive $1
% The \var{\{\$VERSION\}} directive format is major_version.minor_version
% where major_version and minor_version are words.
scan_w_unsupported_asmmode_specifier=W_Nicht unterstÅtzter Assemblermodus $1 angegeben
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=W_ASM-Leser-Schalter ist innerhalb einer Asm-Sequenz nicht mîglich, $1 wird erst in der nÑchsten Sequenz wirksam
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=E_Parameter fÅr Schalter falsch, verwenden Sie ON/OFF oder +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=E_Resourcedateien werden vom aktuellen Zielbetriebssystem nicht unterstÅtzt
% The target you are compiling for doesn't support Resource files. The
% only target which can use resource files is Win32
scan_w_include_env_not_found=W_$1 ist keine Umgebungsvariable
% The included environment variable can't be found in the environment, it'll
% be replaced by an empty string instead.
% \end{description}
#
# Parser
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
scan_e_invalid_maxfpureg_value=E_Nicht erlaubter Wert fÅr MAXFPUREGISTER-Direktive
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
% \end{description}
#
# Parser
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=E_Parser - Syntaxfehler
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_w_proc_far_ignored=W_SchlÅsselwort FAR wurde ignoriert
% This is a warning. \var{FAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_near_ignored=W_SchlÅsselwort NEAR wurde ignoriert
% This is a warning. \var{NEAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_interrupt_ignored=W_Ignoriere Prozedurtyp INTERRUPT
% This is a warning. \var{INTERRUPT} is a i386 specific construct
% and is igonred for other processors.
parser_e_dont_nest_interrupt=E_INTERRUPT-Prozeduren dÅrfen nicht verschachtelt sein
% An \VAR{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=W_Prozedurtyp $1 wird ignoriert
% This is a warning. \var{REGISTER},\var{REINTRODUCE} is ignored by FPC programs for now.
% This is introduced first for Delphi compatibility.
parser_e_no_overload_for_all_procs=E_Nicht alle Deklarationen von "$1" sind mit OVERLOAD deklariert
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_no_dll_file_specified=E_Keine DLL-Datei angegeben
% No longer in use.
parser_e_export_name_double=E_Doppelter Name fÅr exportierte Funktion $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=E_Doppelter Index fÅr exportierte Funktion $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=E_UngÅltiger Index for exportierte Funktion
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=W_FÅr relozierbare DLL oder ausfÅhrbare Datei $1 funktionieren keine Debug-Information, deaktiviert.
parser_w_parser_win32_debug_needs_WN=W_Um Win32-Code debuggen zu kînnen mÅssen die Relozierungen mit -WN option abgeschaltet werden.
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=E_Konstruktor-Name muss INIT sein
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=E_Destruktor-Name muss DONE sein
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_illegal_open_parameter=E_UngÅltiger Open-Parameter
% You are trying to use the wrong type for an open parameter.
parser_e_proc_inline_not_supported=E_SchlÅsselwort INLINE nicht unterstÅtzt
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_priv_meth_not_virtual=W_Private Methoden dÅrfen nicht VIRTUAL sein
% You declared a method in the private part of a object (class) as
% \var{virtual}. This is not allowed. Private methods cannot be overridden
% anyway.
parser_w_constructor_should_be_public=W_Konstruktor muss PUBLIC sein
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=W_Destruktor muss PUBLIC sein
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=N_Klasse darf nur einen Destructor besitzen
% You can declare only one destructor for a class.
parser_e_no_local_objects=E_Lokale Klassendefinitionen sind nicht zulÑssig
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=F_Anonyme Klassendefinitionen sind nicht zulÑssig
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_object_has_no_vmt=E_Das Objekt $1 hat keine VMT
parser_e_illegal_parameter_list=E_UngÅltige Parameterliste
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_type=E_Falscher Parametertyp angegeben fÅr Argument Nr. $1
% There is an error in the parameter list of the function or procedure.
% The compiler cannot determine the error more accurate than this.
parser_e_wrong_parameter_size=E_Falsche Parameterzahl angegeben
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=E_Bezeichner $1 ist keine Funktion, overload nicht mîglich
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=E_Overload-Funktion darf nicht eine identische Parameterliste aufweisen
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=E_Funktionskopf ist nicht identisch mit Forward-Deklaration $1
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=E_Funktionskopf von $1 passt nicht zur Forward-Deklaration, Variablename Ñndert sich: $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=N_Werte in AufzÑhlungen mÅssen aufsteigend sein
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_n_interface_name_diff_implementation_name=N_Namen in Interface und Implementation sind verschieden!
% This note warns you if the implementation and interface names of a
% functions are different, but they have the same mangled name. This
% is important when using overloaded functions (but should produce no error).
parser_e_no_with_for_variable_in_other_segments=E_WITH kann nicht auf Variablen in anderen Segmenten angewendet werden
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=E_Funktionsverschachtelung grîsser als 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=E_BereichsprÅfungsfehler bei Konstantenbestimmung
% The constants are out of their allowed range.
parser_w_range_check_error=W_BereichsprÅfungsfehler bei Konstantenbestimmung
% The constants are out of their allowed range.
parser_e_double_caselabel=E_doppelter CASE-Wert
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=E_Obere Grenze der CASE-Bereichsangabe ist kleiner als untere Grenze
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=E_Klassen sind als typisierte Konstanten unzulÑssig
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=E_Funktionsvariablen von Overload-Funktionen sind unzulÑssig
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=E_StringlÑnge muss ein Wert zwischen 1 und 255 sein
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
% (This is not true for \var{Longstrings} and \var{AnsiStrings}.
parser_w_use_extended_syntax_for_objects=W_Benutzen Sie die erweiterte Syntax von NEW und DISPOSE fÅr Objekt-Instanzen
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=W_Verwendung von NEW oder DISPOSE mit untypisierten Pointern ist ohne Aussage
parser_e_no_new_dispose_on_void_pointers=E_Verwendung von NEW oder DISPOSE mit untypisierten Pointern ist nicht mîglich
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=E_Klassenbezeichner erwartet
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=E_Typbezeichner ist hier nicht zulÑssig
% You cannot use a type inside an expression.
parser_e_methode_id_expected=E_Methodenbezeichner erwartet
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=E_Funktionskopf passt zu keiner Methode der Klasse
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_p_procedure_start=P_Prozedur/Funktion $1
% When using the \var{-vp} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=E_UngÅltige Fliesskommakonstante
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=E_FAIL darf nur in Konstruktoren verwendet werden
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=E_Destruktoren kînnen keine Parameter haben
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=E_Nur Klassenmethoden kînnen Åber den Klassennamen angesprochen werden
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
parser_e_only_class_methods=E_Nur Klassenmethoden kînnen in einer Klassenmethode angesprochen werden
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
parser_e_case_mismatch=E_Konstante und CASE-Typ passen nicht zueinander
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=E_Das Symbol kann nicht aus einer Bibliothek exportiert werden
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=W_Eine Inherit-Methode wird durch $1 verdeckt
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=E_Es gibt keine VorgÑngerklasse mit einer Methode, die damit Åberschrieben werden kînnte: $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=E_Es gibt keine Member-Funktion um auf diese Property zuzugreifen
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=W_Die Stored Property -  Direktive ist noch nicht implementiert
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=E_UngÅltiges Symbol fÅr den Zugriff auf die Property
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code woud cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=E_Kann an dieser Stelle nicht auf das geschÅtzte Feld des Objekts zugreifen
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module wher the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=E_Kann an dieser Stelle nicht auf das private Feld des Objekts zugreifen
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_w_overloaded_are_not_both_virtual=W_Overload-Methode einer virtuellen Methode muss auch virtuell sein: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_w_overloaded_are_not_both_non_virtual=W_Overload-Methode einer nicht-virtuellen Methode muss auch nicht-virtuell sein: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_e_overloaded_methodes_not_same_ret=E_Overload-Methoden, die virtuell sind, mÅssen den gleichen RÅckgabetyp haben: $1
% If you declare virtual overloaded methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=E_Mit EXPORT deklarierte Funktionen dÅrfen nicht verschachtelt sein
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=E_Methoden dÅrfen nicht EXPORTiert werden
% You cannot declare a procedure that is a method for an object as
% \var{export}ed. That is, your methods cannot be called from a C program.
parser_e_call_by_ref_without_typeconv=E_Aufrufe mit VAR-Parametern mÅssen exakt stimmen
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=E_Klasse ist keine Elternklasse zur aktuellen Klasse
% When calling inherited methods, you are trying to call a method of a strange
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=E_SELF ist nur in Methoden erlaubt
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=E_Methoden dÅrfen nur in anderen Methoden direkt mit dem Klassen-Typbezeichner aufgerufen werden
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=E_UnzulÑssige Verwendung von ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=E_BereichsprÅfungsfehler im SET-Konstruktor oder doppeltes Set-Element
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=E_Zeiger auf Objekt erwartet
% You specified an illegal type in a \var{New} statement.
% The extended synax of \var{New} needs an  object as a parameter.
parser_e_expr_have_to_be_constructor_call=E_Ausdruck muss ein Konstruktor-Aufruf sein
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=E_Ausdruck muss ein Destruktor-Aufruf sein
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=E_UnzulÑssige Reihenfolge der Record-Elemente
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=E_Typ des Ausdrucks muss eine Klasse oder ein Recordtyp sein
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=E_Prozeduren kînnen keinen Wert zurÅckliefern
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures  cannot return a value.
parser_e_constructors_always_objects=E_Konstruktoren und Destruktoren mÅssen Methoden sein
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=E_Operator besitzt kein Overload
% You're trying to use an overloaded operator when it isn't overloaded for
% this type.
parser_e_no_such_assignment=E_Es ist nicht mîglich, die Zuweisung fÅr gleiche Typen zu Åberladen
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=E_Unmîgliche Operator-.berladung
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=E_Auslîsen einer Exception an dieser Stelle nicht mîglich
% You are trying to raise an exception where it isn't allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=E_Die erweiterte Syntax von New oder Dispose ist fÅr Klassen unzulÑssig
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{Dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_asm_incomp_with_function_return=E_Assembler-Funktion nicht mit diesem RÅckgabetyp mîglich
% You're trying to implement a \var{assembler} function, but the return type
% of the function doesn't allow that.
parser_e_procedure_overloading_is_off=E_Das öberladen von Prozeduren ist ausgeschaltet
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=E_Es ist nicht mîglich, dieesen Operator zu Åberladen (benutzen Sie stattdessen '=')
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=E_Vergleichsoperator muss einen booleschen Wert zurÅckgeben
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=E_Nur virtuelle Methoden kînnen abstrakt sein
% You are declaring a method as abstract, when it isn't declared to be
% virtual.
parser_f_unsupported_feature=F_Benutzung einer nicht unterstÅtzten Erweiterung!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=E_Das Mischen von Klassen und Objekten ist unzulÑssig
% You cannot derive \var{objects} and \var{classes} intertwined . That is,
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=W_Unbekannte Prozedur, Direktive $1 wurde ignoriert
% The procedure direcive you secified is unknown. Recognised procedure
% directives are \var{cdecl}, \var{stdcall}, \var{popstack}, \var{pascal}
% \var{register}, \var{export}.
parser_e_absolute_only_one_var=E_ABSOLUTE kann nur auf eine alleinstehende Variable angewendet werden
% You cannot specify more than one variable before the \var{absolute} directive.
% Thus, the following construct will provide this error:
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
% \begin{itemize}
% \item [ absolute can only be associated a var or const ]
% \end{itemize}
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_absolute_only_to_var_or_const=E_ABSOLUTE kann nur auf Variablen und Konstanten angewendet werden
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=E_Es kann nur EINE Variable initialisiert werden
% You cannot specify more than one variable with a initial value
% in Delphi syntax.
parser_e_abstract_no_definition=E_Abstrakte Methoden dÅrfen keine Definition (mit Rumpf) haben
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=E_Diese Åberladene Funktion darf nicht lokal sein (muss Exportiert werden)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=W_Virtuelle Methoden wurden ohne Konstruktor verwendet in $1
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_m_macro_defined=M_Makro definiert: $1
% When \var{-vm} is used, the compiler tells you when it defines macros.
parser_m_macro_undefined=M_Makro gelîscht: $1
% When \var{-vm} is used, the compiler tells you when it undefines macros.
parser_m_macro_set_to=M_Makro $1 auf $2 gesetzt
% When \var{-vm} is used, the compiler tells you what values macros get.
parser_i_compiling=I_Kompiliere $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=U_Parse Interface von Unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=U_Parse Implementation von $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=D_Kompiliere $1 zum zweiten Mal
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_paras_allowed=E_Array-Eigenschaften sind an dieser Stelle nicht erlaubt
% You cannot use array properties at that point.
parser_e_no_property_found_to_override=E_Es gibt keine Property diesen Namens, die Åberschrieben werden kînnte
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=E_Es ist nur eine einzelne Default-Property erlaubt, vererbte Default-Property in Klasse $1 gefunden
% You specified a property as \var{Default}, but a parent class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=E_Die Default-Property muss eine Array-Property sein
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=E_Virtuelle Konstruktoren werden nur im Klassen-Objektmodell unterstÅtzt
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=E_Keine Default-Property verfÅgbar
% You try to access a default property of a class, but this class (or one of
% it's ancestors) doesn't have a default property.
parser_e_cant_have_published=E_Die Klasse kann keinen PUBLISHED Bereich haben, benutzen Sie den {$M+} Schalter
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=E_Forward-Deklaration der Klasse $1 muss hier aufgelîst werden, wenn sie als Elternklasse benutzt werden soll
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=E_Lokale Operatoren werden nicht unterstÅtzt
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=E_Prozedur-Direktive $1 unzulÑssig im Interface-Bereich
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=E_Prozedur Direktive $1 unzulÑssig im Implementation-Bereich
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=E_Prozedur-Direktive $1 unzulÑssig in einer Procvar-Deklaration
% This procedure directive cannot be part of a procedural of function
% type declaration.
parser_e_function_already_declared_public_forward=E_Funktion ist bereits als Public oder Forward deklariert: $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=E_Kann nicht EXPORT und EXTERNAL gleichzeitig benutzen
% These two procedure directives are mutually exclusive
parser_e_name_keyword_expected=E_SchlÅsselwort NAME erwartet
% The definition of an external variable needs a \var{name} clause.
parser_w_not_supported_for_inline=W_$1 noch nicht innerhalb von inline Prozeduren/Funktionen unterstÅtzt
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=W_Inlining deaktiviert
% Inlining of procedures is disabled.
parser_i_writing_browser_log=I_Schreibe Browser-Log $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+\}} switch).
parser_h_maybe_deref_caret_missing=H_Mîglicherweise fehlt eine Zeiger-Dereferenzierung
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=F_GewÑhlter Assembler-Leser nicht unterstÅtzt
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=E_Prozedur-Direktive $1 steht in Konflikt mit anderen Direktiven
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=E_Aufrufkonvention passt nicht zur Forward-Deklaration
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_register_calling_not_supported=E_Register-Aufrufkonvention (fastcall) nicht unterstÅtzt
% The \var{register} calling convention, i.e., arguments are passed in
% registers instead of on the stack is not supported. Arguments are always
% passed on the stack.
parser_e_property_cant_have_a_default_value=E_Property kann keinen Vorgabewert haben
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=E_Der Vorgabewert einer Property muss eine Konstante sein
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=E_Symbol darf nicht PUBLISHED sein, dies kann nur eine Klasse sein
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=E_Diese Property-Art kann nicht Published sein
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_w_empty_import_name=W_Leerer Importname angegeben
% Both index and name for the import are 0 or empty
parser_e_empty_import_name=W_Leerer Importname angegeben
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_used_proc_name_changed=E_Funktionsinterner Name hat sich nach der Verwendung einer Funktion geÑndert
% This is an internal error; please report any occurrences of this error
% to the \fpc team.
parser_e_division_by_zero=E_Division durch Null
% There is a divsion by zero encounted
parser_e_invalid_float_operation=E_UngÅltige Fliesskomma-Operation
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=E_Obere Grenze des Bereichs ist kleiner als die untere Grenze
% The upper bound of a \var{case} label is less than the lower bound and this
% is not possible
parser_w_string_too_long=W_String "$1" ist lÑnger als $2
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=E_StringlÑnge ist grîsser als die LÑnge des "array of char"
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=E_UngÅltiger Ausdruck nach der 'Message'-Direktive
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=E_Message-Handler akzeptieren nur einen "Call by Reference" Parameter
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=E_Doppeltes Message-Label: $1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=E_SELF darf nur in Message-Handlern ein expliziter Parameter sein
% The self parameter can be passed only explicit if it is a method which
% is declared as message method handler
parser_e_threadvars_only_sg=E_Threadvariablen kînnen nur statisch oder global sein
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has it's own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=F_Direkter Assembler wird f+r binÑres Ausgabeformat nicht unterstÅtzt
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=W_Laden Sie die OBJPAS Unit nicht manuell, benutzen Sie statdessen {$mode objfpc} oder {$mode delphi}
% You're trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea to do, you can better use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automaticly
parser_e_no_object_override=E_OVERRIDE darf in Objekten nicht verwendet werden
% Override isn't support for objects, use VIRTUAL instead to override
% a method of an anchestor object
parser_e_cant_use_inittable_here=E_Datentypen, die ein Initialiserung oder Finalisierung benîtigen, kînnen in varianten Records nicht verwendet werden
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=E_Resourcestrings dÅrfen nur statisch oder global sein
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=E_EXIT mit Argument darf hier nicht verwendet werden
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=E_Der Typ des STORED-Symbols muss boolesch sein
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=E_Dieses Symbol ist als Speichersymbol unzulÑssig
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=E_Nur Klassen, die im "$M+"-Modus kompiliert wurden, dÅrfen published sein
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=E_Prozedurdirektive erwartet
% When declaring a procedure in a const block you used a ; after the
% procedure declaration after which a procedure directive must follow.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=E_Der Wert fÅr einen Property-Index muss ordinalen Typs sein
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=E_Prozedurname zu kurz um exportiert zu werden
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=E_Es kann kein DEFFILE-Eintrag fÅr unit-globale Variablen erzeugt werden
parser_e_dlltool_unit_var_problem2=E_Kompiliere ohne "-WD"-Option
#
# Type Checking
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=E_Typen passen nicht zusammen
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=E_Inkompatible Typen: Habe "$1" erhalten, aber "$2" erwartet
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
type_e_not_equal_types=E_Typen sind verschieden bei $1 und $2
% The types are not equal
type_e_type_id_expected=E_Typbezeichner erwartet
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=E_Variablenbezeichner erwartet
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=E_Integer-Ausdruck erwartet, aber "$1" erhalten
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=E_Booleschen Ausdruck erwartet, aber "$1" erhalten
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=E_Ganzahligen Ausdruck erwartet
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=E_Zeigertyp erwartet, aber "$1" erhalten
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=E_Klassentyp erwartet, aber "$1" erhalten
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_varid_or_typeid_expected=E_Variable oder Typbezeichner erwartet
% The argument to the \var{High} or \var{Low} function is not a variable
% nor a type identifier.
type_e_cant_eval_constant_expr=E_Kann konstanten Ausdruck nicht auswerten
% No longer in use.
type_e_set_element_are_not_comp=E_Set-Elemente sind nicht kompatibel
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=E_Operation fÅr Sets nicht implementiert
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=W_Automatische Typumwandlung von Fliesskommatyp nach COMP (=integer mit 64 bit)
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{Comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=H_Verwenden sie DIV um ein Integer-Ergebnis zu erhalten
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=E_Stringtypen passen nicht zueinander, da in "$V+"-Modus
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=E_Succ oder Pred kann nicht auf AufzÑhlungen mit Zuweisungen angewendet werden
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=E_Kann Variablen dieses Typs nicht lesen oder schreiben
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% booleans, reals, pchars and strings can be read from/written to a text file.
type_e_no_readln_writeln_for_typed_file=E_Kann ReadLn und WriteLn nicht bei typisierten Dateien verwenden
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=E_Kann Read und Write nicht bei untypisierten Dateien verwenden
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=E_Typkonflikt zwischen den Elementen des Sets
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=W_lo/hi(dword/qword) gibt oberes/unteres Word/DWord zurÅck
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=E_Integer- oder Real-Ausdruck erwartet
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=E_Falscher Typ im Array-Konstructor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=E_Inkompatible Typen fÅr Argument Nr. #$1: habe $2 erhalten, aber $3 erwartet
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=E_Methode (Variable) und Prozedur (Variable) sind nicht kompatibel
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=E_UnzulÑssige Konstante an interne Algebrafunktion Åbergeben
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=E_Von Konstanten kann keine Adresse bestimmt werden
% It's not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=E_Auf das Argument kann nichts zugewiesen werden
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they can't be used as arguments
type_e_cannot_local_proc_to_procvar=E_Kann lokale Prozedur/Funktion nicht an Prozedurvariable zuweisen
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=E_Kann einer Adresse keine Werte zuweisen
% It's not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=E_Kann einer Konstanten keine Werte zuweisen
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing make the parameter value or var.
% \end{description}
#
# Symtable
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=E_Bezeichner "$1" nicht gefunden
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=F_Interner Fehler in SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=E_Doppelter Bezeichner "$1"
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=H_Bezeichner ist bereits definiert in $1 in Zeile $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=E_Unbekannter Bezeichner "$1"
% The identifier encountered hasn't been declared, or is used outside the
% scope where it's defined.
sym_e_forward_not_resolved=E_Forward-Deklaration "$1" nicht gefunden
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_f_id_already_typed=F_Bezeichnertyp ist bereits als Typ definiert
% You are trying to redefine a type.
sym_e_error_in_type_def=E_Fehler in Typdefinition
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_type_id_not_defined=E_Typbezeichner nicht definiert
% The type identifier has not been defined yet.
sym_e_forward_type_not_resolved=E_Forward-Typ "$1" nicht gefunden
% The compiler encountered an unknown type.
sym_e_only_static_in_static=E_Nur statische Variablen kînnen in statischen oder Ñusseren Methoden verwendet werden
% A static method of an object can only access static variables.
sym_e_invalid_call_tvarsymmangledname=E_UngÅltiger Aufruf von tvarsym.mangledname()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_f_type_must_be_rec_or_class=F_Record- oder Klassen-Typ erwartet
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=E_Instanzen von Klassen oder Objekten mit abstrakten Methoden sind unzulÑssig
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=W_Label "$1" nicht definiert
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=E_Label "$1" benutzt aber nicht definiert
% A label was declared and used, but not defined.
sym_e_ill_label_decl=E_UngÅltige Label-Deklaration
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=E_GOTO und LABEL werden nicht unterstÅtzt (verwenden Sie den Schalter -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the  \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=E_Label nicht gefunden
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=E_Bezeichner ist kein Label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=E_Label ist bereits definiert
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=E_UngÅltige Typdeklaration von Set-Elementen
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=E_Forward-Klassendefinition nicht gefunden: $1
% You declared a class, but you didn't implement it.
sym_n_unit_not_used=H_Unit "$1" wird von "$2" nicht verwendet
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=H_Parameter nicht verwendet: $1
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=N_Lokale Variable nicht verwendet: $1
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=H_Werte-Parameter "$1" wurde zugewiesen aber nie verwendet
% This is a warning. The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=N_Lokale Variable "$1" wurde zugewiesen aber nicht verwendet
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=H_Lokales $1-Element "$2" wird nicht verwendet
% A local symbol is never used.
sym_n_private_identifier_not_used=N_Privates Feld $1.$2 wird nie verwendet
sym_n_private_identifier_only_set=N_Privates Feld $1.$2 wurde zugewiesen aber nie verwendet
sym_n_private_method_not_used=N_Private Methode $1.$2 wird nie verwendet


sym_e_set_expected=E_Mengentyp erwartet
% The variable or expression isn't of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=W_Funktionsergebnis scheint keine Menge zu sein
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=W_Typ "$1" ist im aktuellen Record mit C-Packing nicht korrekt ausgerichtet
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=E_Unbekannter Record-Feldbezeichner $1
% The field doesn't exist in the record definition.
sym_n_uninitialized_local_variable=W_Lokale Variable "$1" wird verwendet, bevor ihr ein Wert zugewiesen wurde
sym_n_uninitialized_variable=W_Variable "$1" scheint nicht initialisert worden zu sein
% These messages are displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=E_Bezeichener verweist nicht auf ein Element: $1
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the class you are trying to create. The procedure you specified
% does not exist.
sym_b_param_list=B_Deklaration gefunden: $1
% You get this when you use the \var{-vb} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=E_Datensegment zu gross (max. 2GB)
% You get this when you declare an array whose size exceeds the 2GB limit.
% \end{description}
#
# Codegenerator
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_break_not_allowed=E_BREAK nicht zulÑssig
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=E_CONTINUE nicht zulÑssig
% You're trying to use \var{continue} outside a loop construction.
cg_e_too_complex_expr=E_Ausdruck zu komplex - FPU StackÅberlauf
% Your expression is too long for the compiler. You should try dividing the
% construct over multiple assignments.
cg_e_illegal_expression=E_UngÅltiger Ausdruck
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
cg_e_invalid_integer=E_UngÅltiger Ausdruck, kein Integer
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
cg_e_invalid_qualifier=E_UngÅltige Kombination
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
cg_e_upper_lower_than_lower=E_Oberes Bereichsende < unteres Bereichsende
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
cg_e_illegal_count_var=E_UnzulÑssige ZÑhlvariable
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
cg_e_cant_choose_overload_function=E_Kann mich bestimmen, welche Åberladene Funktion aufgerufen werden soll
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
cg_e_parasize_too_big=E_Grîsse der Parameterliste Åbersteigt 65535 Bytes
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_illegal_type_conversion=E_UnzulÑssige Typumwandlung
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
cg_d_pointer_to_longint_conv_not_portable=D_Umwandlung zwischen ganzen Zahlen und Pointern ist zwischen verschiedenen Plattformen nicht portabel
% If you typecast a pointer to a longint, this code will not compile
% on a machine using 64bit for pointer storage.
cg_e_file_must_call_by_reference=E_Dateitypen mÅssen VAR Parameter sein
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=E_Die Verwendung eines FAR-Zeigers ist in dieser Art nicht erlaubt
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_var_must_be_reference=E_UnzulÑssiger "call by reference"-Parameter
% You are trying to pass a constant or an expression to a procedure that
% requires a \var{var} parameter. Only variables can be passed as a \var{var}
% parameter.
cg_e_dont_call_exported_direct=E_Mit EXPORT deklarierte Funktionen kînnen nicht aufgerufen werden
% No longer in use.
cg_w_member_cd_call_from_method=W_Mîglicherweise unzulÑssiger Aufruf eines Konstruktors oder Destruktors (passt nicht in diesen Kontext)
% No longer in use.
cg_n_inefficient_code=N_Uneffiziente Programmierung
% You construction seems dubious to the compiler.
cg_w_unreachable_code=W_Code wird niemals ausgefÅhrt
% You specified a loop which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_stackframe_with_esp=E_Prozeduraufruf mit Stackframe ESP/SP
% The compiler enocountered a procedure or function call inside a
% procedure that uses a \var{ESP/SP} stackframe. Normally, when a call is
% done the procedure needs a \var{EBP} stackframe.
cg_e_cant_call_abstract_method=E_Abstrakte Methoden kînnen nicht direkt aufgerufen werden
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_f_internal_error_in_getfloatreg=F_Interner Fehler in getfloatreg(), Allozierungsfehler
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_unknown_float_type=F_Unbekannter Fliesskommatyp
% The compiler cannot determine the kind of float that occurs in an expression.
cg_f_secondvecn_base_defined_twice=F_SecondVecn() Basis wurde zweimal definiert
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_extended_cg68k_not_supported=F_Extended wird auf der m68k-Plattform nicht unterstÅtzt
% The var{extended} type is not supported on the m68k platform.
cg_f_32bit_not_supported_in_68000=F_Vorzeichenlose 32-Bit-Typen werden im MC68000-Modus nicht unterstÅzt
% The cardinal/dword is not supported on the m68k platform.
cg_f_internal_error_in_secondinline=F_Interner Fehler in secondinline()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_d_register_weight=D_Register $1 Gewichtung $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_e_stacklimit_in_local_routine=E_Stacklimit in lokaler Routine Åberschritten
% Your code requires a too big stack. Some operating systems pose limits
% on the stack size. You should use less variables or try ro put large
% variables on the heap.
cg_d_stackframe_omited=D_Stackframe wird ausgelassen
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_w_64bit_range_check_not_supported=W_BereichsÅberprÅfung fÅr 64-Bit-Integer wird fÅr die Zielplattform nicht unterstÅtzt
% 64 bit range check is not yet implemented for 32 bit processors.
cg_e_unable_inline_object_methods=E_Objektmethoden kînnen nicht Inline sein
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=E_Procvar-Aufrufe kînnen nicht Inline sein
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=E_Kein Code fÅr Inline-Prozedur gespeichert
% The compiler couldn't store code for the inline procedure.
cg_e_no_call_to_interrupt=E_Direkter Aufruf von Interruptprozedur $1 ist nicht mîglich
% You can not call an interrupt procedure directly from FPC code
cg_e_can_access_element_zero=E_Auf Element Null von Ansi/Wide- oder Longstring kann nicht zugegriffen werden, benutzen Sie stattdessen (Set)Length
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such kinf of string
cg_e_include_not_implemented=E_Include und Exclude sind fÅr diesen Fall noch nicht implementiert
% \var{include} and \var{exclude} are only partially
% implemented for \var{i386} processors
% and not at all for \var{m68k} processors.
cg_e_cannot_call_cons_dest_inside_with=E_Konstruktoren und Destruktoren kînnen in diesem Kontext nicht aufgerufen werden
% Inside a \var{With} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=E_Kann Messagehandler-Methode nicht direkt aufrufen
% A message method handler method can't be called directly if it contains an
% explicit self argument
% \end{description}
# EndOfTeX
#
# Assembler reader
#
cg_e_goto_inout_of_exception_block=E_Sprung in- oder aus Exceptionblock heraus
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
% \end{description}
# EndOfTeX
cg_e_control_flow_outside_finally=E_Kontrollfluss-Anweisungen sind in einem Finally-Block nicht erlaubt
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
# EndOfTeX

#
# Assembler reader
#
asmr_d_start_reading=D_Starte $1 Stil Assembler Parsen
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=D_Beende $1 Stil Assembler Parsen
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=E_Nicht-Label Bezeichner enthÑlt @
% A identifier which isn't a label can't contain a @.
asmr_w_override_op_not_supported=W_Override-Operator wird nicht unterstÅtzt
% The Override operator is not supported
asmr_e_building_record_offset=E_Fehler beim Ermitteln des Recordoffsets
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=E_OFFSET ohne Bezeicner verwendet
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=E_TYPE verwendet ohne Bezeichner
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=E_Lokale Variable oder Parameter kînnen hier nicht verwendet werden
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the %ebp register so the
% address can't be get directly.
asmr_e_need_offset=E_Hier muss OFFSET verwendet werden
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=E_Hier muss "$" verwendet werden
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=E_Verwendung von mehreren verschiebbaren Symbolen nicht mîglich
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=E_Verschiebbares Symbol kann nur addiert werden
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=E_UngÅltiger Konstantenausdruck
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=E_Verschiebbares Symbol ist nicht zulÑssig
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=E_UngÅltige Verweis-Syntax
% There is an error in the reference.
asmr_e_local_para_unreachable=E_Sie kînnen "$1" von diesem Code aus nicht erreichen
% You can not read directly the value of local or para
% of a higher level in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=E_Lokale Symbole/Labels sind nicht als Referenz zulÑssig
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=E_UngÅltige Verwendung von Basis- und Index-Registern
% There is an error with the base and index register
asmr_w_possible_object_field_bug=W_Mîglicher Fehler bei Objektfeld-Behandlung
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=E_Falscher Skalierungsfaktor angegeben
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=E_Mehrfache Verwendung fon Index-Registern
% You are trying to use more than one index register
asmr_e_invalid_operand_type=E_UngÅltiger Operandentyp
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=E_UngÅlitge Zeichenkette als Opcode-Operand: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=W_@CODE und @DATA werden nicht unterstÅtzt
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=E_Null-Label-Bezug nicht mîglich
asmr_e_expr_zero_divide=E_Division durch Null in Assembler-Ausdruck
asmr_e_expr_illegal=E_UngÅltiger Ausdruck
asmr_e_escape_seq_ignored=E_Escape-Sequenz ignoriert: $1
asmr_e_invalid_symbol_ref=E_UngÅltige Symbolverwendung
asmr_w_fwait_emu_prob=W_FWAIT kann Emulationsprobleme mit emu387 verursachen
asmr_w_fadd_to_faddp=W_FADD ohne Operand wurde in FADDPÅbersetzt
asmr_w_enter_not_supported_by_linux=W_Der ENTER-Befehl wird vom Linux-Kernel nicht unterstÅtzt
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=W_Aufruf einer Åberladenen Funktion in Assembler
asmr_e_unsupported_symbol_type=E_Nicht unterstÅtzter Symboltyp fÅr Operand
asmr_e_constant_out_of_bounds=E_Wert der Konstante ausserhalb des zulÑssigen Bereichs
asmr_e_error_converting_decimal=E_Fehler beim Umwandeln in Dezimal $1
asmr_e_error_converting_octal=E_Fehler beim Umwandeln in Oktal $1
asmr_e_error_converting_binary=E_Fehler beim Umwandeln in BinÑr $1
asmr_e_error_converting_hexadecimal=E_Fehler beim Umwandeln in Hexadezimal $1
asmr_h_direct_global_to_mangled=H_$1 Åbersetzt nach $2
asmr_w_direct_global_is_overloaded_func=W_$1 ist einer Åberladenen Funktion zugeordnet
asmr_e_cannot_use_SELF_outside_a_method=E_Kann SELF nicht ausserhalb einer Methode verwenden
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=E_Kann OLDEBP ausserhalb einer verschachtelten Prozedur nicht verwenden
asmr_e_void_function=W_Prozeduren kînnen keinen Wert im Assembler-Code zurÅckliefern
asmr_e_SEG_not_supported=E_SEG nicht unterstÅtzt
asmr_e_size_suffix_and_dest_dont_match=E_Grîssensuffix und Ziel- oder Quellgrîsse passen nicht zusammen
asmr_w_size_suffix_and_dest_dont_match=W_Grîssensuffix und Ziel- oder Quellgrîsse passen nicht zusammen
asmr_e_syntax_error=E_Assembler Syntaxfehler
asmr_e_invalid_opcode_and_operand=E_UngÅltige Kombination von Opcode und Operanden
asmr_e_syn_operand=E_Assembler Syntaxfehler im Operanden
asmr_e_syn_constant=E_Assembler Syntaxfehler in Konstanten
asmr_e_invalid_string_expression=E_UngÅltiger Stringausdruck
asmr_w_const32bit_for_address=32-Bit-Konstanten fÅr Adresse erzeugt
asmr_e_unknown_opcode=E_Unbekannter Opcode $1
asmr_e_invalid_or_missing_opcode=E_UngÅltiger oder fehlender Opcode
asmr_e_invalid_prefix_and_opcode=E_UngÅltige Kombination von Prefix und Opcode: $1
asmr_e_invalid_override_and_opcode=E_UngÅltige Kombination von Override und Opcode: $1
asmr_e_too_many_operands=E_Zu viele Operanden in der Zeile
asmr_w_near_ignored=W_NEAR ignoriert
asmr_w_far_ignored=W_FAR ignoriert
asmr_e_dup_local_sym=E_Doppelters lokales Symbol $1
asmr_e_unknown_local_sym=E_Undefiniertes lokales Symbol $1
asmr_e_unknown_label_identifier=E_Unbekannter Label-Bezeichner $1
asmr_e_invalid_register=E_UngÅltiger Registername
asmr_e_invalid_fpu_register=E_UngÅltiger Name fÅr Fliesskommaregister
asmr_e_nor_not_supported=E_NOR nicht unterstÅtzt
asmr_w_modulo_not_supported=W_Modulo nicht unterstÅtzt
asmr_e_invalid_float_const=E_UngÅltige Fliesskommakonstante $1
asmr_e_invalid_float_expr=E_UngÅltiger Fliesskommaausdruck
asmr_e_wrong_sym_type=E_Falscher Symboltyp
asmr_e_cannot_index_relative_var=E_Kann lokale Variable oder Parameter nicht mit Register indizieren
asmr_e_invalid_seg_override=E_UngÅltiger Segmentoverride-Ausdruck
asmr_w_id_supposed_external=W_Bezeichner $1 ist vermutlich External
asmr_e_string_not_allowed_as_const=E_Strings sind als Konstanten unzulÑssig
asmr_e_no_var_type_specified=Typ der Variablen nicht angegeben
asmr_w_assembler_code_not_returned_to_text=E_Assemblercode kehrt nicht zum Text zurÅck
asmr_e_not_directive_or_local_symbol=E_Keine Direktive oder lokales Symbol $1
asmr_w_using_defined_as_local=E_Verwendung eines definierten Namens als lokales Label
asmr_e_dollar_without_identifier=E_Dollarzeichen wird ohne Bezeichner verwendet
asmr_w_32bit_const_for_address=W_32-Bit-Konstante fÅr Adresse erzeugt
asmr_n_align_is_target_specific=N_.align ist abhÑngig von Zielplattform, verwende .balign oder .p2align
asmr_e_cannot_access_field_directly_for_parameters=E_Kann fÅr Parameter nicht direkt auf Felder zugreifen
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=E_Kann auf Felder von Objekten/Klassen nicht direkt zugreifen
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
#
# Assembler/binary writers
#
asmw_f_too_many_asm_files=F_Zu viele Assembler-Dateien
asmw_f_assembler_output_not_supported=F_GewÑhlte Assemblerausgabe wird nicht unterstÅtzt
asmw_f_comp_not_supported=F_Comp nicht unterstÅtzt
asmw_f_direct_not_supported=F_Direct nicht unterstÅtzt fÅr binÑres Schreiben
asmw_e_alloc_data_only_in_bss=E_Allozieren von Daten ist nur in "bss"-Abschnitten zulÑssig
asmw_f_no_binary_writer_selected=F_Kein BinÑrschreiber ausgewÑhlt
asmw_e_opcode_not_in_table=E_Asm: Opcode $1 nicht in Tabelle enthalten
asmw_e_invalid_opcode_and_operands=E_Asm: $1 ungÅltige Kombination von Opcode und Operanden
asmw_e_16bit_not_supported=E_Asm: 16-Bit-Verweise werden nicht untertÅtzt
asmw_e_invalid_effective_address=E_Asm: UngÅltige effektive Adresse
asmw_e_immediate_or_reference_expected=E_Asm: Konstanten Ausdruck oder Referenz erwartet
asmw_e_value_exceeds_bounds=E_Asm: $1 Wert Åberschreitet Grenzen $2
asmw_e_short_jmp_out_of_range=E_Asm: "Short jump" ist ausserhalb der Bereichs $1
asmw_e_undefined_label=E_Asm: Undefiniertes Label: $1

#
# Executing linker/assembler
#
exec_w_source_os_redefined=W_Quell-Betriebssystem wurde neu definiert
exec_i_assembling_pipe=I_Assembliere (pipe) $1
exec_d_cant_create_asmfile=E_Kann Assemblerdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_objectfile=E_Kann Objektdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_archivefile=E_Kann Archivdatei nicht erzeugen: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_w_assembler_not_found=W_Assembler $1 nicht gefunden, schalte um auf externes Assemblieren
exec_t_using_assembler=T_Benutze Assembler: $1
exec_w_error_while_assembling=W_Fehler wÑhren des Assemblierens, Exitcode $1
exec_w_cant_call_assembler=W_Kann den Assembler nicht aufrufen, Fehler $1 beim Umschalten auf externen Assembler
exec_i_assembling=I_Assembliere $1
exec_i_assembling_smart=I_Assembliere mit Smartlinking $1
exec_w_objfile_not_found=W_Objekt $1 nicht gefunden, Linken kann fehlschlagen!
exec_w_libfile_not_found=W_Bibliothek $1 nicht gefunden, Linken kann fehlschlagen!
exec_w_error_while_linking=W_Fehler wÑhrend des Linkens
exec_w_cant_call_linker=W_Kann Linker nicht aufrufen, schalte um auf externes Linken
exec_i_linking=I_Linke $1
exec_w_util_not_found=W_Hilfsprogramm "$1" nicht gefunden, schalte um auf externes Linken
exec_t_using_util=T_Benutze Hilfsprogramm $1
exec_e_exe_not_supported=E_Erzeugen von ausfÅhrbaren Dateien nicht unterstÅtzt
exec_e_dll_not_supported=E_Dynamische Bibliotheken nicht unterstÅtzt
exec_i_closing_script=I_Schliesse Skript $1
exec_w_res_not_found=W_Resource Compiler nicht gefunden, schalte um auf externen Modus
exec_i_compilingresource=I_Kompiliere Resource $1
#
# Executable information
#
execinfo_f_cant_process_executable=F_Kann ausfÅhrbare Datei nicht nachbearbeiten: $1
execinfo_f_cant_open_executable=F_Kann ausfÅhrbare Datei nicht îffnen: $1
execinfo_x_codesize=X_Grîsse des Codes: $1 Bytes
execinfo_x_initdatasize=X_Grîsse der initialisierten Daten: $1 Bytes
execinfo_x_uninitdatasize=X_Grîsse der nicht initialisierten Daten: $1 Bytes
execinfo_x_stackreserve=X_Stack Bereich "reserved": $1 Bytes
execinfo_x_stackcommit=X_Stack Bereich "commited": $1 Bytes
#
# Unit loading
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=T_Suche Unit: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=T_Lade PPU: $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=U_PPU-Name: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=U_PPU-Flags: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=U_PPU-CRC: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=U_PPU-Zeit: $1
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=U_PPU-Datei zu kurz
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_invalid_header=U_PPU UngÅltiger Header (kein PPU am Anfang)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=U_PPU UngÅltige Version $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=U_PPU ist fÅr einen anderen Prozessor kompiliert
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=U_PPU ist fÅr ein anderes Zielsystem kompiliert
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=U_PPU Quelle: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_write=U_Schreibe $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=F_Kann PPU-Datei nicht schreiben
% An err
unit_f_ppu_read_error=F_Lese PPU-Datei
% Unexpected end of file
unit_f_ppu_read_unexpected_end=F_Unerwartetes Ende der PPU-Datei
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_invalid_entry=F_UngÅltiger Eintrag in PPU-Datei: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=F_PPU Dbx ZÑhler-Problem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=E_UngÅltiger Unitname: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=F_Zu viele Units
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=F_Gegenseitige AbhÑngigkeit von Units zwischen $1 und $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=F_Kann Unit "$1" nicht Åbersetzen, keine Quellen vorhanden
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=F_Kann Unit "$1" nicht finden
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_unit_name_error=W_Unit "$1" wurde nicht gefunden, aber "$2" existiert
unit_f_unit_name_error=F_Unit "$1" gesucht, aber "$2" gefunden
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=W_öbersetzen der Systemunit erfordert den Schalter -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=F_Es traten $1 Fehler beim öbersetzen des Moduls auf, halte an
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=U_Lade aus $1 ($2) die Unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=U_öbersetze $1 erneut, PrÅfsumme fÅr $2 hat sich geÑndert
unit_u_recompile_source_found_alone=U_öbersetze "$1", nur Quellcode gefunden
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=U_öbersetze Unit erneut, statische Biblothek ist Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=U_öbersetze Unit erneut, gemeinsame Bibliothek ist Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=U_öbersetze Unit erneut, obj und asm sind Ñlter als PPU-Datei
% When you use the \var{-vu} flag, the compiler warns if the assembler of
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=U_öbersetze Unit erneut, obj ist Ñlter als asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_start_parse_interface=U_Parse Interface-Abschnitt von "$1"
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_start_parse_implementation=U_Parse Implementation-Abschnitt von "$1"
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=U_Zweites Laden fÅr Unit "$1"
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=U_PPU prÅfe Datei $1 Zeit $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
% \end{description}
# EndOfTeX
#
#  Options
#
option_usage=$1 [Optionen] <Eingabedatei> [Optionen]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=W_Nur eine Quelldatei unterstÅtzt
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=W_DEF-Datei kann nur fÅr OS/2 erzeugt werden
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=E_Verschachtelte Response-Dateien werden nicht unterstÅtzt
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=F_Kein Name fÅr Quelldatei auf der Kommandzeile
% The compiler expects a source file name on the command line.
option_no_option_found=N_Keine Angeben in Konfigurationsdatei "$1" gefunden
% The compiler didn't find any option in that config file.
option_illegal_para=E_UngÅltiger Parameter: $1
% You specified an unknown option.
option_help_pages_para=H_-? zeigt Hilfetext an
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=F_Zu viele verschachtelte Konfigurtionsdateien
% You can only nest up to 16 config files.
option_unable_open_file=F_Kann Datei nicht îffnen $1
% The option file cannot be found.
option_reading_further_from=N_Lese weitere Optionen aus $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=W_Zielsystem ist bereits gesetzt: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=W_Gemeinsame Bibliotheken sind auf der DOS Platform nicht verfÅgbar, verwende stattdessen statische Bibliotheken
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=F_Zu viele IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=F_Zu viele ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=F_Offene Bedingung am Dateiende
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=W_Erzeugung von Debug-Informationen wird von dieser ausfÅhrbaren Datei nicht unterstÅtzt
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=H_Versuchen Sie mit -dGDB erneut zu kompilieren
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=E_Sie verwenden den nun ÅberflÅssigen Schalter $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=E_Sie benutzen den nun ÅberflÅssigen Schalter $1, bitte benutzen Sie $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=N_Schalte Assembler auf den Standard-Assembler-Quellcodeschreiber
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_incompatible_asm=W_Das gewÑhlte Assembler-Ausgabeformat "$1" ist nicht mit "$2" kompatibel
option_asm_forced=W_Verwendung des Assemblers "$1" erzwungen
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_logo_start=Free Pascal Compiler Version $FPCVER [$FPCDATE] fÅr $FPCTARGET
option_logo_end=Copyright (c) 1993-2000 Florian KlÑmpfl
#
# Info (option -i)
#
option_info_start=Free Pascal Compiler Version $FPCVER
option_info1=<lf>
option_info2=Compiler Datum:      $FPCDATE
option_info3=Compiler Zielsystem: $FPCTARGET
option_info4=<lf>
option_info5=Dieses Programm unterliegt der GNU General Public Licence
option_info6=Weitere Informationen sind in COPYING.FPC zu finden
option_info7=<lf>
option_info8=Fehlerberichte, VorschlÑge usw. bitte senden an:
option_info_end=             bugrep@freepascal.org
#
# Help pages (option -? and -h)
#
# note: the ordering of the enum numbering doesn't matter
#
ol_begin=**0*_nach booleschen Optionen geben Sie + zum Ein- bzw. - zum Ausschalten an
ol001=**1a_lîsche die erzeugte Assembler-Datei nicht
ol002=**2al_liste Quellcode-Zeilen in der Assembler-Datei
ol003=**2ar_liste Registerbelegungsinformation in Assembler-Datei
ol004=**2at_liste Temp. Variablenbelegungsinfo in Assembler-Datei
ol005=**1b_erzeuge Browser-Info
ol006=**2bl_erzeuge Info zu lokalen Symbolen
ol007=**1B_erzeuge alle Module (Build)
ol008=**1C<x>_Optionen fÅr Code-Erzeugung:
ol009=3*2CD_erzeuge auch eine dynamische Bibliothek (nicht unterstÅtzt)
ol010=**2Ch<n>_<n> Bytes Heap (zwischen 1023 und 67107840)
ol011=**2Ci_I/O-PrÅfung
ol012=**2Cn_lasse die Linkstufe aus
ol013=**2Co_prÅfe öberlauf von Integer-Operationen
ol014=**2Cr_fÅhre BereichsprÅfung durch
ol015=**2Cs<n>_setze Grîsse des Stacks auf <n>
ol016=**2Ct_fÅhre StackprÅfung durch
ol017=3*2CS_erzeuge statische Bibliothek
ol018=3*2Cx_benutze Smartlinking
ol019=**1d<x>_definiere das Symbol <x>
ol020=*O1D_erzeuge eine DEF-Datei
ol021=*O2Dd<x>_setze Beschreibung zu <x>
ol022=*O2Dw_erzeuge PM-Anwendung
ol023=**1e<x>_setze Pfad zur ausfÅhrbaren Datei
ol024=**1F<x>_Dateinamen und Pfade:
ol025=**2FD<x>_setze das Verzeichnis fÅr die Compiler-Hilfsprogramme
ol026=**2Fe<x>_leite die Fehlerausgabe um nach <x>
ol027=**2FE<x>_setze den Pfad fÅr Exe/Unit-Dateien auf <x>
ol028=*L2Fg<x>_identisch mit -Fl
ol029=**2Fi<x>_ergÑnze <x> zum Include-Pfad
ol030=**2Fl<x>_ergÑnze <x> zum Bibliotheks-Pfad
ol031=*L2FL<x>_benutze <x> als dynamischen Linker
ol032=**2Fo<x>_ergÑnze <x> zum Objektdatei-Pfad
ol033=**2Fr<x>_lade die Fehler-Meldungs Datei <x>
ol034=**2Fu<x>_ergÑnze <x> zum Unit-Pfad
ol035=**2FU<x>_Units werden nach <x> ausgegen, hat Vorrang vor -FE
ol036=*g1g<x>_erzeuge Informationen zur Fehlersuche:
ol037=*g2gg_Verwende gsym
ol038=*g2gd_Verwende dbx
ol039=*g2gh_Heaptrace-Unit einbinden
ol040=*g2gc_ZeigerÅberprÅfung
ol041=**1i_zeige alle Information Åber den Compiler
ol042=**2iD_zeige Compilerdatum
ol043=**2iV_zeige Compilerversion
ol044=**2iSO_zeige Compiler-Betriebssystem
ol045=**2iSP_zeige Compilerprozessor
ol046=**2iTO_zeige Ziel-Betriebssystem
ol047=**2iTP_zeige Zielprozessor
ol048=**1I<x>_ergÑnze <x> zum Include-Pfad
ol049=**1k<x>_Åbergebe <x> an den Linker
ol050=**1l_zeige Logo
ol051=**1n_Standard-Konfigurationsdatei ignorieren
ol052=**1o<x>_die erzeugte, ausfÅhrbaren Datei bekommt den Namen <x>
ol053=**1pg_erzeuge Profiler-Code fÅr gprof
ol054=*L1P_benutze Pipes anstelle von temporÑren Assembler-Dateien
ol055=**1S<x>_Syntax-Optionen:
ol056=**2S2_schalte einige der Delphi 2 Erweiterungen ein
ol057=**2Sc_unterstÅtze spezielle C Operatoren (*=,+=,/= and -=)
ol058=**2Sd_sei Delphi-kompatibel
ol059=**2Se_halte Compiler nach dem ersten Fehler an
ol060=**2Sg_erlaube LABEL und GOTO
ol061=**2Sh_benutze ANSI-Strings
ol062=**2Si_benutze C++ artige INLINE
ol063=**2Sm_unterstÅtze Makros wie in C (global)
ol064=**2So_sei TP/BP 7.0 kompatibel
ol065=**2Sp_sei gpc-kompatibel
ol066=**2Ss_Kon-und Destruktorname mÅssen "Init" und "Done" sein
ol067=**2St_erlaube SchlÅsselwort static in Objekten
ol068=**1s_rufe weder Assembler noch Linker auf (nur mit -a)
ol069=**1u<x>_entferne die Definition fÅr das Symbol <x>
ol070=**1U<x>_Unit-Optionen:
ol071=**2Un_prÅfe den Unitnamen nicht
ol072=**2Us_erzeuge eine Systemunit
ol073=**1v<x>_Meldungen, <x> ist eine Kombination der folgenden Zeichen:
ol074=**2*_e : Fehler (Standard)      d : Debug Info
ol075=**2*_w : Warnungen              u : Unit Info
ol076=**2*_n : Anmerkungen            t : angesprochene/benutzte Dateien
ol077=**2*_h : Hinweise               m : definierte Macros
ol078=**2*_i : allgemeine Info        p : kompilierte Prozeduren
ol079=**2*_l : Zeilennummern          c : Preprozessordirective
ol080=**2*_a : alles                  0 : nichts (ausser Fehlern)
ol081=**2*_b : alle Prozedurdekla-    r : Rhide/GCC kompatibler Modus
ol082=**2*_    rationen im Fehlerfall      x : Exe-Datei Info (nur Win32)
ol083=**2*_
ol084=**1X<x>_Optionen fÅr ausfÅhrbare Dateien:
ol085=*L2Xc_linke mit der C-Bibliothek
ol086=**2Xs_entferne alle Symbole von ausfÅhrbarer Datei
ol087=**2XD_dynamisch linken          (definiert FPC_LINK_DYNAMIC)
ol088=**2XS_statisch linken (default) (definiert FPC_LINK_STATIC)
ol089=**2XX_smartlinken               (definiert FPC_LINK_SMART)
ol090=**0*_Optionen fÅr den Prozessor:
ol091=3*1A<x>_Ausgabeformat:
ol092=3*2Aas_Datei mit Hilfe von GNU AS
ol093=3*2Aasaout_Datei mit Hilfe von GNU AS fÅr aout (Go32v1)
ol094=3*2Anasmcoff_coff-Datei mit Hilfe von Nasm
ol095=3*2Anasmelf_elf32-Datei (Linux) mit Hilfe von Nasm
ol096=3*2Anasmobj_obj-Datei mit Hilfe von Nasm
ol097=3*2Amasm_obj-Datei mit Hilfe von Masm (Microsoft)
ol098=3*2Atasm_obj-Datei mit Hilfe von Tasm (Borland)
ol099=3*2Acoff_coff (Go32v2) benutze internen Schreiber
ol100=3*2Apecoff_pecoff (Win32) benutze internen Schreiber
ol101=3*1R<x>_Assembler-Leser Format:
ol102=3*2Ratt_lese AT&T Assembler Variante
ol103=3*2Rintel_lese Intel Assembler Variante
ol104=3*2Rdirect_kopiere Assembler-Text direkt in die Assembler-Datei
ol105=3*1O<x>_Optimierungen:
ol106=3*2Og_erzeuge kleineren Code
ol107=3*2OG_erzeuge schnelleren Code (Standard)
ol108=3*2Or_Registervariablen (enthÑlt eventuell noch Fehler)
ol109=3*2Ou_aktiviere unsichere Optimierungen (siehe Dokumentation)
ol110=3*2O1_Stufe 1 Optimierungen (schnelle Optimierungen)
ol111=3*2O2_Stufe 2 Optimierungen (-O1 + langsamere Optimierungen)
ol112=3*2O3_Stufe 3 Optimierungen (gleich wie -O2u)
ol113=3*2Op<x>_Ziel-Prozessor:
ol114=3*3Op1_Optimierungen fÅr 386/486
ol115=3*3Op2_Optimierungen fÅr Pentium/PentiumMMX (R)
ol116=3*3Op3_Optimierungen fÅr PPro/PII/c6x86/K6 (R)
ol117=3*1T<x>_Ziel-Betriebssystem
ol118=3*2TGO32V1_Version 1 von DJ Delorie's DOS extender ()
ol119=3*2TGO32V2_Version 2 von DJ Delorie's DOS extender
ol120=3*2TLINUX_Linux
ol121=3*2TOS2_OS/2 2.x
ol122=3*2TWin32_Windows 32 Bit
ol123=6*1A<x>_Ausgabe Format:
ol124=6*2Aas_Unix o-Datei mit Hilfe von GNU AS
ol125=6*2Agas_GNU Motorola Assembler
ol126=6*2Amit_MIT Syntax (alter GAS)
ol127=6*2Amot_Standard Motorola Assembler
ol128=6*1O<x>_Optimierungen:
ol129=6*2Oa_schalte Optimierer ein
ol130=6*2Og_erzeuge kleineren Code
ol131=6*2OG_erzeuge schnelleren Code (Standard)
ol132=6*2Ox_optimiere maximal (enthÑlt eventuell noch Fehler)
ol133=6*2O2_setze Ziel-Prozessor auf MC68020+
ol134=6*1R<x>_Assembler-Leser Format:
ol135=6*2RMOT_lese Assembler nach Motorola-Art
ol136=6*1T<x>_Ziel-Betriebssystem:
ol137=6*2TAMIGA_Commodore Amiga
ol138=6*2TATARI_Atari ST/STe/TT
ol139=6*2TMACOS_Macintosh m68k
ol140=6*2TLINUX_Linux-68k
ol141=**1*_
ol142=**1?_zeigt diese Hilfe an
ol143=6*2RMOT_lese Assembler nach Motorola-Art
ol144=6*1T<x>_Ziel-Betriebssystem:
ol145=6*2TAMIGA_Commodore Amiga
ol146=6*2TATARI_Atari ST/STe/TT
ol147=6*2TMACOS_Macintosh m68k
ol148=6*2TLINUX_Linux-68k
ol149=**1*_
ol150=**1?_zeigt diese Hilfe an
ol_end=**1h_zeigt diese Hilfe ohne Warten an
#
# The End...
#
