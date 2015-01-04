#
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2008 by the Free Pascal Development team
#
#   Dutch Language File for Free Pascal in CP850 (just one line in help affected)
#   Latest updates contributed by Matthijs Willemstein <matthijs at willemstein.net>
#   Based on errore.msg of SVN revision 3982
#
#   See the file COPYING.v2, included in this distribution,
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
general_text_bytes_code=01019_bytes code
general_text_bytes_data=01020_bytes data
# BeginOfTeX
% \section{General compiler messages}
% This section gives the compiler messages which are not fatal, but which
% display useful information. The number of such messages can be
% controlled with the various verbosity level \var{-v} switches.
% \begin{description}
general_t_compilername=01000_T_Gebruikte compiler: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=01001_D_Bronsysteem: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=01002_I_Doelsysteem: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Programmalocatie: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its binaries.
general_t_unitpath=01004_T_Locatie units: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Locatie includes: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Locatie bibliotheken: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Locatie objectbestanden: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 Regels gecompileerd, $2 sec.
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_Geen geheugen meer vrij
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
general_i_writingresourcefile=01010_I_Schrijven van resource string tabel bestand: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Schrijven van resource string tabel bestand: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
general_i_fatal=01012_I_Fatale fout:
% Prefix for Fatal Errors
general_i_error=01013_I_Fout:
% Prefix for Errors
general_i_warning=01014_I_Waarschuwing:
% Prefix for Warnings
general_i_note=01015_I_Noot:
% Prefix for Notes
general_i_hint=01016_I_Tip:
% Prefix for Hints
general_e_path_does_not_exist=01017_E_Pad "$1" bestaat niet
% The specified path does not exist.
general_f_compilation_aborted=01018_F_Compilatie afgebroken
% Compilation was aborted.
general_i_number_of_warnings=01021_I_$1 warning(s) issued
% Total number of warnings issued during compilation.
general_i_number_of_hints=01022_I_$1 hint(s) issued
% Total number of hints issued during compilation.
general_i_number_of_notes=01023_I_$1 tip(s) gegeven
#
# Scanner
#
# 02063 is the last used one
#
% Total number of notes issued during compilation.
% \end{description}
% \section{Scanner messages.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=02000_F_Onverwacht einde van bestand
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment was not closed
% \end{itemize}
scan_f_string_exceeds_line=02001_F_String langer dan regel
% There is a missing closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_ongeldig teken "$1" ($2)
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Taalfout: $2 verwacht in kolom $1
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_TL_Ingevoegd bestand $1 word geopend
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_Commentaar van niveau $1 gevonden
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_ignored_switch=02008_N_Genegeerde compileroptie $1
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Foutieve compileroptie $1
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% does not recognise
scan_w_switch_is_global=02010_W_Deze compileroptie heeft ook een globaal effect
% The compiler switch is misplaced, and should be located at
% the start of the unit or program.
scan_e_illegal_char_const=02011_E_Foutieve constante voor een karakter
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range.
scan_f_cannot_open_input=02012_F_Kan bestand "$1" niet openen
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_Kan ingevoerd bestand niet openen $1
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_illegal_pack_records=02015_E_Ongeldige alignatie-aanduiding voor records: "$1"
% You are specifying the \var{\{\$PACKRECORDS n\} } or \var{\{\$ALIGN n\} }
% with an illegal value for \var{n}. For \$PACKRECORDS valid alignments are 1, 2, 4, 8, 16, 32, C,
% NORMAL, DEFAULT, and for \$ALIGN valid alignment are 1, 2, 4, 8, 16, 32, ON,
% OFF. Under mode MacPas \$ALIGN also supports MAC68K, POWER and RESET.
scan_e_illegal_pack_enum=02016_E_Ongeldige enumeratie grootte aanduiding "$1"
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2,4, NORMAL or DEFAULT are valid here.
scan_e_endif_expected=02017_E_$ENDIF verwacht voor $1 op $2 $3 lijn $4
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Taalfout bij het compileren van een conditionele compilatie uitdrukking
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_e_error_in_preproc_expr=02019_E_Evalueren van een conditionele compilatie uitdrukking
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_w_macro_cut_after_255_chars=02020_W_Macro inhoud is afgekapt op 255 karakters voor evaluatie
% The contents of macros cannot be longer than 255 characters.
scan_e_endif_without_if=02021_E_ENDIF zonder IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_Gebruiker definitie: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_Gebruiker definitie: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_Gebruiker definitie: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_Gebruiker definitie: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_Gebruiker definitie: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_Gebruiker definitie: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_Sleutelwoord herdefinieren als macro heeft geen effect
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_Macro buffer overflow bij lezen of expansie van macro
% Your macro or its result was too long for the compiler.
scan_w_macro_too_deep=02030_W_Uitwerken van macros heeft niveau 16 overschreden
% When expanding a macro, macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_w_wrong_styled_switch=02031_W_Compiler opties in delphi commentaar stijl zijn niet ondersteund
% Compiler switches should be in normal pascal style comments.
scan_d_handling_switch=02032_DL_Behandel schakelaar "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_CL_ENDIF $1 gevonden
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_CL_IFDEF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_CL_IFOPT $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_CL_IF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_CL_IFNDEF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_CL_ELSE $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_CL_Negeer tot...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_Druk <return> om verder te gaan
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Niet ondersteunde optie $1
% When warnings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_Ongeldige compiler optie $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_TL_Terug in $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Niet ondersteund programmatype: $1
% You get this warning, if you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_$APPTYPE niet ondersteund op doelsysteem
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_w_description_not_support=02046_W_DESCRIPTIION is niet ondersteund op het geselecteerde OS
% The \var{\{\$DESCRIPTION\}} directive is not supported on this target OS
scan_n_version_not_support=02047_N_VERSION is niet ondersteund door het doel OS.
% The \var{\{\$VERSION\}} directive is not supported on this target OS
scan_n_only_exe_version=02048_N_VERSION kan alleen voor executables of bibliotheken
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_verkeerd formaat voor VERSION directive $1
% The \var{\{\$VERSION\}} directive format is majorversion.minorversion
% where majorversion and minorversion are words.
scan_e_illegal_asmmode_specifier=02050_E_Ongeldige assembler-stijl opgegeven: "$1"
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_ASM lezer-optie is niet mogelijk in een asm instructie, $1 ingesteld voor volgende blok
% It is not possible to switch from one assembler reader to another
% inside an assembler block. The new reader will be used for next
% assembler statements only.
scan_e_wrong_switch_toggle=02052_E_Verkeerde optiewaarde, gebruik ON/OFF of +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_Resource bestanden worden niet ondersteund op doel OS
% The target you are compiling for doesn't support resource files.
scan_w_include_env_not_found=02054_W_Include omgevingsvariabele $1 niet gevonden in omgeving
% The included environment variable can't be found in the environment, it will
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Ongeldige waarde voor FPU register limiet
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Slechts 1 resource bestand ondersteund voor dit operating systeem.
% The target you are compiling for supports only one resource file.
% The first resource file found is used, the others are discarded.
scan_w_macro_support_turned_off=02057_W_Macro ondersteuning is uitgeschakeld
% A macro declaration has been found, but macro support is currently off,
% so the declaration will be ignored. To turn macro support on compile with
% -Sm on the commandline or add \{\$MACRO ON\} in the source
scan_e_invalid_interface_type=02058_E_Ongeldig interface type opgegeven. Geldige waarden zijn COM, CORBA of DEFAULT
% The interface type that was specified is not supported
scan_w_appid_not_support=02059_W_APPID is slechts ondersteund op PalmOS
% The \var{\{\$APPID\}} directive is only supported for the PalmOS target.
scan_w_appname_not_support=02060_W_APPNAME is slechts ondersteund op PalmOS
% The \var{\{\$APPNAME\}} directive is only supported for the PalmOS target.
scan_e_string_exceeds_255_chars=02061_E_Constante tekenreeksen (strings) kunnen niet langer dan 255 karakters zijn.
% A single string constant can contain at most 255 chars. Try splitting up the
% string in multiple smaller parts and concatenate them with a + operator.
scan_f_include_deep_ten=02062_F_Include bestanden kunnen maar tot op 16 niveaus genest worden
% When including include files the files have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_too_many_push=02063_F_Te veel PUSH niveaus
% A maximum of 20 levels is allowed. This error occurs only in mode MacPas.
scan_e_too_many_pop=02064_E_POP instructie zonder voorafgaande PUSH instructie
% This error occurs only in mode MacPas.
scan_e_error_macro_lacks_value=02065_E_Macro of compilatie-variabele "$1" heeft geen waarde
% Thus the conditional compile time expression cannot be evaluated.
scan_e_wrong_switch_toggle_default=02066_E_Verkeerde schakel-waarde. Gebruik ON/OFF/DEFAULT of +/-/*
% You need to use ON or OFF or DEFAULT or a + or - or * to toggle the switch
scan_e_mode_switch_not_allowed=02067_E_MODE schakel "$1" is niet toegestaan op deze plaats.
% A mode switch has already been encountered, or, in case of option -Mmacpas,
% a mode switch occur after UNIT.
scan_e_error_macro_undefined=02068_E_Macro of compilatie-variabele "$1" is niet gedefinieerd
% Thus the conditional compile time expression cannot be evaluated. Only in mode MacPas.
scan_e_utf8_bigger_than_65535=02069_E_UTF-8 code groter dan 65535 gevonden
% \fpc handles utf-8 strings internally as widestrings e.g. the char codes are limited to 65535
scan_e_utf8_malformed=02070_E_Ongeldige UTF-8 tekenreeks
% The given string isn't a valid UTF-8 string
scan_c_switching_to_utf8=02071_C_UTF-8 handtekening gevonden, gebruik UTF-8 codering
% The compiler found an UTF-8 encoding signature (\$ef, \$bb, \$bf) at the beginning of a file,
% so it interprets it as an UTF-8 file
scan_e_compile_time_typeerror=02072_E_Compilatie-expressie: "$1" gevraagd, maar kreeg "$2" op "$3"
% Type check of a compile time expression failed.
scan_n_app_type_not_support=02073_N_APPTYPE wordt niet ondersteund door het doel OS
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_e_illegal_optimization_specifier=02074_E_Ongeldige optimalisatie gegeven  &quot;$1&quot;
% When you specify an optimization with the \var{\{\$OPTIMIZATION xxx\}}
% the compiler didn't recognize the optimization you specified.
scan_w_setpeflags_not_support=02075_W_SETPEFLAGS wordt niet ondersteund door het doel OS
% The \var{\{\$SETPEFLAGS\}} directive is not supported by the target OS
scan_w_imagebase_not_support=02076_W_IMAGEBASE wordt niet ondersteunddoor het doel OS
% The \var{\{\$IMAGEBASE\}} directive is not supported by the target OS
scan_w_minstacksize_not_support=02077_W_MINSTACKSIZE wordt niet ondersteund door het doel OS
% The \var{\{\$MINSTACKSIZE\}} directive is not supported by the target OS
scan_w_maxstacksize_not_support=02078_W_MAXSTACKSIZE wordt niet ondersteund door het doel OS
#
# Parser
#
# 03192 is the last used one
#
% The \var{\{\$MAXSTACKSIZE\}} directive is not supported by the target OS
% \end{description}
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=03000_E_Parser - Taalfout
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_e_dont_nest_interrupt=03004_E_INTERRUPT procedures kunnen niet genest worden
% An \var{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Procedure type $1 ignored
% The specified procedure directive is ignored by FPC programs.
parser_e_no_overload_for_all_procs=03006_E_Not all declarations of $1 are declared with OVERLOAD
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_export_name_double=03008_E_Dubbel geexporteerde functienaam $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Dubbel geexporteerde functie index $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_Ongeldige index for geexporteerde functie
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_Relocatable bibliotheek of applicatie $1 debug informatie niet ondersteund.
parser_w_parser_win32_debug_needs_WN=03012_W_Om win32 code te debuggen moet relocatie afgezet worden door de -WN optie
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_Constructornaam moet INIT zijn
% You are declaring an object constructor with a name which is not \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_Destructornaam moet DONE zijn
% You are declaring an object destructor with a name which is not \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_proc_inline_not_supported=03016_E_Procedure type INLINE wordt niet ondersteund
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_constructor_should_be_public=03018_W_Constructor mag niet private or protected zijn
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_Destructor mag niet private or protected zijn
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Klasse heeft slechts 1 destructor
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Lokale klassedefinities zijn niet toegestaan
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Anonieme klassedefinities zijn niet toegestaan
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_n_object_has_no_vmt=03023_N_Het object "$1" heeft geen VMT
% This is a note indicating that the declared object has no
% virtual method table.
parser_e_illegal_parameter_list=03024_E_Foutieve parameterlijst
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_size=03026_E_Onjuist aantal parameters meegegeven
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_Overladen symbool "$1" is geen procedure of functie
% The compiler encountered a symbol with the same name as an overloaded
% function, but it is not a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_Overroepen procedures hebben identieke parameterlijst
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_Declaratie komt niet overeen met eerdere declaratie: $1
% You declared a function with same parameters but
% different result type or function modifiers.
parser_e_header_different_var_names=03030_E_Procedurehoofding $1 komt niet overeen met eerdere declaratie: variabelenaam veranderd $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_Waarden in opsommingstypes moeten steeds stijgen
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_e_no_with_for_variable_in_other_segments=03033_E_With kan niet gebruikt worden voor variabelen in verschillende segmenten
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_Procedure meer dan 31 keer genest
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_Bereikfout bij evalueren constanten
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_Bereikscontrolefout bij evalueren constantes.
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_Dubbel case-element
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_Bovengrens van casebereik is lager dan ondergrens
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_Getypeerde constanten van klassen zijn niet toegestaan
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_Procedurele variabelen van overroepen procedures zijn niet toegestaan
% You are trying to assign an overloaded function to a procedural variable.
% This is not allowed
parser_e_invalid_string_size=03041_E_Stringlengte moet tussen 1 en 255 liggen
% The length of a shortstring in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
parser_w_use_extended_syntax_for_objects=03042_W_Gebruik de uitgebreide syntax van DISPOSE en NEW om nieuwe instanties van klassen te genereren
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the object
parser_w_no_new_dispose_on_void_pointers=03043_W_Het gebruik van NEW of DISPOSE voor pointers zonder type is zinloos
parser_e_no_new_dispose_on_void_pointers=03044_E_Het gebruik van NEW of DISPOSE is niet mogelijk voor pointers zonder type
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_Klasse verwacht
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_Typesymbool is hier niet toegestaan
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_Methode verwacht
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_Proceduredeclaratie komt niet overeen met een methode van deze klasse $1
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_d_procedure_start=03049_DL_procedure/functie $1
% When using the \var{-vd} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_Foutieve vlottende-komma constante
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL mag alleen in constructors gebruikt worden
% You are using the \var{fail} keyword outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Destructors hebben geen parameters
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Alleen klassemethoden kunnen gerefereerd worden via een klasse
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
parser_e_only_class_methods=03054_E_Alleen klassemethoden zijn toegankelijk in klassen
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
parser_e_case_mismatch=03055_E_Type van constante komt niet overeen met dat van de case uitrukking
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_Het symbool kan niet uitgevoerd worden uit een bibliotheek
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_Een virtuele methode moet met OVERRIDE overroepen worden: $1
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_Er is geen geerfde methode die overroepen kan worden: $1
% You are trying to \var{override} a virtual method of a parent class that does
% not exist.
parser_e_no_procedure_to_access_property=03059_E_Er is geen lid om de eigenschap te bereiken
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_Stored prorperty directive is not yet implemented
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_Ongeldig symbool voor eigenschaptoegang
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code would cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_Kan niet in beschermd veld van een object schrijven
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module where the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_Kan niet in prive-veld van een object schrijven
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_e_overridden_methods_not_same_ret=03066_E_Overridden methodes moeten dezelfde resultaat type hebben: "$2" wordt overridden door "$1" met een ander type
% If you declare overridden methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_Als EXPORT gedeclareerde procedures kunnen niet genest worden
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_Methoden kunnen niet geexporteerd worden.
% You cannot declare a procedure that is a method for an object as
% \var{export}ed.
parser_e_call_by_ref_without_typeconv=03069_E_Typen van referentieparameters moeten exact overeenkomen. "$1" gekregen, "$2" verwacht.
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_Deze klasse is geen ouderklasse van de huidige klasse
% When calling inherited methods, you are trying to call a method of a non-related
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_SELF is alleen toegelaten in methoden.
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_Methoden kunnen alleen in andere methoden worden aangeroepen met type symbool
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=03073_E_Ongeldig gebruik van ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_Bereikfout in setconstructor of dubbel setelement
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Pointer naar klasse verwacht
% You specified an illegal type in a \var{new} statement.
% The extended syntax of \var{new} needs an object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_Uitdrukking moet een constructoraanroep zijn.
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_Uitdrukking moet een destructoraanroep zijn
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_Ongeldige volgorde van record leden
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_Type van uitdrukking moet class of record type zijn
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Procedures kunnen geen resultaat teruggeven
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures cannot return a value.
parser_e_only_methods_allowed=03081_E_Constructors, destructors en class operators moeten methoden zijn
% You're declaring a procedure as destructor, constructor or class operator, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_Operator is niet hergedefineerd
% You're trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Het is niet toegestaan de toewijzingsoperator te overroepen voor gelijke typen
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Het overroepen van deze operator is onmogelijk
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_Re-raise is hier niet mogelijk
% You are trying to raise an exception where it is not allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_De uitgebreide syntax van NEW of DISPOSE is niet toegestaan voor klassen
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_procedure_overloading_is_off=03088_E_Procedure overroepen is afgezet
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_Deze operator kan niet overroepen worden.
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Vergelijkingsoperator moet een boolean type als resultaat hebben.
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Enkel virtuele methoden kunnen abstract zijn
% You are declaring a method as abstract, when it is not declared to be
% virtual.
parser_f_unsupported_feature=03092_F_Gebruik van niet ondersteunde functionaliteit!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_CLASSES and OBJECTS kunnen niet gemengd worden.
% You cannot derive \var{objects}, \var{classes}, \var{cppclasses} and \var{interfaces} interttwined . E.g.
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Onbekende proceduredirective is genegeerd: $1
% The procedure directive you specified is unknown.
parser_e_absolute_only_one_var=03095_E_Absolute kan maar aan een variabele verbonden worden.
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
parser_e_absolute_only_to_var_or_const=03096_E_Absolute kan slechts aan variabelen of constanten verbonden worden.
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Slechts 1 variabele kan geinitialiseerd worden.
% You cannot specify more than one variable with a initial value
% in Delphi mode.
parser_e_abstract_no_definition=03098_E_Abtracte methoden kunnen niet gedefineerd worden
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Deze overroepen procedure kan niet lokaal zijn (moet geexporteerd worden)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_Gebruik van virtuele methoden zonder constructor in "$1"
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_c_macro_defined=03101_CL_Macro gedefinieerd: $1
% When \var{-vc} is used, the compiler tells you when it defines macros.
parser_c_macro_undefined=03102_CL_Macro definitie verwijderd: $1
% When \var{-vc} is used, the compiler tells you when it undefines macros.
parser_c_macro_set_to=03103_CL_Macro $1 heeft waarde $2 gekregen
% When \var{-vc} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_Compilatie van $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_UL_Parsen van de interface van unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_UL_Parsen van de implementatie van unit $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_DL_Compilatie van $1 voor de tweede keer.
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_property_found_to_override=03109_E_Geen eigenschap gevonden om te herdefinieren
% You want to override a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Slechts 1 standaardeigenschap is toegestaan, geerfde standaardeigenschap in kind gevonden.
% You specified a property as \var{Default}, but the class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_Standaardeigenschappen moeten van type array zijn.
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Virtuele constructors worden alleen ondersteunt in het klassemodel
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_Geen standaardeigenschap gevonden
% You are trying to access a default property of a class, but this class (or one of
% its ancestors) doesn't have a default property.
parser_e_cant_have_published=03114_E_De klasse kan geen gepubliceerde sectie hebben, gebruik {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_Declaratie van klasse $1 moet worden opgelost om de klasse als ouder te gebruiken
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Lokale operatoren worden niet ondersteund
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_Procedure directive $1 niet toegestaan in interfacesectie
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_Procedure directive $1 niet toegestaan in implementatiesectie
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_Procedure directive $1 niet toegestaan in procvar declaratie
% This procedure directive cannot be part of a procedural or function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_Functie is al publiek/forward gedeclareerd $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_Kan niet zowel EXPORT als EXTERNAL gebruiken
% These two procedure directives are mutually exclusive
parser_h_not_supported_for_inline=03123_H_$1 wordt niet ondersteund voor inline procedure/functie
% Inline procedures don't support this declaration.
parser_h_inlining_disabled=03124_H_Inlining uitgeschakeld
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Schrijven van browser logfile $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=03126_H_misschien is het een pointer, ^ ontbreekt dan
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_Geselecteerde assemblerlezer wordt niet ondersteund
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=03128_E_Procedure directive $1 geeft conflicten met andere directives
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_Aanroep conventie klopt niet met voorwaartse
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_property_cant_have_a_default_value=03131_E_Eigenschap kan geen standaard waarde hebben
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_De standaard waarde van een eigenschap moet een constante zijn
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_Symbool kan niet worden gepubliceerd, alleen mogelijk voor een klasse
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Dit soort eigenschappen kunnen niet worden gepubliceerd
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_e_empty_import_name=03136_E_Lege importnaam opgegeven
% Some targets need a name for the imported procedure or a \var{cdecl} specifier
parser_e_division_by_zero=03138_E_Deling door nul
% There is a division by zero encounted
parser_e_invalid_float_operation=03139_E_Ongeldige vlottende-komma bewerking.
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_Bovengrens van bereik is lager dan ondergrens
% The upper bound of a an array declaration is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_string "$1" is langer dan $2
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_Stringlengte moet tussen 1 en 255 liggen
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_Ongeldige uitdrukking achter message optie
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Message handlers aanvaarden slechts 1 variabele parameter
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Dubbel message label: $1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_Self kan alleen een explicite parameter zijn bij string message handlers
% The self parameter can only be passed explicitly to a method which
% is declared as message handler.
parser_e_threadvars_only_sg=03147_E_Threadvariabelen kunnen alleen globaal of statisch zijn
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has its own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Directe assemblercode wordt niet ondersteund door de interne assembler.
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use another assembler reader
parser_w_no_objpas_use_mode=03149_W_Laad de OBJPAS unit niet manueel, gebruik {$mode objfpc} of {$mode delphi}.
% You are trying to load the ObjPas unit manually from a uses clause. This is
% not a good idea. Use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automatically
parser_e_no_object_override=03150_E_OVERRIDE kan niet gebruikt worden in objecten
% Override is not supported for objects, use \var{virtual} instead to override
% a method of a parent object
parser_e_cant_use_inittable_here=03151_E_Datatypen die initialisatie/finalisatie vereisen zijn niet toegestaan in variabele records.
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Resourcestrings kunnen alleen statisch of globaal zijn.
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_Exit met parameter is hier niet toegestaan
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_De stored directive verwacht een boolean argument.
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_Dit symbool is niet toegestaan achter het stored sleutelwoord.
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=03156_E_Alleen klassen gecompileerd in $M+ modus kunnen een published sectie hebben.
% In the published section of a class can be only class as fields used which
% are compiled in \var{\{\$M+\}} or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Proceduredirective verwacht.
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
parser_e_invalid_property_index_value=03158_E_De waarde voor een eigenschapindex moet een ordinale waarde zijn.
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Procedurenaam te kort om te exporteren.
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_Kan geen DEFFILE item genereren voor globale variabelen.
parser_e_dlltool_unit_var_problem2=03161_E_Compileer zonder -WD optie
% You need to compile this file without the -WD switch on the
% commandline
parser_f_need_objfpc_or_delphi_mode=03162_F_U moet OBJFPC (-S2) of DELPHI (-Sd) mode gebruiken om deze code te compileren
% You need to use \{\$mode objfpc\} or \{\$mode delphi\} to compile this file.
% Or use the equivalent commandline switches -S2 or -Sd.
parser_e_no_export_with_index_for_target=03163_E_Kan niet exporteren met indexen kleiner dan $1
% Exporting of functions or procedures with a specified index is not
% supported on this target.
parser_e_no_export_of_variables_for_target=03164_E_Exporteren van variabelen is niet ondersteund op $1
% Exporting of variables is not supported on this target.
parser_e_improper_guid_syntax=03165_E_Ongeldige GUID aanduiding
parser_w_interface_mapping_notfound=03168_W_PGeen procedure genaamd "$1" om "$2.$3" te implementeren gevonden
parser_e_interface_id_expected=03169_E_Interface aanduiding verwacht
% This happens when the compiler scans a \var{class} declaration that contains
% \var{interface} function name mapping code like this:
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% and the \var{interface} before the dot not listed in the inheritance list.
parser_e_type_cant_be_used_in_array_index=03170_E_Type "$1" kan niet als array index type gebruikt worden
% Types like \var{qword} or \var{int64} aren't allowed as array index type
parser_e_no_con_des_in_interfaces=03171_E_Constructors and destructors zijn niet toegestaan in interfaces
% Constructor and destructor declarations aren't allowed in interface
% In the most cases the method \var{QueryInterface} of \var{IUnknown} can
% be used to create a new interface.
parser_e_no_access_specifier_in_interfaces=03172_E_Toegangsmethoden kunnen niet gebruikt worden in interfaces
% The access specifiers \var{public}, \var{private}, \var{protected} and
% \var{pusblished} can't be used in interfaces because all methods
% of an interfaces must be public.
parser_e_no_vars_in_interfaces=03173_E_Een interface definitie kan geen velden bevatten
% Declarations of fields aren't allowed in interfaces. An interface
% can contain only methods
parser_e_no_local_proc_external=03174_E_Can lokale procedure niet als EXTERNAL definieren
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high
parser_w_skipped_fields_before=03175_W_Sommige velden voor "$1" zijn niet geinitialiseerd
% In Delphi mode, not all fields of a typed constant record have to be
% initialized, but the compiler warns you when it detects such situations.
parser_e_skipped_fields_before=03176_E_Sommige velden voor "$1" zijn niet geinitialiseerd
% In all syntax modes but Delphi mode, you can't leave some fields uninitialized
% in the middle of a typed constant record
parser_w_skipped_fields_after=03177_W_Sommige velden na "$1" zijn niet geinitialiseerd
% You can leave some fields at the end of a type constant record uninitialized
% (the compiler will initialize them to zero automatically). This may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_VARARGS aanduiding zonder CDECL of EXTERNAL
% The varargs directive can only be used with procedures or functions
% that are declared with \var{cdecl} and \var{external} directives. The varargs directive
% is only meant to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_SELF moet een normale waarde parameter zijn
% You can't declare self as a const or var parameter, it must always be
% a call-by-value parameter
parser_e_interface_has_no_guid=03180_E_Interface "$1" heeft geen interface definitie
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Ongekend klasse-veld of methode aanduiding "$1"
% Properties must refer to a field or method in the same class.
parser_w_proc_overriding_calling=03182_W_Aanroep-modus "$1" vervangen door "$2"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used
parser_e_no_procvarobj_const=03183_E_Getypeerde constanten van  type "procedure of object" kunnen alleen met NIL geinitialiseerd worden
% You can't assign the address of a method to a typed constant which has a
% 'procedure of object' type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates on (which can not be known at compile time).
parser_e_default_value_only_one_para=03184_E_De default aanduiding kan maar bij 1 parameter horen.
parser_e_default_value_expected_for_para=03185_E_Default parameter vereist voor "$1"
parser_w_unsupported_feature=03186_W_Gebruik van niet-ondersteunde mogelijkheden
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_C arrays worden via referentie doorgegeven
% Any array passed to a C functions is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_"C array of const" moet het laatste argument zijn
% You can not add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Type "$1" herdefinitie
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a cause for errors.
parser_w_cdecl_has_no_high=03190_W_CDECL functies hebben geen HIGH parameter
% Functions declared with cdecl modifier do not pass an extra implicit parameter.
parser_w_cdecl_no_openstring=03191_W_CDECL functies ondersteunen geen open strings
% Openstring is not supported for cdecl'ared functions.
parser_e_initialized_not_for_threadvar=03192_E_Kan THREADVAR variabelen niet initialiseren
% Variables declared as threadvar can not be initialized with a default value.
% The variables will always be filled with zero at the start of a new thread.
parser_e_msg_only_for_classes=03193_E_de MESSAGE aanduiding is alleen toegestaan in classes
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_Procedure of functie verwacht
% A class method can only be specified for procedures and functions.
parser_e_illegal_calling_convention=03195_W_Aanroep-specificatie genegeerd: "$1"
% Some calling conventions are supported only by certain CPUs. I.e. most non-i386 ports support
% only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE kan niet in objecten worden gebruikt
% \var{reintroduce} is not supported for objects.
parser_e_paraloc_only_one_para=03197_E_Elk argument moet zijn eigen locatie hebben
% If locations for arguments are specified explicitly as it is required by
% some syscall conventions, each argument must have it's only location, things
% like \var{procedure p(i,j : longint 'r1');} aren't allowed
parser_e_paraloc_all_paras=03198_E_Elk argument moet een expliciete locatie hebben
% If one argument has an explicit argument location, all arguments of a procedure
% must have one.
parser_e_illegal_explicit_paraloc=03199_E_Ongekende argument locatie
% The location specified for an argument isn't recognized by the compiler
parser_e_32bitint_or_pointer_variable_expected=03200_E_32-bit integer of pointer variabele verwacht
% The libbase for MorphOS/AmigaOS can be give only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_GOTO opdrachten zijn niet toegestaan tussen verschillende procedures
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
parser_f_too_complex_proc=03202_F_Procedure te moeilijk, te veel registers vereist
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_Ongeldige uitdrukking
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
parser_e_invalid_integer=03204_E_Ongeldige integer uitdrukking
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_Ongeldige aanduiding
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_Bovengrens kleiner dan ondergrens
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
parser_e_macpas_exit_wrong_param=03207_E_Exit parameter moet de naam zijn van de procedure waarin het gebruikt wordt
% Non local exit is not allowed. This error occurs only in mode MacPas.
parser_e_illegal_assignment_to_count_var=03208_E_Ongeldige toekenning aan lus-variabele "$1"
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings. You can also not assign values to
% loop variables inside the loop (except in Delphi and TP modes). Use a while or
% repeat loop instead if you need to do something like that, since those
% constructs were built for that.
parser_e_no_local_var_external=03209_E_Kan een locale variabele niet als EXTERNAL aanduiden
% Declaring local variables as external is not allowed. Only global variables can reference
% to external variables.
parser_e_proc_already_external=03210_E_De procedure is reeds als EXTERNAL aangeduid
% The procedure is already declared with the EXTERNAL directive in an interface or
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Impliciet gebruik van de VARIANTS unit
% The Variant type is used in the unit without any used unit using the Variants unit. The
% compiler has implicitly added the Variants unit to the uses list. To remove this warning
% the Variants unit needs to be added to the uses statement.
parser_e_no_static_method_in_interfaces=03212_E_Class en statische methoden kunnen niet gebruikt worden in interfaces
% The specifier \var{class} and directive \var{static} can't be used in interfaces
% because all methods of an interfaces must be public.
parser_e_arithmetic_operation_overflow=03213_E_Overflow in wiskundige berekening
% An operation on two integers values produced an overflow
parser_e_protected_or_private_expected=03214_E_Protected of Private verwacht
% \var{strict} can be only used together with \var{protected} or \var{private}.
parser_e_illegal_slice=03215_E_SLICE kan niet buiten de parameter lijst gebruikt worden
% \var{slice} can be used only for arguments accepting an open array parameter
parser_e_dispinterface_cant_have_parent=03216_E_Een DISPINTERFACE kan geen ouderklasse hebben.
% A DISPINMTERFACE is a special type of interface which can't have a parent class
parser_e_dispinterface_needs_a_guid=03217_E_Een DISPINTERFACE moet een GUID hebben
% A DISPINMTERFACE always needs an interface identification
parser_w_overridden_methods_not_same_ret=03218_W_Methodes die worden override-n moeten een soortgelijke return type hebben. Deze code kan crahsen omdat het afhankelijk is van een Delphi parser bug. (&quot;$2&quot; wordt override-n door &quot;$1&quot;, deze heeft een andere return type)
% If you declare overridden methods in a class definition, they must
% have the same return type. Some versions of Delphi allow you to change the
% return type of interface methods, and even to change procedures into
% functions, but the resulting code may crash depending on the types used
% and the way the methods are called.
parser_e_dispid_must_be_ord_const=03219_E_Bericht ID-s moeten ordinale constanten zijn.
#
# Type Checking
#
# 04049 is the last used one
#
% \end{description}
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Typen stemmen niet overeen
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Incompatibele typen: kreeg $1, verwachtte $2
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
type_e_not_equal_types=04002_E_Typen komen niet overeen $1 en $2
% The types are not equal
type_e_type_id_expected=04003_E_Typesymbool verwacht
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Variabelesymbool verwacht
% This happens when you pass a constant to a routine (such as \var{Inc} var or \var{Dec})
% when it expects a variable. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_Gehele uitdrukking verwacht, $1 gekregen
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Boolean waarde verwacht, kreeg echter "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Ordinale expressie verwacht
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_Pointer type verwacht, "$1" gekregen
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_Klasse type verwacht, "$1" gekregen
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_Kan constante expressie niet evalueren
% This error can occur when the bounds of an array you declared does
% not evaluate to ordinal constants
type_e_set_element_are_not_comp=04012_E_Elementen van set zijn niet compatible
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Bewerking niet ondersteund voor sets
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Typeconversie van vlottende-komma type naar COMP, wat een integer type is.
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_Gebruik DIV voor gehele getallen om geheel resultaat te krijgen
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_String typen stemmen niet overeen wegens de $V+ modus
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_SUCC of PRED op enumeratietypen met toekenningen zijn niet mogelijk
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_Kan dit type variabelen niet lezen of schrijven
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% reals, pchars and strings can be read from/written to a text file.
% Booleans can only be written to text files.
type_e_no_readln_writeln_for_typed_file=04019_E_Kan geen readln of writeln op getypeerde bestanden doen.
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_Kan geen read of write doen op ongetypeerde bestanden.
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Typeconflict tussen setelementen.
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(longint/dword) geeft hoog/laag (ipv byte) woord terug.
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word/dword of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type cast the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Integer of vlottende-komma uitdrukking verwacht
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=04024_E_Verkeerd type "$1" in array constructor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Incompatibel type voor argument #$1: kreeg $2, verwachtte $3
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=04026_E_Methode (variabele) en Procedure (variable) zijn niet compatible
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_Ongeldige constante opgegeven aan interne wiskundige functie
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_Een constante heeft geen adres
% It is not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=04029_E_Kan geen waarde toekennen aan argument
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they cannot be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_Kan geen lokale procedure toekennen aan procedure variabele
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Kan geen waarde toekennen aan een adres
% It is not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Kan geen waarde toekennen aan constante
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing the value make the parameter as a value parameter or a var.
type_e_array_required=04033_E_ARRAY type verwacht
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode also a pointer is allowed.
type_e_interface_type_expected=04034_E_Interface type verwacht, maar kreeg "$1"
type_w_mixed_signed_unsigned=04035_W_Mengen van mogelijk negatieve uitdrukkingen en longwords geeft een 64-bits resultaat
% If you divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64bit which is slower than normal
% 32bit arithmetic. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_w_mixed_signed_unsigned2=04036_W_Mengen van mogelijk negatieve uitdrukkingen en cardinals kan tot range check fouten leiden
% If you use a binary operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% carried out. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_Typecast heeft een verschillende grootte ($1 -> $2) in toekenning
% Type casting to a type with a different size is not allowed when the variable is
% used for assigning.
type_e_array_index_enums_with_assign_not_possible=04038_E_enumeratie typen met vaste waarden kunnen niet als array index gebruikt worden
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use it as index of an array.
type_e_classes_not_related=04039_E_Klasse of object types "$1" en "$2" zijn niet verwant
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_w_classes_not_related=04040_W_Klasse types "$1" en "$2" zijn niet verwant
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_e_class_or_interface_type_expected=04041_E_Klasse of interface type verwacht, maar kreeg "$1"
type_e_type_is_not_completly_defined=04042_E_Type "$1" is niet volledig gedefinieerd
type_w_string_too_long=04043_W_String constante heeft meer karakters dan de lengte van een shortstring
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring
type_w_signed_unsigned_always_false=04044_W_De vergelijking is steeds onwaar omwille van de interval waarden
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% false. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_signed_unsigned_always_true=04045_W_e vergelijking is steeds  waar omwille van de interval waarden
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% true. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_instance_with_abstract=04046_W_Constructie van een klasse "$1" met abstracte methoden
% An instance of a class is created which contains non-implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overridden.
type_h_in_range_check=04047_H_De linker operand van de IN operator moet de grootte van een byte hebben
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8-bits, this may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_Grootte-fout voor type, mogelijk verlies van gegevens/interval-check fout
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_h_smaller_possible_range_check=04049_H_Grootte-fout voor type, mogelijk verlies van gegevens/interval-check fout
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_e_cant_take_address_of_abstract_method=04050_E_Het adres van een abstracte methode kan niet opgevraagd worden
% An abstract method has no body, so the address of an abstract method can't be taken.
type_e_operator_not_allowed=04051_E_De operator kan niet gebruikt worden op dit type operand
% You are trying an operator that is not available for the type of the
% operands
type_e_constant_expr_expected=04052_E_Constante uitdrukking verwacht
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_Operatie "$1" niet ondersteund voor types "$2" en "$3"
% The operation is not allowed for the supplied types
type_e_illegal_type_conversion=04054_E_Ongeldige type-conversie: "$1" naar "$2"
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
type_h_pointer_to_longint_conv_not_portable=04055_H_Conversies tussen ordinalen en pointers is niet overzetbaar naar andere platformen
% If you typecast a pointer to a longint (or vice-versa), this code will not compile
% on a machine using 64-bit for pointer storage.
type_w_pointer_to_longint_conv_not_portable=04056_W_Conversies tussen ordinalen en pointers is niet overzetbaar naar andere platformen
% If you typecast a pointer to a ordinal type of a different size (or vice-versa), this can
% cause problems. This is a warning to help finding the 32bit specific code where cardinal/longint is used
% to typecast pointers to ordinals. A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_Kan niet beslissen welke overladen functie te kiezen
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Ongeldige lus variabele
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
type_w_double_c_varargs=04059_W_Converteer een real constante waarde naar een double voor C compatbiliteit, doe een expliciete type cast om dit te voorkomen.
% In C, constant real values are double by default. For this reason, if you
% pass a constant real value to a variable argument part of a C function, FPC
% by default converts this constant to double as well. If you want to prevent
% this from happening, add an explicit typecast around the constant.
type_e_class_or_cominterface_type_expected=04060_E_Verwachtte een classe- of COM-interface, maar kreeg "$1"
#
# Symtable
#
# 05060 is the last used one
#
% Some operators like the AS operator are only appliable to classes or COM interfaces.
% \end{description}
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Symbool niet gevonden $1
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Interne fout in SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Dubbel symbool $1
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_Symbool reeds gedefinieerd in $1 op lijn $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Onbekende symbool $1
% The identifier encountered has not been declared, or is used outside the
% scope where it is defined.
sym_e_forward_not_resolved=05005_E_Voorwaartse declaratie niet opgelost $1
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Fout in type definitie
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_Forward declaratie niet opgelost $1
% A symbol was forward defined, but no declaration was encountered.
sym_e_only_static_in_static=05010_E_Alleen statische variabelen kunnen gebruikt worden in statische methoden
% A static method of an object can only access static variables.
sym_e_type_must_be_rec_or_object_or_class=05012_E_Record or object or class type expected
% The variable or expression isn't of the type \var{record} or \var{object} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_U kunt geen vertegenwoordiger van een klasse met abstracte methoden instantieren
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Label niet gedefineerd $1
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Label "$1" gebruikt maar niet gedefinieerd
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Label foutief gedefineerd
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO en LABEL zijn afgezet (gebruik -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Label niet gevonden
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_Symbool is geen label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_Label is al gedefineerd
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_Type declaratie van set elementen is foutief
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Voorwaartse definitie van klasse niet opgelost $1
% You declared a class, but you did not implement it.
sym_n_unit_not_used=05023_H_Unit $1 wordt niet gebruikt in $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Parameter niet gebruikt $1
% The identifier was declared (locally or globally) but
% was not used (locally or globally).
sym_n_local_identifier_not_used=05025_N_Lokale variabele niet gebruikt $1
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_Waarde parameter $1 is gedeclareerd maar niet gebruikt.
% The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_Lokale variabele $1 gedeclareerd maar niet gebruikt.
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Lokaal $1 $2 is niet gebruikt
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_Privaat veld $1.$2 wordt niet gebruikt
sym_n_private_identifier_only_set=05030_N_Privaat veld $1.$2 gedeclareerd maar niet gebruikt.
sym_n_private_method_not_used=05031_N_Private methode $1.$2 wordt nooit gebruikt.
sym_e_set_expected=05032_E_Set type verwacht
% The variable or expression is not of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_Resultaat van functie lijkt niet te zijn toegekend
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_Type $1 is not aligned correctly in current record for C
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Onbekend record lid $1
% The field doesn't exist in the record/object definition.
sym_w_uninitialized_local_variable=05036_W_Lokale variabele "$1" lijkt niet geinitialiseerd
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_uninitialized_variable=05037_W_okale variabele "$1" lijkt niet geinitialiseerd
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_Geen lid met de naam $1
% This error is generated when an identifier of a record,
% field, or method is accessed while it is not defined.
sym_h_param_list=05039_H_NDeclaratie "$1" gevonden
% You get this when you use the \var{-vh} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Data segment te groot (max. 2GB)
% You get this when you declare a data element whose size exceeds the
% prescribed limit (2 Gb on 80386+/68020+ processors)
sym_e_no_matching_implementation_found=05042_E_Geen overeenstemmende implementatie voor interface methode "$1" gevonden
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_Symbool "$1" is afgeraden
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Usage of this symbol
% should be avoided as much as possible.
sym_w_non_portable_symbol=05044_W_Symbool "$1" is niet overzetbaar naar andere platformen
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, usage
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_Symbool "$1" is niet geimplementeerd
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_Kan geen uniek type aanmaken van dit type
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}.
sym_h_uninitialized_local_variable=05057_H_ Lokale variabele "$1" lijkt niet geinitialiseerd
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_h_uninitialized_variable=05058_H_Lokale variabele "$1" lijkt niet geinitialiseerd
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_function_result_uninitialized=05059_W_Het resultaat van de Functie lijkt niet geinitialiseerd.
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_h_function_result_uninitialized=05060_H_Het resultaat van de functie lijkt niet geinitialiseerd te zijn.
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_w_identifier_only_read=05061_W_Variabele &quot;$1&quot; wordt gelezen, maar krijgt nergens een waarde
#
# Codegenerator
#
# 06040 is the last used one
#
% You have read the value of a variable, but nowhere assigned a value to
% it.
% \end{description}
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_Parameters gebruiken meer dan 64 kilobyte
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_file_must_call_by_reference=06012_E_File variabelen moeten altijd var parameters zijn
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_U kunt daar geen far pointer gebruiken
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_Procedures die als EXPORT gedeclareerd staan kunnen niet aangeroepen worden
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Mogelijke ongeldige aanroep van constructor of destructor
% The compiler detected that a constructor or destructor is called within a
% a method. This will probably lead to problems, since constructors / destructors
% require parameters on entry.
cg_n_inefficient_code=06017_N_Inefficiente code
% Your statement seems dubious to the compiler.
cg_w_unreachable_code=06018_W_Deze code wordt nooit uitgevoerd
% You specified a construct which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Abstracte methoden kunnen niet direct aangeroepen worden
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Register $1 gewicht $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_Stackframe wordt niet gemaakt
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=06031_E_Kan inline niet gebruiken voor object methoden
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Kan inline niet gebruiken voor procvar aanroepen
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_Geen code voor inline procedure opgeslagen
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Element nul van een ansi/wide- of longstring is niet toegankelijk, gebruik (set)length.
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such a string types
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructors of destructors kunnen niet aangeroepen worden binnen een 'with' clausule
% Inside a \var{with} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_Een message handler methode kan niet direct aangeroepen worden.
% A message method handler method cannot be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=06039_E_Sprong in of uit een exception blok
% It is not allowed to jump in or outside of an exception block like \var{try..finally..end;}:
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
cg_e_control_flow_outside_finally=06040_E_Control flow statements niet toegestaan in een finally blok
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
cg_w_parasize_too_big=06041_W_Grootte van de parameters is te groot voor bepaalde CPUs
% This indicates that you are declaring more than 64K of parameters, which
% might not be supported on other processor targets.
cg_w_localsize_too_big=06042_W_Grootte van lokale variabelen is te groot voor sommige CPUs
% This indicates that you are declaring more than 32K of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_Lokale variabelen te groot
% This indicates that you are declaring more than 32K of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK niet toegelaten
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE niet toegelaten
% You're trying to use \var{continue} outside a loop construction.
cg_f_unknown_compilerproc=06046_F_Ongekende schakel-aanduiding "$1". Kijk na of u de correcte looptijd bibliotheek gebruikt
# EndOfTeX
#
# Assembler reader
#
# 07097 is the last used one
#
% The compiler expects that the runtime library contains some subrountines. If you see this error
% and you didn't mess with the runtime library, it's very likely that the runtime library
% you're using doesn't match the used compiler. If you changed the runtime library this error means
% that you removed a subroutine which the compiler needs for internal use.
% \end{description}
asmr_d_start_reading=07000_DL_Begonnen met verwerken van $1 assembler
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_DL_Gedaan met verwerken van $1 assembler
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Symbool dat geen label is bevat "@"
% A identifier which isn't a label can't contain a @.
asmr_e_building_record_offset=07004_E_Fout bij berekenen offset in record
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET gebruikt zonder symbool
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE gebruikt zonder symbool
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_Kan hier geen lokale variabele of parameter gebruiken
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the frame pointer register so the
% address can't be obtained directly.
asmr_e_need_offset=07008_E_OFFSET moet hier gebruikt worden
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_Gebruik van $ is hier verplicht
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_Kan niet meerdere verplaatsbare symbolen gebruiken
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Een verplaatsbaar symbool kan enkel toegevoegd worden
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Ongeldige constante uitdrukking
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Verplaatsbaar symbool niet toegelaten
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Ongeldige geheugenlocatie schrijfwijze
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_$1 niet bereikbaar vanuit deze code
% You can not read directly the value of a local variable or parameter
% of a higher level procedure in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Lokale symbolen/labels niet toegestaan als referenties
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Ongeldig gebruik van basis en index register
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Mogelijke fout in behandeling velden van object.
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Ongeldige schaalfactor
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Meervoudig indexregister gebruik
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Ongeldig operandus type
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_Ongeldige tekenreeks als instructie operandus: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE en @DATA worden niet ondersteund
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Null label verwijzingen zijn niet toegelaten
asmr_e_expr_zero_divide=07025_E_Deling door nul
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_Ongeldige uitdrukking
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_Expliciete reeks genegeerd: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_Ongeldige symboolverwijzing
asmr_w_fwait_emu_prob=07029_W_Fwait kan emulatieproblemen met emu387 veroorzaken
asmr_w_fadd_to_faddp=07030_W_$1 zonder operand vertaald naar $1P
asmr_w_enter_not_supported_by_linux=07031_W_De ENTER instructie wordt niet ondersteund door de Linux kernel
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Een overroepen functie wordt aangeroepen vanuit assemblercode
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_Niet ondersteund symbool type voor operandus
asmr_e_constant_out_of_bounds=07034_E_Constante waarde valt buiten bereik
asmr_e_error_converting_decimal=07035_E_Fout bij omzetten van decimaal getal $1
% A constant decimal value does not have the correct syntax
asmr_e_error_converting_octal=07036_E_Fout bij omzetten van octaal getal $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Fout bij omzetten van binair getal $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Fout bij omzetten van hexadecimaal getal $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 vertaald naar $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 is geassocieerd met een overroepen functie
asmr_e_cannot_use_SELF_outside_a_method=07041_E_Kan SELF niet buiten een methode gebruiken
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_Kan OLDEBP niet buiten een genestelde procedure gebruiken
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Functies met een ongedefinieerde teruggeefwaarde kunnen geen waarde teruggeven
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG niet ondersteund
asmr_e_size_suffix_and_dest_dont_match=07045_E_Grootte-achtervoegsel en doel- of brongrootte komen niet overeen
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_Grootte-achtervoegsel en doel- of brongrootte komen niet overeen
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Assembler taalfout
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_Ongeldige combinatie van instructie en operandi
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Assembler taalfout in operandum
asmr_e_syn_constant=07050_E_Assembler taalfout in constante
asmr_e_invalid_string_expression=07051_E_Ongeldige String uitdrukking
asmr_w_const32bit_for_address=07052_W_ Constante met symbool $1 voor adres dat zich niet op een pointer bevindt
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Ongekende opcode $1
% This opcode is not known
asmr_e_invalid_or_missing_opcode=07054_E_Ongeldige of ontbrekende instructie
asmr_e_invalid_prefix_and_opcode=07055_E_Ongeldige combinatie van voorvoegsel en instructie: $1
asmr_e_invalid_override_and_opcode=07056_E_Ongeldige combinatie van segment overroeping en instructie: $1
asmr_e_too_many_operands=07057_E_Te veel operandi op een lijn
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR genegeerd
asmr_w_far_ignored=07059_W_FAR genegeerd
asmr_e_dup_local_sym=07060_E_Dubbel lokaal symbool $1
asmr_e_unknown_local_sym=07061_E_Niet gedefinieerd lokaal symbool $1
asmr_e_unknown_label_identifier=07062_E_Onbekende labelnaam $1
asmr_e_invalid_register=07063_E_Ongeldige register naam
% There is an unknown register name used as operand.
asmr_e_invalid_fpu_register=07064_E_Ongeldig vlottende-komma registernaam
% There is an unknown register name used as operand.
asmr_w_modulo_not_supported=07066_W_Modulo wordt niet ondersteund
asmr_e_invalid_float_const=07067_E_Ongeldige vlottende-komma constante $1
% The floating point constant declared in an assembler block is
% invalid.
asmr_e_invalid_float_expr=07068_E_Ongeldige vlottende-komma uitdrukking
% The floating point expression declared in an assembler block is
% invalid.
asmr_e_wrong_sym_type=07069_E_Verkeerd symbool type
asmr_e_cannot_index_relative_var=07070_E_Een lokale variabele of parameter kan niet geindedxeerd worden met een register
% Trying to index using a base register a symbol which is already relative
% to a register. This is not possible, and will probably lead to crashes.
asmr_e_invalid_seg_override=07071_E_Ongeldige segment overroeping
asmr_w_id_supposed_external=07072_W_Symbool $1 wordt verondersteld extern te zijn
% There is a reference to an undefined symbol. This will not result
% in an error, since the symbol might be external, but may cause
% problems at link time if the symbol is not defined anywhere.
asmr_e_string_not_allowed_as_const=07073_E_Strings zijn niet toegelaten als constante
% Character strings are not allowed as constants.
asmr_e_no_var_type_specified=07074_Geen type van de variabele gegeven
% The syntax expects a type idenfitifer after the dot, but
% none was found.
asmr_w_assembler_code_not_returned_to_text=07075_E_De assembler code stopt niet in een '.text' gedeelte
% There was a directive in the assembler block to change sections,
% but there is a missing return to the text section at the end
% of the assembler block. This might cause errors during link time.
asmr_e_not_directive_or_local_symbol=07076_E_Geen aanwijzing of lokaal symbool $1
% This symbol is unknown.
asmr_w_using_defined_as_local=07077_E_Een gedfinieerde naam wordt gebruikt als assembler label
asmr_e_dollar_without_identifier=07078_E_Dollar teken gebruikt zonder identifier
% A constant expression has an identifier which does not start with
% the $ symbol.
asmr_w_32bit_const_for_address=07079_W_32bit constante aangemaakt voor adres
% A constant was used as an address. This is probably an error,
% since using absolute addresses will probably not work.
asmr_n_align_is_target_specific=07080_N_.align is doel specifiek, gebruik .balign of .p2align
% Using the .align directive is platform specific, and its meaning will vary
% from one platform to another.
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Kan velden niet direct gebruiken voor parameters.
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_Kan velden van objecten/klassen niet direct aanspreken
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_Geen grootte aangegegeven en kan de grootte van de operanden niet bepalen
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=07084_E_Kan RESULT aanduiding niet gebruiken in deze functie
% Some functions which return complex types cannot use the \var{result}
% keyword.
asmr_w_adding_explicit_args_fXX=07086_W_"$1" zonder operand vertaald naar "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" vertaald naar "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)"  vertaald naar  "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Karakter < hier niet toegestaan
% The shift operator requires the << characters. Only one
% of those characters was found.
asmr_e_invalid_char_greater=07090_E_Karakter > hier niet toegestaan
% The shift operator requires the >> characters. Only one
% of those characters was found.
asmr_w_align_not_supported=07093_W_ALIGN niet ondersteund
asmr_e_no_inc_and_dec_together=07094_E_INC en DEC kunnen niet samengaan
% Trying to use an increment and a decrement within the same
% opcode on the 680x0. This is impossible.
asmr_e_invalid_reg_list_in_movem=07095_E_Ongeldige register lijst voor movem
% Trying to use the \var{movem} opcode with invalid registers
% to save or restore.
asmr_e_invalid_reg_list_for_opcode=07096_E_Ongeldige register lijst voor opcode
asmr_e_higher_cpu_mode_required=07097_E_hogere CPU mode vereist ($1)
% Trying to use an instruction which is not supported in the current
% cpu mode. Use a higher cpu generation to be able to use this
% opcode in your assembler block
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_Grootte niet opgegeven en kan de grootte van de operanden niet bepalen. De standaard DWORD wordt gebruikt
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Syntax fout bij het bepalen van een shift operand
#
# Assembler/binary writers
#
# 08018 is the last used one
#
% ARM only; ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
asmw_f_too_many_asm_files=08000_F_Te veel assemblerbestanden
% With smartlinking enabled, there are too many assembler
% files generated. Disable smartlinking.
asmw_f_assembler_output_not_supported=08001_F_De geselecteerde assembler modus wordt hier niet ondersteund
asmw_f_comp_not_supported=08002_F_Het COMP type wordt hier niet ondersteund
asmw_f_direct_not_supported=08003_F_Directe assembler modus wordt niet ondersteund door de binaire assemblerschrijver
% Direct assembler mode is not supported for binary writers.
asmw_e_alloc_data_only_in_bss=08004_E_Ruimte voor gegevens reserveren mag enkel in een bss gedeelte
asmw_f_no_binary_writer_selected=08005_F_Binaire assemblerschrijver geselecteerd
asmw_e_opcode_not_in_table=08006_E_Asm: Opcode $1 not gevonden in interne tabel
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 Ongeldige combinatie van instructie en operandi
asmw_e_16bit_not_supported=08008_E_Asm: 16 Bit geheugenlocaties worden niet ondersteund
asmw_e_invalid_effective_address=08009_E_Asm: Ongeldig direct adres
asmw_e_immediate_or_reference_expected=08010_E_Asm: Constante of geheugenadres verwacht
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 waarde overschrijdt grenzen van $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Korte spronginstructie gaat te ver $1
asmw_e_undefined_label=08013_E_Asm: Ongedefinieerd label $1
asmw_e_comp_not_supported=08014_E_Asm: COMP type niet ondersteund voor dit platfirl
asmw_e_extended_not_supported=08015_E_Asm: EXTENDED type niet ondersteund voor dit platform
asmw_e_duplicate_label=08016_E_Asm: Label dubbel gebruikt: $1
asmw_e_redefined_label=08017_E_Asm: Label opnieuw gedefinieerd: $1
asmw_e_first_defined_label=08018_E_Asm: eerste definitie hier
asmw_e_invalid_register=08019_E_Asm: ongeldig register $1
asmw_e_16bit_32bit_not_supported=08020_E_Asm: 16 or 32 Bit references not supported
asmw_e_64bit_not_supported=08021_E_Asm: 64 Bit operands not supported
#
# Executing linker/assembler
#
# 09034 is the last used one
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Bronbesturingssysteem geherdefinieerd
exec_i_assembling_pipe=09001_I_Assembleren (pijp) van $1
exec_d_cant_create_asmfile=09002_E_Kan geen assembler bestand $1 niet maken
% The mentioned file can't be created. Check if you have got
% access permissions to create this file
exec_e_cant_create_objectfile=09003_E_Kan geen object bestand openen: $1
% The mentioned file can't be created. Check if you've
% got access permissions to create this file
exec_e_cant_create_archivefile=09004_E_kan geen archief bestand openen: $1
% The mentioned file can't be created. Check if you've
% access permissions to create this file
exec_e_assembler_not_found=09005_E_Assembler $1 niet gevonden. Schakel over op externe assemblage
exec_t_using_assembler=09006_T_Gebruikte assembler: $1
exec_e_error_while_assembling=09007_E_Fout bij het assembleren, exit code $1
% There was an error while assembling the file using an external assembler.
% Consult the documentation of the assembler tool to find out more information
% on this error.
exec_e_cant_call_assembler=09008_E_Kan de assembler niet oproepen, fout $1. Schakel over op externe assemblage
exec_i_assembling=09009_I_Assembleren van $1
exec_i_assembling_smart=09010_I_Assembleren slim-link $1
exec_w_objfile_not_found=09011_W_Bestand $1 niet gevonden, linken kan foutlopen !
% One of the object file is missing, and linking will probably fail.
% Check your paths.
exec_w_libfile_not_found=09012_W_Bibliotheek $1 niet gevonden, linken kan foutlopen !
% One of the library file is missing, and linking will probably fail.
% Check your paths.
exec_e_error_while_linking=09013_E_Fout bij het linken
% Generic error while linking.
exec_e_cant_call_linker=09014_E_Kan de linker niet oproepen, schakel over op extern linken
exec_i_linking=09015_I_Linken van $1
exec_e_util_not_found=09016_E_Gereedschap $1 niet gevonden, schakel over op extern linken
exec_t_using_util=09017_T_Gebruik tool $1
exec_e_exe_not_supported=09018_E_Creatie van executable bestanden niet ondersteund.
exec_e_dll_not_supported=09019_E_Dynamische bibliotheken niet ondersteund
exec_i_closing_script=09020_I_Afsluiten batch $1
exec_e_res_not_found=09021_E_Resource compiler niet gevonde, schakel over op externe modus
exec_i_compilingresource=09022_I_Compileer resource $1
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_Unit $1 kan niet statisch gelinkt worden. Schakel over op slim linken
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_Unit $1 kan niet slim gelinkt worden. Schakel over op statisch linken
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_Unit $1 kan niet gedeeld gelinkt worden, Schakel over op statisch linken
exec_e_unit_not_smart_or_static_linkable=09026_E_Unit $1 kan niet slim of statisch gelinkt worden
exec_e_unit_not_shared_or_static_linkable=09027_E_Unit $1 kan niet gedeeld of statisch gelinkt worden
exec_d_resbin_params=09028_D_Oproep resource compiler "$1" met "$2" als commando-lijn opties
# EndOfTeX
#
# Executable information
#
%\end{description}
execinfo_f_cant_process_executable=09128_F_Kan applicatie niet nabehandelen $1
execinfo_f_cant_open_executable=09129_F_Kan applicatie niet openen $1
execinfo_x_codesize=09130_X_Grootte van de code: $1 bytes
execinfo_x_initdatasize=09131_X_Grootte van geinitialiseerde data: $1 bytes
execinfo_x_uninitdatasize=09132_X_Grootte van niet-geinitialiseerde data: $1 bytes
execinfo_x_stackreserve=09133_X_Gereserveerde stackruimte: $1 bytes
execinfo_x_stackcommit=09134_X_Toegewezen stackruimte: $1 bytes
#
# Unit loading
#
# 10041 is the last used one
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these messages are
% informational messages.
% \begin{description}
unit_t_unitsearch=10000_T_Unit zoeken: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_PPU Laden $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_PPU Naam: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_PPU Vlaggen: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_PPU CRC: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_PPU Tijd: $1
% When you use the \var{-vu} flag, the time the unit was compiled is shown.
unit_u_ppu_file_too_short=10006_U_PPU bestand te kort
% The ppufile is too short, not all declarations are present.
unit_u_ppu_invalid_header=10007_U_PPU Ongeldige kop (geen PPU aan het begin)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_PPU Ongeldige versie $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_PPU is gecompileerd voor een andere processor
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=10010_U_PPU is gecompileerd voor een ander doelsysteem
% This unit file was compiled for a different target, and
% cannot be read
unit_u_ppu_source=10011_U_PPU bron: $1
% When you use the \var{-vu} flag, the unit source file name is shown.
unit_u_ppu_write=10012_U_Schrijven van $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_Kan PPU bestand niet schrijven
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_Lezen van PPU bestand
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=10015_F_Onverwacht einde van PPU-bestand
% Unexpected end of file. This may mean that the PPU file is
% corrupted.
unit_f_ppu_invalid_entry=10016_F_Ongeldig PPU bestand post: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_PPU Dbx aantal probleem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Ongeldige unitnaam: $1
% The name of the unit does not match the file name.
unit_f_too_much_units=10019_F_Te veel units
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{fmodule.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Uses statement veroorzaakt vicieuze cirkel tussen $1 en $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_Kan unit $1 niet compileren, geen broncode beschikbaar
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_Kan unit $1 niet vinden
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your configuration file for the unit paths
unit_w_unit_name_error=10023_W_Unit $1 niet gevonden maar $2 bestaat
unit_f_unit_name_error=10024_F_Unit $1 gezocht maar $2 gevonden
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_De systeemunit compileren vereist de -Us schakelaar
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_Er waren fouten $1 bij het compileren van een module, compilatie gestopt.
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Laden van $1 ($2) unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_Hercompileer $1, checksum voor $2 veranderd
unit_u_recompile_source_found_alone=10029_U_herkompileer $1, alleen bron gevonden
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Hercompileer unit, statische bibliotheek ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Hercompileer unit, dynamische bibliotheek ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Hercompileer unit, object en asm zijn ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the assembler or
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Hercompileer unit, object is ouder dan assembler
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_Interface van $1 wordt geparsed
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_parsing_implementation=10035_U_Implementatie van $1 wordt geparsed
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_Tweede maal unit $1 laden
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_PPU Controleren van bestand $1 tijd $2
### The following two error msgs is currently disabled.
#unit_h_cond_not_set_in_last_compile=10038_H_Conditional $1 was not set at startup in last compilation of $2
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that currently is defined, but was not used the last
#% time the unit was compiled.
#unit_h_cond_set_in_last_compile=10039_H_Conditional $1 was set at startup in last compilation of $2
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that was used the last time the unit was compiled, but
#% the conditional is currently not defined.
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
unit_w_cant_compile_unit_with_changed_incfile=10040_W_Kan unit $1 niet hercompileren, maar vond gewijzigde include bestanden
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_h_source_modified=10041_H_Bestand $1 is nieuwer dan release  PPU bestand $2
% A modified source file for a unit was found that was compiled with the
% release flag (-Ur). The unit will not implicitly be recompiled because
% this release flag is set.
unit_u_ppu_invalid_fpumode=10042_U_Er wordt een unit gebruikt die niet met de correcte FPU modus is gecompileerd
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_Laden van interface units voor unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the interface part of the unit.
unit_u_loading_implementation_units=10044_U_Laden van implementatie units voor unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the implementation part of the unit.
unit_u_interface_crc_changed=10045_U_Interface CRC voor unit $1 is gewijzigd
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated for the interface has been changed after the implementation
% has been parsed.
unit_u_implementation_crc_changed=10046_U_Implementatie CRC voor unit $1 is gewijzigd
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated has been changed after the implementation
% has been parsed.
unit_u_finished_compiling=10047_U_Klaar met compileren van unit $1
% When you use the \var{-vu} flag, the compiler warns that it
% has finished compiling the unit.
unit_u_add_depend_to=10048_U_Afhankelijkheid van $1 toegevoegd aan $2
% When you use the \var{-vu} flag, the compiler warns that it
% has added a dependency between the two units.
unit_u_no_reload_is_caller=10049_U_Niet herladen, oproeper is: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is the unit that wants
% to load this unit
unit_u_no_reload_in_second_compile=10050_U_Niet herladen, reads in tweede compilatie: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is already in a second recompile
unit_u_flag_for_reload=10051_U_Vlaggen voor herladen: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to reload the unit
unit_u_forced_reload=10052_U_Verplicht herladen
% When you use the \var{-vu} flag, the compiler warns that it
% has is reloading the unit because it was required
unit_u_previous_state=10053_U_Vorige toestand van $1: $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit
unit_u_second_compile_unit=10054_U_$1 wordt reeds gecompileerd, tweede compile wordt gezet
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_loading_unit=10055_U_Laden unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the unit.
unit_u_finished_loading_unit=10056_U_Klaar met laden van unit $1
% When you use the \var{-vu} flag, the compiler warns that it finished
% loading the unit.
unit_u_registering_new_unit=10057_U_Nieuwe unit $1 wordt geregistreerd
% When you use the \var{-vu} flag, the compiler warns that it has
% found a new unit and registers it in the internal lists.
unit_u_reresolving_unit=10058_U_Opnieuw oplossen van unit $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to recalculate the internal data of the unit
unit_u_skipping_reresolving_unit=10059_U_Oplossen van unit $1 wordt overgeslagen, gebruikte units worden nog geladen
# EndOfTeX
#
#  Options
#
# 11039 is the last used one
#
% When you use the \var{-vu} flag, the compiler warns that it
% skips to recalculate the internal data of the unit because there
% is no data to recalculate
% \end{description}
option_usage=11000_O_$1 [opties] <bestand> [opties]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Maar een (1) bronbestand wordt ondersteunt
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_Een DEF file kan alleen worden gemaakt voor OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_Geneste optie bestanden niet ondersteund
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_Geen bronbestand op de commando-lijn
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_geen opties in configuratie bestand $1
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Ongeldige parameter: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? druk hulp af
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Te veel geneste instellingsbestanden
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Kan bestand $1 niet openen
% The option file cannot be found.
option_reading_further_from=11010_D_Meer instellingen worden gelezen van $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Doel is reeds ingesteld op: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Shared bibliotheken niet onderteund onder DOS, val terug op statisch
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_Te veel IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_Te veel ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_Open voorwaardelijke aan eind van bestand
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_Debug informatie generatie wordt niet ondersteund door deze applicatie
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Hercompileer de compiler-applicatie met -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_W_U gebruikt een in onbruik geraakte schakeloptie $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_W_U gebruikt een in onbruik geraakte schakeloptie $1, gebruik $2 a.u.b.
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_De assembler wordt veranderd in de standaard assemblerschrijver
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_Geselecteerde assembler "$1" is niet verenigbaar met "$2"
option_asm_forced=11022_W_Gebruik van "$1" assembler opgelegd
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_Opties worden gelezen van bestand $1
% Options are also read from this file
option_using_env=11027_T_Opties worden gelezen van omgevingsvariabelen $1
% Options are also read from this environment string
option_handling_option=11028_D_Behandelen van optie "$1"
% Debug info that an option is found and will be handled
option_help_press_enter=11029__***Druk op de ENTER toets***
option_start_reading_configfile=11030_H_Start lezen van configuratie bestand $1
% Starting of config file parsing.
option_end_reading_configfile=11031_H_Einde van lezen van configuratie bestand $1
% End of config file parsing.
option_interpreting_option=11032_D_Optie $1 wordt behandeld
option_interpreting_firstpass_option=11036_D_Behandel prioritaire optie "$1"
option_interpreting_file_option=11033_D_Behandel bestandsoptie "$1"
option_read_config_file=11034_D_Lezen configuratie bestand "$1"
option_found_file=11035_D_Bronbestand "$1" gevonden
% Additional infos about options, displayed
% when you have debug option turned on.
option_code_page_not_available=11039_E_Ongekende coderingspagina
option_config_is_dir=11040_F_Config bestand $1 is een directorie
# EndOfTeX
#
# Logo (option -l)
#
% Directories can not be used as configuration files.
%\end{description}
option_logo=11023_[
Free Pascal Compiler versie $FPCFULLVERSION [$FPCDATE] voor $FPCTARGET
Copyright (c) 1993-2015 door Florian Klaempfl en anderen
]
#
# Info (option -i)
#
option_info=11024_[
Free Pascal Compiler versie $FPCVERSION
Compiler datum:       $FPCDATE
Compiler doelsysteem: $FPCTARGET
Dit programma wordt verspreid onder de GNU General Public Licence
Voor meer informatie, lees COPYING.v2. Hiervan is helaas geen
Nederlandse vertaling beschikbaar.

                http://bugs.freepascal.org
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
**0*_+ schakelt optie aan, - af
**1a_De compiler verwijdert gegenereerd assemblerbestand niet
**2al_Toon broncode in assemblerbestand
**2ar_Toon registerreservering/-vrijgave in assemblerbestand
**2at_Toon reservering/vrijgave van tijdelijke ruimte in assemblerbestand
**1b_genereer browserinfo
**2bl_genereer info voor lokale symbolen info
**1B_Bouw alle modules
**1C_Codegeneratie opties
3*2CD_Creeer dynamische bibliotheek
**2Ch<n>_<n> bytes heap (tussen 1023 en 67107840)
**2Ci_IO-checking
**2Cn_Laat linken achterwege
**2Co_Controleer overflow van integeroperaties
**2Cr_Bereikcontrole (range check)
**2Cs<n>_Stel stackgrootte in op <n>
**2Ct_Stackcontrole
3*2CS_Cre‰er dynamische bibliotheek
3*2Cx_Gebruik slim linken
**1d<x>_definieer het symbool <x>
*O1D_genereer een DEF bestand
*O2Dd<x>_Zet beschrijving op <x>
*O2Dw_PM applicatie
**1e<x>_Zet pad naar applicaties
**1E_Zelfde als -Cn
**1F_Zet bestandsnamen en paden
**2Fe<x>_Zend foutboodschappen naar bestand <x>
*L2Fg<x>_Zelfde als -Fl
**2Fi<x>_Voegt <x> toe aan invoegpad
**2Fl<x>_Voegt <x> toe aan bibliotheek-pad
*L2FL<x>_Gebruik <x> als dynamische linker
**2Fo<x>_Voeg <x> toe aan object-pad
**2Fr<x>_Lees foutmeldingen uit bestand <x>
**2Fu<x>_Voeg <x> toe aan unit pad
**2FU<x>_Schrijf units in folder <x>, (primeert op -FE)
*g1g_genereer debuginformatie
*g2gg_gebruik gsym
*g2gd_gebruik dbx
*g2gh_laad heaptrc unit automatisch
*g2gl_Gebruik regelinformatie unit voor meer informatie in backtraces
*g2gc_genereer controles voor pointers
**1i_informatie
**2iD_Toon compilerdatum
**2iV_Toon compilerversie
**2iSO_Toon compiler-OS
**2iSP_Toon compilerprocessor
**2iTO_Toon doel-OS
**2iTP_Toon doelprocessor
**1I<x>_Voeg <x> toe aan invoegpad
**1k<x>_Geef <x> door aan de linker
**1l_Druk logo af
**1n_Standaard configuratie bestand niet lezen
**1o<x>_Stel de naam van het applicatiebestand in op <x>
**1pg_genereer profile code voor gprof
*L1P_Gebruik pipes in plaats van tijdelijke assembler bestanden
**1S_Syntaxisinstellingen
**2S2_Schakel Delphi-2-uitbreidingen in
**2Sc_Ondersteun operatoren als in C (*=,+=,/= en -=)
**2sa_Voeg assertion code toe
**2S2_Tracht Delphi compatibel te zijn
**2Se<x>_Compiler stopt na <x> fouten (standaard 1)
**2Sg_Laat LABEL en GOTO toe
**2Sh_Gebruik ansistrings
**2Si_Ondersteun INLINE
**2Sm_Ondersteun macros zoals in C (globaal)
**2So_Probeer TP/BP-7.0-compatibel te zijn
**2Sp_Probeer GNU-Pascal-compatibel te zijn
**2Ss_Constructor naam moet init zijn (destructor moet done zijn)
**1s_Roep assembler en linker niet op (slechts met -a)
**1sh_Genereer een script om op het hostplatform te linken
**1sr_Voer geen registertoewijzing uit (optimalisaties worden uitgeschakeld)
**1st_Genereer een script om op het doelplatform te linken
**1u<x>_Verwijdert symbooldefinitie <x>
**1U_unit opties
**2Un_Unit naam niet nagaan
**2Us_Compileer een systeemunit
**1v<x>_Wees uitvoerig. <x> is een combinatie van volgende letters:
**2*_e : Toon fouten (standaard)     d : Toon debug informatie
**2*_w : Toon waarschuwingen         u : Toon gebruikte bestanden
**2*_n : Toon notas                  t : Toon geteste bestanden
**2*_h : Toon hints                  m : Toon gedefinieerde macros
**2*_i : Toon algemene informatie    p : Toon gecompileerde routines
**2*_l : Toon regelnummers           c : Toon voorwaardelijken
**2*_a : Toon alles                  0 : Toon alleen fouten
**2*_b : Toon alle procedures        r : Rhide-/GCC-compatibiliteitsmodus
**2*_    declaraties indien een     x : Executable informatie (alleen Win32)
**2*_    fout optreedt
**1X_applicatie instellingen
*L2Xc_link met de C bibliotheek
**2Xs_verwijder alle symbolen uit applicatie
**2XD_link met dynamische bibliotheken (definieert FPC_LINK_DYNAMIC)
**2XS_link met statische bibliotheken (definieert FPC_LINK_STATIC)
**2XS_link slim (definieert FPC_LINK_STATIC_SMART)
**0*_Processor specifieke instellingen:
3*1A_Output formaat
3*2Aas_assembleer met GNU AS
3*2Aasaout_assembleer met GNU AS voor aout (Go32v1)
3*2Anasmcoff_Coff (Go32v2) bestand met Nasm
3*2Anasmelf_elf32 (linux) bestand met Nasm
3*2Anasmobj_obj bestand met Nasm
3*2Amasm_obj bestand met Masm (Microsoft)
3*2Atasm_obj bestand met Tasm (Borland)
3*2Acoff_coff (Go32v2) met interne assembler
3*2Apecoff_pecoff (Win32) met interne assembler
3*1R<x>_Geef de grammatica voor de assemblerlezer op:
3*2Ratt_Lees assembler in AT&T-stijl
3*2Rintel_Lees assembler in Intelstijl
3*2Rdirect_Kopieer assemblercode rechtstreeks naar assemblerbestand
**1O<x>_optimalisaties
**2O1_Niveau 1 optimalisaties (snel en debug-vriendelijk )
**2O2_Niveau 2 optimalisaties (-O1 + snelle optimalisaties)
**2O3_Niveau 3 optimalisaties (-O2 + trage optimalisaties)
**2Oa<x>=<y>_Stel alignering in
**2Oo[NO]<x>_Schakel optimalisatie x aan of uit, zie fpc -i voor mogelijke waarden
**2Op<x>_Stel doel-cpu in voor optimalisaties, zie fpc -i voor mogelijke waarden
**2Os_Genereer kleinere code
6*1T<x>_Doel besturingssysteem:
3*2TGO32V2_versie 2 of DJ Delorie DOS-extender
3*2TLINUX_Linux
3*2TOS2_OS/2 / eComStation
3*2TWIN32_Windows 32-bit
3*1W<x>_Opties voor het Win32-doelplatform
3*2WB<x>_Stel Image base in op (hexadecimale) waarde <x>
3*2WC_Maak een console applicatie
3*2WD_Gebruik DEFFILE om functies van bibliotheek of applicatie te exporteren
3*2WG_Maak een grafische applicatie
3*2WN_Genereer geen relocatiecode (nodig voor debuggen)
3*2WR_Genereer relocatiecode
6*1A_output formaat
6*2Aas_Unix o-bestand met GNU AS
6*2Agas_GNU Motorola-assembler
6*2Amit_MIT Syntax (oude GAS)
6*2Amot_Standaard Motorola-assembler
6*1O_optimalisaties
6*2Oa_Gebruik de optimalisaties
6*2Og_Genereer kleinere code
6*2OG_Genereer snellere code (standaard)
6*2Ox_Optimizeer maximaal (nog steeds BUGGY!!!)
6*2O2_Stel doelprocessor in op MC68020+
6*1R<x>_assembler reading style:
6*2RMOT_read motorola style assembler
6*1T<x>_Target operating system:
6*2TAMIGA_Commodore Amiga
6*2TATARI_Atari ST/STe/TT
6*2TMACOS_Macintosh m68k
6*2TLINUX_Linux-68k
**1*_
**1?_Toont deze hulp
**1h_Toont deze hulp zonder wachten
]
