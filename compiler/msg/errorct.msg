#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1993-2005 by the Free Pascal Development team
#
#   Catalan Language File for Free Pascal
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
# 01017 is the last one used
#
# BeginOfTeX
% \section{General compiler messages}
% This section gives the compiler messages which are not fatal, but which
% display useful information. The number of such messages can be
% controlled with the various verbosity level \var{-v} switches.
% \begin{description}
general_t_compilername=01000_T_Compilador: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=01001_D_SO d'origen: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=01002_I_SO de destinació: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Utilitzant la trajectòria per executables: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=01004_T_Utilitzant la trajectòria per a unitats: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Utilitzant la trajectòria per a inclosos: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Utilitzant la trajectòria per a biblioteques : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Utilitzant la trajectòria per a objectes: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 Línies compilades, $2 seg
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_No queda memòria disponible
% The compiler doesn't have enough memory to compile your program.
% There are several remedies for this:
% \begin{itemize}
% \item If you're using the build option of the compiler, try compiling the
% different units manually.
% \item If you're compiling a huge program, split it up in units, and compile
% these separately.
% \item If the previous two don't work, recompile the compiler with a bigger
% heap (you can use the \var{-Ch} option for this, \seeo{Ch})
% \end{itemize}
general_i_writingresourcefile=01010_I_Escrivint fitxer de taula de recursos: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Error escrivint fitxer de taula de recursos: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
general_i_fatal=01012_I_Fatal:
% Prefix for Fatal Errors
general_i_error=01013_I_Error:
% Prefix for Errors
general_i_warning=01014_I_Avís:
% Prefix for Warnings
general_i_note=01015_I_Nota:
% Prefix for Notes
general_i_hint=01016_I_Suggeriment:
% Prefix for Hints
general_e_path_does_not_exist=01017_E_La trajectòria "$1" no existeix
% The specified path does not exist
general_e_compilation_aborted=01018_E_Compilació avortada
% \end{description}
#
# Scanner
#
# 02066 is the last used one
#
% \section{Scanner messages.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=02000_F_Inesperat fi de fitxer
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement.
% This happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment was not closed.
% \end{itemize}
scan_f_string_exceeds_line=02001_F_La cadena excedeix la línia
% There is a missing closing 'in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_Caràcter il·legal "$1" ($2)
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Error de sintaxi, esperava "$1" però he trobat "$2"
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_TL_Començant a llegir el fitxer inclòs $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_He trobat nivell de comentari $1
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_ignored_switch=02008_N_Commutador de compilador ignorat "$1"
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Commutador de compilador il·legal $1
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% does not recognise.
scan_w_switch_is_global=02010_W_Falta commutador d'efecte global
% The compiler switch is misplaced, and should be located at
% the start of the unit or program
scan_e_illegal_char_const=02011_E_Constant de caràcter il·legal
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range.
scan_f_cannot_open_input=02012_F_No es pot obrir el fitxer "$1"
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_No es pot obrir el fitxer inclòs "$1"
% \fpc cannot find the source file you specified in a \var{\{\$include..\}}
% statement.
scan_w_only_pack_records=02015_W_Els camps dels Registres poden ser alineats només a 1,2,4 o 16 octets
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=02016_W_Els enumerats només poden ser guardats en 1,2 o 4 octets
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=02017_E_Esperava $ENDIF per $1, $2 definit en $3 a la línia $4
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Error de sintaxi mentre es processava una expressió de compilació condicional
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setcd
% compiler directives.
scan_e_error_in_preproc_expr=02019_E_Error avaluant una expressió de compilació condicional
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc
% compiler directives.
scan_w_macro_cut_after_255_chars=02020_W_Els continguts de la macro estan limitats a una longitud de 255 caràcters
% The contents of macros cannot be longer than 255 characters.
scan_e_endif_without_if=02021_E_ENDIF sense IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_L'usuari ha definit: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_L'usuari ha definit: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_L'usuari ha definit: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_L'usuari ha definit: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_L'usuari ha definit: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_L'usuari ha definit: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_Paraula clau re-definida com a macro no té efecte
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_Desbordament de la memòria intermèdia per macroinstrucció mentre es llegia o s'expandia
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=02030_W_L'expansió de macroinstruccions excedeix una profunditat de 16.
% When expanding a macro, macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_w_wrong_styled_switch=02031_W_Els commutadors de compilador no estan permesos en comentaris de l'estil //
% Compiler switches should be in normal pascal style comments
scan_d_handling_switch=02032_DL_Manejant el commutador "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_CL_ENDIF $1 trobat
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_CL_IFDEF $1 trobat, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_CL_IFOPT $1 trobat, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_CL_IF $1 trobat, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_CL_IFNDEF $1 trobat, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_CL_ELSE $1 trobat, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_CL_Ometent fins...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_Prem <retorn> per a continuar
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Commutador "$1" no suportat
% When warnings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_Directiva il·legal del compilador "$1"
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_TL_Torno a $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Tipus d'aplicació no suportada: "$1"
% You get this warning, ff you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_APPTYPE no és suportat pel SO de destinació
% The \var{\{\$APPTYPE\}} directive is supported by certain operanting
% systems only.
scan_w_description_not_support=02046_W_DESCRIPTION no és suportat pel SO de destinació
% The \var{\{\$DESCRIPTION\}} directive is not supported on this target OS
scan_n_version_not_support=02047_N_VERSION no és suportat per SO de destinació
% The \var{\{\$VERSION\}} directive is not supported on this target OS.
scan_n_only_exe_version=02048_N_VERSION només per executables o DLLs
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_Format erroni per directiva VERSION "$1"
% The \var{\{\$VERSION\}} directive format is major_version.minor_version
% where major_version and minor_version are words.
scan_e_unsupported_asmmode_specifier=02050_E_L'estil d'assemblador especificat "$1" no és vàlida
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_El commutador de lector de ASM no és permès dins de blocs ASM "$1" essent efectiu només pel següent
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statements only.
scan_e_wrong_switch_toggle=02052_E_Modificador de commutador erroni, utilitza ON/OFF o +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_Els fitxers de recursos no son suportats per aquesta destinació
% The target you are compiling for doesn't support resource files.
scan_w_include_env_not_found=02054_W_No he trobat la variable d'entorn "$1"
% The included environment variable can't be found in the environment, it will
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Valor il·legal per límit de registre FPU
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Només un fitxer origen és suportat per aquesta destinació
% The target you are compiling for supports only one resource file.
% The first resource file found is used, the others are discarded.
scan_w_macro_support_turned_off=02057_W_El suport per a macroinstruccions ha estat aturat
% A macro declaration has been found, but macro support is currently off,
% so the declaration will be ignored. To turn macro support on, compile with
% -Sm on the commandline or add {\$MACRO ON\} in the source
scan_e_invalid_interface_type=02058_E_S'ha especificat tipus d'interfície no vàlida. Les interfícies vàlides son : COM, CORBA o DEFAULT.
% The interface type that was specified is not supported
scan_w_appid_not_support=02059_W_APPID només és suportat per PalmOS
% The \var{\{\$APPID}} directive is only supported for the PalmOS target.
scan_w_appname_not_support=02060_W_APPNAME només és suportat per PalmOS
% The \var{\{\$APPNAME\}} directive is only supported for the PalmOS target
scan_e_string_exceeds_255_chars=02061_E_Les constants de cadena no poden excedir els 255 caràcters
% A single string contant can contain at most 255 chars. Try splitting up the
% string in multiple smaller parts and concatenate them with a + operator.
scan_f_include_deep_ten=02062_F_Incloure fitxers inclosos supera a 16 niats
% When including include files the files have been nested to a level of 16
% The compiler will expand no further, since this may be a sign that
% recursion is used
scan_e_too_many_push=02063_E_Massa nivells de PUSH
% A maximum of 20 levels is allowed. This error occur only in mode MacPas
scan_e_too_many_pop=02064_E_POP sense PUSH anterior
% This error occur only in mode MacPas
scan_e_error_macro_lacks_value=02065_E_La macroinstrucció "$1" no pot ser avaluada
% Thus the conditional compiling expression cannot be evaluated.
scan_e_wrong_switch_toggle_default=02066_E_Posició de commutador errònia, utilitza ON/OFF/DEFAULT o +/-/*
% You need to use ON or OFF or DEFAULT or a + or - or * to toggle the switch
scan_e_mode_switch_not_allowed=02067_E_El commutador de mode "$1" no és permès aquí
% A mode switch has already been encountered, or, in case of option -Mmacpas,
% a mode switch occur after UNIT 
% \end{description}
#
# Parser
#
# 03211 is the last used one
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
% \section{Parser messages}
parser_e_syntax_error=03000_E_Analitzador - Error de sintaxi
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_e_dont_nest_interrupt=03004_E_El procediment INTERRUPT no pot estar niat
% An \VAR{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Procediment tipus "$1" ignorat
% The specified is ignored fy FPC programs
parser_e_no_overload_for_all_procs=03006_E_No totes les declaracions de "$1" son declarades amb OVERLOAD
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_export_name_double=03008_E_Nom de funció exportada doble "$1"
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Els índex de funcions exportades de DLL no poden ser dobles $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_Índex no vàlid per a la funció exportada de DLL
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_DLL o executable $1 informació de depuració no funciona, des-habilitat.
parser_w_parser_win32_debug_needs_WN=03012_W_Per permetre la depuració pel codi de win32, necessites des-habilitar la re-ubicació amb la opció -WN
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_El nom del constructor ha de ser INIT
% You are declaring a constructor with a name which is not \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_El nom del destructor ha de ser DONE
% You are declaring a constructor with a name which is not \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_proc_inline_not_supported=03016_E_Inseriment estic C++ no suportat
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_constructor_should_be_public=03018_W_El constructor hauria de ser públic
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_El destructor hauria de ser públic
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Una classe hauria de tenir només un destructor
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Les definicions de classes locals no estan permeses
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Les definicions de classes anònimes no estan permeses
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_n_object_has_no_vmt=03023_E_L'objecte "$1" no té VMT
% This is a note indicating that the declared object has no
% virtual method table
parser_e_illegal_parameter_list=03024_E_Llista de paràmetres il·legal
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_size=03026_E_S'ha especificat una quantitat errònia de paràmetres
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_L'identificador sobrecarregat "$1" no és una funció
% The compiler encountered a symbol with the same name as an overloaded
% function, but it is not a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_Funcions sobrecarregades tenen els mateixos paràmetres
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_L'encapçalament de la funció no concorda amb la declaració posterior "$1"
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=03030_E_L'encapçalament de la funció "$1" no concorda amb la posterior declaració : el nom de la variable canvia $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_Els valors en tipus d'enumeració han de ser ascendents 
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_e_no_with_for_variable_in_other_segments=03033_E_With no pot ser utilitzat per variables en un segment diferent
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_Niat de funcions > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_Error de comprovació d'abast mentre s'avaluaven les constants
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_Error de comprovació d'abast mentre s'avaluaven les constants
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_Etiqueta de CASE duplicada
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_El límit superior de CASE és menor que el límit inferior
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_constants de tipus CLASS o OBJECT no son permeses
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_assignar variables de tipus FUNCTION a funcions sobrecarregades no és permès
% You are trying to assign an overloaded function to a procedural variable.
% This is not allowed.
parser_e_invalid_string_size=03041_E_la longitud d'una cadena ha de ser un valor entre 1 i 255
% The length of a shortstring in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
parser_w_use_extended_syntax_for_objects=03042_W_utilitzan't la sintaxi estesa de DISPOSE i NEW per generar instàncies d'objectes
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the object.
parser_w_no_new_dispose_on_void_pointers=03043_W_l'ús de NEW o DISPOSE per punters sense tipus no té sentit
parser_e_no_new_dispose_on_void_pointers=03044_E_l'ús de NEW o DISPOSE no és possible amb punters sense tipus
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_esperava identificador de classe
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_Identificador de tipus no permès aquí
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_Esperava identificador de mètode
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_L'encapçalament de la funció no concorda amb mètode d'aquesta classe "$1"
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_d_procedure_start=03049_DL_PROCEDURE/FUNCTION $1
% When using the \var{-vd} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_Constant il·legal de punt flotant
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL només pot ser utilitzat en constructors
% You are using the \var{fail} keyword outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Els destructors no poden tenir paràmetres
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Només els mètodes de classes poden ser referits amb referències de classe
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
parser_e_only_class_methods=03054_E_Només els mètodes de classe poden ser accedits per mètodes de classe
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
parser_e_case_mismatch=03055_E_Els tipus d'una constant i CASE no concorden
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_El símbol no pot ser exportat d'una biblioteca
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_Un mètode heretat està ocult per "$1"
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_No hi ha un mètode en una classe avantpassada per a ser substituït: "$1"
% You are trying to \var{override} a virtual method of a parent class that does
% not exist.
parser_e_no_procedure_to_access_property=03059_E_No s'ha subministrat un membre per accedir a propietat
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_La directiva STORED PRORPERTY encara no està implementada
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_Símbol il·legal per l'accés a propietat
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code woud cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_No es pot accedir a un camp protegit d'un objecte aquí
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module wher the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_No es pot accedir a un camp privat d'un objecte aquí
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_e_overridden_methods_not_same_ret=03066_E_Els mètodes sobrecarregats han de retornar el mateix tipus: "$2" està sobrecarregat per "$1", el qual retorna un altra tipus
% If you declare overriden methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_Les funcions declarades com EXPORT no poden estar niades
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_Els mètodes no poden ser EXPORTats
% You cannot declare a procedure that is a method for an object as 
% \var{export}ed.
parser_e_call_by_ref_without_typeconv=03069_E_Les trucades per paràmetres variables tenen que concordar exactament: obtingut "$1", esperava "$2"
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_La classe no és un parent de la classe actual
% When calling inherited methods, you are trying to call a method of a non-related
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_SELF només és permès en mètodes
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_Els mètodes només poden estar en altres mètodes cridats directament amb el tipus identificador de la classe
% A construction like \var{sometype.somemethod} is only allowed in a method. 
parser_e_illegal_colon_qualifier=03073_E_Ús il·legal de ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_Error de comprovació d'abast en constructor de definició  o element de definició duplicat
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Esperava un objecte com a paràmetre
% You specified an illegal type in a \var{New} statement.
% The extended syntax of \var{New} needs an  object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_L'expressió ha de ser una trucada al constructor
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_L'expressió ha de ser una trucada al destructor
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_Ordre il·legal d'elements del registre
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_El tipus de l'expressió ha de ser CLASS o RECORD
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Les funcions sense valors de retorn no poden retornar cap valor
% In \fpc, you can specify a return value for a function when using 
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures  cannot return a value.
parser_e_constructors_always_objects=03081_E_Constructors i destructors han de ser mètodes
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_L'operador no està sobrecarregat
% You're trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Impossible sobrecarregar assignació per tipus iguals
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Impossible sobrecarregar l'operador 
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_El re-increment no és possible aquí
% You are trying to raise an exception where it is not allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_La sintaxi estesa de NEW o DISPOSE no està permesa per una classe
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_procedure_overloading_is_off=03088_E_La sobrecàrrega de funcions no està activada
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_No és possible sobrecarregar aquest operador
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_L'operador comparatiu ha de retornar un valor booleà
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Només els mètodes virtuals poden ser abstractes
% You are declaring a method as abstract, when it is not declared to be
% virtual.
parser_f_unsupported_feature=03092_F_­Ús de característica no suportada!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_La barreja de diferents tipus d'objectes (class, object, interface, etc) no és permesa
% You cannot derive \var{objects}, \var{classes}, \var{cppclasses} and
% \var{interfaces} interttwined . E,g.
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Directiva de procediment desconeguda, ha de ser ignorada: "$1"
% The procedure direcive you specified is unknown.
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
parser_e_absolute_only_to_var_or_const=03096_E_ABSOLUTE només es pot associar a una variable o constant
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Només una variable pot ser inicialitzada
% You cannot specify more than one variable with a initial value
% in Delphi mode.
parser_e_abstract_no_definition=03098_E_Els mètodes abstractes no haurien de tenir cap definició (en el cos de la funció)
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Aquesta funció sobrecarregada no pot ser local (ha de ser EXPORTED)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_Mètodes virtuals estan essent utilitzats sense un constructor en "$1"
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_c_macro_defined=03101_CL_Macro definida: $1
% When \var{-vc} is used, the compiler tells you when it defines macros.
parser_c_macro_undefined=03102_CL_Macro indefinida: $1
% When \var{-vc} is used, the compiler tells you when it undefines macros.
parser_c_macro_set_to=03103_CL_Macro $1 posada a $2
% When \var{-vc} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_Compilant $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_UL_Analitzant interfície d'unitat $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_UL_Analitzant implementació de $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_DL_Compilant $1 per segona vegada
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_property_found_to_override=03109_E_No s'ha trobat la propietat a sobre-escriure
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Només es permet una propietat per defecte.
% You specified a property as \var{Default}, but the class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_La propietat per defecte ha de ser un conjunt de propietat
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Els constructors virtuals son suportats només en model CLASS
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_No hi ha propietat per defecte disponible
% You are trying to access a default property of a class, but this class (or one of
% it's ancestors) does not have a default property.
parser_e_cant_have_published=03114_E_La classe no pot tenir una secció PUBLISHED, utilitza el commutador {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_La declaració posterior de la classe "$1" ha de ser resolta aquí per poder utilitzar la classe com anvantpassada
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Operadors locals no suportats
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_La directiva de procediment "$1" no està permesa en la secció INTERFACE
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_La directiva de procediment "$1" no està permesa en la secció IMPLEMENTATION
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_La directiva de procediment "$1" no està permesa en la declaració PROCVAR
% This procedure directive cannot be part of a procedural of function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_La funció està actualment declarada com Public/Forward "$1"
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_No poden ser utilitzats ambdós EXPORT i EXTERNAL
% These two procedure directives are mutually exclusive
parser_w_not_supported_for_inline=03123_W_"$1" encara no està suportat dins de procedure/function
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=03124_W_Inseriment des-habilitat
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Escrivint registre de navegador $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=03126_H_Podia ser que falti la de-referència del punter
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_Lector de l'assemblador seleccionat no suportat
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
earser_e_proc_dir_conflict=03128_E_La directiva de procediment "$1" té conflictes amb altres directives
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_La convenció de crida no concorda amb la posterior
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_property_cant_have_a_default_value=03131_E_La propietat no pot tenir un valor per defecte
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_El valor per defecte d'una propietat ha de ser constant
% The value of a \var{default} declared property must be knwon at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_El símbol no pot ser PUBLISHED, només pot ser una classe
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Aquest tipus de propietat no pot ser PUBLISHED
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_e_empty_import_name=03136_E_Es requereix nom d'importació
% Some targets need a name for the imported procedure or a \var{cdecl} specifier
parser_e_division_by_zero=03138_E_Divisió per zero
% There is a divsion by zero encounted
parser_e_invalid_float_operation=03139_E_Operació de punt flotant no vàlida
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_El límit superior de l'abast és menor que el límit inferior
% The upper bound of a an array declaration is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_La cadena "$1" és més llarga que "$2"
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_La longitud de la cadena és major que el conjunt de caràcters
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_Expressió il·legal després d´una directiva missatge
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Els manipuladors de missatges només poden agafar una trucada per paràmetre de referència
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Etiqueta de missatge duplicada: "$1"
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_SELF només pot ser un paràmetre explícit en manipuladors de missatges
% The self parameter can be passed only explicit if it is a method which
% is declared as message handler
parser_e_threadvars_only_sg=03147_E_THREADVARS només poden ser estàtiques o globals
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has it's own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Assemblador directe no suportat pel format binari de sortida
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
% \end{description}
parser_w_no_objpas_use_mode=03149_W_No carreguis manualment la unitat OBJPAS, utilitza {\$mode objfpc\} o {\$mode delphi\}
% You are trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea. Use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automatically
parser_e_no_object_override=03150_E_OVERRIDE no pot ser utilitzat en objectes
% Override is not supported for objects, use \var{virtual} instead to override
% a method of a parent object
parser_e_cant_use_inittable_here=03151_E_Els tipus de dades les quals requereixen inicialització/finalització no poden ser utilitzades en registres variables
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Les cadenes de recursos només poden ser estàtiques o globals
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_Exit amb argument no es pot utilitzar aquí
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_El tipus de símbol ha de ser booleà
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_Aquest símbol no és permès com a símbol per emmagatzemar
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=03156_E_Només les classes compilades amb el mode $M+ poden ser publicades
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Directiva de procediment esperada
% When declaring a procedure in a const block you used a ; after the
% procedure declaration after which a procedure directive must follow.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_El valor per una propietat índex ha de ser un tipus ordinal
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Nom de procediment massa curt per a ser exportat
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_Una entrada DEFFILE no pot ser generada per variables globals en una unitat
parser_e_dlltool_unit_var_problem2=03161_E_Compila sense la opció -WD
% You need to compile this file without the -WD switch on the
% commandline
parser_f_need_objfpc_or_delphi_mode=03162_F_Necessites el mode ObjFpc (-S2) o Delphi (-Sd) per a compilar aquest mòdul
% You need to use {\$MODE OBJFPC\} or {\$MODE DELPHI\} to compile this file
% Or use the equivalent commandline switches -S2 or -Sd
parser_e_no_export_with_index_for_target=03163_E_No puc exportar amb índex sota $1
% Exporting of functions or procedures with a specific index is not
% supported on this target.
parser_e_no_export_of_variables_for_target=03164_E_L'exportació de variables no està suportat sota $1
% Exporting of variables is not supported on this target.
parser_e_improper_guid_syntax=03165_E_Sintaxi GUID no apropiada
parser_w_interface_mapping_notfound=03168_W_El procediment anomenat "$1" no sembla apropiat per implementar $2.$3
parser_e_interface_id_expected=03169_E_Identificador d'INTERFACE esperat
% This happens when the compiler scans a \var{class} declaration that contains
% \var{interface} function name mapping code like this:
% \begin{verbatim}
% type
%   TMyObject = class (TObject, IDispach)
%   function IUnknown.QueryInterface=MyQueryInterface;
%   ....
% \end{verbatim}
% and the \var{interface} before the dot not listed in the inheritance list.
parser_e_type_cant_be_used_in_array_index=03170_E_El tipus "$1" no pot ser utilitzat com a índex de conjunt
% Types like \var{qword} or \var{int64} aren't allowed as array index type
parser_e_no_con_des_in_interfaces=03171_E_Els constructors i destructors no estan permesos en INTERFACE
% Constructor and destructor declarations aren't allowed in interface
% In the most cases the method \var{QueryInterface} of \var{IUnknown} can
% be used to create a new interface
parser_e_no_access_specifier_in_interfaces=03172_E_Els especificadors ACCESS no poden ser utilitzats en INTERFACES
% The access specifiers \var{public}, \var{private}, \var{protected} and
% \var{published} can't be used in interfaces because all methods
% of an interfaces must be public.
parser_e_no_vars_in_interfaces=03173_E_INTERFACE no pot contenir camps
% Declarations of fields aren't allowed in interfaces. An interface
% can contain only methods
parser_e_no_local_proc_external=03174_E_No es poden declarar procediments locals com EXTERNAL
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high
parser_w_skipped_fields_before=03175_W_Alguns camps d'abans de "$1" no han estat inicialitzats
% In Delphi mode, not all fields of a typed constant record have to be
% initialized, but the compiler warms you when it detects such situations
parser_e_skipped_fields_before=03176_E_Alguns camps d'abans de "$1" no han estat inicialitzats
% In all syntax modes but Delphi mode, you can't leave some fields unitialized
% in the middle of a typed constant record
parser_w_skipped_fields_after=03177_W_Alguns camps de després de "$1" no han estat inicialitzats
% You can leave some fields at the end of a type constant record unitialized
% (the compiler will initialize them to zero automatically), this may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_Directives VarArgs sense CDecl i External
% The varargs directive can only be used with procedures or functions
% that are declared with \var{cdecl} and \var{external} directives. The varargs directive
% is only meant to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_SELF ha de ser un paràmetre normal
% You can't declare self as a const or var parameter, it must always be
% a call-by-value parameter
parser_e_interface_has_no_guid=03180_E_L'interfície "$1" no té identificació
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Identificador de mètode o classe desconegut : "$1"
% Properties must refer to a field or method in the same class
parser_w_proc_overriding_calling=03182_W_Utilitzant "$2" en lloc de "$1"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used
parser_e_noprocvarobj_const=03183_E_Les constants del tipus procediment d'objecte només poden inicialitzades amb NIL
% You can't assign the address of a method to a typed constant which has a
% "procedure of object" type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates an (which can't be known at compile time).
parser_e_default_value_only_one_para=03184_E_El valor per defecte només es pot assignar a un paràmetre
parser_e_default_value_expected_for_para=03185_E_Es requereix paràmetre per "$1"
parser_w_unsupported_feature=03186_W_Característica no suportada!
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_Els conjunts de C es passen com a referència
% Any array passed to a C functions is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_El conjunt de constants de C, han de ser l'últim argument
% You can not add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Redefinició de tipus "$1"
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a cause for errors.
parser_w_cdecl_has_no_high=03190_W_Les funcions declarades amb CDECL no tenen paràmetres alts
% Functions declared with cdecl modifier do not pass an extra implicit parameter
parser_w_cdecl_no_openstring=03191_W_Les funcions declarades amb CDECL no suporten cadenes obertes
% Openstring is not supported for cdecl'ared functions
parser_e_initialized_not_for_threadvar=03192_E_No puc inicialitzar variables definides com a variables del fil
% Variables declared as threadvar can not be initialized with a default value
% The variables will always be filled with zero at the start of a new thread
parser_e_msg_only_for_classes=03193_E_La directiva de missatges només és permesa en classes
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_S'espera procediment o funció
% A class method can only be specified for procedures and functions
parser_e_illegal_calling_convention=03195_E_La convenció de crida no està suportada per aquesta CPU
% Some calling conventions are supported only by certain CPUs. I.e. most
% non-i386 ports support only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE no es pot utilitzar en objectes
% \var{REINTRODUCE} no és suportat en objectes.
parser_e_paraloc_only_one_para=03197_E_Cada argument ha de tenir la seva pròpia localització
% if locations for arguments are specified explicity as it is required by
% some syscalls conventions, each argument must have its only location,
% things like \var{procedure p (i,j:longint 'r');} aren't allowed
parser_e_paraloc_all_paras=03198_E_Cada argument ha de tenir una localització explicita
% if one argument has an explicit argument location, all arguments of a procedure
% must have one
parser_e_illegal_explicit_paraloc=03199_E_Localització d'argument desconeguda
% The location specified for an argument isn't recognized by the compiler
parser_e_32bitint_or_pointer_variable_expected=03200_E_Esperava variable tipus Integer de 32 Bits o Pointer
% The libbase for MorphOS/AmigaOS can be give only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_GOTO no està permès entre diferents procediments
% It is not allowed to use the \var{goto} statements referencing labels outside the
% current procedure. The following example shows the problem:
% \begin{verbatim}
% ...
% procedure p1;
% label
% l1:
%
% procedure p2;
% begin
%  goto l1; // This goto ISN'T allowed
% end;
%
% begin
% p2
% l1;
% end;
% ...
%
% \end{verbatim}
parser_f_too_complex_proc=03202_F_Procediment massa complex, requereix massa registres
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_Expressió il·legal
% This can occur under many circumstancies. Mostly when trying to evaluate
% constant expressions
parser_e_invalid_integer=03204_E_Expressió amb INTEGER no vàlida, el resultat ha de ser INTEGER
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_Qualificador il·legal
% One of the following is happening:
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_Límit més alt < límit més baix de l'abast
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range
parser_e_macpas_exit_wrong_param=03207_E_La sortida del paràmetre ha de ser el nom del procediment on s'utilitza
% Non local exit is not allowed. This error occur only in mode MacPas.
parser_e_illegal_assignment_to_count_var=03208_E_Asignació no vàlida de variable "$1" pel bucle FOR
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
parser_e_no_local_var_external=03209_E_No es pot declarar una variable local com EXTERNAL
% Declaring local variables as external is not allowed. Only global variables can reference
% to external variables.
parser_e_proc_already_external=03210_E_El procediment ja està declarat com EXTERNAL
% The procedure is already declared with the EXTERNAL directive in an interface of
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Afegint la unitat Variants a USES
% The Variant type is used in the unit without any used unit using the variants unit.
% The compiler has implicity added the Variants unit to the uses list.
% To remove this warning, the Variants unit needs to be added to the USES statement.
parser_e_no_static_method_in_interfaces=03212_E_Els mètodes Class i Static no es poden utilitzar dins de INTERFACES
% The specifier \var{class} and directive \var{static} can be used in interfaces
% because all methods of an interface must be public.
parser_e_arithmetic_operation_overflow=03213_E_Sobrecàrrega en operació aritmètica
% An operation on two integers values produced an overflow
% \end{description}
#
# Type Checking
#
#
# 04058 is the last used one
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Els tipus no concorden
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Tipus incompatibles: obtingut "$1",  esperava "$2"
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
type_e_not_equal_types=04002_E_Els tipus "$1" i "$2" no concorden
% The types are not equal
type_e_type_id_expected=04003_E_Esperava identificador de tipus
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Esperava identificador de variable 
% This happens when you pass a constant to a routine (such as \var{Inc} var or \var{Dec})
% when it expects a variable. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_Esperava expressió entera
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Esperava expressió booleana, però he obtingut "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Esperava expressió ordinal
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_Esperava tipus punter
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_Esperava tipus class, però he obtingut "$1"
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_No es pot avaluar l'expressió constant
% This error can ocurr when the bounds of an array you declared does
% not evaluate to ordinal constants
type_e_set_element_are_not_comp=04012_E_Els elements del conjunt no son compatibles
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Operació no implementada per a conjunts
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Conversió automàtica de tipus de punt flotant a COMP el qual és un tipus enter
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_Utilitzan't DIV per obtenir un resultat enter
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_Els tipus cadena no concorden a causa del mode $V+
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_SUCC o PRED en enumeracions amb assignació no és possible
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_No es poden llegir o escriure variables d'aquest tipus
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% reals, pchars and strings can be read from/written to a text file.
% Booleans can only be written to text files.
type_e_no_readln_writeln_for_typed_file=04019_E_No és pot utilitzar READLN o WRITELN en fitxers de tipus
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_No és pot utilitzar READ o WRITE en fitxers sense tipus
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Conflicte de tipus entre elements del conjunt
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(longint/dword) retrona el word superior/inferior
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Esperava expressió tipus INTEGER o REAL
% The first argument to \var{str} must a real or integer type. 
type_e_wrong_type_in_array_constructor=04024_E_Tipus erroni "$1" en constructor de conjunt
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Tipus incompatible per l'argument num. "$1": ha agafat "$2", esperava "$3"
% You are trying to pass an invalid type for the specified parameter.
% \end{description}
type_e_no_method_and_procedure_not_compatible=04026_E_Mètode (variable) i Procedure (variable) no son compatibles
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_Constant il·legal passada a funció matemàtica interna
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_No es troba l'adreça de les constants
% It is not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=04029_E_No es pot assignar l'argument
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they cannot be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_No es pot assignar procediment/funció local a procediment variable
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_No puc assignar valors a una adreça
% It is not allowed to assign a value to an address of a variable, constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_No puc assignar valors variables a una constant ja definida
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing the value make the parameter as a value parameter or a var.
type_e_array_required=04033_E_Es requereix tipus conjunt
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode also a pointer is allowed.
type_e_interface_type_expected=04034_E_Esperava tipus interface, però he trobat "$1"
type_w_mixed_signed_unsigned=04035_W_Barreja d'expressions amb signe i paraules llargues dona un valor de 64 octets
% If yoy divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64bit which is slower than normal
% 32bit arithmetic. You can avoid this by typecasting one operand so it
% matches the resulttype of the other one.
type_w_mixed_signed_unsigned2=04036_W_Barrejar expressions amb signe i tipus LONGWORD aquí, pot causar errors
% If you use a binari operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% matches the resulttype of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_La conversió de tipus té diferents tamanys en les assignacions ($1 -> $2)
% Type casting to a type with a different size is not allowed when the variable is
% used for assigning.
type_e_array_index_enums_with_assign_not_possible=04038_E_Les assignacions en enumerats no es poden utilitzar com a índex de conjunts
% When yoy declare an enumeration type which has assignments in it, as in C,
% like the foloowing:
% \begin{verbatim}
%  Tenum = (a,b,e:=5);
% \end{verbatim}
% yoy cannot use it as index of an array.
type_e_classes_not_related=04039_E_Els tipus Class o Object "$1" i "$2" no estan relacionats
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_w_classes_not_related=04040_W_Els tipus Class "$1" i "$2" no estan relacionats
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_e_class_or_interface_type_expected=04041_E_Esperava tipus Class o Interface, però he trobat "$1"
type_e_type_is_not_completly_defined=04042_E_El tipus "$1" no està completament definit
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring
type_w_signed_unsigned_always_false=04044_W_La comparació és sempre falsa
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% false. Explicitly typecast the constant to the correct range to avoid this problem.
type_w_signed_unsigned_always_true=04045_W_La comparació és sempre verdadera
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% true. Explicitly typecast the constant to the correct range to avoid this problem.
type_w_instance_with_abstract=04046_W_Construint una classe "$1" amb un mètode abstracte 
% An instance of a class is created which contains non_implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overriden.
type_h_in_range_check=04047_H_L'operand esquerra per a l'operador IN ha de tenir el tamany d'un octet
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8-bits, this may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_El tamany dels tipus no concorden, possible pèrdua de dades.
type_h_smaller_possible_range_check=04049_H_El tamany dels tipus no concorden, possible pèrdua de dades.
% There is an assignement to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data
type_e_cant_take_address_of_abtract_method=04050_E_No es pot agafar l'adreça d'un mètode abstracte
% An abstract method has no body, so the address of an abstract method can not 
% be taken
type_e_operator_not_allowed=04051_E_L'operador no és aplicable al tipus d'operant
% You are trying an operator that is not available for the type of the
% operands
type_e_constant_expr_expected=04052_E_Esperava expressió constant
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_La operació "$1" no és suportada pels tipus "$2" i "$3"
% The operation is not allowed for the supplied types
type_e_illegal_type_conversion=04054_E_Conversió no vàlida de tipus: "$1" a "$2"
% When doing a type-cast, you must take care that the sizes of the variable
% and the destination type are the same
type_h_pointer_to_longint_conv_not_portable=04055_H_La conversió entre ORDINAL i POINTER no és portable
% If you typecast a pointer to a longint (or vice-versa), this code will not
% compile on a machineusing 64-bit for pointer storage.
type_w_pointer_to_longint_conv_not_portable=04056_W_La conversió entre LONGINT i POINTER no és portable
% If you typecast a pointer to a ordinal type of different size (or vice-versa)
% this can cause problems. This is a warning to help finding the 32bit specific
% code where cardinal/longint is used to typecast pointers to ordinals
% A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_No puc determinar quina funció sobrecarregada cridar.
% You are calling overloaded functions with a parameter that does not correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Variable no vàlida com a comptador
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
% \end{description}
#
# Symtable
#
# 05056 is the last used one
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Identificador no trobat "$1"
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Error intern en SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Identificador duplicat "$1"
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_L'identificador ja està definit en $1 en la línia $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Identificador desconegut "$1"
% The identifier encountered has not been declared, or is used outside the
% scope where it is defined.
sym_e_forward_not_resolved=05005_E_Declaració posterior no solucionada "$1"
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Error en definició de tipus
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is err¢neous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_Tipus posterior no resolt "$1"
% A symbolwas forward defined, but no declaration was encountered.
sym_e_only_static_in_static=05010_E_Només les variables estàtiques poden ser utilitzades en mètodes estàtics o fora de mètodes
% A static method of an object can only access static variables.
sym_f_type_must_be_rec_or_class=05012_F_Esperava tipus RECORD o CLASS
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Les instàncies de classes o objectes com un mètode abstracte no son permeses 
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Etiqueta no definida "$1"
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Etiqueta declarada i utilitzada però no definida "$1"
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Declaració d'etiqueta no vàlida
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO i LABEL no son suportats (utilitza commutador -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the  \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Etiqueta no trobada
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_L'identificador no és una etiqueta
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_L'etiqueta encara no està definida
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_Declaració il·legal de tipus en elements del conjunt
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Definició posterior de classe no resolta "$1"
% You declared a class, but you did not implement it.
sym_n_unit_not_used=05023_H_Unitat "$1" no utilitzada en $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Paràmetre no utilitzat "$1"
% The identifier was declared (locally or globally) but
% was not used (locally or globally).
sym_n_local_identifier_not_used=05025_N_Variable local no utilitzada "$1"
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_El paràmetre de valor "$1" és assignat però mai utilitzat
% The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_La variable local "$1" es assignada però mai utilitzada
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Local $1 "$2" no utilitzat/ada
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_El camp privat "$1.$2" no s'utilitza mai
sym_n_private_identifier_only_set=05030_N_El camp privat "$1.$2" és assignat però mai utilitzat
sym_n_private_method_not_used=05031_N_Mètode privat "$1.$2" mai utilitzat
sym_e_set_expected=05032_E_Tipus SET esperat
% The variable or expression is not of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_El resultat de la funció no sembla estar posat
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_El tipus "$1" no està alineat correctament en el registre actual per C
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Identificador de camp de registre desconegut "$1"
% The field doesn't exist in the record/object definition.
sym_n_uninitialized_local_variable=05036_W_La variable local "$1" no sembla que hagi estat inicialitzada
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_n_uninitialized_variable=05037_W_La variable "$1" no sembla que hagi estat inicialitzada
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_L'identificador "$1" no identifica cap membre
% This error is generated when an identifier of a record,
% field, or method is accessed while it is not defined.
sym_h_param_list=05039_H_Declaració trobada: $1
% You get this when you use the \var{-vh} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Segment de dades massa llarg
% You get this when you declare an array whose size exceeds the
% prescribed limit (2 Gb on 80368+/68020+ processors)
sym_e_no_matching_implementation_found=05042_E_La implementació pel mètode d'interfície "$1" no concorda
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_El símbol "$1" està desfasat
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit/library. Usage of this symbol
% should be avoied as much as possible.
sym_w_non_portable_symbol=05044_W_El símbol "$1" no és portable
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, usage
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_El símbol "$1" no està implementat
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_No puc crear tipus a partir d'aquest
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}
% \end{description}
#
# Codegenerator
#
# 06045 is the last used one
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_La llista de paràmetres excedeix els 65535 octets
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_file_must_call_by_reference=06012_E_Els tipus FILE han de ser paràmetres variables
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_Free Pascal no suporta l'ús de punters de tipus FAR
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_Les funcions declarades com EXPORT no poden ser cridades
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Possible accés no valid de CONSTRUCTOR o DESTRUCTOR
% The compiler detected that a constructor or destructor is called within a
% method. This will probably lead to problems, since contructors/destructors
% require parameters on entry
cg_n_inefficient_code=06017_N_Codi ineficient
% You statement seems dubious to the compiler.
cg_w_unreachable_code=06018_W_Aquest codi no s'executarà mai
% You specified a construct which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Els mètodes abstractes no poden ser cridats directament
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Registre $1 pes $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_L'esquelet de la pila és omès
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_w_64bit_range_check_not_supported=06030_W_Comprovació d'abast per enters de 64 bit no és suportat en aquest destí
% 64 bit range check is not yet implemented for 32 bit processors.
cg_e_unable_inline_object_methods=06031_E_Incapaç d'inserir mètodes de l'objecte
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Incapaç d'inserir crides a procvar
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_No hi ha informació de codi enmagetzemada pel procediment inserit
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Element zero d'un ansi/wide- o longstring no pot ser accedit, utilitza (set)length
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such a string types
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructors o destructors no poden ser cridats dins de 'with'
% Inside a \var{with} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_No es pot cridat al mètode manipulador de missatges directament
% A message method handler method cannot be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=06039_E_Salt dintre o fora d'un block d'excepció
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
cg_e_control_flow_outside_finally=06040_E_Les declaracions de control de flux no estan permeses en un bloc final
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
cg_w_parasize_too_big=06041_W_El tamany dels paràmetres excedeix el límit per determinades cpu's
% This indicates that you are declaring more than 64k ok parameters, which
% might not be supported on other processor targets.
cg_w_localsize_to_big=06042_W_El tamany de la variable local excedeix el límit per determinades cpu's
% This indicates that you are declaring more than 32k of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_Les variables locals excedeixen el límit suportat
% This indicates that you are declaring more than 32k of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK no permès
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE no permès
% You're trying to use \var{continue} outside a loop construction.
% \end{description}
# EndOfTeX
#
# Assembler reader
#
# 07097 is the last one used
#
asmr_d_start_reading=07000_DL_Començant analització d'assemblador estil $1
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_DL_Finalitzant analizació d'assemblador estil $1
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Una directiva que no és una etiqueta conté @
% A identifier which isn't a label can't contain a @.
asmr_e_building_record_offset=07004_E_Error muntant desplaçament de registre
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET utilitzat sense identificador
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE utilitzat sense identificador
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_No es pot utilitzar variable local o paràmetre aquí
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the frame pointer register so the
% address can't be obtained directly.
asmr_e_need_offset=07008_E_Es necessita utilitzar OFFSET aquí
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_Es necessita utilitzar $ aqui
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_No es poden utilitzar multiples símbols realotjables
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Els símbol realotjables només pot ser afegits
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Expressió constant invàlida
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Símbol realotjable no permès
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Sintaxi de referència invàlida
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_No pots arribar a $1 des d'aquest codi
% You can not read directly the value of local or para
% of a higher level in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Símbols i etiquetes locals no estan permeses com a referències
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Utilització no vàlida de registre base i índex
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Possible error en manipulació de camp d'objecte
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Factor d'escala erroni
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Ús de múltiples registres índex
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Tipus de l'operador no vàlid
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_Cadena no vàlida com operador opcode: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE i @DATA no suportats, s'ignoren
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Les referències a etiqueta nula no estan permeses
asmr_e_expr_zero_divide=07025_E_Divisió per zero en avaluador asm
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_Expressió il·legal
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_Seqüència d'escapament ignorada: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_Símbol de referència no vàlid
asmr_w_fwait_emu_prob=07029_W_Fwait pot causar problemes d'emulació amb emu387
asmr_w_fadd_to_faddp=07030_W_FADD sense operant traduït a FADDP
asmr_w_enter_not_supported_by_linux=07031_W_La instrucció ENTER no està suportada pel kernel del Linux
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Cridant una funció sobrecarregada en asm
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_Tipus de símbol no suportat per l'operador
asmr_e_constant_out_of_bounds=07034_E_Valor constant fora de límits
asmr_e_error_converting_decimal=07035_E_Error convertint decimal $1
% A constant decimal value doen not have the correct syntax
asmr_e_error_converting_octal=07036_E_Error convertint octal $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Error convertint binari $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Error convertint hexadecimal $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 traduït a $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 està associat a una funció sobrecarregada
asmr_e_cannot_use_SELF_outside_a_method=07041_E_No es pot utilitzar SELF fora d'un mètode
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_No es pot utilitzar OLDEBP fora d'un procediment niat
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Els procediments no poden retornar cap valor en codi asm
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG no suportat
asmr_e_size_suffix_and_dest_dont_match=07045_E_El tamany de sufix i destinació o tamany de l'origen no concorden
% The register size and the opcode size suffix does not match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_El tamany de sufix i destinació o tamany de l'origen no concorden
% The register size and the opcode size suffix does not match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Error de sintaxi d'assemblador
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_Combinació no vàlida de opcode i operadors
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Error de sintaxi d'assemblador en operador
asmr_e_syn_constant=07050_E_Error de sintaxi d'assemblador en constant
asmr_e_invalid_string_expression=07051_E_Expressió de cadena no vàlida
asmr_w_const32bit_for_address=07052_W_Constant amb símbol $1, probable adreça incorrecta
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Opcode desconegut $1
% This opcode is not known
asmr_e_invalid_or_missing_opcode=07054_E_Invàlid o falta opcode
asmr_e_invalid_prefix_and_opcode=07055_E_Combinació no vàlida de prefix i opcode: $1
asmr_e_invalid_override_and_opcode=07056_E_Combinació no vàlida de override i opcode: $1
asmr_e_too_many_operands=07057_E_Massa operadors en la línia
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR ignorat
asmr_w_far_ignored=07059_W_FAR ignorat
asmr_e_dup_local_sym=07060_E_Símbol local duplicat $1
asmr_e_unknown_local_sym=07061_E_Símbol local indefinit $1
asmr_e_unknown_label_identifier=07062_E_Identificador d'etiqueta desconegut $1
asmr_e_invalid_register=07063_E_Nom de registre invàlid
% There is
%
asmr_e_invalid_fpu_register=07064_E_Nom de registre de punt flotant no vàlid
asmr_w_modulo_not_supported=07066_W_Mòdul no suportat
asmr_e_invalid_float_const=07067_E_Constant de punt flotant no vàlida $1
asmr_e_invalid_float_expr=07068_E_Expressió de punt flotant no vàlida
asmr_e_wrong_sym_type=07069_E_Tipus de símbol erroni
asmr_e_cannot_index_relative_var=07070_E_No es pot indexar una variable local o un paràmetre amb un registre
asmr_e_invalid_seg_override=07071_E_Expressió de segment sobre-escrita no vàlida
asmr_w_id_supposed_external=07072_W_Identificador $1 suposat extern
asmr_e_string_not_allowed_as_const=07073_E_Cadenes no permeses com a constants
asmr_e_no_var_type_specified=07074_E_No hi ha tipus de variable especificat
asmr_w_assembler_code_not_returned_to_text=07075_E_Codi d'assemblador no retornat a secció de text
asmr_e_not_directive_or_local_symbol=07076_E_$1 No és una directiva o un símbol local
asmr_w_using_defined_as_local=07077_E_Utilitzant un nom definit com una etiqueta local
asmr_e_dollar_without_identifier=07078_E_El símbol Dol·lar és utilitzat sense un identificador
asmr_w_32bit_const_for_address=07079_W_Constant de 32bit creada per adreça
asmr_n_align_is_target_specific=07080_N_.align és destí específic, utilitza .balign o .p2align
asmr_e_cannot_access_field_directly_for_parameters=07081_E_No es pot accedir a camps directament per paràmetres
% Some m68k reader errors
asmr_e_cannot_access_object_field_directly=07082_E_No es pot accedir a camps d'objectes/classes directament
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_No s'ha especificat cap tamany i soc incapaç de determinar el tamany dels operadors
% You should specify explicity a size for the reference, because
% compiler is unable to determine what size (byte, word, dword, etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=07084_E_No es pot utilitzar RESULT en aquesta funció
asmr_w_adding_explicit_args_fXX=07086_W_"$1" sense operador traduït a "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" traduït a "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" traduït a "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Caràcter < no permès aquí
asmr_e_invalid_char_greater=07090_E_Caràcter > no permès aquí
asmr_w_align_not_supported=07093_W_ALIGN no suportat
asmr_e_no_inc_and_dec_together=07094_E_Inc i Dec no poden estar junts
asmr_e_invalid_reg_list_in_movem=07095_E_Reglist invàlid per movem
asmr_e_invalid_reg_list_for_opcode=07096_E_Reglist invàlid per Opcode
asmr_e_68020_mode_required=07097_E_Es requereix mode 68020
% You should load the parameter list first into a register and then access the
% fields using that register.
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_No s'ha especificat tamany i no soc capaç de determinar el tamany dels operands, utilitzo DWORD
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Error de sintaxi mentre s'analitzava un operador desplaçat
% ARM only, ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
% orr r2,r2,r2,lsl #8
% end;
% \end{verbatim}
#
# Assembler/binary writers
#
# 08019 is the last one used
#
asmw_f_too_many_asm_files=08000_F_Massa arxius d'assemblador oberts
asmw_f_assembler_output_not_supported=08001_F_Assemblador de sortida seleccionat no suportat
asmw_f_comp_not_supported=08002_F_COMP no suportat
asmw_f_direct_not_supported=08003_F_Direct no suportat per escriptors binaris
asmw_e_alloc_data_only_in_bss=08004_E_L'alotjament de dades només està permès en secció bss
asmw_f_no_binary_writer_selected=08005_F_No hi ha seleccionat un escriptor binari
asmw_e_opcode_not_in_table=08006_E_Asm: Opcode $1 no és a la taula
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 combinació invàlida de opcode i operadors
asmw_e_16bit_not_supported=08008_E_Asm: Referències de 16 Bit no suportades
asmw_e_invalid_effective_address=08009_E_Asm: Direcció efectiva no vàlida
asmw_e_immediate_or_reference_expected=08010_E_Asm: Immediat o referència esperat
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 valor excedeix els límits $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Salt curt està fora de l'abast $1
asmw_e_undefined_label=08013_E_Asm: Etiqueta indefinida $1
asmw_e_comp_not_supported=08014_E_Asm: tipus comp no suportat per aquesta destinació
asmw_e_extended_not_supported=08015_E_Asm: tipus Extended no suportat per aquesta destinació
asmw_e_duplicate_label=08016_E_Asm: Etiqueta duplicada  : $1
asmw_e_redefined_label=08017_E_Asm: Etiqueta re-definida : $1
asmw_e_first_defined_label=08018_E_Asm: Primera etiqueta definida aquí
asmw_e_invalid_register=08019_E_Asm: registre no vàlid $1
#
# Executing linker/assembler
#
# 09034 is the last one used
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Sistema operatiu d'origen re-definit
exec_i_assembling_pipe=09001_I_Assemblant canonada "$1"
exec_d_cant_create_asmfile=09002_E_No es pot crear fitxer assemblador $1, comprova permisos
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_objectfile=09003_E_No es pot creat fitxer objecte: $1, comprova permisos
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_archivefile=09004_E_No es pot crear arxiu: $1, comprova permisos
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_w_assembler_not_found=09005_W_Assemblador $1 no trobat, canviant a assemblador extern
exec_t_using_assembler=09006_T_Utilitzant assemblador: $1
exec_w_error_while_assembling=09007_W_Error mentre s'assemblava codi de sortida $1
exec_w_cant_call_assembler=09008_W_No es pot cridar l'assemblador, error $1 canviant a assemblat extern
exec_i_assembling=09009_I_Assemblant $1
exec_i_assembling_smart=09010_I_Assemblament intel·ligent $1
exec_w_objfile_not_found=09011_W_Objecte $1 no trobat, l'enllaç podia fallar !
exec_w_libfile_not_found=09012_W_Llibrer¡a $1 no trobada, l'enllaç podia fallar !
exec_w_error_while_linking=09013_W_Error mentre s'enllaçava
exec_w_cant_call_linker=09014_W_No es pot cridar l'enllaçador, canviant a l'enllaçador extern
exec_i_linking=09015_I_Enllaçant $1
exec_w_util_not_found=09016_W_Utilitat $1 no trobada, canviant a enllaçador extern
exec_t_using_util=09017_T_Utilitzant $1
exec_e_exe_not_supported=09018_E_Creació d'executables no suportada
exec_e_dll_not_supported=09019_E_Llibrer¡es dinàmiques no suportades
exec_i_closing_script=09020_I_Tancant script $1
exec_w_res_not_found=09021_W_compilador de recursos no trobat, canviant a mode extern
exec_i_compilingresource=09022_I_Compilant recurs $1
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_La unitat $1 no pot ser enllaçada estàticament, canviant a enllaçament intel·ligent
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_La unitat $1 no pot ser enllaçada intel·ligentment, canviant a enllaçament estàtic.
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_No es pot utilitzar l'enllaçament compartit amb la unitat $1, canviant a enllaçament estàtic
exec_e_unit_not_smart_or_static_linkable=09026_E_La unitat $1 no pot ser enllaçada ni estàticament ni dinàmicament
exec_e_unit_not_shared_or_static_linkable=09027_E_La unitat $1 no pot ser enllaçada ni estaticament ni compartida.
%\end{description}
# EndOfTeX
#
# Executable information
#
execinfo_f_cant_process_executable=09028_F_No es pot post-processar l'executable $1
execinfo_f_cant_open_executable=09029_F_No es pot obrir l'executable $1
execinfo_x_codesize=09030_X_Tamany de codi: $1 octets
execinfo_x_initdatasize=09031_X_Tamany de dades inicialitzades: $1 octets
execinfo_x_uninitdatasize=09032_X_Tamany de dades sense inicialitzar: $1 octets
execinfo_x_stackreserve=09033_X_Espai reservat per a la pila: $1 octets
execinfo_x_stackcommit=09034_X_Espai utilitzat per a la pila: $1 octets
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
unit_t_unitsearch=10000_T_Buscant unitat: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_Carregant PPU $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_Nom de PPU: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_Senyals de PPU: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_Crc de PPU: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_Data de PPU: $1
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=10006_U_Fitxer PPU massa curt
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_invalid_header=10007_U_Encapçalament d'unitat compilada invàlid (no hi ha PPU al principi)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_La versió $1 de la unitat compilada no és vàlida
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_La unitat està compilada per a un altre processador
% This unit file was compiled for a different processor type, and 
% cannot be read
unit_u_ppu_invalid_target=10010_U_La unitat està compilada per una altra destinació

% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=10011_U_Unitat compilada font: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_write=10012_U_Escrivint $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_No es pot escriure el fitxer PPU
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_Error llegint fitxer PPU
% Unexpected end of file
unit_f_ppu_read_unexpected_end=10015_F_Inesperat final de fitxer PPU
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_invalid_entry=10016_F_Entrada invàlida en fitxer PPU: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_Problema contant PPU Dbx
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Nom d'unitat il·legal: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=10019_F_Massa unitats
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Referència circular entre unitats $1 i $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_No es pot compilar la unitat $1, no hi ha fonts disponibles
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_No trobo la unitat $1
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_unit_name_error=10023_W_No trobo la unitat $1 però $2 existeix
unit_f_unit_name_error=10024_F_Buscava Unitat $1, però he trobat $2
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_Compilar la unitat System requereix el commutador -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_Hi ha hagut $1 errors compilant el mòdul, aturat!
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Carregant de $1 ($2) la unitat $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_Re-compilant $1, suma de verificació canviada a $2
unit_u_recompile_source_found_alone=10029_U_Re-compilant $1, només s'han trobat les fonts
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Re-compilant unitat, la biblioteca estàtica és més vella que la unitat compilada
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Re-compilant unitat, la biblioteca compartida és més vella que PPU
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Re-compilant unitat, obj i asm son més vells que PPU
% When you use the \var{-vu} flag, the compiler warns if the assembler of
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Re-compilant unitat, obj és més vell que asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_Analitzant interfície de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_parsing_implementation=10035_U_Analitzant implementació de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_Segona càrrega per la unitat $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_Comprovació de fitxer PPU $1 data $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
### The following two error msgs is currently disabled
# unit_h_cond_not_set_in_last_compile=10038_H_El condicional $1 no ha estat inicialitzat al començament de la última compilació de $2
# % When recompilation of an unit ies required the compiler will check that
# % the same conditionals are set for the recompilation. The compiler has
# % found a conditional that currently is defined, but was not used the last
# % time the unit was compiled
# unit_h_cond_set_in_last_compile=10039_H_El condicional $1 ha estat inicialitzat al començament de la última compilació de $2
# % When recompilation of an unit is required the compiler will check that
# % the same conditionals are set for the recompilation. The compiler has
# % found a conditional that was used the last time the unit was compiled, but
# % the conditional is currently not defined.
unit_w_cant_compile_unit_with_changed_incfile=10040_W_He trobat fitxers inclosos modificats, però no puc re-compilar la unitat $1
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_h_source_modified=10041_H_El fitxer $1 és més nou que la última versió de la PPU $2
% A modified source file for a unit was found that was compiled with the
% release flag (-Ur). The unit will not implicitly be recompiled because
% this release flag is set.
unit_u_ppu_invalid_fpumode=10042_U_Utilitzant una unitat la qual no ha estat compilada amb el mode FPU correcte
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_Carregant unitats de INTERFACE desde $1
% When you use the \var{-vu} flag, the compiler warms that it starts
% loading the units defined in the interface part of the unit
unit_u_loading_implementation_units=10044_U_Carregant unitat de IMPLEMENTATION desde $1
% When you use the \var{-vu} flag, the compiler warms that it starts 
% loading the units defined in the implementation part of the unit
unit_u_interface_crc_changed=10045_U_Interfície CRC canviada per unitat $1
% When you use the \var{-vu} flag, the compiler warms that it the
% CRC calculated for the interface has been changed after the implementation
% has been parsed
unit_u_implementation_crc_changed=10046_U_Implementació CRC canviada per la unitat $1
% When you use the \var{-vu} flag, the compiler warms that it the
% CRC calculated has been changed after the implementation 
% has been parsed
unit_u_finished_compiling=10047_U_Acabant la compilació de la unitat $1
% When you use the \var{-vu} flag, the compiler warms that it 
% has finished compiling the unit
unit_u_add_depend_to=10048_U_Afegida dependència de $1 a $2
% When you use the \var{-vu} flag, the compiler warms that it 
% has added a dependency between the two units
unit_u_no_reload_is_caller=10049_U_No recarrego, crido: $1
% When you use the \var{-vu} flag, the compiler warms that it 
% has will not reload the unit because it is the unit that wants
% to load this unit
unit_u_no_reload_in_second_compile=10050_U_No recarrego, ja en segona compilació: $1
% When you use the \var{-vu} flag, the compiler warms that it 
% has will not reload the unit because it is already in a second recompile
unit_u_flag_for_reload=10051_U_Senyal per recarregar: $1
% When you use the \var{-vu} flag, the compiler warms that it 
% has to reload the unit
unit_u_forced_reload=10052_U_Recàrrega forçada
% When you use the \var{-vu} flag, the compiler warms that it 
% has is reloading the unit because it was required
unit_u_previous_state=10053_U_Estat previ de $1: $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit
unit_u_second_compile_unit=10054_U_Unitat S1 ja compilada, compilant per segona vegada
% When you use the \var{-vu} flag, the compiler warms that it starts
% recompiling a unit for the second time. This can happend with independent
% units
unit_u_loading_unit=10055_U_Carregant unitat $1
% When you use the \var{-vu} flag, the compiler warms that it starts
% loading the unit
unit_u_finished_loading_unit=10056_U_Acavada la càrrega de la unitat $1
% When you use the \var{-vu} flag, the compiler warms that it finished
% loading the unit
unit_u_registering_new_unit=10057_U_Registrant nova unitat $1
% When you use the \var{-vu} flag, the compiler warms that it has 
% found a new unit and registers it in the internal lists
unit_u_reresolving_unit=10058_U_Re-resolvent unitat $1
% When you use the \var{-vu} flag, the compiler warms that it 
% has to recalculate the internal data of the unit
unit_u_skipping_reresolving_unit=10059_U_Em salto unitat $1, encara carregant unitats utilitzades
% When you use the \var{-vu} flag, the compiler warms that it 
% skips to recalculate the internal data of the unit because there
% is no data to racalculate
% \end{description}
# EndOfTeX
#
#  Options
#
# 11039 is the las used one
#
option_usage=11000_$1 [opcions] <fitxer-entrada> [opcions]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Només és suportat un fitxer font
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_El fitxer DEF només pot ser creat per OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_Els fitxers de resposta niats, no son suportats
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_No hi ha fitxer font en la línia d'ordres
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_No hi ha cap opció dins el fitxer de configuració $1
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Paràmetre il·legal: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? escriu las pàgines d'ajuda
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Massa fitxers de configuració niats (max.16)
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Impossible obrir fitxer $1
% The option file cannot be found.
option_reading_further_from=11010_N_Llegint opcions addicionals de $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Destí ja posat a: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Biblioteques compartides no suportades en la plataforma DOS, revocant a estàtiques
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_Massa IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_Massa ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_Condicional obert al final del fitxer
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_La generació d'informació de depuració no és suportada per aquest executable
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Prova re-compilant amb -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_E_Estàs utilitzant el commutador obsolet $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_E_Estàs utilitzant el commutador obsolet $1, sisplau utilitza $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_Canviant a assemblador d'escriptura de codi font per defecte
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_La sortida d'assemblador seleccionada "$1" no és compatible amb "$2"
option_asm_forced=11022_W_Forçat a utilitzar assemblador "$1"
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_Llegint opcions del fitxer $1
% Options are also read from this file
option_using_env=11027_T_Llegint opcions de l'entorn $1
% Options are also read from this environment string
option_handling_option=11028_D_Trobada la opció "$1"
% Debug info that an option is found and will be handled
option_help_press_enter=11029__*** Prem <Intro> ***
option_start_reading_configfile=11030_H_Començo a llegir el fitxer de configuració $1
% Starting of config file parsing.
option_end_reading_configfile=11031_H_Acabo de llegir el fitxer de configuració $1
% End of config file parsing
option_interpreting_option=11032_D_interpretant opció "$1"
option_interpreting_firstpass_option=11036_D_interpretant opció "$1" per 1ª vegada
option_interpreting_file_option=11033_D_interpretant opció de fitxer "$1"
option_read_config_file=11034_D=11034_D_Llegint fitxer de configuració "$1"
option_found_file=11035_D_Trobat nom fitxer font "$1"
% Additional infos about options, displayed
% when you have debug option turned on.
option_code_page_not_available=11039_E_Codi de pàgina desconegut
%\end{description}
# EndOfTeX

#
# Logo (option -l)
#
option_logo=11023_[
Free Pascal Compiler versió $FPCVERSION [$FPCDATE] per $FPCCPU
Copyright (c) 1993-2005 per Florian Klaempfl
]

#
# Info (option -i)
#
option_info=11024_[
Free Pascal Compiler versió $FPCVERSION

Data del compilador         : $FPCDATE
Destinació del compilador   : $FPCCPU

Destinacions suportades     :
 $OSTARGETS

Aquest programa ve sota la Llicència Pública General de GNU
Per a més informació llegeix COPYING.FPC

Comunica errades, suggeriments, etc a:
                 bugrep@freepascal.org
]

#
# Help pages (option -? and -h)
#
option_help_pages=11025_[
**0*_Poseu + després d'un commutador booleà per activar-lo, - per desactivar-lo
**1a_El compilador no esborrarà el fitxer assemblador generat
**2al_Escriure les línies de codi font en el fitxer assemblador
**2an_Escriure informació del node en el fitxer assemblador
*L2ap_Utilitzar canonades en lloc de crear fitxers temporals d'assemblador
**2ar_Escriure info. registre d'alotj/desalotj en fitxer ensamb.
**2at_Escriure info. temporal d'alotj/desalotj en fitxer ensamb.
**1A<x>_format de sortida:
**2Adefault_Utilitzar assemblador per defecte
3*2Aas_Utilitzar GNU AS assemblador
3*2Anasmcoff_fitxer coff (Go32v2) utilitzant Nasm
3*2Aasmelf_fitxer elf32 (Linux) utilitzant Nasm
3*2Anasmwin32_fitxer objecte Win32 utilitzant Nasm
3*2Anasmwdosx_fitxer objecte Win32/WDOSX utilitzant Nasm
3*2Awasm_fitxer objecte utilitzant Wasm (Watcom)
3*2Anasmobj_fitxer objecte utilitzant Nasm
3*2Amasm_fitxer objecte utilitzant Masm (Microsoft)
3*2Atasm_fitxer objecte utilitzant Tasm (Borland)
3*2Aelf_elf32 (Linux) utilitzant escriptor intern
3*2Acoff_coff (Go32v2) utilitzant escriptor intern
3*2Apecoff_pecoff (Win32) utilitzant escriptor intern
4*2Aas_assemblant utilitzant GNU AS
6*2Aas_fitxer objecte Unix utilitzant GNU AS
6*2Agas_assemblador GNU Motorola
6*2Amit_sintaxi MIT (antiga GAS)
6*2Amot_assemblador Motorola estàndard
A*2Aas_assemblant utilitzant GNU AS
P*2Aas_assemblant utilitzant GNU AS
S*2Aas_assemblant utilitzant GNU AS
**1b_Generar informació de navegador
**2bl_Generar informació de símbols locals
**1B_Muntar tots els mòduls
**1C<x>_Opcions de generació de codi:
**2Cc<x>_set default calling convention to <x>
**2CD_Crear biblioteca dinàmica
**2Ce_Compilació amb emulació de coma flotant
**2Cf<x>_Seleccionar instrucció fpu a utilitzar per <x>
**2Cg_Generar codi PIC
**2Ch<n>_<n> octets de pila (entre 1023 i 67107840)
**2Ci_Comprovació d'E/S
**2Cn_Ometre enllaçat
**2Co_Comprovar el desbordament d'operacions amb enters
**2Cr_Comprovació de l'abast
**2CR_Validació de mètode d'objectes
**2Cs<n>_Posar el tamany de la pila a <n>
**2Ct_Comprovació de la pila
3*2CS_Crear biblioteca estàtica
3*2Cx_Utilitzar enllaçament intel·ligent
**1d<x>_Definir el símbol <x>
**1D_Generar un fitxer DEF
**2Dd<x>_Posar la descripció a <x>
**2Dv<x>_Posar versió de DLL a <x>
*O2Dw_Aplicació PM
**1e<x>_Posar la trajectòria a executables
**1E_Igual a -Cn
**1F<x>_Posar noms de fitxers i trajectòries:
**2Fa<x>[,y]_Carregar primer unitats <x> i [y], abans d'analitzar USES
**2Fc<x>_Posar pàgina de codi d'entrada a <x>
**2FD<x>_Posar el directori on buscar les utilitats del compilador
**2Fe<x>_Re-direccionar la sortida de errors a <x>
**2FE<x>_Posar el path de destinació d'executables i unitats a <x>
**2Fi<x>_Afegir <x> a la trajectòria d'inclosos
**2Fl<x>_Afegir <x> a la trajectòria de llibreries
**2FL<x>_Utilitzar <x> com enllaçador dinàmic
**2Fo<x>_Afegir <x> a la trajectòria d'objectes
**2Fr<x>_Carregar fitxer de missatges d'error <x>
**2Fu<x>_Afegir <x> a la trajectòria d'unitats
**2FU<x>_Posar el path de destinació d'unitats a <x>, anul·la -FE
*g1g_Generar informació de depuració:
*g2gc_generar controls pels punters
*g2gd_Utilitzar dbx
*g2gg_Utilitzar gsym
*g2gh_Utilitzar la unitat de traçat de la pila
*g2gl_Utilitzar la unitat d'info. per controlar la depuració
*g2gv_Generar programes traçables amb valgrind
*g2gw_Generar informació de depuració dwarf
**1i_Informació:
**2iD_Retornar la data del compilador
**2iV_Retornar la versió del compilador
**2iSO_Retornar el SO d'origen
**2iSP_Retornar el processador d'origen
**2iTO_Retornar el SO de destinació
**2iTP_Retornar el processador de destinació
**1I<x>_Afegir <x> al path d'afegits
**1k<x>_Passar <x> a l'enllaçador
**1l_Escriure el logotipus
**1M<x>_Ficar llenguatge mode a <x>
**2Mfpc_dialecte Free Pascal (per defecte)
**2Mobjfpc_Activar extensions Delphi 2
**2Mdelphi_Intentar ser compatible amb Delphi
**2Mtp_Intentar ser compatible amb TP/BP 7.0
**2Mgpc_Intentar ser compatible amb gpc
**2Mmacpas_Intentar ser compatible amb dialectes Pascal del Macintosh
**1n_No llegir el fitxer de configuració per defecte
**1o<x>_Canviar el nom de l'executable produït a <x>
**1O<x>_optimitzacions:
3*2Og_generar codi més petit
3*2OG_generar codi més ràpid (predeterminat)
**2Or_mantenir determinades variables en registres
3*2Ou_habilitar determinades optimitzacions (mirar doc)
3*2O1_optimitzacions de nivell 1 (ràpides)
3*2O2_optimitzacions de nivell 2 (-O1 + més lentes)
3*2O3_optimitzacions de nivell 3 (-O2 repetidament, max 5 vegades)
3*2Op<x>_processador de destinació:
3*3Op1_posar processador de destinació a 386/486
3*3Op2_posar processador de destinació a Pentium/PentiumMMX (r)
3*3Op3_posar processador de destinació a PPro/PII/c6x86/K6 (r)
6*2Og_generar codi més petit
6*2OG_generar codi més ràpid (predeterminat)
6*2Ox_optimitzar al màxim (NO SEGUR ENCARA!!)
6*2O0_posar processador de destinació a MC68000
6*2O2_posar processadir de destinació a MC68020+ (predeterminat)
**1pg_generar codi de perfil per gprof
**1R<x>_Estil de lectura de l'assemblador:
**2Rdefault_utilitzar assemblador predeterminat
3*2Ratt_llegir estil d'assemblador AT&T
3*2Rintel_llegir estil d'assemblador Intel
6*2RMOT_llegir estil d'assemblador Motorola
**1S<x>_Opcions de sintaxi:
**2S2_Igual que -Mobjfpc
**2Sc_Suportar operadors tipus C (*=,+=,/= i -=)
**2Sa_Incloure codi reafirmat
**2Sd_Igual que -Mdelphi
**2Se_opcions d'error <x> és una combinació del següent:
**3*_<n>: el compilador s'atura després de <n> errors (predeterminat és 1)
**3*_w: el compilador s'atura també després d'avisos
**3*_n: el compilador s'atura també després de notes
**3*_h: el compilador s'atura també després de suggeriments
**2Sg_Permetre LABEL i GOTO
**2Sh_Utilitzar cadenes ansi (ansistrings)
**2Si_Suportar INLINE estil C++
**2SI<x>_posar estil d'interfície a <x>
**3SIcom_interfície compatible amb COM (predeterminat)
**3SIcorba_interfície compatible amb CORBA
**2Sm_Suportar macros tipus C (globals)
**2So_Igual que -Mtp
**2Sp_Igual que -Mgpc
**2Ss_El nom del constructor ha de ser init (destructor = done)
**2St_Permetre la paraula clau static en objectes
**1s_No cridar assemblador ni enllaçador
**2sh_Generar seqüència per enllaçar a host
**2st_Generar seqüència per enllaçar a destinació
**2sr_Saltar fase de localització de registres (utilitzar amb -alr)
**1T<x>_sistema operatiu de destinació:
3*2Termx_OS/2 via EMX (incloent extensor EMX/RSX)
3*2Tfreebsd_FreeBSD
3*2go32v2_versió 2 de DJ Deloire DOS extender
3*2Tlinux_Linux
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware Module (clib)
3*2Tnetwlibc_Novell Netware Module (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 eComStation
3*2Tsunos_SunOS/Solaris
3*2Twatcom_Watcom compatible DOS extender
3*2Twdosx_WDOSX DOS extender
3*2Twin32_Windows 32 Bit
4*2Tlinux_Linux
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tlinux_Linux-68k
6*2Tmacos_Macintosh m68k (no suportat)
6*2Tpalmos_PalmOS
A*2Tlinux_Linux
P*2Tdarwin_Darwin i MacOS X en PowerPC
P*2Tlinux_Linux en PowerPC
P*2Tmacos_MacOS (clàssic) en PowerPC
P*2Tmorphos_MorphOS
S*2Tlinux_Linux
**1u<x>_Indefineix el símbol <x>
**1U_Opcions d'unitat:
**2Un_No comprovar el nom d'unitat
**2Us_Compilar una unitat de sistema
**1v<x>_Ser explicatiu. <x> és una combinació de les següents lletres :
**2*_e : mostrar errors (predet.)     0 : no mostrar res (excepte errors)
**2*_w : mostrar avisos               u : mostrar info d'unitats
**2*_n : mostrar notes                t : mostrar fitxers selec./utilitz.
**2*_h : mostrar suggeriments         c : mostrar condicionals    
**2*_i : mostrar informació general   d : mostrar informació de depuració  
**2*_l : mostrar números de línia     r : compatibilitat Rhide/GCC
**2*_a : mostrar-ho tot               x : informació de l'executable (win32)
**2*_v : escriure fpcdebug.txt amb    p : escriure registre amb arbre analitzat
**2*_    molta informació depuració
3*1W<x>_Opcions per a destinació tipus Win32
3*2WB<x>_Imatge a valor hexadecimal <x>
3*2WC_Especificar aplicació tipus consola
3*2WD_Utilitzar DEFFILE per exportar funcions de DLL o EXE
3*2WF_Especificar aplicació a tota pantalla (només OS/2)
3*2WG_Especificar aplicació de tipus gràfic
3*2WN_Generar codi necessari per a la depuració
3*2WR_Generar codi re-alotjable
P*2WC_Especificar aplicació tipus consola (només MacOS)
P*2WG_Especificar aplicació gràfica (només MacOS)
P*2WT_Especificar aplicació tipus eina (eina MPW, només MacOS)
**1X_Opcions de l'executable:
**2Xc_enllaçar amb la biblioteca c
**2Xd_no utilitzar la trajectòria normalitzada de biblioteques (necessària per compilar entre diferents SO)
**2XD_intentar enllaçar les unitats dinàmicament (defineix FPC_LINK_DYNAMIC)
**2XP<x>_avant posar el prefix <x> els noms de les utilitats binaries
**2Xr<x>_posar la trajectòria de les biblioteques a <x> (necessari per compilar entre diferents SO)
**2Xs_treure tots els símbols de l'executable
**2XS_intentar enllaçar les unitats estàticament (predeterminat) (defineix FPC_LINK_STATIC)
**2Xt_enllaçar amb biblioteques estàtiques (-static es passa a l'enllaçador)
**2XX_intentar enllaçar les unitats intel·ligentment (defineix FPC_LINK_SMART)

**1*_
**1?_mostrar aquesta ajuda
**1h_mostrar aquesta ajuda sense esperar
]

#
# The End...
#
