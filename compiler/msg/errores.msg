#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998-2000 by the Free Pascal Development team
#
#   Spanish Language File for Free Pascal
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
general_t_compilername=01000_T_Compilador: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=01001_D_OS de origen: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=01002_I_OS de destino: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Usando el path para ejecutables: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=01004_T_Usando el path para unidades: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Usando el  path para incluidos: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Usando el path para librerias: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Usando el path para objetos: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 L¡neas compiladas, $2 seg
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_No queda memoria disponible
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
general_i_writingresourcefile=01010_I_Writing Resource String Table file: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Writing Resource String Table file: $1
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
scan_f_end_of_file=02000_F_Inesperado fin de fichero
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=02001_F_La cadena excede la l¡nea
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_Caracter ilegal
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Error de sintaxis $1 esperado pero $2 encontrado
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_T_Empezando a leer el fichero a¤adido $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_Nivel de comentario $1 encontrado
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_far_directive_ignored=02006_N_Directiva $F (FAR) ignorada
% The \var{FAR} directive is a 16-bit construction which is recorgnised
% but ignored by the compiler, since it produces 32 bit code.
scan_n_stack_check_global_under_linux=02007_N_El testeo de Stack es global bajo linux
% Stack checking with the \var{-Cs} switch is ignored under \linux, since
% \linux does this for you. Only displayed when \var{-vn} is used.
scan_n_ignored_switch=02008_N_Conmutador de compilador ignorado $1
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Conmutador de compilador ilegal $1
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=02010_W_Este conmutador de compilador tiene un efecto global
% When \var{-vw} is used, the compiler warns if a switch is global.
scan_e_illegal_char_const=02011_E_Constante de caracter ilegal
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range. The range
% is 1-255.
scan_f_cannot_open_input=02012_F_No se puede abrir el fichero $1
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_No se puede abrir el fichero incluido $1
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_too_much_endifs=02014_E_Demasiados $ENDIFs o $ELSEs
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_w_only_pack_records=02015_W_Los campos de los Records pueden ser alinieados solo a 1,2,4 o 16 bytes
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=02016_W_Los enumerados solo pueden ser salvados en 1,2 o 4 bytes
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=02017_E_$1 esperado para $2 definido en la l¡nea $3
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Error de sintaxis mientras se procesaba una expresi¢n de compilaci¢n condicional
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_e_error_in_preproc_expr=02019_E_Evaluando una expresi¢n de compilaci¢n condicional
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_w_macro_cut_after_255_chars=02020_W_Los contenidos de la macro son cortados despu‚s del caracter 255 para evaluar la expresi¢n
% The contents of macros cannot be longer than 255 characters. This is a
% safety in the compiler, to prevent buffer overflows. This is shown as a
% warning, i.e. when the \var{-vw} switch is used.
scan_e_endif_without_if=02021_E_ENDIF sin IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_El usuario definio: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_El usuario definio: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_El usuario definio: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_El usuario definio: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_El usuario definio: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_El usuario definio: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_Palabra clave redefinida como macro no tiene efecto
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_Desbordamiento del buffer de macro mi‚ntras se le¡a o se expand¡a
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=02030_W_La extension de macros excede una profundidad de 16.
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_wrong_styled_switch=02031_E_conmutadores de compilador no estan permitidos en comentarios de estilo (* ... *)
% Compiler switches should always be between \var{\{ \}} comment delimiters.
scan_d_handling_switch=02032_D_Manejando el conmutador "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_C_ENDIF $1 encontrado
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_C_IFDEF $1 encontrado, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_C_IFOPT $1 encontrado, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_C_IF $1 encontrado, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_C_IFNDEF $1 encontrado, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_C_ELSE $1 encontrado, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_C_Omitiendo hasta...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_Presiona <retorno> para continuar
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Conmutador $1 no soportado
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_Directiva ilegal del compilador $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_T_Vuelve en $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Tipo de aplicaci¢n no soportada: $1
% You get this warning, ff you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_APPTYPE no es soportado por el OS de destino
% The \var{\{\$APPTYPE\}} directive is supported by win32 applications only.
scan_w_decription_not_support=02046_W_DESCRIPTION is only supported for OS2 and Win32
% The \var{\{\$DESCRIPTION\}} directive is only supported for OS2 and Win32 targets.
scan_n_version_not_support=02047_N_VERSION is not supported by target OS.
% The \var{\{\$VERSION\}} directive is only supported by win32 target.
scan_n_only_exe_version=02048_N_VERSION only for exes or DLLs
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_Wrong format for VERSION directive $1
% The \var{\{\$VERSION\}} directive format is major_version.minor_version
% where major_version and minor_version are words.
scan_w_unsupported_asmmode_specifier=02050_W_El estilo de asamblador especificado $1 no es soportado
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_Conmutador de lector de ASM no esta permitido dentre de bloques asm $1 ser  efectivo solo para el siguiente
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=02052_E_Modificador de conmutador err¢neo, usa ON/OFF o +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_Los ficheros de recursos no son soportados por este destino
% The target you are compiling for doesn't support Resource files. The
% only target which can use resource files is Win32
%
% \end{description}
#
# Parser
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
scan_w_include_env_not_found=02054_W_Include environment $1 not found in environment
% The included environment variable can't be found in the environment, it'll
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Illegal value for FPU register limit
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Only one resource file is supported for this target
% The target you are compiling for supports only one resource file. This is the
% case of OS/2 (EMX) currently. The first resource file found is used, the
% others are discarded.
% \end{description}
#
# Parser
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=03000_E_Analizador - Error de sintaxis
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_w_proc_far_ignored=03001_W_Procedure tipo FAR ignorado
% This is a warning. \var{FAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_near_ignored=03002_W_Procedure tipo NEAR ignorado
% This is a warning. \var{NEAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_interrupt_ignored=03003_W_Procedure type INTERRUPT ignored for not i386
% This is a warning. \var{INTERRUPT} is a i386 specific construct
% and is ignored for other processors.
parser_e_dont_nest_interrupt=03004_E_INTERRUPT procedure can't be nested
% An \VAR{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Procedure type $1 ignored
% This is a warning. \var{REGISTER},\var{REINTRODUCE} is ignored by FPC programs for now.
% This is introduced first for Delphi compatibility.
parser_e_no_overload_for_all_procs=03006_E_Not all declarations of $1 are declared with OVERLOAD
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_no_dll_file_specified=03007_E_No se especific¢ un fichero DLL
% No longer in use.
parser_e_export_name_double=03008_E_Los nombre de funciones exportadas de DLL no pueden ser dobles $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Los indices de funciones exportadas de DLL no pueden ser dobles $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_Indice no v lido para la funcion exportada de DLL
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_Relocatable DLL or executable $1 debug info does not work, disabled.
parser_w_parser_win32_debug_needs_WN=03012_W_To allow debugging for win32 code you need to disable relocation with -WN option
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_El nombre del constructor tiene que ser INIT
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_El nombre del destructor tiene que ser DONE
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_illegal_open_parameter=03015_E_Par metro abierto ilegal
% You are trying to use the wrong type for an open parameter.
parser_e_proc_inline_not_supported=03016_E_Procedure tipo INLINE no soportado
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_priv_meth_not_virtual=03017_W_Los m‚todos privados no deben ser virtuales
% You declared a method in the private part of a object (class) as
% \var{virtual}. This is not allowed. Private methods cannot be overridden
% anyway.
parser_w_constructor_should_be_public=03018_W_El constructor deber¡a ser p£blico
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_El destructor deber¡a ser p£blico
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Una clase deber¡a tener solo un destructor
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Las definiciones de clases locales no est n permitidas
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Las definiciones de clases an¢nimas no est n permitidas
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_object_has_no_vmt=03023_E_El objeto $1 no tiene VMT
parser_e_illegal_parameter_list=03024_E_Lista de par metros ilegal
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_type=03025_E_Tipo de par metro err¢neo especificado para el arg num. $1
% There is an error in the parameter list of the function or procedure.
% The compiler cannot determine the error more accurate than this.
parser_e_wrong_parameter_size=03026_E_Cantidad err¢nea de par metros
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_el identificador sobrecargado $1 no es una funci¢n
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_funciones sobrecargadas tienen los mismos par metros
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_la cabecera de la funci¢n no concuerda con la declaraci¢n posterior $1
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=03030_E_la cabecera de la funcion $1 no concuerda con la posterior declaraci¢n : el nombre de la var. cambia $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_solo los valores mayores pueden ser incluidos en un tipo enumerado
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_n_interface_name_diff_implementation_name=03032_N_­ El nombre en Interface e implementation es diferente !
% This note warns you if the implementation and interface names of a
% functions are different, but they have the same mangled name. This
% is important when using overloaded functions (but should produce no error).
parser_e_no_with_for_variable_in_other_segments=03033_E_With no puede ser usado para variables en un segmento diferente
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_Anidado de funciones > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_error de testeo de rango mientras se evaluaban las constantes
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_error de testeo de rango mientras se evaluaban las constantes
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_etiqueta de case duplicada
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_L¡mite superior del rango de case es menor less que el l¡mite inferior
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_constantes de tipo class o object no est n permitidas
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_el asignar variables de tipo function a funciones sobrecargadas no est  permitido
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=03041_E_la longitud de una cadena tiene que ser un valor entre 1 y 255
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
% (This is not true for \var{Longstrings} and \var{AnsiStrings}.
parser_w_use_extended_syntax_for_objects=03042_W_usa la sintaxis estendida de DISPOSE y NEW para generar instancias de objetos
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=03043_W_el uso de NEW o DISPOSE para punteros sin tipo no tiene sentido
parser_e_no_new_dispose_on_void_pointers=03044_E_el uso de NEW o DISPOSE no es posible con punteros sin tipo
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_identificador de clase esperado
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_identificador de tipo no permitido aqu¡
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_identificador de m‚todo esperado
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_la cabecera de la funcion no concuerda con ning£n m‚todo de esta clase $1
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_p_procedure_start=03049_P_procedure/function $1
% When using the \var{-vp} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_Constante ilegal de punto flotante
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL solo puede ser usado en constructores
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Los destructores no pueden tener par metros
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Solo los m‚todos de clases pueden ser referidos con referencias de clase
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
parser_e_only_class_methods=03054_E_Solo los m‚todos de la clase pueden ser accedidos por los m‚todos de esa clase
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
parser_e_case_mismatch=03055_E_Los tipos de una constante y CASE no concuerdan
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_El s¡mbolo no puede ser exportado de una librer¡a
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_Un m‚todo heredado est  oculto por $1
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_No hay un m‚todo en una clase antepasada para ser substituido: $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=03059_E_No se suministro un miembro para acceder a propiedad
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_La directiva Stored prorperty no est  implementada a£n
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_s¡mbolo ilegal para el acceso a propiedad
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code woud cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_No se puede acceder a un campo protegido de un objeto aqu¡
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module wher the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_No se puede acceder a un campo privado de un objeto aqu¡
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_w_overloaded_are_not_both_virtual=03064_W_m‚todo sobrecargado de un m‚todo virtual deber¡a ser virtual: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_w_overloaded_are_not_both_non_virtual=03065_W_m‚todo sobrecargado de un m‚todo no virtual deber¡a ser no virtual: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_e_overloaded_methodes_not_same_ret=03066_E_m‚todos sobrecargados que son virtuales tienen que tener el mismo tipo para retorno: $1
% If you declare virtual overloaded methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_Las funciones declarados como EXPORT no pueden estar anidados
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_los m‚todos no pueden ser EXPORTados
% You cannot declare a procedure that is a method for an object as
% \var{export}ed. That is, your methods cannot be called from a C program.
parser_e_call_by_ref_without_typeconv=03069_E_los llamadas por par metros variables tienen que concordar exactamente
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_La clase no es un pariente de la clase actual
% When calling inherited methods, you are trying to call a method of a strange
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_SELF solo est  permitido en m‚todos
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_los m‚todos solo pueden estar en otros m‚todos llamados directamente con el tipo identificador de la clase
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=03073_E_Uso ilegal de ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_Error de testeo de rango en constructor de set o elemento de set duplicado
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Puntero a objeto esperado
% You specified an illegal type in a \var{New} statement.
% The extended synax of \var{New} needs an  object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_La expresi¢n tiene que ser una llamada al constructor
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_La expresi¢n tiene que ser una llamada al destructor
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_Orden ilegal de elementos de record
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_El tipo de la expresi¢n tiene que ser class o record
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Funciones sin valores de retorno no pueden devolver ningun valor
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures  cannot return a value.
parser_e_constructors_always_objects=03081_E_constructores y destructores tienen que ser m‚todos
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_El operador no esta sobrecargado
% You're trying to use an overloaded operator when it isn't overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Impossible to overload assignment for equal types
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Impossible operator overload
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_Re-incremento no es posible aqu¡
% You are trying to raise an exception where it isn't allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_La sintaxis extendida de new o dispose no est  permitida para una clase
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{Dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_asm_incomp_with_function_return=03087_E_Ensamblador incompatible con el valor de retorno de la funci¢n
% You're trying to implement a \var{assembler} function, but the return type
% of the function doesn't allow that.
parser_e_procedure_overloading_is_off=03088_E_La sobrecarga de funciones no esta activada
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_No es posible sobrecargar este operador (sobrecarga = en lugar de)
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Operador comparativo tiene que devolver un valor booleano
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Solo los m‚todos virtuales pueden ser abstractos
% You are declaring a method as abstract, when it isn't declared to be
% virtual.
parser_f_unsupported_feature=03092_F_­Uso de caracter¡stica no soportada!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_La mezcla de CLASSES y OBJECTS no est  permitida
% You cannot derive \var{objects} and \var{classes} intertwined . That is,
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Directiva de procedure desconocida debe ser ignorada: $1
% The procedure direcive you secified is unknown. Recognised procedure
% directives are \var{cdecl}, \var{stdcall}, \var{popstack}, \var{pascal}
% \var{register}, \var{export}.
parser_e_absolute_only_one_var=03095_E_absolute solo puede estar asociado a UNA variable
% You cannot specify more than one variable before the \var{absolute} directive.
% Thus, the following construct will provide this error:
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
% \item [ absolute can only be associated a var or const ]
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_absolute_only_to_var_or_const=03096_E_absolute solo puede ser asociado a un var o const
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Solo una variable puede ser inicializada
% You cannot specify more than one variable with a initial value
% in Delphi syntax.
parser_e_abstract_no_definition=03098_E_M‚todos abstractos no dever¡an tener ninguna definici¢n (con el cuerpo de la funci¢n)
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Esta funci¢n sobrecargada no puede ser local (tiene que ser exported)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_M‚todos virtuales est n siendo usados sin un constructor en $1
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_m_macro_defined=03101_M_Macro definida: $1
% When \var{-vm} is used, the compiler tells you when it defines macros.
parser_m_macro_undefined=03102_M_Macro indefinida: $1
% When \var{-vm} is used, the compiler tells you when it undefines macros.
parser_m_macro_set_to=03103_M_Macro $1 puesta a $2
% When \var{-vm} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_Compilando $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_U_Parsing interface of unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_U_Parsing implementation of $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_D_Compilando $1 por segunda vez
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_paras_allowed=03108_E_Propiedades de array no est n permitidas en este punto
% You cannot use  array properties at that point.a
parser_e_no_property_found_to_override=03109_E_No se encontro la propiedad a sobreescribir
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Solo una propiedad por defecto esta permitida, encontrada propiedad por defecto heredada en class $1
% You specified a property as \var{Default}, but a parent class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_La propiedad por defecto tiene que ser un array de propieada
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Constructores virtuales son solo soportados en modelo class
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_No hay propiedad por defecto disponible
% You try to access a default property of a class, but this class (or one of
% it's ancestors) doesn't have a default property.
parser_e_cant_have_published=03114_E_La clase no puede tener una secci¢n published, usa el conmutador {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_Declaraci¢n posterior de la clase $1 tiene que ser resuelta aqu¡ para usar la clase como antepasado
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Operadores locales no soportados
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_Directiva de procedure $1 no esta permitida en la secci¢n interface
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_Directiva de procedure $1 no permitida en la secci¢n implementation
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_Directiva de procedure $1 no permitida en declaraci¢n procvar
% This procedure directive cannot be part of a procedural of function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_La funci¢n esta actualmente declarada como Public/Forward $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_No se pueden usar ambos EXPORT y EXTERNAL
% These two procedure directives are mutually exclusive
parser_e_name_keyword_expected=03122_E_palabra clave NAME esperada
% The definition of an external variable needs a \var{name} clause.
parser_w_not_supported_for_inline=03123_W_$1 no est  soportado todav¡a dentro de procedure/function inline
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=03124_W_Inlining deshabilitado
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Escribiendo Browser log $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=03126_H_podr¡a ser que falte la de-referencia del puntero
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_Lector del ensamblador seleccionado no soportado
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=03128_E_Directiva de procedure $1 tiene conflictos con otras directivas
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_Convencion de llamada no concuerda con la posterior
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_register_calling_not_supported=03130_E_Llamadas por registros (fastcall) no soportadas
% The \var{register} calling convention, i.e., arguments are passed in
% registers instead of on the stack is not supported. Arguments are always
% passed on the stack.
parser_e_property_cant_have_a_default_value=03131_E_Propiedad no puede tener un valor por defecto
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_El valor por defecto de una propiedad tiene que ser constante
% The value of a \var{default} declared property must be knwon at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_El s¡mbolo no puede ser published, solo puede ser una clase
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Este tipo de propiedad no puede ser published
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_w_empty_import_name=03135_W_Nombre de imporatacion vac¡o
% Both index and name for the import are 0 or empty
parser_e_empty_import_name=03136_W_Nombre de importaci¢n especificado
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_used_proc_name_changed=03137_E_Nombre interno de la funci¢n cambi¢ despu‚s de usar la funci¢n
% This is an internal error; please report any occurrences of this error
% to the \fpc team.
parser_e_division_by_zero=03138_E_Division por cero
% There is a divsion by zero encounted
parser_e_invalid_float_operation=03139_E_Operacion de punto flotante no v lida
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_L¡mite superior del rango es menor que el l¡mite inferior
% The upper bound of a \var{case} label is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_string "$1" is longer than $2
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_La longitud de la cadena es mayor que el array de car cteres
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_Expresi¢n ilegal despu‚s de una directiva mensaje
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Manejadores de mensajes solo pueden tomar una llamada por par. de referencia
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Etiqueta de mensaje duplicada: $1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_Self solo puede ser un par metro explicito en manejadores de mensajes
% The self parameter can be passed only explicit if it is a method which
% is declared as message method handler
parser_e_threadvars_only_sg=03147_E_Threadvars solo pueden ser est ticas o globales
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has it's own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Ensamblador directo no soportado para formato binario de salida
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
% \end{description}
parser_w_no_objpas_use_mode=03149_W_No carges manuamente la unidad OBJPAS, usa {$mode objfpc} o {$mode delphi}
% You're trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea to do, you can better use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automaticly
parser_e_no_object_override=03150_E_OVERRIDE no puede ser usado en objetos
% Override isn't support for objects, use VIRTUAL instead to override
% a method of an anchestor object
% \end{description}
#
# Type Checking
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
parser_e_cant_use_inittable_here=03151_E_Data types which require initialization/finalization can't be used in variant records
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Resourcestrings can be only static or global
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_Exit with argument can't be used here
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_The type of the storage symbol must be boolean
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_This symbol isn't allowed as storage symbol
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=03156_E_Only class which are compiled in $M+ mode can be published
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Procedure directive expected
% When declaring a procedure in a const block you used a ; after the
% procedure declaration after which a procedure directive must follow.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_The value for a property index must be of an ordinal type
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Procedure name to short to be exported
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_No DEFFILE entry can be generated for unit global vars
parser_e_dlltool_unit_var_problem2=03161_E_Compile without -WD option
% \end{description}
#
# Type Checking
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_desconcordancia de tipo
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Tipos incompatibles: cogi¢ "$1" esperado "$2"
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
type_e_not_equal_types=04002_E_Desconcuerdo de tipos entre $1 y $2
% The types are not equal
type_e_type_id_expected=04003_E_Identificador de tipo esperado
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Identificador de variable esperado
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_espression entera esperada
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Boolean expression expected, but got "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Espresi¢n ordinal esperada
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_tipo puntero esperado
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_tipo clase esperado
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_varid_or_typeid_expected=04010_E_Identificador de variable o tipo esperado
% The argument to the \var{High} or \var{Low} function is not a variable
% nor a type identifier.
type_e_cant_eval_constant_expr=04011_E_No se puede evaluar la espresi¢n constante
% No longer in use.
type_e_set_element_are_not_comp=04012_E_Elementos del Set no son compatibles
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Operaci¢n no implementada para sets
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Conversion autom tica de tipo de punto flotante a COMP el cual es un tipo entero
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{Comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_usa DIV para tener un resultado entero
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_tipos string no concuerdan, a cause del modo $V+
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_succ o pred en enumeraciones con asignaciones no es posible
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_No se puede leer o escribir variables de este tipo
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% booleans, reals, pchars and strings can be read from/written to a text file.
type_e_no_readln_writeln_for_typed_file=04019_E_Can't use readln or writeln on typed file
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_Can't use read or write on untyped file.
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Conflicto de tipos entre elementos del set
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(longint/dword) devuelve el word superior/inferior
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Espresi¢n entera o real esperada
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=04024_E_Tipo err¢neo en constructor de array
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Tipo incompatible para el arg. num. $1: Cogi¢ $2, esperado $3
% You are trying to pass an invalid type for the specified parameter.
% \end{description}
type_e_no_method_and_procedure_not_compatible=04026_E_M‚todo (variable) y Procedure (variable) no son compatibles
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_Illegal constant passed to internal math function
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_Can't get the address of constants
% It's not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
#
# Symtable
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
type_e_argument_cant_be_assigned=04029_E_Argument can't be assigned to
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they can't be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_Can't assign local procedure/function to procedure variable
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Can't assign values to an address
% It's not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Can't assign values to const variable
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
sym_e_id_not_found=05000_E_Identificador no encontrado $1
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Error interno en SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Identificador duplicado $1
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_El identificador ya est  definido en $1 en la l¡nea $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Identificador desconocido $1
% The identifier encountered hasn't been declared, or is used outside the
% scope where it's defined.
sym_e_forward_not_resolved=05005_E_Declaraci¢n posterior no solucionada $1
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_f_id_already_typed=05006_F_Identificador de tipo ya est  definido como un tipo
% You are trying to redefine a type.
sym_e_error_in_type_def=05007_E_Error en definici¢n de tipo
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is err¢neous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_type_id_not_defined=05008_E_Identificador de tipo no definido
% The type identifier has not been defined yet.
sym_e_forward_type_not_resolved=05009_E_Tipo posterior no resuelto $1
% The compiler encountered an unknown type.
sym_e_only_static_in_static=05010_E_Solo las variables est ticas pueden ser usadas en m‚todos est ticos o fuera de m‚todos
% A static method of an object can only access static variables.
sym_e_invalid_call_tvarsymmangledname=05011_E_Llamada inv lida a tvarsym.mangledname()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_f_type_must_be_rec_or_class=05012_F_tipo record o class esperado
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Instancias de clases u objetos con un m‚todo abstracto no est n permitidas
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Etiqueta no definida $1
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Label used but not defined $1
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Declaraci¢n de etiqueta ilegal
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO y LABEL no son soportados (usa conmutador -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the  \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Etiqueta no encontrada
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_el identificador no es una etiqueta
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_la etiqueta no est  definida todav¡a
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_declaraci¢n ilegal de tipo en elementos de set
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Definici¢n posterior de clase no resuelta $1
% You declared a class, but you didn't implement it.
sym_n_unit_not_used=05023_H_Unit $1 not used in $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Par metro no usado $1
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=05025_N_Variable local no usada $1
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_Value parameter $1 is assigned but never used
% This is a warning. The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_Local variable $1 is assigned but never used
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Local $1 $2 is not used
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_Private field $1.$2 is never used
sym_n_private_identifier_only_set=05030_N_Private field $1.$2 is assigned but never used
sym_n_private_method_not_used=05031_N_Private method $1.$2 never used


sym_e_set_expected=05032_E_Tipo set esperado
% The variable or expression isn't of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_El resultado de la funci¢n no parece estar puesto
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_Type $1 is not aligned correctly in current record for C
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Identificador de campo de record desconocido $1
% The field doesn't exist in the record definition.
sym_n_uninitialized_local_variable=05036_W_La variable local $1 no parace ser inicializada
sym_n_uninitialized_variable=05037_W_Variable $1 does not seem to be initialized
% These messages are displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_identifier idents no member $1
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the class you are trying to create. The procedure you specified
% does not exist.
sym_b_param_list=05039_B_declaraci¢n encontrada: $1
% You get this when you use the \var{-vb} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
% \end{description}
#
# Codegenerator
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
sym_e_segment_too_large=05040_E_Data segment too large (max. 2GB)
% You get this when you declare an array whose size exceeds the 2GB limit.
% \end{description}
#
# Codegenerator
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_break_not_allowed=06000_E_BREAK no est  permitido
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06001_E_CONTINUE no est  permitido
% You're trying to use \var{continue} outside a loop construction.
cg_e_too_complex_expr=06002_E_Espresi¢n demasiado complicada - desboradamiento del stack de FPU
% Your expression is too long for the compiler. You should try dividing the
% construct over multiple assignments.
cg_e_illegal_expression=06003_E_espresi¢n ilegal
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
cg_e_invalid_integer=06004_E_Entero inv lido
% You made an exression which isn't an integer, and the compiler expects the
% result to be an integer.
cg_e_invalid_qualifier=06005_E_Qualificador ilegal
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
cg_e_upper_lower_than_lower=06006_E_L¡mite del rango superior < al l¡mite del rango inferior
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
cg_e_illegal_count_var=06007_E_Variable de contador ilegal
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
cg_e_cant_choose_overload_function=06008_E_No se puede determinar a cual funci¢n sobrecargada llamar
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
cg_e_parasize_too_big=06009_E_La lista de par metros excede los 65535 bytes
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_illegal_type_conversion=06010_E_Conversi¢n de tipos ilegal
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
cg_d_pointer_to_longint_conv_not_portable=06011_D_Conversiones entre ordinales y punteros no son portables entre plataformas
% If you typecast a pointer to a longint, this code will not compile
% on a machine using 64bit for pointer storage.
cg_e_file_must_call_by_reference=06012_E_Los tipos File tienen que ser par metros variables
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_El uso de punteros lejanos (far) no est  permitido ahi
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_var_must_be_reference=06014_E_llamada ilegal por par metros de referencia
% You are trying to pass a constant or an expression to a procedure that
% requires a \var{var} parameter. Only variables can be passed as a \var{var}
% parameter.
cg_e_dont_call_exported_direct=06015_E_las funciones declaradas como EXPORT no pueden ser llamadas
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Posible llamada ilegal de constructor o destructor (no concuerda con este contexto)
% No longer in use.
cg_n_inefficient_code=06017_N_c¢digo ineficiente
% You construction seems dubious to the compiler.
cg_w_unreachable_code=06018_W_este c¢digo no se ejecutar  nunca
% You specified a loop which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_stackframe_with_esp=06019_E_llamada a procedure con stackframe ESP/SP
% The compiler enocountered a procedure  or function call inside a
% procedure that uses a \var{ESP/SP} stackframe. Normally, when a call is
% done the procedure needs a \var{EBP} stackframe.
cg_e_cant_call_abstract_method=06020_E_Los m‚todos abstractos no pueden ser llamados directamente
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_f_internal_error_in_getfloatreg=06021_F_Error interno en getfloatreg(), fallo de alojamiento
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_unknown_float_type=06022_F_tipo flotante desconocido
% The compiler cannot determine the kind of float that occurs in an expression.
cg_f_secondvecn_base_defined_twice=06023_F_base de SecondVecn() definida dos veces
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_extended_cg68k_not_supported=06024_F_El tipo Extended no es soportado en cg68k
% The var{extended} type is not supported on the m68k platform.
cg_f_32bit_not_supported_in_68000=06025_F_el entero sin signo de 32-bit no est  soportado en modo MC68000
% The cardinal is not supported on the m68k platform.
cg_f_internal_error_in_secondinline=06026_F_Error interno en secondinline()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_d_register_weight=06027_D_Registro $1 peso $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_e_stacklimit_in_local_routine=06028_E_El l¡mite del Stack ha sido excedido in una rutina local
% Your code requires a too big stack. Some operating systems pose limits
% on the stack size. You should use less variables or try to put large
% variables on the heap.
cg_d_stackframe_omited=06029_D_El frame del Stack es omitido
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_w_64bit_range_check_not_supported=06030_W_Range check for 64 bit integers is not supported on this target
% 64 bit range check is not yet implemented for 32 bit processors.
cg_e_unable_inline_object_methods=06031_E_Incapaz de "inline" m‚todos del objeto
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Incapaz de "inline" llamadas a procvar
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_No hay informacion de c¢digo almacenada para "inline" procedure
% The compiler couldn't store code for the inline procedure.
cg_e_no_call_to_interrupt=06034_E_Direct call of interrupt procedure $1 is not possible
% You can not call an interrupt procedure directly from FPC code
cg_e_can_access_element_zero=06035_E_Elemento cero de un ansi/wide- o longstring no puede ser accedido, usa (set)length
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such kinf of string
cg_e_include_not_implemented=06036_E_Include or exclude not implemented in this case
% \var{include} and \var{exclude} are only partially
% implemented for \var{i386} processors
% and not at all for \var{m68k} processors.
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructores o destructores no pueden ser llamados aqu¡ dentro
% Inside a \var{With} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_No se puede llamar al m‚todo manejador de mensaje directamente
% A message method handler method can't be called directly if it contains an
% explicit self argument
% \end{description}
# EndOfTeX

#
# Assembler reader
#
cg_e_goto_inout_of_exception_block=06039_E_Jump in or outside of an exception block
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
cg_e_control_flow_outside_finally=06040_E_Control flow statements aren't allowed in a finally block
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
asmr_d_start_reading=07000_D_Empezado analizaci¢n de ensamblador estilo $1
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_D_Finalizado analizaci¢n de ensamblador estilo $1
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Una directiva que no es una etiqueta contiene @
% A identifier which isn't a label can't contain a @.
asmr_w_override_op_not_supported=07003_W_Operador sobreescrito no soportado
% The Override operator is not supported
asmr_e_building_record_offset=07004_E_Error construyendo desplazamiento de record
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET usado sin identificador
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE used without identifier
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_No se puede usar variable local o par metro aqu¡
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the %ebp register so the
% address can't be get directly.
asmr_e_need_offset=07008_E_necesita usar OFFSET aqu¡
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_need to use $ here
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_No se pueden usar multiples s¡mbolos realojables
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_S¡mbolo realojable solo puede ser a¤adido
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Espresi¢n constante inv lida
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_S¡mbolo realojable no permitido
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Sintaxis de referencia inv lida
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_You can not reach $1 from that code
% You can not read directly the value of local or para
% of a higher level in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Local symbols/labels aren't allowed as references
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Base inv lida, uso de registro ¡ndice
% There is an error with the base and index register
asmr_w_possible_object_field_bug=07018_W_Possible error in object field handling
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Factor de escala err¢neo
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Uso de multiples registros ¡ndice
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Tipo del operando no v lido
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_Cadena no v lida como operando opcode: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE y @DATA no soportados
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Referencias a etiqueta nula no permitida
asmr_e_expr_zero_divide=07025_E_Divide by zero in asm evaluator
asmr_e_expr_illegal=07026_E_Illegal expression
asmr_e_escape_seq_ignored=07027_E_Sequencia de escape ignorada: $1
asmr_e_invalid_symbol_ref=07028_E_s¡mbolo de referencia no v lido
asmr_w_fwait_emu_prob=07029_W_Fwait puede causar problemas de emulaci¢n con emu387
asmr_w_fadd_to_faddp=07030_W_FADD without operand translated into FADDP
asmr_w_enter_not_supported_by_linux=07031_W_ENTER instruction is not supported by Linux kernel
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Llamando una funcion sobrecargada en asm
asmr_e_unsupported_symbol_type=07033_E_Tipo de s¡mbolo no soportado para operando
asmr_e_constant_out_of_bounds=07034_E_Valor constante fuera de l¡mites
asmr_e_error_converting_decimal=07035_E_Error convirtiendo decimal $1
asmr_e_error_converting_octal=07036_E_Error convirtiendo octal $1
asmr_e_error_converting_binary=07037_E_Error convirtiendo binario $1
asmr_e_error_converting_hexadecimal=07038_E_Error convirtiendo hexadecimal $1
asmr_h_direct_global_to_mangled=07039_H_$1 traducido a $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 esta asociado a una funci¢n sobrecargada
asmr_e_cannot_use_SELF_outside_a_method=07041_E_No se puede usar SELF fuera de un m‚todo
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_No se puede usar OLDEBP fuera de un procedure anidado
asmr_e_void_function=07043_W_Procedures no pueden devolver ning£n valor en c¢digo asm
asmr_e_SEG_not_supported=07044_E_SEG no soportado
asmr_e_size_suffix_and_dest_dont_match=07045_E_Tama¤o de sufijo y destinaci¢n o tama¤o del origen no concuerdan
asmr_w_size_suffix_and_dest_dont_match=07046_W_Tama¤o de sufijo y destinaci¢n o tama¤o del origen no concuerdan
asmr_e_syntax_error=07047_E_Error de sintaxis de ensamblador
asmr_e_invalid_opcode_and_operand=07048_E_Combinaci¢n no v lida de opcode y operandos
asmr_e_syn_operand=07049_E_Error de sintaxis de ensamblador en operando
asmr_e_syn_constant=07050_E_Error de sintaxis de ensamblador en constante
asmr_e_invalid_string_expression=07051_E_Espresi¢n de cadena no v lida
asmr_w_const32bit_for_address=07052_W_Constante de 32bit creada para direcci¢n
asmr_e_unknown_opcode=07053_E_Unrecognized opcode $1
asmr_e_invalid_or_missing_opcode=07054_E_Inv lido o falta opcode
asmr_e_invalid_prefix_and_opcode=07055_E_Combinaci¢n no v lida de prefijo y opcode: $1
asmr_e_invalid_override_and_opcode=07056_E_Combinaci¢n no v lida de override y opcode: $1
asmr_e_too_many_operands=07057_E_Demasiados operandos en la l¡nea
asmr_w_near_ignored=07058_W_NEAR ignorado
asmr_w_far_ignored=07059_W_FAR ignorado
asmr_e_dup_local_sym=07060_E_S¡mbolo local duplicado $1
asmr_e_unknown_local_sym=07061_E_S¡mbolo local indefinido $1
asmr_e_unknown_label_identifier=07062_E_Identificador de etiqueta desconocido $1
asmr_e_invalid_register=07063_E_Invalid register name
asmr_e_invalid_fpu_register=07064_E_Nombre de registro de punto flotante no v lido
asmr_e_nor_not_supported=07065_E_NOR no soportado
asmr_w_modulo_not_supported=07066_W_Modulo no soportado
asmr_e_invalid_float_const=07067_E_Constante de punto flotante no v lida $1
asmr_e_invalid_float_expr=07068_E_Espresi¢n de punto flotante no v lida
asmr_e_wrong_sym_type=07069_E_Tipo de s¡mbolo err¢neo
asmr_e_cannot_index_relative_var=07070_E_No se puede indexar una var. local o un par metro con un registro
asmr_e_invalid_seg_override=07071_E_Espresi¢n de segmento sobreescrito no v lida
asmr_w_id_supposed_external=07072_W_Identificador $1 supuesto externo
asmr_e_string_not_allowed_as_const=07073_E_Strings no permitidos como constantes
asmr_e_no_var_type_specified=07074_E_No hay tipo de variable especificado
asmr_w_assembler_code_not_returned_to_text=07075_E_C¢digo ensamblador no devuelto a secci¢n de texto
asmr_e_not_directive_or_local_symbol=07076_E_No es una directiva o un s¡mbolo local $1
asmr_w_using_defined_as_local=07077_E_Usando un nombre definido como una etiqueta local

#
# Assembler/binary writers
#
asmr_e_dollar_without_identifier=07078_E_Dollar token is used without an identifier
asmr_w_32bit_const_for_address=07079_W_32bit constant created for address
asmr_n_align_is_target_specific=07080_N_.align is target specific, use .balign or .p2align
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Can't access fields directly for parameters
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_Can't access fields of objects/classes directly
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
#
# Assembler/binary writers
#
asmw_f_too_many_asm_files=08000_F_Demasiados archivos de ensamblador abiertos
asmw_f_assembler_output_not_supported=08001_F_Ensamblador de salida seleccionado no soportado
asmw_f_comp_not_supported=08002_F_Comp no soportado
asmw_f_direct_not_supported=08003_F_Direct no soportado por escritores binarios
asmw_e_alloc_data_only_in_bss=08004_E_El alojamiento de datos solo est  permitido en secci¢n bss
asmw_f_no_binary_writer_selected=08005_F_No hay seleccionado un escritor binario
asmw_e_opcode_not_in_table=08006_E_Asm: Opcode $1 no esta en la tabla
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 combinacion invalida de opcode y operandos
asmw_e_16bit_not_supported=08008_E_Asm: Referencias de 16 Bit no soportadas
asmw_e_invalid_effective_address=08009_E_Asm: Direccion efectiva no valida
asmw_e_immediate_or_reference_expected=08010_E_Asm: Inmediato o referencia esperado
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 valor excede los l¡mites $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Salto corto es fuera de rango $1


#
# Executing linker/assembler
#
asmw_e_undefined_label=08013_E_Asm: Undefined label $1


#
# Executing linker/assembler
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Sistema operativo de origen redefinido
exec_i_assembling_pipe=09001_I_Ensamblando (pipe) $1
exec_d_cant_create_asmfile=09002_E_No se puede crear fichero ensamblador $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_objectfile=09003_E_Can't create object file: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_archivefile=09004_E_Can't create archive file: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_w_assembler_not_found=09005_W_Ensamblador $1 no encontrado, cambiando a ensamblado externo
exec_t_using_assembler=09006_T_Usando ensamblador: $1
exec_w_error_while_assembling=09007_W_Error mientras se ensamblaba c¢digo de salida $1
exec_w_cant_call_assembler=09008_W_No se puede llamar al ensamblador, error $1 cambiando a ensamblado externo
exec_i_assembling=09009_I_Ensamblando $1
exec_i_assembling_smart=09010_I_Assembling smartlink $1
exec_w_objfile_not_found=09011_W_Objeto $1 no encontrado, Enlace podr¡a fallar !
exec_w_libfile_not_found=09012_W_Librer¡a $1 no encontrada, Enlace podr¡a fallar !
exec_w_error_while_linking=09013_W_Error mientras se enlazaba
exec_w_cant_call_linker=09014_W_No se puede llamar al enlazador, cambiando al enlazador externo
exec_i_linking=09015_I_Enlazando $1
exec_w_util_not_found=09016_W_Util $1 not found, switching to external linking
exec_t_using_util=09017_T_Using util $1
exec_e_exe_not_supported=09018_E_Creation of Executables not supported
exec_e_dll_not_supported=09019_E_Librer¡as dinamicas no soportadas
exec_i_closing_script=09020_I_Cerrando script $1
exec_w_res_not_found=09021_W_compilador de resources no encontrado, cambiando a modo externo
exec_i_compilingresource=09022_I_Compilando resource $1

#
# Executable information
#
execinfo_f_cant_process_executable=09023_F_No se puede post-procesar el ejecutable $1
execinfo_f_cant_open_executable=09024_F_No se puede abrir el ejecutable $1
execinfo_x_codesize=09025_X_Tama¤o de C¢digo: $1 bytes
execinfo_x_initdatasize=09026_X_Tama¤o de datos inicializados: $1 bytes
execinfo_x_uninitdatasize=09027_X_Tama¤o de datos sin inicializar: $1 bytes
execinfo_x_stackreserve=09028_X_Espacio reservado para Stack: $1 bytes
execinfo_x_stackcommit=09029_X_Stack space commited: $1 bytes

# Unit loading
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=10000_T_Buscando unidad: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_Cargando PPU $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_PPU Nombre: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_PPU Banderas: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_PPU Crc: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_PPU Fecha: $1
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=10006_U_Fichero PPU demasiado corto
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_invalid_header=10007_U_Cabecera PPU inv lida (no est  PPU al principio)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_Versi¢n $1 de PPU no es v lida
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_PPU est  compilada para otro procesador
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=10010_U_PPU est  compilada para otro destino
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=10011_U_Fuente PPU: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_write=10012_U_Escribiendo $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_No se puede escribir el fichero PPU
% An err
unit_f_ppu_read_error=10014_F_leyendo fichero PPU
% Unexpected end of file
unit_f_ppu_read_unexpected_end=10015_F_inexperado fin de fichero PPU
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_invalid_entry=10016_F_Entrada inv lida en fichero PPU: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_Problema contando PPU Dbx
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Nombre de unidad ilegal: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=10019_F_Demasiades unidades
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Referencia circular entre unidades $1 y $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_No se puede compilar la unidad $1, no hay fuentes disponibles
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_Can't find unit $1
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_unit_name_error=10023_W_Unit $1 was not found but $2 exists
unit_f_unit_name_error=10024_F_Unit $1 searched but $2 found
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_Compilar la unidad System requiere el conmutador -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_Ha habido $1 errors compilando el modulo, parando
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Cargado de $1 ($2) la unidad $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_Recompilando $1, checksum cambiado por $2
unit_u_recompile_source_found_alone=10029_U_Recompilando $1, solo se encontr¢ el fuente
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Recompilando unidad, lib est tica es m s vieja que ppu
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Recompilando unidad, lib compartida es m s vieja que ppu
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Recompilando unidad, obj y asm son m s viejos que ppu
% When you use the \var{-vu} flag, the compiler warns if the assembler of
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Recompilando unidad, obj es m s viejo que asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_start_parse_interface=10034_U_Analizando interface de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_start_parse_implementation=10035_U_Analizando implementation de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_Segunda carga para la unidad $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_Chequeo de fichero PPU $1 fecha $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
% \end{description}
# EndOfTeX

#
#  Options
#
option_usage=11000_$1 [opciones] <fichero_entrada> [opciones]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Solo es soportado un fichero fuente
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_Fichero DEF solo puede ser creado para OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_Ficheros de respuesta anidados no son soportados
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_No hay fichero fuente en l¡nea de comandos
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_No option inside $1 config file
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Par metro ilegal: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? escribe las p ginas de ayuda
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Demasiados ficheros de configuraci¤on anidados
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Imposible abrir fichero $1
% The option file cannot be found.
option_reading_further_from=11010_N_Leyendo opciones adicionales de $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Destino est  ya puesto a: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Librer¡as compartidas no soportadas en la plataforma DOS, revocando a est ticas
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_demasiados IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_demasiados ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_condicional abierto al final del fichero
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_La generaci¢n de informaci¢n de depuraci¢n no es soportada por este ejecutable
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Prueba recompilando con -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_E_Estas usando el conmutador obsoleto $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_E_Est s usando el conmutador obsoleto $1, porfavor usa $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_Cambi ndo a ensamblador de escritura de c¢digo fuente por defecto
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_Assembler output selected "$1" is not compatible with "$2"
option_asm_forced=11022_W_"$1" assembler use forced
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
%\end{description}
# EndOfTeX

#
# Logo (option -l)
#
option_logo=11023_[
Free Pascal Compiler versi¢n $FPCVER [$FPCDATE] para $FPCTARGET
Copyright (c) 1998-2000 por Florian Klaempfl
]

#
# Info (option -i)
#
option_info=11024_[
Free Pascal Compiler versi¢n $FPCVER

Fecha del compilador   : $FPCDATE
Destino del compilador : $FPCTARGET

Este programa viene bajo la Licencia P£blica General de GNU
Para m s informaci¢n lea COPYING.FPC

Comunicanos fallos, sugerencias, etc a:
                 bugrep@freepascal.org
]

#
# Help pages (option -? and -h)
#
option_help_pages=11025_[
**0*_pon + despu‚s de un conmutador boleano para activarlo, - para desactivarlo
**1a_el compilador no borrar  el fichero ensamblador generado
**2al_muestra las l¡neas de c¢digo fuente en el fichero ensamblador
**2ar_mostrar alojamiento/desaloj. registro info. en fichero ensamblador
**2at_mostrar alojamiento/desaloj. temporal info. en fichero ensamblador
**1b_generar informaci¢n de navegador
**2bl_generar informacion de s¡mbolos locales
**1B_construir todos los m¢dulos
**1C<x>_code generation options:
3*2CD_crear librer¡a din mica
**2Ch<n>_<n> bytes de heap (entre 1023 y 67107840)
**2Ci_chequeo de E/S
**2Cn_omitir enlazado
**2Co_chequea el desbordamiento de operaciones con enteros
**2Cr_chequeo de rango
**2Cs<n>_pone el tama¤o del stack a <n>
**2Ct_chequeo del stack
3*2CS_crear librer¡a est tica
3*2Cx_usar enlaze-inteligente
**1d<x>_define el s¡mbolo <x>
*O1D_genera un fichero DEF
*O2Dd<x>_pone la descripci¢n a <x>
*O2Dw_aplicacion PM
**1e<x>_pone el path a ejecutables
**1E_igual a -Cn
**1F_pone nombres de ficheros y paths
**2FD<x>_pone el directorio donde buscar para utilidades del compilador
**2Fe<x>_redirecciona la salida de errores a <x>
**2FE<x>_pone el path de destino de ejecutable/unidad a <x>
**2Fi<x>_a¤ade <x> al path de incluidos
**2Fl<x>_a¤ade <x> al path de librerias
*L2FL<x>_usa <x> como enlazador din mico
**2Fo<x>_a¤ade <x> al path de objetos
**2Fr<x>_carga fichero de mensages de error <x>
**2Fu<x>_a¤ade <x> al path de unidades
**2FU<x>_pone el path de destino de unidades a <x>, anula -FE
*g1g_genera informaci¢n de depuraci¢n
*g2gg_usa gsym
*g2gd_usa dbx
*g2gh_usa la unidad de trazado del heap
*g2gc_generate checks for pointers
**1i_informaci¢n
**2iD_devuelve la fecha del compilador
**2iV_devuelve la versi¢n del compilador
**2iSO_devuelve el OS de origen
**2iSP_devuelve el procesador de origen
**2iTO_devuelve el OS de destino
**2iTP_devuelve el procesador de destino
**1I<x>_a¤ade <x> al path de a¤adidos
**1k<x>_Pasa <x> al enlazador
**1l_escribe logotipo
**1n_no lee el fichero de configuraci¢n por defecto
**1o<x>_cambia el nombre del ejecutable producido a <x>
**1pg_genera c¢digo de perfil para gprof
*L1P_usa pipes en vez de crear ficheros ensamblador temporales
**1S_opciones de sintaxis
**2S2_habilita algunas extensiones de Delphi 2
**2Sc_soportar operadores tipo C (*=,+=,/= y -=)
**2Sd_intenta ser compatible con Delphi
**2Se_el compilador se para despu‚s del primer error
**2Sg_permite LABEL y GOTO
**2Sh_Usar cadenas ansi (ansistrings)
**2Si_soportar INLINE estilo C++
**2Sm_soportar macros tipo C (globales)
**2So_intenta ser compatible con TP/BP 7.0
**2Sp_intenta ser compatible con gpc
**2Ss_nombre del constructor tiene que ser init (destructor tiene que ser done)
**2St_permite la palabra clave static en objetos
**1s_no llama al ensamblador ni al enlazador (solo con -a)
**1u<x>_indefine el s¡mbolo <x>
**1U_opciones de unidad
**2Un_no testea el nombre de unidad
**2Us_compila una unidad de sistema
**1v<x>_Ser explicativo. <x> es una combinaci¢n de las siguientes letras :
**2*_e : muestra errores (defecto)   d : muestra info de depuraci¢n
**2*_w : muestra advertencias        u : muestra info de unidades
**2*_n : muestra notas               t : muestra ficheros inten./usados
**2*_h : muestra trucos              m : muestra macros definidas
**2*_i : muestra informaci¢n general p : muestra procedures compilados
**2*_l : muestra n£mero de l¡nea     c : muestra condicionales
**2*_a : muestra todo                0 : no muestra nada (excepto errores)
**2*_b : muestra toda la declaraci¢n r : modo compatible con Rhide/GCC
**2*_    del procedure si ocurre     x : info. de ejecutable (solo Win32)
**2*_    un error
**1X_opciones del ejecutable
*L2Xc_enlaza con la librer¡a c
**2XD_enlaza con librer¡as din micass (define FPC_LINK_DYNAMIC)
**2Xs_elimina todos los s¡mbolos del ejecutable
**2XS_enlaza con librer¡as est ticas (define FPC_LINK_STATIC)
**0*_Opciones espec¡ficas del procesador:
3*1A<x>_formato de salida
3*2Aas_fichero coff usando GNU AS
3*2Aasaout_fichero coff usando GNU AS for aout (Go32v1)
3*2Anasmcoff_coff (Go32v2) file using Nasm
3*2Anasmelf_elf32 (Linux) file using Nasm
3*2Anasmobj_obj file using Nasm
3*2Amasm_obj usando Masm (Microsoft)
3*2Atasm_obj usando Tasm (Borland)
3*2Acoff_coff (Go32v2) using internal writer
3*2Apecoff_pecoff (Win32) using internal writer
3*1R<x>_tipo de lectura de ensamblador
3*2Ratt_lee ensamblador estilo AT&T
3*2Rintel_lee ensamblador estilo Intel
3*2Rdirect_copiar texto ensamblador directamente al fichero ensamblador
3*1O<x>_optimizaciones
3*2Og_generar c¢digo m s peque¤o
3*2OG_generar c¢digo m s r pido (defecto)
3*2Or_mantener ciertas variables en registros (todab¡a con fallos!!!)
3*2Ou_habilita las optimizaciones inciertas (mira la documentaci¢n)
3*2O1_nivel 1 de optimizaci¢n (optimizaciones r pidas)
3*2O2_nivel 2 de optimizaci¢n (-O1 + optimizaciones lentas)
3*2O3_nivel 3 de optimizaci¢n (igual que -O2u)
3*2Op_procesador de destino
3*3Op1_pone el procesador de destino en 386/486
3*3Op2_pone el procesador de destino en Pentium/PentiumMMX (tm)
3*3Op3_pone el procesador de destino en PPro/PII/c6x86/K6 (tm)
3*1T<x>_Sistema operativo de destino
3*2TGO32V2_versi¢n 2 del extensor del DOS de DJ Delorie
3*2TLINUX_Linux
3*2TOS2_OS/2
3*2TWIN32_Windows 32 Bit
6*1A<x>_formato de salida
6*2Aas_Unix o-file usando GNU AS
6*2Agas_Ensamblador GNU Motorola
6*2Amit_Sintaxis MIT (antiguo GAS)
6*2Amot_Ensamblador estandard de Motorola
6*1O_optimizaciones
6*2Oa_enchega el optimizador
6*2Og_generar el c¢digo m s peque¤o
6*2OG_generar el c¢digo m s r pido (defecto)
6*2Ox_optimizar al m ximo (todab¡a con fallos!!!)
6*2O2_pone el procesador de destino para un MC68020+
6*1R<x>_estilo de lectura de ensamblador
6*2RMOT_lee el estilo de ensamblador de motorola
6*1T<x>_Sistema operativo de destino
6*2TAMIGA_Commodore Amiga
6*2TATARI_Atari ST/STe/TT
6*2TMACOS_Macintosh m68k
6*2TLINUX_Linux-68k
**1*_
**1?_muestra esta ayuda
6*1R<x>_assembler reading style:
6*2RMOT_read motorola style assembler
6*1T<x>_Target operating system:
6*2TAMIGA_Commodore Amiga
6*2TATARI_Atari ST/STe/TT
6*2TMACOS_Macintosh m68k
6*2TLINUX_Linux-68k
**1*_
**1?_shows this help
**1h_muestra esta ayuda sin esperar
]

#
# The End...
#
