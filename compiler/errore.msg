#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2000 by the Free Pascal Development team
#
#   English (default) Language File for Free Pascal
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
general_d_sourceos=D_Compiler OS: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=I_Target OS: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=T_Using executable path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=T_Using unit path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=T_Using include path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=T_Using library path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=T_Using object path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=I_$1 Lines compiled, $2 sec
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=F_No memory left
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
general_i_writingresourcefile=I_Writing Resource String Table file: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=E_Writing Resource String Table file: $1
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
scan_f_end_of_file=F_Unexpected end of file
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=F_String exceeds line
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=F_illegal character
% An illegal character was encountered in the input file.
scan_f_syn_expected=F_Syntax error, $1 expected but $2 found
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=T_Start reading includefile $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=W_Comment level $1 found
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_far_directive_ignored=N_$F directive (FAR) ignored
% The \var{FAR} directive is a 16-bit construction which is recorgnised
% but ignored by the compiler, since it produces 32 bit code.
scan_n_stack_check_global_under_linux=N_Stack check is global under Linux
% Stack checking with the \var{-Cs} switch is ignored under \linux, since
% \linux does this for you. Only displayed when \var{-vn} is used.
scan_n_ignored_switch=N_Ignored compiler switch $1
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=W_Illegal compiler switch $1
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=W_This compiler switch has a global effect
% When \var{-vw} is used, the compiler warns if a switch is global.
scan_e_illegal_char_const=E_Illegal char constant
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range. The range
% is 1-255.
scan_f_cannot_open_input=F_Can't open file $1
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=F_Can't open include file $1
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.

scan_e_too_much_endifs=E_Too many $ENDIFs or $ELSEs
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_w_only_pack_records=W_Records fields can be aligned to 1,2,4 or 16 bytes only
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=W_Enumerated can be saved in 1,2 or 4 bytes only
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=E_$ENDIF expected for $1 $2 defined in line $3
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=E_Syntax error while parsing a conditional compiling expression
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_e_error_in_preproc_expr=E_Evaluating a conditional compiling expression
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_w_marco_cut_after_255_chars=W_Macro contents is cut after char 255 to evalute expression
% The contents of macros canno be longer than 255 characters. This is a
% safety in the compiler, to prevent buffer overflows. This is shown as a
% warning, i.e. when the \var{-vw} switch is used.
scan_e_endif_without_if=E_ENDIF without IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=F_User defined: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=E_User defined: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=W_User defined: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=N_User defined: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=H_User defined: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=I_User defined: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=E_Keyword redefined as macro has no effect
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=F_Macro buffer overflow while reading or expanding a macro
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=W_Extension of macros exceeds a deep of 16.
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_wrong_styled_switch=E_compiler switches aren't allowed in (* ... *) styled comments
% Compiler switches should always be between \var{\{ \}} comment delimiters.
scan_d_handling_switch=D_Handling switch "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=C_ENDIF $1 found
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=C_IFDEF $1 found, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=C_IFOPT $1 found, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=C_IF $1 found, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=C_IFNDEF $1 found, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=C_ELSE $1 found, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=C_Skipping until...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=I_Press <return> to continue
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=W_Unsupported switch $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=W_Illegal compiler directive $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=T_Back in $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=W_Unsupported application type: $1
% You get this warning, ff you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=W_APPTYPE isn't support by the target OS
% The \var{\{\$APPTYPE\}} directive is supported by win32 applications only.
scan_w_decription_not_support=W_DESCRIPTION is only supported for OS2 and Win32
% The \var{\{\$DESCRIPTION\}} directive is only supported for OS2 and Win32 targets.
scan_n_version_not_support=N_VERSION is not supported by target OS.
% The \var{\{\$VERSION\}} directive is only supported by win32 target.
scan_n_only_exe_version=N_VERSION only for exes or DLLs
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=W_Wrong format for VERSION directive $1
% The \var{\{\$VERSION\}} directive format is major_version.minor_version
% where major_version and minor_version are words.
scan_w_unsupported_asmmode_specifier=W_Unsupported assembler style specified $1
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=W_ASM reader switch is not possible inside asm statement, $1 will be effective only for next
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=E_Wrong switch toggle, use ON/OFF or +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=E_Resource files are not supported for this target
% The target you are compiling for doesn't support Resource files. The
% only target which can use resource files is Win32
scan_w_include_env_not_found=W_Include environment $1 not found in environment
% The included environment variable can't be found in the environment, it'll
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=E_Illegal value for FPU register limit
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
parser_e_syntax_error=E_Parser - Syntax Error
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_w_proc_far_ignored=W_Procedure type FAR ignored
% This is a warning. \var{FAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_near_ignored=W_Procedure type NEAR ignored
% This is a warning. \var{NEAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_interrupt_ignored=W_Procedure type INTERRUPT ignored
% This is a warning. \var{INTERRUPT} is a i386 specific construct
% and is igonred for other processors.
parser_e_dont_nest_interrupt=E_INTERRUPT procedure can't be nested
% An \VAR{INTERRUPT} procedure must be global.
parser_w_proc_register_ignored=W_Procedure type REGISTER ignored
% This is a warning. \var{REGISTER} is ignored by FPC programs for now.
% This is introduced first for Delphi compatibility.
parser_e_no_dll_file_specified=E_No DLL File specified
% No longer in use.
parser_e_export_name_double=E_Duplicate exported function name $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=E_Duplicate exported function index $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=E_Invalid index for exported function
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=W_Relocatable DLL or executable $1 debug info does not work, disabled.
parser_w_parser_win32_debug_needs_WN=W_To allow debugging for win32 code you need to disable relocation with -WN option
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=E_Constructor name must be INIT
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=E_Destructor name must be DONE
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_illegal_open_parameter=E_Illegal open parameter
% You are trying to use the wrong type for an open parameter.
parser_e_proc_inline_not_supported=E_Procedure type INLINE not supported
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_priv_meth_not_virtual=W_Private methods shouldn't be VIRTUAL
% You declared a method in the private part of a object (class) as
% \var{virtual}. This is not allowed. Private methods cannot be overridden
% anyway.
parser_w_constructor_should_be_public=W_Constructor should be public
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=W_Destructor should be public
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=N_Class should have one destructor only
% You can declare only one destructor for a class.
parser_e_no_local_objects=E_Local class definitions are not allowed
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=F_Anonym class definitions are not allowed
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_object_has_no_vmt=E_The object $1 has no VMT
parser_e_illegal_parameter_list=E_Illegal parameter list
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_type=E_Wrong parameter type specified for arg no. $1
% There is an error in the parameter list of the function or procedure.
% The compiler cannot determine the error more accurate than this.
parser_e_wrong_parameter_size=E_Wrong amount of parameters specified
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=E_overloaded identifier $1 isn't a function
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=E_overloaded functions have the same parameter list
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=E_function header doesn't match the forward declaration $1
% You declared a function with same parameters but
% different result type or function modifiers.
parser_e_header_different_var_names=E_function header $1 doesn't match forward : var name changes $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=N_Values in enumeration types have to be ascending
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_n_interface_name_diff_implementation_name=N_Interface and implementation names are different $1 => $2
% This note warns you if the implementation and interface names of a
% functions are different, but they have the same mangled name. This
% is important when using overloaded functions (but should produce no error).
parser_e_no_with_for_variable_in_other_segments=E_With can not be used for variables in a different segment
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=E_function nesting > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=E_range check error while evaluating constants
% The constants are out of their allowed range.
parser_w_range_check_error=W_range check error while evaluating constants
% The constants are out of their allowed range.
parser_e_double_caselabel=E_duplicate case label
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=E_Upper bound of case range is less than lower bound
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=E_typed constants of classes are not allowed
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=E_functions variables of overloaded functions are not allowed
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=E_string length must be a value from 1 to 255
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
% (This is not true for \var{Longstrings} and \var{AnsiStrings}.
parser_w_use_extended_syntax_for_objects=W_use extended syntax of NEW and DISPOSE for instances of objects
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=W_use of NEW or DISPOSE for untyped pointers is meaningless
parser_e_no_new_dispose_on_void_pointers=E_use of NEW or DISPOSE is not possible for untyped pointers
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=E_class identifier expected
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=E_type identifier not allowed here
% You cannot use a type inside an expression.
parser_e_methode_id_expected=E_method identifier expected
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=E_function header doesn't match any method of this class
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_p_procedure_start=P_procedure/function $1
% When using the \var{-vp} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=E_Illegal floating point constant
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=E_FAIL can be used in constructors only
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=E_Destructors can't have parameters
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=E_Only class methods can be referred with class references
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
parser_e_only_class_methods=E_Only class methods can be accessed in class methods
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
parser_e_case_mismatch=E_Constant and CASE types do not match
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=E_The symbol can't be exported from a library
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=W_An inherited method is hidden by $1
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=E_There is no method in an ancestor class to be overridden: $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=E_No member is provided to access property
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=W_Stored prorperty directive is not yet implemented
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=E_Illegal symbol for property access
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code woud cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=E_Cannot access a protected field of an object here
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module wher the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=E_Cannot access a private field of an object here
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_w_overloaded_are_not_both_virtual=W_overloaded method of virtual method should be virtual: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_w_overloaded_are_not_both_non_virtual=W_overloaded method of non-virtual method should be non-virtual: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_e_overloaded_methodes_not_same_ret=E_overloaded methods which are virtual must have the same return type: $1
% If you declare virtual overloaded methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=E_EXPORT declared functions can't be nested
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=E_methods can't be EXPORTed
% You cannot declare a procedure that is a method for an object as
% \var{export}ed. That is, your methods cannot be called from a C program.
parser_e_call_by_ref_without_typeconv=E_call by var parameters have to match exactly: Got $1 expected $2
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=E_Class isn't a parent class of the current class
% When calling inherited methods, you are trying to call a method of a strange
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=E_SELF is only allowed in methods
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=E_methods can be only in other methods called direct with type identifier of the class
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=E_Illegal use of ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=E_range check error in set constructor or duplicate set element
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=E_Pointer to object expected
% You specified an illegal type in a \var{New} statement.
% The extended synax of \var{New} needs an  object as a parameter.
parser_e_expr_have_to_be_constructor_call=E_Expression must be constructor call
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=E_Expression must be destructor call
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=E_Illegal order of record elements
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=E_Expression type must be class or record type
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=E_Procedures can't return a value
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures  cannot return a value.
parser_e_constructors_always_objects=E_constructors and destructors must be methods
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=E_Operator is not overloaded
% You're trying to use an overloaded operator when it isn't overloaded for
% this type.
parser_e_no_such_assignment=E_Impossible to overload assignment for equal types
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_no_reraise_possible=E_Re-raise isn't possible there
% You are trying to raise an exception where it isn't allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=E_The extended syntax of new or dispose isn't allowed for a class
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{Dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_asm_incomp_with_function_return=E_Assembler incompatible with function return type
% You're trying to implement a \var{assembler} function, but the return type
% of the function doesn't allow that.
parser_e_procedure_overloading_is_off=E_Procedure overloading is switched off
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=E_It is not possible to overload this operator (overload = instead)
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=E_Comparative operator must return a boolean value
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=E_Only virtual methods can be abstract
% You are declaring a method as abstract, when it isn't declared to be
% virtual.
parser_f_unsupported_feature=F_Use of unsupported feature!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=E_The mix of CLASSES and OBJECTS isn't allowed
% You cannot derive \var{objects} and \var{classes} intertwined . That is,
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=W_Unknown procedure directive had to be ignored: $1
% The procedure direcive you secified is unknown. Recognised procedure
% directives are \var{cdecl}, \var{stdcall}, \var{popstack}, \var{pascal}
% \var{register}, \var{export}.
parser_e_absolute_only_one_var=E_absolute can only be associated to ONE variable
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
parser_e_absolute_only_to_var_or_const=E_absolute can only be associated a var or const
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=E_Only ONE variable can be initialized
% You cannot specify more than one variable with a initial value
% in Delphi syntax.
parser_e_abstract_no_definition=E_Abstract methods shouldn't have any definition (with function body)
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=E_This overloaded function can't be local (must be exported)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=W_Virtual methods are used without a constructor in $1
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_m_macro_defined=M_Macro defined: $1
% When \var{-vm} is used, the compiler tells you when it defines macros.
parser_m_macro_undefined=M_Macro undefined: $1
% When \var{-vm} is used, the compiler tells you when it undefines macros.
parser_m_macro_set_to=M_Macro $1 set to $2
% When \var{-vm} is used, the compiler tells you what values macros get.
parser_i_compiling=I_Compiling $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=U_Parsing interface of unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=U_Parsing implementation of $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=D_Compiling $1 for the second time
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_paras_allowed=E_Array properties aren't allowed here
% You cannot use array properties at that point in the source.
parser_e_no_property_found_to_override=E_No property found to override
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=E_Only one default property is allowed, found inherited default property in class $1
% You specified a property as \var{Default}, but a parent class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=E_The default property must be an array property
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=E_Virtual constructors are only supported in class object model
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=E_No default property available
% You try to access a default property of a class, but this class (or one of
% it's ancestors) doesn't have a default property.
parser_e_cant_have_published=E_The class can't have a published section, use the {$M+} switch
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=E_Forward declaration of class $1 must be resolved here to use the class as ancestor
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=E_Local operators not supported
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=E_Procedure directive $1 not allowed in interface section
% This procedure directive is not allowed in the \var{interface} section of

% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=E_Procedure directive $1 not allowed in implementation section
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=E_Procedure directive $1 not allowed in procvar declaration
% This procedure directive cannot be part of a procedural or function
% type declaration.
parser_e_function_already_declared_public_forward=E_Function is already declared Public/Forward $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=E_Can't use both EXPORT and EXTERNAL
% These two procedure directives are mutually exclusive
parser_e_name_keyword_expected=E_NAME keyword expected
% The definition of an external variable needs a \var{name} clause.
parser_w_not_supported_for_inline=W_$1 not yet supported inside inline procedure/function
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=W_Inlining disabled
% Inlining of procedures is disabled.
parser_i_writing_browser_log=I_Writing Browser log $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=H_may be pointer dereference is missing
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=F_Selected assembler reader not supported
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not

% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=E_Procedure directive $1 has conflicts with other directives
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=E_Calling convention doesn't match forward
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_register_calling_not_supported=E_Register calling (fastcall) not supported
% The \var{register} calling convention, i.e., arguments are passed in
% registers instead of on the stack is not supported. Arguments are always
% passed on the stack.
parser_e_property_cant_have_a_default_value=E_Property can't have a default value
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=E_The default value of a property must be constant
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=E_Symbol can't be published, can be only a class
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=E_That kind of property can't be published
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_w_empty_import_name=W_Empty import name specified
% Both index and name for the import are 0 or empty
parser_e_empty_import_name=W_An import name is required
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_used_proc_name_changed=E_Function internal name changed after use of function
% This is an internal error; please report any occurrences of this error
% to the \fpc team.
parser_e_division_by_zero=E_Division by zero
% There is a divsion by zero encounted
parser_e_invalid_float_operation=E_Invalid floating point operation
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=E_Upper bound of range is less than lower bound
% The upper bound of a \var{case} label is less than the lower bound and this
% is not possible
parser_e_string_larger_array=E_string length is larger than array of char length
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=E_Illegal expression after message directive
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=E_Message handlers can take only one call by ref. parameter
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=E_Duplicate message label: $1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=E_Self can be only an explicit parameter in message handlers
% The self parameter can be passed only explicitly in a method which
% is declared as message method handler.
parser_e_threadvars_only_sg=E_Threadvars can be only static or global
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has it's own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=F_Direct assembler not supported for binary output format
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=W_Don't load OBJPAS unit manual, use {$mode objfpc} or {$mode delphi} instead
% You're trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea to do, you can better use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automaticly
parser_e_no_object_override=E_OVERRIDE can't be used in objects
% Override isn't support for objects, use VIRTUAL instead to override
% a method of an anchestor object
parser_e_cant_use_inittable_here=E_Data types which requires initialization/finalization can't be used in variant records
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=E_Resourcestrings can be only static or global
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=E_Exit with argument can't be used here
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=E_The type of the storage symbol must be boolean
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=E_This symbol isn't allowed as storage symbol
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=E_Only class which are compiled in $M+ mode can be published
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=E_Procedure directive expected
% When declaring a procedure in a const block you used a ; after the
% procedure declaration after which a procedure directive must follow.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=E_The value for a property index must be of an ordinal type
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=E_Procedure name to short to be exported
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=E_No DEFFILE entry can be generated for unit global vars
parser_e_dlltool_unit_var_problem2=E_Compile without -WD option
% \end{description}
#
# Type Checking
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=E_Type mismatch
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=E_Incompatible types: got "$1" expected "$2"
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
type_e_not_equal_types=E_Type mismatch between $1 and $2
% The types are not equal
type_e_type_id_expected=E_Type identifier expected
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=E_Variable identifier expected
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=E_Integer expression expected, but got "$1"
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=E_Boolean expression expected, but got "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=E_Ordinal expression expected
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=E_pointer type expected, but got "$1"
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=E_class type expected, but got "$1"
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_varid_or_typeid_expected=E_Variable or type indentifier expected
% The argument to the \var{High} or \var{Low} function is not a variable
% nor a type identifier.
type_e_cant_eval_constant_expr=E_Can't evaluate constant expression
% No longer in use.
type_e_set_element_are_not_comp=E_Set elements are not compatible
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=E_Operation not implemented for sets
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=W_Automatic type conversion from floating type to COMP which is an integer type
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{Comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=H_use DIV instead to get an integer result
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=E_string types doesn't match, because of $V+ mode
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=E_succ or pred on enums with assignments not possible
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=E_Can't read or write variables of this type
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% booleans, reals, pchars and strings can be read from/written to a text file.
type_e_no_readln_writeln_for_typed_file=E_Can't use readln or writeln on typed file
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=E_Can't use read or write on untyped file.
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=E_Type conflict between set elements
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=W_lo/hi(dword/qword) returns the upper/lower word/dword
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word/dword of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type cast the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=E_Integer or real expression expected
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=E_Wrong type $1 in array constructor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=E_Incompatible type for arg no. $1: Got $2, expected $3
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=E_Method (variable) and Procedure (variable) are not compatible
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=E_Illegal constant passed to internal math function
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=E_Can't get the address of constants
% It's not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=E_Argument can't be assigned to
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they can't be used as arguments
type_e_cannot_local_proc_to_procvar=E_Can't assign local procedure/function to procedure variable
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=E_Can't assign values to an address
% It's not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=E_Can't assign values to const variable
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
sym_e_id_not_found=E_Identifier not found $1
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=F_Internal Error in SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=E_Duplicate identifier $1
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=H_Identifier already defined in $1 at line $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=E_Unknown identifier $1
% The identifier encountered hasn't been declared, or is used outside the
% scope where it's defined.
sym_e_forward_not_resolved=E_Forward declaration not solved $1
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_f_id_already_typed=F_Identifier type already defined as type
% You are trying to redefine a type.
sym_e_error_in_type_def=E_Error in type definition
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_type_id_not_defined=E_Type identifier not defined
% The type identifier has not been defined yet.
sym_e_forward_type_not_resolved=E_Forward type not resolved $1
% A symbol was forward defined, but no declaration was encountered.
sym_e_only_static_in_static=E_Only static variables can be used in static methods or outside methods
% A static method of an object can only access static variables.
sym_e_invalid_call_tvarsymmangledname=E_Invalid call to tvarsym.mangledname()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_f_type_must_be_rec_or_class=F_record or class type expected
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=E_Instances of classes or objects with an abstract method are not allowed
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=W_Label not defined $1
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=E_Label used but not defined $1
% A label was declared and used, but not defined.
sym_e_ill_label_decl=E_Illegal label declaration
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=E_GOTO and LABEL are not supported (use switch -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the  \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=E_Label not found
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=E_identifier isn't a label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=E_label already defined
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=E_illegal type declaration of set elements
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=E_Forward class definition not resolved $1
% You declared a class, but you didn't implement it.
sym_n_unit_not_used=H_Unit $1 not used in $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=H_Parameter $1 not used
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=N_Local variable $1 not used
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=H_Value parameter $1 is assigned but never used
% This is a warning. The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=N_Local variable $1 is assigned but never used
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=H_Local $1 $2 is not used
% A local symbol is never used.
sym_n_private_identifier_not_used=N_Private field $1.$2 is never used
sym_n_private_identifier_only_set=N_Private field $1.$2 is assigned but never used
sym_n_private_method_not_used=N_Private method $1.$2 never used


sym_e_set_expected=E_Set type expected
% The variable or expression isn't of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=W_Function result does not seem to be set
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_e_illegal_field=E_Unknown record field identifier $1
% The field doesn't exist in the record definition.
sym_n_uninitialized_local_variable=W_Local variable $1 does not seem to be initialized
sym_n_uninitialized_variable=W_Variable $1 does not seem to be initialized
% These messages are displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=E_identifier idents no member $1
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the class you are trying to create. The procedure you specified
% does not exist.
sym_b_param_list=B_Found declaration: $1
% You get this when you use the \var{-vb} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=E_Data segment too large (max. 2GB)
% You get this when you declare an array whose size exceeds the 2GB limit.
% \end{description}
#
# Codegenerator
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_break_not_allowed=E_BREAK not allowed
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=E_CONTINUE not allowed
% You're trying to use \var{continue} outside a loop construction.
cg_e_too_complex_expr=E_Expression too complicated - FPU stack overflow
% Your expression is too long for the compiler. You should try dividing the
% construct over multiple assignments.
cg_e_illegal_expression=E_Illegal expression
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
cg_e_invalid_integer=E_Invalid integer expression
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
cg_e_invalid_qualifier=E_Illegal qualifier
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
cg_e_upper_lower_than_lower=E_High range limit < low range limit
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
cg_e_illegal_count_var=E_Illegal counter variable
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
cg_e_cant_choose_overload_function=E_Can't determine which overloaded function to call
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
cg_e_parasize_too_big=E_Parameter list size exceeds 65535 bytes
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_illegal_type_conversion=E_Illegal type conversion
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
cg_d_pointer_to_longint_conv_not_portable=D_Conversion between ordinals and pointers is not portable across platforms
% If you typecast a pointer to a longint, this code will not compile
% on a machine using 64bit for pointer storage.
cg_e_file_must_call_by_reference=E_File types must be var parameters
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=E_The use of a far pointer isn't allowed there
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_var_must_be_reference=E_illegal call by reference parameters
% You are trying to pass a constant or an expression to a procedure that
% requires a \var{var} parameter. Only variables can be passed as a \var{var}
% parameter.
cg_e_dont_call_exported_direct=E_EXPORT declared functions can't be called
% No longer in use.
cg_w_member_cd_call_from_method=W_Possible illegal call of constructor or destructor (doesn't match to this context)
% No longer in use.
cg_n_inefficient_code=N_Inefficient code
% You construction seems dubious to the compiler.
cg_w_unreachable_code=W_unreachable code
% You specified a loop which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_stackframe_with_esp=E_procedure call with stackframe ESP/SP
% The compiler encountered a procedure  or function call inside a
% procedure that uses a \var{ESP/SP} stackframe. Normally, when a call is
% done the procedure needs a \var{EBP} stackframe.
cg_e_cant_call_abstract_method=E_Abstract methods can't be called directly
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_f_internal_error_in_getfloatreg=F_Internal Error in getfloatreg(), allocation failure
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_unknown_float_type=F_Unknown float type
% The compiler cannot determine the kind of float that occurs in an expression.
cg_f_secondvecn_base_defined_twice=F_SecondVecn() base defined twice
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_extended_cg68k_not_supported=F_Extended cg68k not supported
% The \var{extended} type is not supported on the m68k platform.
cg_f_32bit_not_supported_in_68000=F_32-bit unsigned not supported in MC68000 mode
% The cardinal is not supported on the m68k platform.
cg_f_internal_error_in_secondinline=F_Internal Error in secondinline()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_d_register_weight=D_Register $1 weight $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_e_stacklimit_in_local_routine=E_Stack limit excedeed in local routine
% Your code requires a too big stack. Some operating systems pose limits
% on the stack size. You should use less variables or try ro put large
% variables on the heap.
cg_d_stackframe_omited=D_Stack frame is omitted
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_w_64bit_range_check_not_supported=W_Range check for 64 bit integers is not supported on this target
% 64 bit range check is not yet implemented for 32 bit processors.
cg_e_unable_inline_object_methods=E_Object or class methods can't be inline.
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=E_Procvar calls can't be inline.
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=E_No code for inline procedure stored
% The compiler couldn't store code for the inline procedure.
cg_e_no_call_to_interrupt=E_Direct call of interrupt procedure $1 is not possible
% You can not call an interrupt procedure directly from FPC code
cg_e_can_access_element_zero=E_Element zero of an ansi/wide- or longstring can't be accessed, use (set)length instead
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such kinf of string
cg_e_include_not_implemented=E_Include and exclude not implemented in this case
% \var{include} and \var{exclude} are only partially
% implemented for \var{i386} processors
% and not at all for \var{m68k} processors.
cg_e_cannot_call_cons_dest_inside_with=E_Constructors or destructors can not be called inside a 'with' clause
% Inside a \var{With} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=E_Cannot call message handler method directly
% A message method handler method can't be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=E_Jump in or outside of an exception block
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
cg_e_control_flow_outside_finally=E_Control flow statements aren't allowed in a finally block
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
asmr_d_start_reading=D_Starting $1 styled assembler parsing
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=D_Finished $1 styled assembler parsing
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=E_Non-label pattern contains @
% A identifier which isn't a label can't contain a @.
asmr_w_override_op_not_supported=W_Override operator not supported
% The Override operator is not supported
asmr_e_building_record_offset=E_Error building record offset
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=E_OFFSET used without identifier
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=E_TYPE used without identifier
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=E_Cannot use local variable or parameters here
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the %ebp register so the
% address can't be get directly.
asmr_e_need_offset=E_need to use OFFSET here
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=E_Cannot use multiple relocatable symbols
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=E_Relocatable symbol can only be added
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=E_Invalid constant expression
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=E_Relocatable symbol is not allowed
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=E_Invalid reference syntax
% There is an error in the reference.
asmr_e_local_para_unreachable=E_You can not reach $1 from that code
% You can not read directly the value of local or para
% of a higher level in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=E_Local symbols/labels aren't allowed as references
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=E_Invalid base and index register usage
% There is an error with the base and index register
asmr_w_possible_object_field_bug=W_Possible error in object field handling
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=E_Wrong scale factor specified
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=E_Multiple index register usage
% You are trying to use more than one index register
asmr_e_invalid_operand_type=E_Invalid operand type
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=E_Invalid string as opcode operand: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=W_@CODE and @DATA not supported
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=E_Null label references are not allowed
asmr_e_ev_zero_divide=F_Divide by zero in asm evaluator
asmr_e_ev_stack_overflow=F_Evaluator stack overflow
asmr_e_ev_stack_underflow=F_Evaluator stack underflow
asmr_e_ev_invalid_number=F_Invalid numeric format in asm evaluator
asmr_e_ev_invalid_op=F_Invalid Operator in asm evaluator
asmr_e_escape_seq_ignored=E_escape sequence ignored: $1
asmr_e_invalid_symbol_ref=E_Invalid symbol reference
asmr_w_fwait_emu_prob=W_Fwait can cause emulation problems with emu387
asmr_w_calling_overload_func=W_Calling an overload function in assembler
asmr_e_unsupported_symbol_type=E_Unsupported symbol type for operand
asmr_e_constant_out_of_bounds=E_Constant value out of bounds
asmr_e_error_converting_decimal=E_Error converting decimal $1
asmr_e_error_converting_octal=E_Error converting octal $1
asmr_e_error_converting_binary=E_Error converting binary $1
asmr_e_error_converting_hexadecimal=E_Error converting hexadecimal $1
asmr_h_direct_global_to_mangled=H_$1 translated to $2
asmr_w_direct_global_is_overloaded_func=W_$1 is associated to an overloaded function
asmr_e_cannot_use_SELF_outside_a_method=E_Cannot use SELF outside a method
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=E_Cannot use OLDEBP outside a nested procedure
asmr_e_void_function=W_Procedures can't return any value in asm code
asmr_e_SEG_not_supported=E_SEG not supported
asmr_e_size_suffix_and_dest_dont_match=E_Size suffix and destination or source size do not match
asmr_w_size_suffix_and_dest_dont_match=W_Size suffix and destination or source size do not match
asmr_e_syntax_error=E_Assembler syntax error
asmr_e_invalid_opcode_and_operand=E_Invalid combination of opcode and operands
asmr_e_syn_operand=E_Assemler syntax error in operand
asmr_e_syn_constant=E_Assemler syntax error in constant
asmr_e_invalid_string_expression=E_Invalid String expression
asmr_w_const32bit_for_address=32bit constant created for address
asmr_e_unknown_opcode=E_Unrecognized opcode $1
asmr_e_invalid_or_missing_opcode=E_Invalid or missing opcode
asmr_e_invalid_prefix_and_opcode=E_Invalid combination of prefix and opcode: $1
asmr_e_invalid_override_and_opcode=E_Invalid combination of override and opcode: $1
asmr_e_too_many_operands=E_Too many operands on line
asmr_w_near_ignored=W_NEAR ignored
asmr_w_far_ignored=W_FAR ignored
asmr_e_dup_local_sym=E_Duplicate local symbol $1
asmr_e_unknown_local_sym=E_Undefined local symbol $1
asmr_e_unknown_label_identifier=E_Unknown label identifier $1
asmr_e_invalid_fpu_register=E_Invalid floating point register name
asmr_e_nor_not_supported=E_NOR not supported
asmr_w_modulo_not_supported=W_Modulo not supported
asmr_e_invalid_float_const=E_Invalid floating point constant $1
asmr_e_invalid_float_expr=E_Invalid floating point expression
asmr_e_wrong_sym_type=E_Wrong symbol type
asmr_e_cannot_index_relative_var=E_Cannot index a local var or parameter with a register
asmr_e_invalid_seg_override=E_Invalid segment override expression
asmr_w_id_supposed_external=W_Identifier $1 supposed external
asmr_e_string_not_allowed_as_const=E_Strings not allowed as constants
asmr_e_no_var_type_specified=No type of variable specified
asmr_w_assembler_code_not_returned_to_text=E_assembler code not returned to text section
asmr_e_not_directive_or_local_symbol=E_Not a directive or local symbol $1
asmr_w_using_defined_as_local=E_Using a defined name as a local label
asmr_e_dollar_without_identifier=E_Dollar token is used without an identifier
asmr_w_32bit_const_for_address=W_32bit constant created for address
asmr_n_align_is_target_specific=N_.align is target specific, use .balign or .p2align
asmr_e_cannot_access_field_directly_for_parameters=E_Can't access fields directly for parameters
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=E_Can't access fields of objects/classes directly
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
#
# Assembler/binary writers
#
asmw_f_too_many_asm_files=F_Too many assembler files
asmw_f_assembler_output_not_supported=F_Selected assembler output not supported
asmw_f_comp_not_supported=F_Comp not supported
asmw_f_direct_not_supported=F_Direct not support for binary writers
asmw_e_alloc_data_only_in_bss=E_Allocating of data is only allowed in bss section
asmw_f_no_binary_writer_selected=F_No binary writer selected
asmw_e_opcode_not_in_table=E_Asm: Opcode $1 not in table
asmw_e_invalid_opcode_and_operands=E_Asm: $1 invalid combination of opcode and operands
asmw_e_16bit_not_supported=E_Asm: 16 Bit references not supported
asmw_e_invalid_effective_address=E_Asm: Invalid effective address
asmw_e_immediate_or_reference_expected=E_Asm: Immediate or reference expected
asmw_e_value_exceeds_bounds=E_Asm: $1 value exceeds bounds $2
asmw_e_short_jmp_out_of_range=E_Asm: Short jump is out of range $1
asmw_e_undefined_label=E_Asm: Undefined label $1


#
# Executing linker/assembler
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=W_Source operating system redefined
exec_i_assembling_pipe=I_Assembling (pipe) $1
exec_d_cant_create_asmfile=E_Can't create assember file: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_objectfile=E_Can't create object file: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_e_cant_create_archivefile=E_Can't create archive file: $1
% The mentioned file can't be create. Check if you've
% permission to create this file
exec_w_assembler_not_found=W_Assembler $1 not found, switching to external assembling
exec_t_using_assembler=T_Using assembler: $1
exec_w_error_while_assembling=W_Error while assembling exitcode $1
exec_w_cant_call_assembler=W_Can't call the assembler, error $1 switching to external assembling
exec_i_assembling=I_Assembling $1
exec_i_assembling_smart=I_Assembling smartlink $1
exec_w_objfile_not_found=W_Object $1 not found, Linking may fail !
exec_w_libfile_not_found=W_Library $1 not found, Linking may fail !
exec_w_error_while_linking=W_Error while linking
exec_w_cant_call_linker=W_Can't call the linker, switching to external linking
exec_i_linking=I_Linking $1
exec_w_util_not_found=W_Util $1 not found, switching to external linking
exec_t_using_util=T_Using util $1
exec_e_exe_not_supported=E_Creation of Executables not supported
exec_e_dll_not_supported=E_Creation of Dynamic/Shared Libraries not supported
exec_i_closing_script=I_Closing script $1
exec_w_res_not_found=W_resource compiler not found, switching to external mode
exec_i_compilingresource=I_Compiling resource $1
%\end{description}
# EndOfTeX

#
# Executable information
#
execinfo_f_cant_process_executable=F_Can't post process executable $1
execinfo_f_cant_open_executable=F_Can't open executable $1
execinfo_x_codesize=X_Size of Code: $1 bytes
execinfo_x_initdatasize=X_Size of initialized data: $1 bytes
execinfo_x_uninitdatasize=X_Size of uninitialized data: $1 bytes
execinfo_x_stackreserve=X_Stack space reserved: $1 bytes
execinfo_x_stackcommit=X_Stack space commited: $1 bytes

# Unit loading
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=T_Unitsearch: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=T_PPU Loading $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=U_PPU Name: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=U_PPU Flags: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=U_PPU Crc: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=U_PPU Time: $1
% When you use the \var{-vu} flag, the time the unit was compiled is shown.
unit_u_ppu_file_too_short=U_PPU File too short
% The ppufile is too short, not all declarations are present.
unit_u_ppu_invalid_header=U_PPU Invalid Header (no PPU at the begin)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=U_PPU Invalid Version $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=U_PPU is compiled for an other processor
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=U_PPU is compiled for an other target
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=U_PPU Source: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_write=U_Writing $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=F_Can't Write PPU-File
% An error occurred when writing the unit file.
unit_f_ppu_read_error=F_Error reading PPU-File
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=F_unexpected end of PPU-File
% Unexpected end of file.
unit_f_ppu_invalid_entry=F_Invalid PPU-File entry: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=F_PPU Dbx count problem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=E_Illegal unit name: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=F_Too much units
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=F_Circular unit reference between $1 and $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=F_Can't compile unit $1, no sources available
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=F_Can't find unit $1
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_unit_name_error=W_Unit $1 was not found but $2 exists
unit_f_unit_name_error=F_Unit $1 searched but $2 found
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=W_Compiling the system unit requires the -Us switch
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=F_There were $1 errors compiling module, stopping
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=U_Load from $1 ($2) unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=U_Recompiling $1, checksum changed for $2
unit_u_recompile_source_found_alone=U_Recompiling $1, source found only
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=U_Recompiling unit, static lib is older than ppufile
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=U_Recompiling unit, shared lib is older than ppufile
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=U_Recompiling unit, obj and asm are older than ppufile
% When you use the \var{-vu} flag, the compiler warns if the assembler or
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=U_Recompiling unit, obj is older than asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_start_parse_interface=U_Parsing interface of $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_start_parse_implementation=U_Parsing implementation of $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=U_Second load for unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=U_PPU Check file $1 time $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
% \end{description}
# EndOfTeX
#
#  Options
#
option_usage=$1 [options] <inputfile> [options]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=W_Only one source file supported
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=W_DEF file can be created only for OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=E_nested response files are not supported
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=F_No source file name in command line
% The compiler expects a source file name on the command line.
option_no_option_found=N_No option inside $1 config file
% The compiler didn't find any option in that config file.
option_illegal_para=E_Illegal parameter: $1
% You specified an unknown option.
option_help_pages_para=H_-? writes help pages
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=F_Too many config files nested
% You can only nest up to 16 config files.
option_unable_open_file=F_Unable to open file $1
% The option file cannot be found.
option_reading_further_from=N_Reading further options from $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=W_Target is already set to: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=W_Shared libs not supported on DOS platform, reverting to static
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=F_too many IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=F_too many ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=F_open conditional at the end of the file
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=W_Debug information generation is not supported by this executable
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=H_Try recompiling with -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=E_You are using the obsolete switch $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=E_You are using the obsolete switch $1, please use $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=N_Switching assembler to default source writing assembler
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_logo_start=Free Pascal Compiler version $FPCVER [$FPCDATE] for $FPCTARGET
option_logo_end=Copyright (c) 1993-2000 by Florian Klaempfl

#
# Info (option -i)
#
option_info_start=Free Pascal Compiler version $FPCVER
option_info1=<lf>
option_info2=Compiler Date  : $FPCDATE
option_info3=Compiler Target: $FPCTARGET
option_info4=<lf>
option_info5=This program comes under the GNU General Public Licence
option_info6=For more information read COPYING.FPC
option_info7=<lf>
option_info8=Report bugs,suggestions etc to:
option_info_end=                 bugrep@freepascal.org

#
# Help pages (option -? and -h)
#
# note: the ordering of the enum numbering doens't matter
#
ol_begin=**0*_put + after a boolean switch option to enable it, - to disable it
ol001=**1a_the compiler doesn't delete the generated assembler file
ol002=**2al_list sourcecode lines in assembler file
ol003=**2ar_list register allocation/release info in assembler file
ol004=**2at_list temp allocation/release info in assembler file
ol005=**1b_generate browser info
ol006=**2bl_generate local symbol info
ol007=**1B_build all modules
ol008=**1C<x>_code generation options:
ol009=**2CD_create also dynamic library (not supported)
ol010=**2Ch<n>_<n> bytes heap (between 1023 and 67107840)
ol011=**2Ci_IO-checking
ol012=**2Cn_omit linking stage
ol013=**2Co_check overflow of integer operations
ol014=**2Cr_range checking
ol015=**2Cs<n>_set stack size to <n>
ol016=**2Ct_stack checking
ol017=**2CX_create also smartlinked library
ol018=**1d<x>_defines the symbol <x>
ol019=*O1D_generate a DEF file
ol020=*O2Dd<x>_set description to <x>
ol021=*O2Dw_PM application
ol022=**1e<x>_set path to executable
ol023=**1E_same as -Cn
ol024=**1F<x>_set file names and paths:
ol025=**2FD<x>_sets the directory where to search for compiler utilities
ol026=**2Fe<x>_redirect error output to <x>
ol027=**2FE<x>_set exe/unit output path to <x>
ol028=**2Fi<x>_adds <x> to include path
ol029=**2Fl<x>_adds <x> to library path
ol030=*L2FL<x>_uses <x> as dynamic linker
ol031=**2Fo<x>_adds <x> to object path
ol032=**2Fr<x>_load error message file <x>
ol033=**2Fu<x>_adds <x> to unit path
ol034=**2FU<x>_set unit output path to <x>, overrides -FE
ol035=*g1g_generate debugger information:
ol036=*g2gg_use gsym
ol037=*g2gd_use dbx
ol038=*g2gh_use heap trace unit (for memory leak debugging)
ol039=*g2gl_use line info unit to show more info for backtraces
ol040=*g2gc_generate checks for pointers
ol041=**1i_information
ol042=**2iD_return compiler date
ol043=**2iV_return compiler version
ol044=**2iSO_return compiler OS
ol045=**2iSP_return compiler processor
ol046=**2iTO_return target OS
ol047=**2iTP_return target processor
ol048=**1I<x>_adds <x> to include path
ol049=**1k<x>_Pass <x> to the linker
ol050=**1l_write logo
ol051=**1n_don't read the default config file
ol052=**1o<x>_change the name of the executable produced to <x>
ol053=**1pg_generate profile code for gprof (defines FPC_PROFILE)
ol054=*L1P_use pipes instead of creating temporary assembler files
ol055=**1S<x>_syntax options:
ol056=**2S2_switch some Delphi 2 extensions on
ol057=**2Sc_supports operators like C (*=,+=,/= and -=)
ol058=**2sa_include assertion code.
ol059=**2Sd_tries to be Delphi compatible
ol060=**2Se<x>_compiler stops after the <x> errors (default is 1)
ol061=**2Sg_allow LABEL and GOTO
ol062=**2Sh_Use ansistrings
ol063=**2Si_support C++ styled INLINE
ol064=**2Sm_support macros like C (global)
ol065=**2So_tries to be TP/BP 7.0 compatible
ol066=**2Sp_tries to be gpc compatible
ol067=**2Ss_constructor name must be init (destructor must be done)
ol068=**2St_allow static keyword in objects
ol069=**1s_don't call assembler and linker (only with -a)
ol070=**1u<x>_undefines the symbol <x>
ol071=**1U_unit options:
ol072=**2Un_don't check the unit name
ol073=**2Us_compile a system unit
ol074=**1v<x>_Be verbose. <x> is a combination of the following letters:
ol075=**2*_e : Show errors (default)       d : Show debug info
ol076=**2*_w : Show warnings               u : Show unit info
ol077=**2*_n : Show notes                  t : Show tried/used files
ol078=**2*_h : Show hints                  m : Show defined macros
ol079=**2*_i : Show general info           p : Show compiled procedures
ol080=**2*_l : Show linenumbers            c : Show conditionals
ol081=**2*_a : Show everything             0 : Show nothing (except errors)
ol082=**2*_b : Show all procedure          r : Rhide/GCC compatibility mode
ol083=**2*_    declarations if an error    x : Executable info (Win32 only)
ol084=**2*_    occurs
ol085=**1X_executable options:
ol086=*L2Xc_link with the c library
ol087=**2Xs_strip all symbols from executable
ol088=**2XD_try to link dynamic          (defines FPC_LINK_DYNAMIC)
ol089=**2XS_try to link static (default) (defines FPC_LINK_STATIC)
ol090=**2XX_try to link smart            (defines FPC_LINK_SMART)
ol091=**0*_Processor specific options:
ol092=3*1A<x>_output format:
ol093=3*2Aas_assemble using GNU AS
ol094=3*2Aasaout_assemble using GNU AS for aout (Go32v1)
ol095=3*2Anasmcoff_coff (Go32v2) file using Nasm
ol096=3*2Anasmelf_elf32 (Linux) file using Nasm
ol097=3*2Anasmobj_obj file using Nasm
ol098=3*2Amasm_obj file using Masm (Microsoft)
ol099=3*2Atasm_obj file using Tasm (Borland)
ol100=3*2Acoff_coff (Go32v2) using internal writer
ol101=3*2Apecoff_pecoff (Win32) using internal writer
ol102=3*1R<x>_assembler reading style:
ol103=3*2Ratt_read AT&T style assembler
ol104=3*2Rintel_read Intel style assembler
ol105=3*2Rdirect_copy assembler text directly to assembler file
ol106=3*1O<x>_optimizations:
ol107=3*2Og_generate smaller code
ol108=3*2OG_generate faster code (default)
ol109=3*2Or_keep certain variables in registers
ol110=3*2Ou_enable uncertain optimizations (see docs)
ol111=3*2O1_level 1 optimizations (quick optimizations)
ol112=3*2O2_level 2 optimizations (-O1 + slower optimizations)
ol113=3*2O3_level 3 optimizations (same as -O2u)
ol114=3*2Op<x>_target processor:
ol115=3*3Op1_set target processor to 386/486
ol116=3*3Op2_set target processor to Pentium/PentiumMMX (tm)
ol117=3*3Op3_set target processor to PPro/PII/c6x86/K6 (tm)
ol118=3*1T<x>_Target operating system:
ol119=3*2TGO32V1_version 1 of DJ Delorie DOS extender
ol120=3*2TGO32V2_version 2 of DJ Delorie DOS extender
ol121=3*2TLINUX_Linux
ol122=3*2TOS2_OS/2 2.x
ol123=3*2TWin32_Windows 32 Bit
ol124=3*1W<x>_Win32 target options
ol125=3*2WB<x>_Set Image base to Hexadecimal <x> value
ol126=3*2WC_Specify console type application
ol127=3*2WD_Use DEFFILE to export functions of DLL or EXE
ol128=3*2WG_Specify graphic type application
ol129=3*2WN_Do not generate relocation code (necessary for debugging)
ol130=3*2WR_Generate relocation code
ol131=6*1A<x>_output format
ol132=6*2Aas_Unix o-file using GNU AS
ol133=6*2Agas_GNU Motorola assembler
ol134=6*2Amit_MIT Syntax (old GAS)
ol135=6*2Amot_Standard Motorola assembler
ol136=6*1O_optimizations:
ol137=6*2Oa_turn on the optimizer
ol138=6*2Og_generate smaller code
ol139=6*2OG_generate faster code (default)
ol140=6*2Ox_optimize maximum (still BUGGY!!!)
ol141=6*2O2_set target processor to a MC68020+
ol142=6*1R<x>_assembler reading style:
ol143=6*2RMOT_read motorola style assembler
ol144=6*1T<x>_Target operating system:
ol145=6*2TAMIGA_Commodore Amiga
ol146=6*2TATARI_Atari ST/STe/TT
ol147=6*2TMACOS_Macintosh m68k
ol148=6*2TLINUX_Linux-68k
ol149=**1*_
ol150=**1?_shows this help
ol_end=**1h_shows this help without waiting
#

# The End...
#