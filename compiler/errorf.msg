#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998-2000 by the Free Pascal Development team
#
#   French (cp850) Language File for Free Pascal
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
general_t_compilername=T_Compilateur : $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=D_SystŠme d'exploitation source : $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=I_SystŠme d'exploitation cible : $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=T_R‚pertoire pour fichiers executables : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=T_R‚pertoire de recherche d'unit‚s : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=T_R‚pertoire pour recherche de fichiers inclus : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=T_R‚pertoire pour librairies : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=T_R‚pertoire pour fichiers objets : $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=I_$1 lignes compil‚es, $2 secondes
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=F_Plus de m‚moire disponible
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
scan_f_end_of_file=F_Fin de fichier inattendue
% this typically happens in on of the following cases :
% \begin{itemize}
% \item The source file ends befor then final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=F_String au-del… de la fin de ligne
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=F_caractŠre ill‚gal
% An illegal character was encountered in the input file.
scan_f_syn_expected=F_Erreur de syntaxe, $1 attendu mais $2 trouv‚
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=T_D‚but de lecture du fichier inclus $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=W_Commentaire de niveau $1
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_far_directive_ignored=N_$F directive (FAR) ignor‚e
% The \var{FAR} directive is a 16-bit construction which is recorgnised
% but ignored by the compiler, since it produces 32 bit code.
scan_n_stack_check_global_under_linux=N_Le controle de la gestion de la pile est global sous Linux
% Stack checking with the \var{-Cs} switch is ignored under \linux, since
% \linux does this for you. Only displayed when \var{-vn} is used.
scan_n_ignored_switch=N_Switch "$1" ignor‚
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=W_Switch "$1" ill‚gal
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=W_Ce switch a un effet global
% When \var{-vw} is used, the compiler warns if a switch is global.
scan_e_illegal_char_const=E_Constante de type char ill‚gale
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range. The range
% is 1-255.
scan_f_cannot_open_input=F_Impossible d'ouvrir le fichier $1
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=F_Impossible d'ouvrir le fichier inclus $1
% \fpc cannot find the source file you specified in a \var{\{\$include \}}
% stateent.
scan_e_too_much_endifs=E_Trop de $ENDIFs ou de $ELSEs
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_w_only_pack_records=W_L'alignement des Records peut uniquement ˆtre de 1,2,4,8 ou 16 bytes
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=W_Les ‚numerations peuvent seulement ˆtre sauvegard‚s en 1,2 ou 4 bytes
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=E_$1 attendu pour $2 d‚fini … la ligne $3
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=E_Erreur de syntaxe pendant l'interpr‚tation d'une directive de compilation
% There is an error in the expression following the \var{\{\$if \}} compiler
% directive.
scan_e_error_in_preproc_expr=E_Erreur d'‚valuation d'une directive de compilation
% There is an error in the expression following the \var{\{\$if \}} compiler
% directive.
scan_w_marco_cut_after_255_chars=W_Le contenu d'une macro est tronqu‚ … 255 caractŠres
% The contents of macros canno be longer than 255 characters. This is a
% safety in the compiler, to prevent buffer overflows. This is shown as a
% warning, i.e. when the \var{-vw} switch is used.
scan_e_endif_without_if=E_ENDIF sans IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=F_D‚fini par l'utilisateur : $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=E_D‚fini par l'utilisateur : $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=W_D‚fini par l'utilisateur : $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=N_D‚fini par l'utilisateur : $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=H_D‚fini par l'utilisateur : $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=I_D‚fini par l'utilisateur : $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=E_Impossible de redefinir un mot r‚serv‚
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=F_D‚bordement du buffer de la macro en lecture ou expansion
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=W_L'expansion des macros d‚passe un niveau de 16.
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_wrong_styled_switch=E_Les directives de compilation entre (* ... *) ne sont pas accept‚es
% Compiler switches should always be between \var{\{ \}} comment delimiters.
scan_d_handling_switch=D_Interpr‚tation switch "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=C_ENDIF $1 trouv‚
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=C_IFDEF $1 trouv‚, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=C_IFOPT $1 trouv‚, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=C_IF $1 trouv‚, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=C_IFNDEF $1 trouv‚, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=C_ELSE $1 trouv‚, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=C_Passant jusqu'…...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=I_Appuyez sur <Entr‚e> pour continuer
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=W_Switch non support‚ $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=W_directive de compilation ill‚gale $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=T_De retour dans $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=W_Type d'application non support‚e : $1
% You get this warning, ff you specify an unknown application type
% with the directive $APPTYPE
scan_w_app_type_not_support=W_$APPTYPE non support‚ par OS cible
% The $APPTYPE directive is supported by win32 applications only
scan_w_unsupported_asmmode_specifier=W_Style assembleur non support‚ $1
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=W_Changement de type d'interpr‚teur ASM … l'int‚rieur de code assembleur, $1 sera effectif seulement pour le prochain
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=E_Mauvais argument de switch, utilisez ON/OFF ou +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=E_Fichiers ressource non support‚ pour cette cible
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
parser_e_syntax_error=E_Parseur - Erreur de syntaxe
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_w_proc_far_ignored=W_Directive pour proc‚dure FAR ignor‚
% This is a warning. \var{FAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_near_ignored=W_Directive pour proc‚dure NEAR ignor‚
% This is a warning. \var{NEAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_register_ignored=W_Diretive pour proc‚dure REGISTER ignor‚
% This is a warning. \var{REGISTER} is ignored by FPC programs for now.
% This is introduced first for Delphi compatibility.
parser_e_no_dll_file_specified=E_Pas de fichier DLL sp‚cifi‚
% No longer in use.
parser_e_export_name_double=E_Nom de fonction export‚e doubl‚ $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=E_Index de fonction export‚e doubl‚ $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=E_Index non valide pour fonction export‚e
% DLL function index must be in the range 1..$FFFF
parser_e_constructorname_must_be_init=E_Le nom du constructeur doit ˆtre INIT
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=E_Le nom du destructeur doit ˆtre DONE
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_illegal_open_parameter=E_ParamŠtre ouvert non valide
% You are trying to use the wrong type for an open parameter.
parser_e_proc_inline_not_supported=E_Directive de fonction INLINE non support‚e
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_priv_meth_not_virtual=W_Les m‚thodes priv‚es ne devraient pas ˆtre virutelles
% You declared a method in the private part of a object (class) as
% \var{virtual}. This is not allowed. Private methods cannot be overridden
% anyway.
parser_w_constructor_should_be_public=W_Le constructeur devrait ˆtre public
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=W_Le destructeur devrait ˆtre public
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=N_Les classes ne devraient avoir qu'un selu destructeur
% You can declare only one destructor for a class.
parser_e_no_local_objects=E_Les D‚finitions locales de classes ne sont pas autoris‚es
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=F_Les d‚finitions de classes anonymes sont interdites
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_object_has_no_vmt=E_L'object $1 n'a pas de table de m‚thodes virtuelles (VMT)
parser_e_illegal_parameter_list=E_Liste de paramŠtres ill‚gale
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_type=E_Mauvais type pour paramŠtre #$1
% There is an error in the parameter list of the function or procedure.
% The compiler cannot determine the error more accurate than this.
parser_e_wrong_parameter_size=E_Mauvais nombre de paramŠtres sp‚cifi‚
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=E_L'identificateur surcharg‚ $1 n'est pas une fonction
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=E_Les fonctions surcharg‚es ont les mˆmes paramŠtres
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=E_L'entˆte de la fonction ne correspond pas … la d‚claration $1
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=E_L'entˆte de la fonction $1 ne correspond pas … la d‚claration : le nom de variable change $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=N_Les valeurs d'une ‚num‚ration doivent ˆtre en ordre ascendant
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_n_interface_name_diff_implementation_name=N_Les noms d'interface et d'implementation sont diff‚rents $1 => $2
% This note warns you if the implementation and interface names of a
% functions are different, but they have the same mangled name. This
% is important when using overloaded functions (but should produce no error).
parser_e_no_with_for_variable_in_other_segments=E_With ne peut ˆtre utilis‚ pour des variables dans un autre segment
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=E_Niveau d'inbrication de fonctions > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=E_Erreur d'intervalle dans l'‚valuation d'une constante
% The constants are out of their allowed range.
parser_w_range_check_error=W_Erreur d'intervalle dans l'‚valuation d'une constante
% The constants are out of their allowed range.
parser_e_double_caselabel=E_Valeur en double dans une instruction  CASE
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=E_Valeur max inf‚rieure … la valeur min dans CASE
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=E_Les constantes typ‚es de classes sont interdites
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=E_Impossible d'assigner … une variable fonctionelle une fonction surcharg‚e
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=E_La longueur d'une STRING doit ˆtre situ‚e entre 1 et 255
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
% (This is not true for \var{Longstrings} and \var{AnsiStrings}.
parser_w_use_extended_syntax_for_objects=W_Utilisez la syntaxe ‚tendue de NEW et DISPOSE pour les objets
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=W_L'usage de NEW ou DISPOSE pour des pointeurs non typ‚s est sans signification meaningless
parser_e_no_new_dispose_on_void_pointers=E_L'usage de NEW ou DISPOSE pour des pointeurs non typ‚s est sans signification meaningless
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \vat{delphi} modes.
parser_e_class_id_expected=E_Identificateur de classe attendu
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=E_Identificateur de type interdit ici
% You cannot use a type inside an expression.
parser_e_methode_id_expected=E_Identificateur de m‚thode attendu
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=E_La fonction ne correspond … aucune m‚thode de cette classe
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_p_procedure_start=P_proc‚dure/fonction $1
% When using the \var{-vp} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=E_Constante r‚elle ill‚gale
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=E_FAIL peut uniquement ˆtre utilis‚ dans un constructeur
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=E_Les destructeurs ne peuvent avoir de paramŠtres
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=E_Seules des m‚thodes de classes peuvent ˆtre r‚f‚r‚ avec des r‚f‚rences de classes
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
parser_e_only_class_methods=E_Seuls des m‚thodes de classes peuvent ˆtre appel‚s dans une m‚thode de classe
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
parser_e_case_mismatch=E_La constante et le type de CASE ne conserpodnet pas
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=E_The symbol can't be exported from a library
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=W_Une m‚thode h‚rit‚e est cach‚e par $1
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=E_Il n'y a pas de m‚thode dans l'ancˆtre que l'on peut surcharger : $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=E_Il manque un champ pour sp‚cifier l'accŠss … la property
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=W_Stored prorperty directive is not yet implemented
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=E_Symbole ill‚gal pour accŠs … la property
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
parser_e_call_by_ref_without_typeconv=E_call by var parameters have to match exactly
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
parser_d_compiling_second_time=D_Compiling $1 for the second time
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_paras_allowed=E_Array properties aren't allowed at this point
% You cannot use  array properties at that point.a
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
% This procedure directive cannot be part of a procedural of function
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
% The value of a \var{default} declared property must be knwon at compile
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
parser_e_empty_import_name=W_Empty import name specified
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_used_proc_name_changed=E_Function internal name changed after use of function
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
parser_e_duplicate_message_label=E_Duplicate message label: %1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=E_Self can be only an explicit parameter in message handlers
% The self parameter can be passed only explicit if it is a method which
% is declared as message method handler
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
% not a good idea to do, you can better use the {$mode objfpc} or {$mode delphi}
% directives which load the unit automaticly
parser_e_no_object_override=E_OVERRIDE can't be used in objects
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
type_e_mismatch=E_Type mismatch
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=E_Incompatible types: got $1 expected $2
% There is no conversion possible between the two types
type_e_not_equal_types=E_Type mismatch between $1 and $2
% The types are not equal
type_e_integer_expr_expected=E_Integer expression expected
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_ordinal_expr_expected=E_Ordinal expression expected
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_type_id_expected=E_Type identifier expected
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=E_Variable identifier expected
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_pointer_type_expected=E_pointer type expected
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=E_class type expected
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
% When compiling in \var{\{\$V+ \}} mode, the string you pass as a parameter
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
type_e_typeconflict_in_set=E_Type conflict between set elements
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=W_lo/hi(longint/dword) returns the upper/lower word
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=E_Integer or real expression expected
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=E_Wrong type in array constructor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=E_Incompatible type for arg #$1: Got $2, expected $3
% You are trying to pass an invalid type for the specified parameter.
% \end{description}
type_e_no_method_and_procedure_not_compatible=E_Method (variable) and Procedure (variable) are not compatible
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=E_Illegal constant passed to internal math function
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=E_Can't get the address of constants
% It's not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
% \end{description}
#
# Symtable
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
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
% The compiler encountered an unknown type.
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
sym_e_ill_label_decl=E_Illegal label declaration
sym_e_goto_and_label_not_supported=E_GOTO und LABEL are not supported (use switch -Sg)
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
sym_h_para_identifier_not_used=H_Parameter not used $1
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=N_Local variable not used $1
% You have declared, but not used a variable in a procedure or function
% implementation.
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
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the right-hand side of an
% assigment)
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
% The var{extended} type is not supported on the m68k platform.
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
cg_e_unable_inline_object_methods=E_Object or class methods can't be inline.
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=E_Procvar calls can't be inline.
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=E_No code for inline procedure stored
% The compiler couldn't store code for the inline procedure.
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
% \end{description}
# EndOfTeX
#
# Assembler reader
#
asmr_d_start_reading=D_Starting $1 styled assembler parsing
asmr_d_finish_reading=D_Finished $1 styled assembler parsing
asmr_e_none_label_contain_at=E_Non-label pattern contains @
asmr_w_override_op_not_supported=W_Override operator not supported
asmr_e_building_record_offset=E_Error building record offset
asmr_e_offset_without_identifier=E_OFFSET used without identifier
asmr_e_no_local_or_para_allowed=E_Cannot use local variable or parameters here
asmr_e_need_offset=E_need to use OFFSET here
asmr_e_cant_have_multiple_relocatable_symbols=E_Cannot use multiple relocatable symbols
asmr_e_only_add_relocatable_symbol=E_Relocatable symbol can only be added
asmr_e_invalid_constant_expression=E_Invalid constant expression
asmr_e_relocatable_symbol_not_allowed=E_Relocatable symbol is not allowed
asmr_e_invalid_reference_syntax=E_Invalid reference syntax
asmr_e_local_symbol_not_allowed_as_ref=E_Local symbols not allowed as references
asmr_e_wrong_base_index=E_Invalid base and index register usage
asmr_e_wrong_scale_factor=E_Wrong scale factor specified
asmr_e_multiple_index=E_Multiple index register usage
asmr_e_invalid_operand_type=E_Invalid operand type
asmr_e_invalid_string_as_opcode_operand=E_Invalid string as opcode operand: $1
asmr_w_CODE_and_DATA_not_supported=W_@CODE and @DATA not supported
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
asmr_e_void_function=W_Functions with void return value can't return any value in asm code
asmr_e_SEG_not_supported=E_SEG not supported
asmr_e_size_suffix_and_dest_dont_match=E_Size suffix and destination or source size do not match
asmr_w_size_suffix_and_dest_dont_match=W_Size suffix and destination or source size do not match
asmr_e_syntax_error=E_Assembler syntax error
asmr_e_invalid_opcode_and_operand=E_Invalid combination of opcode and operands
asmr_e_syn_operand=E_Assemler syntax error in operand
asmr_e_syn_constant=E_Assemler syntax error in constant
asmr_e_invalid_string_expression=E_Invalid String expression
asmr_w_const32bit_for_address=32bit constant created for address
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

#
# Executing linker/assembler
#
exec_w_source_os_redefined=W_Source operating system redefined
exec_i_assembling_pipe=I_Assembling (pipe) $1
exec_d_cant_create_asmfile=E_Can't create assember file $1
exec_w_assembler_not_found=W_Assembler $1 not found, switching to external assembling
exec_t_using_assembler=T_Using assembler: $1
exec_w_error_while_assembling=W_Error while assembling exitcode $1
exec_w_cant_call_assembler=W_Can't call the assembler, error $1 switching to external assembling
exec_i_assembling=I_Assembling $1
exec_w_linker_not_found=W_Linker $1 not found, switching to external linking
exec_t_using_linker=T_Using linker: $1
exec_w_objfile_not_found=W_Object $1 not found, Linking may fail !
exec_w_libfile_not_found=W_Library $1 not found, Linking may fail !
exec_w_error_while_linking=W_Error while linking
exec_w_cant_call_linker=W_Can't call the linker, switching to external linking
exec_i_linking=I_Linking $1
exec_w_binder_not_found=W_binder not found, switching to external binding
exec_w_ar_not_found=W_ar not found, switching to external ar
exec_e_dll_not_supported=E_Dynamic Libraries not supported
exec_i_closing_script=I_Closing script $1
exec_w_res_not_found=W_resource compiler not found, switching to external mode
exec_i_compilingresource=I_Compiling resource $1
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
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=U_PPU File too short
% When you use the \var{-vu} flag, the unit time is shown.
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
% An err
unit_f_ppu_read_error=F_reading PPU-File
% Unexpected end of file
unit_f_ppu_read_unexpected_end=F_unexpected end of PPU-File
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
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
% When you use the \var{-vu} flag, the compiler warns if the assembler of
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
% you cannot nest response files with the \var {@file} command-line option.
option_no_source_found=F_No source file name in command line
% The compiler expects a source file name on the command line.
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
option_obsolete_switch=W_You are using the obsolete switch $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=W_You are using the obsolete switch $1, please use $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
%
# EndOfTeX
#
# Logo (option -l)
#
option_switch_bin_to_src_assembler=N_Switching assembler to default source writing assembler
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_logo_start=Compilatuer Free Pascal version $FPCVER [$FPCDATE] pour $FPCTARGET
option_logo_end=Copyright (c) 1998-2000 by Florian Klaempfl
#
# Info (option -i)
#
option_info_start=Compilatuer Free Pascal version $FPCVER
option_info1=<lf>
option_info2=Date du compilateur  : $FPCDATE
option_info3=Cible du compilateur : $FPCTARGET
# the next lines are NOT translated on purpose !!!
option_info4=<lf>
option_info5=This program comes under the GNU General Public Licence
option_info6=For more information read COPYING.FPC
option_info7=<lf>
option_info8=Report bugs,suggestions etc to:
option_info_end=                 fpc-devel@vekoll.saturnus.vein.hu
#
# Help pages (option -? and -h)
#
# note: the ordering of the enum numbering doens't matter
#
ol_begin=**0*_mettre + aprŠs un bool‚en pour activer l'option, - pour la d‚sactiver
ol001=**1a_conserve les fichiers assembleurs cr‚‚s pendant la compilation
ol002=**2al_liste le code source dans les fichiers assembleur
ol003=**2ar_liste les allocations de registres dans les fichiers assembleur
ol004=**2at_liste les allocations de donn‚es temporaire dans la pile
ol005=**1b_g‚nŠre les informations pour le browser
ol006=**2bl_g‚nŠre les informations locales ‚galement
ol007=**1B_recompile toutes les unit‚s
ol008=**1C<x>_options de g‚n‚ration de code :
ol009=3*2CD_cr‚erune librairie dynamique
ol010=**2Ch<n>_<n> taille du tas en bytes (entre 1023 et 67107840)
ol011=**2Ci_IO-checking
ol012=**2Cn_pas de linking
ol013=**2Co_g‚nŠre des tests d'overflow pour les op‚rations sur les entiers
ol014=**2Cr_Controle d'intervalles
ol015=**2Cs<n>_sp‚cifie <n> comme  taille de la pile
ol016=**2Ct_test de d‚bordement de pile
ol017=3*2CS_cr‚er une librairie statique
ol018=3*2Cx_utiliser le smartlinking
ol019=**1d<x>_d‚finit le symbole <x>
ol020=*O1D_g‚nŠre un fichier DEF
ol021=*O2Dd<x>_assigne la description … <x>
ol022=*O2Dw_application en mode prot‚g‚
ol023=**1e<x>_d‚finit le chemin vers l'ex‚cutable
ol024=**1E_comme -Cn
ol025=**1F<x>_d‚finit des chemins et noms de fichiers :
ol026=**2FD<x>_d‚finit le r‚pertoire o— chercher les utilitaires de compilation
ol027=**2Fe<x>_redirige les erreurs vers le fichier <x>
ol028=**2FE<x>_r‚pertoire pour les exe/unit‚s : <x>
ol029=**2Fi<x>_ajoute <x> … la liste des r‚pertoires pour fichiers inclus
ol030=**2Fl<x>_ajoute <x> … la liste des r‚pertoires pour librairies
ol031=*L2FL<x>_utilises <x> comme lieur dynamique
ol032=**2Fo<x>_ajoute <x> … la liste des r‚pertoires pour fichiers objets
ol033=**2Fr<x>_charge le fichier erreur <x>
ol034=**2Fu<x>_ajoute <x> … la liste des r‚pertoires pour unit‚s
ol035=**2FU<x>_d‚finit le r‚pertoire d'‚criture des unit‚s … <x>, en d‚pit de -FE
ol036=*g1g<x>_g‚nŠre des informations de d‚bogage :
ol037=*g2gg_utilise gsym
ol038=*g2gd_utilise dbx
ol039=*g2gh_use l'unit‚ de tracage du tas
ol040=*g2gc_generate checks for pointers
ol041=**1i_information
ol042=**2iD_donne la date du compilateur
ol043=**2iV_donne la version du compilateur
ol044=**2iSO_donne l'OS du compilateur
ol045=**2iSP_donne le processeur du compilateur
ol046=**2iTO_donne l'OS cible
ol047=**2iTP_donne le processeur cible
ol048=**1I<x>_ajoute <x> … la liste des r‚pertoires pour fichiers inclus
ol049=**1k<x>_transmet <x> au linker
ol050=**1l_‚crit le logo
ol051=**1n_ne pas lire le fichier de configuration par d‚faut
ol052=**1o<x>_change le nom de l'executable en <x>
ol053=**1pg_g‚nŠre du code pour profiler avec gprof
ol054=*L1P_utilise des pipes au lieu de cr‚er des fichiers temporaires
ol055=**1S<x>_options de syntaxe :
ol056=**2S2_autorise quelques extensions Delphi 2
ol057=**2Sc_autorise les operateurs type C (*=,+=,/= et -=)
ol058=**2Sd_essaye d'ˆtre compatible avec Delphi
ol059=**2Se_stoppe la compilation … la premiŠre erreur
ol060=**2Sg_autorise LABEL et GOTO
ol061=**2Sh_Utilise les ansistrings
ol062=**2Si_supporte les INLINE type C++
ol063=**2Sm_support des macros comme C (global)
ol064=**2So_essaye d'ˆtre compatible avec TP/BP 7.0
ol065=**2Sp_essaye d'ˆtre compatible avec GPC
ol066=**2Ss_les constructeurs doivent s'appeler init (et les destructeurs done)
ol067=**2St_autorise "static" dans les objects
ol068=**1s_n'appelle pas l'assembleur ni le linker
ol069=**1u<x>_rend le symbole <x> non d‚fini
ol070=**1U_options d'unit‚s :
ol071=**2Un_ne pas v‚rifier le nom de l'unit‚
ol072=**2Us_compiler en temps qu'unit‚ systŠme
ol073=**1v<x>_donne des informations. <x> est une combination des lettres suivantes :
ol074=**2*_e : montre les erreurs (d‚faut)   d : informations de d‚boggage
ol075=**2*_w : montre les avertissements     u : montre les info sur les unit‚s
ol076=**2*_n : montre les notes              t : montre les fichiers essay‚s/utilis‚s
ol077=**2*_h : montre les suggestions        m : montre les macros d‚finies
ol078=**2*_i : montre des infos g‚n‚rales    p : montre les proc‚dures compil‚es
ol079=**2*_l : montre les num‚ros de lignes  c : montre les conditionniels
ol080=**2*_a : montre tout                   0 : ne montre rien (sauf les erreurs)
ol081=**2*_b : montre toutes les proc‚dures  r : mode compatibilit‚ Rhide/GCC
ol082=**2*_    quand une erreur survient     x : infos Executable (Win32 seulement)
ol083=**2*_
ol084=**1X_options pour executable :
ol085=*L2Xc_lien avec librairie C
ol086=**2XD_lien avec la librarie dynamique (d‚finit FPC_LINK_DYNAMIC)
ol087=**2Xs_enlˆve tous les symboles de l'executable
ol088=**2XS_lien avec les librairies statiques (d‚finit FPC_LINK_STATIC)
ol089=**0*_options sp‚cifiques au processeur :
ol090=3*1A<x>_format de sortie :
ol091=3*2Aas_fichier objet g‚n‚r‚ par GNU AS
ol092=3*2Aasaout_fichier objet g‚n‚r‚ par GNU AS pour aout (Go32v1)
ol093=3*2Anasmcoff_fichier COFF (Go32v2) avec Nasm
ol094=3*2Anasmelf_fichier ELF32 (Linux) avec Nasm
ol095=3*2Anasmobj_fichier OBJ avec Nasm
ol096=3*2Amasm_fichier OBJ avec Masm (Mircosoft)
ol097=3*2Atasm_fichier OBJ avec Tasm (Borland)
ol098=3*2Acoff_coff (Go32v2) using internal writer
ol099=3*2Apecoff_pecoff (Win32) using internal writer
ol100=3*1R<x>_type lecture assembleur :
ol101=3*2Ratt_lit l'assembleur AT&T
ol102=3*2Rintel_lit l'assembleur Intel
ol103=3*2Rdirect_copie le texte assembleur directement dans le fichier assembleur
ol104=3*1O<x>_optimisations :
ol105=3*2Og_g‚nŠre du code compact
ol106=3*2OG_g‚nŠre du code rapide (defaut)
ol107=3*2Or_garde certaines variables dans des registres (toujours BUGGY!!!)
ol108=3*2Ou_autorise les optimisations incertaines (voir docs)
ol109=3*2O1_optimisations niveau 1 (optimisations rapides)
ol110=3*2O2_optimisations niveau 2 (-O1 + optimisations plus lentes)
ol111=3*2O3_optimisations niveau 3 (comme -O2u)
ol112=3*2Op<x>_processeur cible :
ol113=3*3Op1_d‚finit 386/486 comme processeur cible
ol114=3*3Op2_d‚finit Pentium/PentiumMMX (tm) comme processeur cycle
ol115=3*3Op3_d‚finit  PPro/PII/c6x86/K6 (tm) comme processeur cycle
ol116=3*1T<x>_systŠme d'explioitation cible :
ol117=3*2TGO32V1_version 1 de l'extension DOS de DJ Delorie
ol118=3*2TGO32V2_version 2 de l'extension DOS de DJ Delorie
ol119=3*2TLINUX_Linux
ol120=3*2TOS2_OS/2 2.x
ol121=3*2TWin32_Windows 32 Bits
ol122=6*1A<x>_output format
ol123=6*2Aas_Unix o-file using GNU AS
ol124=6*2Agas_GNU Motorola assembler
ol125=6*2Amit_MIT Syntax (old GAS)
ol126=6*2Amot_Standard Motorola assembler
ol127=6*1O_optimizations:
ol128=6*2Oa_turn on the optimizer
ol129=6*2Og_generate smaller code
ol130=6*2OG_generate faster code (default)
ol131=6*2Ox_optimize maximum (still BUGGY!!!)
ol132=6*2O2_set target processor to a MC68020+
ol133=6*1R<x>_styles d'assembleur :
ol134=6*2RMOT_lire l'assembleur type motorola
ol135=6*1T<x>_SystŠme d'exploitation cible :
ol136=6*2TAMIGA_Commodore Amiga
ol137=6*2TATARI_Atari ST/STe/TT
ol138=6*2TMACOS_Macintosh m68k
ol139=6*2TLINUX_Linux-68k
ol140=**1*_
ol141=**1?_affiche cette aide
ol_end=**1h_affiche cette aide sans attente
#
# The End...
#

