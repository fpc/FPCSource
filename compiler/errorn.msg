#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998-2000 by the Free Pascal Development team
#
#   Dutch Language File for Free Pascal
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
# Enkele punten om bij het vertalen in het achterhoofd te houden:
#
# - "Methoden" wordt vaak foutief als "methodes" geschreven.
# - "Typen" wordt vaak foutief als "types" geschreven.
# - "Illegal" wordt niet met "illegaal" vertaald, maar met "ongeldig".
#   Illegaal betekent onwettig in het Nederlands.
# - Zo wordt "execute" niet met "executeren" vertaalt maar met "uitvoeren".
#   Programma, hebt u nog een laatste wens voordat de trekker wordt
#       overgehaald?
# - In het Nederlands schrijft men woorden vaker aan elkaar dan in het
#   Engels: "compiler switch" (2 woorden ) wordt "compileroptie" (1 woord).
#   Let ook op voorvoegingen: "Interface and implementation names" wordt:
#    "Interface- en implementatienamen" (streepje!)
# - Pas op met het vernederlandsen van Engelse woorden. Bijvoorbeeld
#   "symbool". Voor deze categorie woorden bestaat geen goede
#   vertaling. Iedereen die zo'n vernederlandsing ziet krijgt echter gelijk
#   een pijnscheut in zijn taalknobbel. Soms helpt een woordenboek, anders
#   is het beter om de engelse term te laten staan. Wat improvisatie helpt
#   soms ook, "symbool" is niet precies hetzelfde maar kan in dit geval
#   prima als vertaling gebruikt worden.

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
general_d_sourceos=D_Bronsysteem: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=I_Doelsysteem: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=T_Programmalocatie: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=U_Locatie units: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=T_Locatie includes: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=T_Locatie bibliotheken: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=T_Locatie objectbestanden: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=I_$1 Regels gecompileerd, $2 sec.
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=F_Geen geheugen meer vrij
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
scan_f_end_of_file=F_Onverwacht einde van bestand
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment wasn't closed.
% \end{itemize}
scan_f_string_exceeds_line=F_String langer dan regel
% You forgot probably to include the closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=F_ongeldig teken
% An illegal character was encountered in the input file.
scan_f_syn_expected=F_Taalfout: $2 verwacht in kolom $1
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=T_Ingevoegd bestand $1 word geopend
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=W_Commentaar van niveau $1 gevonden
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_far_directive_ignored=N_$F directive (FAR) genegeerd
% The \var{FAR} directive is a 16-bit construction which is recorgnised
% but ignored by the compiler, since it produces 32 bit code.
scan_n_stack_check_global_under_linux=N_Stapeloverloop test is globaal onder linux
% Stack checking with the \var{-Cs} switch is ignored under \linux, since
% \linux does this for you. Only displayed when \var{-vn} is used.
scan_n_ignored_switch=N_Genegeerde compileroptie $1
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=W_Foutieve compileroptie $1
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% doesn't know.
scan_w_switch_is_global=W_Deze compileroptie heeft ook een globaal effect
% When \var{-vw} is used, the compiler warns if a switch is global.
scan_e_illegal_char_const=E_Foutieve constante voor een karakter
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range. The range
% is 1-255.
scan_f_cannot_open_input=F_Kan bestand niet openen
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=F_Kan ingevoerd bestand niet openen $1
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_too_much_endifs=E_Te veel $ENDIFs of $ELSEs
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_w_only_pack_records=W_Record velden kunnen op 1,2 of 4 bytes uitgelijnd worden.
% You are specifying the \var{\{\$PACKRECORDS n\} } with an illegal value for
% \var{n}. Only 1,2,4 or 16 are valid in this case.
scan_w_only_pack_enum=W_Enumeraties kunnen alleen worden bewaard in 1,2 of 4 bytes
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2 or 4 are valid in this case.
scan_e_endif_expected=E_$ENDIF verwacht voor $1 op $2 $3
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=E_Taalfout bij het compileren van een conditionele compilatie uitdrukking
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_e_error_in_preproc_expr=E_Evalueren van een conditionele compilatie uitdrukking
% There is an error in the expression following the \var{\{\$if ..\}} compiler
% directive.
scan_w_marco_cut_after_255_chars=W_Macro inhoud is afgekapt op 255 karakters voor evaluatie
% The contents of macros canno be longer than 255 characters. This is a
% safety in the compiler, to prevent buffer overflows. This is shown as a
% warning, i.e. when the \var{-vw} switch is used.
scan_e_endif_without_if=E_ENDIF zonder IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=F_Gebruiker definitie: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=E_Gebruiker definitie: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=W_Gebruiker definitie: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=N_Gebruiker definitie: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=H_Gebruiker definitie: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=I_Gebruiker definitie: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=E_Sleutelwoord herdefinieren als macro heeft geen effect
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=F_Macro buffer overflow bij lezen of expansie van macro
% Your macro or it's result  was too long for the compiler.
scan_w_macro_deep_ten=W_Expansie van macro meer dan 16 niveaus diep, waarschijnlijk een recursieve macro
% When expanding a macro macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_wrong_styled_switch=E_Compileroptie is niet toegestaan in (* ... *) commentaar
% Compiler switches should always be between \var{\{ \}} comment delimiters.
scan_d_handling_switch=D_Behandel schakelaar "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=C_ENDIF $1 gevonden
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=C_IFDEF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=C_IFOPT $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=C_IF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=C_IFNDEF $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=C_ELSE $1 gevonden, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=C_Negeer tot...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=I_Druk <return> om verder te gaan
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=W_Niet ondersteunde optie $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=W_Ongeldige compiler optie $1
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=T_Terug in $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=W_Applicatie type niet ondersteund: $1
% You get this warning, ff you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=W_$APPTYPE niet ondersteund op doelsysteem
% The \var{\{\$APPTYPE\}} directive is supported by win32 applications only.
scan_w_unsupported_asmmode_specifier=W_Niet ondersteunde assembler soort gegeven $1
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=W_ASM lezer-optie is niet mogelijk in een asm instructie, $1 ingesteld voor volgende blok
scan_w_unsupported_app_type=W_Niet ondersteund programmatype: $1
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=E_Verkeerde optiewaarde, gebruik ON/OFF of +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=E_Resource bestanden worden niet ondersteund op doel OS
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
parser_e_syntax_error=E_Parser - Taalfout
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_w_proc_far_ignored=W_Procedure type FAR genegeerd
% This is a warning. \var{FAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_near_ignored=W_Procedure type NEAR genegeerd
% This is a warning. \var{NEAR} is a construct for 8 or 16 bit programs. Since
% the compile generates 32 bit programs, it ignores this directive.
parser_w_proc_register_ignored=W_Procedure type REGISTER wordt genegeerd
% This is a warning. \var{REGISTER} is ignored by FPC programs for now.
% This is introduced first for Delphi compatibility.
parser_e_no_dll_file_specified=E_Geen DLL bestand opgegeven
% No longer in use.
parser_e_export_name_double=E_Dubbel geexporteerde functie naam $1
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=E_Dubbel geexporteerde functie index $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=E_Ongeldige index for geexporteerde functie
% DLL function index must be in the range \var{1..\$FFFF}
parser_e_constructorname_must_be_init=E_Constructornaam moet INIT zijn
% You are declaring a constructor with a name which isn't \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=E_Destructornaam moet DONE zijn
% You are declaring a constructor with a name which isn't \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_illegal_open_parameter=E_Foutieve open parameter
% You are trying to use the wrong type for an open parameter.
parser_e_proc_inline_not_supported=E_Procedure type INLINE wordt niet ondersteund
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_priv_meth_not_virtual=W_Private methoden zijn niet VIRTUAL
% You declared a method in the private part of a object (class) as
% \var{virtual}. This is not allowed. Private methods cannot be overridden
% anyway.
parser_w_constructor_should_be_public=E_Constructor mag niet private or protected zijn
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=E_Destructor mag niet private or protected zijn
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=N_Klasse heeft slechts 1 destructor
% You can declare only one destructor for a class.
parser_e_no_local_objects=E_Lokale klassedefinities zijn niet toegestaan
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=F_Anonieme klassedefinities zijn niet toegestaan
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_object_has_no_vmt=E_Het object $1 heeft geen VMT
parser_e_illegal_parameter_list=E_Foutieve parameterlijst
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_type=E_Foutief parametertype meegegeven voor arg #$1
% There is an error in the parameter list of the function or procedure.
% The compiler cannot determine the error more accurate than this.
parser_e_wrong_parameter_size=E_Onjuist aantal parameters meegegeven
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=E_Overroepen symbool is geen procedure of functie
% The compiler encountered a symbol with the same name as an overloaded
% function, but it isn't a function it can overload.
parser_e_overloaded_have_same_parameters=E_Overroepen procedures hebben identieke parameterlijst
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=E_Declaratie komt niet overeen met eerdere declaratie: $1
% You declared a function with same parameters but
% different result type or function specifiers.
parser_e_header_different_var_names=E_Procedurehoofding $1 komt niet overeen met eerdere declaratie: variabelenaam veranderd $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=N_Waarden in opsommingstypes moeten steeds stijgen
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_n_interface_name_diff_implementation_name=N_Interface- en implementatienamen zijn verschillend
% This note warns you if the implementation and interface names of a
% functions are different, but they have the same mangled name. This
% is important when using overloaded functions (but should produce no error).
parser_e_no_with_for_variable_in_other_segments=E_With kan niet gebruikt worden voor variabelen in verschillende segmenten
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=E_Procedure meer dan 31 keer genest
% You can nest function definitions only 31 times.
parser_e_range_check_error=E_Bereikfout bij evalueren constanten
% The constants are out of their allowed range.
parser_w_range_check_error=W_Bereikscontrolefout bij evalueren constantes.
% The constants are out of their allowed range.
parser_e_double_caselabel=E_Dubbel case-element
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=E_Bovengrens van casebereik is lager dan ondergrens
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=E_Getypeerde constanten van klassen zijn niet toegestaan
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=E_Procedurele variabelen van overroepen procedures zijn niet toegestaan
% You are trying to assign an overloaded function to a procedural variable.
% This isn't allowed.
parser_e_invalid_string_size=E_Stringlengte moet tussen 1 en 255 liggen
% The length of a string in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
% (This is not true for \var{Longstrings} and \var{AnsiStrings}.
parser_w_use_extended_syntax_for_objects=W_Gebruik de uitgebreide syntax van DISPOSE en NEW om nieuwe instanties van klassen te genereren
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the class.
parser_w_no_new_dispose_on_void_pointers=W_Het gebruik van NEW of DISPOSE voor pointers zonder type is zinloos
parser_e_no_new_dispose_on_void_pointers=E_Het gebruik van NEW of DISPOSE is niet mogelijk voor pointers zonder type
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=E_Klasse verwacht
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=E_Typesymbool is hier niet toegestaan
% You cannot use a type inside an expression.
parser_e_methode_id_expected=E_Methode verwacht
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=E_Proceduredeclaratie komt niet overeen met een methode van deze klasse
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_p_procedure_start=P_Procedure/functie $1 ($2) op regel $3
% When using the \var{-vp} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=E_Foutieve vlottende-komma constante
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=E_FAIL mag alleen in constructors gebruikt worden
% You are using the \var{FAIL} instruction outside a constructor method.
parser_e_no_paras_for_destructor=E_Destructors hebben geen parameters
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=E_Alleen klassemethoden kunnen gerefereerd worden via een klasse
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
parser_e_only_class_methods=E_Alleen klassemethoden zijn toegankelijk in klassen
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
parser_e_case_mismatch=E_Type van constante komt niet overeen met dat van de case uitrukking
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=E_Het symbool kan niet uitgevoerd worden uit een bibliotheek
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=E_Een virtuele methode moet met OVERRIDE overroepen worden: $1
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=E_Er is geen geerfde methode die overroepen kan worden: $1
% You try to \var{override} a virtual method of a parent class that doesn't
% exist.
parser_e_no_procedure_to_access_property=E_Er is geen lid om de eigenschap te bereiken
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=W_Stored prorperty directive is not yet implemented
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=E_Ongeldig symbool voor eigenschap toegang
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code woud cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=E_Kan niet in beschermd veld van een object schrijven
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module wher the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=E_Kan niet in prive-veld van een object schrijven
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_w_overloaded_are_not_both_virtual=W_Alle overroepen methoden moeten virtueel zijn als 1 virtueel is: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_w_overloaded_are_not_both_non_virtual=W_Overroepen methode van een niet virtuele methode moet niet-virtueel zijn: $1
% If you declare overloaded methods in a class, then they should either all be
% virtual, or none. You shouldn't mix them.
parser_e_overloaded_methoden_not_same_ret=E_Virtuele overroepen methoden moeten hetzelfde resultaat type hebben: $1
% If you declare virtual overloaded methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=E_Als EXPORT gedeclareerde procedures kunnen niet genest worden
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=E_Methoden kunnen niet geexporteerd worden.
% You cannot declare a procedure that is a method for an object as
% \var{export}ed. That is, your methods cannot be called from a C program.
parser_e_call_by_ref_without_typeconv=E_Typen van referentieparameters moeten exact overeenkomen.
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=E_Deze klasse is geen ouderklasse van de huidige klasse
% When calling inherited methods, you are trying to call a method of a strange
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=E_SELF is alleen toegelaten in methoden.
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=E_Methoden kunnen alleen in andere methoden worden aangeroepen met type symbool
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=E_Ongeldig gebruik van ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=E_Bereikfout in setconstructor of dubbel setelement
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=E_Pointer naar klasse verwacht
% You specified an illegal type in a \var{New} statement.
% The extended synax of \var{New} needs an  object as a parameter.
parser_e_expr_have_to_be_constructor_call=E_Uitdrukking moet een constructoraanroep zijn.
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=E_Uitdrukking moet een destructoraanroep zijn
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=E_Ongeldige volgorde van record leden
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=E_Type van uitdrukking moet class of record type zijn
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=E_Procedures kunnen geen resultaat teruggeven
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures  cannot return a value.
parser_e_constructors_always_objects=E_Constructors en destructors moeten methoden zijn
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=E_Operator is niet hergedefineerd
% You're trying to use an overloaded operator when it isn't overloaded for
% this type.
parser_e_no_reraise_possible=E_Re-raise is hier niet mogelijk
% You are trying to raise an exception where it isn't allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=E_De uitgebreide syntax van NEW of DISPOSE is niet toegestaan voor klassen
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{Dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_asm_incomp_with_function_return=E_Assembler stemt niet overeen met type van functieresultaat
% You're trying to implement a \var{assembler} function, but the return type
% of the function doesn't allow that.
parser_e_procedure_overloading_is_off=E_Procedure overroepen is afgezet
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=E_Deze operator kan niet overroepen worden.
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=E_Vergelijkingsoperator moet een boolean type als resultaat hebben.
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=E_Enkel virtuele methoden kunnen abstract zijn
% You are declaring a method as abstract, when it isn't declared to be
% virtual.
parser_f_unsupported_feature=F_Gebruik van niet ondersteunde optie !
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=E_CLASSES and OBJECTS kunnen niet gemengd worden.
% You cannot derive \var{objects} and \var{classes} intertwined . That is,
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=W_Onbekende proceduredirective is genegeerd: $1
% The procedure direcive you secified is unknown. Recognised procedure
% directives are \var{cdecl}, \var{stdcall}, \var{popstack}, \var{pascal}
% \var{register}, \var{export}.
parser_e_absolute_only_one_var=E_Absolute kan maar aan een variabele verbonden worden.
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
parser_e_absolute_only_to_var_or_const=E_Absolute kan slechts aan variabelen of constanten verbonden worden.
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=E_Slechts 1 variabele kan geinitialiseerd worden.
% You cannot specify more than one variable with a initial value
% in Delphi syntax.
parser_e_abstract_no_definition=E_Abtracte methoden kunnen niet gedefineerd worden
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=E_Deze overroepen procedure kan niet lokaal zijn (moet geexporteerd worden)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=W_Gebruik van virtuele methoden zonder constructor
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_m_macro_defined=M_Macro gedefinieerd: $1
% When \var{-vm} is used, the compiler tells you when it defines macros.
parser_m_macro_undefined=M_Macro opgeheven: $1
% When \var{-vm} is used, the compiler tells you when it undefines macros.
parser_m_macro_set_to=M_Macro $1 wordt $2
% When \var{-vm} is used, the compiler tells you what values macros get.
parser_i_compiling=I_Compilatie van $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_d_compiling_second_time=D_Compilatie van $1 voor de tweede keer.
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_paras_allowed=E_Array eigenschappen zijn hier niet toegestaan.
% You cannot use  array properties at that point.a
parser_e_no_property_found_to_override=E_Geen eigenschap gevonden om te herdefinieren
% You want to overrride a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=E_Slechts 1 standaardeigenschap is toegestaan, geerfde standaardeigenschap in kind gevonden.
% You specified a property as \var{Default}, but a parent class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=E_Standaardeigenschappen moeten van type array zijn.
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=E_Virtuele constructors worden alleen ondersteunt in het klassemodel
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=E_Geen standaardeigenschap gevonden
% You try to access a default property of a class, but this class (or one of
% it's ancestors) doesn't have a default property.
parser_e_cant_have_published=E_De klasse kan geen gepubliceerde sectie hebben, gebruik {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=E_Declaratie van klasse $1 moet worden opgelost om de klasse als ouder te gebruiken
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=E_Lokale operatoren worden niet ondersteund
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=E_Procedure directive $1 niet toegestaan in interface sectie
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=E_Procedure directive $1 niet toegestaan in implementation sectie
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=E_Procedure directive $1 niet toegestaan in procvar declaratie
% This procedure directive cannot be part of a procedural of function
% type declaration.
parser_e_function_already_declared_public_forward=E_Functie is al publiek/forward gedeclareerd $1
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=E_Kan niet zowel EXPORT als EXTERNAL gebruiken
% These two procedure directives are mutually exclusive
parser_e_name_keyword_expected=E_NAME sleutelwoord verwacht
% The definition of an external variable needs a \var{name} clause.
parser_w_not_supported_for_inline=W_$1 wordt niet ondersteund voor inline procedure/functie
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=W_Inlining uitgeschakeld
% Inlining of procedures is disabled.
parser_i_writing_browser_log=I_Schrijven van browser logfile $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=H_misschien is het een pointer, ^ wordt dan gemist
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=F_Geselecteerde assemblerlezer wordt niet ondersteund
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=E_Procedure directive $1 geeft conflicten met andere directives
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=E_Aanroep conventie klopt niet met voorwaartse
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_register_calling_not_supported=E_Registeraanroep (fastcall) wordt niet ondersteund
% The \var{register} calling convention, i.e., arguments are passed in
% registers instead of on the stack is not supported. Arguments are always
% passed on the stack.
parser_e_property_cant_have_a_default_value=E_Eigenschap kan geen standaard waarde hebben
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=E_De standaard waarde van een eigenschap moet een constante zijn
% The value of a \var{default} declared property must be knwon at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=E_Symbool kan niet worden gepubliceerd, alleen mogelijk voor een klasse
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=E_Dit soort eigenschappen kunnen niet worden gepubliceerd
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_w_empty_import_name=W_Lege importnaam opgegeven.
% Both index and name for the import are 0 or empty
parser_e_empty_import_name=W_Lege importnaam opgegeven
% Some targets need a name for the imported procedure or a cdecl specifier
parser_e_used_proc_name_changed=E_Interne functienaam gewijzigd na declaratie.
parser_e_division_by_zero=E_Deling door nul
% There is a divsion by zero encounted
parser_e_invalid_float_operation=E_Ongeldige vlottende-komma bewerking.
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=E_Bovengrens van bereik is lager dan ondergrens
% The upper bound of a \var{case} label is less than the lower bound and this
% is not possible
parser_e_string_larger_array=E_Stringlengte moet tussen 1 en 255 liggen
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=E_Ongeldige uitdrukking achter message optie
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=E_Message handlers aanvaarden slechts 1 variabele parameter
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=E_Dubbel message label: %1
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=E_Self kan alleen een explicite parameter zijn bij string message handlers
% The self parameter can be passed only explicit if it is a method which
% is declared as message method handler
parser_e_threadvars_only_sg=E_Thread variabelen kunnen alleen globaal of statisch zijn
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has it's own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=F_Directe assembler wordt niet ondersteund door interne assembler.
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=W_Laad de OBJPAS unit niet manueel, gebruik {$mode objfpc} of {$mode delphi}.
% You're trying to load the ObjPas unit manual from a uses clause. This is
% not a good idea to do, you can better use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automaticly
parser_e_no_object_override=E_OVERRIDE kan niet gebruikt worden in objecten
% Override isn't support for objects, use VIRTUAL instead to override
% a method of an anchestor object
parser_e_cant_use_inittable_here=E_Data typen die initializatie/finalizatie vereisen zijn niet toegestaan in variabele records.
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
% \end{description}
#
# Type Checking
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=E_Typen stemmen niet overeen
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=E_Incompatibele typen: kreeg $1, verwachtte $2
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
type_e_not_equal_types=E_Typen komen niet overeen $1 en $2
% The types are not equal
type_e_integer_expr_expected=E_Integerexpressie verwacht
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_ordinal_expr_expected=E_Ordinale expressie verwacht
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_type_id_expected=E_Typesymbool verwacht
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=E_Variabelesymbool verwacht
% This happens when you pass a constant to a \var{Inc} var or \var{Dec}
% procedure. You can only pass variables as arguments to these functions.
type_e_pointer_type_expected=E_Pointertype verwacht
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=E_Klassetype verwacht
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_varid_or_typeid_expected=E_Variabele- of typesymbool verwacht
% The argument to the \var{High} or \var{Low} function is not a variable
% nor a type identifier.
type_e_cant_eval_constant_expr=E_Kan constante expressie niet evalueren
% No longer in use.
type_e_set_element_are_not_comp=E_Elementen van set zijn niet compatible
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=E_Bewerking niet ondersteund voor sets
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=W_Typeconversie van vlottende-komma type naar COMP, wat een integer type is.
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{Comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=H_Gebruik DIV voor gehele getallen om geheel resultaat te krijgen
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=E_String typen stemmen niet overeen wegens de $V+ modus
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=E_SUCC of PRED op enumeratie types met toekenningen zijn niet mogelijk
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=E_Kan dit type variabelen niet lezen of schrijven
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% booleans, reals, pchars and strings can be read from/written to a text file.
type_e_typeconflict_in_set=E_Type conflict tussen set elementen.
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=W_lo/hi(longint/dword) geeft hoog/laag (ipv byte) woord terug.
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword}
% which returns the lower/upper word of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type case the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=E_Integer of vlottende komma uitdrukking verwacht
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=E_Verkeerd type in array constructor
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=E_Incompatibel type voor argument #$1: kreeg $2, verwachtte $3
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=E_Methode (variabele) en Procedure (variable) zijn niet compatible
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=E_Ongeldige constante opgegeven aan interne wiskundige functie
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=E_Een constante heeft geen adres
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
sym_e_id_not_found=E_Symbool niet gevonden $1
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=F_Interne fout in SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=E_Dubbel symbool $1
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=H_Symbool reeds gedefinieerd in $1 op lijn $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=E_Onbekende symbool $1
% The identifier encountered hasn't been declared, or is used outside the
% scope where it's defined.
sym_e_forward_not_resolved=E_Voorwaartse declaratie niet opgelost $1
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_f_id_already_typed=F_Typesymbool reeds gedefineerd als type
% You are trying to redefine a type.
sym_e_error_in_type_def=E_Fout in type definitie
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_type_id_not_defined=E_Typesymbool niet gedefinieerd
% The type identifier has not been defined yet.
sym_e_forward_type_not_resolved=E_Forward declaratie niet opgelost $1
% The compiler encountered an unknown type.
sym_e_only_static_in_static=E_Alleen statische variabelen kunnen gebruikt worden in statische methoden
% A static method of an object can only access static variables.
sym_e_invalid_call_tvarsymmangledname=E_Ongeldige aanroep van tvarsym.mangledname()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
sym_f_type_must_be_rec_or_class=F_Record- of klassetype verwacht
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=E_U kunt geen vertegenwoordiger van een klasse met abstracte methoden instantieren
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=W_Label niet gedefineerd $1
% A label was declared, but not defined.
sym_e_ill_label_decl=E_Label foutief gedefineerd
sym_e_goto_and_label_not_supported=E_GOTO en LABEL zijn afgezet (gebruik -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the  \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=E_Label niet gevonden
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=E_Symbool is geen label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=E_Label is al gedefineerd
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=E_Type declaratie van set elementen is foutief
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=E_Voorwaartse definitie van klasse niet opgelost $1
% You declared a class, but you didn't implement it.
sym_h_para_identifier_not_used=H_Parameter niet gebruikt $1
% This is a warning. The identifier was declared (locally or globally) but
% wasn't used (locally or globally).
sym_n_local_identifier_not_used=N_Lokale variabele niet gebruikt $1
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_e_set_expected=E_Set type verwacht
% The variable or expression isn't of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=W_Resultaat van functie lijkt niet te zijn toegekend
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_e_illegal_field=E_Onbekend record lid $1
% The field doesn't exist in the record definition.
sym_n_uninitialized_local_variable=N_Locale variabele $1 lijkt niet geinitialiseerd te zijn
sym_n_uninitialized_variable=W_Variabele $1 lijkt niet geinitialiseerd te zijn
% These messages are displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% wasn't initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=E_Geen lid met de naam $1
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the class you are trying to create. The procedure you specified
% does not exist.
sym_b_param_list=B_Declaratie gevonden: $1
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
cg_e_break_not_allowed=E_BREAK niet toegelaten
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=E_CONTINUE niet toegelaten
% You're trying to use \var{continue} outside a loop construction.
cg_e_too_complex_expr=E_Uitdrukking te ingewikkeld - overloop van wiskundige processor
% Your expression is too long for the compiler. You should try dividing the
% construct over multiple assignments.
cg_e_illegal_expression=E_Foutieve uitdrukking
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
cg_e_invalid_integer=E_Foutieve gehele uitdruking
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
cg_e_invalid_qualifier=E_Ongeldige constructie
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
cg_e_upper_lower_than_lower=E_Bovengrens is kleiner dan ondergrens
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
cg_e_illegal_count_var=E_Ongeldige lusteller
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
cg_e_cant_choose_overload_function=E_Weet niet welke overroepen procedure moet worden aangeroepen
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
cg_e_parasize_too_big=E_Parameters gebruiken meer dan 64 kilobyte
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_illegal_type_conversion=E_Foute typeconversie
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
cg_d_pointer_to_longint_conv_not_portable=D_Conversie tussen ordinale en pointertypen is niet overdraagbaar naar andere processoren
% If you typecast a pointer to a longint, this code will not compile
% on a machine using 64bit for pointer storage.
cg_e_file_must_call_by_reference=E_File variabelen moeten altijd var parameters zijn
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=E_U kunt daar geen far pointer gebruiken
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_var_must_be_reference=E_Ongeldige aanroep met referentie parameters
% You are trying to pass a constant or an expression to a procedure that
% requires a \var{var} parameter. Only variables can be passed as a \var{var}
% parameter.
cg_e_dont_call_exported_direct=E_Procedures die als EXPORT gedeclareerd staan kunnen niet aangeroepen worden
% No longer in use.
cg_w_member_cd_call_from_method=W_Mogelijke ongeldige aanroep van constructor of destructor
% No longer in use.
cg_n_inefficient_code=N_Inefficiente code
% You construction seems dubious to the compiler.
cg_w_unreachable_code=W_Deze code wordt nooit uitgevoerd
% You specified a loop which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_stackframe_with_esp=E_Procedure aanroep met stackframe ESP/SP
% The compiler encountered a procedure  or function call inside a
% procedure that uses a \var{ESP/SP} stackframe. Normally, when a call is
% done the procedure needs a \var{EBP} stackframe.
cg_e_cant_call_abstract_method=E_Abstracte methoden kunnen niet direct aangeroepen worden
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_f_internal_error_in_getfloatreg=F_Interne fout in getfloatreg(), allocatie faling
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_unknown_float_type=F_Onbekend vlottende-komma type
% The compiler cannot determine the kind of float that occurs in an expression.
cg_f_secondvecn_base_defined_twice=F_SecondVecn() basis tweemaal gedefinieerd.
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_f_extended_cg68k_not_supported=F_Extended cg68k niet ondersteund
% The var{extended} type is not supported on the m68k platform.
cg_f_32bit_not_supported_in_68000=F_32-bit unsigned niet ondersteund in MC68000 modus
% The cardinal is not supported on the m68k platform.
cg_f_internal_error_in_secondinline=F_Interne fout in secondinline()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide  an exact description of
% the circumstances in which the error occurs.
cg_d_register_weight=D_Register $1 gewicht $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_e_stacklimit_in_local_routine=E_Stacklimiet overschreden in lokale routine
% Your code requires a too big stack. Some operating systems pose limits
% on the stack size. You should use less variables or try ro put large
% variables on the heap.
cg_d_stackframe_omited=D_Stackframe wordt niet gemaakt
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=E_Kan inline niet gebruiken voor object methoden
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=E_Kan inline niet gebruiken voor procvar aanroepen
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=E_Geen code voor inline procedure opgeslagen
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=E_Element nul van een ansi/wide- of longstring is niet toegankelijk, gebruik (set)length.
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such kinf of string
cg_e_include_not_implemented=E_Include en exclude niet ondersteund in dit geval
% \var{include} and \var{exclude} are only partially
% implemented for \var{i386} processors
% and not at all for \var{m68k} processors.
cg_e_cannot_call_cons_dest_inside_with=E_Constructors of destructors kunnen niet aangeroepen worden binnen een 'with' clausule
% Inside a \var{With} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=E_Een message handler methode kan niet direct aangeroepen worden.
% A message method handler method can't be called directly if it contains an
% explicit self argument
% \end{description}
# EndOfTeX
#
# Assembler reader
#
asmr_d_start_reading=D_Begonnen met verwerken van $1 assembler
asmr_d_finish_reading=D_Gedaan met verwerken van $1 assembler
asmr_e_none_label_contain_at=E_Patroon dat geen label is bevat "@"
asmr_w_override_op_not_supported=W_De "override" operator wordt niet ondersteund
asmr_e_building_record_offset=E_Fout bij berekenen offset in record
asmr_e_offset_without_identifier=E_OFFSET gebruikt zonder symbool
asmr_e_no_local_or_para_allowed=E_Kan hier geen lokale variabele of parameter gebruiken
asmr_e_need_offset=E_OFFSET moet hier gebruikt worden
asmr_e_cant_have_multiple_relocatable_symbols=E_Kan niet meerdere verplaatsbare symbolen gebruiken
asmr_e_only_add_relocatable_symbol=E_Een verplaatsbaar symbool kan enkel toegevoegd worden
asmr_e_invalid_constant_expression=E_Ongeldige constante uitdrukking
asmr_e_relocatable_symbol_not_allowed=E_Verplaatsbaar symbool niet toegelaten
asmr_e_invalid_reference_syntax=E_Ongeldige geheugenlocatie schrijfwijze
asmr_e_local_symbol_not_allowed_as_ref=E_Lokale symbolen kunnen niet gebruikt worden als verwijzing
asmr_e_wrong_base_index=E_Ongeldig gebruik van basis en index register
asmr_e_wrong_scale_factor=E_Ongeldige schaalfactor
asmr_e_multiple_index=E_Meervoudig indexregister gebruik
asmr_e_invalid_operand_type=E_Ongeldig operandus type
asmr_e_invalid_string_as_opcode_operand=E_Ongeldige tekenreeks als instructie operandus: $1
asmr_w_CODE_and_DATA_not_supported=W_@CODE en @DATA worden niet ondersteund
asmr_e_null_label_ref_not_allowed=E_Null label verwijzingen zijn niet toegelaten
asmr_e_ev_zero_divide=F_Deling door nul in de assembler evaluator
asmr_e_ev_stack_overflow=F_Assembler evaluator stapeloverloop
asmr_e_ev_stack_underflow=F_Assembler evaluator stapelonderloop
asmr_e_ev_invalid_number=F_Ongeldig numeriek formaat in assembler evaluator
asmr_e_ev_invalid_op=F_Ongeldige operator in assembler evaluator
asmr_e_escape_seq_ignored=E_Expliciete reeks genegeerd: $1
asmr_e_invalid_symbol_ref=E_Ongeldige symboolverwijzing
asmr_w_fwait_emu_prob=W_Fwait kan emulatieproblemen met emu387 veroorzaken
asmr_w_calling_overload_func=W_Een overroepen functie wordt aangeroepen vanuit de assembler code
asmr_e_unsupported_symbol_type=E_Niet ondersteund symbool type voor operandus
asmr_e_constant_out_of_bounds=E_Constante waarde valt buiten bereik
asmr_e_error_converting_decimal=E_Fout bij omzetten van decimaal getal $1
asmr_e_error_converting_octal=E_Fout bij omzetten van octaal getal $1
asmr_e_error_converting_binary=E_Fout bij omzetten van binair getal $1
asmr_e_error_converting_hexadecimal=E_Fout bij omzetten van hexadecimaal getal $1
asmr_h_direct_global_to_mangled=H_$1 vertaald naar $2
asmr_w_direct_global_is_overloaded_func=W_$1 is geassocieerd met een overroepen functie
asmr_e_cannot_use_SELF_outside_a_method=E_Kan SELF niet buiten een methode gebruiken
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=E_Kan OLDEBP niet buiten een genestelde procedure gebruiken
asmr_e_void_function=W_Functies met een ongedefinieerde teruggeefwaarde kunnen geen waarde teruggeven
asmr_e_SEG_not_supported=E_SEG not supported
asmr_e_size_suffix_and_dest_dont_match=E_Grootte-achtervoegsel en doel- of brongrootte komen niet overeen
asmr_w_size_suffix_and_dest_dont_match=W_Grootte-achtervoegsel en doel- of brongrootte komen niet overeen
asmr_e_syntax_error=E_Assembler taalfout
asmr_e_invalid_opcode_and_operand=E_Ongeldige combinatie van instructie en operandi
asmr_e_syn_operand=E_Assemler taalfout in operandum
asmr_e_syn_constant=E_Assemler taalfout in constante
asmr_e_invalid_string_expression=E_Ongeldige String uitdrukking
asmr_w_const32bit_for_address=32 bits constante gemaakt voor adres
asmr_e_invalid_or_missing_opcode=E_Ongeldige of ontbrekende instructie
asmr_e_invalid_prefix_and_opcode=E_Ongeldige combinatie van voorvoegsel en instructie: $1
asmr_e_invalid_override_and_opcode=E_Ongeldige combinatie van segment overroeping en instructie: $1
asmr_e_too_many_operands=E_Te veel operandi op een lijn
asmr_w_near_ignored=W_NEAR genegeerd
asmr_w_far_ignored=W_FAR genegeerd
asmr_e_dup_local_sym=E_Dubbel lokaal symbool $1
asmr_e_unknown_local_sym=E_Niet gedefinieerd lokaal symbool $1
asmr_e_unknown_label_identifier=E_Onbekende labelnaam $1
asmr_e_invalid_fpu_register=E_Ongeldig vlottende-komma registernaam
asmr_e_nor_not_supported=E_NOR wordt niet ondersteund
asmr_w_modulo_not_supported=W_Modulo wordt niet ondersteund
asmr_e_invalid_float_const=E_Ongeldige vlottende-komma constante $1
asmr_e_invalid_float_expr=E_Ongeldige vlottende-komma uitdrukking
asmr_e_wrong_sym_type=E_Verkeerd symbool type
asmr_e_cannot_index_relative_var=E_Een lokale variabele of parameter kan niet geindedxeerd worden met een register
asmr_e_invalid_seg_override=E_Ongeldige segment overroeping
asmr_w_id_supposed_external=W_Symbool $1 wordt verondersteld extern te zijn
asmr_e_string_not_allowed_as_const=E_Strings zijn niet toegelaten als constante
asmr_e_no_var_type_specified=Geen type van de variabele gegeven
asmr_w_assembler_code_not_returned_to_text=E_De assembler code stopt niet in een '.text' gedeelte
asmr_e_not_directive_or_local_symbol=E_Geen aanwijzing of lokaal symbool $1
asmr_w_using_defined_as_local=E_Een gedfinieerde naam wordt gebruikt als assembler label

#
# Assembler/binary writers
#
asmw_f_too_many_asm_files=F_Te veel assembler bestanden
asmw_f_assembler_output_not_supported=F_De geselecteerde assembler modus wordt hier niet ondersteund
asmw_f_comp_not_supported=F_Het COMP type wordt hier niet ondersteund
asmw_f_direct_not_supported=F_Directe assembler modus wordt niet ondersteund door de binaire assemblerschrijver
asmw_e_alloc_data_only_in_bss=E_Ruimte voor gegevens reserveren mag enkel in een bss gedeelte
asmw_f_no_binary_writer_selected=F_Binaire assemblerschrijver geselecteerd
asmw_e_opcode_not_in_table=E_Asm: Opcode $1 not gevonden in interne tabel
asmw_e_invalid_opcode_and_operands=E_Asm: $1 Ongeldige combinatie van instructie en operandi
asmw_e_16bit_not_supported=E_Asm: 16 Bit geheugenlocaties worden niet ondersteund
asmw_e_invalid_effective_address=E_Asm: Ongeldig direct adres
asmw_e_immediate_or_reference_expected=E_Asm: Constante of geheugenadres verwacht
asmw_e_value_exceeds_bounds=E_Asm: $1 value exceeds bounds $2
asmw_e_short_jmp_out_of_range=E_Asm: Korte spronginstructie gaat te ver $1

#
# Executing linker/assembler
#
exec_w_source_os_redefined=W_Bronbesturingssysteem geherdefinieerd
exec_i_assembling_pipe=I_Assembleren (pijp) van $1
exec_d_cant_create_asmfile=E_Kan geen assembler bestand $1 niet maken
exec_w_assembler_not_found=W_Assembler $1 niet gevonden, overschakeling naar externe assemblage
exec_t_using_assembler=T_Gebruikte assembler: $1
exec_w_error_while_assembling=W_Fout bij assembleren
exec_w_cant_call_assembler=W_Kan assembleerder niet oproepen, overschakeling naar externe assemblage
exec_i_assembling=I_Assembleren van $1
exec_w_linker_not_found=W_Linker $1 niet gevonden, oveschakeling naar externe linker
exec_t_using_linker=T_Gebruikte linker: $1
exec_w_objfile_not_found=E_Bestand $1 niet gevonden, linken kan foutlopen !
exec_w_libfile_not_found=E_Bibliotheek $1 niet gevonden, linken kan foutlopen !
exec_w_error_while_linking=W_Fout tijdens linken
exec_w_cant_call_linker=W_Kan linker niet oproepen, overschakeling naar extern linken
exec_i_linking=I_Linken van $1
exec_w_binder_not_found=W_Linker niet gevonden, overschakeling naar extern binden.
exec_w_ar_not_found=W_ar niet gevonden, overschakeling naar externe ar
exec_e_dll_not_supported=E_Dynamische bibliotheken niet ondersteund
exec_i_closing_script=I_Afsluiten batch $1
exec_w_res_not_found=W_resource compiler niet gevonden, ga over op externe modus
exec_i_compilingresource=I_Compileer resource $1
#
# Executable information
#
execinfo_f_cant_process_executable=F_Kan applicatie niet nabehandelen $1
execinfo_f_cant_open_executable=F_Kan applicatie niet openen $1
execinfo_x_codesize=X_Grootte van de code: $1 bytes
execinfo_x_initdatasize=X_Grootte van geinitialiseerde data: $1 bytes
execinfo_x_uninitdatasize=X_Grootte van niet-geinitialiseerde data: $1 bytes
execinfo_x_stackreserve=X_Gereserveerde Stack ruimte: $1 bytes
execinfo_x_stackcommit=X_Toegewezen Stack ruimte: $1 bytes
# Unit loading
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=T_Unit zoeken: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=U_PPU Laden $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=U_PPU Naam: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=D_PPU Vlaggen: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=D_PPU CRC: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=D_PPU Tijd: $1
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_file_too_short=D_PPU bestand te kort
% When you use the \var{-vu} flag, the unit time is shown.
unit_u_ppu_invalid_header=D_PPU Ongeldige kop (geen PPU aan het begin)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=D_PPU Ongeldige versie $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=U_PPU is gecompileerd voor een andere processor
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=U_PPU is gecompileerd voor een ander doelsysteem
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_source=U_PPU bron: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_write=U_Schrijven van $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=F_Kan PPU bestand niet schrijven
% An err
unit_f_ppu_read_error=F_Lezen van PPU bestand
% Unexpected end of file
unit_f_ppu_read_unexpected_end=F_Onverwacht einde van PPU-bestand
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_invalid_entry=F_Ongeldig PPU bestand post: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=F_PPU Dbx aantal probleem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=E_Ongeldige unitnaam: $1
% The name of the unit doesn't match the file name.
unit_f_too_much_units=F_Te veel units
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{files.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=F_Uses statement veroorzaakt vicieuze cirkel
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=F_Kan unit $1 niet compileren, geen broncode beschikbaar
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_w_switch_us_missed=W_De systeemunit compileren vereist de -Us schakelaar
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=F_Er waren fouten $1 bij het compileren van een module, compilatie gestopt.
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=U_Laden van $1 ($2) unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=U_Hercompileer $1, checksum voor $2 veranderd
unit_u_recompile_source_found_alone=U_herkompileer $1, alleen bron gevonden
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=U_Hercompileer unit, statische bibliotheek ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=U_Hercompileer unit, dynamische bibliotheek ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=U_Hercompileer unit, object en asm zijn ouder dan ppu-bestand
% When you use the \var{-vu} flag, the compiler warns if the assembler of
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=U_Hercompileer unit, object is ouder dan assembler
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_start_parse_interface=U_Parsen van interface van $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_start_parse_implementation=U_Parsen van implementatie van $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=U_Tweede maal unit $1 laden
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=U_PPU Controleren van bestand $1 tijd $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
% \end{description}
# EndOfTeX
#
#  Options
#
option_usage=$1 [opties] <bestand> [opties]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=Maar een (1) bronbestand wordt ondersteunt
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=Een DEF file kan alleen worden gemaakt voor OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=Geneste optie bestanden niet ondersteund
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=Geen bronbestand op de commando-lijn
% The compiler expects a source file name on the command line.
option_illegal_para=Ongeldige parameter: $1
% You specified an unknown option.
option_help_pages_para=-? druk hulp af
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=Te veel geneste instellingsbestanden
% You can only nest up to 16 config files.
option_unable_open_file=Kan bestand $1 niet openen
% The option file cannot be found.
option_reading_further_from=Meer instellingen worden gelezen van $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=Doel is reeds ingesteld op: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=Shared bibliotheken niet onderteund onder DOS, val terug op statisch
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=Te veel IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=Te veel ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=Open voorwaardelijke aan eind van bestand
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=Debug informatie generatie is niet ondersteund door deze applicatie
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=Hercompileer de compiler-applicatie met -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=W_U gebruikt een in onbruik geraakte schakeloptie $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=W_U gebruikt een in onbruik geraakte schakeloptie $1, gebruik $2 a.u.b.
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=N_De assembler wordt veranderd in de standaard assembler schrijver
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_logo_start=Free Pascal Compiler versie $FPCVER [$FPCDATE] voor $FPCTARGET
option_logo_end=Copyright (c) 1998-2000 door Florian Klaempfl en anderen
#
# Info (option -i)
#
option_info_start=Free Pascal Compiler versie $FPCVER
option_info1=<lf>
option_info2=Compiler datum:       $FPCDATE
option_info3=Compiler doelsysteem: $FPCTARGET
option_info4=<lf>
option_info5=Dit programma wordt verspreid onder de GNU General Public Licence
option_info6=Voor meer informatie, lees COPYING.FPC. Hiervan is helaas geen
option_info7=Nederlandse vertaling beschikbaar.
option_info8=<lf>
option_info9=Meld bugs,suggesties, enz. aan:
option_info_end=                fpc-devel@mail.tolna.hungary.net
#
# Help pages (option -? and -h)
#
# note: the ordering of the enum numbering doens't matter
#
ol_begin=**0*_+ schakelt optie aan, - af
ol001=**1a_De compiler verwijdert gegenereerd assembler bestand niet
ol002=*t1b_Gebruik EMS
ol003=**1B_Bouw alle modules
ol004=**1C_Code generatie opties
ol005=3*2CD_Creeer dynamische bibliotheek
ol006=**2Ch<n>_<n> bytes heap (tussen 1023 en 67107840)
ol007=**2Ci_IO-checking
ol008=**2Cn_Laat linken achterwege
ol009=**2Co_Controleer overflow van integer operaties
ol010=**2Cr_Bereik controle
ol011=**2Cs<n>_Stel stack grootte in op <n>
ol012=**2Ct_Stack controle
ol013=3*2CS_Cre‰er dynamische bibliotheek
ol014=3*2Cx_Gebruik slim linken
ol015=**1d<x>_definieer het symbool <x>
ol016=*O1D_genereer een DEF bestand
ol017=*O2Dd<x>_Zet beschrijving op <x>
ol018=*O2Dw_PM applicatie
ol019=**1e<x>_Zet pad naar applicaties
ol020=**1E_Zelfde als -Cn
ol021=**1F_Zet bestandsnamen en paden
ol022=**2Fe<x>_Zend foutboodschappen naar bestand <x>
ol023=*L2Fg<x>_Zelfde als -Fl
ol024=**2Fi<x>_Voegt <x> toe aan invoegpad
ol025=**2Fl<x>_Voegt <x> toe aan bibliotheek pad
ol026=*L2FL<x>_Gebruik <x> als dynamische linker
ol027=**2Fo<x>_Voeg <x> toe aan object pad
ol028=**2Fr<x>_Gebruik foutboodschappen in bestand <x>
ol029=**2Fu<x>_Voeg <x> toe aan unit pad
ol030=*g1g_genereer debug informatie
ol031=*g2gg_gebruik gsym
ol032=*g2gd_gebruik dbx
ol033=*g2gh_laad heaptrc unit automatisch
ol034=*g2gc_genereer meer controlers voor wijzers
ol035=**1i_informatie
ol036=**1I<x>_Voeg <x> toe aan invoegpad
ol037=**1k<x>_Geef <x> door aan de linker
ol038=**1l_Druk logo af
ol039=**1n_Standaard configuratie bestand niet lezen
ol040=**1o<x>_Stel de naam van het applicatiebestand in op <x>
ol041=**1pg_genereer profile code voor gprof
ol042=*L1P_Gebruik pipes in plaats van tijdelijke assembler bestanden
ol043=**1S_Syntax instellingen
ol044=**2S2_Stel Delphi 2 uitbreidingen in
ol045=**2Sc_Ondersteun operatoren als in C (*=,+=,/= en -=)
ol046=**2Sd_Compiler verwijdert assembler lijsten (minder geheugengebruik, maar trager)
ol047=**2Se_Compiler stopt na de eerste fout
ol048=**2Sg_Laat LABEL en GOTO toe
ol049=**2Si_Ondersteun C++ stijl INLINE
ol050=**2Sm_Ondersteun macros zoals in C (globaal)
ol051=**2So_Probeer TP/BP 7.0 compatibel te zijn
ol052=**2Sp_Probeer to be gpc compatibel te zijn
ol053=**2Ss_Constructor naam moet init zijn (destructor moet done zijn)
ol054=**2St_Sta static sleutelwoord toe in objecten
ol055=**1s_Roep assembler en linker niet op (slechts met -a)
ol056=**1T<x>_Doel besturingssysteem
ol057=3*2TGO32V1_versie 1 van de DJ Delorie DOS uitbreider
ol058=3*2TGO32V2_versie 2 van de DJ Delorie DOS uitbreider
ol059=3*2TLINUX_Linux
ol060=3*2TOS2_OS/2 (EMX)
ol061=3*2TWin32_Windows 32 Bit
ol062=6*2TAMIGA_Commodore Amiga
ol063=6*2TATARI_Atari ST/STe/TT
ol064=6*2TMACOS_Macintosh m68k
ol065=6*2TLINUX_Linux-68k
ol066=**1u<x>_Verwijdert symbooldefinitie <x>
ol067=**1U_unit opties
ol068=**2Un_Unit naam niet nagaan
ol069=**2Up<x>_Zelfde als -Fu<x>
ol070=**2Us_Compileer een systeemunit
ol071=**1v<x>_Wees uitvoerig. <x> is een combinatie van volgende letters:
ol072=**2*_e : Toon fouten (standaard)     d : Toon debug informatie
ol073=**2*_w : Toon waarschuwingen         u : Toon gebruikte bestanden
ol074=**2*_n : Toon notas                  t : Toon geteste bestanden
ol075=**2*_h : Toon hints                  m : Toon gedefinieerde macros
ol076=**2*_i : Toon algemene informatie    p : Toon gecompileerde routines
ol077=**2*_l : Toon regelnummers           c : Toon voorwaardelijken
ol078=**2*_a : Toon alles                  0 : Toon alleen fouten
ol079=**1X_applicatie instellingen
ol080=*L2Xc_link met de C bibliotheek
ol081=**2XD_link met dynamische bibliotheken (definieert FPC_LINK_DYNAMIC)
ol082=**2Xs_verwijder alle symbolen uit applicatie
ol083=**2XS_link met statische bibliotheken (definieert FPC_LINK_STATIC)
ol084=**0*_Processor specifieke instellingen:
ol085=3*1A_Output formaat
ol086=3*2Aas_assembleer met GNU AS
ol087=3*2Aasaout_assembleer met GNU AS voor aout (Go32v1)
ol088=3*2Anasmcoff_Coff (Go32v2) bestand met Nasm
ol089=3*2Anasmelf_elf32 (linux) bestand met Nasm
ol090=3*2Anasmobj_obj bestand met Nasm
ol091=3*2Amasm_obj bestand met Masm (Microsoft)
ol092=3*2Atasm_obj bestand met Tasm (Borland)
ol093=3*2Acoff_coff (Go32v2) met interne assembler
ol094=3*2Apecoff_pecoff (Win32) met interne assembler
ol095=3*1R_assembler lezer stijl:
ol096=3*2Ratt_Lees AT&T stijl assembler
ol097=3*2Rintel_Lees Intel stijl assembler
ol098=3*2Rdirect_Kopieer assembler dadelijk naar assembler bestand
ol099=3*1O<x>_optimalizaties
ol100=3*2Og_Genereer kleinere code
ol101=3*2OG_Genereerd snellere code (standaard)
ol102=3*2Or_Houd zekere variabelen in registers (Nog steeds BUGGY!!!)
ol103=3*2Ou_Probeer onzekere optimalizaties (zie documentatie)
ol104=3*2O1_Niveau 1 optimalizaties (snelle optimizaties)
ol105=3*2O2_Niveau 2 optimalizaties (-O1 + tragere optimizaties)
ol106=3*2O3_Niveau 3 optimalizaties (Zelfde als -O2u)
ol107=3*2Op_Doel processor
ol108=3*3Op1_Stel doel processor in op 386/486
ol109=3*3Op2_Stel doel processor in op Pentium/PentiumMMX (tm)
ol110=3*3Op3_Stel doel processor in op PPro/PII/c6x86/K6 (tm)
ol111=6*1A_output formaat
ol112=6*2Aas_Unix o-bestand met GNU AS
ol113=6*2Agas_GNU Motorola assembler
ol114=6*2Amit_MIT Syntax (oude GAS)
ol115=6*2Amot_Standaard Motorola assembler
ol116=6*1O_optimizaties
ol117=6*2Oa_Gebruik de optimizaties
ol118=6*2Og_Genereer kleinere code
ol119=6*2OG_Genereer snellere code (standaard)
ol120=6*2Ox_Optimizeer maximaal (nog steeds BUGGY!!!)
ol121=6*2O2_Stel doelprocessor in op MC68020+
ol122=**1*_
ol123=**1?_Toont deze hulp
ol124=6*2Agas_GNU Motorola assembler
ol125=6*2Amit_MIT Syntax (old GAS)
ol126=6*2Amot_Standard Motorola assembler
ol127=6*1O_optimizaties:
ol128=6*2Oa_Geruik optimizatie
ol129=6*2Og_Maak kleinere
ol130=6*2OG_Maak snellere code (standaard)
ol131=6*2Ox_Optimaliseer maximaal (nog altijd BUGGY!!!)
ol132=6*2O2_Stel doelprocessor in op MC68020+
ol133=6*1R<x>_assembler lezer stijl:
ol134=6*2RMOT_Lees motorola stijl assembler
ol135=6*1T<x>_Doel besturingssysteem:
ol136=6*2TAMIGA_Commodore Amiga
ol137=6*2TATARI_Atari ST/STe/TT
ol138=6*2TMACOS_Macintosh m68k
ol139=6*2TLINUX_Linux-68k
ol140=**1*_
ol141=**1?_Toont deze hulp
ol_end=**1h_Toont deze hulp zonder wachten
#
# The End...
#
