#
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2008 by the Free Pascal Development team
#
#   French (ISO 8859-1) Language File for Free Pascal
#   Latest updates contributed by Rémi Dorlet <remi at dorlet.org>
#   Based on errore.msg of SVN revision 4165
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
general_text_bytes_code=01019_Code de type octet
general_text_bytes_data=01020_Donnée de type octet
# BeginOfTeX
% \section{General compiler messages}
% This section gives the compiler messages which are not fatal, but which
% display useful information. The number of such messages can be
% controlled with the various verbosity level \var{-v} switches.
% \begin{description}
general_t_compilername=01000_T_Compilateur: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.
general_d_sourceos=01001_D_Système d'exploitation du compilateur: $1
% When the \var{-vd} switch is used, this line tells you what the source
% operating system is.
general_i_targetos=01002_I_Système d'exploitation cible: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Répertoire des fichiers exécutables: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its binaries.
general_t_unitpath=01004_T_Répertoire de recherche des unités: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Répertoire de recherche des fichiers inclus: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Répertoire de recherche des bibliothèques: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Répertoire de recherche des fichiers objet: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 lignes compilées, $2 sec$3
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_Plus de mémoire disponible
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
general_i_writingresourcefile=01010_I_Écriture du fichier de ressource: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Écriture du fichier de chaînes de ressource: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
general_i_fatal=01012_I_Fatal:
% Prefix for Fatal Errors
general_i_error=01013_I_Erreur:
% Prefix for Errors
general_i_warning=01014_I_Attention:
% Prefix for Warnings
general_i_note=01015_I_Note:
% Prefix for Notes
general_i_hint=01016_I_Conseil:
% Prefix for Hints
general_e_path_does_not_exist=01017_E_Le chemin "$1" n'existe pas
% The specified path does not exist.
general_f_compilation_aborted=01018_F_Compilation interrompue
% Compilation was aborted.
general_i_number_of_warnings=01021_I_$1 avertissement(s) émis
% Total number of warnings issued during compilation.
general_i_number_of_hints=01022_I_$1 conseil(s) émis
% Total number of hints issued during compilation.
general_i_number_of_notes=01023_I_$1 note(s) émises
% Total number of notes issued during compilation.
% \end{description}
#
# Scanner
#
# 02063 is the last used one
#
% \section{Scanner messages.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=02000_F_Fin de fichier inattendue
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment was not closed
% \end{itemize}
scan_f_string_exceeds_line=02001_F_Chaîne de caractères sur plus d'une ligne
% There is a missing closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_Caractère "$1" illégal ($2)
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Erreur de syntaxe, "$1" attendu mais "$2" trouvé
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_T_Début de lecture du fichier include $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_Commentaire de niveau $1 trouvé
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_ignored_switch=02008_N_Commutateur "$1" ignoré
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Commutateur "$1" illégal
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% does not recognise
scan_w_switch_is_global=02010_W_Commutateur global mal placé
% The compiler switch is misplaced, and should be located at
% the start of the unit or program.
scan_e_illegal_char_const=02011_E_Constante de type char illégale
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range.
scan_f_cannot_open_input=02012_F_Impossible d'ouvrir le fichier "$1"
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_Impossible d'ouvrir le fichier include "$1"
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_illegal_pack_records=02015_E_Paramètre d'alignement de record incorrect "$1"
% You are specifying the \var{\{\$PACKRECORDS n\} } or \var{\{\$ALIGN n\} }
% with an illegal value for \var{n}. For \$PACKRECORDS valid alignments are 1, 2, 4, 8, 16, 32, C,
% NORMAL, DEFAULT, and for \$ALIGN valid alignment are 1, 2, 4, 8, 16, 32, ON,
% OFF. Under mode MacPas \$ALIGN also supports MAC68K, POWER and RESET.
scan_e_illegal_pack_enum=02016_E_Paramètre de taille minimum d'énumération incorrect "$1"
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2,4, NORMAL or DEFAULT are valid here.
scan_e_endif_expected=02017_E_$ENDIF attendu pour $1 $2 défini dans $3 à la ligne $4
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Erreur de syntaxe pendant l'interprétation d'une directive de compilation
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_e_error_in_preproc_expr=02019_E_Évaluation d'une directive de compilation
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_w_macro_cut_after_255_chars=02020_W_Le contenu d'une macro est tronqué à 255 caractères
% The contents of macros cannot be longer than 255 characters.
scan_e_endif_without_if=02021_E_ENDIF sans IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_Défini par l'utilisateur : $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_Défini par l'utilisateur : $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_Défini par l'utilisateur : $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_Défini par l'utilisateur : $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_Défini par l'utilisateur : $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_Défini par l'utilisateur : $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_La redéfinition d'un mot réservé en tant que macro est dans effet
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_Débordement de tampon lors de la lecture ou de l'expansion d'une macro
% Your macro or its result was too long for the compiler.
scan_w_macro_too_deep=02030_W_L'expansion des macros dépasse une profondeur de 16.
% When expanding a macro, macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_w_wrong_styled_switch=02031_W_Les commutateurs du compilateur ne sont pas supportés dans les commentaires de style // 
% Compiler switches should be in normal pascal style comments.
scan_d_handling_switch=02032_D_Interprétation du commutateur "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_CL_ENDIF $1 trouvé
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_CL_IFDEF $1 trouvé, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_CL_IFOPT $1 trouvé, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_CL_IF $1 trouvé, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_CL_IFNDEF $1 trouvé, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_CL_ELSE $1 trouvé, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_CL_Passant jusqu'à...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_Appuyez sur <Entrée> pour continuer
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Commutateur "$1" non supporté
% When warnings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_Directive de compilation "$1" illégale
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_T_De retour dans $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Type d'application non supportée : $1
% You get this warning, if you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_$APPTYPE n'est pas supporté par l'OS cible
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_w_description_not_support=02046_W_DESCRIPTION n'est pas supporté par l'OS cible
% The \var{\{\$DESCRIPTION\}} directive is not supported on this target OS
scan_n_version_not_support=02047_N_VERSION non supporté par l'OS cible
% The \var{\{\$VERSION\}} directive is not supported on this target OS
scan_n_only_exe_version=02048_N_VERSION seulement pour les EXE et les DLL
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_Mauvais format de la directive VERSION "$1"
% The \var{\{\$VERSION\}} directive format is majorversion.minorversion
% where majorversion and minorversion are words.
scan_e_illegal_asmmode_specifier=02050_E_Style d'assembleur incorrect "$1"
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_Changement de type d'interpréteur ASM à l'intérieur de code assembleur, $1 sera effectif seulement pour le prochain
% It is not possible to switch from one assembler reader to another
% inside an assembler block. The new reader will be used for next
% assembler statements only.
scan_e_wrong_switch_toggle=02052_E_Mauvais argument de commutateur, utilisez ON/OFF ou +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_Fichiers de ressources non supportés pour cette cible
% The target you are compiling for doesn't support resource files.
scan_w_include_env_not_found=02054_W_Variable d'environment "$1" non trouvée
% The included environment variable can't be found in the environment, it will
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Valeur invalide pour nombre limite de variables en registre FPU
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Un seul fichier de ressource est supporté pour cette cible
% The target you are compiling for supports only one resource file.
% The first resource file found is used, the others are discarded.
scan_w_macro_support_turned_off=02057_W_L'utilisation des macros à été désactivée
 	
% A macro declaration has been found, but macro support is currently off,
% so the declaration will be ignored. To turn macro support on compile with
% -Sm on the commandline or add \{\$MACRO ON\} in the source
scan_e_invalid_interface_type=02058_E_Type d'interface spécifié incorrect. Les valeurs acceptées sont COM, CORBA et DEFAULT.
% The interface type that was specified is not supported
scan_w_appid_not_support=02059_W_APPID n'est supporté que pour PalmOS
% The \var{\{\$APPID\}} directive is only supported for the PalmOS target.
scan_w_appname_not_support=02060_W_APPNAME n'est supporté que pour PalmOS
% The \var{\{\$APPNAME\}} directive is only supported for the PalmOS target.
scan_e_string_exceeds_255_chars=02061_E_Les constantes chaîne de caractères ne peuvent dépasser 255 caractères
% A single string constant can contain at most 255 chars. Try splitting up the
% string in multiple smaller parts and concatenate them with a + operator.
scan_f_include_deep_ten=02062_F_La profondeur des inclusions de fichiers include dépasse 16.
% When including include files the files have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_too_many_push=02063_F_Trop de niveaux de PUSH
% A maximum of 20 levels is allowed. This error occurs only in mode MacPas.
scan_e_too_many_pop=02064_E_Un POP non précédé d'un PUSH
% This error occurs only in mode MacPas.
scan_e_error_macro_lacks_value=02065_E_Macro ou variable d´heure de compilation "$1" sans valeur
% Thus the conditional compile time expression cannot be evaluated.
scan_e_wrong_switch_toggle_default=02066_E_Mauvais argument de commutateur, utilisez ON/OFF/DEFAULT ou +/-/*
% You need to use ON or OFF or DEFAULT or a + or - or * to toggle the switch
scan_e_mode_switch_not_allowed=02067_E_Le commutateur de mode "$1" n'est pas autorisé ici
% A mode switch has already been encountered, or, in case of option -Mmacpas,
% a mode switch occur after UNIT.
scan_e_error_macro_undefined=02068_E_Macro ou variable d´heure de compilation "$1" non définie
% Thus the conditional compile time expression cannot be evaluated. Only in mode MacPas.
scan_e_utf8_bigger_than_65535=02069_E_Code UTF-8 supérieur à 65535 trouvé
% \fpc handles utf-8 strings internally as widestrings e.g. the char codes are limited to 65535
scan_e_utf8_malformed=02070_E_Chaîne UTF-8 malformée
% The given string isn't a valid UTF-8 string
scan_c_switching_to_utf8=02071_C_Signature UTF-8 trouvée, utilisation de l'encodage UTF-8
% The compiler found an UTF-8 encoding signature (\$ef, \$bb, \$bf) at the beginning of a file,
% so it interprets it as an UTF-8 file
scan_e_compile_time_typeerror=02072_E_Heure de compilation: $1 attendue mais $2 à $3 obtenue
% Type check of a compile time expression failed.
scan_n_app_type_not_support=02073_N_APPTYPE n'est pas supporté par l'OS cible
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_e_illegal_optimization_specifier=02074_E_Optimisation "$1" spécifiée incorrecte
% When you specify an optimization with the \var{\{\$OPTIMIZATION xxx\}}
% the compiler didn't recognize the optimization you specified.
scan_w_setpeflags_not_support=02075_W_SETPEFLAGS n'est pas supporté par l'OS cible
% The \var{\{\$SETPEFLAGS\}} directive is not supported by the target OS
scan_w_imagebase_not_support=02076_W_IMAGEBASE n'est pas supporté par l'OS cible
% The \var{\{\$IMAGEBASE\}} directive is not supported by the target OS
scan_w_minstacksize_not_support=02077_W_MINSTACKSIZE n'est pas supporté sur l'OS cible
% The \var{\{\$MINSTACKSIZE\}} directive is not supported by the target OS
scan_w_maxstacksize_not_support=02078_W_MAXSTACKSIZE n'est pas supporté sur l'OS cible
% The \var{\{\$MAXSTACKSIZE\}} directive is not supported by the target OS
% \end{description}
#
# Parser
#
# 03192 is the last used one
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=03000_E_Analyseur syntaxique - Erreur de syntaxe
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_e_dont_nest_interrupt=03004_E_Une procédure de type INTERRUPT ne peut pas être imbriquée
% An \var{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Type procédure "$1" ignoré
% The specified procedure directive is ignored by FPC programs.
parser_e_no_overload_for_all_procs=03006_E_Toutes les déclaration de "$1" ne sont pas déclarées OVERLOAD
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_export_name_double=03008_E_Nom de fonction exportée "$1" dupliqué
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Index de fonction exportée "$1" dupliqué
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_Index de fonction exportée invalide
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_Les DLL ou EXE relogeables sont incompatibles avec les informations de débogage, débogage désactivé.
parser_w_parser_win32_debug_needs_WN=03012_W_Pour permettre le débogage de code win32, utilisez l'option -WN
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_Le nom du constructeur doit être INIT
% You are declaring an object constructor with a name which is not \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_Le nom du destructeur doit être DONE
% You are declaring an object destructor with a name which is not \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_proc_inline_not_supported=03016_E_Procédure INLINE non supportée
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_constructor_should_be_public=03018_W_Le constructeur devrait être public
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_Le destructeur devrait être public
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Les classes ne doivent avoir qu'un seul destructeur
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Les définitions de classes locales ne sont pas autorisées
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Les définitions de classes anonymes sont interdites
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_n_object_has_no_vmt=03023_N_L'objet "$1" n'a pas de VMT
% This is a note indicating that the declared object has no
% virtual method table.
parser_e_illegal_parameter_list=03024_E_Liste de paramètres illégale
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_size=03026_E_Mauvais nombre de paramètres spécifié
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_L'identificateur surchargé "$1" n'est pas une fonction
% The compiler encountered a symbol with the same name as an overloaded
% function, but it is not a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_Les fonctions surchargées ont les mêmes paramètres
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_L'en-tête de la fonction ne correspond pas à la déclaration anticipée "$1"
% You declared a function with same parameters but
% different result type or function modifiers.
parser_e_header_different_var_names=03030_E_L'entête de la fonction "$1" ne correspond pas à sa déclaration anticipée : le nom de variable change $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_Les valeurs d'une énumération doivent être en ordre croissant
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_e_no_with_for_variable_in_other_segments=03033_E_With ne peut pas être utilisé avec les variables d'un autre segment
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_Niveau d'inbrication de fonctions > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_Erreur d'intervalle dans l'évaluation d'une constante
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_Erreur d'intervalle dans l'évaluation d'une constante
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_Valeur dupliquée dans une instruction CASE
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_Valeur max. inférieure à la valeur min. dans l'instruction CASE
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_Les constantes typées de classes sont interdites
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_Les variables fonction de fonctions surchargées ne sont pas autorisées
% You are trying to assign an overloaded function to a procedural variable.
% This is not allowed
parser_e_invalid_string_size=03041_E_La longueur d'une chaîne doit être comprise entre 1 et 255
% The length of a shortstring in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
parser_w_use_extended_syntax_for_objects=03042_W_Utilisez la syntaxe étendue de NEW et DISPOSE pour les instances d'objet
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the object
parser_w_no_new_dispose_on_void_pointers=03043_W_L'usage de NEW ou DISPOSE pour des pointeurs non typés n'a pas de sens
parser_e_no_new_dispose_on_void_pointers=03044_E_L'usage de NEW ou DISPOSE pour des pointeurs non typés est impossible
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_Identificateur de classe attendu
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_Identificateur de type interdit ici
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_Identificateur de méthode attendu
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_L'en-tête de la fonction ne correspond à aucune méthode de cette classe "$1"
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_d_procedure_start=03049_DL_procédure/fonction $1
% When using the \var{-vd} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_Constante réelle illégale
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL ne peut être utilisé que dans un constructeur
% You are using the \var{fail} keyword outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Les destructeurs ne peuvent pas avoir de paramètres
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Seules des références de classe peuvent faire référence à des méthodes de classe
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
parser_e_only_class_methods=03054_E_Seules des méthodes de classe peuvent être appelées dans des méthodes de classe
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
parser_e_case_mismatch=03055_E_La constante et l'instruction CASE ne sont pas du même type
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_Le symbole ne peut pas être exporté depuis une bibliothèque
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_Une méthode héritée est cachée par "$1"
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_Il n'y a pas de méthode dans la classe ancêtre que l'on peut surcharger : "$1"
% You are trying to \var{override} a virtual method of a parent class that does
% not exist.
parser_e_no_procedure_to_access_property=03059_E_Aucun membre n'est fourni pour accéder à la propriété
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_Directive de propriété stockée non encore implementée
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_Symbole illégal pour l'accès à la propriété
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code would cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_Impossible d'accéder aux champs "protected" d'un object ici
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module where the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_Impossible d'accéder aux champs "private" d'un object ici
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_e_overridden_methods_not_same_ret=03066_E_Les méthodes surchargées doivent renvoyer le même type: "$2" est surchargée par "$1" qui retourne un type différent
% If you declare overridden methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_Les fonctions déclarées comme EXPORT ne peuvent pas être imbriquées
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_Les méthodes ne peuvent pas être EXPORTées
% You cannot declare a procedure that is a method for an object as
% \var{export}ed.
parser_e_call_by_ref_without_typeconv=03069_E_Les paramètres passés par adresse doivent être du même type que la déclaration: "$1" spécifié mais "$2" attendu
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_Cette classe n'est pas un parent de la classe actuelle
% When calling inherited methods, you are trying to call a method of a non-related
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_Self n'est possible que dans les méthodes
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_Une méthode ne peut être appelée avec son identificateur de classe qu'à partir d'une méthode
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=03073_E_Utilisation non valide de ':'
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_Erreur d'intervalle dans un constructeur d'ensemble ou élément d'ensemble dupliqué
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Pointeur sur un objet attendu
% You specified an illegal type in a \var{new} statement.
% The extended syntax of \var{new} needs an object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_L'expression doit être un appel à un constructeur
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_L'expression doit être un appel à un destructeur
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_Mauvais ordre des éléments d'un record
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_L'expression doit être une classe ou un record
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Les procédures ne peuvent pas retourner une valeur
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures cannot return a value.
parser_e_constructors_always_objects=03081_E_Les constructeurs et les destructeurs doivent être des méthodes
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_L'opérateur n'est pas surchargé
% You're trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Impossible de surcharger l'affectation de types équivalents
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Surcharge d'opérateur impossible
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_Redéclenchement d'exception impossible ici
% You are trying to raise an exception where it is not allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_La syntaxe étendue de new ou dispose n'est pas autorisée pour une classe
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_procedure_overloading_is_off=03088_E_La surcharge de procédures est désactivée
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_Impossible de surcharger cet opérateur
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Un opératuer de comparaison doit retourner une valeur booléenne
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Seules les méthodes virtuelles peuvent être abstraites
% You are declaring a method as abstract, when it is not declared to be
% virtual.
parser_f_unsupported_feature=03092_F_Utilisation d'une caractéristique non supportée !
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_Le mélange de différents types d'objets (classe, objet, interface, etc.) est interdit
% You cannot derive \var{objects}, \var{classes}, \var{cppclasses} and \var{interfaces} interttwined . E.g.
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Une directive de procédure inconnue a due être ignorée : "$1"
% The procedure directive you specified is unknown.
parser_e_absolute_only_one_var=03095_E_absolute ne peut être associé qu'à une seule variable
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
parser_e_absolute_only_to_var_or_const=03096_E_absolute ne peut être associé qu'à une variable ou une constante
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Seule UNE variable peut être initialisée
% You cannot specify more than one variable with a initial value
% in Delphi mode.
parser_e_abstract_no_definition=03098_E_Les méthodes abstraites ne doivent pas avoir d'implementation
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Cette fonction surchargée ne peut pas être locale (elle doit être exportée)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_Des méthodes virtuelles sont utilisées sans constructeur dans "$1"
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_c_macro_defined=03101_CL_Macro définie: $1
% When \var{-vc} is used, the compiler tells you when it defines macros.
parser_c_macro_undefined=03102_CL_Macro non défine: $1
% When \var{-vc} is used, the compiler tells you when it undefines macros.
parser_c_macro_set_to=03103_CL_Macro $1 définie à $2
% When \var{-vc} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_Compilation de $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_U_Analyse syntaxique de l'interface de l'unité $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_U_Analyse syntaxique de l'implémentation de $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_DL_Compilation de $1 pour la seconde fois
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_property_found_to_override=03109_E_Aucune propriété à surcharger n'a été trouvée
% You want to override a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Une seule propriété par défaut est autorisée
% You specified a property as \var{Default}, but the class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_La propriété par défaut doit être de type tableau
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Les constructeurs virtuels ne sont possibles que pour les classes
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_Pas de propriété par défaut disponible
% You are trying to access a default property of a class, but this class (or one of
% its ancestors) doesn't have a default property.
parser_e_cant_have_published=03114_E_La classe ne peut pas avoir une section "published", utilisez le commutateur {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_La déclaration anticipée de la classe "$1" doit être résolue ici pour pouvoir utiliser la classe comme ancêtre
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Les opérateurs locaux ne sont pas supportés
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_La directive de procédure "$1" n'est pas autorisée dans la section interface
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_La directive de procédure "$1" n'est pas autorisée dans la section implementation
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_La directive de procédure "$1" n'est pas autorisée dans la déclaration
% This procedure directive cannot be part of a procedural or function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_La fonction "$1" est déjà déclarée comme Public ou Forward
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_Impossible d'utiliser à la fois EXPORT et EXTERNAL
% These two procedure directives are mutually exclusive
parser_w_not_supported_for_inline=03123_W_"$1" n'est pas encore supporté à l'intérieur de procédures/fonctions INLINE
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=03124_W_Inlining désactivé
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Ecriture du Browser log $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=03126_H_Le déréférencement du pointeur semble manquer
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_Lecteur de code assembleur sélectionné non supporté
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=03128_E_La directive de procédure "$1" entre en conflit avec d'autres directives
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_La convention d'appel ne correspond pas à la déclaration anticipée
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_property_cant_have_a_default_value=03131_E_Cette propriété ne peut pas avoir de valeur par défaut
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_La valeur par défaut d'une propriété doit être une constante
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_Ce symbole ne peut pas être "published", il ne peut être qu'une classe
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Ce genre de propriété ne peut pas être PUBLISHED
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_e_empty_import_name=03136_W_Un nom d'import est requis
% Some targets need a name for the imported procedure or a \var{cdecl} specifier
parser_e_division_by_zero=03138_E_Division par zéro
% There is a division by zero encounted
parser_e_invalid_float_operation=03139_E_Opération en virgule flottante invalide
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_Limite supérieure de l'intervalle plus petite que la limite inférieure
% The upper bound of a an array declaration is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_La chaîne "$1" est plus longue que $2
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_Longueur de chaîne supérieure à la longueur d'un tableau de caractères
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_Expression illégale après la directive Message
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Les gestionnaires de message ne peuvent accepter qu'un paramètre par référence
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Message dupliqué dans un objet ou une classe: "%1"
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_Self ne peut être qu'un paramètre explicite dans les gestionnaires de message
% The self parameter can only be passed explicitly to a method which
% is declared as message handler.
parser_e_threadvars_only_sg=03147_E_Les variables de Threads peuvent seulement être statiques ou globales
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has its own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Assembleur direct non supporté pour la sortie binaire
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=03149_W_Ne chargez pas l'unité OBJPAS manuellement, utilisez de préférence \{\$mode Objfpc\} ou \{\$mode Delphi\}
% You are trying to load the ObjPas unit manually from a uses clause. This is
% not a good idea. Use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automatically
parser_e_no_object_override=03150_E_OVERRIDE ne peut pas être utilisé avec des objets
% Override is not supported for objects, use \var{virtual} instead to override
% a method of a parent object
parser_e_cant_use_inittable_here=03151_E_Les types de données nécessitant une initialisation/finalisation ne peuvent pas être utilisés dans des records variants
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Les chaînes de ressource ne peuvent être que statiques ou globales
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_Exit avec un argument ne peut pas être utilisé ici
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_Le type du symbole STORED doit être un booléen
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_Ce symbole n'est pas admis comme symbole STORED
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=03156_E_Seules les classes compilées avec le mode $M+ peuvent être PUBLISHED
% In the published section of a class can be only class as fields used which
% are compiled in \var{\{\$M+\}} or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Directive de procédure attendue
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
parser_e_invalid_property_index_value=03158_E_La valeur d'un index de propriété doit être de type scalaire
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Nom de procédure trop court pour être exporté
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_Impossible de créer une entrée DEFFILE pour des variables globales d'unité
parser_e_dlltool_unit_var_problem2=03161_E_Compilez sans l'option -WD
% You need to compile this file without the -WD switch on the
% commandline
parser_f_need_objfpc_or_delphi_mode=03162_F_Vous devez utilisez le mode ObjFpc (-S2) ou Delphi (-Sd) pour compiler ce module
% You need to use \{\$mode objfpc\} or \{\$mode delphi\} to compile this file.
% Or use the equivalent commandline switches -S2 or -Sd.
parser_e_no_export_with_index_for_target=03163_E_Impossible d'exporter avec un index sous $1
% Exporting of functions or procedures with a specified index is not
% supported on this target.
parser_e_no_export_of_variables_for_target=03164_E_L'export de variables n'est pas supporté sous $1
% Exporting of variables is not supported on this target.
parser_e_improper_guid_syntax=03165_E_Syntaxe GUID incorrecte
% The GUID indication does not have the proper syntax. It should be of the form
% \begin{verbatim}
% {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}
% \end{verbatim}
% Where each \var{X} represents a hexadecimal digit.
parser_w_interface_mapping_notfound=03168_W_Impossible de trouver la procédure nommée "$1" nécessaire à l'implémentation de $2.$3
% The compiler cannot find a suitable procedure which implements the given method of an interface.
% A procedure with the same name is found, but the arguments do not match.
parser_e_interface_id_expected=03169_E_Identificateur d'interface attendu
% This happens when the compiler scans a \var{class} declaration that contains
% \var{interface} function name mapping code like this:
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% and the \var{interface} before the dot not listed in the inheritance list.
parser_e_type_cant_be_used_in_array_index=03170_E_Le type "$1" ne peut être utilisé comme indice de tableau
% Types like \var{qword} or \var{int64} aren't allowed as array index type
parser_e_no_con_des_in_interfaces=03171_E_Constructeurs et destructeurs ne sont pas autorisés dans les interfaces
% Constructor and destructor declarations aren't allowed in interface
% In the most cases the method \var{QueryInterface} of \var{IUnknown} can
% be used to create a new interface.
parser_e_no_access_specifier_in_interfaces=03172_E_Les descripteurs de portée ne peuvent pas être utilisés dans les INTERFACES
% The access specifiers \var{public}, \var{private}, \var{protected} and
% \var{pusblished} can't be used in interfaces because all methods
% of an interfaces must be public.
parser_e_no_vars_in_interfaces=03173_E_Une interface ne peut pas contenir de propriétés
% Declarations of fields aren't allowed in interfaces. An interface
% can contain only methods
parser_e_no_local_proc_external=03174_E_Impossible de déclarer une procédure locale comme EXTERNAL
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high
parser_w_skipped_fields_before=03175_W_Certaines propriétés avant "$1" n'ont pas été initialisées
% In Delphi mode, not all fields of a typed constant record have to be
% initialized, but the compiler warns you when it detects such situations.
parser_e_skipped_fields_before=03176_E_Certaines propriétés situées avant "$1" n'ont pas été initialisées
% In all syntax modes but Delphi mode, you can't leave some fields uninitialized
% in the middle of a typed constant record
parser_w_skipped_fields_after=03177_W_Certaines propriétés après "$1" n'ont pas été initialisées
% You can leave some fields at the end of a type constant record uninitialized
% (the compiler will initialize them to zero automatically). This may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_Directive VarArgs sans CDecl ni External
% The varargs directive can only be used with procedures or functions
% that are declared with \var{cdecl} and \var{external} directives. The varargs directive
% is only meant to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_Self doit être un paramètre normal (appel par valeur)
% You can't declare self as a const or var parameter, it must always be
% a call-by-value parameter
parser_e_interface_has_no_guid=03180_E_L'interface "$1" n'a pas d'identification d'interface
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Champ de classe ou identificateur de méthode "$1" inconnu
% Properties must refer to a field or method in the same class.
parser_w_proc_overriding_calling=03182_W_Surcharge de la convention d'appel "$1" avec "$2"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used
parser_e_no_procvarobj_const=03183_E_Les constantes typées de la forme "procedure of object" ne peuvent être initialisées qu'avec NIL
% You can't assign the address of a method to a typed constant which has a
% 'procedure of object' type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates on (which can not be known at compile time).
parser_e_default_value_only_one_para=03184_E_Une valeur par défaut ne peut être affectée qu'à un paramètre
% It is not possible to specify a default value for several parameters at once. 
% The following is invalid:
% \begin{verbatim}
% Procedure MyProcedure (A,B : Integer = 0);
% \end{verbatim}
% Instead, this should be declared as
% \begin{verbatim}
% Procedure MyProcedure (A : Integer = 0; B : Integer = 0);
% \end{verbatim}
parser_e_default_value_expected_for_para=03185_E_Paramètre par défaut requis pour "$1"
% The specified parameter requires a default value.
parser_w_unsupported_feature=03186_W_Fonctionnalité non supportée
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_Les tableaux C sont passés par référence
% Any array passed to a C functions is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_Un tableau de constantes C doit être le dernier argument
% You can not add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Redéfinition du Type "$1"
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a cause for errors.
parser_w_cdecl_has_no_high=03190_W_les fonctions cdecl n'ont pas de paramètre implicite
% Functions declared with cdecl modifier do not pass an extra implicit parameter.
parser_w_cdecl_no_openstring=03191_W_Les fonctions cdecl n'acceptent pas les chaînes ouvertes
% Openstring is not supported for cdecl'ared functions.
parser_e_initialized_not_for_threadvar=03192_E_Impossible d'initialiser des variables de thread
% Variables declared as threadvar can not be initialized with a default value.
% The variables will always be filled with zero at the start of a new thread.
parser_e_msg_only_for_classes=03193_E_La directive Message n'est autorisée que dans les classes
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_Procédure ou foncction attendue
% A class method can only be specified for procedures and functions.
parser_e_illegal_calling_convention=03195_W_Directive de convention d'appel ignorée: "$1"
% Some calling conventions are supported only by certain CPUs. I.e. most non-i386 ports support
% only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE ne peut pas être utilisé avec des objets
% \var{reintroduce} is not supported for objects.
parser_e_paraloc_only_one_para=03197_E_Chaque argument doit avoir son propre emplacement
% If locations for arguments are specified explicitly as it is required by
% some syscall conventions, each argument must have it's only location, things
% like \var{procedure p(i,j : longint 'r1');} aren't allowed
parser_e_paraloc_all_paras=03198_E_Chaque argument doit avoir un emplacement explicite
% If one argument has an explicit argument location, all arguments of a procedure
% must have one.
parser_e_illegal_explicit_paraloc=03199_E_Emplacement d'argument inconnu
% The location specified for an argument isn't recognized by the compiler
parser_e_32bitint_or_pointer_variable_expected=03200_E_Variable entière sur 32 bits ou pointeur attendu
% The libbase for MorphOS/AmigaOS can be give only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_Les instructions Goto ne sont pas autorisées entre procédures différentes
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
parser_f_too_complex_proc=03202_F_Procédure trop complexe, nécessite trop de registres
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_Expression illégale
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
parser_e_invalid_integer=03204_E_Expression entière incorrecte
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_Qualificateur incorrect
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_Limite haute < limite basse
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
parser_e_macpas_exit_wrong_param=03207_E_Le paramètre de sortie doit être le nom de la procédure dans laquelle il est utilisé
% Non local exit is not allowed. This error occurs only in mode MacPas.
parser_e_illegal_assignment_to_count_var=03208_E_Affectation illégale à la variable de boucle for "$1"
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings. You can also not assign values to
% loop variables inside the loop (except in Delphi and TP modes). Use a while or
% repeat loop instead if you need to do something like that, since those
% constructs were built for that.
parser_e_no_local_var_external=03209_E_Impossible de déclarer une variable locale comme EXTERNAL
% Declaring local variables as external is not allowed. Only global variables can reference
% to external variables.
parser_e_proc_already_external=03210_E_Procédure déjà déclarée EXTERNAL
% The procedure is already declared with the EXTERNAL directive in an interface or
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Utilisation implicite de l'unité Variants
% The Variant type is used in the unit without any used unit using the Variants unit. The
% compiler has implicitly added the Variants unit to the uses list. To remove this warning
% the Variants unit needs to be added to the uses statement.
parser_e_no_static_method_in_interfaces=03212_E_Les méthodes de classe et les méthodes statiques ne peuvent pas être utilisées dans les interfaces
% The specifier \var{class} and directive \var{static} can't be used in interfaces
% because all methods of an interfaces must be public.
parser_e_arithmetic_operation_overflow=03213_E_Dépassement dans une opération arithmétique
% An operation on two integers values produced an overflow
parser_e_protected_or_private_expected=03214_E_Protected ou private attendu
% \var{strict} can be only used together with \var{protected} or \var{private}.
parser_e_illegal_slice=03215_E_SLICE ne peut pas être utilisée en dehors d'une liste de paramètres
% \var{slice} can be used only for arguments accepting an open array parameter
parser_e_dispinterface_cant_have_parent=03216_E_Une DISPINTERFACE ne peut pas avoir de classe parent
% A DISPINMTERFACE is a special type of interface which can't have a parent class
parser_e_dispinterface_needs_a_guid=03217_E_Une DISPINTERFACE requiert un GUID
% A DISPINMTERFACE always needs an interface identification
parser_w_overridden_methods_not_same_ret=03218_W_Les méthodes surchargées doivent avoir un type de résultat similaire. Ce code peut "crasher", il dépend d'un bug de l'analyseur syntaxique de Delphi ("$2" est surchargé par "$1" qui a un type de résultat différent)
% If you declare overridden methods in a class definition, they must
% have the same return type. Some versions of Delphi allow you to change the
% return type of interface methods, and even to change procedures into
% functions, but the resulting code may crash depending on the types used
% and the way the methods are called.
parser_e_dispid_must_be_ord_const=03219_E_Les Dispatch ID doivent être des constantes scalaires
% \end{description}
#
# Type Checking
#
# 04049 is the last used one
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Type non valable
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Types incompatibles: $1 spécifié mais $2 attendu
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
type_e_not_equal_types=04002_E_Incompatibilité de types entre $1 et $2
% The types are not equal
type_e_type_id_expected=04003_E_Identificateur de type attendu
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Identificateur de variable attendu
% This happens when you pass a constant to a routine (such as \var{Inc} var or \var{Dec})
% when it expects a variable. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_Expression entière attendue, "$1" spécifié
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Expression booléenne attendue mais "$1" spécifié
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Expression ordinale attendue
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_Type pointeur attendu, "$1" spécifié
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_Type classe attendu mais "$1" spécifié
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_Impossible d'évaluer l'expression constante
% This error can occur when the bounds of an array you declared does
% not evaluate to ordinal constants
type_e_set_element_are_not_comp=04012_E_Éléments de l'ensemble incompatibles
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Opération non implémentée pour des ensembles
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Conversion automatique d'un réel vers COMP qui est un type entier
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_Utilisez DIV pour obtenir un résultat entier
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_Types de chaînes incompatibles à cause du mode $V+
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_Succ ou Pred impossible pour des énumérations contenant des affectations
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_Impossible de lire ou d'écrire des variables de ce type
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% reals, pchars and strings can be read from/written to a text file.
% Booleans can only be written to text files.
type_e_no_readln_writeln_for_typed_file=04019_E_Impossible d'utiliser ReadLn ou WriteLn sur un ficher typé
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_Impossible d'utiliser Read ou Write sur un fichier non typé
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Conflit de type dans les éléments de l'ensemble
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_Lo/Hi(Longint/DWord) retourne le mot ou le double mot de poids fort ou de poids faible
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word/dword of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type cast the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Expression entière ou réelle attendue
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=04024_E_Mauvais type "$1" dans le constructeur
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Type incompatible pour l'argument no. $1: $2 spécifié au lieu de $3
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=04026_E_Méthodes (variable) et procédures (variable) sont incompatibles
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_Constante invalide passée à une fonction mathématique interne
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_Impossible d'obtenir l'adresse des constantes
% It is not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=04029_E_Cet argument ne peut pas être passé par référence
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they cannot be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_Impossible d'assigner une fonction ou une procédure locale à une variable de type procédure
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Impossible d'affecter des valeurs à une adresse
% It is not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Impossible d'affecter des valeurs à une constante
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing the value make the parameter as a value parameter or a var.
type_e_array_required=04033_E_Type de tableau requis
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode also a pointer is allowed.
type_e_interface_type_expected=04034_E_Type interface attendu, "$1" spécifié
% The compiler expected to encounter an interface type name, but got something else.
% The following code would provoke this error:
% \begin{verbatim}
% Type
%   TMyStream = Class(TStream,Integer)
% \end{verbatim}
type_w_mixed_signed_unsigned=04035_W_Le mélange d'expressions signées et de mots longs (longwords) donne un résultat sur 64 bits
% If you divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64bit which is slower than normal
% 32bit arithmetic. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_w_mixed_signed_unsigned2=04036_W_Le mélange d'expressions signées et de cardinaux ici peut provoquer une erreur de dépassement d'intervalle
% If you use a binary operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% carried out. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_Transtypage de types de tailles différentes ($1 -> $2) dans l'affectation
% Type casting to a type with a different size is not allowed when the variable is
% used for assigning.
type_e_array_index_enums_with_assign_not_possible=04038_E_Les énumérations contenant des affectations ne peuvent pas être utilisées comme index de tableau
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use it as index of an array.
type_e_classes_not_related=04039_E_Les types de classe ou d'objet "$1" et "$2" ne sont pas compatibles
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_w_classes_not_related=04040_W_Classes "$1" et "$2" incompatibles
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_e_class_or_interface_type_expected=04041_E_Classe ou interface attendue mais "$1" spécifié
% The compiler expected a class or interface name, but got another type or identifier.
type_e_type_is_not_completly_defined=04042_E_Le type "$1" n'est pas complètement défini
% This error occurs when a type is not complete: i.e. a pointer type which points to
% an undefined type.
type_w_string_too_long=04043_W_La chaîne contient plus de caractères que ne peut en contenir une chaîne courte
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring
type_w_signed_unsigned_always_false=04044_W_La comparaison est toujours FAUSSE du fait de l'intervalle des valeurs
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% false. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_signed_unsigned_always_true=04045_W_La comparaison est toujours VRAIE du fait de l'intervalle des valeurs
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% true. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_instance_with_abstract=04046_W_Construction d´une classe "$1" avec des méthodes abstraites
% An instance of a class is created which contains non-implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overriden.
type_h_in_range_check=L´opérande à gauche de l´opérateur IN doit avoir la longueur d´un octet (8 bits)
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8-bits, this may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_Divergence de taille des types, risque de perte de données ou d'erreur de dépassement d'intervalle
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_h_smaller_possible_range_check=04049_H_Taille des types non concordante, possibilité de perte de données ou d´erreur de dépassement d´intervalle
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_e_cant_take_address_of_abstract_method=04050_E_L'adresse d'une méthode abstraite ne peut pas être utilisée
% An abstract method has no body, so the address of an abstract method can't be taken.
type_e_operator_not_allowed=04051_E_L'opérateur n'est pas utilisable pour le type d'opérande
% You are trying an operator that is not available for the type of the
% operands
type_e_constant_expr_expected=04052_E_Expression constante attendue
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_L'opération "$1" n'est pas supportée pour les types "$2" et "$3"
% The operation is not allowed for the supplied types
type_e_illegal_type_conversion=04054_E_Conversion de types illégale: de "$1" vers "$2"
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
type_h_pointer_to_longint_conv_not_portable=La conversion entre types scalaires et pointeurs n´est pas portable
% If you typecast a pointer to a longint (or vice-versa), this code will not compile
% on a machine using 64-bit for pointer storage.
type_w_pointer_to_longint_conv_not_portable=04056_W_La conversion entre cardinaux et pointeurs n'est pas portable
% If you typecast a pointer to a ordinal type of a different size (or vice-versa), this can
% cause problems. This is a warning to help finding the 32bit specific code where cardinal/longint is used
% to typecast pointers to ordinals. A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_Impossible de déterminer quelle fonction overload appeler
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Variable de boucle FOR invalide
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
type_w_double_c_varargs=04059_W_Conversion d'une valeur réelle constante en valeur double pour un paramètre variable C, ajoutez un transtypage explicite pour éviter ceci
% In C, constant real values are double by default. For this reason, if you
% pass a constant real value to a variable argument part of a C function, FPC
% by default converts this constant to double as well. If you want to prevent
% this from happening, add an explicit typecast around the constant.
type_e_class_or_cominterface_type_expected=04060_E_Classe ou interface COM attendue mais "$1" spécifié
% Some operators like the AS operator are only appliable to classes or COM interfaces.
% \end{description}
#
# Symtable
#
# 05060 is the last used one
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Identificateur "$1" non trouvé
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Erreur interne dans SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Double déclaration de l'identificateur "$1"
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_Identificateur déjà défini dans $1 à la ligne $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Identificateur "$1" inconnu
% The identifier encountered has not been declared, or is used outside the
% scope where it is defined.
sym_e_forward_not_resolved=05005_E_Déclaration forward "$1" non résolue
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Erreur dans la définition du type
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_Déclaration forward "$1" non résolue
% A symbol was forward defined, but no declaration was encountered.
sym_e_only_static_in_static=05010_E_Seules les variables statiques peuvent être utilisées dans les méthodes statiques ou en dehors des méthodes
% A static method of an object can only access static variables.
sym_f_type_must_be_rec_or_class=05012_F_Type record ou classe attendu
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Les instances de classes ou les objets avec une méthode abstraite ne sont pas autorisés
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Label non défini $1
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Le Label "$1" est utilisé mais indéfini
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Déclaration de label illégale
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO et LABEL non supportés (utilisez l'option -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Label non trouvé
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_L'identificateur n'est pas un label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_Label déjà défini
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_Déclaration de type d'éléments d'ensemble illégale
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Déclaration forward de classe "$1" non résolue
% You declared a class, but you did not implement it.
sym_n_unit_not_used=05023_H_L'unité $1 n'est pas utilisée par $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Le paramètre "$1" n'est pas utilisé
% The identifier was declared (locally or globally) but
% was not used (locally or globally).
sym_n_local_identifier_not_used=05025_N_La variable locale "$1" n'est pas utilisée
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_La valeur du paramètre "$1" est définie mais jamais utilisée
% The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_La variable locale "$1" a une valeur mais n'est jamais utilisée
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Le symbole local $1 "$2" n'est pas utilisé
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_Le champ privé "$1.$2" n'est jamais utilisé
% The indicated private field is defined, but is never used in the code.
sym_n_private_identifier_only_set=05030_N_Le champ privé "$1.$2" a une valeur mais n'est jamais utilisé
% The indicated private field is declared, assigned but never read.
sym_n_private_method_not_used=05031_N_La méthode privée "$1.$2" n'est jamais utilisée
% The indicated private method is declared but is never used in the code.
sym_e_set_expected=05032_E_Type ensemble attendu
% The variable or expression is not of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_Le résultat de la fonction semble indéfini
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_Le type $1 n'est pas aligné correctement pour C dans l'enregistrement courant
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Identificateur de champ "$1" inconnu
% The field doesn't exist in the record/object definition.
sym_w_uninitialized_local_variable=05036_W_La variable locale "$1" ne semble pas être initialisée
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_uninitialized_variable=05037_W_La variable "$1" ne semble pas être initialisée
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_L'identificateur "$1" n'a pas de membres
% This error is generated when an identifier of a record,
% field, or method is accessed while it is not defined.
sym_h_param_list=05039_H_Déclaration trouvée: $1
% You get this when you use the \var{-vh} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Donnée trop grande (max. 2GB)
% You get this when you declare a data element whose size exceeds the
% prescribed limit (2 Gb on 80386+/68020+ processors)
sym_e_no_matching_implementation_found=05042_E_Aucune implémentation correspondante pour la méthode "$1" trouvée dans l'interface
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_Le symbole "$1" est déprécié
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Usage of this symbol
% should be avoided as much as possible.
sym_w_non_portable_symbol=05044_W_Le symbole "$1" n'est pas portable
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, usage
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_Le symbole "$1" n'est pas implémenté
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_Impossible de créer un type unique depuis ce type
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}.
sym_h_uninitialized_local_variable=05057_H_La variable locale "$1" ne semble pas initialisée
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_h_uninitialized_variable=05058_H_La variable "$1" ne semble pas initialisée
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_function_result_uninitialized=05059_W_La variable Result de la fonction ne semble pas initialisée
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_h_function_result_uninitialized=05060_H_La variable Result de la fonction ne semble pas initialisée
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_w_identifier_only_read=05061_W_La variable "$1" est lue mais ne reçoit de valeur nulle part
% You have read the value of a variable, but nowhere assigned a value to
% it.
% \end{description}
#
# Codegenerator
#
# 06040 is the last used one
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_La taille de la liste de paramètre dépasse 65535 octets
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_file_must_call_by_reference=06012_E_Les paramètres de type File doivent être passés par adresse
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_L'utilisation d'un pointeur long n'est pas autorisée ici
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_Les fonctions déclarées EXPORT ne peuvent pas être appelées
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Appel probablement illégal d'un constructeur ou d'un destructeur
% The compiler detected that a constructor or destructor is called within a
% a method. This will probably lead to problems, since constructors / destructors
% require parameters on entry.
cg_n_inefficient_code=06017_N_Code inefficace
% Your statement seems dubious to the compiler.
cg_w_unreachable_code=06018_W_Code inaccessible
% You specified a construct which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Les méthodes abstraites ne peuvent pas être appelées directement
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Registre $1 poids $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_Cadre de pile omis
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=06031_E_Un object ou des méthodes de classe ne peuvent pas être "inline".
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Les appels à des variables procédure ne peuvent pas être "inline".
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_Pas de code pour la procédure inline stockée
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Impossible d'accéder à l'élément zéro d'une chaîne ANSI, large ou longue, utilisez à la place (set)length 
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such a string types
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructeurs et destructeurs ne peuvent pas être appelés à l'intérieur d'une clause 'with'
% Inside a \var{with} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_Impossible d'appeler directement les méthodes du gestionnaire de messages
% A message method handler method cannot be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=06039_E_Saut à l'intérieur ou en dehors d'un bloc de traitement d'une exception
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
cg_e_control_flow_outside_finally=06040_E_Les instructions de contrôle d'exécution ne sont pas autorisées dans un bloc finally
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
cg_w_parasize_too_big=06041_W_La taille des paramètres dépasse la limite supportée par certains processeurs.
% This indicates that you are declaring more than 64K of parameters, which
% might not be supported on other processor targets.
cg_w_localsize_too_big=06042_W_La taille de la variable locale dépasse la limite supportée par certains processeurs.
% This indicates that you are declaring more than 32K of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_La taille des variables locales dépasse la limite supportée
% This indicates that you are declaring more than 32K of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK non autorisé
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE non autorisé
% You're trying to use \var{continue} outside a loop construction.
cg_f_unknown_compilerproc=06046_F_Procédure "$1" inconnue. Verifiez que vous utilisez la bonne version de bibliothèque d'exécution.
% The compiler expects that the runtime library contains some subrountines. If you see this error
% and you didn't mess with the runtime library, it's very likely that the runtime library
% you're using doesn't match the used compiler. If you changed the runtime library this error means
% that you removed a subroutine which the compiler needs for internal use.
% \end{description}
# EndOfTeX
#
# Assembler reader
#
# 07097 is the last used one
#
asmr_d_start_reading=07000_DL_Début d'analyse assembleur de style $1
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_DL_Analyse assembleur de style $1 terminée
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Un identificateur qui n'est pas un label contient @
% A identifier which isn't a label can't contain a @.
asmr_e_building_record_offset=07004_E_Erreur de construction de l'offset du record
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET utilisé sans identificateur
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE utilisé sans identificateur
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_Impossible d'utiliser une variable locale ou des paramètres ici
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the frame pointer register so the
% address can't be obtained directly.
asmr_e_need_offset=07008_E_Utilisation d'OFFSET nécessaire
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_Utilisation de $ nécessaire
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_Impossible d'utiliser plusieurs symboles relogeables
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Les symboles relogeables ne peuvent qu'être ajoutés
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Expression contante incorrecte
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Symbole relogeable non autorisé
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Syntaxe de référence incorrecte
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_Impossible d´atteindre $1 depuis ce code
% You can not read directly the value of a local variable or parameter
% of a higher level procedure in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Les symboles locaux et les labels ne sont pas autorisés en tant que références
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Utilisation d'un registre de base et d'un index de registre incorrecte
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Erreur possible dans la gestion d'un champ d'objet
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Facteur d'échelle spécifié incorrect
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Utilisation d'index de registre multiples
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Type de paramètre incorrect
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_Paramètre chaîne incorrect pour le code opération: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE et @DATA non supportés
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Référence à un label Null non autorisée
asmr_e_expr_zero_divide=07025_E_Division par zéro dans l'évaluateur assembleur
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_Expression illégale
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_Séquence d'échappement ignorée: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_Référence au symbole incorrecte
asmr_w_fwait_emu_prob=07029_W_Fwait peut causer des problèmes d'émulation avec emu387
asmr_w_fadd_to_faddp=07030_W_$1 sans paramètre traduit en $1P
asmr_w_enter_not_supported_by_linux=07031_W_Instruction ENTER non supportée par le noyau Linux
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Appel d'une fonction surchargée en assembleur
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_Type de symbole non supporté
asmr_e_constant_out_of_bounds=07034_E_Valeur de constante hors intervalle
asmr_e_error_converting_decimal=07035_E_Erreur de conversion décimale $1
% A constant decimal value does not have the correct syntax
asmr_e_error_converting_octal=07036_E_Erreur de conversion octale $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Erreur de conversion binaire $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Erreur de conversion hexadecimale $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 traduit en $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 est associé(e) à une fonction surchargée
asmr_e_cannot_use_SELF_outside_a_method=07041_E_Impossible d'utiliser SELF en dehors d'une méthode
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_Impossible d'utiliser OLDEBP en dehors d'une procédure imbriquée
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Les procédures ne peuvent pas retourner de valeur en code assembleur
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG non supporté
asmr_e_size_suffix_and_dest_dont_match=07045_E_La taille du suffixe et celle de la destination ou de la source ne correspondent pas
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_La taille du suffixe ne correspond pas à celle de la destination ou de la source
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Erreur de syntaxe assembleur
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_Combinaison de codes opération et de paramètres incorrecte
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Erreur de syntaxe assembleur dans le paramètre
asmr_e_syn_constant=07050_E_Erreur de syntaxe assembleur dans la constante
asmr_e_invalid_string_expression=07051_E_Expression chaîne incorrecte
asmr_w_const32bit_for_address=07052_W_Constante avec symbole $1 utilisée comme adresse n'est pas un pointeur
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Code opération $1 non reconnu
% This opcode is not known
asmr_e_invalid_or_missing_opcode=07054_E_Code opération incorrect ou manquant
asmr_e_invalid_prefix_and_opcode=07055_E_Combinaison invalide de prefix avec un code opération: $1
asmr_e_invalid_override_and_opcode=07056_E_Combinaison invalide de override avec un code opération: $1
asmr_e_too_many_operands=07057_E_Nombre de paramètres insuffisant
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR ignoré
asmr_w_far_ignored=07059_W_FAR ignoré
asmr_e_dup_local_sym=07060_E_Symbole local $1 dupliqué
asmr_e_unknown_local_sym=07061_E_Symbole local $1 inconnu
asmr_e_unknown_label_identifier=07062_E_Identificateur de label $1 inconnu
asmr_e_invalid_register=07063_E_Nom de registre incorrect
% There is an unknown register name used as operand.
asmr_e_invalid_fpu_register=07064_E_Nom de registre de calcul réel incorrect
% There is an unknown register name used as operand.
asmr_w_modulo_not_supported=07066_W_Modulo non supporté
asmr_e_invalid_float_const=07067_E_Constante réelle $1 invalide
% The floating point constant declared in an assembler block is
% invalid.
asmr_e_invalid_float_expr=07068_E_Expression réelle invalide
% The floating point expression declared in an assembler block is
% invalid.
asmr_e_wrong_sym_type=07069_E_Type de symbole incorrect
asmr_e_cannot_index_relative_var=07070_E_Impossible d'indicer directement une variable locale ou un paramètre avec un registre
% Trying to index using a base register a symbol which is already relative
% to a register. This is not possible, and will probably lead to crashes.
asmr_e_invalid_seg_override=07071_E_Expression d'override du segment incorrecte
asmr_w_id_supposed_external=07072_W_Identificateur $1 supposé external
% There is a reference to an undefined symbol. This will not result
% in an error, since the symbol might be external, but may cause
% problems at link time if the symbol is not defined anywhere.
asmr_e_string_not_allowed_as_const=07073_E_Constantes chaînes non autorisées
% Character strings are not allowed as constants.
asmr_e_no_var_type_specified=07074_Type de variable non spécifié
% The syntax expects a type idenfitifer after the dot, but
% none was found.
asmr_w_assembler_code_not_returned_to_text=07075_E_Code assembleur non retourné à la section text
% There was a directive in the assembler block to change sections,
% but there is a missing return to the text section at the end
% of the assembler block. This might cause errors during link time.
asmr_e_not_directive_or_local_symbol=07076_E_$1 n'est pas une directive ou un symbole local
% This symbol is unknown.
asmr_w_using_defined_as_local=07077_E_Utilisation d'un nom défini comme label local
asmr_e_dollar_without_identifier=07078_E_Dollar utilisé sans identificateur
% A constant expression has an identifier which does not start with
% the $ symbol.
asmr_w_32bit_const_for_address=07079_W_Constante 32 bits créée comme adresse
% A constant was used as an address. This is probably an error,
% since using absolute addresses will probably not work.
asmr_n_align_is_target_specific=07080_N_.align dépend de la cible, utilisez .balign ou .p2align
% Using the .align directive is platform specific, and its meaning will vary
% from one platform to another.
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Impossible d'accéder directement aux champs transmis en paramètres
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_Impossible d'accéder directement aux champs d'objets ou de classes
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_Taille non spécifiée et impossibilité de déterminer la taille des paramètres
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=Impossible d'utiliser RESULT dans cette fonction
% Some functions which return complex types cannot use the \var{result}
% keyword.
asmr_w_adding_explicit_args_fXX=07086_W_"$1" sans paramètre traduit en "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" traduit en "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" traduit en "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Caractère < non autorisé ici
% The shift operator requires the << characters. Only one
% of those characters was found.
asmr_e_invalid_char_greater=07090_E_Caractère > non autorisé ici
% The shift operator requires the >> characters. Only one
% of those characters was found.
asmr_w_align_not_supported=07093_W_ALIGN non supporté
asmr_e_no_inc_and_dec_together=07094_E_Inc et Dec ne peuvent pas être ensemble
% Trying to use an increment and a decrement within the same
% opcode on the 680x0. This is impossible.
asmr_e_invalid_reg_list_in_movem=07095_E_Liste de registres invalide pour l'opération movem
% Trying to use the \var{movem} opcode with invalid registers
% to save or restore.
asmr_e_invalid_reg_list_for_opcode=07096_E_Liste de registres invalide pour le code opération
asmr_e_higher_cpu_mode_required=07097_E_Mode CPU supérieur nécessaire ($1)
% Trying to use an instruction which is not supported in the current
% cpu mode. Use a higher cpu generation to be able to use this
% opcode in your assembler block
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_Taille non spécifiée et impossible de déterminer la taille des paramètres, utilisation de DWORD par défaut
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Erreur de syntaxe durant l'analyse d'une opération de décalage
% ARM only; ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
#
# Assembler/binary writers
#
# 08018 is the last used one
#
asmw_f_too_many_asm_files=08000_F_Trop peu de fichiers assembleur
% With smartlinking enabled, there are too many assembler
% files generated. Disable smartlinking.
asmw_f_assembler_output_not_supported=08001_F_Résultat de l'assembleur sélectionné non supporté
asmw_f_comp_not_supported=08002_F_Comp non supporté
asmw_f_direct_not_supported=08003_F_Direct non supporté pour les 'writers' binaires
% Direct assembler mode is not supported for binary writers.
asmw_e_alloc_data_only_in_bss=08004_E_Allocation de données supportée uniquement dans la section bss
asmw_f_no_binary_writer_selected=08005_F_Aucun 'writer' binaire sélectionné
asmw_e_opcode_not_in_table=08006_E_Asm: Code opération $1 absent de la liste
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 combinaison du code opération et des paramètres incorrecte
asmw_e_16bit_not_supported=08008_E_Asm: référence 16 bits non supportée
asmw_e_invalid_effective_address=08009_E_Asm: Adresse effective incorrecte
asmw_e_immediate_or_reference_expected=08010_E_Asm: Immediate ou référence attendue
asmw_e_value_exceeds_bounds=08011_E_Asm: la valeur $1 dépasse les limites $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Saut court hors limites $1
asmw_e_undefined_label=08013_E_Asm: Label $1 non défini
asmw_e_comp_not_supported=08014_E_Asm: type Comp non supporté pour cette cible
asmw_e_extended_not_supported=08015_E_Asm: type Extended non supporté pour cette cible
asmw_e_duplicate_label=08016_E_Asm: label $1 dupliqué
asmw_e_redefined_label=08017_E_Asm: label $1 redéfini
asmw_e_first_defined_label=08018_E_Asm: Première définition
asmw_e_invalid_register=08019_E_Asm: Registre $1 incorrect
asmw_e_16bit_32bit_not_supported=08020_E_Asm: références 16 et 32 bits non supportées
asmw_e_64bit_not_supported=08021_E_Asm: paramètres 64 bits non supportés
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
exec_w_source_os_redefined=09000_W_Système d'exploitation source redéfini
% The source operating system is redefined.
exec_i_assembling_pipe=09001_I_Assemblage (pipe) de $1
% Assembling using a pipe to an external assembler.
exec_d_cant_create_asmfile=09002_E_Impossible de créer le fichier assembleur: $1
% The mentioned file can't be created. Check if you have got
% access permissions to create this file
exec_e_cant_create_objectfile=09003_E_Impossible de créer le fichier object: $1
% The mentioned file can't be created. Check if you've
% got access permissions to create this file
exec_e_cant_create_archivefile=09004_E_Impossible de créer le fichier archive: $1
% The mentioned file can't be created. Check if you've
% access permissions to create this file
exec_e_assembler_not_found=09005_E_Assembleur $1 non trouvé, utilisation d'un assembleur externe
% The assembler program was not found. The compiler will produce a script that
% can be used to assemble and link the program.
exec_t_using_assembler=09006_T_Utilisation de l'assembleur: $1
% Information message saying which assembler is being used.
exec_e_error_while_assembling=09007_E_Erreur durant l'assemblage, code de sortie $1
% There was an error while assembling the file using an external assembler.
% Consult the documentation of the assembler tool to find out more information
% on this error.
exec_e_cant_call_assembler=09008_E_Impossible d'appeler l'assembleur, erreur $1, utilisation d'un assembleur externe
% An error occurred when calling an external assembler, The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_assembling=09009_I_Assemblage de $1
% An informational message stating which file is being assembled.
exec_i_assembling_smart=09010_I_Assemblage avec édition intelligente des liens de $1
% An informational message stating which file is being assembled using smartlinking.
exec_w_objfile_not_found=09011_W_Objet $1 non trouvé, l'édition des liens peut échouer !
% One of the object file is missing, and linking will probably fail.
% Check your paths.
exec_w_libfile_not_found=09012_W_Bibliothèque $1 non trouvée, l'édition des liens peut échouer !
% One of the library file is missing, and linking will probably fail.
% Check your paths.
exec_e_error_while_linking=09013_E_Erreur durant l'édition des liens
% Generic error while linking.
exec_e_cant_call_linker=09014_E_Impossible d'appeler l'éditeur de liens, utilisation d'un éditeur de liens externe
% An error occurred when calling an external linker, The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_linking=09015_I_Édition des liens de $1
% An informational message, showing which program or library is being linked.
exec_e_util_not_found=09016_E_Utilitaire $1 non trouvé, utilisation d'un éditeur de liens externe
% An external tool was not found, the compiler will produce a script that
% can be used to assemble and link or postprocess the program.
exec_t_using_util=09017_T_Utilisation de l'utilitaire $1
% An informational message, showing which external program (usually a postprocessor) is being used.
exec_e_exe_not_supported=09018_E_Création d'exécutables non supportée
% Creating executable programs is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_e_dll_not_supported=09019_E_Création de bibliothèques dynamiques/partagées non supportée
% Creating dynamically loadable libraries is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_i_closing_script=09020_I_Fermeture du script $1
% Informational message showing when the external assembling an linking script is finished.
exec_e_res_not_found=09021_E_Compilateur de ressource non trouvé, utilisation d'un compilateur externe
% An external resource compiler was not found, the compiler will produce a script that
% can be used to assemble, compile resources and link or postprocess the program.
exec_i_compilingresource=09022_I_Compilation de la resource $1
% An informational message, showing which resource is being compiled.
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_Édition statique des liens de l'unité $1 impossible, utilisation d'une édition intelligente des liens
% Statical linking was requested, but a unit which is not statically linkable was used.
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_Édition intelligente des liens de l'unité $1 impossible, utilisation d'une édition de liens statique
% Smart linking was requested, but a unit which is not smart-linkable was used.
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_Édition des liens partagée de l'unité $1 impossible, utilisation d'une édition de liens statique
% Shared linking was requested, but a unit which is not shared-linkable  was used.
exec_e_unit_not_smart_or_static_linkable=09026_E_Édition intelligente ou statique des liens de l'unité $1 impossible
% Smart or static linking was requested, but a unit which cannot be used for either  was used.
exec_e_unit_not_shared_or_static_linkable=09027_E_Édition des liens partagée ou statique de l'unité $1 impossible
% Shared or static linking was requested, but a unit which cannot be used for either  was used.
exec_d_resbin_params=09028_D_Exécution du compilateur de ressource "$1" avec "$2" comme ligne de commande
% An informational message showing which command-line is used for the resource compiler.
%\end{description}
# EndOfTeX
#
# Executable information
#
# BeginOfTeX
% \section{Executable information messages.}
% This section lists all messages that the compiler emits when an executable program is produced,
% and only when the internal linker is used.
% \begin{description}
execinfo_f_cant_process_executable=09128_F_Post-traitement de l'exécutable $1 impossible
% Fatal error when the compiler is unable to post-process an executable.
execinfo_f_cant_open_executable=09129_F_Impossible d'ouvrir l'exécutable $1
% Fatal error when the compiler cannot open the file for the executable.
execinfo_x_codesize=09130_X_Taille du code: $1 octets
% Informational message showing the size of the produced code section.
execinfo_x_initdatasize=09131_X_Taille des données initialisées: $1 octets
% Informational message showing the size of the initialized data section.
execinfo_x_uninitdatasize=09132_X_Taille des données non-initialisées: $1 octets
% Informational message showing the size of the uninitialized data section.
execinfo_x_stackreserve=09133_X_Taille de pile réservée: $1 octets
% Informational message showing the stack size that the compiler reserved for the executable.
execinfo_x_stackcommit=09134_X_Taille de pile validée: $1 octets
% Informational message showing the stack size that the compiler committed for the executable.
%\end{description}
# EndOfTeX
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
unit_t_unitsearch=10000_T_Recherche de l'unité $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_Chargement du fichier PPU $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_Fichier PPU: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_Drapeaux PPU: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_Somme de contrôle du fichier PPU: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_Fichier PPU compilé à $1
% When you use the \var{-vu} flag, the time the unit was compiled is shown.
unit_u_ppu_file_too_short=10006_U_Fichier PPU trop court
% The ppufile is too short, not all declarations are present.
unit_u_ppu_invalid_header=10007_U_En-tête PPU invalide (PPU non trouvé au début)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_Version $1 de fichier PPU invalide
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_Fichier PPU compilé pour un autre processeur
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=10010_U_Fichier PPU compilé pour une autre cible
% This unit file was compiled for a different target, and
% cannot be read
unit_u_ppu_source=10011_U_Fichier source: $1
% When you use the \var{-vu} flag, the unit source file name is shown.
unit_u_ppu_write=10012_U_Écriture de $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_Impossible d'écrire le fichier PPU
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_Erreur lors de la lecture du fichier PPU
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=10015_F_Fin de fichier PPU inattendue
% Unexpected end of file. This may mean that the PPU file is
% corrupted.
unit_f_ppu_invalid_entry=10016_F_Entrée invalide dans le fichier PPU $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_PPU Dbx count problem
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Nom d'unité illégal: $1
% The name of the unit does not match the file name.
unit_f_too_much_units=10019_F_Trop d'unités
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{fmodule.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Référence circulaire entre les unités $1 et $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_Compilation de l'unité $1 impossible, pas de sources disponibles
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_Impossible de trouver l'unité $1
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your configuration file for the unit paths
unit_w_unit_name_error=10023_W_Unité $1 non trouvée mais $2 existe
% This error message is no longer used.
unit_f_unit_name_error=10024_F_Unité $1 cherchée mais $2 trouvée
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_La compilation de l´unité System requiert l´option -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_$1 erreurs lors de la compilation du module, arrêt
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Chargement à partir de $1 ($2) de l'unité $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_Recompilation de $1, la somme de contrôle est maintenant $2
% The unit is recompiled because the checksum of a unit it depends on has 
% changed.
unit_u_recompile_source_found_alone=10029_U_Recompilation de $1, seul le source a été trouvé
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Recompilation de l'unité, la bibliothèque statique est plus ancienne que le fichier PPU
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Recompilation de l'unité, la bibliothèque partagée est plus ancienne que le fichier PPU
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Recompilation de l'unité, les fichiers OBJ et ASM sont plus anciens que le fichier PPU
% When you use the \var{-vu} flag, the compiler warns if the assembler or
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Recompilation de l'unité, le fichier OBJ est plus ancien que le fichier ASM
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_Analyse de l'interface de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_parsing_implementation=10035_U_Analyse de l'implémentation de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_Deuxième compilation de l'unité $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_Vérification du fichier PPU $1, date/heure $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
### The following two error msgs are currently disabled.
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
unit_w_cant_compile_unit_with_changed_incfile=10040_W_Impossible de recompiler l'unité $1 alors que des fichiers inclus ont été trouvés
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_h_source_modified=10041_H_Le fichier $1 est plus récent que le fichier PPU $2
% A modified source file for a unit was found that was compiled with the
% release flag (-Ur). The unit will not implicitly be recompiled because
% this release flag is set.
unit_u_ppu_invalid_fpumode=10042_U_Utilisation d'une unité compilée avec un mode FPU incorrect
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_Chargement des unités déclarées dans l'interface de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the interface part of the unit.
unit_u_loading_implementation_units=10044_U_Chargement des unités déclarées dans l'implementation de $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the implementation part of the unit.
unit_u_interface_crc_changed=10045_U_La somme de contrôle de l'interface de l'unité $1 a changé
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated for the interface has been changed after the implementation
% has been parsed.
unit_u_implementation_crc_changed=10046_U_La somme de contrôle de l'implémentation de l'unité $1 a changé
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated has been changed after the implementation
% has been parsed.
unit_u_finished_compiling=10047_U_Recompilation de l'unité $1 terminée
% When you use the \var{-vu} flag, the compiler warns that it
% has finished compiling the unit.
unit_u_add_depend_to=10048_U_Ajout d'une dépendance entre $1 et $2
% When you use the \var{-vu} flag, the compiler warns that it
% has added a dependency between the two units.
unit_u_no_reload_is_caller=10049_U_Pas de rechargement de $1 qui est l'unité appelante
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is the unit that wants
% to load this unit
unit_u_no_reload_in_second_compile=10050_U_Pas de rechargement de $1 déjà en cours de recompilation
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is already in a second recompile
unit_u_flag_for_reload=10051_U_Nécessité de recharger $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to reload the unit
unit_u_forced_reload=10052_U_Rechargement forcé
% When you use the \var{-vu} flag, the compiler warns that it
% has is reloading the unit because it was required
unit_u_previous_state=10053_U_État précédent de l'unité $1 : $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit
unit_u_second_compile_unit=10054_U_Recompilation de l'unité $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_loading_unit=10055_U_Chargement de l'unité $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the unit.
unit_u_finished_loading_unit=10056_U_Chargement de l'unité $1 terminé
% When you use the \var{-vu} flag, the compiler warns that it finished
% loading the unit.
unit_u_registering_new_unit=10057_U_Enregistrement de la nouvelle unité $1
% When you use the \var{-vu} flag, the compiler warns that it has
% found a new unit and registers it in the internal lists.
unit_u_reresolving_unit=10058_U_Re-résolution de l'unité $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to recalculate the internal data of the unit
unit_u_skipping_reresolving_unit=10059_U_Saut de la re-resolution de l'unité $1, chargement des unités utilisées
% When you use the \var{-vu} flag, the compiler warns that it
% skips to recalculate the internal data of the unit because there
% is no data to recalculate
% \end{description}
# EndOfTeX
#
#  Options
#
# 11039 is the last used one
#
option_usage=11000_O_$1 [options] <nom_fichier> [options]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Un seul fichier source supporté
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_Les fichiers DEF ne peuvent être créés que pour OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_Les fichiers de réponse imbriqués ne sont pas supportés
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_Pas de nom de fichier source sur la ligne de commande
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_Aucune option trouvée dans le fichier de configuration $1
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Paramètre illégal: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? génère les pages d'aide
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Trop de fichiers de configuration imbriqués
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Ouverture du fichier $1 impossible
% The option file cannot be found.
option_reading_further_from=11010_D_Lecture d'options supplémentaires depuis $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_La destination est déjà définie à: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Bibliothèques partagées non supportées sous DOS, retour à une édition de liens statique
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_Trop de IF(N)DEF
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_Trop de ENDIF
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_Directive de compilation conditionnelle non fermée à la fin du fichier
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_La génération d'informations de débogage n'est pas supportée par cet exécutable
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Essayez de recompiler avec -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_W_Vous utilisez le commutateur obsolète $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_W_Vous utilisez le commutateur obsolète $1, veuillez utiliser $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_Switching assembler to default source writing assembler
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_La sortie assembleur "$1" n'est pas compatible avec "$2"
option_asm_forced=11022_W_Utilisation de l'assembleur "$1" forcée
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_Lecture des options depuis le fichier $1
% Options are also read from this file
option_using_env=11027_T_Lecture des options depuis la variable d'environnement $1
% Options are also read from this environment string
option_handling_option=11028_D_Utilisation de l'option "$1"
% Debug info that an option is found and will be handled
option_help_press_enter=11029__*** Appuyez sur Entrée ***
option_start_reading_configfile=11030_H_Début de la lecture du fichier de configuration $1
% Starting of config file parsing.
option_end_reading_configfile=11031_H_Fin de la lecture du fichier de configuration $1
% End of config file parsing.
option_interpreting_option=11032_D_Interprétation de l'option "$1"
option_interpreting_firstpass_option=11036_D_Interprétation de l'option de 1ère passe "$1"
option_interpreting_file_option=11033_D_Interprétation de l'option de fichier "$1"
option_read_config_file=11034_D_Lecture du fichier de configuration "$1"
option_found_file=11035_D_Fichier source "$1" trouvé
% Additional infos about options, displayed
% when you have debug option turned on.
option_code_page_not_available=11039_E_Page de codes inconnue
option_config_is_dir=11040_F_Le fichier de configuration $1 est un répertoire
% Directories can not be used as configuration files.
%\end{description}
# EndOfTeX
#
# Logo (option -l)
#
option_logo=11023_[ Compilateur Free Pascal version $FPCFULLVERSION [$FPCDATE] for $FPCCPU
Copyright (c) 1993-2008, Florian Klaempfl ]
#
# Info (option -i)
#
option_info=11024_[ Compilateur Free Pascal version $FPCVERSION
Date du compilateur: $FPCDATE
Processeur cible: $FPCCPU
Cibles supportées: $OSTARGETS
Jeu d'instructions CPU supportés: $INSTRUCTIONSETS
Jeu d'instructions FPU supportés: $FPUINSTRUCTIONSETS
Optimisations supportées: $OPTIMIZATIONS
Ce programme est fourni sous la License Publique Générale GNU (GNU GPL).
Pour plus d'informations, consultez le fichier COPYING.FPC.
Rapports de bogues, suggestions, etc. à: bugs@freepascal.org ]
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
**0*_mettre + après un booléen pour activer l'option, - pour la désactiver
**1a_conserve les fichiers assembleurs créés pendant la compilation
**2al_liste le code source dans les fichiers assembleur
**2an_liste les infos de noeud dans le fichier assembleur
*L2ap_utilise des pipes au lieu de fichiers assembleur temporaires
**2ar_liste les allocations de registres dans les fichiers assembleur
**2at_liste les allocations de données temporaire dans la pile
**1A<x>_format de sortie :
**2Adefault_utilise l'assembleur défini par défaut
3*2Aas_fichier objet généré par GNU AS
3*2Anasmcoff_fichier COFF (Go32v2) avec Nasm
3*2Anasmelf_fichier ELF32 (Linux) avec Nasm
3*2Anasmwin32_fichier objet Win32 utilisant Nasm
3*2Anasmwdosx_fichier objet Win32/WDOSX utilisant Nasm
3*2Awasm_fichier objet utilisant Wasm (Watcom)
3*2Anasmobj_fichier OBJ avec Nasm
3*2Amasm_fichier OBJ avec Masm (Microsoft)
3*2Atasm_fichier OBJ avec Tasm (Borland)
3*2Aelf_elf32 (Linux) utilisant le writer interne
3*2Acoff_coff (Go32v2) using internal writer
3*2Apecoff_pecoff (Win32) using internal writer
4*2Aas_fichier objet généré par GNU AS
6*2Aas_fichier .o Unix utilisant GNU AS
6*2Agas_assembleur GNU Motorola
6*2Amit_syntaxe MIT (anciennement GAS)
6*2Amot_assembleur Motorola standard
A*2Aas_assembler avec GNU AS
P*2Aas_assembler avec GNU AS
S*2Aas_assembler avec GNU AS
**1b_génère les informations pour le browser
**2bl_génère les informations locales également
**1B_recompile toutes les unités
**1C<x>_options de génération de code :
**2Cc<x>_définir les convention d'appel <x> par défaut
**2CD_créer une bibliothèque dynamique
**2Ce_compilation avec émulation des opérations à virgule flottante
**2Cf<x>_sélectionner le jeu d'instructions FPU à utiliser, voir fpc -i pour les valeurs possibles
**2Cg_Générer du code PIC
**2Ch<n>_<n> taille du tas en bytes (entre 1023 et 67107840)
**2Ci_IO-checking
**2Cn_pas de linking
**2Co_génère des tests d'overflow pour les opérations sur les entiers
**2Cp<x>_sélectionner le jeu d'instructions, voir fpc -i pour les valeurs possibles
**2Cr_controle d'intervalles
**2CR_vérifier la validité de l'appel à la méthode d'objet
**2Cs<n>_spécifie <n> comme taille de la pile
**2Ct_test de débordement de pile
**2CX_créer également une bibliothèque avec liens optimisés
**1d<x>_définit le symbole <x>
**1D_génère un fichier DEF
**2Dd<x>_assigne la description à <x>
**2Dv<x>_affecter la version <x> à la DLL
*O2Dw_application en mode protégé
**1e<x>_définit le chemin vers l'exécutable
**1E_comme -Cn
**1F<x>_définit des chemins et noms de fichiers :
**2Fa<x>[,y]_Pour un programme charger d'abord les unités <x> et [y] avant que uses ne soit analysé
**2Fc<x>_Utiliser la page de codes <x> en entrée
**2FD<x>_définit le répertoire où chercher les utilitaires de compilation
**2Fe<x>_redirige les erreurs vers le fichier <x>
**2FE<x>_répertoire pour les exe/unités : <x>
**2Fi<x>_ajoute <x> à la liste des répertoires pour fichiers inclus
**2Fl<x>_ajoute <x> à la liste des répertoires pour librairies
**2FL<x>_utilises <x> comme lieur dynamique
**2Fo<x>_ajoute <x> à la liste des répertoires pour fichiers objets
**2Fr<x>_charge le fichier erreur <x>
**2Fu<x>_ajoute <x> à la liste des répertoires pour unités
**2FU<x>_définit le répertoire d'écriture des unités à <x>, en dépit de -FE
*g1g<x>_génère des informations de débogage :
*g2gc_générer des contrôles pour les pointeurs
*g2gd_utilise dbx
*g2gg_utilise gsym
*g2gh_use l'unité de tracage du tas
*g2gl_utiliser l'unité line info pour montrer plus de détails dans les traces
*g2gv_générer des programmes traçables avec valgrind
*g2gw_générer des infos de débogage au format dwarf
**1i_information
**2iD_donne la date du compilateur
**2iV_donne la version du compilateur
**2iW_retourner la version complète du compilateur
**2iSO_donne l'OS du compilateur
**2iSP_donne le processeur du compilateur
**2iTO_donne l'OS cible
**2iTP_donne le processeur cible
**1I<x>_ajoute <x> à la liste des répertoires pour fichiers inclus
**1k<x>_transmet <x> au linker
**1l_écrit le logo
**1M<x>_utiliser le type de langage <x>
**2Mfpc_dialecte Free Pascal (par défaut)
**2Mobjfpc_activation de certaines extensions Delphi 2
**2Mdelphi_essaye d'être compatible avec Delphi
**2Mtp_essaye d'être compatible avec TP/BP 7.0
**2Mgpc_essaye d'être compatible avec gpc
**2Mmacpas_essaye d'être compatible avec les dialectes Pascal du Macintosh
**1n_ne pas lire le fichier de configuration par défaut
**1o<x>_change le nom de l'executable en <x>
**1O<x>_optimisations :
3*2Oa_définir l'alignement <type>=<values>
3*2Og_génère du code compact
3*2OG_génère du code rapide (defaut)
3*2Or_garde certaines variables dans des registres
3*2Ou_autorise les optimisations incertaines (voir docs)
3*2O1_optimisations niveau 1 (optimisations rapides)
3*2O2_optimisations niveau 2 (-O1 + optimisations plus lentes)
3*2O3_optimisations niveau 3 (comme -O2u)
3*2Op<x>_processeur cible :
3*3Op1_définit 386/486 comme processeur cible
3*3Op2_définit Pentium/PentiumMMX (tm) comme processeur cycle
3*3Op3_définit PPro/PII/c6x86/K6 (tm) comme processeur cycle
3*3Op4_définit Pentium 4 (tm) comme processeur cycle
3*3Op5_définit Pentium M (tm) comme processeur cycle
6*2Og_génère du code compact
6*2OG_génère du code rapide (par défaut)
6*2Ox_optimiser au maximum (encore BOGUÉ !!!)
6*2O0_définir le processeur cible MC68000
6*2O2_définir le processeur cible MC68020+
**1pg_génère du code pour profiler avec gprof (définit FPC_PROFILE)
3*1R<x>_type lecture assembleur :
3*2Ratt_lit l'assembleur AT&T
3*2Rintel_lit l'assembleur Intel
6*2RMOT_lire l'assembleur type motorola
**1S<x>_options de syntaxe :
**2S2_identique à -Mobjfpc
**2Sa_include assertion code.
**2Sc_autorise les operateurs type C (*=,+=,/= et -=)
**2Sd_identique à -Mdelphi
**2Se<x>_erreur d'options. <x> est une combinaison de :
**3*_<n> : le compilateur s'arrête après <n> erreurs (1 par défaut)
**3*_w : le compilateur s'arrête également en cas d'avertissements
**3*_n : le compilateur s'arrête également en cas de notes
**3*_h : le compilateur s'arrête également en cas de suggestions
**2Sg_autorise LABEL et GOTO
**2Sh_utilise les ansistrings
**2Si_supporte les INLINE type C++
**2SI<x>_définir le style d'interface <x>
**3SIcom_interface compatible COM (par défaut)
**3SIcorba_interface compatible CORBA
**2Sk_charger l'unité fpcylix
**2Sm_support des macros comme C (global)
**2So_identique à -Mtp
**2Sp_identique à -Mgpc
**2Ss_les constructeurs doivent s'appeler init (et les destructeurs done)
**2St_autorise "static" dans les objects
**1s_n'appelle pas l'assembleur ni le linker
**2sh_générer un script à lier sur l'hôte
**2st_générer un script à lier sur la cible
**2sr_passer la phase d'allocation de registre (à utiliser avec -alr)
**1T<x>_système d'expliotation cible:
3*2Temx_OS/2 via EMX (et les extensions EMX/RSX)
3*2Tfreebsd_FreeBSD
3*2Tgo32v2_version 2 de l'extension DOS de DJ Delorie
3*2Tlinux_Linux
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware Module (clib)
3*2Tnetwlibc_Novell Netware Module (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 / eComStation
3*2Tsunos_SunOS/Solaris
3*2Twatcom_DOS extender compatible avec Watcom
3*2Twdosx_WDOSX DOS extension
3*2Twin32_Windows 32 Bits
4*2Tlinux_Linux
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tlinux_Linux/m68k
6*2Tmacos_Macintosh m68k
6*2Tpalmos_PalmOS
A*2Tlinux_Linux
P*2Tdarwin_Darwin et MacOS X pour PowerPC
P*2Tlinux_Linux pour PowerPC
P*2Tmacos_MacOS (classique) pour PowerPC
P*2Tmorphos_MorphOS
S*2Tlinux_Linux
**1u<x>_rend le symbole <x> non défini
**1U_options d'unités :
**2Un_ne pas vérifier le nom de l'unité
**2Ur_générer des fichiers d'unité distribuables
**2Us_compiler en temps qu'unité système
**1v<x>_donne des informations. <x> est une combination des lettres suivantes :
**2*_e : montre les erreurs (défaut)   0 : ne montre rien (sauf les erreurs)
**2*_w : montre les avertissements     u : montre les info sur les unités
**2*_n : montre les notes              t : montre les fichiers essayés/utilisés
**2*_h : montre les suggestions        c : montre les conditionniels
**2*_i : montre des infos générales    d : informations de déboggage
**2*_l : montre les numéros de lignes  r : mode compatibilité Rhide/GCC
**2*_a : montre tout                   x : infos Executable (Win32 seulement)
**2*_b : montre le nom des fichiers avec chemin complet dans les messages
**2*_v : écrit fpcdebug.txt avec       p : écrit tree.log avec arbre d'analyse
**2*_    un maximum d'informations de débogage
3*1W<x>_options pour cibles de type Win32
3*2WB_créer une image relogeable
3*2WB<x>_définir la base de l'Image à la valeur hexadecimale <x>
3*2WC_générer une application console
3*2WD_utiliser DEFFILE pour exporter les fonction de la DLL ou de l'EXE
3*2WF_générer une application plein écran (OS/2 seulement)
3*2WG_générer une application graphique
3*2WN_ne pas générer de code relogeable (nécessaire pour le débogage)
3*2WR_générer du code relogeable
P*2WC_générer une application console (MacOS seulement)
P*2WG_générer une application graphique (MacOS seulement)
P*2WT_générer une application utilitaire (MPW tool, MacOS seulement)
**1X_options pour executable :
**2Xc_passer --shared à l'éditeur de liens (Unix seulement)
**2Xd_ne pas utiliser le chemin de recherche des bibliothèques standard (nécessaire à la compilation pour une autre cible)
**2XD_lien avec la librarie dynamique (définit FPC_LINK_DYNAMIC)
**2Xm_générer un fichier MAP
**2XM<x>_définir le nom de la procédure 'main' ('main' par défaut)
**2XP<x>_prepend the binutils names with the prefix <x>
**2Xr<x>_définir <x> comme chemin de recherche des bibliothèques (nécessaire à la compilation pour une autre cible)
**2Xs_enlêve tous les symboles de l'executable
**2XS_lien avec les librairies statiques (définit FPC_LINK_STATIC)
**2Xt_lier avec des bibliothèques statiques (-static est passé à l'éditeur de liens)
**2XX_essayer de lier les unités intelligemment (définit FPC_LINK_SMART)
**1*_
**1?_affiche cette aide
**1h_affiche cette aide sans attente
]

#
# The End...
