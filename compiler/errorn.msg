#
#   $Id$
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1998 by the Free Pascal Development team
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
#   assem_    assembler parsing
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
#

#
# General
#
general_u_compilername=U_Compiler: $1
general_d_sourceos=D_Bron OS: $1
general_i_targetos=I_Doel OS: $1
general_u_exepath=U_Programma pad: $1
general_u_unitpath=U_Units pad: $1
general_u_includepath=U_Include pad: $1
general_u_librarypath=U_Bibliotheek pad: $1
general_u_objectpath=U_Object pad: $1
general_i_abslines_compiled=I_$1 Lijnen gecompileerd, $2 sec
general_f_no_memory_left=F_Geen geheugen meer vrij

#
# Scanner
#
scan_f_end_of_file=F_Onverwacht einde van bestand
scan_f_string_exceeds_line=F_String langer dan regel
scan_f_illegal_char=F_ongeldig teken
scan_f_syn_expected=F_Taalfout: $2 verwacht in kolom $1
scan_u_start_include_file=U_Ingevoegd bestand $1 word geopend
scan_w_comment_level=W_Commentaar diepte $1 gevonden
scan_n_far_directive_ignored=N_$F directive (FAR) genegeerd
scan_n_stack_check_global_under_linux=N_Stapeloverloop test is globaal onder linux
scan_n_ignored_switch=N_Genegeerde compiler schakelaar $1
scan_w_illegal_switch=W_Foutieve compiler schakelaar $1
scan_w_switch_is_global=W_Deze compiler schakelaar heeft ook een globaal effect
scan_e_illegal_char_const=E_Foutieve constante voor een karakter
scan_f_cannot_open_input=F_kan bestand niet openen
scan_f_cannot_open_includefile=F_Kan ingevoerd bestand niet openen $1
scan_e_too_much_endifs=E_Te veel $ENDIFs of $ELSEs
scan_w_only_pack_records=W_Record velden kunnen maar op 1,2 of 4 bytes uitgelijnd worden.
scan_w_only_pack_enum=W_Enumerated kan alleen worden bewaard in 1,2 of 4 bytes
scan_e_endif_expected=E_$ENDIF verwacht voor $1 op $2 $3
scan_e_preproc_syntax_error=E_Taalfout bij het compileren van een conditionele compilatie uitdrukking
scan_e_error_in_preproc_expr=E_Evalueren van een conditionele compilatie uitdrukking
scan_w_marco_cut_after_255_chars=W_Macro inhoud is afgsneden op 255 karakters voor evaluatie
scan_e_endif_without_if=E_ENDIF zonder IF(N)DEF
scan_f_user_defined=F_Gebruiker definitie: $1
scan_e_user_defined=E_Gebruiker definitie: $1
scan_w_user_defined=W_Gebruiker definitie: $1
scan_n_user_defined=N_Gebruiker definitie: $1
scan_h_user_defined=H_Gebruiker definitie: $1
scan_i_user_defined=I_Gebruiker definitie: $1
scan_e_keyword_cant_be_a_macro=E_Sleutelwoord herdefinieren als macro heeft geen effect
scan_f_macro_buffer_overflow=F_Macro buffer overflow bij lezen of expanderen van macro
scan_w_macro_deep_ten=W_Expansie van macro meer dan 16 niveus diep, waarschijnlijk een recursieve macro
scan_e_wrong_styled_switch=E_compiler schakelaar is niet toegestaan in (* ... *) commentaar
scan_d_handling_switch=D_Behandel schakelaar "$1"
scan_c_endif_found=C_ENDIF $1 gevonden
scan_c_ifdef_found=C_IFDEF $1 gevonden, $2
scan_c_ifopt_found=C_IFOPT $1 gevonden, $2
scan_c_if_found=C_IF $1 gevonden, $2
scan_c_ifndef_found=C_IFNDEF $1 gevonden, $2
scan_c_else_found=C_ELSE $1 gevonden, $2
scan_c_skipping_until=C_Negeer tot...
scan_i_press_enter=I_Druk <return> om voort te gaan
scan_w_unsupported_switch=W_Niet ondersteunde schakelaar $1
scan_w_illegal_directive=W_Illegale compiler schakelaar $1
scan_d_back_in=D_Terug in $1
scan_w_unsupported_asmmode_specifier=W_Niet ondersteunde assembler soort gegeven $1

#
# Parser
#
parser_e_syntax_error=E_Parser - Syntax fout
parser_w_proc_far_ignored=W_Procedure type FAR genegeerd
parser_w_proc_near_ignored=W_Procedure type NEAR genegeerd
parser_e_no_dll_file_specified=E_geen DLL File opgegeven
parser_e_constructorname_must_be_init=E_Constructor naam moet INIT zijn
parser_e_destructorname_must_be_done=E_Destructor name moet DONE zijn
parser_e_illegal_open_parameter=E_Foutieve open parameter
parser_e_proc_inline_not_supported=E_Procedure type INLINE wordt niet ondersteund
parser_w_priv_meth_not_virtual=W_Private methodes zijn niet VIRTUAL
parser_w_constructor_should_be_public=E_Constructor mag niet private or protected zijn
parser_w_destructor_should_be_public=E_Destructor mag niet private or protected zijn
parser_n_only_one_destructor=N_Class heeft slechts 1 destructor
parser_e_no_local_objects=E_Lokale class definities zijn niet toegestaan
parser_e_no_anonym_objects=E_Anonieme class definities zijn niet toegestaan
parser_e_illegal_parameter_list=E_Foutieve parameter lijst
parser_e_wrong_parameter_type=E_Foutieve parameter type meegegeven
parser_e_wrong_parameter_size=E_Foutief aantal parameters meegegeven
parser_e_overloaded_no_procedure=E_overladen identifieerder is geen functie identifieerder
parser_e_overloaded_have_same_parameters=E_overladen functies hebben identieke parameterlijst
parser_e_header_dont_match_forward=E_functie declaratie klopt niet met voorwaartse declaratie $1
parser_n_duplicate_enum=N_Slechts waarden kunnen overgeslagen worden in opsommingstypes
parser_n_interface_name_diff_implementation_name=N_Interface en implementatie namen zijn verschillend
parser_e_too_much_lexlevel=E_functie meer dan 31 keer genest
parser_e_range_check_error=E_Bereikfout bij evalueren constantes
parser_e_double_caselabel=E_Dubbel case geval
parser_e_type_const_not_possible=E_Getypeerde constantes van classes zijn niet toegestaan
parser_e_no_overloaded_procvars=E_functie variabelen van meervoudige functies zijn niet toegestaan
parser_e_string_too_long=E_string lengte moet tussen 1 en 255 liggen
parser_w_use_extended_syntax_for_objects=W_Gebruik de uitgebreide syntax van DISPOSE en NEW om nieuwe instanties van classes te genereren
parser_e_class_id_expected=E_class verwacht
parser_e_methode_id_expected=E_methode verwacht
parser_e_header_dont_match_any_member=E_function declaratie komt niet overeen met een methode van deze class
parser_p_procedure_start=P_procedure/functie $1 ($2) op lijn $3
parser_e_error_in_real=E_Illegale vlottende komma constante
parser_e_fail_only_in_constructor=E_FAIL mag slechts in constructors gebruikt worden
parser_e_no_paras_for_destructor=E_Destructors hebben geen parameters
parser_e_only_class_methods_via_class_ref=E_Alleen class methodes kunnen gerefereerd met classe methodes
parser_e_only_class_methods=E_Alleen class methodes togankelijk in classen
parser_e_case_mismatch=E_Constante en CASE type komen niet overeen
parser_e_illegal_symbol_exported=E_Het symbool kan niet uitgevoerd worden van een bibliotheek
parser_w_should_use_override=E_Een virtuele methode moet met OVERRIDE overroepen worden: $1
parser_e_nothing_to_be_overridden=E_Er is geen geerfde methode die overroepen kan worden: $1
parser_e_no_procedure_to_access_property=E_Er is geen lid om de eigenschap te bereiken
parser_e_ill_property_access_sym=E_Illegaal symbool voor eigenschap toegang
parser_e_cant_access_protected_member=E_Kan niet in beschermd veld van een object schrijven
parser_e_cant_access_private_member=E_Kan niet in prive veld van een object schrijven
parser_e_overloaded_are_not_both_virtual=E_Alle overladen methodse moeten virtueel zijn als 1 virtueel is: $1
parser_e_overloaded_methodes_not_same_ret=E_Virtuele Overladen methodes moeten hetzelfde resultaat type hebben: $1
parser_e_dont_nest_export=E_EXPORT gedeclareerde functions kunnen niet genest worden.
parser_e_methods_dont_be_export=E_Methodes kunnen niet geexporteerd worden.
parser_e_call_by_ref_without_typeconv=E_Aanroep per referentie parameters moet overeenstemmende types hebben.
parser_e_no_super_class=E_Class is geen ouder class van de huidige class
parser_e_self_not_in_method=E_SELF is alleen toegestaan in methodes.
parser_e_generic_methods_only_in_methods=E_Methodes kunnen alleen in andere methodes worden aangeroepen met type identifieerder
parser_e_illegal_colon_qualifier=E_Ongeldig gebruik van ':'
parser_e_illegal_set_expr=E_Bereik fout in set constructor of dubbel set element
parser_e_pointer_to_class_expected=E_Pointer naar class verwacht
parser_e_expr_have_to_be_constructor_call=E_Uitdrukking moet een constructor call zijn.
parser_e_expr_have_to_be_destructor_call=E_Expression must be destructor call
parser_e_invalid_record_const=E_Ongeldige volgorde van record leden.
parser_e_false_with_expr=E_Type van uitdrukking moet class of record type zijn.
parser_e_void_function=E_Procedures kunnen geen resultaat teruggeven.
parser_e_constructors_always_objects=E_Constructors en Destructors moeten methodes zijn
parser_e_operator_not_overloaded=E_Operator is niet hergedefineerd
parser_e_no_reraise_possible=E_Re-raise is hier niet mogelijk
parser_e_no_new_or_dispose_for_classes=E_De uitgebreide syntax van NEW of DISPOSE is niet toegestaan voor classes
parser_e_asm_incomp_with_function_return=E_Assembler stemt niet overeen met functieresultaat type
parser_e_procedure_overloading_is_off=E_Procedure overladen is afgezet
parser_e_overload_operator_failed=E_Deze operator kan niet overladen worden.
parser_e_comparative_operator_return_boolean=E_Vergelijkings operator moet een boolean type als resultaat hebben.
parser_e_only_virtual_methods_abstract=E_Enkel virtuele methodes kunnen abstract zijn.
parser_f_unsupported_feature=F_Gebruik van niet ondersteunde optie !
parser_e_mix_of_classes_and_objects=E_CLASSES and OBJECTS kunnen niet gemengd worden.
parser_w_unknown_proc_directive_ignored=W_Ongekende procedure schakelaar is genegeerd: $1
parser_e_absolute_only_one_var=E_Absolute kan maar aan een variabele verbonden worden.
parser_e_absolute_only_to_var_or_const=E_Absolute kan slechts aan variabelen of constantes verbonden worden.
parser_e_abstract_no_definition=E_Abtracte methodes kunnen niet gedefineerd worden.
parser_e_overloaded_must_be_all_global=E_Deze overladen procedure kan niet lokaal zijn, (moet geexporteerd worden)
parser_w_virtual_without_constructor=W_Gebruike van Virtuele methods zonder constructor
parser_m_macro_defined=M_Macro gedefinieerd: $1
parser_m_macro_undefined=M_Macro ongedefinieerd: $1
parser_m_macro_set_to=M_Macro $1 wordt $2
parser_i_compiling=I_Compilatie van $1
parser_d_compiling_second_time=D_Compilatie van $1 voor de tweede keer.
parser_e_no_paras_allowed=E_Array eigenschappen zijn hier niet toegestaan.
parser_e_no_property_found_to_override=E_Geen eigenschap gevonden om te herdefinieren
parser_e_only_one_default_property=E_Slechts 1 default eigenschap is toegestaan, geerfde default eigenschap in kind gevonden.
parser_e_property_need_paras=E_Default eigenschappen moeten van type Array zijn.
parser_e_constructor_cannot_be_not_virtual=E_Virtuele constructors worden alleen ondersteunt in het object model
parser_e_no_default_property_available=E_Geen standaard eigenschap gevonden
parser_e_cant_have_published=E_De class kan geen gepubliceerde sectie hebben, gebruik {$M+}
parser_e_forward_declaration_must_be_resolved=E_Declaratie van class $1 moet worden opgelost om de class als ouder te gebruiken
parser_e_no_local_operator=E_Lokake operatoren worden niet ondersteunt
parser_e_proc_dir_not_allowed_in_interface=E_Procedure directive $1 niet toegestaan in interface sectie
parser_e_proc_dir_not_allowed_in_implementation=E_Procedure directive $1 niet toegestaan in implementation sectie
parser_e_proc_dir_not_allowed_in_procvar=E_Procedure directive $1 niet toegestaan in procvar declaratie
parser_e_function_already_declared_public_forward=E_Functie is al publiek/forward gedeclareerd $1
parser_e_not_external_and_export=E_Kan niet beide EXPORT en EXTERNAL gebruiken
parser_e_name_keyword_expected=E_NAME sleutelwoord verwacht
parser_w_not_supported_for_inline=W_$1 wordt niet ondersteund voor inline procedure/function
parser_w_inlining_disabled=W_Inlining uitgeschakeld
parser_i_writing_browser_log=I_Schrijven van Browser logfile $1
parser_h_maybe_deref_caret_missing=H_misschien is het een wijzer, ^ wordt dan gemist
parser_f_assembler_reader_not_supported=F_Geselecteerde assembler lezer wordt niet ondersteund
parser_e_proc_dir_conflict=E_Procedure directive $1 maakt conflicten met andere directives
parser_e_call_convention_dont_match_forward=E_Calling convention doesn't match forward
parser_e_register_calling_not_supported=E_Register calling (fastcall) wordt niet ondersteund

#
# Type Checking
#
type_e_mismatch=E_Types stemmen niet overeen
type_e_integer_expr_expected=E_Integer expressie verwacht
type_e_ordinal_expr_expected=E_Ordinal expressie verwacht
type_e_type_id_expected=E_Type identifieerder verwacht
type_e_variable_id_expected=E_Variabel identifier verwacht
type_e_pointer_type_expected=E_Pointer type verwacht
type_e_class_type_expected=E_class type verwacht
type_e_varid_or_typeid_expected=E_Variable of type indentifieerder verwacht
type_e_cant_eval_constant_expr=E_Kan constante expressie niet evaluaren
type_e_set_element_are_not_comp=E_Elementen van set zijn incompatible
type_w_convert_real_2_comp=W_Type conversion van floating type to COMP, hetgeen een integer type is.
type_w_int_slash_int=W_De operator / geeft een resultaat is van type real
type_h_use_div_for_int=H_Gebruik DIV voor integers.
type_e_strict_var_string_violation=E_String types stemmen niet overeen, wegens $V+ modus
type_e_succ_and_pred_enums_with_assign_not_possible=E_SUCC of PRED op enumeratie types met toekenningen zijn niet mogelijk
type_e_cant_read_write_type=E_Kan niet lezen of schrijven met dit type variabele
type_e_typeconflict_in_set=E_Type conflict tussen set elementen.

#
# Symtable
#
sym_e_id_not_found=E_Identifieerder niet gevonden $1
sym_f_internal_error_in_symtablestack=F_Interne fout in SymTableStack()
sym_e_duplicate_id=E_Dubbele identifieerder $1
sym_e_unknown_id=E_Onbekende identifieerder $1
sym_e_forward_not_resolved=E_Voorwaartse declaratie niet opgelost $1
sym_f_id_already_typed=F_Type Identifieerder reeds gedefineerd als type
sym_e_error_in_type_def=E_Fout in type defenitie
sym_e_type_id_not_defined=E_Type identifieerder niet gedefinieerd
sym_e_only_static_in_static=E_Alleen statische variabelen kunnen gebruikt worden in statische methodes
sym_e_invalid_call_tvarsymmangledname=E_Ongeldige aanroep van tvarsym.mangledname()
sym_f_type_must_be_rec_or_class=F_record of class type verwacht
sym_e_no_instance_of_abstract_object=E_U kan geen vertegenwoordiger van een klasse met abstracte methodes instantieren
sym_e_label_not_defined=E_Label niet gedefineerd $1
sym_e_ill_label_decl=E_Label foutief gedefineerd
sym_e_goto_and_label_not_supported=E_GOTO en LABEL zijn momenteel uitgeschakeld (gebruik -Sg)
sym_e_label_not_found=E_Label niet gevonden
sym_e_id_is_no_label_id=E_identifier is geen label
sym_e_label_already_defined=E_label is al gedefineerd
sym_e_ill_type_decl_set=E_Type declaratie van set elementen is foutief
sym_e_class_forward_not_resolved=E_Voorwaartse definitie van klasse niet opgelost $1
sym_n_para_identifier_not_used=H_Parameter niet gebruikt $1
sym_w_local_identifier_not_used=W_Lokale variable niet gebruikt $1
sym_e_set_expected=E_Set type verwacht
sym_w_function_result_not_set=W_Resultaat van functie ogeschijnlijk niet present
sym_e_illegal_field=E_Onbekend record lid
sym_n_uninitialized_local_variable=N_Locale variabele $1 schijnt niet geinitialiseert te zijn
sym_e_id_no_member=E_Geen lid met de naam $1

#
# Codegenerator
#
cg_e_break_not_allowed=E_BREAK niet toegestaan
cg_e_continue_not_allowed=E_CONTINUE niet toegestaan
cg_e_too_complex_expr=E_Uitdrukking te ingewikkeld - overloop van wiskundige processor
cg_e_illegal_expression=E_Foutieve expressie
cg_e_invalid_integer=E_Foutieve integer
cg_e_invalid_qualifier=E_Foutieve qualifier
cg_e_upper_lower_than_lower=E_Bovengrens < ondergrens
cg_e_illegal_count_var=E_Foutive lusteller
cg_e_cant_choose_overload_function=E_Weet niet welke meervoudige procedure ik moet aanroepen
cg_e_parasize_too_big=E_Parameters gebruiken meer dan 64 kilobyte
cg_e_illegal_type_conversion=E_Foute type conversie
cg_e_file_must_call_by_reference=E_Files moeten var parameters zijn
cg_e_cant_use_far_pointer_there=E_U kunt daar geen far pointer gebruiken
cg_e_var_must_be_reference=E_Ongeldige aanroep met referentie parameters
cg_e_dont_call_exported_direct=E_procedures die EXPORT gedeclareerd staan kunnen niet aangeroepen worden
cg_w_member_cd_call_from_method=W_Mogelijke illegale aanroep van constructor of destructor
cg_n_inefficient_code=N_Inefficiente code
cg_w_unreachable_code=W_Deze code wordt nooit uitgevoerd
cg_e_stackframe_with_esp=E_procedure aanroep met stackframe ESP/SP
cg_e_cant_call_abstract_method=E_Abstracte methodes kunnen niet direct aangeroepen worden.
cg_f_internal_error_in_getfloatreg=F_Interne fout in getfloatreg(), allocatie faling
cg_f_unknown_float_type=F_Onbekend vlottende komma type
cg_f_secondvecn_base_defined_twice=F_SecondVecn() basis tweemaal gedefinieerd.
cg_f_extended_cg68k_not_supported=F_Extended cg68k niet ondersteund
cg_f_32bit_not_supported_in_68000=F_32-bit unsigned niet ondersteund in MC68000 modus
cg_f_internal_error_in_secondinline=F_Interne fout in secondinline()
cg_d_register_weight=D_Register $1 gewicht $2 $3
cg_e_stacklimit_in_local_routine=E_Stack limiet excedeed in local routine
cg_d_stackframe_omited=D_Stack frame wordt niet gemaakt
cg_e_unable_inline_object_methods=E_Kan niet inline gebruiken voor object methodes
cg_e_unable_inline_procvar=E_Kan niet inline gebruiken voor procvar aanroepen
cg_e_no_code_for_inline_stored=E_Geen code voor inline procedure opgeslagen

#
# Assembler reader/writers
#
assem_f_ev_zero_divide=F_Delen door nul in asm evaluator
assem_f_ev_stack_overflow=F_Evaluator stack overloop
assem_f_ev_stack_underflow=F_Evaluator stack leegloop
assem_f_ev_invalid_number=F_Invalid numeric format in asm evaluator
assem_f_ev_invalid_op=F_Invalid Operator in asm evaluator
assem_f_ev_unknown=F_Unknown error in asm evaluator
assem_w_invalid_numeric=W_Ongeldige numerieke waarde
assem_e_escape_seq_ignored=E_escape sequence ignored $1
assem_e_syn_prefix_not_found=E_Asm syntax error - Prefix not found
assem_e_syn_try_add_more_prefix=E_Asm syntax error - Trying to add more than one prefix
assem_e_syn_opcode_not_found=E_Asm syntax error - Opcode not found
assem_e_invalid_symbol_ref=E_Invalid symbol reference
assem_w_calling_overload_func=W_Calling an overload function in an asm
assem_e_constant_out_of_bounds=E_Constant value out of bounds
assem_e_none_label_contain_at=E_Non-label pattern contains @
assem_e_invalid_operand=E_Invalid Operand: $1
assem_w_override_op_not_supported=W_Override operator not supported
assem_e_error_in_binary_const=E_Error in binary constant: $1
assem_e_error_in_octal_const=E_Error in octal constant: $1
assem_e_error_in_hex_const=E_Error in hexadecimal constant: $1
assem_e_error_in_integer_const=E_Error in integer constant: $1
assem_e_invalid_labeled_opcode=E_Invalid labeled opcode
assem_f_internal_error_in_findtype=F_Internal error in Findtype()
assem_e_invalid_size_movzx=E_Invalid size for MOVSX/MOVZX
assem_e_16bit_base_in_32bit_segment=E_16-bit base in 32-bit segment
assem_e_16bit_index_in_32bit_segment=E_16-bit index in 32-bit segment
assem_e_invalid_opcode=E_Invalid Opcode
assem_e_const_ref_not_allowed=E_Constant reference not allowed
assem_w_fwait_emu_prob=W_Fwait can cause emulation problems with emu387
assem_e_invalid_opcode_and_operand=E_Invalid combination of opcode and operands
assem_w_opcode_not_in_table=W_Opcode $1 not in table, operands not checked
assem_f_internal_error_in_concatopcode=F_Internal Error in ConcatOpcode()
assem_e_invalid_size_in_ref=E_Invalid size in reference
assem_e_invalid_middle_sized_operand=E_Invalid middle sized operand
assem_e_invalid_three_operand_opcode=E_Invalid three operand opcode
assem_e_syntax_error=E_Assembler syntax error
assem_e_invalid_operand_type=E_Invalid operand type
assem_e_segment_override_not_supported=E_Segment overrides not supported
assem_e_invalid_const_symbol=E_Invalid constant symbol $1
assem_f_error_converting_bin=F_Internal Errror converting binary
assem_f_error_converting_hex=F_Internal Errror converting hexadecimal
assem_f_error_converting_octal=F_Internal Errror converting octal
assem_e_invalid_constant_expression=E_Invalid constant expression
assem_e_unknown_id=E_Unknown identifier: $1
assem_e_defining_index_more_than_once=E_Trying to define an index register more than once
assem_e_invalid_field_specifier=E_Invalid field specifier
assem_f_internal_error_in_buildscale=F_Internal Error in BuildScaling()
assem_e_invalid_scaling_factor=E_Invalid scaling factor
assem_e_invalid_scaling_value=E_Invalid scaling value
assem_e_scaling_value_only_allowed_with_index=E_Scaling value only allowed with index
assem_e_syn_no_ref_with_brackets=E_Invalid assembler syntax. No ref with brackets)
assem_e_expression_form_not_supported=E_Expressions of the form [sreg:reg...] are currently not supported
assem_e_defining_seg_more_than_once=E_Trying to define a segment register twice
assem_e_defining_base_more_than_once=E_Trying to define a base register twice
assem_e_negative_index_register=E_Trying to use a negative index register
assem_e_syn_reference=E_Asm syntax error - error in reference
assem_e_local_symbol_not_allowed_as_ref=E_Local symbols not allowed as references
assem_e_invalid_operand_in_bracket_expression=E_Invalid operand in bracket expression
assem_e_invalid_symbol_name=E_Invalid symbol name: $1
assem_e_invalid_reference_syntax=E_Invalid Reference syntax
assem_e_invalid_string_as_opcode_operand=E_Invalid string as opcode operand: $1
assem_w_CODE_and_DATA_not_supported=W_@CODE and @DATA not supported
assem_e_null_label_ref_not_allowed=E_Null label references are not allowed
assem_w_calling_overloaded=W_Calling of an overloaded function in direct assembler
assem_e_cannot_use_SELF_outside_a_method=E_Cannot use SELF outside a method
assem_e_syn_start_with_bracket=E_Asm syntax error - Should start with bracket
assem_e_syn_register=E_Asm syntax error - register: $1
assem_e_SEG_and_OFFSET_not_supported=E_SEG and OFFSET not supported
assem_e_syn_opcode_operand=E_Asm syntax error - in opcode operand
assem_e_invalid_string_expression=E_Invalid String expression
assem_e_expression_out_of_bounds=E_Constant expression out of bounds
assem_f_internal_error_in_buildconstant=F_Internal Error in BuildConstant()
assem_w_repeat_prefix_and_seg_override=W_A repeat prefix and a segment override on <= i386 may result in errors if an interrupt occurs
assem_e_invalid_or_missing_opcode=E_Invalid or missing opcode
assem_e_invalid_prefix_and_opcode=E_Invalid combination of prefix and opcode: $1
assem_e_invalid_override_and_opcode=E_Invalid combination of override and opcode: $1
assem_e_too_many_operands=E_Too many operands on line
assem_e_dup_local_sym=E_Duplicate local symbol: $1
assem_e_unknown_label_identifer=E_Unknown label identifer: $1
assem_e_assemble_node_syntax_error=E_Assemble node syntax error
assem_e_unknown_local_sym=E_Undefined local symbol: $1
assem_d_start_intel=D_Starting intel styled assembler parsing...
assem_d_finish_intel=D_Finished intel styled assembler parsing...
assem_e_not_directive_or_local_symbol=E_Not a directive or local symbol: $1
assem_e_slash_at_begin_of_line_not_allowed=E_/ at beginning of line not allowed
assem_e_nor_not_supported=E_NOR not supported
assem_e_invalid_fpu_register=E_Invalid floating point register name
assem_w_modulo_not_supported=W_Modulo not supported
assem_e_invalid_float_const=E_Invalid floating point constant: $1
assem_e_size_suffix_and_dest_reg_dont_match=E_Size suffix and destination register do not match
assem_f_internal_error_in_concatlabeledinstr=E_Internal error in ConcatLabeledInstr()
assem_w_float_bin_ignored=W_Floating point binary representation ignored
assem_w_float_hex_ignored=W_Floating point hexadecimal representation ignored
assem_w_float_octal_ignored=W_Floating point octal representation ignored
assem_e_invalid_real_const=E_Invalid real constant expression
assem_e_parenthesis_are_not_allowed=E_Parenthesis are not allowed
assem_e_invalid_reference=E_Invalid Reference
assem_e_cannot_use___SELF_outside_methode=E_Cannot use __SELF outside a method
assem_e_cannot_use___OLDEBP_outside_nested_procedure=E_Cannot use __OLDEBP outside a nested procedure
assem_w_id_supposed_external=W_Identifier $1 supposed external
assem_e_invalid_seg_override=E_Invalid segment override expression
assem_e_string_not_allowed_as_const=E_Strings not allowed as constants
assem_d_start_att=D_Starting AT&T styled assembler parsing...
assem_d_finish_att=D_Finished AT&T styled assembler parsing...
assem_e_switching_sections_not_allowed=E_Switching sections is not allowed in an assembler block
assem_e_invalid_global_def=E_Invalid global definition
assem_e_line_separator_expected=E_Line separator expected
assem_w_globl_not_supported=W_globl not supported
assem_w_align_not_supported=W_align not supported
assem_w_lcomm_not_supported=W_lcomm not supported
assem_w_comm_not_supported=W_comm not supported
assem_e_invalid_lcomm_def=E_Invalid local common definition
assem_e_invalid_comm_def=E_Invalid global common definition
assem_e_local_sym_not_found_in_asm_statement=E_local symbol: $1 not found inside asm statement
assem_e_assembler_code_not_returned_to_text=E_assembler code not returned to text
assem_f_internal_error_in_buildreference=F_internal error in BuildReference()
assem_e_invalid_opcode_size=E_invalid opcode size
assem_w_near_ignored=W_NEAR ignored
assem_w_far_ignored=W_FAR ignored
assem_d_creating_lookup_tables=D_Creating inline asm lookup tables
assem_w_using_defined_as_local=W_Using a defined name as a local label
assem_f_internal_error_in_handleextend=F_internal error in HandleExtend()
assem_e_invalid_char_smaller=E_Invalid character: <
assem_e_invalid_char_greater=E_Invalid character: >
assem_e_unsupported_opcode=E_Unsupported opcode
assem_e_no_inc_and_dec_together=E_Increment and Decrement mode not allowed together
assem_e_invalid_reg_list_in_movem=E_Invalid Register list in movem/fmovem
assem_e_invalid_reg_list_for_opcode=E_Invalid Register list for opcode
assem_e_68020_mode_required=E_68020+ mode required to assemble
assem_d_start_motorola=D_Starting Motorola styled assembler parsing...
assem_d_finish_motorola=D_Finished Motorola styled assembler parsing...
assem_w_xdef_not_supported=W_XDEF not supported
assem_w_void_function=W_Functions with void return value can't return any value in asm code
assem_f_invalid_suffix_intel=E_Invalid suffix for intel assembler
assem_e_extended_not_supported=E_Extended not supported in this mode
assem_e_comp_not_supported=E_Comp not supported in this mode
assem_w_mmxwarning_as_281=W_You need GNU as version >= 2.81 to compile this MMX code
assem_f_too_many_asm_files=F_Teveel assembler bestanden
assem_f_assembler_output_not_supported=F_Geselecteerde assembler schrijven wordt niet ondersteund
assem_e_unsupported_symbol_type=E_Unsupported symbol type for operand

#
# Executing linker/assembler
#
exec_i_assembling_pipe=I_Assembleren (pijp) van $1
exec_d_cant_create_asmfile=E_Kan geen assembler bestand $1 niet maken
exec_w_assembler_not_found=W_Assembleerder $1 niet gevonden, overschakeling naar externe assemblage
exec_u_using_assembler=U_Gebruikte assembler: $1
exec_w_error_while_assembling=W_Fout bij assembleren
exec_w_cant_call_assembler=W_Kan assembleerder niet oproepen, overschakeling naar externe assemblage
exec_i_assembling=I_Assembleren van $1
exec_w_linker_not_found=W_Linker $1 niet gevonden, oveschakeling naar externe linker
exec_u_using_linker=U_Gebruikte linker: $1
exec_w_objfile_not_found=E_Bestand $1 niet gevonden, linken kan foutlopen !
exec_w_libfile_not_found=E_Bibliotheek $1 niet gevonden, linken kan foutlopen !
exec_w_error_while_linking=W_Error tijdens linken
exec_w_cant_call_linker=W_Kan linker niet oproepen, overschakeling naar extern linken
exec_i_linking=I_Linken van $1
exec_w_binder_not_found=W_binder niet gevonden, overschakeling naar extern binden.
exec_w_ar_not_found=W_ar niet gevonden, overschakeling naar externe ar
exec_e_dll_not_supported=E_Dynamische bibliotheken niet ondersteund
exec_i_closing_script=I_Afsluiten batch $1

#
# Unit loading
#
unit_u_ppu_loading=U_PPU Laden $1
unit_d_ppu_time=D_PPU Tijd: $1
unit_d_ppu_file_too_short=D_PPU bestand te kort
unit_d_ppu_invalid_header=D_PPU Ongeldige hoofding (geen PPU aan het begin)
unit_d_ppu_invalid_version=D_PPU Ongeldige Versie $1
unit_d_ppu_flags=D_PPU Vlaggen: $1
unit_d_ppu_crc=D_PPU CRC: $1
unit_t_ppu_source=T_PPU Bronbestand: $1
unit_d_obj_and_asm_are_older_than_ppu=D_Objectbestand en assemblerbestand zijn ouder als PPU bestand
unit_d_obj_is_older_than_asm=D_Objectbestand is ouder dan assembler bestand.
unit_t_unitsearch=T_Unit zoeken: $1
unit_u_ppu_write=U_Schrijven van $1
unit_f_ppu_cannot_write=F_Kan geen PPU bestand schrijven
unit_f_ppu_read_error=F_Lezen van PPU bestand
unit_f_ppu_invalid_entry=F_Ongeldig PPU bestand post: $1
unit_f_ppu_dbx_count_problem=F_PPU Dbx aantal probleem
unit_e_illegal_unit_name=E_Ongeldige unit naam: $1
unit_f_too_much_units=F_Te veel units
unit_f_circular_unit_reference=F_Circulaire unit referentie
unit_f_cant_compile_unit=F_Kan unit $1 niet compileren, geen broncode beschikbaar
unit_w_switch_us_missed=W_De systeem unit compileren vereist de -Us schakelaar
unit_f_errors_in_unit=F_Er waren fouten $1 bij het compileren van een module, compilatie gestopt.

#
#  Optie
#
option_usage=$1 [opties] <bestand> [opties]
option_only_one_source_support=Maar een bronbestand wordt ondersteunt
option_def_only_for_os2=DEF file kan alleen worden gemaakt voor OS/2
option_no_nested_response_file=nested response files are not supported
option_no_source_found=No source file name in command line
option_illegal_para=Illegal parameter: $1
option_help_pages_para=-? writes help pages
option_too_many_cfg_files=Too many config files nested
option_unable_open_file=Unable to open file $1
option_reading_further_from=Reading further options from $1
option_target_is_already_set=Target is already set to: $1
option_no_shared_lib_under_dos=Shared libs not supported on DOS platform, reverting to static
option_too_many_ifdef=too many IF(N)DEFs
option_too_many_endif=too many ENDIFs
option_too_less_endif=open conditional at the end of the file
option_no_debug_support=Debug information generation is not supported by this executable
option_no_debug_support_recompile_fpc=Try recompiling with -dGDB

#
# Logo (option -l)
#
option_logo_start=Free Pascal Compiler version $FPCVER [$FPCDATE] for $FPCTARGET
option_logo_end=Copyright (c) 1993-98 by Florian Klaempfl

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
option_info_end=                fpc-devel@mail.tolna.hungary.net

#
# Help pages (option -? and -h)
#
# note: the ordering of the enum numbering doens't matter
#
ol_begin=**0*_+ switch option on, - off
ol001=**1a_the compiler doesn't delete the generated assembler file
ol120=**2al_list sourcecode lines in assembler file  (still BETA !!)
ol002=*t1b_use EMS
ol003=**1B_build all modules
ol004=**1C_code generation options
ol005=3*2CD_create dynamic library
ol006=**2Ch<n>_<n> bytes heap (between 1023 and 67107840)
ol007=**2Ci_IO-checking
ol008=**2Cn_omit linking stage
ol009=**2Co_check overflow of integer operations
ol010=**2Cr_range checking
ol011=**2Cs<n>_set stack size to <n>
ol012=**2Ct_stack checking
ol013=3*2CS_create static library
ol014=3*2Cx_use smartlinking
ol015=**1d<x>_defines the symbol <x>
ol016=*O1D_generate a DEF file
ol017=*O2Dd<x>_set description to <x>
ol018=*O2Dw_PM application
ol019=**1e<x>_set path to executable
ol020=**1E_same as -Cn
ol021=**1F_set file names and paths
ol022=**2Fe<x>_redirect error output to <x>
ol023=*L2Fg<x>_same as -Fl
ol024=**2Fi<x>_adds <x> to include path
ol025=**2Fl<x>_adds <x> to library path
ol026=*L2FL<x>_uses <x> as dynamic linker
ol027=**2Fo<x>_adds <x> to object path
ol028=**2Fr<x>_load error message file <x>
ol029=**2Fu<x>_adds <x> to unit path
ol030=*g1g_generate debugger information
ol031=*g2gg_use gsym
ol032=*g2gd_use dbx
ol033=**1i_information
ol034=**1I<x>_adds <x> to include path
ol035=**1k<x>_Pass <x> to the linker
ol036=**1l_write logo
ol037=**1n_don't read the default config file
ol038=**1o<x>_change the name of the executable produced to <x>
ol039=**1pg_generate profile code for gprof
ol040=*L1P_use pipes instead of creating temporary assembler files
ol041=**1S_syntax options
ol042=**2S2_switch some Delphi 2 extensions on
ol043=**2Sc_supports operators like C (*=,+=,/= and -=)
ol044=**2Sd_compiler disposes asm lists (uses less memory but slower)
ol045=**2Se_compiler stops after the first error
ol046=**2Sg_allow LABEL and GOTO
ol047=**2Si_support C++ stlyed INLINE
ol048=**2Sm_support macros like C (global)
ol049=**2So_tries to be TP/BP 7.0 compatible
ol050=**2Sp_tries to be gpc compatible
ol051=**2Ss_constructor name must be init (destructor must be done)
ol052=**2St_allow static keyword in objects
ol053=**2Sv_allow variable directives (cvar,external,public,export)
ol054=**1s_don't call assembler and linker (only with -a)
ol055=**1T<x>_Target operating system
ol056=3*2TGO32V1_version 1 of DJ Delorie DOS extender
ol057=3*2TGO32V2_version 2 of DJ Delorie DOS extender
ol058=3*2TLINUX_Linux
ol059=3*2TOS2_OS/2 2.x
ol060=3*2TWin32_Windows 32 Bit
ol061=6*2TAMIGA_Commodore Amiga
ol062=6*2TATARI_Atari ST/STe/TT
ol063=6*2TMACOS_Macintosh m68k
ol064=6*2TLINUX_Linux-68k
ol065=**1u<x>_undefines the symbol <x>
ol066=**1U_unit options
ol067=**2Un_don't check the unit name
ol068=**2Up<x>_same as -Fu<x>
ol069=**2Us_compile a system unit
ol070=**1v<x>_Be verbose. <x> is a combination of the following letters :
ol071=**2*_e : Show errors (default)       d : Show debug info
ol072=**2*_w : Show warnings               u : Show used files
ol073=**2*_n : Show notes                  t : Show tried files
ol074=**2*_h : Show hints                  m : Show defined macros
ol075=**2*_i : Show general info           p : Show compiled procedures
ol076=**2*_l : Show linenumbers            c : Show conditionals
ol077=**2*_a : Show everything             0 : Show nothing (except errors)
ol078=**1X_executable options
ol079=*L2Xc_link with the c library
ol080=**2XD_link with dynamic libraries (defines FPC_LINK_DYNAMIC)
ol081=**2Xs_strip all symbols from executable
ol082=**2XS_link with static libraries (defines FPC_LINK_STATIC)
ol083=**0*_Processor specific options:
ol084=3*1A_output format
ol085=3*2Ao_coff file using GNU AS
ol086=3*2Anasmcoff_coff file using Nasm
ol087=3*2Anasmelf_elf32 (linux) file using Nasm
ol088=3*2Anasmobj_obj file using Nasm
ol089=3*2Amasm_obj using Masm (Mircosoft)
ol090=3*2Atasm_obj using Tasm (Borland)
ol091=3*1R_assembler reading style
ol092=3*2Ratt_read AT&T style assembler
ol093=3*2Rintel_read Intel style assembler
ol094=3*2Rdirect_copy assembler text directly to assembler file
ol095=3*1O<x>_optimizations
ol096=3*2Og_generate smaller code
ol097=3*2OG_generate faster code (default)
ol098=3*2Or_keep certain variables in registers (still BUGGY!!!)
ol099=3*2Ou_enable uncertain optimizations (see docs)
ol100=3*2O1_level 1 optimizations (quick optimizations)
ol101=3*2O2_level 2 optimizations (-O1 + slower optimizations)
ol102=3*2O3_level 3 optimizations (same as -O2u)
ol103=3*2Op_target processor
ol104=3*3Op1_set target processor to 386/486
ol105=3*3Op2_set target processor to Pentium/PentiumMMX (tm)
ol106=3*3Op3_set target processor to PPro/PII/c6x86/K6 (tm)
ol107=6*1A_output format
ol108=6*2Ao_Unix o-file using GNU AS
ol109=6*2Agas_GNU Motorola assembler
ol110=6*2Amit_MIT Syntax (old GAS)
ol111=6*2Amot_Standard Motorola assembler
ol112=6*1O_optimizations
ol113=6*2Oa_turn on the optimizer
ol114=6*2Og_generate smaller code
ol115=6*2OG_generate faster code (default)
ol116=6*2Ox_optimize maximum (still BUGGY!!!)
ol117=6*2O2_set target processor to a MC68020+
ol118=**1*_
ol119=**1?_shows this help
ol_end=**1h_shows this help without waiting

#
# Het einde...
#

