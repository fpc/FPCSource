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
general_t_compilername=T_Compiler: $1
general_d_sourceos=D_Bron OS: $1
general_i_targetos=I_Doel OS: $1
general_t_exepath=T_Programma pad: $1
general_t_unitpath=U_Units pad: $1
general_t_includepath=T_Include pad: $1
general_t_librarypath=T_Bibliotheek pad: $1
general_t_objectpath=T_Object pad: $1
general_i_abslines_compiled=I_$1 Lijnen gecompileerd, $2 sec
general_f_no_memory_left=F_Geen geheugen meer vrij

#
# Scanner
#
scan_f_end_of_file=F_Onverwacht einde van bestand
scan_f_string_exceeds_line=F_String langer dan regel
scan_f_illegal_char=F_ongeldig teken
scan_f_syn_expected=F_Taalfout: $2 verwacht in kolom $1
scan_t_start_include_file=T_Ingevoegd bestand $1 word geopend
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
scan_t_back_in=T_Terug in $1
scan_w_unsupported_asmmode_specifier=W_Niet ondersteunde assembler soort gegeven $1
scan_e_wrong_switch_toggle=E_Verkeerde switch schakelaar, gebruik ON/OFF of +/-
scan_e_resourcefiles_not_supported=E_Resource bestanden zijn niet ondersteund op doel OS
scan_w_app_type_not_support=W_$APPTYPE niet ondersteund op doel OS
scan_w_no_asm_reader_switch_inside_asm=W_ASM lezer schakelaar is niet mogelijk in een asm instructie, $1 ingesteld voor volgende blok.scan_w_unsupported_app_type=W_Unsupported application type: $1
scan_w_unsupported_app_type=W_applocatie type niet ondersteund: $1


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
parser_f_no_anonym_objects=F_Anonieme class definities zijn niet toegestaan
parser_e_illegal_parameter_list=E_Foutieve parameter lijst
parser_e_wrong_parameter_type=E_Foutieve parameter type meegegeven arg #$1
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
parser_e_string_larger_array=E_string lengte moet tussen 1 en 255 liggen
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
parser_w_stored_not_implemented=W_Stored prorperty directive is not yet implemented
parser_e_ill_property_access_sym=E_Illegaal symbool voor eigenschap toegang
parser_e_cant_access_protected_member=E_Kan niet in beschermd veld van een object schrijven
parser_e_cant_access_private_member=E_Kan niet in prive veld van een object schrijven
parser_w_overloaded_are_not_both_virtual=W_Alle overladen methodse moeten virtueel zijn als 1 virtueel is: $1
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
parser_e_call_convention_dont_match_forward=E_Aanroep conventie klopt niet met voorwaartse
parser_e_register_calling_not_supported=E_Register aanroep (fastcall) wordt niet ondersteund
parser_e_property_cant_have_a_default_value=E_Eigenschap kan geen standaard waarde hebben
parser_e_property_default_value_must_const=E_De standaard waarde van een eigenschap moet een constant zijn
parser_e_cant_publish_that=E_Symbool kan niet worden gepubliceerd, alleen mogelijk voor een klasse
parser_e_cant_publish_that_property=E_Deze soort eigenschap kan niet worden gepubliceerd
parser_e_array_lower_less_than_upper_bound=E_Bovengrens van bereik is lager dan ondergrens
parser_e_case_lower_less_than_upper_bound=E_Bovengrens van case bereik is lager dan ondergrens
parser_e_division_by_zero=E_Deling door nul
parser_e_duplicate_message_label=E_Dubbele message label: %1
parser_e_empty_import_name=W_Lege import naam opgegeven
parser_e_export_invalid_index=E_Ongeldige index for geexporteerde functie
parser_e_export_name_double=E_Dubbele geexporteerde functie naam $1
parser_e_export_ordinal_double=E_Dubbele geexporteerde functie index $1
parser_e_header_different_var_names=E_function hoofding $1 komt niet overeen met voorwaartse: variabele naam veranderd $2 => $3
parser_e_ill_msg_expr=E_Ongeldige uitdrukking achter message schakelaar
parser_e_ill_msg_param=E_Message handlers aanvaarden slechts 1 variabele paameter
parser_e_initialized_only_one_var=E_Slechts 1 variabele kan geinitialiseerd worden.
parser_e_invalid_string_size=E_string lengte moet tussen 1 en 255 liggen
parser_w_no_new_dispose_on_void_pointers=W_Het gebruik van NEW of DISPOSE voor pointer zonder type is zinloos
parser_e_no_new_dispose_on_void_pointers=E_Het gebruik van NEW of DISPOSE is niet mogelijk voor pointers zonder type
parser_e_no_object_override=E_OVERRIDE kan niet gebruikt worden in objecten
parser_e_no_type_not_allowed_here=E_type identifier is hier niet toegestaan.
parser_e_no_with_for_variable_in_other_segments=E_With kan niet gebruikt worden voor variabelen in verschillende segmenten.
parser_e_self_in_non_message_handler=E_Self kan alleen een explicite parameter zijn bij string message handlers
parser_e_threadvars_only_sg=E_Thread variabelen kunnen alleen globaal of statisch zijn.
parser_e_used_proc_name_changed=E_Interne functienaam gewijzigd na declaratie.
parser_f_direct_assembler_not_allowed=F_Directe assembler is niet ondersteund bij directe assembler schrijver.
parser_object_has_no_vmt=E_Het object $1 heeft geen VMT
parser_w_empty_import_name=W_Lege import naam opgegeven.
parser_w_no_objpas_use_mode=W_De OBJPAS unit niet manueel laden, gebruik {$mode objfpc} of {$mode delphi}.
parser_w_overloaded_are_not_both_non_virtual=W_overloaded methode van een niet virtuele methode moet niet-virtueel zijn: $1
parser_w_proc_register_ignored=W_Procedure type REGISTER wordt genegeerd
parser_w_range_check_error=W_Bereik controle fout bij evalueren constantes.
parser_e_invalid_float_operation=E_Ongeldige vlottende komma bewerking.

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
type_h_use_div_for_int=H_Gebruik DIV voor integers.
type_e_strict_var_string_violation=E_String types stemmen niet overeen, wegens $V+ modus
type_e_succ_and_pred_enums_with_assign_not_possible=E_SUCC of PRED op enumeratie types met toekenningen zijn niet mogelijk
type_e_cant_read_write_type=E_Kan niet lezen of schrijven met dit type variabele
type_e_typeconflict_in_set=E_Type conflict tussen set elementen.
type_e_integer_or_real_expr_expected=E_Integer of real expressie verwacht
type_e_incompatible_types=E_Incompatibele types: kreeg $1, verwachtte $2
type_e_set_operation_unknown=E_Operatie niet ondersteund voor sets
type_e_wrong_parameter_type=E_Incompatibel type voor arg #$1: kreeg $2, verwachtte $3
type_e_wrong_type_in_array_constructor=E_Verkeerd type in array constructor
type_w_maybe_wrong_hi_lo=W_lo/hi(longint/dword) geeft hoog/laag woord terug.

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
sym_w_label_not_defined=W_Label niet gedefineerd $1
sym_e_ill_label_decl=E_Label foutief gedefineerd
sym_e_goto_and_label_not_supported=E_GOTO en LABEL zijn momenteel uitgeschakeld (gebruik -Sg)
sym_e_label_not_found=E_Label niet gevonden
sym_e_id_is_no_label_id=E_identifier is geen label
sym_e_label_already_defined=E_label is al gedefineerd
sym_e_ill_type_decl_set=E_Type declaratie van set elementen is foutief
sym_e_class_forward_not_resolved=E_Voorwaartse definitie van klasse niet opgelost $1
sym_h_para_identifier_not_used=H_Parameter niet gebruikt $1
sym_e_set_expected=E_Set type verwacht
sym_w_function_result_not_set=W_Resultaat van functie ogeschijnlijk niet present
sym_e_illegal_field=E_Onbekend record lid $1
sym_n_uninitialized_local_variable=N_Locale variabele $1 schijnt niet geinitialiseerd te zijn
sym_e_id_no_member=E_Geen lid met de naam $1
sym_b_param_list=B_Declaratie gevonden: $1
sym_e_forward_type_not_resolved=E_Forward declaratie noet opgelost $1
sym_h_duplicate_id_where=H_Identifier reeds gedefinieerd in $1 op lijn $2
sym_n_local_identifier_not_used=N_Lokale variabele niet gebruikt $1

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
cg_e_stacklimit_in_local_routine=E_Stack limiet overschreden in lokale routine
cg_d_stackframe_omited=D_Stack frame wordt niet gemaakt
cg_e_unable_inline_object_methods=E_Kan niet inline gebruiken voor object methodes
cg_e_unable_inline_procvar=E_Kan niet inline gebruiken voor procvar aanroepen
cg_e_no_code_for_inline_stored=E_Geen code voor inline procedure opgeslagen
cg_e_can_access_element_zero=E_Element nul van een ansi/wide- of longstring is niet toegankelijk, gebruik (set)length.
cg_e_include_not_implemented=E_Include en exclude niet ondersteund in dit geval
cg_w_may_wrong_math_argument=W_Waarschijnlijk ongeldige constante doorgegeven aan interne routine
cg_e_cannot_call_cons_dest_inside_with=E_Constructors of destructors kunnen niet aangeroepen worden binnen een 'with' clausule
cg_e_cannot_call_message_direct=E_Een message handler methode kan niet direct aangeroepen worden.

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
asmr_e_cannot_use___SELF_outside_methode=E_Cannot use __SELF outside a method
asmr_e_cannot_use___OLDEBP_outside_nested_procedure=E_Cannot use __OLDEBP outside a nested procedure
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
exec_i_assembling_pipe=I_Assembleren (pijp) van $1
exec_d_cant_create_asmfile=E_Kan geen assembler bestand $1 niet maken
exec_w_assembler_not_found=W_Assembleerder $1 niet gevonden, overschakeling naar externe assemblage
exec_w_error_while_assembling=W_Fout bij assembleren
exec_w_cant_call_assembler=W_Kan assembleerder niet oproepen, overschakeling naar externe assemblage
exec_i_assembling=I_Assembleren van $1
exec_w_linker_not_found=W_Linker $1 niet gevonden, oveschakeling naar externe linker
exec_w_objfile_not_found=E_Bestand $1 niet gevonden, linken kan foutlopen !
exec_w_libfile_not_found=E_Bibliotheek $1 niet gevonden, linken kan foutlopen !
exec_w_error_while_linking=W_Error tijdens linken
exec_w_cant_call_linker=W_Kan linker niet oproepen, overschakeling naar extern linken
exec_i_linking=I_Linken van $1
exec_w_binder_not_found=W_binder niet gevonden, overschakeling naar extern binden.
exec_w_ar_not_found=W_ar niet gevonden, overschakeling naar externe ar
exec_e_dll_not_supported=E_Dynamische bibliotheken niet ondersteund
exec_i_closing_script=I_Afsluiten batch $1
exec_w_source_os_redefined=W_Bron besturingssysteem opnieuw definieerd
exec_i_compilingresource=I_Compileer resource $1
exec_w_res_not_found=W_resource compiler niet gevonden, ga over op externe modus
exec_t_using_linker=T_Gebruikte linker: $1
exec_t_using_assembler=T_Gebruikte assembler: $1

#
# Unit loading
#
unit_t_ppu_loading=U_PPU Laden $1
unit_u_ppu_time=D_PPU Tijd: $1
unit_u_ppu_file_too_short=D_PPU bestand te kort
unit_u_ppu_invalid_header=D_PPU Ongeldige hoofding (geen PPU aan het begin)
unit_u_ppu_invalid_version=D_PPU Ongeldige Versie $1
unit_u_ppu_flags=D_PPU Vlaggen: $1
unit_u_ppu_crc=D_PPU CRC: $1
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
unit_u_load_unit=U_Laden van $1 ($2) unit $3
unit_f_ppu_read_unexpected_end=F_Onverwacht einde van PPU-bestand
unit_u_check_time=U_PPU Check bestand $1 tijd $2
unit_u_ppu_invalid_processor=U_PPU is gecompileerd voor een andere processor
unit_u_ppu_invalid_target=U_PPU is gecompileerd voor een ander doel OS
unit_u_ppu_source=U_PPU bron: $1
unit_u_recompile_crc_change=U_Hercompileer $1, checksum voor $2 veranderd
unit_u_ppu_name=U_PPU Naam: $1
unit_u_recompile_staticlib_is_older=U_Hercompileer unit, statische bibliotheek ouder dan ppu-bestand
unit_u_recompile_sharedlib_is_older=U_Hercompileer unit, dynamische bibliotheek ouder dan ppu-bestand
unit_u_recompile_obj_and_asm_older=U_Hercompileer unit, object en asm zijn ouder dan ppu-bestand
unit_u_recompile_obj_older_than_asm=U_Hercompileer unit, object is ouder dan assembler
unit_u_start_parse_interface=U_Parsen van interface van $1 
unit_u_start_parse_implementation=U_Parsen van implementation van $1
unit_u_second_load_unit=U_Tweede maal unit $1 laden
unit_u_recompile_source_found_alone=U_herkompileer $1, alleen bron gevonden


#
#  Optie
#
option_usage=$1 [opties] <bestand> [opties]
option_only_one_source_support=Maar een bronbestand wordt ondersteunt
option_def_only_for_os2=DEF file kan alleen worden gemaakt voor OS/2
option_no_nested_response_file=Geneste optie bestanden niet ondersteund
option_no_source_found=Geen bronbestand op de commando-lijn
option_illegal_para=Ongeldige parameter: $1
option_help_pages_para=-? druk hulp af
option_too_many_cfg_files=Te veel geneste instelling bestanden
option_unable_open_file=Kan bestand niet openen $1
option_reading_further_from=Meer instellingen worden gelezen van $1
option_target_is_already_set=Doel is reeds ingesteld op: $1
option_no_shared_lib_under_dos=Shared bibliotheken niet onderteund onder DOS, val terug op statisch
option_too_many_ifdef=Te veel IF(N)DEFs
option_too_many_endif=Te veel ENDIFs
option_too_less_endif=Open voorwaardelijke aan eind van bestand
option_no_debug_support=Debug informatie generatie is niet ondersteund door deze applicatie
option_no_debug_support_recompile_fpc=Hercompileer de compiler-applicatie met -dGDB
option_obsolete_switch=W_U gebruikt een in onbruik geraakte schakeloptie $1
option_obsolete_switch_use_new=W_U gebruikt een in onbruik geraakte schakeloptie $1, gebruik $2 a.u.b.
#
# Executable information
#
execinfo_f_cant_process_executable=F_Kan applicatie niet nabehandelen $1
execinfo_f_cant_open_executable=F_Kan applicatie niet openen $1
execinfo_x_codesize=X_Grootte van de code: $1 bytes
execinfo_x_initdatasize=X_Grootte van geinitialiseerde data: $1 bytes
execinfo_x_uninitdatasize=X_Grootte van niet-geinitialiseerde data: $1 bytes
execinfo_x_stackreserve=X_Gereserveerde Stack ruimte: $1 bytes
execinfo_x_stackcommit=X_Gecommitte Stack ruimte: $1 bytes

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
ol013=3*2CS_Creeer dynamische bibliotheek
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
ol033=**1i_informatie
ol034=**1I<x>_Voeg <x> toe aan invoegpad
ol035=**1k<x>_Geef <x> door aan de linker
ol036=**1l_Druk logo af
ol037=**1n_Standaard configuratie bestand niet lezen
ol038=**1o<x>_Stel de naam van het applicatiebestand in op <x>
ol039=**1pg_genereer profile code voor gprof
ol040=*L1P_Gebruik pipes in plaats van tijdelijke assembler bestanden
ol041=**1S_Syntax instellingen
ol042=**2S2_Stel Delphi 2 uitbreidingen in
ol043=**2Sc_Ondersteun operatoren als in C (*=,+=,/= en -=)
ol044=**2Sd_Compiler verwijdert assembler lijsten (minder geheugengebruik, maar trager)
ol045=**2Se_Compiler stopt na de eerste fout
ol046=**2Sg_Sta gebruik van LABEL en GOTO toe
ol047=**2Si_Ondersteun C++ stijl INLINE
ol048=**2Sm_Ondersteun macros zoals in C (globaal)
ol049=**2So_Probeer TP/BP 7.0 compatibel te zijn
ol050=**2Sp_Probeer to be gpc compatibel te zijn
ol051=**2Ss_Constructor naam moet init zijn (destructor moet done zijn)
ol052=**2St_Sta static sleutelwoord toe in objecten
ol053=**2Sv_Sta variabele opties toe (cvar,external,public,export)
ol054=**1s_Roep assembler en linker niet op (slechts met -a)
ol055=**1T<x>_Doel besturingssysteem
ol056=3*2TGO32V1_versie 1 van de DJ Delorie DOS uitbreider
ol057=3*2TGO32V2_versie 2 van de DJ Delorie DOS uitbreider
ol058=3*2TLINUX_Linux
ol059=3*2TOS2_OS/2 2.x
ol060=3*2TWin32_Windows 32 Bit
ol061=6*2TAMIGA_Commodore Amiga
ol062=6*2TATARI_Atari ST/STe/TT
ol063=6*2TMACOS_Macintosh m68k
ol064=6*2TLINUX_Linux-68k
ol065=**1u<x>_Verwijdert symbooldefinitie <x>
ol066=**1U_unit opties
ol067=**2Un_Unit naam niet nagaan
ol068=**2Up<x>_Zelfde als -Fu<x>
ol069=**2Us_Compileer een systeemunit
ol070=**1v<x>_Wees uitvoerig. <x> is een combinatie van volgende letters:
ol071=**2*_e : Toon fouten (standaard)     d : Toon debug informatie
ol072=**2*_w : Toon waarschuwingen         u : Toon gebruikte bestanden
ol073=**2*_n : Toon notas                  t : Toon geteste bestanden
ol074=**2*_h : Toon hints                  m : Toon gedefinieerde macros
ol075=**2*_i : Toon algemene informatie    p : Toon gecompileerde routines
ol076=**2*_l : Toon lijnnummers            c : Toon voorwaardelijken
ol077=**2*_a : Toon alles                  0 : Toon alleen fouten
ol078=**1X_applicatie instellingen
ol079=*L2Xc_link met de C bibliotheek
ol080=**2XD_link met dynamische bibliotheken (definieert FPC_LINK_DYNAMIC)
ol081=**2Xs_verwijder alle symbolen uit applicatie
ol082=**2XS_link met statische bibliotheken (definieert FPC_LINK_STATIC)
ol083=**0*_Processor specifieke instellingen:
ol084=3*1A_Output formaat
ol085=3*2Ao_Coff bestand met GNU AS
ol086=3*2Anasmcoff_Coff bestand met Nasm
ol087=3*2Anasmelf_elf32 (linux) bestand met Nasm
ol088=3*2Anasmobj_obj bestand met Nasm
ol089=3*2Amasm_obj bestand met Masm (Microsoft)
ol090=3*2Atasm_obj bestand met Tasm (Borland)
ol091=3*1R_assembler lezer stijl:
ol092=3*2Ratt_Lees AT&T stijl assembler
ol093=3*2Rintel_Lees Intel stijl assembler
ol094=3*2Rdirect_Kopieer assembler dadelijk naar assembler bestand
ol095=3*1O<x>_optimizaties
ol096=3*2Og_Genereer kleinere code
ol097=3*2OG_Genereerd snellere code (standaard)
ol098=3*2Or_Houd zekere variabelen in registers (Nog steeds BUGGY!!!)
ol099=3*2Ou_Probeer onzekere optimizaties (zie documentatie)
ol100=3*2O1_Niveau 1 optimizaties (snelle optimizaties)
ol101=3*2O2_Niveau 2 optimizaties (-O1 + tragere optimizaties)
ol102=3*2O3_Niveau 3 optimizaties (Zelfde als -O2u)
ol103=3*2Op_Doel processor
ol104=3*3Op1_Stel doel processor in op 386/486
ol105=3*3Op2_Stel doel processor in op Pentium/PentiumMMX (tm)
ol106=3*3Op3_Stel doel processor in op PPro/PII/c6x86/K6 (tm)
ol107=6*1A_output formaat
ol108=6*2Ao_Unix o-bestandmet GNU AS
ol109=6*2Agas_GNU Motorola assembler
ol110=6*2Amit_MIT Syntax (oude GAS)
ol111=6*2Amot_Standaard Motorola assembler
ol112=6*1O_optimizaties
ol113=6*2Oa_Gebruik de optimizaties
ol114=6*2Og_Genereer kleinere code
ol115=6*2OG_Genereer snellere code (standaard)
ol116=6*2Ox_Optimizeer maximaal (nog steeds BUGGY!!!)
ol117=6*2O2_Stel doel processor in op MC68020+
ol118=**1*_
ol119=**1?_Toont deze hulp
ol120=6*2Agas_GNU Motorola assembler
ol121=6*2Amit_MIT Syntax (old GAS)
ol122=6*2Amot_Standard Motorola assembler
ol123=6*1O_optimizaties:
ol124=6*2Oa_Geruik optimizatie
ol125=6*2Og_Maak kleinere
ol126=6*2OG_Maak snellere code (standaard)
ol127=6*2Ox_Optimaliseer maximaal (still BUGGY!!!)
ol128=6*2O2_Stel doel processor in op MC68020+
ol129=6*1R<x>_assembler lezer stijl:
ol130=6*2RMOT_Lees motorola stijl assembler
ol131=6*1T<x>_Doel besturingssysteem:
ol132=6*2TAMIGA_Commodore Amiga
ol133=6*2TATARI_Atari ST/STe/TT
ol134=6*2TMACOS_Macintosh m68k
ol135=6*2TLINUX_Linux-68k
ol136=**1*_
ol137=**1?_Toont deze hulp
ol_end=**1h_Toont deze hulp zonder wachten

#
# Het einde...
#
