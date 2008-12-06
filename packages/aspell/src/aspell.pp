unit aspell;

{ * This file is header translation of The New Aspell
  * Copyright (C) 2001-2002 by Kevin Atkinson under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with this library if you did not you can find it
  * at http://www.gnu.org/.                                              * }

{ * Translation to pascal (c) 2008 by Aleš Katona. * }

{$PACKRECORDS C}

interface

uses
  cTypes;

{$IFDEF UNIX}
  const libaspell = 'aspell';
{$ELSE} // windows
  // TODO: figure this out
  const libaspell = 'aspell-%s.dll';
{$ENDIF}

  {$i aspelltypes.inc}

  {************************* mutable container ************************* }

  function aspell_mutable_container_add(ths:PAspellMutableContainer; to_add:pchar):cint;cdecl;external libaspell name 'aspell_mutable_container_add';

  function aspell_mutable_container_remove(ths:PAspellMutableContainer; to_rem:pchar):cint;cdecl;external libaspell name 'aspell_mutable_container_remove';

  procedure aspell_mutable_container_clear(ths:PAspellMutableContainer);cdecl;external libaspell name 'aspell_mutable_container_clear';

  function aspell_mutable_container_to_mutable_container(ths:PAspellMutableContainer):PAspellMutableContainer; cdecl;external libaspell name 'aspell_mutable_container_to_mutable_container';

    {******************************* config ******************************* }

    function aspell_key_info_enumeration_at_end(ths:PAspellKeyInfoEnumeration):cint;cdecl;external libaspell name 'aspell_key_info_enumeration_at_end';

    function aspell_key_info_enumeration_next(ths:PAspellKeyInfoEnumeration):PAspellKeyInfo;cdecl;external libaspell name 'aspell_key_info_enumeration_next';

    procedure delete_aspell_key_info_enumeration(ths:PAspellKeyInfoEnumeration);cdecl;external libaspell name 'delete_aspell_key_info_enumeration';

    function aspell_key_info_enumeration_clone(ths:PAspellKeyInfoEnumeration):PAspellKeyInfoEnumeration; cdecl;external libaspell name 'aspell_key_info_enumeration_clone';

    procedure aspell_key_info_enumeration_assign(ths:PAspellKeyInfoEnumeration; other:PAspellKeyInfoEnumeration);cdecl;external libaspell name 'aspell_key_info_enumeration_assign';

    function new_aspell_config():PAspellConfig;cdecl;external libaspell name 'new_aspell_config';

    procedure delete_aspell_config(ths:PAspellConfig);cdecl;external libaspell name 'delete_aspell_config';

    function aspell_config_clone(ths:PAspellConfig):PAspellConfig;cdecl;external libaspell name 'aspell_config_clone';

    procedure aspell_config_assign(ths:PAspellConfig; other:PAspellConfig);cdecl;external libaspell name 'aspell_config_assign';

    function aspell_config_error_number(ths:PAspellConfig):cuint;cdecl;external libaspell name 'aspell_config_error_number';

    function aspell_config_error_message(ths:PAspellConfig):pchar;cdecl;external libaspell name 'aspell_config_error_message';

    function aspell_config_error(ths:PAspellConfig):PAspellError;cdecl;external libaspell name 'aspell_config_error';

    { Sets extra keys which this config class should
     * accept. begin and end are expected to point to
     * the beginning and ending of an array of Aspell
     * Key Info.  }

    procedure aspell_config_set_extra(ths:PAspellConfig; b:PAspellKeyInfo; e:PAspellKeyInfo);cdecl;external libaspell name 'aspell_config_set_extra';

    { Returns the KeyInfo object for the
     * corresponding key or returns NULL and sets
     * error_num to PERROR_UNKNOWN_KEY if the key is
     * not valid. The pointer returned is valid for
     * the lifetime of the object.  }

    function aspell_config_keyinfo(ths:PAspellConfig; key:pchar):PAspellKeyInfo;cdecl;external libaspell name 'aspell_config_keyinfo';

    { Returns a newly allocated enumeration of all
     * the possible objects this config class uses.  }

    function aspell_config_possible_elements(ths:PAspellConfig; include_extra:cint):PAspellKeyInfoEnumeration;cdecl;external libaspell name 'aspell_config_possible_elements';

    { Returns the default value for given key which
     * may involve substituting variables, thus it is
     * not the same as keyinfo(key)->def returns NULL
     * and sets error_num to PERROR_UNKNOWN_KEY if
     * the key is not valid. Uses the temporary
     * string.  }

    function aspell_config_get_default(ths:PAspellConfig; key:pchar):pchar;cdecl;external libaspell name 'aspell_config_get_default';

    { Returns a newly allocated enumeration of all
     * the key/value pairs. This DOES not include ones
     * which are set to their default values.  }

    function aspell_config_elements(ths:PAspellConfig):PAspellStringPairEnumeration;cdecl;external libaspell name 'aspell_config_elements';

    { Inserts an item, if the item already exists it
     * will be replaced. Returns TRUE if it succeeded
     * or FALSE on error. If the key is not valid it
     * sets error_num to PERROR_UNKNOWN_KEY, if the
     * value is not valid it will set error_num to
     * PERROR_BAD_VALUE, if the value can not be
     * changed it sets error_num to
     * PERROR_CANT_CHANGE_VALUE, and if the value is
     * a list and you are trying to set its directory,
     * it sets error_num to PERROR_LIST_SET  }

    function aspell_config_replace(ths:PAspellConfig; key:pchar; value:pchar):cint;cdecl;external libaspell name 'aspell_config_replace';

    { Remove a key and returns TRUE if it exists
     * otherwise return FALSE. This effectively sets
     * the key to its default value. Calling replace
     * with a value of "<default>" will also call
     * remove. If the key does not exist then it sets
     * error_num to 0 or PERROR_NOT, if the key is
     * not valid then it sets error_num to
     * PERROR_UNKNOWN_KEY, if the value can not be
     * changed then it sets error_num to
     * PERROR_CANT_CHANGE_VALUE  }

    function aspell_config_remove(ths:PAspellConfig; key:pchar):cint;cdecl;external libaspell name 'aspell_config_remove';

    function aspell_config_have(ths:PAspellConfig; key:pchar):cint;cdecl;external libaspell name 'aspell_config_have';

    { Returns NULL on error.  }

    function aspell_config_retrieve(ths:PAspellConfig; key:pchar):pchar;cdecl;external libaspell name 'aspell_config_retrieve';

    function aspell_config_retrieve_list(ths:PAspellConfig; key:pchar; lst:PAspellMutableContainer):cint;cdecl;external libaspell name 'aspell_config_retrieve_list';

    { In "ths" Aspell configuration, search for a
     * character string matching "key" string.
     * If "key" is found then return 1 else return 0.
     * If error encountered, then return -1.  }

    function aspell_config_retrieve_bool(ths:PAspellConfig; key:pchar):cint;cdecl;external libaspell name 'aspell_config_retrieve_bool';

    { In "ths" Aspell configuration, search for an
     * integer value matching "key" string.
     * Return -1 on error.  }

    function aspell_config_retrieve_int(ths:PAspellConfig; key:pchar):cint;cdecl;external libaspell name 'aspell_config_retrieve_int';

    {******************************* error ******************************* }

    function aspell_error_is_a(ths:PAspellError; e:PAspellErrorInfo):cint;cdecl;external libaspell name 'aspell_error_is_a';

    {*************************** can have error *************************** }

    function aspell_error_number(ths:PAspellCanHaveError):cuint;cdecl;external libaspell name 'aspell_error_number';

    function aspell_error_message(ths:PAspellCanHaveError):pchar;cdecl;external libaspell name 'aspell_error_message';

    function aspell_error(ths:PAspellCanHaveError):PAspellError;cdecl;external libaspell name 'aspell_error';

    procedure delete_aspell_can_have_error(ths:PAspellCanHaveError);cdecl;external libaspell name 'delete_aspell_can_have_error';

    {******************************* errors ******************************* }

    // ignored

    {****************************** speller ****************************** }

    function new_aspell_speller(config:PAspellConfig):PAspellCanHaveError;cdecl;external libaspell name 'new_aspell_speller';

    function to_aspell_speller(obj:PAspellCanHaveError):PAspellSpeller;cdecl;external libaspell name 'to_aspell_speller';

    procedure delete_aspell_speller(ths:PAspellSpeller);cdecl;external libaspell name 'delete_aspell_speller';

    function aspell_speller_error_number(ths:PAspellSpeller):cuint;cdecl;external libaspell name 'aspell_speller_error_number';

    function aspell_speller_error_message(ths:PAspellSpeller):pchar;cdecl;external libaspell name 'aspell_speller_error_message';

    function aspell_speller_error(ths:PAspellSpeller):PAspellError;cdecl;external libaspell name 'aspell_speller_error';

    function aspell_speller_config(ths:PAspellSpeller):PAspellConfig;cdecl;external libaspell name 'aspell_speller_config';
    { Returns 0 if it is not in the dictionary,
     * 1 if it is, or -1 on error.  }

    function aspell_speller_check(ths:PAspellSpeller; word:pchar; word_size:cint):cint;cdecl;external libaspell name 'aspell_speller_check';

    { Add this word to your own personal word list.  }

    function aspell_speller_add_to_personal(ths:PAspellSpeller; word:pchar; word_size:cint):cint;cdecl;external libaspell name 'aspell_speller_add_to_personal';

    { Add this word to the current spelling session.  }

    function aspell_speller_add_to_session(ths:PAspellSpeller; word:pchar; word_size:cint):cint;cdecl;external libaspell name 'aspell_speller_add_to_session';

    { This is your own personal word list file plus
     * any extra words added during this session to
     * your own personal word list.  }

    function aspell_speller_personal_word_list(ths:PAspellSpeller):PAspellWordList;cdecl;external libaspell name 'aspell_speller_personal_word_list';

    { This is a list of words added to this session
     * that are not in the main word list or in your
     * own personal list but are considered valid for
     * this spelling session.  }

    function aspell_speller_session_word_list(ths:PAspellSpeller):PAspellWordList;cdecl;external libaspell name 'aspell_speller_session_word_list';

    { This is the main list of words used during this
     * spelling session.  }

    function aspell_speller_main_word_list(ths:PAspellSpeller):PAspellWordList;cdecl;external libaspell name 'aspell_speller_main_word_list';

    function aspell_speller_save_all_word_lists(ths:PAspellSpeller):cint;cdecl;external libaspell name 'aspell_speller_save_all_word_lists';

    function aspell_speller_clear_session(ths:PAspellSpeller):cint;cdecl;external libaspell name 'aspell_speller_clear_session';

    { Return NULL on error.
     * The word list returned by suggest is only
     * valid until the next call to suggest.  }

    function aspell_speller_suggest(ths:PAspellSpeller; word:pchar; word_size:cint):PAspellWordList;cdecl;external libaspell name 'aspell_speller_suggest';

    function aspell_speller_store_replacement(ths:PAspellSpeller; mis:pchar; mis_size:cint; cor:pchar; cor_size:cint):cint;cdecl;external libaspell name 'aspell_speller_store_replacement';

    {******************************* filter ******************************* }

    procedure delete_aspell_filter(ths:PAspellFilter);cdecl;external libaspell name 'delete_aspell_filter';

    function aspell_filter_error_number(ths:PAspellFilter):cuint;cdecl;external libaspell name 'aspell_filter_error_number';

    function aspell_filter_error_message(ths:PAspellFilter):pchar;cdecl;external libaspell name 'aspell_filter_error_message';

    function aspell_filter_error(ths:PAspellFilter):PAspellError;cdecl;external libaspell name 'aspell_filter_error';

    function to_aspell_filter(obj:PAspellCanHaveError):PAspellFilter;cdecl;external libaspell name 'to_aspell_filter';

    {************************** document checker ************************** }

    procedure delete_aspell_document_checker(ths:PAspellDocumentChecker);cdecl;external libaspell name 'delete_aspell_document_checker';

    function aspell_document_checker_error_number(ths:PAspellDocumentChecker):cuint;cdecl;external libaspell name 'aspell_document_checker_error_number';

    function aspell_document_checker_error_message(ths:PAspellDocumentChecker):pchar;cdecl;external libaspell name 'aspell_document_checker_error_message';

    function aspell_document_checker_error(ths:PAspellDocumentChecker):PAspellError;cdecl;external libaspell name 'aspell_document_checker_error';

    { Creates a new document checker.
     * The speller class is expected to last until
     * this class is destroyed.
     * If config is given it will be used to override
     * any relevent options set by this speller class.
     * The config class is not once this function is done.
     * If filter is given then it will take ownership of
     * the filter class and use it to do the filtering.
     * You are expected to free the checker when done.  }

    function new_aspell_document_checker(speller: PAspellSpeller): PAspellCanHaveError;cdecl;external libaspell name 'new_aspell_document_checker';

    function to_aspell_document_checker(obj:PAspellCanHaveError):PAspellDocumentChecker;cdecl;external libaspell name 'to_aspell_document_checker';

    { Reset the internal state of the filter.
     * Should be called whenever a new document is
     * being filtered.  }
    procedure aspell_document_checker_reset(ths:PAspellDocumentChecker);cdecl;external libaspell name 'aspell_document_checker_reset';

    { Process a string.
     * The string passed in should only be split on
     * white space characters.  Furthermore, between
     * calls to reset, each string should be passed
     * in exactly once and in the order they appeared
     * in the document.  Passing in strings out of
     * order, skipping strings or passing them in
     * more than once may lead to undefined results.  }

    procedure aspell_document_checker_process(ths:PAspellDocumentChecker; str:pchar; size:cint);cdecl;external libaspell name 'aspell_document_checker_process';

    { Returns the next misspelled word in the
     * processed string.  If there are no more
     * misspelled words, then token.word will be
     * NULL and token.size will be 0  }

    // internal hacky version to go around a bug regarding struct results/cdecl
    function __aspell_document_checker_next_misspelling(ths:PAspellDocumentChecker):{$IFDEF CPU64}{$IFDEF LINUX}QWord{$ELSE}AspellToken{$ENDIF}{$ELSE}AspellToken{$ENDIF};cdecl;external libaspell name 'aspell_document_checker_next_misspelling';

    { Returns the underlying filter class.  }

    function aspell_document_checker_filter(ths:PAspellDocumentChecker):PAspellFilter;cdecl;external libaspell name 'aspell_document_checker_filter';

    {***************************** word list ***************************** }

    function aspell_word_list_empty(ths:PAspellWordList):cint;cdecl;external libaspell name 'aspell_word_list_empty';

    function aspell_word_list_size(ths:PAspellWordList):cuint;cdecl;external libaspell name 'aspell_word_list_size';

    function aspell_word_list_elements(ths:PAspellWordList):PAspellStringEnumeration;cdecl;external libaspell name 'aspell_word_list_elements';

    {************************* string enumeration ************************* }

    procedure delete_aspell_string_enumeration(ths:PAspellStringEnumeration);cdecl;external libaspell name 'delete_aspell_string_enumeration';

    function aspell_string_enumeration_clone(ths:PAspellStringEnumeration):PAspellStringEnumeration;cdecl;external libaspell name 'aspell_string_enumeration_clone';

    procedure aspell_string_enumeration_assign(ths:PAspellStringEnumeration; other:PAspellStringEnumeration);cdecl;external libaspell name 'aspell_string_enumeration_assign';

    function aspell_string_enumeration_at_end(ths:PAspellStringEnumeration):cint;cdecl;external libaspell name 'aspell_string_enumeration_at_end';

    function aspell_string_enumeration_next(ths:PAspellStringEnumeration):pchar;cdecl;external libaspell name 'aspell_string_enumeration_next';

    {******************************** info ******************************** }

    function get_aspell_module_info_list(config:PAspellConfig):PAspellModuleInfoList;cdecl;external libaspell name 'get_aspell_module_info_list';

    function aspell_module_info_list_empty(ths:PAspellModuleInfoList):cint;cdecl;external libaspell name 'aspell_module_info_list_empty';

    function aspell_module_info_list_size(ths:PAspellModuleInfoList):cuint;cdecl;external libaspell name 'aspell_module_info_list_size';

    function aspell_module_info_list_elements(ths:PAspellModuleInfoList):PAspellModuleInfoEnumeration;cdecl;external libaspell name 'aspell_module_info_list_elements';

    function get_aspell_dict_info_list(config:PAspellConfig):PAspellDictInfoList;cdecl;external libaspell name 'get_aspell_dict_info_list';

    function aspell_dict_info_list_empty(ths:PAspellDictInfoList):cint;cdecl;external libaspell name 'aspell_dict_info_list_empty';

    function aspell_dict_info_list_size(ths:PAspellDictInfoList):cuint;cdecl;external libaspell name 'aspell_dict_info_list_size';

    function aspell_dict_info_list_elements(ths:PAspellDictInfoList):PAspellDictInfoEnumeration;cdecl;external libaspell name 'aspell_dict_info_list_elements';

    function aspell_module_info_enumeration_at_end(ths:PAspellModuleInfoEnumeration):cint;cdecl;external libaspell name 'aspell_module_info_enumeration_at_end';

    function aspell_module_info_enumeration_next(ths:PAspellModuleInfoEnumeration):PAspellModuleInfo;cdecl;external libaspell name 'aspell_module_info_enumeration_next';

    procedure delete_aspell_module_info_enumeration(ths:PAspellModuleInfoEnumeration);cdecl;external libaspell name 'delete_aspell_module_info_enumeration';

    function aspell_module_info_enumeration_clone(ths:PAspellModuleInfoEnumeration):PAspellModuleInfoEnumeration;cdecl;external libaspell name 'aspell_module_info_enumeration_clone';

    procedure aspell_module_info_enumeration_assign(ths:PAspellModuleInfoEnumeration; other:PAspellModuleInfoEnumeration);cdecl;external libaspell name 'aspell_module_info_enumeration_assign';

    function aspell_dict_info_enumeration_at_end(ths:PAspellDictInfoEnumeration):cint;cdecl;external libaspell name 'aspell_dict_info_enumeration_at_end';

    function aspell_dict_info_enumeration_next(ths:PAspellDictInfoEnumeration):PAspellDictInfo;cdecl;external libaspell name 'aspell_dict_info_enumeration_next';

    procedure delete_aspell_dict_info_enumeration(ths:PAspellDictInfoEnumeration);cdecl;external libaspell name 'delete_aspell_dict_info_enumeration';

    function aspell_dict_info_enumeration_clone(ths:PAspellDictInfoEnumeration):PAspellDictInfoEnumeration;cdecl;external libaspell name 'aspell_dict_info_enumeration_clone';

    procedure aspell_dict_info_enumeration_assign(ths:PAspellDictInfoEnumeration; other:PAspellDictInfoEnumeration);cdecl;external libaspell name 'aspell_dict_info_enumeration_assign';

    {**************************** string list **************************** }

    function new_aspell_string_list():PAspellStringList;cdecl;external libaspell name 'new_aspell_string_list';

    function aspell_string_list_empty(ths:PAspellStringList):cint;cdecl;external libaspell name 'aspell_string_list_empty';

    function aspell_string_list_size(ths:PAspellStringList):cuint;cdecl;external libaspell name 'aspell_string_list_size';

    function aspell_string_list_elements(ths:PAspellStringList):PAspellStringEnumeration;cdecl;external libaspell name 'aspell_string_list_elements';

    function aspell_string_list_add(ths:PAspellStringList; to_add:pchar):cint;cdecl;external libaspell name 'aspell_string_list_add';

    function aspell_string_list_remove(ths:PAspellStringList; to_rem:pchar):cint;cdecl;external libaspell name 'aspell_string_list_remove';

    procedure aspell_string_list_clear(ths:PAspellStringList);cdecl;external libaspell name 'aspell_string_list_clear';

    function aspell_string_list_to_mutable_container(ths:PAspellStringList):PAspellMutableContainer;cdecl;external libaspell name 'aspell_string_list_to_mutable_container';

    procedure delete_aspell_string_list(ths:PAspellStringList);cdecl;external libaspell name 'delete_aspell_string_list';

    function aspell_string_list_clone(ths:PAspellStringList):PAspellStringList;cdecl;external libaspell name 'aspell_string_list_clone';

    procedure aspell_string_list_assign(ths:PAspellStringList; other:PAspellStringList);cdecl;external libaspell name 'aspell_string_list_assign';

    {***************************** string map ***************************** }

    function new_aspell_string_map():PAspellStringMap;cdecl;external libaspell name 'new_aspell_string_map';

    function aspell_string_map_add(ths:PAspellStringMap; to_add:pchar):cint;cdecl;external libaspell name 'aspell_string_map_add';

    function aspell_string_map_remove(ths:PAspellStringMap; to_rem:pchar):cint;cdecl;external libaspell name 'aspell_string_map_remove';

    procedure aspell_string_map_clear(ths:PAspellStringMap);cdecl;external libaspell name 'aspell_string_map_clear';

    function aspell_string_map_to_mutable_container(ths:PAspellStringMap):PAspellMutableContainer;cdecl;external libaspell name 'aspell_string_map_to_mutable_container';

    procedure delete_aspell_string_map(ths:PAspellStringMap);cdecl;external libaspell name 'delete_aspell_string_map';

    function aspell_string_map_clone(ths:PAspellStringMap):PAspellStringMap;cdecl;external libaspell name 'aspell_string_map_clone';

    procedure aspell_string_map_assign(ths:PAspellStringMap; other:PAspellStringMap);cdecl;external libaspell name 'aspell_string_map_assign';

    function aspell_string_map_empty(ths:PAspellStringMap):cint;cdecl;external libaspell name 'aspell_string_map_empty';

    function aspell_string_map_size(ths:PAspellStringMap):cuint;cdecl;external libaspell name 'aspell_string_map_size';

    function aspell_string_map_elements(ths:PAspellStringMap):PAspellStringPairEnumeration;cdecl;external libaspell name 'aspell_string_map_elements';

    { Insert a new element.
     * Will NOT overwrite an existing entry.
     * Returns FALSE if the element already exists.  }

    function aspell_string_map_insert(ths:PAspellStringMap; key:pchar; value:pchar):cint;cdecl;external libaspell name 'aspell_string_map_insert';

    { Insert a new element.
     * Will overwrite an existing entry.
     * Always returns TRUE.  }

    function aspell_string_map_replace(ths:PAspellStringMap; key:pchar; value:pchar):cint;cdecl;external libaspell name 'aspell_string_map_replace';

    { Looks up an element and returns the value.
     * Returns NULL if the element does not exist.
     * Returns an empty string if the element exists
     * but has a NULL value.  }

    function aspell_string_map_lookup(ths:PAspellStringMap; key:pchar):pchar;cdecl;external libaspell name 'aspell_string_map_lookup';

    {********************** string pair enumeration ********************** }

    function aspell_string_pair_enumeration_at_end(ths:PAspellStringPairEnumeration):cint;cdecl;external libaspell name 'aspell_string_pair_enumeration_at_end';

    function aspell_string_pair_enumeration_next(ths:PAspellStringPairEnumeration):AspellStringPair;cdecl;external libaspell name 'aspell_string_pair_enumeration_next';

    procedure delete_aspell_string_pair_enumeration(ths:PAspellStringPairEnumeration);cdecl;external libaspell name 'delete_aspell_string_pair_enumeration';

    function aspell_string_pair_enumeration_clone(ths:PAspellStringPairEnumeration):PAspellStringPairEnumeration;cdecl;external libaspell name 'aspell_string_pair_enumeration_clone';

    procedure aspell_string_pair_enumeration_assign(ths:PAspellStringPairEnumeration; other:PAspellStringPairEnumeration);cdecl;external libaspell name 'aspell_string_pair_enumeration_assign';

    {******************************* cache ******************************* }
    { Reset the global cache(s) so that cache queries will
     * create a new object. If existing objects are still in
     * use they are not deleted. If which is NULL then all
     * caches will be reset. Current caches are "encode",
     * "decode", "dictionary", "language", and "keyboard".  }

    function aspell_reset_cache(which:pchar):cint;cdecl;external libaspell name 'aspell_reset_cache';
    
  function aspell_init(const libn: ansistring): Boolean;
  function aspell_loaded: Boolean;
  function aspell_document_checker_next_misspelling(ths:PAspellDocumentChecker):AspellToken;

implementation

function aspell_init(const libn: ansistring): Boolean;
begin
  aspell_init := True;
end;

function aspell_loaded: Boolean;
begin
  aspell_loaded := True;
end;

function aspell_document_checker_next_misspelling(ths: PAspellDocumentChecker
  ): AspellToken;
begin
  // yup...
  aspell_document_checker_next_misspelling := AspellToken(__aspell_document_checker_next_misspelling(ths));
end;

end.
