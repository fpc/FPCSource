// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   Pgunichar  = ^gunichar;
   gunichar   = guint32;

   Pgunichar2 = ^gunichar2;
   gunichar2 = guint16;

   PGUnicodeType = ^TGUnicodeType;
   TGUnicodeType = (
     G_UNICODE_CONTROL,
     G_UNICODE_FORMAT,
     G_UNICODE_UNASSIGNED,
     G_UNICODE_PRIVATE_USE,
     G_UNICODE_SURROGATE,
     G_UNICODE_LOWERCASE_LETTER,
     G_UNICODE_MODIFIER_LETTER,
     G_UNICODE_OTHER_LETTER,
     G_UNICODE_TITLECASE_LETTER,
     G_UNICODE_UPPERCASE_LETTER,
     G_UNICODE_COMBINING_MARK,
     G_UNICODE_ENCLOSING_MARK,
     G_UNICODE_NON_SPACING_MARK,
     G_UNICODE_DECIMAL_NUMBER,
     G_UNICODE_LETTER_NUMBER,
     G_UNICODE_OTHER_NUMBER,
     G_UNICODE_CONNECT_PUNCTUATION,
     G_UNICODE_DASH_PUNCTUATION,
     G_UNICODE_CLOSE_PUNCTUATION,
     G_UNICODE_FINAL_PUNCTUATION,
     G_UNICODE_INITIAL_PUNCTUATION,
     G_UNICODE_OTHER_PUNCTUATION,
     G_UNICODE_OPEN_PUNCTUATION,
     G_UNICODE_CURRENCY_SYMBOL,
     G_UNICODE_MODIFIER_SYMBOL,
     G_UNICODE_MATH_SYMBOL,
     G_UNICODE_OTHER_SYMBOL,
     G_UNICODE_LINE_SEPARATOR,
     G_UNICODE_PARAGRAPH_SEPARATOR,
     G_UNICODE_SPACE_SEPARATOR
   );

   PGUnicodeBreakType = ^TGUnicodeBreakType;
   TGUnicodeBreakType = (
     G_UNICODE_BREAK_MANDATORY,
     G_UNICODE_BREAK_CARRIAGE_RETURN,
     G_UNICODE_BREAK_LINE_FEED,
     G_UNICODE_BREAK_COMBINING_MARK,
     G_UNICODE_BREAK_SURROGATE,
     G_UNICODE_BREAK_ZERO_WIDTH_SPACE,
     G_UNICODE_BREAK_INSEPARABLE,
     G_UNICODE_BREAK_NON_BREAKING_GLUE,
     G_UNICODE_BREAK_CONTINGENT,
     G_UNICODE_BREAK_SPACE,
     G_UNICODE_BREAK_AFTER,
     G_UNICODE_BREAK_BEFORE,
     G_UNICODE_BREAK_BEFORE_AND_AFTER,
     G_UNICODE_BREAK_HYPHEN,
     G_UNICODE_BREAK_NON_STARTER,
     G_UNICODE_BREAK_OPEN_PUNCTUATION,
     G_UNICODE_BREAK_CLOSE_PUNCTUATION,
     G_UNICODE_BREAK_QUOTATION,
     G_UNICODE_BREAK_EXCLAMATION,
     G_UNICODE_BREAK_IDEOGRAPHIC,
     G_UNICODE_BREAK_NUMERIC,
     G_UNICODE_BREAK_INFIX_SEPARATOR,
     G_UNICODE_BREAK_SYMBOL,
     G_UNICODE_BREAK_ALPHABETIC,
     G_UNICODE_BREAK_PREFIX,
     G_UNICODE_BREAK_POSTFIX,
     G_UNICODE_BREAK_COMPLEX_CONTEXT,
     G_UNICODE_BREAK_AMBIGUOUS,
     G_UNICODE_BREAK_UNKNOWN
   );

   PGNormalizeMode = ^TGNormalizeMode;
   TGNormalizeMode = gint;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
    { Returns TRUE if current locale uses UTF-8 charset.  If CHARSET is
       not null, sets  CHARSET to the name of the current locale's
       charset.  This value is statically allocated, and should be copied
       in case the locale's charset will be changed later using setlocale()
       or in some other way.
      }
    function g_get_charset(charset:PPchar):gboolean;cdecl;external gliblib name 'g_get_charset';

    { These are all analogs of the <ctype.h> functions.
      }
    function g_unichar_isalnum(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isalnum';

    function g_unichar_isalpha(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isalpha';

    function g_unichar_iscntrl(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_iscntrl';

    function g_unichar_isdigit(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isdigit';

    function g_unichar_isgraph(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isgraph';

    function g_unichar_islower(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_islower';

    function g_unichar_isprint(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isprint';

    function g_unichar_ispunct(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_ispunct';

    function g_unichar_isspace(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isspace';

    function g_unichar_isupper(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isupper';

    function g_unichar_isxdigit(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isxdigit';

    function g_unichar_istitle(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_istitle';

    function g_unichar_isdefined(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_isdefined';

    function g_unichar_iswide(c:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_iswide';

    { More <ctype.h> functions.  These convert between the three cases.
       See the Unicode book to understand title case.   }
    function g_unichar_toupper(c:gunichar):gunichar;cdecl;external gliblib name 'g_unichar_toupper';

    function g_unichar_tolower(c:gunichar):gunichar;cdecl;external gliblib name 'g_unichar_tolower';

    function g_unichar_totitle(c:gunichar):gunichar;cdecl;external gliblib name 'g_unichar_totitle';

    { If C is a digit (according to `g_unichar_isdigit'), then return its
       numeric value.  Otherwise return -1.   }
    function g_unichar_digit_value(c:gunichar):gint;cdecl;external gliblib name 'g_unichar_digit_value';

    function g_unichar_xdigit_value(c:gunichar):gint;cdecl;external gliblib name 'g_unichar_xdigit_value';

    { Return the Unicode character type of a given character.   }
    function g_unichar_type(c:gunichar):TGUnicodeType;cdecl;external gliblib name 'g_unichar_type';

    { Return the line break property for a given character  }
    function g_unichar_break_type(c:gunichar):TGUnicodeBreakType;cdecl;external gliblib name 'g_unichar_break_type';

    { Compute canonical ordering of a string in-place.  This rearranges
       decomposed characters in the string according to their combining
       classes.  See the Unicode manual for more information.   }
    procedure g_unicode_canonical_ordering(_string:Pgunichar; len:gsize);cdecl;external gliblib name 'g_unicode_canonical_ordering';

    { Compute canonical decomposition of a character.  Returns g_malloc()d
       string of Unicode characters.  RESULT_LEN is set to the resulting
       length of the string.   }
    function g_unicode_canonical_decomposition(ch:gunichar; result_len:Pgsize):Pgunichar;cdecl;external gliblib name 'g_unicode_canonical_decomposition';

{$IFNDEF KYLIX}
    { Array of skip-bytes-per-initial character.
      }
    var
       g_utf8_skip : pgchar; external gliblib name 'g_utf8_skip';
{$ENDIF}

    function g_utf8_next_char (p: pguchar):pguchar;


    function g_utf8_get_char(p:Pgchar):gunichar;cdecl;external gliblib name 'g_utf8_get_char';

    function g_utf8_get_char_validated(p:Pgchar; max_len:gssize):gunichar;cdecl;external gliblib name 'g_utf8_get_char_validated';

    function g_utf8_offset_to_pointer(str:Pgchar; offset:glong):Pgchar;cdecl;external gliblib name 'g_utf8_offset_to_pointer';

    function g_utf8_pointer_to_offset(str:Pgchar; pos:Pgchar):glong;cdecl;external gliblib name 'g_utf8_pointer_to_offset';

    function g_utf8_prev_char(p:Pgchar):Pgchar;cdecl;external gliblib name 'g_utf8_prev_char';

    function g_utf8_find_next_char(p:Pgchar; _end:Pgchar):Pgchar;cdecl;external gliblib name 'g_utf8_find_next_char';

    function g_utf8_find_prev_char(str:Pgchar; p:Pgchar):Pgchar;cdecl;external gliblib name 'g_utf8_find_prev_char';

    function g_utf8_strlen(p:Pgchar; max:gssize):glong;cdecl;external gliblib name 'g_utf8_strlen';

    { Copies n characters from src to dest  }
    function g_utf8_strncpy(dest:Pgchar; src:Pgchar; n:gsize):Pgchar;cdecl;external gliblib name 'g_utf8_strncpy';

    { Find the UTF-8 character corresponding to ch, in string p. These
       functions are equivalants to strchr and strrchr  }
    function g_utf8_strchr(p:Pgchar; len:gssize; c:gunichar):Pgchar;cdecl;external gliblib name 'g_utf8_strchr';

    function g_utf8_strrchr(p:Pgchar; len:gssize; c:gunichar):Pgchar;cdecl;external gliblib name 'g_utf8_strrchr';

    function g_utf8_to_utf16(str:Pgchar; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgunichar2;cdecl;external gliblib name 'g_utf8_to_utf16';

    function g_utf8_to_ucs4(str:Pgchar; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgunichar;cdecl;external gliblib name 'g_utf8_to_ucs4';

    function g_utf8_to_ucs4_fast(str:Pgchar; len:glong; items_written:Pglong):Pgunichar;cdecl;external gliblib name 'g_utf8_to_ucs4_fast';

    function g_utf16_to_ucs4(str:Pgunichar2; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgunichar;cdecl;external gliblib name 'g_utf16_to_ucs4';

    function g_utf16_to_utf8(str:Pgunichar2; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgchar;cdecl;external gliblib name 'g_utf16_to_utf8';

    function g_ucs4_to_utf16(str:Pgunichar; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgunichar2;cdecl;external gliblib name 'g_ucs4_to_utf16';

    function g_ucs4_to_utf8(str:Pgunichar; len:glong; items_read:Pglong; items_written:Pglong; error:PPGError):Pgchar;cdecl;external gliblib name 'g_ucs4_to_utf8';

    { Convert a single character into UTF-8. outbuf must have at
       least 6 bytes of space. Returns the number of bytes in the
       result.
      }
    function g_unichar_to_utf8(c:gunichar; outbuf:Pgchar):gint;cdecl;external gliblib name 'g_unichar_to_utf8';

    { Validate a UTF8 string, return TRUE if valid, put pointer to
       first invalid char in   end
      }
    function g_utf8_validate(str:Pgchar; max_len:gssize; _end:PPgchar):gboolean;cdecl;external gliblib name 'g_utf8_validate';

    { Validate a Unicode character  }
    function g_unichar_validate(ch:gunichar):gboolean;cdecl;external gliblib name 'g_unichar_validate';

    function g_utf8_strup(str:Pgchar; len:gssize):Pgchar;cdecl;external gliblib name 'g_utf8_strup';

    function g_utf8_strdown(str:Pgchar; len:gssize):Pgchar;cdecl;external gliblib name 'g_utf8_strdown';

    function g_utf8_casefold(str:Pgchar; len:gssize):Pgchar;cdecl;external gliblib name 'g_utf8_casefold';


    const G_NORMALIZE_DEFAULT         = 0;
          G_NORMALIZE_NFD             = G_NORMALIZE_DEFAULT;
          G_NORMALIZE_DEFAULT_COMPOSE = 1;
          G_NORMALIZE_NFC             = G_NORMALIZE_DEFAULT_COMPOSE;
          G_NORMALIZE_ALL             = 2;
          G_NORMALIZE_NFKD            = G_NORMALIZE_ALL;
          G_NORMALIZE_ALL_COMPOSE     = 3;
          G_NORMALIZE_NFKC            = G_NORMALIZE_ALL_COMPOSE;

    function g_utf8_normalize(str:Pgchar; len:gssize; mode:TGNormalizeMode):Pgchar;cdecl;external gliblib name 'g_utf8_normalize';

    function g_utf8_collate(str1:Pgchar; str2:Pgchar):gint;cdecl;external gliblib name 'g_utf8_collate';

    function g_utf8_collate_key(str:Pgchar; len:gssize):Pgchar;cdecl;external gliblib name 'g_utf8_collate_key';
{$ENDIF read_interface_rest}
