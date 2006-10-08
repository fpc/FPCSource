// included by glib2.pas

{ Functions like the ones in <ctype.h> that are not affected by locale.  }
type
   PGAsciiType = ^TGAsciiType;
   TGAsciiType = longint;

const
   G_ASCII_ALNUM = 1 shl 0;
   G_ASCII_ALPHA = 1 shl 1;
   G_ASCII_CNTRL = 1 shl 2;
   G_ASCII_DIGIT = 1 shl 3;
   G_ASCII_GRAPH = 1 shl 4;
   G_ASCII_LOWER = 1 shl 5;
   G_ASCII_PRINT = 1 shl 6;
   G_ASCII_PUNCT = 1 shl 7;
   G_ASCII_SPACE = 1 shl 8;
   G_ASCII_UPPER = 1 shl 9;
   G_ASCII_XDIGIT= 1 shl 10;

{$IFNDEF KYLIX}
var
   g_ascii_table : Pguint16; external gliblib name 'g_ascii_table';
{$ENDIF}

function g_ascii_isalnum(c : gchar) : boolean;
function g_ascii_isalpha(c : gchar) : boolean;
function g_ascii_iscntrl(c : gchar) : boolean;
function g_ascii_isdigit(c : gchar) : boolean;
function g_ascii_isgraph(c : gchar) : boolean;
function g_ascii_islower(c : gchar) : boolean;
function g_ascii_isprint(c : gchar) : boolean;
function g_ascii_ispunct(c : gchar) : boolean;
function g_ascii_isspace(c : gchar) : boolean;
function g_ascii_isupper(c : gchar) : boolean;
function g_ascii_isxdigit(c : gchar) : boolean;

function g_ascii_tolower(c:gchar):gchar; cdecl; external gliblib;
function g_ascii_toupper(c:gchar):gchar; cdecl; external gliblib;
function g_ascii_digit_value(c:gchar):gint; cdecl; external gliblib;
function g_ascii_xdigit_value(c:gchar):gint; cdecl; external gliblib;


{ String utility functions that modify a string argument or
   return a constant string that must not be freed. }

const
   G_STR_DELIMITERS = '_-|> <.';

function g_strdelimit(_string:Pgchar; delimiters:Pgchar; new_delimiter:gchar):Pgchar; cdecl; external gliblib;
function g_strcanon(_string:Pgchar; valid_chars:Pgchar; substitutor:gchar):Pgchar; cdecl; external gliblib;
function g_strerror(errnum:gint):Pgchar; cdecl; external gliblib;
function g_strsignal(signum:gint):Pgchar; cdecl; external gliblib;
function g_strreverse(_string:Pgchar):Pgchar; cdecl; external gliblib;
function g_strlcpy(dest:Pgchar; src:Pgchar; dest_size:gsize):gsize; cdecl; external gliblib;
function g_strlcat(dest:Pgchar; src:Pgchar; dest_size:gsize):gsize; cdecl; external gliblib;
function g_strstr_len(haystack:Pgchar; haystack_len:gssize; needle:Pgchar):Pgchar; cdecl; external gliblib;
function g_strrstr(haystack:Pgchar; needle:Pgchar):Pgchar; cdecl; external gliblib;
function g_strrstr_len(haystack:Pgchar; haystack_len:gssize; needle:Pgchar):Pgchar; cdecl; external gliblib;
function g_str_has_suffix(str:Pgchar; suffix:Pgchar):gboolean; cdecl; external gliblib;
function g_str_has_prefix(str:Pgchar; prefix:Pgchar):gboolean; cdecl; external gliblib;
{ String to/from double conversion functions  }
function g_strtod(nptr:Pgchar; endptr:PPgchar):gdouble; cdecl; external gliblib;
function g_ascii_strtod(nptr:Pgchar; endptr:PPgchar):gdouble; cdecl; external gliblib;

{ 29 bytes should enough for all possible values that
   g_ascii_dtostr can produce.
   Then add 10 for good measure  }
const
   G_ASCII_DTOSTR_BUF_SIZE = 29 + 10;

function g_ascii_dtostr(buffer:Pgchar; buf_len:gint; d:gdouble):Pgchar; cdecl; external gliblib;
function g_ascii_formatd(buffer:Pgchar; buf_len:gint; format:Pgchar; d:gdouble):Pgchar; cdecl; external gliblib;
{ removes leading spaces  }
function g_strchug(_string:Pgchar):Pgchar; cdecl; external gliblib;
{ removes trailing spaces  }
function g_strchomp(_string:Pgchar):Pgchar; cdecl; external gliblib;
{ removes leading & trailing spaces  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function g_strstrip(_string : PGChar) : PGChar;

function g_ascii_strcasecmp(s1:Pgchar; s2:Pgchar):gint; cdecl; external gliblib;
function g_ascii_strncasecmp(s1:Pgchar; s2:Pgchar; n:gsize):gint; cdecl; external gliblib;
function g_ascii_strdown(str:Pgchar; len:gssize):Pgchar; cdecl; external gliblib;
function g_ascii_strup(str:Pgchar; len:gssize):Pgchar; cdecl; external gliblib;

{$ifndef G_DISABLE_DEPRECATED}
{ The following four functions are deprecated and will be removed in
   the next major release. They use the locale-specific tolower and
   toupper, which is almost never the right thing.
  }
function g_strcasecmp(s1:Pgchar; s2:Pgchar):gint; cdecl; external gliblib;
function g_strncasecmp(s1:Pgchar; s2:Pgchar; n:guint):gint; cdecl; external gliblib;
function g_strdown(_string:Pgchar):Pgchar; cdecl; external gliblib;
function g_strup(_string:Pgchar):Pgchar; cdecl; external gliblib;
{$endif}
{ G_DISABLE_DEPRECATED  }

{ String utility functions that return a newly allocated string which
   ought to be freed with g_free from the caller at some point.
  }
function g_strdup(str:Pgchar):Pgchar; cdecl; external gliblib;
{$IFNDEF KYLIX}
function g_strdup_printf(format:Pgchar; args:array of const):Pgchar; cdecl; overload; external gliblib;
function g_strdup_printf(format:Pgchar):Pgchar; cdecl; overload; varargs; external gliblib;
function g_strdup_vprintf(format:Pgchar; args: array of const):Pgchar; cdecl; external gliblib;
{$ELSE}
function g_strdup_printf(format:Pgchar):Pgchar; varargs; cdecl; external gliblib;
function g_strdup_vprintf(format:Pgchar):Pgchar; varargs; cdecl; external gliblib;
{$ENDIF}
function g_strndup(str:Pgchar; n:gsize):Pgchar; cdecl; external gliblib;
function g_strnfill(length:gsize; fill_char:gchar):Pgchar; cdecl; external gliblib;
{$IFNDEF KYLIX}
function g_strconcat(string1:Pgchar; args:array of const):Pgchar; cdecl; overload; external gliblib;
function g_strconcat(string1:Pgchar):Pgchar; cdecl; overload; varargs; external gliblib;
{ NULL terminated  }
function g_strjoin(separator:Pgchar; args:array of const):Pgchar; cdecl; overload; external gliblib;
function g_strjoin(separator:Pgchar):Pgchar; cdecl; overload; varargs; external gliblib;
{$ELSE}
function g_strconcat(string1:Pgchar):Pgchar; varargs; cdecl; external gliblib;
function g_strjoin(separator:Pgchar):Pgchar; varargs; cdecl; external gliblib;
{$ENDIF}
{ NULL terminated  }
{ Make a copy of a string interpreting C string -style escape
   sequences. Inverse of g_strescape. The recognized sequences are \b
   \f \n \r \t \\ \" and the octal format.
  }
function g_strcompress(source:Pgchar):Pgchar; cdecl; external gliblib;
{ Copy a string escaping nonprintable characters like in C strings.
   Inverse of g_strcompress. The exceptions parameter, if non-NULL, points
   to a string containing characters that are not to be escaped.

   Deprecated API: gchar  g_strescape (const gchar  source);
   Luckily this function wasn't used much, using NULL as second parameter
   provides mostly identical semantics.
  }
function g_strescape(source:Pgchar; exceptions:Pgchar):Pgchar; cdecl; external gliblib;
function g_memdup(mem:gconstpointer; byte_size:guint):gpointer; cdecl; external gliblib;
{ NULL terminated string arrays.
   g_strsplit() splits up string into max_tokens tokens at delim and
   returns a newly allocated string array.
   g_strjoinv() concatenates all of str_array's strings, sliding in an
   optional separator, the returned string is newly allocated.
   g_strfreev() frees the array itself and all of its strings.
   g_strdupv() copies a NULL-terminated array of strings
  }
function g_strsplit(_string:Pgchar; delimiter:Pgchar; max_tokens:gint):PPgchar; cdecl; external gliblib;
function g_strjoinv(separator:Pgchar; str_array:PPgchar):Pgchar; cdecl; external gliblib;
procedure g_strfreev(str_array:PPgchar); cdecl; external gliblib;
function g_strdupv(str_array:PPgchar):PPgchar; cdecl; external gliblib;
function g_stpcpy(dest:Pgchar; src:Pchar):Pgchar; cdecl; external gliblib;


// included by glib2.pas

