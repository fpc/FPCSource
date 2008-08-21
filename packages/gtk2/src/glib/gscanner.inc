// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGTokenType = ^TGTokenType;
  TGTokenType = gint;

  PGScanner = ^TGScanner;
  PGScannerConfig = ^TGScannerConfig;

  PGTokenValue = ^TGTokenValue;
  TGTokenValue = record
    case longint of
       0 : ( v_symbol : gpointer );
       1 : ( v_identifier : Pgchar );
       2 : ( v_binary  : gulong );
       3 : ( v_octal   : gulong );
       4 : ( v_int     : gulong );
       5 : ( v_float   : gdouble );
       6 : ( v_hex     : gulong );
       7 : ( v_string  : Pgchar );
       8 : ( v_comment : Pgchar );
       9 : ( v_char    : guchar );
       10 : ( v_error  : guint );
    end;

  TGScannerMsgFunc = procedure (scanner:PGScanner; message:Pgchar; error:gboolean);cdecl;

  TGScanner = record
    user_data         : gpointer;          { unused fields  }
    max_parse_errors  : guint;

    parse_errors      : guint;             { g_scanner_error() increments this field  }

    input_name        : Pgchar;            { name of input stream, featured by
                                             the default message handler  }

    qdata             : PGData;            { quarked data  }

    config            : PGScannerConfig;   { link into the scanner configuration  }

    token             : TGTokenType;       { fields filled in after g_scanner_get_next_token()  }
    value             : TGTokenValue;
    line              : guint;
    position          : guint;

    next_token        : TGTokenType;       { fields filled in after g_scanner_peek_next_token()  }
    next_value        : TGTokenValue;
    next_line         : guint;
    next_position     : guint;

    symbol_table      : PGHashTable;       { to be considered private  }
    input_fd          : gint;
    text              : Pgchar;
    text_end          : Pgchar;
    buffer            : Pgchar;
    scope_id          : guint;

    msg_handler       : TGScannerMsgFunc;  { handler function for _warn and _error  }
  end;

  TGScannerConfig = record
   { Character sets  }
    cset_skip_characters   : Pgchar;        { default: " \t\n"  }

    cset_identifier_first  : Pgchar;
    cset_identifier_nth    : Pgchar;
    cpair_comment_single   : Pgchar;        { default: "#\n"  }

    flag0                  : longint;    { Boolean values to be adjusted "on the fly"
                                           to configure scanning behaviour. }
    padding_dummy          : guint;
  end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  G_TOKEN_LEFT_PAREN          = 40;  // ASCII Code of '('
  G_TOKEN_RIGHT_PAREN         = 41;  // ASCII Code of ')'
  G_TOKEN_LEFT_CURLY          = 123; // ASCII Code of '{'
  G_TOKEN_RIGHT_CURLY         = 125; // ASCII Code of '}'
  G_TOKEN_LEFT_BRACE          = 91;  // ASCII Code of '['
  G_TOKEN_RIGHT_BRACE         = 93;  // ASCII Code of ']'
  G_TOKEN_EQUAL_SIGN          = 61;  // ASCII Code of '='
  G_TOKEN_COMMA               = 44;  // ASCII Code of ','
  G_TOKEN_NONE                = 256;
  G_TOKEN_ERROR               = 257;
  G_TOKEN_CHAR                = 258;
  G_TOKEN_OCTAL               = 260;
  G_TOKEN_INT                 = 261;
  G_TOKEN_HEX                 = 262;
  G_TOKEN_FLOAT               = 263;
  G_TOKEN_STRING              = 264;
  G_TOKEN_SYMBOL              = 265;
  G_TOKEN_IDENTIFIER          = 266;
  G_TOKEN_IDENTIFIER_NULL     = 267;
  G_TOKEN_COMMENT_SINGLE      = 268;
  G_TOKEN_COMMENT_MULTI       = 269;
  G_TOKEN_LAST                = 270;

{ Character sets  }

const
  G_CSET_A_2_Z_UCASE  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';        // NOTE: renamed
  G_CSET_a_2_z_lcase  = 'abcdefghijklmnopqrstuvwxyz';        //  dito
  G_CSET_DIGITS = '0123456789';
{

  How to express an escape sequenced string in pascal???

     G_CSET_LATINC = #300 + #301 + #302 + #303 + #304 + #305 + #306 + #307 + #310 + #311 +
                     #312 + #313 + #314 + #315 + #316 + #317 + #320 + #321 + #322 + #323 +
                     #324 + #325 + #326 + #330 + #331 + #332 + #333 + #334 + #335 + #336;

     G_CSET_LATINS = #337#340#341#342#343#344#345#346#347#350#351#352#353#354#355#356#357#360
                     #361#362#363#364#365#366#370#371#372#373#374#375#376#377;

  This is the original version:

  #define G_CSET_LATINC "\300\301\302\303\304\305\306"\
                        "\307\310\311\312\313\314\315\316\317\320"\
                        "\321\322\323\324\325\326"\
                        "\330\331\332\333\334\335\336"
  #define G_CSET_LATINS "\337\340\341\342\343\344\345\346"\
                        "\347\350\351\352\353\354\355\356\357\360"\
                        "\361\362\363\364\365\366"\
                        "\370\371\372\373\374\375\376\377"

}

const
  bm_TGScannerConfig_case_sensitive = $1;
  bp_TGScannerConfig_case_sensitive = 0;
  bm_TGScannerConfig_skip_comment_multi = $2;
  bp_TGScannerConfig_skip_comment_multi = 1;
  bm_TGScannerConfig_skip_comment_single = $4;
  bp_TGScannerConfig_skip_comment_single = 2;
  bm_TGScannerConfig_scan_comment_multi = $8;
  bp_TGScannerConfig_scan_comment_multi = 3;
  bm_TGScannerConfig_scan_identifier = $10;
  bp_TGScannerConfig_scan_identifier = 4;
  bm_TGScannerConfig_scan_identifier_1char = $20;
  bp_TGScannerConfig_scan_identifier_1char = 5;
  bm_TGScannerConfig_scan_identifier_NULL = $40;
  bp_TGScannerConfig_scan_identifier_NULL = 6;
  bm_TGScannerConfig_scan_symbols = $80;
  bp_TGScannerConfig_scan_symbols = 7;
  bm_TGScannerConfig_scan_binary = $100;
  bp_TGScannerConfig_scan_binary = 8;
  bm_TGScannerConfig_scan_octal = $200;
  bp_TGScannerConfig_scan_octal = 9;
  bm_TGScannerConfig_scan_float = $400;
  bp_TGScannerConfig_scan_float = 10;
  bm_TGScannerConfig_scan_hex = $800;
  bp_TGScannerConfig_scan_hex = 11;
  bm_TGScannerConfig_scan_hex_dollar = $1000;
  bp_TGScannerConfig_scan_hex_dollar = 12;
  bm_TGScannerConfig_scan_string_sq = $2000;
  bp_TGScannerConfig_scan_string_sq = 13;
  bm_TGScannerConfig_scan_string_dq = $4000;
  bp_TGScannerConfig_scan_string_dq = 14;
  bm_TGScannerConfig_numbers_2_int = $8000;
  bp_TGScannerConfig_numbers_2_int = 15;
  bm_TGScannerConfig_int_2_float = $10000;
  bp_TGScannerConfig_int_2_float = 16;
  bm_TGScannerConfig_identifier_2_string = $20000;
  bp_TGScannerConfig_identifier_2_string = 17;
  bm_TGScannerConfig_char_2_token = $40000;
  bp_TGScannerConfig_char_2_token = 18;
  bm_TGScannerConfig_symbol_2_token = $80000;
  bp_TGScannerConfig_symbol_2_token = 19;
  bm_TGScannerConfig_scope_0_fallback = $100000;
  bp_TGScannerConfig_scope_0_fallback = 20;

{ Should symbol lookup work case sensitive?  }
function TGScannerConfig_case_sensitive(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_case_sensitive(var a : TGScannerConfig; __case_sensitive : guint);

{ C-like comment  }
function TGScannerConfig_skip_comment_multi(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_skip_comment_multi(var a : TGScannerConfig; __skip_comment_multi : guint);

{ single line comment  }
function TGScannerConfig_skip_comment_single(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_skip_comment_single(var a : TGScannerConfig; __skip_comment_single : guint);

{ scan multi line comments?  }
function TGScannerConfig_scan_comment_multi(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_comment_multi(var a : TGScannerConfig; __scan_comment_multi : guint);

function TGScannerConfig_scan_identifier(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_identifier(var a : TGScannerConfig; __scan_identifier : guint);

function TGScannerConfig_scan_identifier_1char(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_identifier_1char(var a : TGScannerConfig; __scan_identifier_1char : guint);

function TGScannerConfig_scan_identifier_NULL(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_identifier_NULL(var a : TGScannerConfig; __scan_identifier_NULL : guint);

function TGScannerConfig_scan_symbols(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_symbols(var a : TGScannerConfig; __scan_symbols : guint);

function TGScannerConfig_scan_binary(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_binary(var a : TGScannerConfig; __scan_binary : guint);

function TGScannerConfig_scan_octal(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_octal(var a : TGScannerConfig; __scan_octal : guint);

function TGScannerConfig_scan_float(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_float(var a : TGScannerConfig; __scan_float : guint);

{ `0x0ff0'  }
function TGScannerConfig_scan_hex(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_hex(var a : TGScannerConfig; __scan_hex : guint);

{ `$0ff0'  }
function TGScannerConfig_scan_hex_dollar(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_hex_dollar(var a : TGScannerConfig; __scan_hex_dollar : guint);

{ string: 'anything'  }
function TGScannerConfig_scan_string_sq(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_string_sq(var a : TGScannerConfig; __scan_string_sq : guint);

{ string: "\\-escapes!\n"  }
function TGScannerConfig_scan_string_dq(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scan_string_dq(var a : TGScannerConfig; __scan_string_dq : guint);

{ bin, octal, hex => int  }
function TGScannerConfig_numbers_2_int(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_numbers_2_int(var a : TGScannerConfig; __numbers_2_int : guint);

{ int => G_TOKEN_FLOAT?  }
function TGScannerConfig_int_2_float(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_int_2_float(var a : TGScannerConfig; __int_2_float : guint);

function TGScannerConfig_identifier_2_string(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_identifier_2_string(var a : TGScannerConfig; __identifier_2_string : guint);

{ return G_TOKEN_CHAR?  }
function TGScannerConfig_char_2_token(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_char_2_token(var a : TGScannerConfig; __char_2_token : guint);

function TGScannerConfig_symbol_2_token(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_symbol_2_token(var a : TGScannerConfig; __symbol_2_token : guint);

{ try scope 0 on lookups?  }
function TGScannerConfig_scope_0_fallback(var a : TGScannerConfig) : guint;
procedure TGScannerConfig_set_scope_0_fallback(var a : TGScannerConfig; __scope_0_fallback : guint);


function  g_scanner_new(config_templ:PGScannerConfig):PGScanner;cdecl;external gliblib name 'g_scanner_new';
procedure g_scanner_destroy(scanner:PGScanner);cdecl;external gliblib name 'g_scanner_destroy';
procedure g_scanner_input_file(scanner:PGScanner; input_fd:gint);cdecl;external gliblib name 'g_scanner_input_file';
procedure g_scanner_sync_file_offset(scanner:PGScanner);cdecl;external gliblib name 'g_scanner_sync_file_offset';
procedure g_scanner_input_text(scanner:PGScanner; text:Pgchar; text_len:guint);cdecl;external gliblib name 'g_scanner_input_text';
function  g_scanner_get_next_token(scanner:PGScanner):TGTokenType;cdecl;external gliblib name 'g_scanner_get_next_token';
function  g_scanner_peek_next_token(scanner:PGScanner):TGTokenType;cdecl;external gliblib name 'g_scanner_peek_next_token';
function  g_scanner_cur_token(scanner:PGScanner):TGTokenType;cdecl;external gliblib name 'g_scanner_cur_token';
function  g_scanner_cur_value(scanner:PGScanner):TGTokenValue;cdecl;external gliblib name 'g_scanner_cur_value';
function  g_scanner_cur_line(scanner:PGScanner):guint;cdecl;external gliblib name 'g_scanner_cur_line';
function  g_scanner_cur_position(scanner:PGScanner):guint;cdecl;external gliblib name 'g_scanner_cur_position';
function  g_scanner_eof(scanner:PGScanner):gboolean;cdecl;external gliblib name 'g_scanner_eof';
function  g_scanner_set_scope(scanner:PGScanner; scope_id:guint):guint;cdecl;external gliblib name 'g_scanner_set_scope';
procedure g_scanner_scope_add_symbol(scanner:PGScanner; scope_id:guint; symbol:Pgchar; value:gpointer);cdecl;external gliblib name 'g_scanner_scope_add_symbol';
procedure g_scanner_scope_remove_symbol(scanner:PGScanner; scope_id:guint; symbol:Pgchar);cdecl;external gliblib name 'g_scanner_scope_remove_symbol';
function  g_scanner_scope_lookup_symbol(scanner:PGScanner; scope_id:guint; symbol:Pgchar):gpointer;cdecl;external gliblib name 'g_scanner_scope_lookup_symbol';
procedure g_scanner_scope_foreach_symbol(scanner:PGScanner; scope_id:guint; func:TGHFunc; user_data:gpointer);cdecl;external gliblib name 'g_scanner_scope_foreach_symbol';
function g_scanner_lookup_symbol(scanner:PGScanner; symbol:Pgchar):gpointer;cdecl;external gliblib name 'g_scanner_lookup_symbol';
procedure g_scanner_unexp_token(scanner:PGScanner; expected_token:TGTokenType; identifier_spec:Pgchar; symbol_spec:Pgchar; symbol_name:Pgchar;
            _message:Pgchar; is_error:gint);cdecl;external gliblib name 'g_scanner_unexp_token';


{$IFNDEF KYLIX}
procedure g_scanner_error(scanner:PGScanner; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_scanner_error';
procedure g_scanner_error(scanner:PGScanner; format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_scanner_error';

procedure g_scanner_warn(scanner:PGScanner; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_scanner_warn';
procedure g_scanner_warn(scanner:PGScanner; format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_scanner_warn';
{$ELSE}
procedure g_scanner_error(scanner:PGScanner; format:Pgchar);varargs;cdecl;external gliblib name 'g_scanner_error';
procedure g_scanner_warn(scanner:PGScanner; format:Pgchar);varargs;cdecl;external gliblib name 'g_scanner_warn';
{$ENDIF}

{$ifndef G_DISABLE_DEPRECATED}
procedure g_scanner_freeze_symbol_table(scanner : PGScanner);
procedure g_scanner_thaw_symbol_table(scanner : PGScanner);
{$endif} { G_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}
// included by glib2.pas

