// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGMarkupError = ^TGMarkupError;
  TGMarkupError = (
    G_MARKUP_ERROR_BAD_UTF8,
    G_MARKUP_ERROR_EMPTY,
    G_MARKUP_ERROR_PARSE,
    G_MARKUP_ERROR_UNKNOWN_ELEMENT,
    G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE,
    G_MARKUP_ERROR_INVALID_CONTENT
  );

  PGMarkupParseFlags = ^TGMarkupParseFlags;
  TGMarkupParseFlags = integer;

  PGMarkupParseContext = ^TGMarkupParseContext;
  TGMarkupParseContext = pointer;                // please correct me if it's wrong


  PGMarkupParser = ^TGMarkupParser;
  TGMarkupParser = record
    start_element : procedure (context          : PGMarkupParseContext;
                               element_name     : Pgchar;
                               attribute_names  : PPgchar;
                               attribute_values : PPgchar;
                               user_data        : gpointer; error:PPGError);cdecl;
    end_element   : procedure (context          : PGMarkupParseContext;
                               element_name     : Pgchar;
                               user_data        : gpointer;
                               error            : PPGError); cdecl;
    text          : procedure (context          : PGMarkupParseContext;
                               text             : Pgchar;
                               text_len         : gsize;
                               user_data        : gpointer;
                               error            : PPGError); cdecl;
    passthrough   : procedure (context          : PGMarkupParseContext;
                               passthrough_text : Pgchar;
                               text_len         : gsize;
                               user_data        : gpointer;
                               error            : PPGError); cdecl;
    error          : procedure (context         : PGMarkupParseContext;
                                error           : PGError;
                                user_data       : gpointer); cdecl;
    end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function G_MARKUP_ERROR : TGQuark;

function g_markup_error_quark:TGQuark;cdecl;external gliblib name 'g_markup_error_quark';

const
  G_MARKUP_DO_NOT_USE_THIS_UNSUPPORTED_FLAG = 1 shl 0;


function  g_markup_parse_context_new (parser            : PGMarkupParser;
                                      flags             : TGMarkupParseFlags;
                                      user_data         : gpointer;
                                      user_data_dnotify : TGDestroyNotify): PGMarkupParseContext;  cdecl;external gliblib name 'g_markup_parse_context_new';

procedure g_markup_parse_context_free(context           : PGMarkupParseContext);cdecl;external gliblib name 'g_markup_parse_context_free';
function  g_markup_parse_context_parse(context          : PGMarkupParseContext;
                                       text             : Pgchar;
                                       text_len         : gssize;
                                       error            : PPGError):gboolean;cdecl;external gliblib name 'g_markup_parse_context_parse';

function  g_markup_parse_context_end_parse(context      : PGMarkupParseContext;
                                           error        : PPGError):gboolean;cdecl;external gliblib name 'g_markup_parse_context_end_parse';
procedure g_markup_parse_context_get_position(context     : PGMarkupParseContext;
                                              line_number : Pgint;
                                              char_number : Pgint);cdecl;external gliblib name 'g_markup_parse_context_get_position';
function g_markup_escape_text (text   : Pgchar;
                               length : gssize):Pgchar;cdecl;external gliblib name 'g_markup_escape_text';

{$ENDIF read_interface_rest}
// included by glib2.pas
