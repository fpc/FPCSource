{*
 * gmarkup.inc
 *
 * depends on gerror.inc
 *}


  { These three are primarily intended for specific GMarkupParser
       implementations to set.
      }
  type

     PGMarkupError = ^TGMarkupError;
     TGMarkupError = (G_MARKUP_ERROR_BAD_UTF8,
                      G_MARKUP_ERROR_EMPTY,
                      G_MARKUP_ERROR_PARSE,
                      G_MARKUP_ERROR_UNKNOWN_ELEMENT,
                      G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE,
                      G_MARKUP_ERROR_INVALID_CONTENT);


  function G_MARKUP_ERROR : TGQuark;


  function g_markup_error_quark:TGQuark;cdecl;external gliblib name 'g_markup_error_quark';

  type
     PGMarkupParseFlags = ^TGMarkupParseFlags;
     TGMarkupParseFlags = integer;

  const
     { Hmm, can't think of any at the moment  }
     G_MARKUP_DO_NOT_USE_THIS_UNSUPPORTED_FLAG = 1 shl 0;

  type
     PGMarkupParseContext = ^TGMarkupParseContext;
     TGMarkupParseContext = pointer;                // please correct me if it's wrong


     PGMarkupParser = ^TGMarkupParser;
     TGMarkupParser = record

       { Called for open tags <foo bar="baz">  }
          start_element : procedure (context          : PGMarkupParseContext;
                                     element_name     : Pgchar;
                                     attribute_names  : PPgchar;
                                     attribute_values : PPgchar;
                                     user_data        : gpointer; error:PPGError);cdecl;

       { Called for close tags </foo>  }
          end_element   : procedure (context          : PGMarkupParseContext;
                                     element_name     : Pgchar;
                                     user_data        : gpointer;
                                     error            : PPGError); cdecl;

       { Called for character data  }
       { text is not nul-terminated  }
          text          : procedure (context          : PGMarkupParseContext;
                                     text             : Pgchar;
                                     text_len         : gsize;
                                     user_data        : gpointer;
                                     error            : PPGError); cdecl;

  { Called for strings that should be re-saved verbatim in this same
       position, but are not otherwise interpretable.  At the moment
       this includes comments and processing instructions.
      }
  { text is not nul-terminated.  }

          passthrough   : procedure (context          : PGMarkupParseContext;
                                     passthrough_text : Pgchar;
                                     text_len         : gsize;
                                     user_data        : gpointer;
                                     error            : PPGError); cdecl;

  { Called on error, including one set by other
       methods in the vtable. The GError should not be freed.
      }
          error          : procedure (context         : PGMarkupParseContext;
                                      error           : PGError;
                                      user_data       : gpointer); cdecl;
       end;



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

  { For user-constructed error messages, has no precise semantics  }
  procedure g_markup_parse_context_get_position(context     : PGMarkupParseContext;
                                                line_number : Pgint;
                                                char_number : Pgint);cdecl;external gliblib name 'g_markup_parse_context_get_position';

  { useful when saving  }
  function g_markup_escape_text (text   : Pgchar;
                                 length : gssize):Pgchar;cdecl;external gliblib name 'g_markup_escape_text';



