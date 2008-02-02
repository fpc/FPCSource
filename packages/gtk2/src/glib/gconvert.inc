// {$include <stddef.h>      /* For size_t */}
// {$include <glib/gerror.h>}

  type
     PGConvertError = ^TGConvertError;
     TGConvertError = (G_CONVERT_ERROR_NO_CONVERSION,
                       G_CONVERT_ERROR_ILLEGAL_SEQUENCE,
                       G_CONVERT_ERROR_FAILED,
                       G_CONVERT_ERROR_PARTIAL_INPUT,
                       G_CONVERT_ERROR_BAD_URI,
                       G_CONVERT_ERROR_NOT_ABSOLUTE_PATH
       );

    function G_CONVERT_ERROR : TGQuark;


    function g_convert_error_quark:TGQuark;cdecl;external gliblib name 'g_convert_error_quark';

{ Thin wrappers around iconv  }
    type
       PGIConv = ^TGIConv;
       TGIConv  = pointer;

    function g_iconv_open(to_codeset:Pgchar; from_codeset:Pgchar):TGIConv;cdecl;external gliblib name 'g_iconv_open';

    function g_iconv(converter:TGIConv; inbuf:PPgchar; inbytes_left:Pgsize; outbuf:PPgchar; outbytes_left:Pgsize):gsize;cdecl;external gliblib name 'g_iconv';

    function g_iconv_close(converter:TGIConv):gint;cdecl;external gliblib name 'g_iconv_close';

    function g_convert(str:Pgchar; len:gssize; to_codeset:Pgchar; from_codeset:Pgchar; bytes_read:Pgsize;
                       bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_convert';

    function g_convert_with_iconv(str:Pgchar; len:gssize; converter:TGIConv; bytes_read:Pgsize; bytes_written:Pgsize;
                                  error:PPGError):Pgchar;cdecl;external gliblib name 'g_convert_with_iconv';

    function g_convert_with_fallback(str:Pgchar; len:gssize; to_codeset:Pgchar; from_codeset:Pgchar; fallback:Pgchar;
                                     bytes_read:Pgsize; bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_convert_with_fallback';

{ Convert between libc's idea of strings and UTF-8. }

    function g_locale_to_utf8(opsysstring:Pgchar; len:gssize; bytes_read:Pgsize; bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_locale_to_utf8';

    function g_locale_from_utf8(utf8string:Pgchar; len:gssize; bytes_read:Pgsize; bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_locale_from_utf8';

    { Convert between the operating system (or C runtime)
       representation of file names and UTF-8.
      }
    function g_filename_to_utf8(opsysstring:Pgchar; len:gssize; bytes_read:Pgsize; bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_filename_to_utf8';

    function g_filename_from_utf8(utf8string:Pgchar; len:gssize; bytes_read:Pgsize; bytes_written:Pgsize; error:PPGError):Pgchar;cdecl;external gliblib name 'g_filename_from_utf8';

    function g_filename_from_uri(uri:Pchar; hostname:PPchar; error:PPGError):Pgchar;cdecl;external gliblib name 'g_filename_from_uri';

    function g_filename_to_uri(filename:Pchar; hostname:Pchar; error:PPGError):Pgchar;cdecl;external gliblib name 'g_filename_to_uri';


