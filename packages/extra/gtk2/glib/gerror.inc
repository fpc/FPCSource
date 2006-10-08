{$ifndef __G_ERROR_H__}
{$define __G_ERROR_H__}

//{$include gquark.inc}

  type
    PPGError = ^PGError;
    PGError  = ^TGError;
    TGError  = record
            domain  : TGQuark;
            code    : gint;
            message : Pgchar;
         end;

    function  g_error_new(domain:TGQuark; code:gint; format:Pgchar):PGError;varargs;cdecl;external gliblib name 'g_error_new_literal';

    function  g_error_new_literal(domain:TGQuark; code:gint; message:Pgchar):PGError;cdecl;external gliblib name 'g_error_new_literal';

    procedure g_error_free(error:PGError);cdecl;external gliblib name 'g_error_free';

    function  g_error_copy(error:PGError):PGError;cdecl;external gliblib name 'g_error_copy';

    function  g_error_matches(error:PGError; domain:TGQuark; code:gint):gboolean;cdecl;external gliblib name 'g_error_matches';

    { if (err)  err = g_error_new(domain, code, format, ...), also has
      some sanity checks.
    }

    procedure g_set_error  (err: PPGError; domain: TGQuark; code: gint; format: Pgchar);varargs;cdecl;external gliblib name 'g_set_error';

    { if dest then dest := src; also has some sanity checks. }
    procedure g_propagate_error(dest:PPGError; src:PGError);cdecl;external gliblib name 'g_propagate_error';

    { if (err &&  err) then g_error_free( err);  err = NULL;   }
    procedure g_clear_error(err:PPGError);cdecl;external gliblib name 'g_clear_error';

{$endif}    { __G_ERROR_H__  }

