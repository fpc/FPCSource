{*
 * gshell.inc
 *
 * depends on gerror.inc
 *}

  function G_SHELL_ERROR : TGQuark;





  type

     PGShellError = ^TGShellError;
     TGShellError = (G_SHELL_ERROR_BAD_QUOTING,   { mismatched or otherwise mangled quoting  }
                     G_SHELL_ERROR_EMPTY_STRING,  { string to be parsed was empty  }
                     G_SHELL_ERROR_FAILED);

  function g_shell_error_quark:TGQuark;cdecl;external gliblib name 'g_shell_error_quark';

  function g_shell_quote(unquoted_string:Pgchar):Pgchar;cdecl;external gliblib name 'g_shell_quote';

  function g_shell_unquote(quoted_string:Pgchar; error:PPGError):Pgchar;cdecl;external gliblib name 'g_shell_unquote';

  function g_shell_parse_argv(command_line:Pgchar; argcp:Pgint; argvp:PPPgchar; error:PPGError):gboolean;cdecl;external gliblib name 'g_shell_parse_argv';


