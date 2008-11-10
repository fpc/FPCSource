// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGFileError = ^TGFileError;
  TGFileError =  gint;

  PGFileTest = ^TGFileTest;
  TGFileTest = integer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  G_FILE_TEST_IS_REGULAR = 1 shl 0;
  G_FILE_TEST_IS_SYMLINK = 1 shl 1;
  G_FILE_TEST_IS_DIR = 1 shl 2;
  G_FILE_TEST_IS_EXECUTABLE = 1 shl 3;
  G_FILE_TEST_EXISTS = 1 shl 4;

const
  G_FILE_ERROR_EXIST       = 0;
  G_FILE_ERROR_ISDIR       = 1;
  G_FILE_ERROR_ACCES       = 2;
  G_FILE_ERROR_NAMETOOLONG = 3;
  G_FILE_ERROR_NOENT       = 4;
  G_FILE_ERROR_NOTDIR      = 5;
  G_FILE_ERROR_NXIO        = 6;
  G_FILE_ERROR_NODEV       = 7;
  G_FILE_ERROR_ROFS        = 8;
  G_FILE_ERROR_TXTBSY      = 9;
  G_FILE_ERROR_FAULT       = 10;
  G_FILE_ERROR_LOOP        = 11;
  G_FILE_ERROR_NOSPC       = 12;
  G_FILE_ERROR_NOMEM       = 13;
  G_FILE_ERROR_MFILE       = 14;
  G_FILE_ERROR_NFILE       = 15;
  G_FILE_ERROR_BADF        = 16;
  G_FILE_ERROR_INVAL       = 17;
  G_FILE_ERROR_PIPE        = 18;
  G_FILE_ERROR_AGAIN       = 19;
  G_FILE_ERROR_INTR        = 20;
  G_FILE_ERROR_IO          = 21;
  G_FILE_ERROR_PERM        = 22;
  G_FILE_ERROR_FAILED      = 23;

function G_FILE_ERROR: TGQuark;

function g_file_error_quark:TGQuark;cdecl;external gliblib name 'g_file_error_quark';
function g_file_error_from_errno(err_no:gint):TGFileError;cdecl;external gliblib name 'g_file_error_from_errno';
function g_file_test(filename:Pgchar; test:TGFileTest):gboolean;cdecl;external gliblib name 'g_file_test';
function g_file_get_contents(filename:Pgchar; contents:PPgchar; length:Pgsize; error:PPGError):gboolean;cdecl;external gliblib name 'g_file_get_contents';
function g_mkstemp(tmpl:Pchar):longint;cdecl;external gliblib name 'g_mkstemp';
function g_file_open_tmp(tmpl:Pchar; name_used:PPchar; error:PPGError):longint;cdecl;external gliblib name 'g_file_open_tmp';

{$IFNDEF KYLIX}
  function g_build_path(separator:Pgchar; first_element:Pgchar; args:array of const):Pgchar;cdecl;overload;external gliblib name 'g_build_path';
  function g_build_path(separator:Pgchar; first_element:Pgchar):Pgchar;cdecl;overload;varargs;external gliblib name 'g_build_path';
  function g_build_filename(first_element:Pgchar; args:array of const):Pgchar;cdecl;overload;external gliblib name 'g_build_filename';
  function g_build_filename(first_element:Pgchar):Pgchar;cdecl;overload;varargs;external gliblib name 'g_build_filename';
{$ELSE}
  function g_build_path(separator:Pgchar; first_element:Pgchar):Pgchar;varargs;cdecl;external gliblib name 'g_build_path';
  function g_build_filename(first_element:Pgchar):Pgchar;varargs;cdecl;external gliblib name 'g_build_filename';
{$ENDIF}

{$ENDIF read_interface_rest}
// included by glib2.pas



