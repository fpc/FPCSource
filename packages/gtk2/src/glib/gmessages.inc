// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGLogLevelFlags = ^TGLogLevelFlags;
  TGLogLevelFlags = longint;

  TGLogFunc = procedure (log_domain:Pgchar; log_level:TGLogLevelFlags; TheMessage:Pgchar; user_data:gpointer);cdecl;

  TGPrintFunc = procedure   (_string: pgchar);
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{$IFNDEF KYLIX}
  function g_printf_string_upper_bound(format:Pgchar; args:array of const):gsize;cdecl;external gliblib name 'g_printf_string_upper_bound';
{$ELSE}
  function g_printf_string_upper_bound(format:Pgchar):gsize;varargs;cdecl;external gliblib name 'g_printf_string_upper_bound';
{$ENDIF}

{ Log level shift offset for user defined log levels (0-7 are used by GLib). }

const
   G_LOG_LEVEL_USER_SHIFT = 8;
{ Glib log levels and flags. }


const
  G_LOG_FLAG_RECURSION          = 1 shl 0;
  G_LOG_FLAG_FATAL              = 1 shl 1;
  G_LOG_LEVEL_ERROR             = 1 shl 2;
  G_LOG_LEVEL_CRITICAL          = 1 shl 3;
  G_LOG_LEVEL_WARNING           = 1 shl 4;
  G_LOG_LEVEL_MESSAGE           = 1 shl 5;
  G_LOG_LEVEL_INFO              = 1 shl 6;
  G_LOG_LEVEL_DEBUG             = 1 shl 7;
  G_LOG_LEVEL_MASK              = not 3;
//  G_LOG_LEVEL_MASK              =  not (G_LOG_FLAG_RECURSION or G_LOG_FLAG_FATAL);


{ GLib log levels that are considered fatal by default  }
const
   G_LOG_FATAL_MASK = 5;

{ Logging mechanism }

function g_log_set_handler(log_domain:Pgchar; log_levels:TGLogLevelFlags; log_func:TGLogFunc; user_data:gpointer):guint;cdecl;external gliblib name 'g_log_set_handler';
procedure g_log_remove_handler(log_domain:Pgchar; handler_id:guint);cdecl;external gliblib name 'g_log_remove_handler';
procedure g_log_default_handler(log_domain:Pgchar; log_level:TGLogLevelFlags; TheMessage:Pgchar; unused_data:gpointer);cdecl;external gliblib name 'g_log_default_handler';

{$IFNDEF KYLIX}
  procedure g_log(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_log';
  procedure g_log(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_log';

  procedure g_logv(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_logv';
  procedure g_logv(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_logv';
{$ELSE}
  procedure g_log(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar);varargs;cdecl;external gliblib name 'g_log';
  procedure g_logv(log_domain:Pgchar; log_level:TGLogLevelFlags; format:Pgchar);varargs;cdecl;external gliblib name 'g_logv';
{$ENDIF}


function g_log_set_fatal_mask(log_domain:Pgchar; fatal_mask:TGLogLevelFlags):TGLogLevelFlags;cdecl;external gliblib name 'g_log_set_fatal_mask';
function g_log_set_always_fatal(fatal_mask:TGLogLevelFlags):TGLogLevelFlags;cdecl;external gliblib name 'g_log_set_always_fatal';

{ internal  }
procedure _g_log_fallback_handler(log_domain:Pgchar; log_level:TGLogLevelFlags; message:Pgchar; unused_data:gpointer);cdecl;external gliblib name '_g_log_fallback_handler';

const
  G_LOG_DOMAIN = nil;


procedure g_error    (format:Pgchar; args: array of const); overload;
procedure g_error    (format:Pgchar); overload;

procedure g_message  (format:Pgchar; args: array of const); overload;
procedure g_message  (format:Pgchar); overload;

procedure g_critical (format:Pgchar; args: array of const); overload;
procedure g_critical (format:Pgchar); overload;

procedure g_warning  (format:Pgchar; args: array of const); overload;
procedure g_warning  (format:Pgchar); overload;

{$IFNDEF KYLIX}
  procedure g_print(format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_print';
  procedure g_print(format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_print';
{$ELSE}
  procedure g_print(format:Pgchar);varargs;cdecl;external gliblib name 'g_print';
{$ENDIF}

function g_set_print_handler(func:TGPrintFunc):TGPrintFunc;cdecl;external gliblib name 'g_set_print_handler';

{$IFNDEF KYLIX}
  procedure g_printerr(format:Pgchar; args:array of const);cdecl;overload;external gliblib name 'g_printerr';
  procedure g_printerr(format:Pgchar);cdecl;overload;varargs;external gliblib name 'g_printerr';
{$ELSE}
  procedure g_printerr(format:Pgchar);varargs;cdecl;external gliblib name 'g_printerr';
{$ENDIF}

function g_set_printerr_handler(func:TGPrintFunc):TGPrintFunc;cdecl;external gliblib name 'g_set_printerr_handler';
{$ENDIF read_interface_rest}
// included by glib2.pas

