{$IfDef read_interface}
type
   PPGError = ^PGError;
   PGError = ^TGError;
   TGError = record
        domain : TGQuark;
        code : gint;
        message : Pgchar;
     end;

Function g_error_new(domain : TGQuark; code : gint; const format:Pgchar; args:array of const):PGError;cdecl;external gconfdll name 'g_error_new';
function g_error_new_literal(domain:TGQuark; code:gint; message:Pgchar):PGError;cdecl;external gconfdll name 'g_error_new_literal';
procedure g_error_free(error:PGError);cdecl;external gconfdll name 'g_error_free';
function g_error_copy(error:PGError):PGError;cdecl;external gconfdll name 'g_error_copy';
function g_error_matches(error:PGError; domain:TGQuark; code:gint):gboolean;cdecl;external gconfdll name 'g_error_matches';

Procedure g_set_error(Err : PPGError; domain:TGQuark; code:gint; const format:Pgchar; args:array of const);cdecl;external gconfdll name 'g_set_error';

procedure g_clear_error(err:PPGError);cdecl;external gconfdll name 'g_clear_error';
procedure g_propagate_error(dest:PPGError; src:PGError);cdecl;external gconfdll name 'g_propagate_error';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
