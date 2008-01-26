// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkBindingEntry = ^TGtkBindingEntry;
   PGtkBindingSignal = ^TGtkBindingSignal;
   PGtkBindingArg = ^TGtkBindingArg;

{ Binding sets }
{ From RC content  }
   PGtkBindingSet = ^TGtkBindingSet;
   TGtkBindingSet = record
        set_name : Pgchar;
        priority : gint;
        widget_path_pspecs : PGSList;
        widget_class_pspecs : PGSList;
        class_branch_pspecs : PGSList;
        entries : PGtkBindingEntry;
        current : PGtkBindingEntry;
        flag0 : word;
     end;

{ key portion }
   TGtkBindingEntry = record
        keyval : guint;
        modifiers : TGdkModifierType;
        binding_set : PGtkBindingSet;
        flag0 : word;
        set_next : PGtkBindingEntry;
        hash_next : PGtkBindingEntry;
        signals : PGtkBindingSignal;
     end;

   TGtkBindingSignal = record
        next : PGtkBindingSignal;
        signal_name : Pgchar;
        n_args : guint;
        args : PGtkBindingArg;
     end;

   TGtkBindingArg = record
        arg_type : TGtkType;
        d : record
            case longint of
               0 : ( long_data : glong );
               1 : ( double_data : gdouble );
               2 : ( string_data : Pgchar );
            end;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   bm_TGtkBindingSet_parsed = $1;
   bp_TGtkBindingSet_parsed = 0;

   bm_TGtkBindingEntry_destroyed = $1;
   bp_TGtkBindingEntry_destroyed = 0;
   bm_TGtkBindingEntry_in_emission = $2;
   bp_TGtkBindingEntry_in_emission = 1;

procedure gtk_binding_entry_add(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType);

function parsed(var a : TGtkBindingSet) : guint;
procedure set_parsed(var a : TGtkBindingSet; __parsed : guint);

function destroyed(var a : TGtkBindingEntry) : guint;
procedure set_destroyed(var a : TGtkBindingEntry; __destroyed : guint);
function in_emission(var a : TGtkBindingEntry) : guint;
procedure set_in_emission(var a : TGtkBindingEntry; __in_emission : guint);

{ Application-level methods  }
function gtk_binding_set_new(set_name:Pgchar):PGtkBindingSet; cdecl; external gtklib;
function gtk_binding_set_by_class(object_class:gpointer):PGtkBindingSet; cdecl; external gtklib;
function gtk_binding_set_find(set_name:Pgchar):PGtkBindingSet; cdecl; external gtklib;
function gtk_bindings_activate(anObject:PGtkObject; keyval:guint; modifiers:TGdkModifierType):gboolean; cdecl; external gtklib;
function gtk_binding_set_activate(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType; anObject:PGtkObject):gboolean; cdecl; external gtklib;

procedure gtk_binding_entry_clear(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_binding_entry_add_signal(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType; signal_name:Pgchar; n_args:guint;
            args:array of const); cdecl; overload; external gtklib;
procedure gtk_binding_entry_add_signal(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType; signal_name:Pgchar; n_args:guint); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_binding_entry_add_signal(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType; signal_name:Pgchar; n_args:guint); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_binding_set_add_path(binding_set:PGtkBindingSet; path_type:TGtkPathType; path_pattern:Pgchar; priority:TGtkPathPriorityType); cdecl; external gtklib;
{ Non-public methods  }
procedure gtk_binding_entry_remove(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType); cdecl; external gtklib;
procedure gtk_binding_entry_add_signall(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType; signal_name:Pgchar; binding_args:PGSList); cdecl; external gtklib;
function gtk_binding_parse_binding(scanner:PGScanner):guint; cdecl; external gtklib;
function _gtk_bindings_activate_event(anObject:PGtkObject; event:PGdkEventKey):gboolean; cdecl; external gtklib;
procedure _gtk_binding_reset_parsed; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function parsed(var a : TGtkBindingSet) : guint;
begin
   parsed:=(a.flag0 and bm_TGtkBindingSet_parsed) shr bp_TGtkBindingSet_parsed;
end;

procedure set_parsed(var a : TGtkBindingSet; __parsed : guint);
begin
   a.flag0:=a.flag0 or ((__parsed shl bp_TGtkBindingSet_parsed) and bm_TGtkBindingSet_parsed);
end;

function destroyed(var a : TGtkBindingEntry) : guint;
begin
   destroyed:=(a.flag0 and bm_TGtkBindingEntry_destroyed) shr bp_TGtkBindingEntry_destroyed;
end;

procedure set_destroyed(var a : TGtkBindingEntry; __destroyed : guint);
begin
   a.flag0:=a.flag0 or ((__destroyed shl bp_TGtkBindingEntry_destroyed) and bm_TGtkBindingEntry_destroyed);
end;

function in_emission(var a : TGtkBindingEntry) : guint;
begin
   in_emission:=(a.flag0 and bm_TGtkBindingEntry_in_emission) shr bp_TGtkBindingEntry_in_emission;
end;

procedure set_in_emission(var a : TGtkBindingEntry; __in_emission : guint);
begin
   a.flag0:=a.flag0 or ((__in_emission shl bp_TGtkBindingEntry_in_emission) and bm_TGtkBindingEntry_in_emission);
end;

procedure gtk_binding_entry_add(binding_set:PGtkBindingSet; keyval:guint; modifiers:TGdkModifierType);
begin
   gtk_binding_entry_clear(binding_set,keyval,modifiers);
end;
{$ENDIF read_implementation}
// included by gtk2.pas

