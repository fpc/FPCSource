{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkPatternSpec = ^TGtkPatternSpec;
     TGtkPatternSpec = record
          match_type : TGtkMatchType;
          pattern_length : guint;
          pattern : Pgchar;
          pattern_reversed : Pgchar;
          user_data : gpointer;
          seq_id : guint;
       end;

procedure gtk_pattern_spec_init(pspec:PGtkPatternSpec; pattern:Pgchar);cdecl;external gtkdll name 'gtk_pattern_spec_init';
procedure gtk_pattern_spec_free_segs(pspec:PGtkPatternSpec);cdecl;external gtkdll name 'gtk_pattern_spec_free_segs';
function  gtk_pattern_match(pspec:PGtkPatternSpec; string_length:guint; thestring:Pgchar; string_reversed:Pgchar):gboolean;cdecl;external gtkdll name 'gtk_pattern_match';
function  gtk_pattern_match_string(pspec:PGtkPatternSpec; thestring:Pgchar):gboolean;cdecl;external gtkdll name 'gtk_pattern_match_string';
function  gtk_pattern_match_simple(pattern:Pgchar; thestring:Pgchar):gboolean;cdecl;external gtkdll name 'gtk_pattern_match_simple';

  type
     PGtkBindingSet = ^TGtkBindingSet;
     PGtkBindingEntry = ^TGtkBindingEntry;
     PGtkBindingSignal = ^TGtkBindingSignal;
     PGtkBindingArg = ^TGtkBindingArg;

     TGtkBindingSet = record
          set_name : Pgchar;
          priority : gint;
          widget_path_pspecs : PGSList;
          widget_class_pspecs : PGSList;
          class_branch_pspecs : PGSList;
          entries : PGtkBindingEntry;
          current : PGtkBindingEntry;
       end;

     TGtkBindingEntry = record
          keyval : guint;
          modifiers : guint;
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

  const
     bm_TGtkBindingEntry_destroyed = $1;
     bp_TGtkBindingEntry_destroyed = 0;
     bm_TGtkBindingEntry_in_emission = $2;
     bp_TGtkBindingEntry_in_emission = 1;
function  destroyed(var a : TGtkBindingEntry) : guint;
procedure set_destroyed(var a : TGtkBindingEntry; __destroyed : guint);
function  in_emission(var a : TGtkBindingEntry) : guint;
procedure set_in_emission(var a : TGtkBindingEntry; __in_emission : guint);


function  gtk_binding_set_new(set_name:Pgchar):PGtkBindingSet;cdecl;external gtkdll name 'gtk_binding_set_new';
function  gtk_binding_set_by_class(object_class:gpointer):PGtkBindingSet;cdecl;external gtkdll name 'gtk_binding_set_by_class';
function  gtk_binding_set_find(set_name:Pgchar):PGtkBindingSet;cdecl;external gtkdll name 'gtk_binding_set_find';
function  gtk_bindings_activate(theobject:PGtkObject; keyval:guint; modifiers:guint):gboolean;cdecl;external gtkdll name 'gtk_bindings_activate';
function  gtk_binding_set_activate(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint; theobject:PGtkObject):gboolean;cdecl;external gtkdll name 'gtk_binding_set_activate';
procedure gtk_binding_entry_add(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint);cdecl;external gtkdll name 'gtk_binding_entry_clear';
procedure gtk_binding_entry_clear(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint);cdecl;external gtkdll name 'gtk_binding_entry_clear';
procedure gtk_binding_entry_add_signal(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint; signal_name:Pgchar; n_args:guint; args:array of const);cdecl;external gtkdll name 'gtk_binding_entry_add_signal';
procedure gtk_binding_set_add_path(binding_set:PGtkBindingSet; path_type:TGtkPathType; path_pattern:Pgchar; priority:TGtkPathPriorityType);cdecl;external gtkdll name 'gtk_binding_set_add_path';
{$ifndef gtkwin}
procedure gtk_binding_entry_remove(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint);cdecl;external gtkdll name 'gtk_binding_entry_remove';
procedure gtk_binding_entry_add_signall(binding_set:PGtkBindingSet; keyval:guint; modifiers:guint; signal_name:Pgchar; binding_args:PGSList);cdecl;external gtkdll name 'gtk_binding_entry_add_signall';
function  gtk_binding_parse_binding(scanner:PGScanner):guint;cdecl;external gtkdll name 'gtk_binding_parse_binding';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}


function  destroyed(var a : TGtkBindingEntry) : guint;
    begin
       destroyed:=(a.flag0 and bm_TGtkBindingEntry_destroyed) shr bp_TGtkBindingEntry_destroyed;
    end;

procedure set_destroyed(var a : TGtkBindingEntry; __destroyed : guint);
    begin
       a.flag0:=a.flag0 or ((__destroyed shl bp_TGtkBindingEntry_destroyed) and bm_TGtkBindingEntry_destroyed);
    end;

function  in_emission(var a : TGtkBindingEntry) : guint;
    begin
       in_emission:=(a.flag0 and bm_TGtkBindingEntry_in_emission) shr bp_TGtkBindingEntry_in_emission;
    end;

procedure set_in_emission(var a : TGtkBindingEntry; __in_emission : guint);
    begin
       a.flag0:=a.flag0 or ((__in_emission shl bp_TGtkBindingEntry_in_emission) and bm_TGtkBindingEntry_in_emission);
    end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.7  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.6  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.5  1999/07/23 16:11:59  peter
    * use packrecords C

  Revision 1.4  1999/05/11 00:38:11  peter
    * win32 fixes

  Revision 1.3  1999/05/10 15:18:58  peter
    * cdecl fixes

  Revision 1.2  1999/05/10 09:02:56  peter
    * gtk 1.2 port working

  Revision 1.1  1999/05/07 10:40:30  peter
    * first things for 1.2

}

