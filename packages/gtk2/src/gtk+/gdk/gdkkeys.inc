// included by gdk2.pp

{$IFDEF read_interface_types}
{ GdkKeymapKey is a hardware key that can be mapped to a keyval  }
   PGdkKeymapKey = ^TGdkKeymapKey;
   TGdkKeymapKey = record
        keycode : guint;
        group : gint;
        level : gint;
     end;

   PGdkKeymap = ^TGdkKeymap;
   TGdkKeymap = record
        parent_instance : TGObject;
        display : PGdkDisplay;
     end;

   PGdkKeymapClass = ^TGdkKeymapClass;
   TGdkKeymapClass = record
        parent_class : TGObjectClass;
        direction_changed : procedure (keymap:PGdkKeymap); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ A GdkKeymap defines the translation from keyboard state
   (including a hardware key, a modifier mask, and active keyboard group)
   to a keyval. This translation has two phases. The first phase is
   to determine the effective keyboard group and level for the keyboard
   state; the second phase is to look up the keycode/group/level triplet
   in the keymap and see what keyval it corresponds to.
  }
function GDK_TYPE_KEYMAP : GType;
function GDK_KEYMAP(anObject : Pointer) : PGdkKeymap;
function GDK_KEYMAP_CLASS(klass : Pointer) : PGdkKeymapClass;
function GDK_IS_KEYMAP(anObject : Pointer) : boolean;
function GDK_IS_KEYMAP_CLASS(klass : Pointer) : boolean;
function GDK_KEYMAP_GET_CLASS(obj : Pointer) : PGdkKeymapClass;


function gdk_keymap_get_type:GType; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_keymap_get_default:PGdkKeymap; cdecl; external gdklib;
{$endif}

function gdk_keymap_get_for_display(display:PGdkDisplay):PGdkKeymap; cdecl; external gdklib;
function gdk_keymap_lookup_key(keymap:PGdkKeymap; key:PGdkKeymapKey):guint; cdecl; external gdklib;
function gdk_keymap_translate_keyboard_state(keymap:PGdkKeymap; hardware_keycode:guint; state:TGdkModifierType; group:gint; keyval:Pguint;
           effective_group:Pgint; level:Pgint; consumed_modifiers:PGdkModifierType):gboolean; cdecl; external gdklib;
function gdk_keymap_get_entries_for_keyval(keymap:PGdkKeymap; keyval:guint; var keys:PGdkKeymapKey; n_keys:Pgint):gboolean; cdecl; external gdklib;
function gdk_keymap_get_entries_for_keycode(keymap:PGdkKeymap; hardware_keycode:guint; var keys:PGdkKeymapKey; var keyvals:Pguint; n_entries:Pgint):gboolean; cdecl; external gdklib;
function gdk_keymap_get_direction(keymap:PGdkKeymap):TPangoDirection; cdecl; external gdklib;


{ Key values }

function gdk_keyval_name(keyval:guint):Pgchar; cdecl; external gdklib;
function gdk_keyval_from_name(keyval_name:Pgchar):guint; cdecl; external gdklib;
procedure gdk_keyval_convert_case(symbol:guint; lower:Pguint; upper:Pguint); cdecl; external gdklib;
function gdk_keyval_to_upper(keyval:guint):guint; cdecl; external gdklib;
function gdk_keyval_to_lower(keyval:guint):guint; cdecl; external gdklib;
function gdk_keyval_is_upper(keyval:guint):gboolean; cdecl; external gdklib;
function gdk_keyval_is_lower(keyval:guint):gboolean; cdecl; external gdklib;
function gdk_keyval_to_unicode(keyval:guint):guint32; cdecl; external gdklib;
function gdk_unicode_to_keyval(wc:guint32):guint; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_KEYMAP : GType;
begin
  GDK_TYPE_KEYMAP:=gdk_keymap_get_type;
end;

function GDK_KEYMAP(anObject : Pointer) : PGdkKeymap;
begin
  GDK_KEYMAP:=PGdkKeymap(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_KEYMAP));
end;

function GDK_KEYMAP_CLASS(klass : Pointer) : PGdkKeymapClass;
begin
  GDK_KEYMAP_CLASS:=PGdkKeymapClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_KEYMAP));
end;

function GDK_IS_KEYMAP(anObject : Pointer) : boolean;
begin
  GDK_IS_KEYMAP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_KEYMAP);
end;

function GDK_IS_KEYMAP_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_KEYMAP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_KEYMAP);
end;

function GDK_KEYMAP_GET_CLASS(obj : Pointer) : PGdkKeymapClass;
begin
  GDK_KEYMAP_GET_CLASS:=PGdkKeymapClass(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_KEYMAP));
end;
{$ENDIF}

