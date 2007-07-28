// included by gdk2.pp

{$IFDEF read_forward_definitions}
{ Forward declarations of commonly used types }
   PGdkInputCondition = ^TGdkInputCondition;
   TGdkInputCondition = longint; // see below for possible values

   PGdkStatus = ^TGdkStatus;
   TGdkStatus = longint;

   TGdkPoint  = record
        x : gint;
        y : gint;
     end;
   PGdkPoint  = ^TGdkPoint;
   PPGdkPoint = ^PGdkPoint;

   PGdkSpan = ^TGdkSpan;

{   Note that on some platforms the wchar_t type
   is not the same as GdkWChar. For instance
   on Win32, wchar_t is unsigned short.
  }
   PGdkWChar = ^TGdkWChar;
   TGdkWChar = guint32;

   PGdkSegment = ^TGdkSegment;
   TGdkSegment = record
        x1 : gint;
        y1 : gint;
        x2 : gint;
        y2 : gint;
     end;

   PGdkRectangle = ^TGdkRectangle;
   TGdkRectangle = record
        x : gint;
        y : gint;
        width : gint;
        height : gint;
     end;

   PGdkAtom = ^TGdkAtom;
   // internal type
   TGdkAtom = PtrUInt;

   PGdkByteOrder = ^TGdkByteOrder;
   TGdkByteOrder = (
     GDK_LSB_FIRST,
     GDK_MSB_FIRST
   );

{$ifdef GDK_NATIVE_WINDOW_POINTER}
   PGdkNativeWindow = ^TGdkNativeWindow;
   TGdkNativeWindow = gpointer;
{$else}
   PGdkNativeWindow = ^TGdkNativeWindow;
   TGdkNativeWindow = guint32;
{$endif}

{ Types of modifiers. }
{ The next few modifiers are used by XKB, so we skip to the theEnd }
   PGdkModifierType = ^TGdkModifierType;
   TGdkModifierType = gint;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Type definitions for the basic structures. }

{ We define specific numeric values for these constants,
   since old application code may depend on them matching the X values
   We don't actually depend on the matchup ourselves.
  }

   PGdkGrabStatus = ^TGdkGrabStatus;
   TGdkGrabStatus = integer; // see below for possible values

   TGdkInputFunction = procedure (data:gpointer; source:gint;
                                  condition:TGdkInputCondition); cdecl;

   TGdkDestroyNotify = procedure (data:gpointer); cdecl;

   TGdkSpan = record
        x : gint;
        y : gint;
        width : gint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ some common magic values  }
const
   GDK_CURRENT_TIME = 0;
   GDK_PARENT_RELATIVE = 1;

   GDK_OK = 0;
   GDK_ERROR = -(1);
   GDK_ERROR_PARAM = -(2);
   GDK_ERROR_FILE = -(3);
   GDK_ERROR_MEM = -(4);

   GDK_SHIFT_MASK = 1 shl 0;
   GDK_LOCK_MASK = 1 shl 1;
   GDK_CONTROL_MASK = 1 shl 2;
   GDK_MOD1_MASK = 1 shl 3;
   GDK_MOD2_MASK = 1 shl 4;
   GDK_MOD3_MASK = 1 shl 5;
   GDK_MOD4_MASK = 1 shl 6;
   GDK_MOD5_MASK = 1 shl 7;
   GDK_BUTTON1_MASK = 1 shl 8;
   GDK_BUTTON2_MASK = 1 shl 9;
   GDK_BUTTON3_MASK = 1 shl 10;
   GDK_BUTTON4_MASK = 1 shl 11;
   GDK_BUTTON5_MASK = 1 shl 12;
   GDK_RELEASE_MASK = 1 shl 30;
   GDK_MODIFIER_MASK = ord(GDK_RELEASE_MASK) or $1fff;

   // values for TGdkInputCondition
   GDK_INPUT_READ = 1 shl 0;
   GDK_INPUT_WRITE = 1 shl 1;
   GDK_INPUT_EXCEPTION = 1 shl 2;

   //  values for TGdkGrabStatus
   GDK_GRAB_SUCCESS = 0;
   GDK_GRAB_ALREADY_GRABBED = 1;
   GDK_GRAB_INVALID_TIME = 2;
   GDK_GRAB_NOT_VIEWABLE = 3;
   GDK_GRAB_FROZEN = 4;


function GDK_ATOM_TO_POINTER(atom : TGdkAtom) : Pointer;
function GDK_POINTER_TO_ATOM(ptr : Pointer) : TGdkAtom;
function _GDK_MAKE_ATOM(val : guint) : TGdkAtom;
function GDK_NONE : TGdkAtom;

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_ATOM_TO_POINTER(atom : TGdkAtom) : pointer;
begin
  GDK_ATOM_TO_POINTER:=Pointer(PtrInt(atom));
end;

function GDK_POINTER_TO_ATOM(ptr : Pointer) : TGdkAtom;
begin
  GDK_POINTER_TO_ATOM:=TGdkAtom(PtrInt(ptr));
end;

function _GDK_MAKE_ATOM(val : guint) : TGdkAtom;
begin
  _GDK_MAKE_ATOM:=TGdkAtom(PtrInt(GUINT_TO_POINTER(val)));
end;

function GDK_NONE : TGdkAtom;
begin
  GDK_NONE:=_GDK_MAKE_ATOM(0);
end;
{$ENDIF}

