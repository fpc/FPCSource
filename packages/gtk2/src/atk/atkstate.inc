// included by atk.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
  AtkStateType:
  @ATK_STATE_INVALID: Indicates an invalid state
  @ATK_STATE_ACTIVE: Indicates a window is currently the active window
  @ATK_STATE_ARMED: Indicates that the anObject is armed
  @ATK_STATE_BUSY: Indicates the current anObject is busy
  @ATK_STATE_CHECKED: Indicates this anObject is currently checked
  @ATK_STATE_DEFUNCT: Indicates the user interface anObject corresponding to this anObject no longer exists
  @ATK_STATE_EDITABLE: Indicates the user can change the contents of this anObject
  @ATK_STATE_ENABLED: Indicates that this anObject is enabled
  @ATK_STATE_EXPANDABLE: Indicates this anObject allows progressive disclosure of its children
  @ATK_STATE_EXPANDED: Indicates this anObject its expanded
  @ATK_STATE_FOCUSABLE: Indicates this anObject can accept keyboard focus, which means all events resulting from typing on the keyboard will normally be passed to it when it has focus
  @ATK_STATE_FOCUSED: Indicates this anObject currently has the keyboard focus
  @ATK_STATE_HORIZONTAL: Indicates the orientation of this anObject is horizontal
  @ATK_STATE_ICONIFIED: Indicates this anObject is minimized and is represented only by an icon
  @ATK_STATE_MODAL: Indicates something must be done with this anObject before the user can interact with an anObject in a different window
  @ATK_STATE_MULTI_LINE: Indicates this (text) anObject can contain multiple lines of text
  @ATK_STATE_MULTISELECTABLE: Indicates this anObject allows more than one of its children to be selected at the same time
  @ATK_STATE_OPAQUE: Indicates this anObject paints every pixel within its rectangular region
  @ATK_STATE_PRESSED: Indicates this anObject is currently pressed
  @ATK_STATE_RESIZABLE: Indicates the size of this anObject is not fixed
  @ATK_STATE_SELECTABLE: Indicates this anObject is the child of an anObject that allows its children to be selected and that this child is one of those children that can be selected
  @ATK_STATE_SELECTED: Indicates this anObject is the child of an anObject that allows its children to be selected and that this child is one of those children that has been selected
  @ATK_STATE_SENSITIVE: Indicates this anObject is sensitive
  @ATK_STATE_SHOWING: Indicates this anObject, the anObject's parent, the anObject's parent's parent, and so on, are all visible
  @ATK_STATE_SINGLE_LINE: Indicates this (text) anObject can contain only a single line of text
  @ATK_STATE_STALE: Indicates that the index associated with this anObject has changed since the user accessed the anObject.
  @ATK_STATE_TRANSIENT: Indicates this anObject is transient
  @ATK_STATE_VERTICAL: Indicates the orientation of this anObject is vertical
  @ATK_STATE_VISIBLE: Indicates this anObject is visible
  @ATK_STATE_LAST_DEFINED: Not a valid role, used for finding theEnd of enumeration

  The possible types of states of an anObject
   }
   PAtkStateType = ^TAtkStateType;
   TAtkStateType = (
     ATK_STATE_INVALID,
     ATK_STATE_ACTIVE,
     ATK_STATE_ARMED,
     ATK_STATE_BUSY,
     ATK_STATE_CHECKED,
     ATK_STATE_DEFUNCT,
     ATK_STATE_EDITABLE,
     ATK_STATE_ENABLED,
     ATK_STATE_EXPANDABLE,
     ATK_STATE_EXPANDED,
     ATK_STATE_FOCUSABLE,
     ATK_STATE_FOCUSED,
     ATK_STATE_HORIZONTAL,
     ATK_STATE_ICONIFIED,
     ATK_STATE_MODAL,
     ATK_STATE_MULTI_LINE,
     ATK_STATE_MULTISELECTABLE,
     ATK_STATE_OPAQUE,
     ATK_STATE_PRESSED,
     ATK_STATE_RESIZABLE,
     ATK_STATE_SELECTABLE,
     ATK_STATE_SELECTED,
     ATK_STATE_SENSITIVE,
     ATK_STATE_SHOWING,
     ATK_STATE_SINGLE_LINE,
     ATK_STATE_STALE,
     ATK_STATE_TRANSIENT,
     ATK_STATE_VERTICAL,
     ATK_STATE_VISIBLE,
     ATK_STATE_LAST_DEFINED
   );

   PAtkState = ^TAtkState;
   TAtkState = guint64;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function atk_state_type_register(name:Pgchar):TAtkStateType; cdecl; external atklib;
function atk_state_type_get_name(_type:TAtkStateType):Pgchar; cdecl; external atklib;
function atk_state_type_for_name(name:Pgchar):TAtkStateType; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF read_implementation}

