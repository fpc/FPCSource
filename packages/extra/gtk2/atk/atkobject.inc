// included by atk.pp

{
   AtkObject represents the minimum information all accessible objects
   return. This information includes accessible name, accessible
   description, role and state of the anObject, as well information about
   its parent and children. It is also possible to obtain more specific
   accessibility information about a component if it supports one or more
   of the following interfaces:
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

{
  AtkRole:
  @ATK_ROLE_INVALID: Invalid role
  @ATK_ROLE_ACCEL_LABEL: A label which represents an accelerator
  @ATK_ROLE_ALERT: An anObject which is an alert to the user
  @ATK_ROLE_ANIMATION: An anObject which is an animated image
  @ATK_ROLE_ARROW: An arrow in one of the four cardinal directions
  @ATK_ROLE_CALENDAR:  An anObject that displays a calendar and allows the user to select a date
  @ATK_ROLE_CANVAS: An anObject that can be drawn into and is used to trap events
  @ATK_ROLE_CHECK_BOX: A choice that can be checked or unchecked and provides a separate indicator for the current state
  @ATK_ROLE_CHECK_MENU_ITEM: A menu item with a check box
  @ATK_ROLE_COLOR_CHOOSER: A specialized dialog that lets the user choose a color
  @ATK_ROLE_COLUMN_HEADER: The header for a column of data
  @ATK_ROLE_COMBO_BOX: A list of choices the user can select from
  @ATK_ROLE_DATE_EDITOR: An anObject whose purpose is to allow a user to edit a date
  @ATK_ROLE_DESKTOP_ICON: An inconifed internal frame within a DESKTOP_PANE
  @ATK_ROLE_DESKTOP_FRAME: A pane that supports internal frames and iconified versions of those internal frames
  @ATK_ROLE_DIAL: An anObject whose purpose is to allow a user to set a value
  @ATK_ROLE_DIALOG: A top level window with title bar and a border
  @ATK_ROLE_DIRECTORY_PANE: A pane that allows the user to navigate through and select the contents of a directory
  @ATK_ROLE_DRAWING_AREA: An anObject used for drawing custom user interface elements
  @ATK_ROLE_FILE_CHOOSER: A specialized dialog that lets the user choose a file
  @ATK_ROLE_FILLER: A anObject that fills up space in a user interface
  @ATK_ROLE_FONT_CHOOSER: A specialized dialog that lets the user choose a font
  @ATK_ROLE_FRAME: A top level window with a title bar, border, menubar, etc.
  @ATK_ROLE_GLASS_PANE: A pane that is guaranteed to be painted on top of all panes beneath it
  @ATK_ROLE_HTML_CONTAINER: A document container for HTML, whose children represent the document content
  @ATK_ROLE_ICON: A small fixed size picture, typically used to decorate components
  @ATK_ROLE_IMAGE: An anObject whose primary purpose is to display an image
  @ATK_ROLE_INTERNAL_FRAME: A frame-like anObject that is clipped by a desktop pane
  @ATK_ROLE_LABEL: An anObject used to present an icon or short string in an interface
  @ATK_ROLE_LAYERED_PANE: A specialized pane that allows its children to be drawn in layers, providing a form of stacking order
  @ATK_ROLE_LIST: An anObject that presents a list of objects to the user and allows the user to select one or more of them
  @ATK_ROLE_LIST_ITEM: An anObject that represents an element of a list
  @ATK_ROLE_MENU: An anObject usually found inside a menu bar that contains a list of actions the user can choose from
  @ATK_ROLE_MENU_BAR: An anObject usually drawn at the top of the primary dialog box of an application that contains a list of menus the user can choose from
  @ATK_ROLE_MENU_ITEM: An anObject usually contained in a menu that presents an action the user can choose
  @ATK_ROLE_OPTION_PANE: A specialized pane whose primary use is inside a DIALOG
  @ATK_ROLE_PAGE_TAB: An anObject that is a child of a page tab list
  @ATK_ROLE_PAGE_TAB_LIST: An anObject that presents a series of panels (or page tabs), one at a time, through some mechanism provided by the anObject
  @ATK_ROLE_PANEL: A generic container that is often used to group objects
  @ATK_ROLE_PASSWORD_TEXT: A text anObject uses for passwords, or other places where the text content is not shown visibly to the user
  @ATK_ROLE_POPUP_MENU: A temporary window that is usually used to offer the user a list of choices, and then hides when the user selects one of those choices
  @ATK_ROLE_PROGRESS_BAR: An anObject used to indicate how much of a task has been completed
  @ATK_ROLE_PUSH_BUTTON: An anObject the user can manipulate to tell the application to do something
  @ATK_ROLE_RADIO_BUTTON: A specialized check box that will cause other radio buttons in the same group to become unchecked when this one is checked
  @ATK_ROLE_RADIO_MENU_ITEM: A check menu item which belongs to a group. At each instant exactly one of the radio menu items from a group is selected
  @ATK_ROLE_ROOT_PANE: A specialized pane that has a glass pane and a layered pane as its children
  @ATK_ROLE_ROW_HEADER: The header for a row of data
  @ATK_ROLE_SCROLL_BAR: An anObject usually used to allow a user to incrementally view a large amount of data.
  @ATK_ROLE_SCROLL_PANE: An anObject that allows a user to incrementally view a large amount of information
  @ATK_ROLE_SEPARATOR: An anObject usually contained in a menu to provide a visible and logical separation of the contents in a menu
  @ATK_ROLE_SLIDER: An anObject that allows the user to select from a bounded range
  @ATK_ROLE_SPLIT_PANE: A specialized panel that presents two other panels at the same time
  @ATK_ROLE_SPIN_BUTTON: An anObject used to get an integer or floating point number from the user
  @ATK_ROLE_STATUSBAR: An anObject which reports messages of minor importance to the user
  @ATK_ROLE_TABLE: An anObject used to represent information in terms of rows and columns
  @ATK_ROLE_TABLE_CELL: A cell in a table
  @ATK_ROLE_TABLE_COLUMN_HEADER: The header for a column of a table
  @ATK_ROLE_TABLE_ROW_HEADER: The header for a row of a table
  @ATK_ROLE_TEAR_OFF_MENU_ITEM: A menu item used to tear off and reattach its menu
  @ATK_ROLE_TERMINAL: An anObject that represents an accessible terminal
  @ATK_ROLE_TEXT: An anObject that presents text to the user
  @ATK_ROLE_TOGGLE_BUTTON: A specialized push button that can be checked or unchecked, but does not provide a separate indicator for the current state
  @ATK_ROLE_TOOL_BAR: A bar or palette usually composed of push buttons or toggle buttons
  @ATK_ROLE_TOOL_TIP: An anObject that provides information about another anObject
  @ATK_ROLE_TREE: An anObject used to represent hierarchical information to the user
  @ATK_ROLE_TREE_TABLE: An anObject capable of expanding and collapsing rows as well as showing multiple columns of data
  @ATK_ROLE_UNKNOWN: The anObject contains some Accessible information, but its role is not known
  @ATK_ROLE_VIEWPORT: An anObject usually used in a scroll pane
  @ATK_ROLE_WINDOW: A top level window with no title or border
  @ATK_ROLE_LAST_DEFINED: not a valid role, used for finding theEnd of enumeration

  Describes the role of an anObject
   }
   PAtkRole = ^TAtkRole;
   TAtkRole = (
     ATK_ROLE_INVALID {:= 0},
     ATK_ROLE_ACCEL_LABEL,
     ATK_ROLE_ALERT,
     ATK_ROLE_ANIMATION,
     ATK_ROLE_ARROW,
     ATK_ROLE_CALENDAR,
     ATK_ROLE_CANVAS,
     ATK_ROLE_CHECK_BOX,
     ATK_ROLE_CHECK_MENU_ITEM,
     ATK_ROLE_COLOR_CHOOSER,
     ATK_ROLE_COLUMN_HEADER,
     ATK_ROLE_COMBO_BOX,
     ATK_ROLE_DATE_EDITOR,
     ATK_ROLE_DESKTOP_ICON,
     ATK_ROLE_DESKTOP_FRAME,
     ATK_ROLE_DIAL,
     ATK_ROLE_DIALOG,
     ATK_ROLE_DIRECTORY_PANE,
     ATK_ROLE_DRAWING_AREA,
     ATK_ROLE_FILE_CHOOSER,
     ATK_ROLE_FILLER,
     ATK_ROLE_FONT_CHOOSER,
     ATK_ROLE_FRAME,
     ATK_ROLE_GLASS_PANE,
     ATK_ROLE_HTML_CONTAINER,
     ATK_ROLE_ICON,
     ATK_ROLE_IMAGE,
     ATK_ROLE_INTERNAL_FRAME,
     ATK_ROLE_LABEL,
     ATK_ROLE_LAYERED_PANE,
     ATK_ROLE_LIST,
     ATK_ROLE_LIST_ITEM,
     ATK_ROLE_MENU,
     ATK_ROLE_MENU_BAR,
     ATK_ROLE_MENU_ITEM,
     ATK_ROLE_OPTION_PANE,
     ATK_ROLE_PAGE_TAB,
     ATK_ROLE_PAGE_TAB_LIST,
     ATK_ROLE_PANEL,
     ATK_ROLE_PASSWORD_TEXT,
     ATK_ROLE_POPUP_MENU,
     ATK_ROLE_PROGRESS_BAR,
     ATK_ROLE_PUSH_BUTTON,
     ATK_ROLE_RADIO_BUTTON,
     ATK_ROLE_RADIO_MENU_ITEM,
     ATK_ROLE_ROOT_PANE,
     ATK_ROLE_ROW_HEADER,
     ATK_ROLE_SCROLL_BAR,
     ATK_ROLE_SCROLL_PANE,
     ATK_ROLE_SEPARATOR,
     ATK_ROLE_SLIDER,
     ATK_ROLE_SPLIT_PANE,
     ATK_ROLE_SPIN_BUTTON,
     ATK_ROLE_STATUSBAR,
     ATK_ROLE_TABLE,
     ATK_ROLE_TABLE_CELL,
     ATK_ROLE_TABLE_COLUMN_HEADER,
     ATK_ROLE_TABLE_ROW_HEADER,
     ATK_ROLE_TEAR_OFF_MENU_ITEM,
     ATK_ROLE_TERMINAL,
     ATK_ROLE_TEXT,
     ATK_ROLE_TOGGLE_BUTTON,
     ATK_ROLE_TOOL_BAR,
     ATK_ROLE_TOOL_TIP,
     ATK_ROLE_TREE,
     ATK_ROLE_TREE_TABLE,
     ATK_ROLE_UNKNOWN,
     ATK_ROLE_VIEWPORT,
     ATK_ROLE_WINDOW,
     ATK_ROLE_LAST_DEFINED
   );


{
  AtkLayer:
  @ATK_LAYER_INVALID: The anObject does not have a layer
  @ATK_LAYER_BACKGROUND: This layer is reserved for the desktop background
  @ATK_LAYER_CANVAS: This layer is used for Canvas components
  @ATK_LAYER_WIDGET: This layer is normally used for components
  @ATK_LAYER_MDI: This layer is used for layered components
  @ATK_LAYER_POPUP: This layer is used for popup components, such as menus
  @ATK_LAYER_OVERLAY: This layer is reserved for future use.

   Describes the layer of a component
   }
   PAtkLayer = ^TAtkLayer;
   TAtkLayer = (
     ATK_LAYER_INVALID,
     ATK_LAYER_BACKGROUND,
     ATK_LAYER_CANVAS,
     ATK_LAYER_WIDGET,
     ATK_LAYER_MDI,
     ATK_LAYER_POPUP,
     ATK_LAYER_OVERLAY
   );


   PAtkPropertyValues = ^TAtkPropertyValues;
   TAtkPropertyValues = record
        property_name : Pgchar;
        old_value : TGValue;
        new_value : TGValue;
     end;


   TAtkFunction = function (data:gpointer):gboolean; cdecl;
{
   For most properties the old_value field of AtkPropertyValues will
   not contain a valid value.

   Currently, the only property for which old_value is used is
   accessible-state; for instance if there is a focus state the
   property change handler will be called for the anObject which lost the focus
   with the old_value containing an AtkState value corresponding to focused
   and the property change handler will be called for the anObject which
   received the focus with the new_value containing an AtkState value
   corresponding to focused.
  }

   PAtkObject = ^TAtkObject;
   PPAtkObject = ^PAtkObject;
   TAtkObject = record
        parent : TGObject;
        description : Pgchar;
        name : Pgchar;
        accessible_parent : PAtkObject;
        role : TAtkRole;
        relation_set : PAtkRelationSet;
        layer : TAtkLayer;
     end;

   TAtkPropertyChangeHandler = procedure (para1:PAtkObject; para2:PAtkPropertyValues); cdecl;

{
     Gets the accessible name of the anObject
    }
{
     Gets the accessible description of the anObject
    }
{
     Gets the accessible parent of the anObject
    }
{
     Gets the number of accessible children of the anObject
    }
{
     Returns a reference to the specified accessible child of the anObject.
     The accessible children are 0-based so the first accessible child is
     at index 0, the second at index 1 and so on.
    }
{
     Gets the 0-based index of this anObject in its parent; returns -1 if the
     anObject does not have an accessible parent.
    }
{
     Gets the RelationSet associated with the anObject
    }
{
     Gets the role of the anObject
    }
{
     Gets the state set of the anObject
    }
{
     Sets the accessible name of the anObject
    }
{
     Sets the accessible description of the anObject
    }
{
     Sets the accessible parent of the anObject
    }
{
     Sets the accessible role of the anObject
    }
{
     Specifies a function to be called when a property changes value
    }
{
     Removes a property change handler which was specified using
     connect_property_change_handler
    }
{
     The signal handler which is executed when there is a change in the
     children of the anObject
    }
{
     The signal handler which is executed  when there is a focus event
     for an anObject.
    }
{
     The signal handler which is executed  when there is a property_change
     signal for an anObject.
    }
{
     The signal handler which is executed  when there is a state_change
     signal for an anObject.
    }
{
     The signal handler which is executed when there is a change in the
     visible data for an anObject
    }
   PAtkObjectClass = ^TAtkObjectClass;
   TAtkObjectClass = record
        parent : TGObjectClass;
        get_name : function (accessible:PAtkObject):Pgchar; cdecl;
        get_description : function (accessible:PAtkObject):Pgchar; cdecl;
        get_parent : function (accessible:PAtkObject):PAtkObject; cdecl;
        get_n_children : function (accessible:PAtkObject):gint; cdecl;
        ref_child : function (accessible:PAtkObject; i:gint):PAtkObject; cdecl;
        get_index_in_parent : function (accessible:PAtkObject):gint; cdecl;
        ref_relation_set : function (accessible:PAtkObject):PAtkRelationSet; cdecl;
        get_role : function (accessible:PAtkObject):TAtkRole; cdecl;
        get_layer : function (accessible:PAtkObject):TAtkLayer; cdecl;
        get_mdi_zorder : function (accessible:PAtkObject):gint; cdecl;
        ref_state_set : function (accessible:PAtkObject):PAtkStateSet; cdecl;
        set_name : procedure (accessible:PAtkObject; name:Pgchar); cdecl;
        set_description : procedure (accessible:PAtkObject; description:Pgchar); cdecl;
        set_parent : procedure (accessible:PAtkObject; parent:PAtkObject); cdecl;
        set_role : procedure (accessible:PAtkObject; role:TAtkRole); cdecl;
        connect_property_change_handler : function (accessible:PAtkObject; handler:TAtkPropertyChangeHandler):guint; cdecl;
        remove_property_change_handler : procedure (accessible:PAtkObject; handler_id:guint); cdecl;
        initialize : procedure (accessible:PAtkObject; data:gpointer); cdecl;
        children_changed : procedure (accessible:PAtkObject; change_index:guint; changed_child:gpointer); cdecl;
        focus_event : procedure (accessible:PAtkObject; focus_in:gboolean); cdecl;
        property_change : procedure (accessible:PAtkObject; values:PAtkPropertyValues); cdecl;
        state_change : procedure (accessible:PAtkObject; name:Pgchar; state_set:gboolean); cdecl;
        visible_data_changed : procedure (accessible:PAtkObject); cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

   PAtkImplementorIface = ^TAtkImplementorIface;
   TAtkImplementorIface = record
        parent : TGTypeInterface;
        ref_accessible : function (implementor:PAtkImplementor):PAtkObject; cdecl;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function atk_role_register(name:Pgchar):TAtkRole; cdecl; external atklib;

function atk_object_get_type:GType; cdecl; external atklib;

function ATK_TYPE_OBJECT : GType;
function ATK_OBJECT(obj: pointer) : PAtkObject;
function ATK_OBJECT_CLASS(klass: pointer) : PAtkObjectClass;
function ATK_IS_OBJECT(obj: pointer) : boolean;
function ATK_IS_OBJECT_CLASS(klass: pointer) : boolean;
function ATK_OBJECT_GET_CLASS(obj: pointer) : PAtkObjectClass;

function ATK_TYPE_IMPLEMENTOR : GType;
function ATK_IS_IMPLEMENTOR(obj: pointer) : boolean;
function ATK_IMPLEMENTOR(obj: pointer) : PAtkImplementor;
function ATK_IMPLEMENTOR_GET_IFACE(obj: pointer) : PAtkImplementorIface;

function atk_implementor_get_type:GType; cdecl; external atklib;
{
   This method uses the ref_accessible method in AtkImplementorIface,
   if the anObject's class implements AtkImplementorIface.
   Otherwise it returns %NULL.

   IMPORTANT:
   Note also that because this method may return flyweight objects,
   it increments the returned AtkObject's reference count.
   Therefore it is the responsibility of the calling
   program to unreference the anObject when no longer needed.
   (c.f. gtk_widget_get_accessible() where this is not the case).
  }
function atk_implementor_ref_accessible(implementor:PAtkImplementor):PAtkObject; cdecl; external atklib;
{
   Properties directly supported by AtkObject
  }
function atk_object_get_name(accessible:PAtkObject):Pgchar; cdecl; external atklib;
function atk_object_get_description(accessible:PAtkObject):Pgchar; cdecl; external atklib;
function atk_object_get_parent(accessible:PAtkObject):PAtkObject; cdecl; external atklib;
function atk_object_get_n_accessible_children(accessible:PAtkObject):gint; cdecl; external atklib;
function atk_object_ref_accessible_child(accessible:PAtkObject; i:gint):PAtkObject; cdecl; external atklib;
function atk_object_ref_relation_set(accessible:PAtkObject):PAtkRelationSet; cdecl; external atklib;
function atk_object_get_role(accessible:PAtkObject):TAtkRole; cdecl; external atklib;
function atk_object_get_layer(accessible:PAtkObject):TAtkLayer; cdecl; external atklib;
function atk_object_get_mdi_zorder(accessible:PAtkObject):gint; cdecl; external atklib;
function atk_object_ref_state_set(accessible:PAtkObject):PAtkStateSet; cdecl; external atklib;
function atk_object_get_index_in_parent(accessible:PAtkObject):gint; cdecl; external atklib;
procedure atk_object_set_name(accessible:PAtkObject; name:Pgchar); cdecl; external atklib;
procedure atk_object_set_description(accessible:PAtkObject; description:Pgchar); cdecl; external atklib;
procedure atk_object_set_parent(accessible:PAtkObject; parent:PAtkObject); cdecl; external atklib;
procedure atk_object_set_role(accessible:PAtkObject; role:TAtkRole); cdecl; external atklib;
function atk_object_connect_property_change_handler(accessible:PAtkObject; handler:TAtkPropertyChangeHandler):guint; cdecl; external atklib;
procedure atk_object_remove_property_change_handler(accessible:PAtkObject; handler_id:guint); cdecl; external atklib;
procedure atk_object_notify_state_change(accessible:PAtkObject; state:TAtkState; value:gboolean); cdecl; external atklib;
procedure atk_object_initialize(accessible:PAtkObject; data:gpointer); cdecl; external atklib;
function atk_role_get_name(role:TAtkRole):Pgchar; cdecl; external atklib;
function atk_role_for_name(name:Pgchar):TAtkRole; cdecl; external atklib;
{
   Note: the properties which are registered with the GType
     property registry, for type ATK_TYPE_OBJECT, are as follows:

     "accessible-name"
     "accessible-description"
     "accessible-parent"
     "accessible-role"
     "accessible-value"
     "accessible-component-layer"
     "accessible-component-zorder"
     "accessible-table-caption"
     "accessible-table-column-description"
     "accessible-table-column-header"
     "accessible-table-row-description"
     "accessible-table-row-header"
     "accessible-table-summary"
     "accessible-model"

   accessibility property change listeners should use the
     normal GObject property interfaces and "property-change"
     signal handler semantics to interpret the property change
     information relayed from AtkObject.
     (AtkObject instances will connect to the "notify"
     signal in their host objects, and relay the signals when appropriate).
  }
(* For other signals, see related interfaces

      AtkActionIface,
      AtkComponentIface,
      AtkHypertextIface,
      AtkImageIface,
      AtkSelectionIface,
      AtkTableIface,
      AtkTextIface,
      AtkValueIface.

    The usage model for obtaining these interface instances is:
      ATK_<interfacename>_GET_IFACE(GObject  accessible),
      where accessible, though specified as a GObject, is
      the AtkObject instance being queried.
    More usually, the interface will be used via a cast to the
      interface's corresponding "type":

      var
        textImpl: PAtkText;
      begin
        textImpl := ATK_TEXT(accessible);
        if (textImpl<>nil) then
          cpos := atk_text_get_caret_position(textImpl);
      end;

    If it's known in advance that accessible implements AtkTextIface,
      this is shortened to:

      cpos := atk_text_get_caret_position (ATK_TEXT (accessible));
*)

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_OBJECT : GType;
begin
  ATK_TYPE_OBJECT:=atk_object_get_type;
end;

function ATK_OBJECT(obj: pointer) : PAtkObject;
begin
  ATK_OBJECT:=PAtkObject(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_OBJECT));
end;

function ATK_OBJECT_CLASS(klass: pointer) : PAtkObjectClass;
begin
  ATK_OBJECT_CLASS:=PAtkObjectClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_OBJECT));
end;

function ATK_IS_OBJECT(obj: pointer) : boolean;
begin
  ATK_IS_OBJECT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_OBJECT);
end;

function ATK_IS_OBJECT_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_OBJECT_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_OBJECT);
end;

function ATK_OBJECT_GET_CLASS(obj: pointer) : PAtkObjectClass;
begin
   ATK_OBJECT_GET_CLASS:=PAtkObjectClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_OBJECT));
end;

function ATK_TYPE_IMPLEMENTOR : GType;
begin
  ATK_TYPE_IMPLEMENTOR:=atk_implementor_get_type;
end;

function ATK_IS_IMPLEMENTOR(obj: pointer) : boolean;
begin
  ATK_IS_IMPLEMENTOR:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_IMPLEMENTOR);
end;

function ATK_IMPLEMENTOR(obj: pointer) : PAtkImplementor;
begin
  ATK_IMPLEMENTOR:=PAtkImplementor(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_IMPLEMENTOR));
end;

function ATK_IMPLEMENTOR_GET_IFACE(obj: pointer) : PAtkImplementorIface;
begin
   ATK_IMPLEMENTOR_GET_IFACE:=PAtkImplementorIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_IMPLEMENTOR));
end;

{$ENDIF read_implementation}

