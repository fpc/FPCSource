// included by gtk2.pas

{$IFDEF read_forward_definitions}

{ Anchor types  }

   PGtkAnchorType = ^TGtkAnchorType;
   TGtkAnchorType =  Longint;

{ Arrow types  }

   PGtkArrowType = ^TGtkArrowType;
   TGtkArrowType =  Longint;

{ Attach options (for tables)  }

   PGtkAttachOptions = ^TGtkAttachOptions;
   TGtkAttachOptions =  Longint;

{ Button box styles  }

   PGtkButtonBoxStyle = ^TGtkButtonBoxStyle;
   TGtkButtonBoxStyle =  Longint;

{ Curve types  }

   PGtkCurveType = ^TGtkCurveType;
   TGtkCurveType =  Longint;

{ Delete types }
   PGtkDeleteType = ^TGtkDeleteType;
   TGtkDeleteType =  Longint;

{ Focus movement types  }

   PGtkDirectionType = ^TGtkDirectionType;
   TGtkDirectionType =  Longint;

{ Expander styles  }

   PGtkExpanderStyle = ^TGtkExpanderStyle;
   TGtkExpanderStyle =  Longint;

{ Built-in stock icon sizes  }

   PPGtkIconSize = ^PGtkIconSize;
   PGtkIconSize = ^TGtkIconSize;
   TGtkIconSize =  Longint;

{$ifndef GTK_DISABLE_DEPRECATED}

{ side types  }

   PGtkSideType = ^TGtkSideType;
   TGtkSideType =  Longint;

{$endif} { GTK_DISABLE_DEPRECATED  }

{ Reading directions for text  }

   PGtkTextDirection = ^TGtkTextDirection;
   TGtkTextDirection =  Longint;

{ justification for label and maybe other widgets (text?)  }

   PGtkJustification = ^TGtkJustification;
   TGtkJustification =  Longint;

{$ifndef GTK_DISABLE_DEPRECATED}
{ GtkPatternSpec match types  }
{ " A?A "  }
{ " A?AA"  }
{ "AAAA "  }
{ " AAAA"  }
{ "AAAAA"  }

   PGtkMatchType = ^TGtkMatchType;
   TGtkMatchType =  Longint;

{$endif} { GTK_DISABLE_DEPRECATED  }

{ Menu keyboard movement types  }

   PGtkMenuDirectionType = ^TGtkMenuDirectionType;
   TGtkMenuDirectionType =  Longint;


   PGtkMetricType = ^TGtkMetricType;
   TGtkMetricType =  Longint;

   PGtkMovementStep = ^TGtkMovementStep;
   TGtkMovementStep =  Longint;

{ Orientation for toolbars, etc.  }

   PGtkOrientation = ^TGtkOrientation;
   TGtkOrientation =  Longint;

{ Placement type for scrolled window  }

   PGtkCornerType = ^TGtkCornerType;
   TGtkCornerType =  Longint;

{ Packing types (for boxes)  }

   PGtkPackType = ^TGtkPackType;
   TGtkPackType =  Longint;

{ priorities for path lookups  }

   PGtkPathPriorityType = ^TGtkPathPriorityType;
   TGtkPathPriorityType =  Longint;

{ widget path types  }

   PGtkPathType = ^TGtkPathType;
   TGtkPathType =  Longint;

{ Scrollbar policy types (for scrolled windows)  }

   PGtkPolicyType = ^TGtkPolicyType;
   TGtkPolicyType =  Longint;

   PGtkPositionType = ^TGtkPositionType;
   TGtkPositionType =  Longint;

{$ifndef GTK_DISABLE_DEPRECATED}

   PGtkPreviewType = ^TGtkPreviewType;
   TGtkPreviewType =  Longint;

{$endif} { GTK_DISABLE_DEPRECATED  }

{ Style for buttons  }

   PGtkReliefStyle = ^TGtkReliefStyle;
   TGtkReliefStyle =  Longint;

   PGtkResizeMode = ^TGtkResizeMode;
   TGtkResizeMode =  Longint;

{$ifndef GTK_DISABLE_DEPRECATED}

   PGtkSignalRunType = ^TGtkSignalRunType;
   TGtkSignalRunType =  Longint;

{$endif} { GTK_DISABLE_DEPRECATED  }

{ scrolling types  }

   PGtkScrollType = ^TGtkScrollType;
   TGtkScrollType =  Longint;

{ list selection modes  }

   PGtkSelectionMode = ^TGtkSelectionMode;
   TGtkSelectionMode =  Longint;

{ Shadow types  }

   PGtkShadowType = ^TGtkShadowType;
   TGtkShadowType =  Longint;

{ Widget states  }

   PGtkStateType = ^TGtkStateType;
   TGtkStateType =  Longint;

{ Directions for submenus  }

   PGtkSubmenuDirection = ^TGtkSubmenuDirection;
   TGtkSubmenuDirection =  Longint;

{ Placement of submenus  }

   PGtkSubmenuPlacement = ^TGtkSubmenuPlacement;
   TGtkSubmenuPlacement =  Longint;

{ Style for toolbars  }

   PGtkToolbarStyle = ^TGtkToolbarStyle;
   TGtkToolbarStyle =  Longint;

{ Data update types (for ranges)  }

   PGtkUpdateType = ^TGtkUpdateType;
   TGtkUpdateType =  Longint;

{ Generic visibility flags  }

   PGtkVisibility = ^TGtkVisibility;
   TGtkVisibility =  Longint;

{ Window position types  }

   PGtkWindowPosition = ^TGtkWindowPosition;
   TGtkWindowPosition =  Longint;

{ Window types  }

   PGtkWindowType = ^TGtkWindowType;
   TGtkWindowType =  Longint;

{ Text wrap  }

   PGtkWrapMode = ^TGtkWrapMode;
   TGtkWrapMode =  Longint;

{ How to sort  }

   PGtkSortType = ^TGtkSortType;
   TGtkSortType =  Longint;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
   Const
     GTK_ANCHOR_CENTER = 0;
     GTK_ANCHOR_NORTH = 1;
     GTK_ANCHOR_NORTH_WEST = 2;
     GTK_ANCHOR_NORTH_EAST = 3;
     GTK_ANCHOR_SOUTH = 4;
     GTK_ANCHOR_SOUTH_WEST = 5;
     GTK_ANCHOR_SOUTH_EAST = 6;
     GTK_ANCHOR_WEST = 7;
     GTK_ANCHOR_EAST = 8;
     GTK_ANCHOR_N = GTK_ANCHOR_NORTH;
     GTK_ANCHOR_NW = GTK_ANCHOR_NORTH_WEST;
     GTK_ANCHOR_NE = GTK_ANCHOR_NORTH_EAST;
     GTK_ANCHOR_S = GTK_ANCHOR_SOUTH;
     GTK_ANCHOR_SW = GTK_ANCHOR_SOUTH_WEST;
     GTK_ANCHOR_SE = GTK_ANCHOR_SOUTH_EAST;
     GTK_ANCHOR_W = GTK_ANCHOR_WEST;
     GTK_ANCHOR_E = GTK_ANCHOR_EAST;

     GTK_ARROW_UP = 0;
     GTK_ARROW_DOWN = 1;
     GTK_ARROW_LEFT = 2;
     GTK_ARROW_RIGHT = 3;

     GTK_EXPAND = 1 shl 0;
     GTK_SHRINK = 1 shl 1;
     GTK_FILL = 1 shl 2;

     GTK_BUTTONBOX_DEFAULT_STYLE = 0;
     GTK_BUTTONBOX_SPREAD = 1;
     GTK_BUTTONBOX_EDGE = 2;
     GTK_BUTTONBOX_START = 3;
     GTK_BUTTONBOX_END = 4;

     GTK_CURVE_TYPE_LINEAR = 0;  { linear interpolation  }
     GTK_CURVE_TYPE_SPLINE = 1;  { spline interpolation  }
     GTK_CURVE_TYPE_FREE = 2;    { free form curve  }

     GTK_DELETE_CHARS = 0;
     GTK_DELETE_WORD_ENDS = 1;
     GTK_DELETE_WORDS = 2;
     GTK_DELETE_DISPLAY_LINES = 3;
     GTK_DELETE_DISPLAY_LINE_ENDS = 4;
     GTK_DELETE_PARAGRAPH_ENDS = 5;
     GTK_DELETE_PARAGRAPHS = 6;
     GTK_DELETE_WHITESPACE = 7;

     GTK_DIR_TAB_FORWARD = 0;
     GTK_DIR_TAB_BACKWARD = 1;
     GTK_DIR_UP = 2;
     GTK_DIR_DOWN = 3;
     GTK_DIR_LEFT = 4;
     GTK_DIR_RIGHT = 5;

     GTK_EXPANDER_COLLAPSED = 0;
     GTK_EXPANDER_SEMI_COLLAPSED = 1;
     GTK_EXPANDER_SEMI_EXPANDED = 2;
     GTK_EXPANDER_EXPANDED = 3;

     GTK_ICON_SIZE_INVALID = 0;
     GTK_ICON_SIZE_MENU = 1;
     GTK_ICON_SIZE_SMALL_TOOLBAR = 2;
     GTK_ICON_SIZE_LARGE_TOOLBAR = 3;
     GTK_ICON_SIZE_BUTTON = 4;
     GTK_ICON_SIZE_DND = 5;
     GTK_ICON_SIZE_DIALOG = 6;

{$ifndef GTK_DISABLE_DEPRECATED}

     GTK_SIDE_TOP = 0;
     GTK_SIDE_BOTTOM = 1;
     GTK_SIDE_LEFT = 2;
     GTK_SIDE_RIGHT = 3;

{$endif}

     GTK_TEXT_DIR_NONE = 0;
     GTK_TEXT_DIR_LTR = 1;
     GTK_TEXT_DIR_RTL = 2;

     GTK_JUSTIFY_LEFT = 0;
     GTK_JUSTIFY_RIGHT = 1;
     GTK_JUSTIFY_CENTER = 2;
     GTK_JUSTIFY_FILL = 3;

{$ifndef GTK_DISABLE_DEPRECATED}

     GTK_MATCH_ALL = 0;
     GTK_MATCH_ALL_TAIL = 1;
     GTK_MATCH_HEAD = 2;
     GTK_MATCH_TAIL = 3;
     GTK_MATCH_EXACT = 4;
     GTK_MATCH_LAST = 5;
{$endif}

     GTK_MENU_DIR_PARENT = 0;
     GTK_MENU_DIR_CHILD = 1;
     GTK_MENU_DIR_NEXT = 2;
     GTK_MENU_DIR_PREV = 3;

     GTK_PIXELS = 0;
     GTK_INCHES = 1;
     GTK_CENTIMETERS = 2;

     GTK_MOVEMENT_LOGICAL_POSITIONS = 0;  { move by forw/back graphemes  }
     GTK_MOVEMENT_VISUAL_POSITIONS = 1;   { move by left/right graphemes  }
     GTK_MOVEMENT_WORDS = 2;              { move by forward/back words  }
     GTK_MOVEMENT_DISPLAY_LINES = 3;      { move up/down lines (wrapped lines)  }
     GTK_MOVEMENT_DISPLAY_LINE_ENDS = 4;  { move up/down lines (wrapped lines)  }
     GTK_MOVEMENT_PARAGRAPHS = 5;         { move up/down paragraphs (newline-ended lines)  }
     GTK_MOVEMENT_PARAGRAPH_ENDS = 6;     { move to either end of a paragraph  }
     GTK_MOVEMENT_PAGES = 7;              { move by pages  }
     GTK_MOVEMENT_BUFFER_ENDS = 8;        { move to ends of the buffer  }

     GTK_ORIENTATION_HORIZONTAL = 0;
     GTK_ORIENTATION_VERTICAL = 1;

     GTK_CORNER_TOP_LEFT = 0;
     GTK_CORNER_BOTTOM_LEFT = 1;
     GTK_CORNER_TOP_RIGHT = 2;
     GTK_CORNER_BOTTOM_RIGHT = 3;

     GTK_PACK_START = 0;
     GTK_PACK_END = 1;

     GTK_PATH_PRIO_LOWEST = 0;
     GTK_PATH_PRIO_GTK = 4;
     GTK_PATH_PRIO_APPLICATION = 8;
     GTK_PATH_PRIO_THEME = 10;
     GTK_PATH_PRIO_RC = 12;
     GTK_PATH_PRIO_HIGHEST = 15;

     GTK_PATH_WIDGET = 0;
     GTK_PATH_WIDGET_CLASS = 1;
     GTK_PATH_CLASS = 2;

     GTK_POLICY_ALWAYS = 0;
     GTK_POLICY_AUTOMATIC = 1;
     GTK_POLICY_NEVER = 2;

     GTK_POS_LEFT = 0;
     GTK_POS_RIGHT = 1;
     GTK_POS_TOP = 2;
     GTK_POS_BOTTOM = 3;

     GTK_PREVIEW_COLOR = 0;
     GTK_PREVIEW_GRAYSCALE = 1;

     GTK_RELIEF_NORMAL = 0;
     GTK_RELIEF_HALF = 1;
     GTK_RELIEF_NONE = 2;

     GTK_RESIZE_PARENT = 0;
     GTK_RESIZE_QUEUE = 1;
     GTK_RESIZE_IMMEDIATE = 2;

{$ifndef GTK_DISABLE_DEPRECATED}

     GTK_RUN_FIRST = G_SIGNAL_RUN_FIRST;
     GTK_RUN_LAST = G_SIGNAL_RUN_LAST;
     GTK_RUN_BOTH = GTK_RUN_FIRST or GTK_RUN_LAST;
     GTK_RUN_NO_RECURSE = G_SIGNAL_NO_RECURSE;
     GTK_RUN_ACTION = G_SIGNAL_ACTION;
     GTK_RUN_NO_HOOKS = G_SIGNAL_NO_HOOKS;

{$endif} { GTK_DISABLE_DEPRECATED  }

     GTK_SCROLL_NONE = 0;
     GTK_SCROLL_JUMP = 1;
     GTK_SCROLL_STEP_BACKWARD = 2;
     GTK_SCROLL_STEP_FORWARD = 3;
     GTK_SCROLL_PAGE_BACKWARD = 4;
     GTK_SCROLL_PAGE_FORWARD = 5;
     GTK_SCROLL_STEP_UP = 6;
     GTK_SCROLL_STEP_DOWN = 7;
     GTK_SCROLL_PAGE_UP = 8;
     GTK_SCROLL_PAGE_DOWN = 9;
     GTK_SCROLL_STEP_LEFT = 10;
     GTK_SCROLL_STEP_RIGHT = 11;
     GTK_SCROLL_PAGE_LEFT = 12;
     GTK_SCROLL_PAGE_RIGHT = 13;
     GTK_SCROLL_START = 14;
     GTK_SCROLL_END = 15;

     GTK_SELECTION_NONE = 0;        { Nothing can be selected  }
     GTK_SELECTION_SINGLE = 1;
     GTK_SELECTION_BROWSE = 2;
     GTK_SELECTION_MULTIPLE = 3;
     GTK_SELECTION_EXTENDED = GTK_SELECTION_MULTIPLE; { Deprecated  }

     GTK_SHADOW_NONE = 0;
     GTK_SHADOW_IN = 1;
     GTK_SHADOW_OUT = 2;
     GTK_SHADOW_ETCHED_IN = 3;
     GTK_SHADOW_ETCHED_OUT = 4;

     GTK_STATE_NORMAL = 0;
     GTK_STATE_ACTIVE = 1;
     GTK_STATE_PRELIGHT = 2;
     GTK_STATE_SELECTED = 3;
     GTK_STATE_INSENSITIVE = 4;

     GTK_DIRECTION_LEFT = 0;
     GTK_DIRECTION_RIGHT = 1;

     GTK_TOP_BOTTOM = 0;
     GTK_LEFT_RIGHT = 1;

     GTK_TOOLBAR_ICONS = 0;
     GTK_TOOLBAR_TEXT = 1;
     GTK_TOOLBAR_BOTH = 2;
     GTK_TOOLBAR_BOTH_HORIZ = 3;

     GTK_UPDATE_CONTINUOUS = 0;
     GTK_UPDATE_DISCONTINUOUS = 1;
     GTK_UPDATE_DELAYED = 2;

     GTK_VISIBILITY_NONE = 0;
     GTK_VISIBILITY_PARTIAL = 1;
     GTK_VISIBILITY_FULL = 2;

     GTK_WIN_POS_NONE = 0;
     GTK_WIN_POS_CENTER = 1;
     GTK_WIN_POS_MOUSE = 2;
     GTK_WIN_POS_CENTER_ALWAYS = 3;
     GTK_WIN_POS_CENTER_ON_PARENT = 4;

     GTK_WINDOW_TOPLEVEL = 0;
     GTK_WINDOW_POPUP = 1;

     GTK_WRAP_NONE = 0;
     GTK_WRAP_CHAR = 1;
     GTK_WRAP_WORD = 2;

     GTK_SORT_ASCENDING = 0;
     GTK_SORT_DESCENDING = 1;

{$ENDIF read_interface_rest}
// included by gtk2.pas

