{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

type
     PGtkArrowType = ^TGtkArrowType;
     TGtkArrowType = longint;
const
     GTK_ARROW_UP = 0;
     GTK_ARROW_DOWN = 1;
     GTK_ARROW_LEFT = 2;
     GTK_ARROW_RIGHT = 3;

type
     PGtkAttachOptions = ^TGtkAttachOptions;
     TGtkAttachOptions = longint;
const
     GTK_EXPAND = 1 shl 0;
     GTK_SHRINK = 1 shl 1;
     GTK_FILL = 1 shl 2;

type
     PGtkButtonBoxStyle = ^TGtkButtonBoxStyle;
     TGtkButtonBoxStyle = longint;
const
     GTK_BUTTONBOX_DEFAULT_STYLE = 0;
     GTK_BUTTONBOX_SPREAD = 1;
     GTK_BUTTONBOX_EDGE = 2;
     GTK_BUTTONBOX_START = 3;
     GTK_BUTTONBOX_END =4;

type
     PGtkCurveType = ^TGtkCurveType;
     TGtkCurveType = longint;
const
     GTK_CURVE_TYPE_LINEAR = 0;
     GTK_CURVE_TYPE_SPLINE = 1;
     GTK_CURVE_TYPE_FREE = 2;

type
     PGtkDirectionType = ^TGtkDirectionType;
     TGtkDirectionType = longint;
const
     GTK_DIR_TAB_FORWARD = 0;
     GTK_DIR_TAB_BACKWARD = 1;
     GTK_DIR_UP = 2;
     GTK_DIR_DOWN = 3;
     GTK_DIR_LEFT = 4;
     GTK_DIR_RIGHT = 5;

type
     PGtkJustification = ^TGtkJustification;
     TGtkJustification = longint;
const
     GTK_JUSTIFY_LEFT = 0;
     GTK_JUSTIFY_RIGHT = 1;
     GTK_JUSTIFY_CENTER = 2;
     GTK_JUSTIFY_FILL = 3;

type
     PGtkMatchType = ^TGtkMatchType;
     TGtkMatchType = longint;
const
     GTK_MATCH_ALL = 0;
     GTK_MATCH_ALL_TAIL = 1;
     GTK_MATCH_HEAD = 2;
     GTK_MATCH_TAIL = 3;
     GTK_MATCH_EXACT = 4;
     GTK_MATCH_LAST = 5;

type
     PGtkMenuDirectionType = ^TGtkMenuDirectionType;
     TGtkMenuDirectionType = longint;
const
     GTK_MENU_DIR_PARENT = 0;
     GTK_MENU_DIR_CHILD = 1;
     GTK_MENU_DIR_NEXT = 2;
     GTK_MENU_DIR_PREV = 3;

type
     PGtkMenuFactoryType = ^TGtkMenuFactoryType;
     TGtkMenuFactoryType = longint;
const
     GTK_MENU_FACTORY_MENU = 0;
     GTK_MENU_FACTORY_MENU_BAR = 1;
     GTK_MENU_FACTORY_OPTION_MENU = 2;

type
     PGtkMetricType = ^TGtkMetricType;
     TGtkMetricType = longint;
const
     GTK_PIXELS = 0;
     GTK_INCHES = 1;
     GTK_CENTIMETERS = 2;

type
     PGtkOrientation = ^TGtkOrientation;
     TGtkOrientation = longint;
const
     GTK_ORIENTATION_HORIZONTAL = 0;
     GTK_ORIENTATION_VERTICAL = 1;

type
     PGtkCornerType = ^TGtkCornerType;
     TGtkCornerType = longint;
const
     GTK_CORNER_TOP_LEFT = 0;
     GTK_CORNER_BOTTOM_LEFT = 1;
     GTK_CORNER_TOP_RIGHT = 2;
     GTK_CORNER_BOTTOM_RIGHT =3;

type
     PGtkPackType = ^TGtkPackType;
     TGtkPackType = longint;
const
     GTK_PACK_START = 0;
     GTK_PACK_END = 1;

type
     PGtkPathPriorityType = ^TGtkPathPriorityType;
     TGtkPathPriorityType = longint;
const
     GTK_PATH_PRIO_LOWEST = 0;
     GTK_PATH_PRIO_GTK = 4;
     GTK_PATH_PRIO_APPLICATION = 8;
     GTK_PATH_PRIO_RC = 12;
     GTK_PATH_PRIO_HIGHEST = 15;
     GTK_PATH_PRIO_MASK = GTK_PATH_PRIO_HIGHEST;

type
     PGtkPathType = ^TGtkPathType;
     TGtkPathType = longint;
const
     GTK_PATH_WIDGET = 0;
     GTK_PATH_WIDGET_CLASS = 1;
     GTK_PATH_CLASS = 2;

type
     PGtkPolicyType = ^TGtkPolicyType;
     TGtkPolicyType = longint;
const
     GTK_POLICY_ALWAYS = 0;
     GTK_POLICY_AUTOMATIC = 1;
     GTK_POLICY_NEVER = 2;

type
     PGtkPositionType = ^TGtkPositionType;
     TGtkPositionType = longint;
const
     GTK_POS_LEFT = 0;
     GTK_POS_RIGHT = 1;
     GTK_POS_TOP = 2;
     GTK_POS_BOTTOM = 3;

type
     PGtkPreviewType = ^TGtkPreviewType;
     TGtkPreviewType = longint;
const
     GTK_PREVIEW_COLOR = 0;
     GTK_PREVIEW_GRAYSCALE = 1;

type
     PGtkReliefStyle = ^TGtkReliefStyle;
     TGtkReliefStyle = longint;
const
     GTK_RELIEF_NORMAL = 0;
     GTK_RELIEF_HALF = 1;
     GTK_RELIEF_NONE = 2;

type
     PGtkResizeMode = ^TGtkResizeMode;
     TGtkResizeMode = longint;
const
     GTK_RESIZE_PARENT = 0;
     GTK_RESIZE_QUEUE = 1;
     GTK_RESIZE_IMMEDIATE = 2;

type
     PGtkSignalRunType = ^TGtkSignalRunType;
     TGtkSignalRunType = longint;
const
     GTK_RUN_FIRST = 1 shl 0;
     GTK_RUN_LAST = 1 shl 1;
     GTK_RUN_BOTH = 3;
     GTK_RUN_NO_RECURSE = 1 shl 2;
     GTK_RUN_ACTION = 1 shl 3;
     GTK_RUN_NO_HOOKS = 1 shl 4;

type
     PGtkScrollType = ^TGtkScrollType;
     TGtkScrollType = longint;
const
     GTK_SCROLL_NONE = 0;
     GTK_SCROLL_STEP_BACKWARD = 1;
     GTK_SCROLL_STEP_FORWARD = 2;
     GTK_SCROLL_PAGE_BACKWARD = 3;
     GTK_SCROLL_PAGE_FORWARD = 4;
     GTK_SCROLL_JUMP = 5;

type
     PGtkSelectionMode = ^TGtkSelectionMode;
     TGtkSelectionMode = longint;
const
     GTK_SELECTION_SINGLE = 0;
     GTK_SELECTION_BROWSE = 1;
     GTK_SELECTION_MULTIPLE = 2;
     GTK_SELECTION_EXTENDED = 3;

type
     PGtkShadowType = ^TGtkShadowType;
     TGtkShadowType = longint;
const
     GTK_SHADOW_NONE = 0;
     GTK_SHADOW_IN = 1;
     GTK_SHADOW_OUT = 2;
     GTK_SHADOW_ETCHED_IN = 3;
     GTK_SHADOW_ETCHED_OUT = 4;

type
     PGtkStateType = ^TGtkStateType;
     TGtkStateType = longint;
const
     GTK_STATE_NORMAL = 0;
     GTK_STATE_ACTIVE = 1;
     GTK_STATE_PRELIGHT = 2;
     GTK_STATE_SELECTED = 3;
     GTK_STATE_INSENSITIVE = 4;

type
     PGtkSubmenuDirection = ^TGtkSubmenuDirection;
     TGtkSubmenuDirection = longint;
const
     GTK_DIRECTION_LEFT = 0;
     GTK_DIRECTION_RIGHT = 1;

type
     PGtkSubmenuPlacement = ^TGtkSubmenuPlacement;
     TGtkSubmenuPlacement = longint;
const
     GTK_TOP_BOTTOM = 0;
     GTK_LEFT_RIGHT = 1;

type
     PGtkToolbarStyle = ^TGtkToolbarStyle;
     TGtkToolbarStyle = longint;
const
     GTK_TOOLBAR_ICONS = 0;
     GTK_TOOLBAR_TEXT = 1;
     GTK_TOOLBAR_BOTH = 2;

type
     PGtkTroughType = ^TGtkTroughType;
     TGtkTroughType = longint;
const
     GTK_TROUGH_NONE = 0;
     GTK_TROUGH_START = 1;
     GTK_TROUGH_END = 2;
     GTK_TROUGH_JUMP = 3;

type
     PGtkUpdateType = ^TGtkUpdateType;
     TGtkUpdateType = longint;
const
     GTK_UPDATE_CONTINUOUS = 0;
     GTK_UPDATE_DISCONTINUOUS = 1;
     GTK_UPDATE_DELAYED = 2;

type
     PGtkVisibility = ^TGtkVisibility;
     TGtkVisibility = longint;
const
     GTK_VISIBILITY_NONE = 0;
     GTK_VISIBILITY_PARTIAL = 1;
     GTK_VISIBILITY_FULL = 2;

type
     PGtkWindowPosition = ^TGtkWindowPosition;
     TGtkWindowPosition = longint;
const
     GTK_WIN_POS_NONE = 0;
     GTK_WIN_POS_CENTER = 1;
     GTK_WIN_POS_MOUSE = 2;

type
     PGtkWindowType = ^TGtkWindowType;
     TGtkWindowType = longint;
const
     GTK_WINDOW_TOPLEVEL = 0;
     GTK_WINDOW_DIALOG = 1;
     GTK_WINDOW_POPUP = 2;

type
     PGtkSortType = ^TGtkSortType;
     TGtkSortType = longint;
const
     GTK_SORT_ASCENDING = 0;
     GTK_SORT_DESCENDING = 1;

{$endif read_interface}

