
Unit forms;

Interface

{$LinkLib forms}

uses x,xlib,xutil,xresource;

const 
  FL_VERSION=0;
  FL_REVISION=88;
  FL_FIXLEVEL=1;
  FL_INCLUDE_VERSION=FL_VERSION * 1000 + FL_REVISION;

{ some general constants }
    FL_ON = 1;
    FL_OK = 1;
    FL_VALID = 1;
    FL_PREEMPT = 1;
    FL_AUTO = 2;
    FL_WHEN_NEEDED = FL_AUTO;

    FL_OFF = 0;
    FL_NONE = 0;
    FL_CANCEL = 0;
    FL_INVALID = 0;

    FL_IGNORE = -1;
    FL_CLOSE = -2;
{
 * The screen coordinate unit, FL_Coord, must be of signed type. Without
 * prototype support, a type other than integer might not work right.
 * If FL_Coord is float, FL_CoordIsFloat must be defined to be 1 so that
 * round-off error can be checked. **TODO Float not tested ***
 }
Type PWord   = ^Word;
     PDouble = ^Double;
     PFloat  = ^Real;
     PLongint = ^Longint;
          
type TFL_Coord = Longint {was int};
     PFL_Coord = ^TFL_Coord;
{ #define FL_CoordIsFloat 0	 define this if FL_Coord is of type float }
   TFL_COLOR = longint; { Was cardinal }
   PFL_COLOR = ^TFL_COLOR;

{
 * Coordinates can be in pixels, milli-meters or points (1/72inch)
 }
  TFL_COORD_UNIT=Longint {was int};
  { POssible values }
Const 
    FL_COORD_PIXEL=0;		{ default, Pixel           }
    FL_COORD_MM=1;		{ milli-meter              }
    FL_COORD_POINT=2;		{ point                    }
    FL_COORD_centiMM=3;		{ one hundredth of a mm    }
    FL_COORD_centiPOINT=4;	{ one hundredth of a point }

{
 * All object classes.
 }

type TFL_Class = Longint;

Const { Possible values }
    FL_INVALID_CLASS=0;
    FL_BUTTON=1;
    FL_LIGHTBUTTON=2;
    FL_ROUNDBUTTON=3;
    FL_ROUND_3DBUTTON=4;
    FL_CHECKBUTTON=5;
    FL_BITMAPBUTTON=6;
    FL_PIXMAPBUTTON=7;
    FL_BITMAP=8;
    FL_PIXMAP=9;
    FL_BOX=10;
    FL_TEXT=11;
    FL_MENU=12;
    FL_CHART=13;
    FL_CHOICE=14;
    FL_COUNTER=15;
    FL_SLIDER=16;
    FL_VALSLIDER=17;
    FL_INPUT=18;
    FL_BROWSER=19;
    FL_DIAL=21;
    FL_TIMER=21;
    FL_CLOCK=22;
    FL_POSITIONER=23;
    FL_FREE=24;
    FL_XYPLOT=25;
    FL_FRAME=26;
    FL_LABELFRAME=27;
    FL_CANVAS=28;
    FL_GLCANVAS=29;
    FL_IMAGECANVAS=30;
    FL_FOLDER=31;
    FL_TEXTBOX=32;

{ how to display a form onto screen }

Type
    TFL_PLace = Longint;

Const
    FL_PLACE_FREE = 0;		{ size remain resizable      }
    FL_PLACE_MOUSE = 1;		{ mouse centered on form     }
    FL_PLACE_CENTER = 2;	{ center of the screen       }
    FL_PLACE_POSITION = 4;	{ specific size              }
    FL_PLACE_SIZE = 8;		{ specific size              }
    FL_PLACE_GEOMETRY = 16;	{ specific position          }
    FL_PLACE_ASPECT = 32;	{ keep aspect ratio          }
    FL_PLACE_FULLSCREEN = 64;	{ scale to fit screen        }
    FL_PLACE_HOTSPOT = 128;	{ so mouse fall on (x,y)     }
    FL_PLACE_ICONIC = 256;
    { modifier }
    FL_FREE_SIZE = 1 shl 14;
    FL_FIX_SIZE = 1 shl 15;

    FL_PLACE_FREE_CENTER =  FL_PLACE_CENTER or FL_FREE_SIZE;
    FL_PLACE_CENTERFREE = FL_PLACE_CENTER or FL_FREE_SIZE;

{ Window manager decoration request }

type 
    TFL_DECORATION = Longint;
  
Const { Possible values for FL_DECORATION }
    FL_FULLBORDER = 1;		{ normal }
    FL_TRANSIENT = 2;		{ set TRANSIENT_FOR property              }
    FL_NOBORDER = 3;		{ use override_redirect to supress decor. }
    FL_MODAL = 1 << 8;
type
 { All box types }
    TFL_BOX_TYPE = Longint;
Const { Possible values }
    FL_NO_BOX=0;
    FL_UP_BOX=1;
    FL_DOWN_BOX=2;
    FL_BORDER_BOX=3;
    FL_SHADOW_BOX=4;
    FL_FRAME_BOX=5;
    FL_ROUNDED_BOX=6;
    FL_EMBOSSED_BOX=7;
    FL_FLAT_BOX=8;
    FL_RFLAT_BOX=9;
    FL_RSHADOW_BOX=10;
    FL_OVAL_BOX=11;
    FL_ROUNDED3D_UPBOX=12;
    FL_ROUNDED3D_DOWNBOX=13;
    FL_OVAL3D_UPBOX=14;
    FL_OVAL3D_DOWNBOX=15;
    FL_OSHADOW_BOX=16;

{ How to place text relative to a box }
Type
    TFL_ALIGN = Longint;

Const { Possible values for FL_ALIGN } 
    FL_ALIGN_CENTER = 0;
    FL_ALIGN_TOP = 1;
    FL_ALIGN_BOTTOM = 2;
    FL_ALIGN_LEFT = 4;
    FL_ALIGN_RIGHT = 8;
    FL_ALIGN_TOP_LEFT = FL_ALIGN_TOP or FL_ALIGN_LEFT;
    FL_ALIGN_TOP_RIGHT = FL_ALIGN_TOP or FL_ALIGN_RIGHT;
    FL_ALIGN_BOTTOM_LEFT = FL_ALIGN_BOTTOM or FL_ALIGN_LEFT;
    FL_ALIGN_BOTTOM_RIGHT = FL_ALIGN_BOTTOM or FL_ALIGN_RIGHT;
    FL_ALIGN_INSIDE = (1 shl 13);
    FL_ALIGN_VERT = (1 shl 14);	{ not functional yet  }

    FL_ALIGN_LEFT_TOP = FL_ALIGN_TOP_LEFT;
    FL_ALIGN_RIGHT_TOP = FL_ALIGN_TOP_RIGHT;
    FL_ALIGN_LEFT_BOTTOM = FL_ALIGN_BOTTOM_LEFT;
    FL_ALIGN_RIGHT_BOTTOM = FL_ALIGN_BOTTOM_RIGHT;

{ Mouse buttons }

FL_MBUTTON1=1; FL_LEFT_MOUSE=FL_MBUTTON1;
FL_MBUTTON2=2; FL_MIDDLE_MOUSE=FL_MBUTTON2;
FL_MBUTTON3=3; FL_RIGHT_MOUSE=FL_MBUTTON3;
FL_MBUTTON4=4;
FL_MBUTTON5=5;


{ control when to reutrn input, slider and dial object. }

    FL_RETURN_END_CHANGED = 0;
    FL_RETURN_CHANGED = 1;
    FL_RETURN_END = 2;
    FL_RETURN_ALWAYS = 3;
    FL_RETURN_DBLCLICK = 4;

{
 *  Some special color indeces for FL private colormap. It does not matter
 *  what the value of each enum is, but it must start from 0 and be
 *  consecutive.
 }
 
 
 
Type
    TFL_DP_COL = Longint;
Const
    FL_BLACK		= 0;
    FL_RED		= 1;
    FL_GREEN		= 2;
    FL_YELLOW		= 3;
    FL_BLUE		= 4;
    FL_MAGENTA		= 5;
    FL_CYAN		= 6;
    FL_WHITE		= 7;

    FL_TOMATO		= 8;
    FL_INDIANRED	= 9;
    FL_SLATEBLUE	= 10;

    FL_COL1		= 11;
    FL_RIGHT_BCOL	= 12;
    FL_BOTTOM_BCOL	= 13;
    FL_TOP_BCOL		= 14;
    FL_LEFT_BCOL	= 15;
    FL_MCOL		= 16;

    FL_INACTIVE		= 17;
    FL_PALEGREEN	= 18;
    FL_DARKGOLD		= 19;

    FL_ORCHID		= 20;
    FL_DARKCYAN		= 21;
    FL_DARKTOMATO	= 22;
    FL_WHEAT		= 23;
    FL_DARKORANGE	= 24;
    FL_DEEPPINK		= 25;
    FL_CHARTREUSE	= 26;
    FL_DARKVIOLET	= 27;
    FL_SPRINGGREEN	= 28;
    FL_DOGERBLUE	= 29;

    FL_FREE_COL1 = 256; 
    FL_FREE_COL2 = 257;
    FL_FREE_COL3 = 258; 
    FL_FREE_COL4 = 259;
    FL_FREE_COL5 = 260; 
    FL_FREE_COL6 = 261;
    FL_FREE_COL7 = 262;
    FL_FREE_COL8 = 263;
    FL_FREE_COL9 = 264;
    FL_FREE_COL10 = 265;
    FL_FREE_COL11 = 266;
    FL_FREE_COL12 = 267;
    FL_FREE_COL13 = 268;
    FL_FREE_COL14 = 269;
    FL_FREE_COL15 = 270;
    FL_FREE_COL16 = 271;

FL_BUILT_IN_COLS  = FL_DOGERBLUE+1;
FL_INACTIVE_COL =  FL_INACTIVE;

{ Some aliases for the color. This is actually backwards ... }

Const
FL_GRAY16=FL_RIGHT_BCOL;
FL_GRAY35      =     FL_BOTTOM_BCOL;
FL_GRAY80      =     FL_TOP_BCOL;
FL_GRAY90      =     FL_LEFT_BCOL;
FL_GRAY63      =     FL_COL1;
FL_GRAY75      =     FL_MCOL;
FL_LCOL        =     FL_BLACK;

{
 *  Pop-up menu item attributes. NOTE if more than 8, need to change
 *  choice and menu class where mode is kept by a single byte
 }
    FL_PUP_NONE = 0;
    FL_PUP_GREY = 1;
    FL_PUP_BOX = 2;
    FL_PUP_CHECK = 4;
    FL_PUP_RADIO = 8;

FL_PUP_GRAY     = FL_PUP_GREY;
FL_PUP_TOGGLE   = FL_PUP_BOX;
FL_PUP_INACTIVE = FL_PUP_GREY;


{ Events that a form reacts to.  }
type TFL_EVENT = Longint;
Const { Possible values }
    FL_NOEVENT		= 0;
    FL_DRAW		= 1;
    FL_PUSH		= 2;
    FL_RELEASE		= 3;
    FL_ENTER		= 4;
    FL_LEAVE		= 5;
    FL_MOUSE		= 6;
    FL_FOCUS		= 7;
    FL_UNFOCUS		= 8;
    FL_KEYBOARD		= 9;
    FL_MOTION		= 10;
    FL_STEP		= 11;
    FL_SHORTCUT		= 12;
    FL_FREEMEM		= 13;
    FL_OTHER		= 14;		{ property, selection etc }
    FL_DRAWLABEL	= 15;
    FL_DBLCLICK		= 16;		{ double click            }
    FL_TRPLCLICK	= 17;		{ triple click            }
    FL_PS		= 18;		{ dump a form into EPS    }

const 
FL_MOVE=FL_MOTION;	{ for compatibility }

{ Resize policies }
type TFL_RESIZE_T = Longint;

const
    FL_RESIZE_NONE = 0;
    FL_RESIZE_X    = 1;
    FL_RESIZE_Y    = 2;

const
    FL_RESIZE_ALL = FL_RESIZE_X or FL_RESIZE_Y;

{ Keyboard focus control }
type
    TFL_KEY = Longint;

Const { Possible values for FL_KEY_Normal }
    FL_KEY_NORMAL = 1;		{ normal keys(0-255) - tab +left/right }
    FL_KEY_TAB = 2;		{ normal keys + 4 direction cursor     }
    FL_KEY_SPECIAL = 4;		{ only needs special keys(>255)        }
    FL_KEY_ALL = 7;		{ all keys                             }

    FL_ALT_VAL = (1 shl 17);	{ alt + Key --> FL_ALT_VAL + key }

{ Internal use }
Type
    TFL_FIND = Longint;
Const
    FL_FIND_INPUT	=0;
    FL_FIND_AUTOMATIC	=1;
    FL_FIND_MOUSE	=2;
    FL_FIND_CANVAS	=3;
    FL_FIND_KEYSPECIAL	=4;

{******************************************************************
 * FONTS
 *****************************************************************}

Const FL_MAXFONTS=32;	{ max number of fonts }

type TFL_TEXT_STYLE = Longint;
Const
    FL_INVALID_STYLE		= -1;
    FL_NORMAL_STYLE		= 0;
    FL_BOLD_STYLE		= 1;
    FL_ITALIC_STYLE		= 2;
    FL_BOLDITALIC_STYLE 	= 3;

    FL_FIXED_STYLE		= 4;
    FL_FIXEDBOLD_STYLE 		= 5;
    FL_FIXEDITALIC_STYLE	= 6;
    FL_FIXEDBOLDITALIC_STYLE	= 7;

    FL_TIMES_STYLE		= 8;
    FL_TIMESBOLD_STYLE		= 9;
    FL_TIMESITALIC_STYLE	= 10;
    FL_TIMESBOLDITALIC_STYLE	= 11;

    { The following are derived and must differ by multiples of SHADOW,
       i.e., (FL_ENGRAVED%SHADOW) == 0. All being 2^n has the benefit that
       (lstyle | FL_SHADOW) == (lstyle + FL_SHADOW). }

Const
    FL_SHADOW_STYLE = (1 shl 9);
    FL_ENGRAVED_STYLE = (1 shl 10);
    FL_EMBOSSED_STYLE = (1 shl 11);


Type TFL_FONT_STYLE= TFL_TEXT_STYLE;

{ Standard sizes in XForms }
Const
FL_TINY_SIZE     =  8;
FL_SMALL_SIZE    =  10;
FL_NORMAL_SIZE   =  12;
FL_MEDIUM_SIZE   =  14;
FL_LARGE_SIZE    =  18;
FL_HUGE_SIZE     =  24;

FL_DEFAULT_SIZE = FL_SMALL_SIZE;

{ Defines for compatibility }

FL_TINY_FONT  = FL_TINY_SIZE;
FL_SMALL_FONT =  FL_SMALL_SIZE;
FL_NORMAL_FONT=  FL_NORMAL_SIZE;
FL_MEDIUM_FONT=  FL_MEDIUM_SIZE;
FL_LARGE_FONT =  FL_LARGE_SIZE;
FL_HUGE_FONT  =  FL_HUGE_SIZE;

FL_NORMAL_FONT1 =  FL_SMALL_FONT;
FL_NORMAL_FONT2 =  FL_NORMAL_FONT;
FL_DEFAULT_FONT =  FL_SMALL_FONT;


const  
FL_BOUND_WIDTH = 3;	{ Border width of boxes }

cFL_BEGIN_GROUP  =  10000;
cFL_END_GROUP    =  20000;
{
 *  Definition of basic struct that holds an object
 }

FL_CLICK_TIMEOUT = 350;	{ double click interval }

Type

TFL_pixmap = record
    thepixmap : Tpixmap;
    Thewin    : TWindow;
    TheVisual : TVisual;
    x,y       : TFL_Coord;
    w, h      : word;
    depth     : Longint;
end;

Pform     = ^TForm;

{Pfl_pixmap = ^fl_pixmap;
}

Pflobjs    = ^Tflobjs;

THandle = Function (p1 : Pflobjs; p2 : Longint {was int}; p3 : TFL_Coord; p4 : TFL_Coord;p5 :  Longint {was int}; p6 : pointer) : Longint;
PHandle = ^THandle;
TObject_Callback = Procedure  (P1 : Pflobjs;p2 :  longint);
PObject_Callback = ^TObject_Callback;

Tflobjs = record
    form : PForm;		{ the form this object belong        }
    u_vdata : Pointer;
    u_ldata : longint;
    
    objclass : Longint {was int};		{ class of object, button, slider etc }
    thetype : Longint;		{ type within the class              }
    boxtype : Longint {was int};		{ what kind of box type              }
    x, y, w, h : TFL_Coord;	{ obj. location and size             }
    bw : TFL_Coord;
    col1, col2 : TFL_color;	{ colors of obj                      }
    thelabel : pchar;		{ object label                       }
    lcol : TFL_COLOR;		{ label color                        }
    align : Longint;
    lsize, lstyle : Longint;	{ label size and style               }

    shortcut : ^Longint;

    handle     : PHandle;
    object_callback : PObject_Callback;
    argument : Longint;

    spec          : pointer;	{ instantiation                      }

    prehandle  : PHandle;
    posthandle : PHandle;


    { re-configure preference }
    resize    : cardinal;	{ what to do if WM resizes the FORM     }
    nwgravity : cardinal;	{ how to re-position top-left corner    }
    segravity : cardinal;	{ how to re-position lower-right corner }

    parent,child,nc : Pflobjs;
    ischild : longint;

    prev : pflobjs;		{ prev. obj in form                  }
    next : pflobjs;		{ next. obj in form                  }

    flpixmap      : Pointer;	{ pixmap double buffering stateinfo  }
    use_pixmap    : Longint;	{ true to use pixmap double buffering }

    double_buffer : Longint;	{ only used by mesa/gl canvas        }

    { some interaction flags }
    pushed: Longint {was int};
    focus: Longint {was int};
    belowmouse: Longint {was int};
    active: Longint {was int};			{ if accept event }
    input: Longint {was int};
    wantkey: Longint {was int};
    radio: Longint {was int};
    automatic: Longint {was int};
    redraw: Longint {was int};
    visible: Longint {was int};
    clip: Longint {was int};
    click_timeout : cardinal;
    c_vdata : pointer;		{ for class use }
    c_ldata : longint;		{ for class use }
    cspec_size : cardinal;
    
    reserved : array[0..5] of cardinal;
    { the following are for application programs }
end;

TFL_OBJECT = TFlobjs;
PFL_OBJECT = ^TFL_OBJECT;

TCallback = Procedure (P1 : pflobjs; P2 : pointer);
PCallback = ^TCallback;
{ callback function for an entire form }
PFL_FORMCALLBACKPTR = PCallback;

{ object callback function      }
PFL_CALLBACKPTR = PCallBack;

{ preemptive callback function  }
PFL_RAW_CALLBACK = PCallback;

{ at close (WM menu delete/close etc.) }
PFL_FORM_ATCLOSE = PCallback;

{ deactivate/activate callback }
PFL_FORM_ATDEACTIVATE = PCallback; 

PFL_FORM_ATACTIVATE = PCallback;

THandlePtr = Procedure ( p1 : PFL_Object; p2 : Longint; p3,p4 : TFL_COORD; p5 : Longint; p6 : pointer);
PHandlePTR = ^THandlePTR;

PFL_HANDLEPTR = PHandlePTR;

{
extern FL_OBJECT *FL_EVENT;
}
{** FORM ***}


Tform = record
    fdui : Pointer;
    u_vdata : pointer;		{ for application         }
    u_ldata : longint;
    
    thelabel : pchar;		{ window title            }
    window : cardinal;	{ X resource ID for window }
    x, y, w, h : TFL_COORD;	{ current geometry info   }
    hotx, hoty : TFL_COORD;	{ hot-spot of the form    }

    first,last,focusobj : pflobjs;

    form_callback     : PFL_FORMCALLBACKPTR;
    activate_callback : PFL_FORM_ATACTIVATE;
    deactivate_callback: PFL_FORM_ATDEACTIVATE ;
    form_cb_data, activate_data, deactivate_data : pointer;

    key_callback, 
    push_callback, 
    crossing_callback,
    motion_callback,
    all_callback: PFL_RAW_CALLBACK ;

    compress_mask : cardinal;
    evmask : cardinal;

    { WM_DELETE_WINDOW message handler }
    close_callback : PFL_FORM_ATCLOSE ;
    close_data : pointer;


    flpixmap : pointer;		{ back buffer             }

    icon_pixmap : cardinal;
    icon_mask : cardinal;

    { interaction and other flags }
    vmode : Longint {was int};			{ current X visual class  }
    deactivated : Longint {was int};		{ true if sensitive       }
    use_pixmap : Longint {was int};		{ true if dbl buffering   }
    frozen : Longint {was int};			{ true if sync change     }
    visible : Longint {was int};		{ true if mapped          }
    wm_border : Longint {was int};		{ window manager info     }
    prop : cardinal;		{ other attributes        }
    has_auto : Longint {was int};
    top : Longint {was int};
    sort_of_modal : Longint;
    reserved : array[0..9] of Longint;
end;
TFL_FORM = Tform;
PFL_FORM = ^TFL_FORM;

{
 * Async IO stuff
 }

const
    FL_READ = 1;
    FL_WRITE = 2;
    FL_EXCEPT = 4;

{ IO other than XEvent Q }
Type

TFL_IO_CALLBACK = Procedure (p1 :Longint {was int};p2 : pointer);
PFL_IO_CALLBACK = ^TFL_IO_CALLBACK;
{ function definitions }
procedure fl_add_io_callback(p1 : Longint {was int} ; p2 : word; p3 : PFL_IO_CALLBACK; p4 : pointer);
procedure fl_remove_io_callback(p1 : Longint {was int};p2 :  word; p3 : PFL_IO_CALLBACK);

{ signals }
type 

TFL_SIGNAL_HANDLER = Procedure (p1 : Longint {was int}; p2 : pointer);
PFL_SIGNAL_HANDLER = ^TFL_SIGNAL_HANDLER ;

procedure fl_add_signal_callback(p1 : LongInt;p2 : PFL_SIGNAL_HANDLER;p3 : pointer);
procedure fl_remove_signal_callback(p1 : Longint {was int});
procedure fl_signal_caught(p1 :Longint {was int});
procedure fl_app_signal_direct(p1 : Longint {was int});

function fl_add_timeout (p1 : longint;p2 : PFL_SIGNAL_HANDLER;p3 : pointer) : longint;
procedure fl_remove_timeout(p1 : Longint);

{  Some utility stuff }
type 

TFL_VN_PAIR = record
    val : Longint;
    name : Pchar;
end;

PFL_VN_PAIR = ^TFL_VN_PAIR;

function fl_get_vn_value (p1 : PFL_VN_PAIR; p2 : pchar) : Longint;
function fl_get_vn_name (p1 : PFL_VN_PAIR; p2 : Longint {was int}) : Pchar;
function fl_msleep (p1 : cardinal) : cardinal;

{
 *  Basic public routine prototypes
 }

procedure fl_library_version(p1,p2 : pointer );

{* Generic routines that deal with FORMS *}

function fl_bgn_form(P1 : Longint {was int}; P2,p3 : TFL_Coord) : PFL_Form;
Procedure fl_end_form;
function fl_do_forms : PFL_OBJECT ;
function fl_check_forms : PFL_OBJECT;
Function fl_do_only_forms : PFL_OBJECT;
function fl_check_only_forms : PFL_OBJECT;
procedure fl_freeze_form(P1 : PFL_FORM);

procedure fl_set_focus_object( P1 : PFL_FORM;p2 : PFL_OBJECT);
procedure fl_reset_focus_object( p1 : PFL_OBJECT);

{
#define fl_set_object_focus   fl_set_focus_object
}
Function fl_set_form_atclose(p1 : PFL_FORM; p2 : PFL_FORM_ATCLOSE; p3 : pointer) : PFL_FORM_ATCLOSE ;
Function fl_set_atclose(p1 : PFL_FORM_ATCLOSE; p2:  pointer) : PFL_FORM_ATCLOSE ;

Function fl_set_form_atactivate(p1 : PFL_FORM; P2 : PFL_FORM_ATACTIVATE; P3 : pointer) : PFL_FORM_ATACTIVATE ;
Function fl_set_form_atdeactivate(p1 : PFL_FORM;p2 : PFL_FORM_ATDEACTIVATE;p3 : pointer) : PFL_FORM_ATDEACTIVATE ;

Procedure fl_unfreeze_form (p1 : PFL_FORM);
Procedure fl_deactivate_form(p1 : PFL_FORM);
Procedure fl_activate_form(p1 : PFL_FORM);
Procedure fl_deactivate_all_forms;
Procedure fl_activate_all_forms;
Procedure fl_freeze_all_forms;
Procedure fl_unfreeze_all_forms;
Procedure fl_scale_form(p1 : PFL_FORM;p2,p3 : double);
Procedure fl_set_form_position(p1 : PFL_FORM; p2,p3 : TFL_Coord);
Procedure fl_set_form_title(p1 : PFL_FORM; p2 : pchar);

procedure fl_set_form_property(p1 : PFL_FORM; p2 : word);
procedure fl_set_app_mainform( p1 : PFL_FORM);
function fl_get_app_mainform : PFL_FORM;
procedure fl_set_app_nomainform(P1 : Longint {was int});

procedure fl_set_form_callback(p1 : PFL_FORM; P2 : PFL_FORMCALLBACKPTR; P3 : pointer);
{
#define  fl_set_form_call_back    fl_set_form_callback
}

procedure fl_set_form_size(p1 : PFL_FORM; p2,p3 : TFL_Coord);
procedure fl_set_form_hotspot(p1 : PFL_FORM; p2,p3 : TFL_Coord);
procedure fl_set_form_hotobject(p1 : PFL_FORM; p2 : PFL_OBJECT);
procedure fl_set_form_minsize(p1 : PFL_FORM ; p2,p3 :TFL_Coord);
procedure fl_set_form_maxsize(p1 : PFL_FORM; p2,p3 :TFL_Coord);
procedure fl_set_form_event_cmask(p1 : PFL_FORM; p2 : cardinal);
function fl_get_form_event_cmask(p1 : PFL_FORM) : cardinal;

procedure fl_set_form_geometry(p1 : PFL_FORM; p2,p3,p4,p5 : TFL_Coord);

{
#define fl_set_initial_placement fl_set_form_geometry
}

function fl_show_form (p1 : PFL_FORM; p2,p3 : Longint {was int}; p4 : pchar) : Longint;
procedure fl_hide_form(p1 : PFL_FORM);
procedure fl_free_form(p1 : PFL_FORM);
procedure fl_redraw_form(p1 : PFL_FORM);
procedure fl_set_form_dblbuffer(p1 : PFL_FORM; p2 : Longint {was int});
procedure fl_prepare_form_window(p1 : PFL_FORM; p2,p3 :  Longint {was int}; p4 : pchar);
procedure fl_show_form_window(p1 : PFL_FORM);
function fl_adjust_form_size(p1 : PFL_FORM) : double;
function fl_form_is_visibe (p1 : PFL_FORM) : Longint;

function  fl_register_raw_callback(p1 : PFL_FORM; p2 : cardinal; P3 : PFL_RAW_CALLBACK): PFL_RAW_CALLBACK;

{
#define fl_register_call_back fl_register_raw_callback
}
function fl_bgn_group : PFL_OBJECT;
function fl_end_group : PFL_OBJECT;
procedure fl_addto_group (p1 : PFL_OBJECT);

{***** Routines that deal with FL_OBJECTS *******}

procedure fl_set_object_boxtype(p1 : PFL_OBJECT; p2 : Longint {was int});
procedure fl_set_object_bw(p1 : PFL_OBJECT; p2 : Longint {was int});
procedure fl_set_object_resize(p1 : PFL_OBJECT; p2 : word);
procedure fl_set_object_gravity(p1 : PFL_OBJECT; p2,p3 : word );
procedure fl_set_object_lsize(p1 : PFL_OBJECT; p2 : Longint {was int});
procedure fl_set_object_lstyle(p1 : PFL_OBJECT; p2 : Longint {was int});
procedure fl_set_object_lcol(p1 : PFL_OBJECT; p2 : TFL_COLOR);
procedure fl_set_object_return(p1 : PFL_OBJECT; p2 : Longint {was int});
procedure fl_set_object_lalign(p1 : PFL_OBJECT; p2 : Longint {was int});	{ to be removed }
procedure fl_set_object_shortcut(p1 : PFL_OBJECT; p2 : pchar;p3 : Longint {was int});
procedure fl_set_object_shortcutkey(p1 : PFL_OBJECT; p2 :  word);
procedure fl_set_object_dblbuffer(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_object_color(p1 : PFL_OBJECT; p2 : TFL_COLOR;p3 : TFL_COLOR);
procedure fl_set_object_label(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_set_object_position(p1 : PFL_OBJECT; p2,p3 :TFL_Coord);
procedure fl_set_object_size(p1 : PFL_OBJECT; p2,p3 :TFL_Coord);
procedure fl_set_object_automatic(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_draw_object_label(p1 : PFL_OBJECT); 
procedure fl_draw_object_label_outside(p1 : PFL_OBJECT); 

{
#define  fl_set_object_dblclick(p1 : ob; p2 : timeout)  (p1 : ob)->click_timeout = (p1 : timeout); p3 :p2 : 
}
procedure fl_set_object_geometry(p1 : PFL_OBJECT; p2,p3,p4,p5 :TFL_Coord);


procedure fl_fit_object_label(p1 : PFL_OBJECT; p2,p3 :TFL_Coord);

{ no much get (p1 : yet ?) }

procedure fl_get_object_geometry(p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_Coord );
procedure fl_get_object_position(p1 : PFL_OBJECT; p2,p3 :PFL_COORD);

{ this one takes into account the label }
procedure fl_get_object_bbox (p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_COORD);

procedure fl_call_object_callback(p1 : PFL_OBJECT); 
function fl_set_object_prehandler(p1 : PFL_OBJECT; p2 : PFL_HANDLEPTR) : PFL_HANDLEPTR ;
function fl_set_object_posthandler(p1 : PFL_OBJECT; p2 : PFL_HANDLEPTR): PFL_HANDLEPTR ;
function fl_set_object_callback(p1 : PFL_OBJECT; p2 : PFL_CALLBACKPTR; p3 :longint) : PFL_CALLBACKPTR ;
{
#define fl_set_object_align   fl_set_object_lalign
#define fl_set_call_back      fl_set_object_callback
}
procedure fl_redraw_object(p1 : PFL_OBJECT);
procedure fl_scale_object(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_show_object(p1 : PFL_OBJECT); 
procedure fl_hide_object(p1 : PFL_OBJECT);
procedure fl_free_object(p1 : PFL_OBJECT);  
procedure fl_delete_object(p1 : PFL_OBJECT);  
procedure fl_trigger_object(p1 : PFL_OBJECT);  
procedure fl_activate_object(p1 : PFL_OBJECT);  
procedure fl_deactivate_object(p1 : PFL_OBJECT);

Type TFL_ENUMERATEPTR = procedure (p : pchar); 
     PFL_ENUMERATEPTR = ^TFL_ENUMERATEPTR;

procedure fl_enumerate_fonts (p1 : PFL_ENUMERATEPTR; p2 :  longint);

function  fl_set_font_name(p1 : Longint {was int}; p2 :  pchar) : longint;
procedure fl_set_font(p1 : Longint {was int}; p2 :  Longint {was int});

{ routines that facilitate free object }

function fl_get_char_height(p1,p2 : Longint {was int}; p3,p4 : pointer) : Longint {was int};
function fl_get_char_width(p1,p2 :  Longint {was int}) : Longint;
function fl_get_string_height(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}; p5,p6 : pointer) : Longint {was int};
function fl_get_string_width(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}) : Longint {was int};
function fl_get_string_widthTAB(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}) : Longint {was int};
function fl_get_string_dimension(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}; p5,p6 : pointer) : Longint {was int};
{
#define fl_get_string_size  fl_get_string_dimension
}
procedure fl_get_align_xy(p1,p2,p3,p4,p5,p6,p7,p8,p9 : Longint {was int}; p10,p11 : pointer);

procedure fl_drw_text(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7,p8 : Longint {was int}; p9 : pchar);

procedure fl_drw_text_beside(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7,p8 : Longint {was int}; p9 : pchar);
{
#define fl_draw_text(p1 : a; p2 : x;y;w;h;c;st;sz;s)    \
      (p1 : (p1 : (p1 : a) & FL_ALIGN_INSIDE) ? fl_drw_text:fl_drw_text_beside)\
      (p1 : a; p2 : x;y;w;h;c;st;sz;s)
}

procedure fl_drw_text_cursor(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6,p7,p8: Longint {was int}; p9 : Pchar; p10,p11: Longint {was int});

procedure fl_drw_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7 : Longint {was int});

type

FL_DRAWPTR = Procedure (p1,p2,p3,p4 : TFL_Coord; p5 : Longint {was int};p6: TFL_COLOR);
PFL_DRAWPTR = ^FL_DRAWPTR;

function fl_add_symbol(p1 : pchar; p2 : PFL_DRAWPTR; p3 :Longint {was int}) : Longint {was int};
function fl_draw_symbol(p1 : pchar; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR) : Longint {was int};

const
  FL_SLIDER_NONE = 0;
  FL_SLIDER_BOX = 1;
  FL_SLIDER_KNOB = 2;
  FL_SLIDER_UP = 4;
  FL_SLIDER_DOWN = 8;
  FL_SLIDER_ALL = 15;
  
procedure fl_drw_slider (p1 :longint; p2,p3,P4,p5 : TFL_COORD; p6,p7 : TFL_COLOR;
                         p8 : Longint; p9,p10 : double; P11 : pchar; p12,p13,p14 : Longint);
                         

function fl_mapcolor(p1 : TFL_COLOR; p2,p3,p4 : Longint {was int}) : cardinal;
function fl_mapcolorname(p1 : TFL_COLOR; p2 :  pchar) : longint;
{
#define fl_mapcolor_name  fl_mapcolorname
}
function fl_getmcolor(p1 : TFL_COLOR; p2,p3,p4 : pointer) : cardinal;
procedure fl_free_colors(p1 : PFL_COLOR; p2 :  Longint {was int});
procedure fl_free_pixels(p1 : pcardinal; p2 :  Longint {was int});
procedure fl_set_color_leak(p1 : Longint {was int}); 
function fl_get_pixel(p1 : TFL_COLOR): cardinal; 
{
#define fl_get_flcolor   fl_get_pixel
}
procedure fl_get_icm_color(p1 : TFL_COLOR; p2,p3,p4 : pointer);
procedure fl_set_icm_color(p1 : TFL_COLOR; p2,p3,p4 : Longint {was int});

procedure fl_color(p1 : TFL_COLOR);
procedure fl_bk_color(p1 : TFL_COLOR); 
procedure fl_textcolor(p1 : TFL_COLOR); 
procedure fl_bk_textcolor(p1 : TFL_COLOR); 
procedure fl_set_gamma(p1,p2,p3 :double);

procedure fl_show_errors(p1 : Longint {was int}); 

{ Some macros }
{
#define FL_max(p1 : a; p2 : b)      (p1 :  (p1 : a) > (p1 : b) ? (p1 : a):(p1 : b) )
#define FL_min(p1 : a; p2 : b)      (p1 :  (p1 : a) < (p1 : b) ? (p1 : a):(p1 : b) )
#define FL_abs(p1 : a)        (p1 :  (p1 : a) > 0 ? (p1 : a):(p1 : -(p1 : a)))
#define FL_nint(p1 : a)       (p1 :  (p1 : a) > 0 ? (p1 : (p1 : a) + 0.5):(p1 : (p1 : a) - 0.5))
}
type TFL_FSCB = function (p1 : pchar; p2 : pointer) : Longint;
PFL_FSCB = ^TFL_FSCB;

{ utilities for new objects }
function fl_current_form : PFL_FORM;
procedure fl_add_object(p1 : PFL_FORM; p2 : PFL_OBJECT);
procedure fl_addto_form(p1 : PFL_FORM); 
function fl_make_object(p1 : Longint {was int}; p2,p3,p4,p5,p6 : TFL_Coord; p7 : pchar; P8 : PFL_HANDLEPTR) : PFL_OBJECT;

procedure fl_set_coordunit(p1 : Longint {was int});
function fl_get_coordunit : Longint {was int}; 
procedure fl_set_border_width(p1 : Longint {was int}); 
function fl_get_border_width : Longint {was int};  
procedure fl_flip_yorigin; 

procedure fl_ringbell ( p1 : longint);
procedure fl_gettime (p1,p2 : Plongint);
function fl_mouse_button : longint;


{ this gives more flexibility for future changes }

{
#define fl_free       free
#define fl_malloc     malloc
#define fl_calloc     calloc
#define fl_realloc    realloc
}
{
 * $Id$
 *
 *  X Window dependent stuff
 *
 }

{
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
}
Const
 FL_MINDEPTH = 1;


{ FL_xxx does not do anything anymore; but kept for compatibility }
    FL_illegalVisual = -1;
    FL_StaticGray = StaticGray;
    FL_GrayScale = GrayScale;
    FL_StaticColor = StaticColor;
    FL_PseudoColor = PseudoColor;
    FL_TrueColor = TrueColor;
    FL_DirectColor = DirectColor;
    FL_DefaultVisual = 10;	{ special request }

    FL_North = NorthGravity;
    FL_NorthEast = NorthEastGravity;
    FL_NorthWest = NorthWestGravity;
    FL_South = SouthGravity;
    FL_SouthEast = SouthEastGravity;
    FL_SouthWest = SouthWestGravity;
    FL_East = EastGravity;
    FL_West = WestGravity;
    FL_NoGravity = ForgetGravity;
    FL_ForgetGravity = ForgetGravity;

{
#define FL_is_gray(p1 : v)  (p1 : v==GrayScale || v==StaticGray)
#define FL_is_rgb(p1 : v)   (p1 : v==TrueColor || v==DirectColor)
}

{
 * Internal colormap size. Not really very meaningful as fl_mapcolor
 * and company allow color "leakage"; that is; although only FL_MAX_COLS
 * are kept in the internal colormap; the server might have substantially
 * more colors allocated
 }

FL_MAX_COLS =  1024;

{
 * FL graphics state information. Some are redundant.
 }

type
TFL_STATE = record
    xvinfo : PXVisualInfo;
    cur_fnt : PXFontStruct;	{ current font in default GC       }
    colormap : TColormap ;		{ colormap valid for xvinfo        }
    trailblazer : TWindow ;		{ a valid window for xvinfo        }
    vclass, depth,		{ visual class and color depth     }
    rgb_bits,		{ primary color resolution         }
    dithered,		{ true if dithered color           }
    pcm : Longint;			{ true if colormap is not shared   }
    gc : Array [0..16] of TGC;			{ working GC                       }
    textgc : array [0..16] of TGC;		{ GC used exclusively for text     }
    dimmedGC : TGC;		{ A GC having a checkboard stipple }
    lut :array [0..FL_MAX_COLS] of cardinal;	{ secondary lookup table         }
    rshift : word; 
    rmask, rbits : Longint;
    gshift : word;
    gmask,gbits : Longint {was int};
    bshift : word; 
    bmask, bbits : Longint {was int};
end;
PFL_State = ^TFL_State;

{
#define FL_State FL_STATE	{ for compatibility }
}

{**** Global variables *****}

Var

fl_display : PDisplay;
fl_screen : Longint;
fl_root : TWindow;		{ root window                }
fl_vroot : TWindow;		{ virtual root window        }
fl_scrh : Longint; 
fl_scrw : Longint;	{ screen dimension in pixels }
fl_vmode : Longint {was int};

{ current version only runs in single visual mode }
{
#define  fl_get_vclass(p1 : )        fl_vmode
#define  fl_get_form_vclass(p1 : a)  fl_vmode
}
{
fl_state[] : PFL_State ;
fl_ul_magic_char : pchar;
}

function fl_mode_capable(p1,p2 : longint ) : longint;

{
#define fl_default_win(p1 : )       (p1 : fl_state[fl_vmode].trailblazer)
#define fl_default_window(p1 : )    (p1 : fl_state[fl_vmode].trailblazer)
}
{
 * All pixmaps used by FL_OBJECT to simulate double buffering have the
 * following entries in the structure. TFL_Coord x;y are used to shift
 * the origin of the drawing routines
 }


{ fonts related }
Const
FL_MAX_FONTSIZES=10;

type
 FL_FONT = record
    fs : array[0..FL_MAX_FONTSIZES] of PXFontStruct;	{ cached fontstruct }
    size : array [0..FL_MAX_FONTSIZES] of byte;	{ cached sizes      }
    nsize : byte;		{ cached so far     }
    fname : Array[1..80] of char;		{ without size info     }
end;

{
 * Some basic drawing routines
 }

type TFL_POINT = TXPoint;
     PFL_Point = ^TFL_Point;
     TFL_RECT = TXRECTANGLE;
     PFL_RECT = ^TFL_RECT;     
     
{ rectangles }
procedure fl_rectangle(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR);
procedure fl_rectbound(p1,p2,p3,p4 : TFL_Coord;p5 : TFL_COLOR);

procedure fl_rectf(x,y,w,h : TFL_COORD;c : TFL_COLOR);
procedure fl_rect(x,y,w,h : TFL_COORD;c : TFL_COLOR);


{ rectangle with rounded-corners }
procedure fl_roundrectangle(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 :TFL_COLOR);

{
#define fl_roundrectf(p1 : x; p2 : y;w;h;c) fl_roundrectangle(p1 : 1; p2 : x;y;w;h;c)
#define fl_roundrect(p1 : x; p2 : y;w;h;c) fl_roundrectangle(p1 : 0; p2 : x;y;w;h;c)
}
{ general polygon and polylines }
procedure fl_polygon(p1 : Longint {was int}; p2 : PFL_POINT; p3 : Longint {was int}; p4 : TFL_COLOR);

{
#define fl_polyf(p1 : p; p2 : n;c)  fl_polygon(p1 : 1; p2 : p; p3 : n; p4 : c)
#define fl_polyl(p1 : p; p2 : n;c)  fl_polygon(p1 : 0; p2 : p; p3 : n; p4 : c)
#define fl_polybound(p1 : p; p2 : n;c) do {fl_polyf(p1 : p; p2 : n;c);fl_polyl(p1 : p; p2 : n;FL_BLACK);}while(p1 : 0)
}

procedure fl_lines(p1 : TFL_POINT; p2 : Longint {was int}; p3 :TFL_COLOR);
procedure fl_line(p1,p2,p3,p4 : TFL_Coord; p5 : TFL_COLOR);
{
#define fl_simple_line fl_line
}
procedure fl_dashedlinestyle(p1 : pchar; p2 :  Longint {was int});
procedure fl_drawmode(p1 : Longint {was int}); 

{
#define fl_diagline(p1 : x; p2 : y;w;h;c) fl_line(p1 : x; p2 : y;(p1 : x)+(p1 : w)-1; p2 : (p1 : y)+(p1 : h)-1; p2 : c)
}
{ line attributes }
procedure fl_linewidth(p1 : Longint {was int}); 
procedure fl_linestyle(p1 : Longint {was int}); 

function fl_get_linewidth : longint;
function fl_get_linestyle : longint;


{* ellipses *}
procedure fl_oval(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR);
procedure fl_ovalbound(p1,p2,p3,p4 : TFL_Coord; p5 : TFL_COLOR);
procedure fl_ovalarc(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6,p7 : Longint; p8 : TFL_COLOR);

{
#define fl_ovalf(p1 : x; p2 : y;w;h;c)     fl_oval(p1 : 1; p2 : x;y;w;h;c)
#define fl_ovall(p1 : x; p2 : y;w;h;c)     fl_oval(p1 : 0; p2 : x;y;w;h;c)
#define fl_oval_bound           fl_ovalbound

#define fl_circf(p1 : x; p2 : y;r;col)  fl_oval(p1 : 1; p2 : (p1 : x)-(p1 : r); p2 : (p1 : y)-(p1 : r); p2 : 2*(p1 : r); p2 : 2*(p1 : r); p2 : col)
#define fl_circ(p1 : x; p2 : y;r;col)   fl_oval(p1 : 0; p2 : (p1 : x)-(p1 : r); p2 : (p1 : y)-(p1 : r); p2 : 2*(p1 : r); p2 : 2*(p1 : r); p2 : col)
}

{ arcs }
procedure fl_pieslice(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6,p7 :Longint {was int}; p8 : TFL_COLOR);
{
#define fl_arcf(p1 : x; p2 : y;r;a1;a2;c)  fl_pieslice(p1 : 1; p2 : (p1 : x)-(p1 : r); p2 : (p1 : y)-(p1 : r); p2 : \
                                (p1 : 2*(p1 : r)); p2 : (p1 : 2*(p1 : r)); p2 :  a1;a2;c)

#define fl_arc(p1 : x; p2 : y;r;a1;a2;c)  fl_pieslice(p1 : 0; p2 : (p1 : x)-(p1 : r); p2 : (p1 : y)-(p1 : r); p2 :  \
                               (p1 : 2*(p1 : r)); p2 : (p1 : 2*(p1 : r)); p2 :  a1;a2;c)
}
{ misc. stuff }
procedure fl_add_vertex(p1,p2 :  TFL_Coord);
procedure fl_add_float_vertex(p1,p2 :  real);
procedure fl_reset_vertex; 
procedure fl_endline;
procedure fl_endpolygon;
procedure fl_endclosedline; 

{
#define fl_bgnline       fl_reset_vertex
#define fl_bgnclosedline fl_reset_vertex
#define fl_bgnpolygon    fl_reset_vertex
#define fl_v2s(p1 : v)        fl_add_vertex(p1 : v[0]; p2 :  v[1])
#define fl_v2i(p1 : v)        fl_add_vertex(p1 : v[0]; p2 :  v[1])
#define fl_v2f(p1 : v)        fl_add_float_vertex(p1 : v[0]; p2 :  v[1])
#define fl_v2d(p1 : v)        fl_add_float_vertex(p1 : v[0]; p2 :  v[1])
}

{ high level drawing routines }
procedure fl_drw_frame(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR; p7 : Longint {was int});
procedure fl_drw_checkbox(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR;p7 : Longint {was int});

{
 * Interfaces
 }
function fl_get_fontstruct(p1,p2 :  Longint {was int}) : PXFontStruct;
{
#define fl_get_font_struct fl_get_fontstruct
#define fl_get_fntstruct fl_get_font_struct
}

function fl_get_mouse(p1,p2 : PFL_COORD; p3 : pword) : TWindow;
procedure fl_set_mouse(p1,p2 :  TFL_Coord);
function fl_get_win_mouse(p1 : TWindow; p2,p3 : PFL_COORD; p4 : pword) : TWindow;
function fl_get_form_mouse(p1 : PFL_FORM; p2,p3 : PFL_COORD; p4 : pWord) : TWindow;
function fl_win_to_form(p1 : TWindow) : PFL_form; 
procedure fl_set_form_icon(p1 : PFL_FORM; p2 : TPixmap; p3 :TPixmap);

{
#define fl_raise_form(p1 : f) if(p1 : f->window) XRaiseWindow(p1 : fl_display; p2 : f->window)
#define fl_lower_form(p1 : f) if(p1 : f->window) XLowerWindow(p1 : fl_display; p2 : f->window)
}
{
#define fl_set_foreground(p1 : gc; p2 : c) XSetForeground(p1 : fl_display; p2 : gc;fl_get_pixel(p1 : c))
#define fl_set_background(p1 : gc; p2 : c) XSetBackground(p1 : fl_display; p2 : gc;fl_get_pixel(p1 : c))
}
{ General windowing support }

function fl_wincreate(p1 : pchar) : TWindow; 
function fl_winshow(p1 : TWindow) : TWindow; 
function fl_winopen(p1 : pchar) : TWindow; 
procedure fl_winhide(p1 : TWindow); 
procedure fl_winclose(p1 : TWindow);  
procedure fl_winset(p1 : TWindow); 
function fl_winget : Twindow; 

procedure fl_winresize(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_winmove(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_winreshape(p1 : TWindow; p2,p3,p4,p5 : TFL_Coord);
procedure fl_winicon(p1 : TWindow; p2 : TPixmap; p3 :TPixmap);
procedure fl_winbackground(p1 : TWindow; p2 :  cardinal);
procedure fl_winstepunit(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_winisvalid(p1 : TWindow); 
procedure fl_wintitle(p1 : TWindow; p2 :  pchar);
procedure fl_winposition(p1,p2 :  TFL_Coord);

{
#define fl_pref_winposition fl_winposition
#define fl_win_background     fl_winbackground
#define fl_set_winstepunit    fl_winstepunit
}


procedure fl_winminsize(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_winmaxsize(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_winaspect(p1 : TWindow; p2,p3 :TFL_Coord);
procedure fl_reset_winconstraints(p1 : TWindow); 

procedure fl_winsize(p1,p2 :  TFL_Coord);
procedure fl_initial_winsize(p1,p2 :  TFL_Coord);
{
#define fl_pref_winsize  fl_winsize
}
procedure fl_initial_winstate(p1 : Longint {was int}); 

function fl_create_colormap(p1 : PXVisualInfo; p2 :  Longint {was int}) : TColormap;


procedure fl_wingeometry(p1,p2,p3,p4 : TFL_Coord);
{
#define fl_pref_wingeometry  fl_wingeometry
}
procedure fl_initial_wingeometry(p1,p2,p3,p4 : TFL_Coord);

{
procedure fl_noborder;
procedure fl_transient;
}

procedure fl_get_winsize(p1 : TWindow; p2,p3 :PFL_COORD);
procedure fl_get_winorigin(p1 : TWindow; p2,p3 :PFL_COORD);
procedure fl_get_wingeometry(p1 : TWindow; p2,p3,p4,p5 : PFL_COORD);

{ for compatibility }
{
#define fl_get_win_size          fl_get_winsize
#define fl_get_win_origin        fl_get_winorigin
#define fl_get_win_geometry      fl_get_wingeometry
#define fl_initial_winposition   fl_pref_winposition

#define fl_get_display(p1 : )           fl_display
#define FL_FormDisplay(p1 : form)       fl_display
#define FL_ObjectDisplay(p1 : object)   fl_display
}

{ the window an object belongints }

Function FL_ObjWin ( P : PFL_Object) : TWindow;

{
#define FL_OBJECT_WID  FL_ObjWin
}

{  all registerable events; including Client Message }
const FL_ALL_EVENT = (KeyPressMask or KeyReleaseMask or
                      ButtonPressMask or ButtonReleaseMask or
                      EnterWindowMask or LeaveWindowMask or
                      ButtonMotionMask or PointerMotionMask);

{ Timer related }

 FL_TIMER_EVENT = $40000000;


function fl_XNextEvent(p1 : PXEvent) : Longint; 
function fl_XPeekEvent(p1 : PXEvent) : Longint; 
function fl_XEventsQueued(p1 : Longint {was int}) : Longint; 
procedure fl_XPutBackEvent(p1 : PXEvent);  
function fl_last_event : PXEvent ; 

type
TFL_APPEVENT_CB = procedure (p1 : PXEvent; p2 : pointer);
PFL_APPEVENT_CB = ^TFL_APPEVENT_CB;

function fl_set_event_callback(p1 : PFL_APPEVENT_CB; p2 : pointer) : PFL_APPEVENT_CB ;
function fl_set_idle_callback(p1 : PFL_APPEVENT_CB; p2 :  pointer) : PFL_APPEVENT_CB ;
function fl_addto_selected_xevent(p1 : TWindow; p2 :  longint) : Cardinal;
function fl_remove_selected_xevent(p1 : TWindow; p2 :  longint) : cardinal;
{
#define fl_add_selected_xevent  fl_addto_selected_xevent
}
{
 * Group some WM stuff into a structure for easy maintainance
 }
const
    FL_WM_SHIFT = 1;
    FL_WM_NORMAL = 2;

type TFL_WM_STUFF = record
    rpx, rpy,		{ reparenting offset for full border }
    trpx, trpy,		{ reparenting offset for transient   }
    bw,			{ additional border                  }
    rep_method : Longint;		{ 1 for shifting; 2 for normal       }
    pos_request : word;	{ USPOSITION or PPOSITION            }
end;
PFL_WM_STUFF = ^TFL_WM_STUFF;


function fl_add_event_callback(p1 : TWindow; p2 :  Longint {was int}; p3 : PFL_APPEVENT_CB; p4 : pointer) : PFL_APPEVENT_CB ;

procedure fl_remove_event_callback(p1 : TWindow; p2 :  Longint {was int});
procedure fl_activate_event_callbacks(p1 : TWindow); 

function fl_print_xevent_name(p1 : pchar; p2 : PXEvent) : PXEvent;

{
#define metakey_down(p1 : mask)     (p1 : (p1 : mask) & Mod1Mask)
#define shiftkey_down(p1 : mask)    (p1 : (p1 : mask) & ShiftMask)
#define controlkey_down(p1 : mask)  (p1 : (p1 : mask) & ControlMask)
#define button_down(p1 : mask)      (p1 : (p1 : (p1 : mask) & Button1Mask) || \
                               (p1 : (p1 : mask) & Button2Mask) || \
			       (p1 : (p1 : mask) & Button3Mask))
#define fl_keypressed          fl_keysym_pressed
}
{***************** Resources **************}


{ bool is Longint {was int}. FL_NONE is defined elsewhere }
const
    FL_SHORT  = 10; 
    FL_BOOL   = 11; 
    FL_INT    = 12;
    FL_LONG   = 13; 
    FL_FLOAT  = 14; 
    FL_STRING = 15;
    
Type  TFL_RTYPE = Longint;

TFL_RESOURCE = record
    res_name,			{ resource name                        }
    res_class : Pchar;		{ resource class                       }
    rtype : TFL_RTYPE;		{ FL_INT; FL_FLOAT; FL_BOOL;FL_STRING  }
    thevar : pointer;		{ address for the variable             }
    defval : pchar;		{ default setting in string form       }
    bytes : Longint {was int}		{ used only for strings                }
end;
PFL_RESOURCE = ^TFL_RESOURCE;

Type
 TFL_CMD_OPT = TXrmOptionDescRec;
 PFL_CMD_OPT = ^TFL_CMD_OPT;
 
function fl_initialize(p1 : pointer; p2 : ppchar; p3 :pchar; p4 : PFL_CMD_OPT; p5 : Longint {was int}) : Pdisplay;
procedure fl_finish;  

{ addfromhere }

function fl_get_resource(p1 : pchar; p2 :  pchar; p3 : TFL_RTYPE; p4 : pchar; p5 : pointer; p6 :Longint {was int}) : pchar;
procedure fl_set_resource(p1 : pchar; p2 :  pchar);

procedure fl_get_app_resources(p1 : PFL_resource; p2 : Longint {was int});
procedure fl_set_graphics_mode(p1 : Longint {was int}; p2 :  Longint {was int});
procedure fl_set_visualID(p1 : longint);  
function fl_keysym_pressed(p1 : TKeySym) : Longint; 

{
#define buttonLabelSize  buttonFontSize
#define sliderLabelSize  sliderFontSize
#define inputLabelSize   inputFontSize
}
{ All Form control variables. Named closely as its resource name }
Type TFL_IOPT =  record
    rgamma, ggamma, bgamma : Real;
    debug, sync,
    depth, vclass, doubleBuffer,
    ulPropWidth, ulThickness,	{ underline stuff       }
    buttonFontSize,
    sliderFontSize,
    inputFontSize,
    browserFontSize,
    menuFontSize,
    choiceFontSize,
    labelFontSize,		{ all other labels fonts }
    pupFontSize, pupFontStyle,	{ font for pop-up menus  }
    privateColormap,
    sharedColormap,
    standardColormap,
    leftScrollBar,
    backingStore,
    coordUnit,
    borderWidth,
    safe,
    xFirst : Longint {was int};
    rgbfile : pchar;		{ where RGB file is     }
    vname : array [0..24] of char;
end;
PFL_IOPT = ^TFL_IOPT; 


Const

{ program default masks }

    FL_PDDepth = 1 shl 1 ; 
    FL_PDClass = 1 shl 2 ; 
    FL_PDDouble = 1 shl 3 ; 
    FL_PDSync = 1 shl 4 ; 
    FL_PDPrivateMap = 1 shl 5 ; 
    FL_PDLeftScrollBar = 1 shl 6 ; 
    FL_PDPupFontSize = 1 shl 7 ; 
    FL_PDButtonFontSize = 1 shl 8 ; 
    FL_PDInputFontSize = 1 shl 9 ; 
    FL_PDSliderFontSize = 1 shl 10 ; 
    FL_PDVisual = 1 shl 11 ; 
    FL_PDULThickness = 1 shl 12 ; 
    FL_PDULPropWidth = 1 shl 13 ; 
    FL_PDBS = 1 shl 14 ; 
    FL_PDCoordUnit = 1 shl 15 ; 
    FL_PDDebug = 1 shl 16 ; 
    FL_PDSharedMap = 1 shl 17 ; 
    FL_PDStandardMap = 1 shl 18 ; 
    FL_PDBorderWidth = 1 shl 19 ; 
    FL_PDSafe = 1 shl 20 ; 
    FL_PDMenuFontSize = 1 shl 21 ; 
    FL_PDBrowserFontSize = 1 shl 22 ; 
    FL_PDChoiceFontSize = 1 shl 23 ; 
    FL_PDLabelFontSize = 1 shl 24;
    FL_PDButtonLabelSize = FL_PDButtonFontSize;
    FL_PDSliderLabelSize = FL_PDSliderFontSize;
    FL_PDInputLabelSize  = FL_PDInputFontSize;
    FL_PDButtonLabel  = FL_PDButtonLabelSize;
 
procedure fl_set_defaults(p1 : cardinal; p2 :  PFL_IOPT);
procedure fl_set_tabstop(p1 : pchar); 
procedure fl_get_defaults(p1 : PFL_IOPT); 
function fl_get_visual_depth : Longint; 
function fl_vclass_name(p1 : Longint {was int}) : pchar; 
function fl_vclass_val(p1 : pchar) : Longint {was int}; 
procedure fl_set_ul_property(p1 : Longint {was int}; p2 :  Longint {was int});
procedure fl_set_clipping(p1 : TFL_Coord; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord);
procedure fl_set_gc_clipping(p1 : TGC; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord);
procedure fl_unset_gc_clipping(p1 : TGC); 
procedure fl_set_clippings(p1 : PFL_RECT; p2 :  Longint {was int});
procedure fl_unset_clipping; 

function fl_textgc : TGC;
{
#define fl_set_text_clipping(p1 : a; p2 : b;c;d)   fl_set_gc_clipping(p1 : fl_textgc; p2 : a;b;c;d)
#define fl_unset_text_clipping(p1 : ) fl_unset_gc_clipping(p1 : fl_textgc)
}

Const
 FL_NORMAL_BITMAP = 0;

{**** Defaults ****}
FL_BITMAP_BOXTYPE	=FL_NO_BOX;
FL_BITMAP_COL1		=FL_COL1;	{ background of bitmap }
FL_BITMAP_COL2		=FL_COL1;	{ not used currently   }
FL_BITMAP_LCOL		=FL_LCOL;	{ foreground of bitmap }
FL_BITMAP_ALIGN		=FL_ALIGN_BOTTOM;

{**** Others   ****}

{FL_BITMAP_MAXSIZE	= 128*128;}

{**** Routines ****}
function fl_create_bitmap(p1 : Longint {was int}; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_bitmap(p1 : Longint {was int}; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
procedure fl_set_bitmap_data(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : pointer);
procedure fl_set_bitmap_file(p1 : PFL_OBJECT; p2 :  pchar);
function fl_read_bitmapfile(p1 : TWindow; p2 :  pchar; p3,p4 : pword; p5,p6 : pointer) : TPixmap ;

{
#define fl_create_from_bitmapdata(p1 : win; p2 : data; p3 : w; p4 : h)\
                   XCreateBitmapFromData(p1 : fl_get_display(p1 : ); p2 : win; p3 :\
                   (p1 : char *)data; p2 : w; p3 :h)

{ for compatibility }
#define fl_set_bitmap_datafile fl_set_bitmap_file
}

{ PIXMAP stuff }

Const
FL_NORMAL_PIXMAP   = 0;

function fl_create_pixmap(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6  :pchar) : PFL_OBJECT;
function fl_add_pixmap(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;

procedure fl_set_pixmap_data(p1 : PFL_OBJECT; p2 :  ppchar);
procedure fl_set_pixmap_file(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_set_pixmap_align(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : Longint {was int});
procedure fl_set_pixmap_pixmap(p1 : PFL_OBJECT; p2 : TPixmap; p3 :TPixmap);
procedure fl_set_pixmap_colorcloseness(p1 : Longint {was int}; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_free_pixmap_pixmap(p1 : PFL_OBJECT); 
function fl_get_pixmap_pixmap(p1 : PFL_OBJECT; p2 : PPixmap; p3 : PPixmap) : TPixmap ;

function fl_read_pixmapfile(p1 : TWindow; p2 :  pchar;p3,p4 : pword; p5 : PPixmap;p6,p7 : pointer; p8 : TFL_COLOR)  : TPixmap ;
function fl_create_from_pixmapdata(p1 : TWindow; p2 :  ppchar;p3,p4 : pword; p5 : PPixmap;p6,p7 : pointer; p8 : TFL_COLOR) : TPixmap ;
{
#define fl_free_pixmap(p1 : id)  if(p1 : id != None) XFreePixmap(p1 : fl_display; p2 :  id);
}

function fl_create_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;

Type TFL_BROWSER_TYPE = Longint;

Const
    FL_NORMAL_BROWSER = 0;
    FL_SELECT_BROWSER = 1;
    FL_HOLD_BROWSER   = 2;
    FL_MULTI_BROWSER  = 3;

{**** Defaults ****}

 FL_BROWSER_BOXTYPE	= FL_DOWN_BOX;
 FL_BROWSER_COL1	= FL_COL1;
 FL_BROWSER_COL2	= FL_YELLOW;
 FL_BROWSER_LCOL	= FL_LCOL;
 FL_BROWSER_ALIGN	= FL_ALIGN_BOTTOM;

{**** Others   ****}

 FL_BROWSER_SLCOL	= FL_COL1;
 FL_BROWSER_LINELENGTH	= 1024;
 FL_BROWSER_FONTSIZE    = FL_SMALL_FONT;

{ as of .86
 FL_SCROLLBAR_OFF = 0;
 FL_SCROLLBAR_ON = 1;
 FL_SCROLLBAR_ALWAYS_ON = 2;
}

{**** Routines ****}

function fl_create_browser(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_add_browser(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6:  pchar) : PFL_OBJECT;
procedure fl_clear_browser(p1 : PFL_OBJECT);
procedure fl_add_browser_line(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_addto_browser(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_addto_browserchars(p1 : PFL_OBJECT; p2 :  pchar);

procedure fl_insert_browser_line(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);
procedure fl_delete_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_replace_browser_line(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);
function fl_get_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar;
function fl_load_browser(p1 : PFL_OBJECT; p2 :  pchar ) : Longint ;

procedure fl_select_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_deselect_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_deselect_browser(p1 : PFL_OBJECT); 
function fl_isselected_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : Longint ;

function fl_get_browser_topline(p1 : PFL_OBJECT ) : Longint ; 
function fl_get_browser(p1 : PFL_OBJECT ) : Longint ;
function fl_get_browser_maxline(p1 : PFL_OBJECT ) : Longint ; 
function fl_get_browser_screenlines(p1 : PFL_OBJECT ) : Longint ;

procedure fl_set_browser_topline(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_specialkey(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_vscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_hscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_leftslider(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_browser_line_selectable(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_get_browser_dimension(p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_COORD);
procedure fl_set_browser_dblclick_callback(p1 : PFL_OBJECT; p2 : PFL_CALLBACKPTR; p3 :longint);
{
#define fl_set_browser_leftscrollbar fl_set_browser_leftslider
}
procedure fl_set_browser_xoffset(p1 : PFL_OBJECT; p2 :  TFL_Coord);
procedure fl_set_browser_scrollbarsize (p1 : PFL_OBJECT;p2,p3 : longint);

type
 TFL_BUTTON_TYPE= Longint;

Const
    FL_NORMAL_BUTTON	= 0;
    FL_PUSH_BUTTON	= 1;
    FL_RADIO_BUTTON	= 2;
    FL_HIDDEN_BUTTON	= 3;
    FL_TOUCH_BUTTON	= 4;
    FL_INOUT_BUTTON	= 5;
    FL_RETURN_BUTTON	= 6;
    FL_HIDDEN_RET_BUTTON= 7;
    FL_MENU_BUTTON	= 8;

type TFL_BUTTON_SPEC = record
    pixmap,mask :  TPixmap;
    bits_w, bits_h : Cardinal;
    val,			{ whether on }
    mousebut,		{ mouse button that caused the push     }
    timdel,			{ time since last touch (p1 : TOUCH buttons) }
    event : Longint;			{ what event triggers redraw            }
    cspecl : longint;		{ reserved for class specfic stuff      }
    cspecv : pointer;		{ misc. things                          }
    filename : pchar;
end;
TFL_BUTTON_STRUCT = TFL_BUTTON_SPEC;
PFL_BUTTON_STRUCT = ^TFL_BUTTON_STRUCT;

TFL_DrawButton = procedure  (p1 : PFL_OBJECT); 
PFL_DrawButton  = ^TFL_DrawButton;
TFL_CleanupButton = procedure (p1 : PFL_BUTTON_STRUCT); 
PFL_CleanupButton= ^TFL_CleanupButton;

Const
FL_BUTTON_BOXTYPE	= FL_UP_BOX;
FL_BUTTON_COL1		= FL_COL1;
FL_BUTTON_COL2		= FL_COL1;
FL_BUTTON_LCOL		= FL_LCOL;
FL_BUTTON_ALIGN		= FL_ALIGN_CENTER;

FL_BUTTON_MCOL1		= FL_MCOL;
FL_BUTTON_MCOL2		= FL_MCOL;

FL_BUTTON_BW		= FL_BOUND_WIDTH;

{
 *  light button defaults
 }
FL_LIGHTBUTTON_BOXTYPE	= FL_UP_BOX;
FL_LIGHTBUTTON_COL1	= FL_COL1;
FL_LIGHTBUTTON_COL2	= FL_YELLOW;
FL_LIGHTBUTTON_LCOL	= FL_LCOL;
FL_LIGHTBUTTON_ALIGN	= FL_ALIGN_CENTER;

{**** Others   ****}

FL_LIGHTBUTTON_TOPCOL	= FL_COL1;
FL_LIGHTBUTTON_MCOL	= FL_MCOL;
FL_LIGHTBUTTON_MINSIZE	= 12;

{* round button defaults **}

  FL_ROUNDBUTTON_BOXTYPE	= FL_NO_BOX;
  FL_ROUNDBUTTON_COL1	= FL_MCOL;
  FL_ROUNDBUTTON_COL2	= FL_YELLOW;
  FL_ROUNDBUTTON_LCOL	= FL_LCOL;
  FL_ROUNDBUTTON_ALIGN	= FL_ALIGN_CENTER;

  FL_ROUNDBUTTON_TOPCOL	= FL_COL1;
  FL_ROUNDBUTTON_MCOL	= FL_MCOL;

{* round3d button defaults **}

  FL_ROUND3DBUTTON_BOXTYPE	= FL_NO_BOX;
  FL_ROUND3DBUTTON3D_COL1	= FL_MCOL;
  FL_ROUND3DBUTTON_COL2	= FL_YELLOW;
  FL_ROUND3DBUTTON_LCOL	= FL_LCOL;
  FL_ROUND3DBUTTON_ALIGN	= FL_ALIGN_CENTER;

  FL_ROUND3DBUTTON_TOPCOL	= FL_COL1;
  FL_ROUND3DBUTTON_MCOL	= FL_MCOL;

{* check button defaults **}

  FL_CHECKBUTTON_BOXTYPE	= FL_NO_BOX;
  FL_CHECKBUTTON_COL1	= FL_COL1;
  FL_CHECKBUTTON_COL2	= FL_YELLOW;
  FL_CHECKBUTTON_LCOL	= FL_LCOL;
  FL_CHECKBUTTON_ALIGN	= FL_ALIGN_CENTER;

  FL_CHECKBUTTON_TOPCOL	= FL_COL1;
  FL_CHECKBUTTON_MCOL	= FL_MCOL;

{* bitmap button defaults *}
  FL_BITMAPBUTTON_BOXTYPE	= FL_UP_BOX;
  FL_BITMAPBUTTON_COL1	= FL_COL1;	{ bitmap background  }
  FL_BITMAPBUTTON_COL2	= FL_BLUE;	{ "focus" color       }
  FL_BITMAPBUTTON_LCOL	= FL_LCOL;	{ bitmap foreground   }
  FL_BITMAPBUTTON_ALIGN	= FL_ALIGN_BOTTOM;

{* bitmap button defaults *}
  FL_PIXMAPBUTTON_BOXTYPE	= FL_UP_BOX;
  FL_PIXMAPBUTTON_COL1	= FL_COL1;	{ box col    }
  FL_PIXMAPBUTTON_COL2	= FL_YELLOW;	{ bound rect }
  FL_PIXMAPBUTTON_LCOL	= FL_LCOL;
  FL_PIXMAPBUTTON_ALIGN	= FL_ALIGN_BOTTOM;

{**** Routines ****}

function fl_create_button(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_roundbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_round3dbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_lightbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_checkbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_bitmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_pixmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 :  pchar) : PFL_OBJECT;

function fl_add_roundbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;
function fl_add_round3dbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;

function fl_add_lightbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;
function fl_add_checkbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;
function fl_add_button(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;


procedure fl_set_bitmapbutton_data (p1 : PFL_OBJECT; p2,p3 : Longint {was int};   p4 : pchar);
procedure fl_set_bitmapbutton_file (p1 : PFL_OBJECT; p2: pchar);
function  fl_add_bitmapbutton (p1 : longint; p2,p3,p4,p5: TFL_Coord;p6 : pchar) : PFL_OBJECT;

{
#define fl_set_bitmapbutton_datafile  fl_set_bitmapbutton_file
}

function fl_add_pixmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
procedure fl_set_pixmapbutton_data(p1 : PFL_OBJECT; p2 :  ppchar);
procedure fl_set_pixmapbutton_file(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_set_pixmapbutton_align(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : Longint {was int});
procedure fl_set_pixmapbutton_pixmap(p1 : PFL_OBJECT; p2 : TPixmap; p3 :TPixmap);
procedure fl_set_pixmapbutton_colorcloseness(p1 : Longint {was int}; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_free_pixmapbutton_pixmap(p1 : PFL_OBJECT); 
function fl_get_pixmapbutton_pixmap(p1 : PFL_OBJECT; p2 : PPixmap; p3 : PPixmap) : TPixmap ;

function fl_get_button(p1 : PFL_OBJECT ) : Longint ;  
procedure fl_set_button(p1 : PFL_OBJECT; p2 :  Longint {was int});
function fl_get_button_numb(p1 : PFL_OBJECT ) : Longint ;

procedure fl_set_button_shortcut(p1 : PFL_OBJECT; p2 : pchar;p3 : Longint {was int});

function fl_create_generic_button(p1,p2 : Longint {was int}; p3,p4,p5,p6 : TFL_Coord;p7 : pchar) : PFL_OBJECT;
procedure fl_add_button_class(p1 : Longint {was int}; p2 : PFL_DRAWBUTTON; p3 : PFL_CLEANUPBUTTON);


{
 * $Id$
 *
 * Header for FL_CANVAS
 *
 }



type TFL_CANVAS_TYPE = Longint;

const 
    FL_NORMAL_CANVAS = 0;
    FL_SCROLLED_CANVAS = 1;

Type
TFL_HANDLE_CANVAS = Procedure (PFL_OBJECT; TWindow; Longint {was int}; Longint {was int};
				 PXEvent; pointer);
PFL_HANDLE_CANVAS = ^TFL_HANDLE_CANVAS;

TFL_MODIFY_CANVAS_PROP = Procedure (p1 : PFL_OBJECT);
PFL_MODIFY_CANVAS_PROP = ^TFL_MODIFY_CANVAS_PROP;

{******************* Default ********************}

Const   FL_CANVAS_BOXTYPE  =  FL_NO_BOX;
        FL_CANVAS_ALIGN    =  FL_ALIGN_TOP;


{*********** Interfaces    ***********************}


function fl_create_generic_canvas(p1 : Longint; p2 : Longint {was int}; p3,p4,p5,p6 : TFL_Coord; p7 : pchar) : PFL_OBJECT;

function fl_add_canvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar): PFL_OBJECT;

function fl_create_canvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;

function fl_create_mesacanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord;p6 :  pchar) : PFL_OBJECT;

function fl_add_mesacanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;



procedure fl_set_canvas_decoration(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_canvas_colormap(p1 : PFL_OBJECT; p2 :  TColormap);
procedure fl_set_canvas_visual(p1 : PFL_OBJECT; p2 :  PVisual);
procedure fl_set_canvas_depth(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_canvas_attributes(p1 : PFL_OBJECT; p2 : word;  p3 : PXSetWindowAttributes);

function fl_add_canvas_handler(p1 : PFL_OBJECT; p2 :  Longint {was int}; p3 : PFL_HANDLE_CANVAS; p4 : pointer) : PFL_HANDLE_CANVAS ;

function fl_get_canvas_id(p1 : PFL_OBJECT ) : TWindow ;
function fl_get_canvas_colormap(p1 : PFL_OBJECT ) : TColormap; 
function fl_get_canvas_depth(p1 : PFL_OBJECT ) : Longint ; 
procedure fl_remove_canvas_handler(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : PFL_HANDLE_CANVAS);
procedure fl_hide_canvas(p1 : PFL_OBJECT); 	{ internal use only }
procedure fl_canvas_yield_to_shortcut(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_modify_canvas_prop(p1 : PFL_OBJECT; 
				  P2 : PFL_MODIFY_CANVAS_PROP;
				  p3 : PFL_MODIFY_CANVAS_PROP;
				  p4 : PFL_MODIFY_CANVAS_PROP);

{ OpenGL canvases }
function fl_create_glcanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;

function fl_add_glcanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;

procedure fl_set_glcanvas_defaults(p1 : pointer); 
procedure fl_get_glcanvas_defaults(p1 : pointer); 
procedure fl_set_glcanvas_attributes(p1 : PFL_OBJECT; p2 : pointer);
procedure fl_get_glcanvas_attributes(p1 : PFL_OBJECT; p2 : pointer);
procedure fl_set_glcanvas_direct(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_activate_glcanvas(p1 : PFL_OBJECT);

function fl_get_glcanvas_xvisualinfo(p1 : PFL_OBJECT ) : PXVisualInfo; 

{
# if defined(p1 : __GLX_glx_h__) || defined(p1 : GLX_H)
function fl_get_glcanvas_context(p1 : PFL_OBJECT ob ) : GLXContext ; 
function fl_glwincreate(p1 : *; p2 : GLXContext *; p3 : Longint {was int}; p4 : Longint {was int} ) : TWindow ;
function fl_glwinopen(p1 : *; p2 : GLXContext *; p3 : Longint {was int}; p4 : Longint {was int} ) : TWindow ;
}



{
 * $Id$
 *
 * Object Class: Chart
 *
 }


const
    FL_BAR_CHART	= 0;
    FL_HORBAR_CHART	= 1;
    FL_LINE_CHART	= 2;
    FL_FILL_CHART	= 3;
    FL_SPIKE_CHART	= 4;
    FL_PIE_CHART	= 5;
    FL_SPECIALPIE_CHART	= 6;
     
Type  TFL_CHART_TYPE = Longint;

{**** Defaults ****}

Const  
  FL_CHART_BOXTYPE	= FL_BORDER_BOX;
  FL_CHART_COL1		= FL_COL1;
  FL_CHART_LCOL		= FL_LCOL;
  FL_CHART_ALIGN	= FL_ALIGN_BOTTOM;

{**** Others   ****}
  FL_CHART_MAX		= 512;

{**** Routines ****}

function fl_create_chart(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 :  pchar) : PFL_OBJECT;
function fl_add_chart(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;

procedure fl_clear_chart(p1 : PFL_OBJECT);
procedure fl_add_chart_value(p1 : PFL_OBJECT; p2 : double; p3 : pchar; p4 : Longint {was int});
procedure fl_insert_chart_value(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : pchar; p5 : Longint {was int});
procedure fl_replace_chart_value(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : pchar; p5 : Longint {was int});
procedure fl_set_chart_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_set_chart_maxnumb(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_chart_autosize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_chart_lstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_chart_lsize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_chart_lcolor(p1 : PFL_OBJECT; p2 :  TFL_COLOR);



{
 * $Id$
 *
 }


const
  FL_NORMAL_CHOICE	= 0;
  FL_NORMAL_CHOICE2     = 1;
  FL_DROPLIST_CHOICE	= 2;

Type
 TFL_CHOICE_TYPE = Longint;

Const
  FL_SIMPLE_CHOICE  	= FL_NORMAL_CHOICE;

{**** Defaults ****}

 FL_CHOICE_BOXTYPE	= FL_ROUNDED_BOX;
 FL_CHOICE_COL1		= FL_COL1;
 FL_CHOICE_COL2		= FL_LCOL;
 FL_CHOICE_LCOL		= FL_LCOL;
 FL_CHOICE_ALIGN	= FL_ALIGN_LEFT;

{**** Others   ****}

 FL_CHOICE_MCOL		= FL_MCOL;
 FL_CHOICE_MAXITEMS	= 63;

{**** Routines ****}

function fl_create_choice(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) :  PFL_OBJECT;

function fl_add_choice(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar ) : PFL_OBJECT;
procedure fl_clear_choice(p1 : PFL_OBJECT);  
procedure fl_addto_choice(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_replace_choice(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);
procedure fl_delete_choice(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_choice(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_choice_text(p1 : PFL_OBJECT; p2 :  pchar);
function fl_get_choice(p1 : PFL_OBJECT ) : Longint ;
function fl_get_choice_item_text(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar;
function fl_get_choice_maxitems(p1 : PFL_OBJECT ) : Longint ; 
function fl_get_choice_text(p1 : PFL_OBJECT ) : pchar;
procedure fl_set_choice_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_choice_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_choice_align(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_choice_item_mode(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : word);
procedure fl_set_choice_item_shortcut(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);



{
 * $Id$
 *
 }


Const
    FL_ANALOG_CLOCK	= 0;
    FL_DIGITAL_CLOCK	= 1;

 FL_CLOCK_BOXTYPE   = FL_UP_BOX;
 FL_CLOCK_COL1      = FL_INACTIVE_COL;
 FL_CLOCK_COL2      = FL_BOTTOM_BCOL;
 FL_CLOCK_LCOL      = FL_BLACK;
 FL_CLOCK_ALIGN     = FL_ALIGN_BOTTOM;

 FL_CLOCK_TOPCOL    = FL_COL1;

function fl_create_clock(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;

function fl_add_clock(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
procedure fl_get_clock(p1 : PFL_OBJECT; p2,p3,p4 : pointer);



{
 * $Id$
 *
 }


Const
    FL_NORMAL_COUNTER = 0;
    FL_SIMPLE_COUNTER = 1;
    
type TFL_COUNTER_TYPE = Longint;

{**** Defaults ****}
Const
 FL_COUNTER_BOXTYPE	= FL_UP_BOX;
 FL_COUNTER_COL1	= FL_COL1;
 FL_COUNTER_COL2	= FL_BLUE;	{ ct label     }
 FL_COUNTER_LCOL	= FL_LCOL;	{ ct reporting }
 FL_COUNTER_ALIGN	= FL_ALIGN_BOTTOM;

{**** Others   ****}

 FL_COUNTER_BW		= FL_BOUND_WIDTH-1;

{**** Routines ****}

function fl_create_counter(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;

function fl_add_counter(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT;

procedure fl_set_counter_value(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_counter_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_set_counter_step(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_set_counter_precision(p1 : PFL_OBJECT; p2 :  Longint {was int});
function fl_get_counter_value(p1 : PFL_OBJECT ) : double ; 
procedure fl_get_counter_bounds(p1 : PFL_OBJECT; p2,p3 : pdouble);
procedure fl_set_counter_return(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_counter_filter(p1 : PFL_OBJECT;p2 : pointer); {!!!!}
{
				  pchar(p1 : *)(p1 : PFL_OBJECT; p2 : double; p3 :Longint {was int}));
}

{
 * $Id$
 *
 * Cursor defs and prototypes
 *
 }


{$i cursorfont.inc}

const FL_DEFAULT_CURSOR = -1;
      FL_INVISIBLE_CURSOR = -2;

procedure fl_set_cursor(p1 : TWindow; p2 :  Longint {was int});
procedure fl_set_cursor_color(p1 : longint; p2 : TFL_COLOR; p3 :TFL_COLOR);
function fl_create_bitmap_cursor(p1 : pchar; p2 :  pchar; p3,p4,p5,p6 : Longint {was int}) : TCursor;
function fl_get_cursor_byname(p1 : longint ) : TCursor; 
{
#define fl_reset_cursor(p1 : win) fl_set_cursor(p1 : win; p2 :  -1);
}



{
 * $Id$
 }


Const
    FL_NORMAL_DIAL = 0;
    FL_LINE_DIAL   = 1;
    FL_FILL_DIAL   = 2;
    
    FL_DIAL_CW  = 0;
    FL_DIAL_CCW = 1;

Type
  TFL_DIAL_TYPE = Longint;

{**** Defaults ****}
Const

 FL_DIAL_BOXTYPE	= FL_FLAT_BOX;
 FL_DIAL_COL1		= FL_COL1;
 FL_DIAL_COL2		= FL_RIGHT_BCOL;
 FL_DIAL_LCOL		= FL_LCOL;
 FL_DIAL_ALIGN		= FL_ALIGN_BOTTOM;

{**** Others   ****}

 FL_DIAL_TOPCOL		= FL_COL1;

{**** Routines ****}

function fl_create_dial(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 :pchar) : PFL_OBJECT;
function fl_add_dial(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT;

procedure fl_set_dial_value(p1 : PFL_OBJECT; p2 :  double);
function fl_get_dial_value(p1 : PFL_OBJECT ) : double ; 
procedure fl_set_dial_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_get_dial_bounds(p1 : PFL_OBJECT; p2 : pdouble; p3 :pdouble);

procedure fl_set_dial_step(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_dial_return(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_dial_angles(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_set_dial_cross(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_dial_direction (p1 : PFL_OBJECT; p2 : Longint);

{
 * $Id$
 *
 *  Convenience functions to read a directory
 }


{  File types }

Const
    FT_FILE  = 0; 
    FT_DIR   = 1; 
    FT_LINK  = 2; 
    FT_SOCK  = 3;
    FT_FIFO  = 4; 
    FT_BLK   = 5;
    FT_CHR   = 6; 
    FT_OTHER = 7;

type TFL_Dirlist = record
    name : pchar;			{ entry name }
    ftype : longint;			{ FILE_TYPE  }
    dl_mtime : longint;
    dl_size : cardinal;
    filler : array[0..2] of longint;
    end;
    PFL_Dirlist = ^TFL_Dirlist;

    
TFL_DIRLIST_FILTER = procedure (p1 : pchar; p2 :  Longint {was int});
PFL_DIRLIST_FILTER = ^TFL_DIRLIST_FILTER;

Const
  FL_ALPHASORT  = 1;
  FL_RALPHASORT = 2;
  FL_MTIMESORT  = 3;
  FL_RMTIMESORT = 4;
  FL_SIZESORT  = 5;
  FL_RSIZESORT = 6;
  

{ read dir with pattern filtering. All dirs read might be cached.
 * must not change dirlist in anyway.
 }
function fl_get_dirlist(p1 : pchar; p2 : pchar;	p3 : pointer;p4	:longint) : PFL_Dirlist;	{ rescan }


function fl_set_dirlist_filter (p1 : PFL_DIRLIST_FILTER ) : PFL_DIRLIST_FILTER ;
procedure fl_set_dirlist_sort ( p1 : longint);

procedure fl_free_dirlist(p1 : PFL_Dirlist); 

{ Free all directory caches }
procedure fl_free_all_dirlist; 

function fl_is_valid_dir(p1 : pchar ) : Longint;
function fl_fmtime(p1 : pchar ) : cardinal ; 
function fl_fix_dirname(p1 : pchar) : pchar; 


{
 * $Id$
 *
 }


{ types of frames }
Const
    FL_NO_FRAME		= 0;
    FL_UP_FRAME		= 1;
    FL_DOWN_FRAME	= 2;
    FL_BORDER_FRAME	= 3;
    FL_SHADOW_FRAME	= 4;
    FL_ENGRAVED_FRAME	= 5;
    FL_ROUNDED_FRAME	= 6;
    FL_EMBOSSED_FRAME	= 7;
    FL_OVAL_FRAME	= 8;

    FL_FRAME_COL1 = FL_BLACK;
    FL_FRAME_COL2 = FL_COL1;
    FL_FRAME_LCOL = FL_BLACK;

function fl_create_frame(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_frame(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;
function fl_create_labelframe(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_labelframe(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;


{
 * $Id$
 *
 *  Object Class: Free
 }

Const
    FL_NORMAL_FREE	= 0;
    FL_INACTIVE_FREE	= 1;
    FL_INPUT_FREE	= 2;
    FL_CONTINUOUS_FREE	= 3;
    FL_ALL_FREE		= 4;
Type
   TFL_FREE_TYPE = Longint;
   PFL_FREE_TYPE = ^TFL_FREE_TYPE;

Const FL_SLEEPING_FREE =  FL_INACTIVE_FREE;


function fl_create_free(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar; p7 : PFL_HANDLEPTR) : PFL_OBJECT;
function fl_add_free(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6 : pchar; P7 : PFL_HANDLEPTR) : PFL_OBJECT;

{
 * $Id$
 }

Const
 FLAlertDismissLabel = 'flAlert.dismiss.label';
 FLQuestionYesLabel  = 'flQuestion.yes.label';
 FLQuestionNoLabel   = 'flQuestion.no.label';
 FLOKLabel           = 'flInput.ok.label';
 FLInputClearLabel   = 'flInput.clear.label';
 FLInputCancelLabel   = 'flInput.cancel.label';

{ from goodies.c }
procedure fl_set_goodies_font(p1 : LongInt; p2 :  Longint {was int});
procedure fl_show_message(p1 : pchar; p2 : pchar; p3 :pchar);
procedure fl_show_messages(p1 : pchar);
procedure fl_show_alert(p1 : pchar; p2 : pchar; p3 : pchar; p4 : Longint {was int});
function fl_show_question(p1 : pchar; p2 : longint) : Longint ;
function fl_show_input(p1 : pchar; p2 :  pchar ) : pchar;
function fl_show_simple_input(p1 : pchar; p2 :  pchar ) : pchar;
function fl_show_colormap(p1 : Longint ) : Longint ;  
function fl_show_choice(p1,p2,p3 : pchar; p4 : Longint {was int}; p5,p6,p7 : pchar; p8 : Longint) : Longint;
function fl_show_choices(p1 : pchar; p4 : Longint {was int}; p5,p6,p7 : pchar; p8 : Longint) : Longint;
procedure fl_set_choices_shortcut(p1,p2,p3 :pchar);

procedure fl_show_oneliner(p1 : pchar; p2 : TFL_Coord; p3 :TFL_Coord);
procedure fl_hide_oneliner; 
procedure fl_set_oneliner_font(p1 : LongInt; p2 :  Longint {was int});
procedure fl_set_oneliner_color(p1 : TFL_COLOR; p2 :  TFL_COLOR);

type TFD_CMDLOG = record
    form : PFL_FORM;
    browser,close_browser,clear_browser : PFL_OBJECT;
end;
PFD_CMDLOG = ^TFD_CMDLOG;

function fl_exe_command(p1 : pchar; p2 : Longint) : Longint;
function fl_end_command(p1 : longint) : Longint;
Function fl_end_all_command : Longint;
procedure fl_show_command_log(p1 : Longint);
procedure fl_hide_command_log;
procedure fl_clear_command_log;
procedure fl_addto_command_log(p1 : pchar);
procedure fl_set_command_log_position(p1,p2 :longint);
Function fl_get_command_log_fdstruct : PFD_CMDLOG;

{ aliases }
{
#define fl_open_command    fl_exe_command
#define fl_close_command   fl_end_command
}

{****** from file selector ****************}

Const  FL_MAX_FSELECTOR =  6;

type TFD_FSELECTOR = record
    fselect : PFL_FORM;
    browser, theinput, prompt, resbutt,
    patbutt,dirbutt, cancel, ready : PFL_OBJECT;
    dirlabel,patlabel : PFL_OBJECT;
    appbut : array[0..2] of PFL_OBJECT;
end;

PFD_FSELECTOR = ^TFD_FSELECTOR;

function fl_use_fselector(p1 : LongInt ) : LongInt; 
function fl_show_fselector(p1,p2,p3,p4 :  pchar) : pchar;

procedure fl_set_fselector_fontsize (p1 : Longint);
procedure fl_set_fselector_fontstyle (p1 : longint);
procedure fl_set_fselector_placement(p1 : LongInt); 
procedure fl_set_fselector_border(p1 : LongInt); 

{
#define fl_set_fselector_transient(p1 : b)   \
                     fl_set_fselector_border(p1 : (p1 : b)?FL_TRANSIENT:FL_FULLBORDER)
}

Type TFSelector_Callback = Function (P1 : Pchar; P2 : Pointer) : Longint; 
     PFSelector_Callback = ^TFSelector_Callback;
     TFL_Procedure = Procedure;
     PFL_Procedure = ^TFL_Procedure;

procedure fl_set_fselector_callback( p1 : PFSelector_Callback; p2 : pointer);
function fl_get_filename : pchar;
function fl_get_directory : pchar; 
function fl_get_pattern : pchar;  
function fl_set_directory (p1 : pchar ) : LongInt; 
procedure fl_set_pattern (p1 : pchar);
procedure fl_refresh_fselector; 
procedure fl_add_fselector_appbutton(p1 : pchar; p2 : PFL_Procedure; p3 : pointer);
procedure fl_remove_fselector_appbutton(p1 : pchar);
procedure fl_disable_fselector_cache(p1 : LongInt); 
procedure fl_invalidate_fselector_cache; 
function fl_get_fselector_form : PFL_FORM; 
function fl_get_fselector_fdstruct  : PFD_FSELECTOR; 
procedure fl_hide_fselector; 


procedure fl_set_fselector_filetype_marker(p1,p2,p3,p4,p5 : Longint);

{
#define fl_show_file_selector     fl_show_fselector
#define fl_set_fselector_cb       fl_set_fselector_callback
#define fl_set_fselector_title(p1 : s) fl_set_form_title(p1 : fl_get_fselector_form(p1 : ); p2 : s)
}



{
 * $Id$
 *
 }


{**** Types    ****}

Const 
    FL_NORMAL_INPUT	= 0;
    FL_FLOAT_INPUT	= 1;
    FL_INT_INPUT	= 2;
    FL_DATE_INPUT	= 3;
    FL_MULTILINE_INPUT	= 4;
    FL_HIDDEN_INPUT	= 5;
    FL_SECRET_INPUT	= 6;
    
    FL_INPUT_MMDD       = 0;
    FL_INPUT_DDMM	= 1;
    
Type TFL_INPUT_TYPE = Longint;

{**** Defaults ****}

Const
   FL_INPUT_BOXTYPE		= FL_DOWN_BOX;
   FL_INPUT_COL1		= FL_COL1;
   FL_INPUT_COL2		= FL_MCOL;
   FL_INPUT_LCOL		= FL_LCOL;
   FL_INPUT_ALIGN		= FL_ALIGN_LEFT;

{**** Others   ****}

   FL_INPUT_TCOL		= FL_LCOL;
   FL_INPUT_CCOL		= FL_BLUE;

   cFL_RINGBELL             = (1 shl 4);

{**** Routines ****}

function fl_create_input(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6: pchar) : PFL_OBJECT;

function fl_add_input(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar ) : PFL_OBJECT;

procedure fl_set_input(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_set_input_color(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_set_input_return(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_scroll(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_cursorpos(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_set_input_selected(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_selected_range(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_set_input_maxchars(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_format(p1 : PFL_OBJECT; p2,p3 :  Longint {was int});
procedure fl_set_input_hscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_vscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_xoffset(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_topline(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_input_scrollbarsize(p1 : PFL_OBJECT; p2,p3 :  Longint {was int});

function fl_get_input(p1 : PFL_OBJECT ) : pchar;  
function fl_get_input_cursorpos(p1 : PFL_OBJECT; p2,p3 : Pointer ) : LongInt;
function fl_get_input_topline(p1 : PFL_OBJECT;p2,p3 : PLongint) : Longint;
function fl_get_input_screenlines(p1 : PFL_OBJECT) : Longint;
function fl_get_input_numberoflines(p1 : PFL_OBJECT) : Longint;
procedure fl_get_input_format(p1 : PFL_OBJECT;p2,p3 : PLongint);



type
 TFL_INPUTVALIDATOR = Procedure (p1 : PFL_OBJECT; p2,p3 : pchar; p4 : Longint {was int});
 PFL_INPUTVALIDATOR= ^TFL_INPUTVALIDATOR;

function fl_set_input_filter(p1 : PFL_OBJECT; p2 :  PFL_INPUTVALIDATOR ) : PFL_INPUTVALIDATOR ;

{
#define fl_set_input_shortcut fl_set_object_shortcut
#define ringbell(p1 : )  XBell(p1 : fl_display; p2 :  0)
}

type TFL_EditKeymap = record
    { basic editing }
    del_prev_char : Longint;		{ delete previous char    }
    del_next_char : Longint;		{ delete next char        }
    del_prev_word : Longint;		{ delete previous word    }
    del_next_word : Longint;		{ delete next word        }

    { movement }
    moveto_prev_line : Longint;	{ one line  up             }
    moveto_next_line : Longint;	{ one line down            }
    moveto_prev_char : Longint;	{ one char left            }
    moveto_next_char : Longint;	{ one char right           }
    moveto_prev_word : Longint;	{ one word left            }
    moveto_next_word : Longint;	{ one word right           }
    moveto_prev_page : Longint;	{ one page up              }
    moveto_next_page : Longint;	{ one page down            }
    moveto_bol : Longint;		{ move to begining of line }
    moveto_eol : Longint;		{ move to end of line      }
    moveto_bof : Longint;		{ move to begin of file    }
    moveto_eof : Longint;		{ move to end of file      }

    { misc. stuff }
    transpose : Longint;		{ switch two char positions }
    paste : Longint;			{ paste the edit buffer    }
    backspace : Longint;		{ another  del_prev_char   }
    del_to_bol : Longint;		{ cut to begining of line  }
    del_to_eol : Longint;		{ cut to end of line       }
    clear_field : Longint;		{ delete everything        }
    del_to_eos : Longint;		{ not implemented          }
    reserverd : array[0..3] of Longint;		{ fillter                  }
end;
PFL_EditKeymap = ^TFL_EditKeymap;

procedure fl_set_input_editkeymap(PFL_EditKeymap);




{
 * $Id$
 *
 }


{***********   Object Class: Menu         ***********}

Const
    FL_TOUCH_MENU	= 0;
    FL_PUSH_MENU	= 1;
    FL_PULLDOWN_MENU	= 2;
    
Type TFL_MENU_TYPE = Longint;

{**** Defaults ****}

Const
  FL_MENU_BOXTYPE	= FL_BORDER_BOX;
  FL_MENU_COL1		= FL_COL1;
  FL_MENU_COL2		= FL_MCOL;
  FL_MENU_LCOL		= FL_LCOL;
  FL_MENU_ALIGN		= FL_ALIGN_CENTER;

{**** Others   ****}

  FL_MENU_MAXITEMS	= 128;
  FL_MENU_MAXSTR	= 64;

{**** Routines ****}

function fl_create_menu(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar) : PFL_OBJECT;

function fl_add_menu(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar) : PFL_OBJECT;

procedure fl_clear_menu(p1 : PFL_OBJECT); 
procedure fl_set_menu(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_addto_menu(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_replace_menu_item(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);
procedure fl_delete_menu_item(p1 : PFL_OBJECT; p2 :  Longint {was int});

procedure fl_set_menu_item_shortcut(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);
procedure fl_set_menu_item_mode(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Cardinal);
procedure fl_show_menu_symbol(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_menu_popup(p1 : PFL_OBJECT; p2 :  Longint {was int});

function fl_get_menu(p1 : PFL_OBJECT ) : Longint ; 
function fl_get_menu_item_text(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar;
function fl_get_menu_maxitems(p1 : PFL_OBJECT ) : Longint ; 
function fl_get_menu_item_mode(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : Cardinal ;
function fl_get_menu_text(p1 : PFL_OBJECT ) : pchar;



{
 *  $Id$
 *
 }
{
 * $Id$
 *
 *  Prototypes for pop-up menus
 }


Const
 FL_MAXPUPI   = 64;	{ max item each pup        }
 FL_PUP_PADH  =  4;	{ space between each items }

type
 TFL_PUP_CB = Procedure  (p1 : Longint);  	{ call back prototype  }
 PFL_PUP_CB = ^TFL_PUP_CB;

function fl_newpup(p1 : TWindow ) : LongInt; 

function fl_defpup(p1 : TWindow; p2 :  pchar) : LongInt;
function fl_addtopup(p1 : longint; p2 :  pchar) : LongInt;

function fl_setpup_mode(p1 : Longint; p2 : Longint {was int}; p3 : Cardinal) : Longint;
procedure fl_freepup(p1 : Longint); 
function fl_dopup(p1 : Longint ) : LongInt; 

procedure fl_setpup_shortcut(p1 : Longint; p2 : Longint {was int}; p3 :pchar);
procedure fl_setpup_position(p1 : Longint; p2 :  Longint {was int});
procedure fl_setpup_selection(p1 : Longint; p2 :  Longint {was int});
function fl_setpup_fontsize(p1 : LongInt) : Longint; 
function fl_setpup_fontstyle(p1 : LongInt) : Longint;  
procedure fl_setpup_shadow(p1 : LongInt; p2 :  Longint {was LongInt});
procedure fl_setpup_softedge(p1 : LongInt; p2 :  Longint {was int});
procedure fl_setpup_color(p1 : TFL_COLOR; p2 :  TFL_COLOR);
procedure fl_setpup_checkcolor(p1 : TFL_COLOR); 
procedure fl_setpup_title(p1 : LongInt; p2 :  pchar);
procedure fl_setpup_bw(p1 : LongInt; p2 :  Longint {was int});
procedure fl_setpup_pad(p1 : LongInt; p2 : Longint {was int}; p3 :Longint {was int});
function fl_setpup_cursor(p1 : LongInt; p2 :  Longint {was int} ) : TCursor ;
function fl_setpup_default_cursor(p1 : LongInt ) : TCursor ; 
function fl_setpup_maxpup(p1 : LongInt ) : LongInt; 
function fl_getpup_mode(p1 : LongInt; p2 :  Longint {was int} ) : Cardinal;
function fl_getpup_text(p1 : LongInt; p2 :  Longint {was int} ) : pchar;
procedure fl_showpup(p1 : LongInt); 
procedure fl_hidepup(p1 : LongInt);

{
#define fl_setpup_hotkey    fl_setpup_shortcut
}

function fl_setpup_itemcb(p1 : LongInt; p2 : Longint {was int}; p3 :PFL_PUP_CB ) : PFL_PUP_CB ;
function fl_setpup_menucb(p1 : LongInt; p2 :  PFL_PUP_CB ) : PFL_PUP_CB ;
procedure fl_setpup_submenu(p1 : LongInt; p2 : Longint {was int}; p3 :Longint {was int});

{
#define fl_setpup    fl_setpup_mode
}



{
 * $Id$
 *
 }

Const
 FL_NORMAL_POSITIONER	= 0;

{**** Defaults ****}

 FL_POSITIONER_BOXTYPE	= FL_DOWN_BOX;
 FL_POSITIONER_COL1	= FL_COL1;
 FL_POSITIONER_COL2	= FL_RED;
 FL_POSITIONER_LCOL	= FL_LCOL;
 FL_POSITIONER_ALIGN	= FL_ALIGN_BOTTOM;

{**** Others   ****}


{**** Routines ****}

function fl_create_positioner(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_positioner(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;

procedure fl_set_positioner_xvalue(p1 : PFL_OBJECT; p2 :  double);
function fl_get_positioner_xvalue(p1 : PFL_OBJECT ) : double ;
procedure fl_set_positioner_xbounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_get_positioner_xbounds(p1 : PFL_OBJECT; p2 : pdouble ; p3 : pdouble);
procedure fl_set_positioner_yvalue(p1 : PFL_OBJECT; p2 :  double);
function fl_get_positioner_yvalue(p1 : PFL_OBJECT ) : double ;
procedure fl_set_positioner_ybounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_get_positioner_ybounds(p1 : PFL_OBJECT; p2 ,p3 :pdouble);
procedure fl_set_positioner_xstep(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_positioner_ystep(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_positioner_return(p1 : PFL_OBJECT; p2 :  Longint {was int});


{
 * $Id$
 *
 * Object Class: Slider
 *
 }


Const
    FL_VERT_SLIDER	= 0;
    FL_HOR_SLIDER	= 1;
    FL_VERT_FILL_SLIDER	= 2;
    FL_HOR_FILL_SLIDER	= 3;
    FL_VERT_NICE_SLIDER	= 4;
    FL_HOR_NICE_SLIDER	= 5;
    FL_HOR_BROWSER_SLIDER = 6;
    FL_VERT_BROWSER_SLIDER = 7;
    FL_HOR_BROWSER_SLIDER2	= 8;	{ for Longint {was int}ernal use only }
    FL_VERT_BROWSER_SLIDER2	= 9;	{ for Longint {was int}ernal use only }

Type
 TFL_SLIDER_TYPE = Longint;
 PFL_SLIDER_TYPE = ^TFL_SLIDER_TYPE; 


{**** Defaults ****}
Const
  FL_SLIDER_BW1         = FL_BOUND_WIDTH;
  FL_SLIDER_BW2         = FL_BOUND_WIDTH-1;

  FL_SLIDER_BOXTYPE	= FL_DOWN_BOX;
  FL_SLIDER_COL1	= FL_COL1;
  FL_SLIDER_COL2	= FL_COL1;
  FL_SLIDER_LCOL	= FL_LCOL;
  FL_SLIDER_ALIGN	= FL_ALIGN_BOTTOM;

{**** Others   ****}

  FL_SLIDER_FINE		= 0.05;
  FL_SLIDER_WIDTH		= 0.10;


{**** Routines ****}

function fl_create_slider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT;
function fl_add_slider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT;

function fl_create_valslider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar): PFL_OBJECT;
function fl_add_valslider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;P6: pchar) : PFL_OBJECT;

procedure fl_set_slider_value(p1 : PFL_OBJECT; p2 :  double);
function fl_get_slider_value(p1 : PFL_OBJECT ) : double ; 
procedure fl_set_slider_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_get_slider_bounds(p1 : PFL_OBJECT; p2,p3 : pdouble);

procedure fl_set_slider_return(p1 : PFL_OBJECT; p2 :  Longint {was int});

procedure fl_set_slider_step(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_slider_increment(p1 : PFL_OBJECT; p2,p3 :  double);
procedure fl_set_slider_size(p1 : PFL_OBJECT; p2 :  double);
procedure fl_set_slider_precision(p1 : PFL_OBJECT; p2 :  Longint {was int});

Type
  TFL_SLIDER_FILTER = procedure (p1 : PFL_OBJECT; p2 : double; p3 :Longint {was int});
  PTFL_SLIDER_FILTER= ^TFL_SLIDER_FILTER  ;
  
procedure fl_set_slider_filter(p1 : PFL_OBJECT; p2 : PTFL_SLIDER_FILTER);


{
 * $Id$
 *
 }

Const
    FL_NORMAL_TEXT = 0;

 FL_TEXT_BOXTYPE    = FL_FLAT_BOX;
 FL_TEXT_COL1       = FL_COL1;
 FL_TEXT_COL2       = FL_MCOL;
 FL_TEXT_LCOL       = FL_LCOL;
 FL_TEXT_ALIGN      = FL_ALIGN_LEFT;

function fl_create_text(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;
function fl_add_text(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar): PFL_OBJECT;



{
 * $Id$
 *
 *  Object Class: Timer
 *
 }


Const 
    FL_NORMAL_TIMER = 0;
    FL_VALUE_TIMER  = 1;
    FL_HIDDEN_TIMER = 2;
    
Type TFL_TIMER_TYPE = Longint;

{**** Defaults ****}
Const

FL_TIMER_BOXTYPE	= FL_DOWN_BOX;
FL_TIMER_COL1		= FL_COL1;
FL_TIMER_COL2		= FL_RED;
FL_TIMER_LCOL		= FL_LCOL;
FL_TIMER_ALIGN		= FL_ALIGN_CENTER;

{**** Others   ****}

  FL_TIMER_BLINKRATE	= 0.2;
  
Type TFL_TIMER_FILTER = Function (P1 : PFL_OBJECT;p2 : double) : pchar;
     PFL_TIMER_FILTER = ^TFL_TIMER_FILTER;  

{**** Routines ****}

function fl_create_timer(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;

function fl_add_timer(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT;

procedure fl_set_timer(p1 : PFL_OBJECT; p2 :  double);
function fl_get_timer(p1 : PFL_OBJECT ) : double ; 
procedure fl_set_timer_countup(p1 : PFL_OBJECT; p2 :  Longint);
function fl_set_timer_filter (p1 : PFL_OBJECT; p2 : PFL_TIMER_FILTER) : PFL_TIMER_FILTER;
procedure fl_suspend_timer(p1 : PFL_OBJECT);
procedure fl_resume_timer(p1 : PFL_OBJECT);

{
 * $Id$
 *
 }


{
 * Class FL_XYPLOT
 }

Const
    FL_NORMAL_XYPLOT	= 0;		{ solid line                        }
    FL_SQUARE_XYPLOT	= 1;		{ with added square                 }
    FL_CIRCLE_XYPLOT	= 2;		{ with added circle                 }
    FL_FILL_XYPLOT	= 3;		{ fill completely                   }
    FL_POINTS_XYPLOT	= 4;		{ only data points                  }
    FL_DASHED_XYPLOT	= 5;		{ dashed line                       }
    FL_IMPULSE_XYPLOT	= 6;
    FL_ACTIVE_XYPLOT	= 7;		{ accepts Longint {was int}eractive manipulations }
    FL_EMPTY_XYPLOT	= 8;
Type 
    TFL_XYPLOT_TYPE = Longint;

Const
    FL_LINEAR = 0;
    FL_LOG    = 1;

Const
    FL_GRID_NONE  = 0;
    FL_GRID_MAJOR = 1;
    FL_GRID_MINOR = 2;
 
{**** Defaults ****}

 FL_XYPLOT_BOXTYPE       = FL_FLAT_BOX;
 FL_XYPLOT_COL1          = FL_COL1;
 FL_XYPLOT_LCOL          = FL_LCOL;
 FL_XYPLOT_ALIGN         = FL_ALIGN_BOTTOM;
 FL_MAX_XYPLOTOVERLAY    = 32;

{**** Others   ****}

function fl_create_xyplot(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6: pchar) : PFL_OBJECT;
function fl_add_xyplot(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar ) : PFL_OBJECT;
procedure fl_set_xyplot_data(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : Longint {was int}; p5,p6,p7 : pchar);
procedure fl_set_xyplot_file(p1 : PFL_OBJECT; p2,p3,p4,p5 :pchar);

procedure fl_set_xyplot_return(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_xyplot_xtics(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_set_xyplot_ytics(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});
procedure fl_set_xyplot_xbounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
procedure fl_set_xyplot_ybounds(p1 : PFL_OBJECT; p2 : double; p3 :double);
function  fl_set_xyplot_maxoverlays(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : Longint ;
procedure fl_set_xyplot_overlay_type(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int});
procedure fl_set_xyplot_interpolate(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : double);
procedure fl_set_xyplot_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_xyplot_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_xyplot_inspect(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_xyplot_symbolsize(p1 : PFL_OBJECT; p2 :  Longint {was int});
procedure fl_set_xyplot_xscale(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :double);
procedure fl_set_xyplot_yscale(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :double);

procedure fl_set_xyplot_linewidth(p1 : PFL_OBJECT; p2,p3 : Longint);
procedure fl_set_xyplot_xgrid(p1 : PFL_OBJECT; p2 : Longint);
procedure fl_set_xyplot_ygrid(p1 : PFL_OBJECT; p2 : Longint);
procedure fl_set_xyplot_alphaxtics(p1 : PFL_OBJECT; p2,p3 : pchar);
procedure fl_set_xyplot_alphaytics(p1 : PFL_OBJECT; p2,p3 : pchar);
procedure fl_set_xyplot_fixed_xaxis(p1 : PFL_OBJECT; p2,p3 : pchar);
procedure fl_set_xyplot_fixed_yaxis(p1 : PFL_OBJECT; p2,p3 : pchar);


procedure fl_get_xyplot_xbounds(p1 : PFL_OBJECT; p2,p3 : pfloat );
procedure fl_get_xyplot_ybounds(p1 : PFL_OBJECT; p2,p3 : pfloat );
procedure fl_get_xyplot(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : pointer);
procedure fl_get_xyplot_data(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : pointer);
procedure fl_get_xyplot_xmapping(p1 : PFL_OBJECT; p2,p3 : pfloat);
procedure fl_get_xyplot_ymapping(p1 : PFL_OBJECT; p2,p3 : pfloat);

{
#define fl_set_xyplot_datafile fl_set_xyplot_file
}
procedure fl_add_xyplot_text(p1 : PFL_OBJECT; p2,p3 : double; p4 : pchar;p5 : Longint {was int}; p6 :TFL_COLOR);
procedure fl_add_xyplot_overlay(p1 : PFL_OBJECT; p2 : Longint {was int}; p3,p4 : pfloat; p5 : Longint {was int}; p6 :TFL_COLOR);
procedure fl_delete_xyplot_text(p1 : PFL_OBJECT; p2 :  pchar);
procedure fl_delete_xyplot_overlay(p1 : PFL_OBJECT; p2 :  Longint {was int});

procedure fl_replace_xyplot_point(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : double);
function  fl_interpolate(p1,p2 : pfloat; p3 :Longint {was int};p4,p5 : pfloat; p6 : double; p7 : Longint {was int}) : Longint;
procedure fl_xyplot_s2w(p1 : PFL_OBJECT; p2 : double; p3 : double; p4,p5 : pfloat);
procedure fl_xyplot_w2s(p1 : PFL_OBJECT; p2 : double; p3 : double; p4,p5 : pfloat);

implementation

procedure fl_add_io_callback(p1 : Longint {was int} ; p2 : word; p3 : PFL_IO_CALLBACK; p4 : pointer); [ C ];
procedure fl_remove_io_callback(p1 : Longint {was int};p2 :  word; p3 : PFL_IO_CALLBACK); [ C ];
procedure fl_add_signal_callback(p1 : Longint;p2 : PFL_SIGNAL_HANDLER;p3 : pointer); [ C ];
procedure fl_remove_signal_callback(p1 : Longint {was int}); [ C ];
procedure fl_signal_caught(p1 :Longint {was int}); [ C ];
procedure fl_app_signal_direct(p1 : Longint {was int}); [ C ];
function fl_add_timeout (p1 : longint;p2 : PFL_SIGNAL_HANDLER;p3 : pointer) : longint;[ C ];
procedure fl_remove_timeout(p1 : Longint);[ C ];

function fl_get_vn_value (p1 : PFL_VN_PAIR; p2 : pchar) : Longint; [ C ];
function fl_get_vn_name (p1 : PFL_VN_PAIR; p2 : Longint {was int}) : Pchar; [ C ];
function fl_msleep (p1 : cardinal) : cardinal; [ C ];
procedure fl_library_version(p1,p2 : pointer ); [ C ];
function fl_bgn_form(P1 : Longint {was int}; P2,p3 : TFL_Coord) : PFL_Form; [ C ];
Procedure fl_end_form; [ C ];
function fl_do_forms : PFL_OBJECT ; [ C ];
function fl_check_forms : PFL_OBJECT; [ C ];
Function fl_do_only_forms : PFL_OBJECT; [ C ];
function fl_check_only_forms : PFL_OBJECT; [ C ];
procedure fl_freeze_form(P1 : PFL_FORM); [ C ];
procedure fl_set_focus_object( P1 : PFL_FORM;p2 : PFL_OBJECT); [ C ];
procedure fl_reset_focus_object( p1 : PFL_OBJECT);[ C ];

Function fl_set_form_atclose(p1 : PFL_FORM; p2 : PFL_FORM_ATCLOSE; p3 : pointer) : PFL_FORM_ATCLOSE ; [ C ];
Function fl_set_atclose(p1 : PFL_FORM_ATCLOSE; p2:  pointer) : PFL_FORM_ATCLOSE ; [ C ];
Function fl_set_form_atactivate(p1 : PFL_FORM; P2 : PFL_FORM_ATACTIVATE; P3 : pointer) : PFL_FORM_ATACTIVATE ; [ C ];
Function fl_set_form_atdeactivate(p1 : PFL_FORM;p2 : PFL_FORM_ATDEACTIVATE;p3 : pointer) : PFL_FORM_ATDEACTIVATE ; [ C ];
Procedure fl_unfreeze_form (p1 : PFL_FORM); [ C ];
Procedure fl_deactivate_form(p1 : PFL_FORM); [ C ];
Procedure fl_activate_form(p1 : PFL_FORM); [ C ];
Procedure fl_deactivate_all_forms; [ C ];
Procedure fl_activate_all_forms; [ C ];
Procedure fl_freeze_all_forms; [ C ];
Procedure fl_unfreeze_all_forms; [ C ];
Procedure fl_scale_form(p1 : PFL_FORM;p2,p3 : double); [ C ];
Procedure fl_set_form_position(p1 : PFL_FORM; p2,p3 : TFL_Coord); [ C ];
Procedure fl_set_form_title(p1 : PFL_FORM; p2 : pchar); [ C ];
procedure fl_set_form_property(p1 : PFL_FORM; p2 : word); [ C ];
procedure fl_set_app_mainform( p1 : PFL_FORM); [ C ];
function fl_get_app_mainform : PFL_FORM; [ C ];
procedure fl_set_app_nomainform(P1 : Longint {was int}); [ C ];
procedure fl_set_form_callback(p1 : PFL_FORM; P2 : PFL_FORMCALLBACKPTR; P3 : pointer); [ C ];
procedure fl_set_form_size(p1 : PFL_FORM; p2,p3 : TFL_Coord); [ C ];
procedure fl_set_form_hotspot(p1 : PFL_FORM; p2,p3 : TFL_Coord); [ C ];
procedure fl_set_form_hotobject(p1 : PFL_FORM; p2 : PFL_OBJECT); [ C ];
procedure fl_set_form_minsize(p1 : PFL_FORM ; p2,p3 :TFL_Coord); [ C ];
procedure fl_set_form_maxsize(p1 : PFL_FORM; p2,p3 :TFL_Coord); [ C ];
procedure fl_set_form_event_cmask(p1 : PFL_FORM; p2 : cardinal); [ C ];
function fl_get_form_event_cmask(p1 : PFL_FORM) : cardinal; [ C ];
procedure fl_set_form_geometry(p1 : PFL_FORM; p2,p3,p4,p5 : TFL_Coord); [ C ];
function fl_show_form (p1 : PFL_FORM; p2,p3 : Longint {was int}; p4 : pchar) : longint; [ C ];
procedure fl_hide_form(p1 : PFL_FORM); [ C ];
procedure fl_free_form(p1 : PFL_FORM); [ C ];
procedure fl_redraw_form(p1 : PFL_FORM); [ C ];
procedure fl_set_form_dblbuffer(p1 : PFL_FORM; p2 : Longint {was int}); [ C ];
procedure fl_prepare_form_window(p1 : PFL_FORM; p2,p3 :  Longint {was int}; p4 : pchar); [ C ];
procedure fl_show_form_window(p1 : PFL_FORM); [ C ];
function fl_adjust_form_size(p1 : PFL_FORM) : double; [ C ];
function fl_form_is_visibe (p1 : PFL_FORM) : Longint; [ C ];

function  fl_register_raw_callback(p1 : PFL_FORM; p2 : cardinal; P3 : PFL_RAW_CALLBACK): PFL_RAW_CALLBACK; [ C ];
function fl_bgn_group : PFL_OBJECT; [ C ];
function fl_end_group : PFL_OBJECT; [ C ];
procedure fl_addto_group (p1 : PFL_OBJECT); [ C ];
procedure fl_set_object_boxtype(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_bw(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_resize(p1 : PFL_OBJECT; p2 : word); [ C ];
procedure fl_set_object_gravity(p1 : PFL_OBJECT; p2,p3 : word ); [ C ];
procedure fl_set_object_lsize(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_lstyle(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_lcol(p1 : PFL_OBJECT; p2 : TFL_COLOR); [ C ];
procedure fl_set_object_return(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_lalign(p1 : PFL_OBJECT; p2 : Longint {was int}); [ C ];
procedure fl_set_object_shortcut(p1 : PFL_OBJECT; p2 : pchar;p3 : Longint {was int}); [ C ];
procedure fl_set_object_shortcutkey(p1 : PFL_OBJECT; p2 :  word); [ C ];
procedure fl_set_object_dblbuffer(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_object_color(p1 : PFL_OBJECT; p2 : TFL_COLOR;p3 : TFL_COLOR); [ C ];
procedure fl_set_object_label(p1 : PFL_OBJECT; p2 :  pchar); [ C ];
procedure fl_set_object_position(p1 : PFL_OBJECT; p2,p3 :TFL_Coord); [ C ];
procedure fl_set_object_size(p1 : PFL_OBJECT; p2,p3 :TFL_Coord); [ C ];
procedure fl_set_object_automatic(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_draw_object_label(p1 : PFL_OBJECT); [ C ];
procedure fl_draw_object_label_outside(p1 : PFL_OBJECT); [ C ];

 procedure fl_set_object_geometry(p1 : PFL_OBJECT; p2,p3,p4,p5 :TFL_Coord); [ C ];
procedure fl_fit_object_label(p1 : PFL_OBJECT; p2,p3 :TFL_Coord); [ C ];
procedure fl_get_object_geometry(p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_Coord ); [ C ];
procedure fl_get_object_position(p1 : PFL_OBJECT; p2,p3 :PFL_COORD); [ C ];
procedure fl_get_object_bbox(p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_COORD); [ C ];
procedure fl_call_object_callback(p1 : PFL_OBJECT); [ C ];
function fl_set_object_prehandler(p1 : PFL_OBJECT; p2 : PFL_HANDLEPTR) : PFL_HANDLEPTR ; [ C ];
function fl_set_object_posthandler(p1 : PFL_OBJECT; p2 : PFL_HANDLEPTR): PFL_HANDLEPTR ; [ C ];
function fl_set_object_callback(p1 : PFL_OBJECT; p2 : PFL_CALLBACKPTR; p3 :longint) : PFL_CALLBACKPTR ; [ C ];
procedure fl_redraw_object(p1 : PFL_OBJECT); [ C ];
procedure fl_scale_object(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_show_object(p1 : PFL_OBJECT); [ C ];
procedure fl_hide_object(p1 : PFL_OBJECT); [ C ];
procedure fl_free_object(p1 : PFL_OBJECT); [ C ];
procedure fl_delete_object(p1 : PFL_OBJECT); [ C ];
procedure fl_trigger_object(p1 : PFL_OBJECT); [ C ];
procedure fl_activate_object(p1 : PFL_OBJECT); [ C ];
procedure fl_deactivate_object(p1 : PFL_OBJECT); [ C ];
function fl_set_font_name(p1 : Longint {was int}; p2 :  pchar) : longint; [ C ];
procedure fl_set_font(p1 : Longint {was int}; p2 :  Longint {was int}); [ C ];
function fl_get_char_height(p1,p2 : Longint {was int}; p3,p4 : pointer) : Longint {was int}; [ C ];
function fl_get_char_width(p1,p2 :  Longint {was int}) : Longint; [ C ];
function fl_get_string_height(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}; p5,p6 : pointer) : Longint {was int}; [ C ];
function fl_get_string_width(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}) : Longint {was int}; [ C ];
function fl_get_string_widthTAB(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}) : Longint {was int}; [ C ];
function fl_get_string_dimension(p1 : Longint {was int}; p2 : Longint {was int}; p3 : pchar; p4 : Longint {was int}; p5,p6 : pointer) : Longint {was int}; [ C ];
procedure fl_get_align_xy(p1,p2,p3,p4,p5,p6,p7,p8,p9 : Longint {was int}; p10,p11 : pointer); [ C ];
procedure fl_drw_text(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7,p8 : Longint {was int}; p9 : pchar); [ C ];
procedure fl_drw_text_beside(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7,p8 : Longint {was int}; p9 : pchar); [ C ];
procedure fl_drw_text_cursor(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6,p7,p8: Longint {was int}; p9 : Pchar; p10,p11: Longint {was int}); [ C ];
procedure fl_drw_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR; p7 : Longint {was int}); [ C ];
function fl_add_symbol(p1 : pchar; p2 : PFL_DRAWPTR; p3 :Longint {was int}) : Longint {was int}; [ C ];
function fl_draw_symbol(p1 : pchar; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR) : Longint {was int}; [ C ];
procedure fl_drw_slider (p1 :longint; p2,p3,P4,p5 : TFL_COORD; p6,p7 : TFL_COLOR;
                         p8 : Longint; p9,p10 : double; P11 : pchar; p12,p13,p14 : Longint); [ C ];

function fl_mapcolor(p1 : TFL_COLOR; p2,p3,p4 : Longint {was int}) : cardinal; [ C ];
function fl_mapcolorname(p1 : TFL_COLOR; p2 :  pchar) : longint; [ C ];
function fl_getmcolor(p1 : TFL_COLOR; p2,p3,p4 : pointer) : cardinal; [ C ];
procedure fl_free_colors(p1 : PFL_COLOR; p2 :  Longint {was int}); [ C ];
procedure fl_free_pixels(p1 : pcardinal; p2 :  Longint {was int}); [ C ];
procedure fl_set_color_leak(p1 : Longint {was int}); [ C ];
function fl_get_pixel(p1 : TFL_COLOR): cardinal; [ C ];
procedure fl_get_icm_color(p1 : TFL_COLOR; p2,p3,p4 : pointer); [ C ];
procedure fl_set_icm_color(p1 : TFL_COLOR; p2,p3,p4 : Longint {was int}); [ C ];
procedure fl_color(p1 : TFL_COLOR); [ C ];
procedure fl_bk_color(p1 : TFL_COLOR); [ C ];
procedure fl_textcolor(p1 : TFL_COLOR); [ C ];
procedure fl_bk_textcolor(p1 : TFL_COLOR); [ C ];
procedure fl_set_gamma(p1,p2,p3 :double); [ C ];
procedure fl_show_errors(p1 : Longint {was int}); [ C ];
procedure fl_add_object(p1 : PFL_FORM; p2 : PFL_OBJECT); [ C ];
procedure fl_addto_form(p1 : PFL_FORM); [ C ];
function fl_make_object(p1 : Longint {was int}; p2,p3,p4,p5,p6 : TFL_Coord; p7 : pchar; P8 : PFL_HANDLEPTR) : PFL_OBJECT; [ C ];
procedure fl_set_coordunit(p1 : Longint {was int}); [ C ];
function fl_get_coordunit : Longint {was int}; [ C ];
procedure fl_set_border_width(p1 : Longint {was int}); [ C ];
function fl_get_border_width : Longint {was int}; [ C ];
procedure fl_flip_yorigin; [ C ];
procedure fl_ringbell ( p1 : longint);[ C ];
procedure fl_gettime (p1,p2 : Plongint);[ C ];
function fl_mouse_button : longint; [ C ];

procedure fl_rectangle(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR); [ C ];
procedure fl_rectbound(p1,p2,p3,p4 : TFL_Coord;p5 : TFL_COLOR); [ C ];
procedure fl_roundrectangle(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR); [ C ];
procedure fl_polygon(p1 : Longint {was int}; p2 : PFL_POINT; p3 : Longint {was int}; p4 : TFL_COLOR); [ C ];
procedure fl_lines(p1 : TFL_POINT; p2 : Longint {was int}; p3 :TFL_COLOR); [ C ];
procedure fl_line(p1,p2,p3,p4 : TFL_Coord; p5 : TFL_COLOR); [ C ];
procedure fl_dashedlinestyle(p1 : pchar; p2 :  Longint {was int}); [ C ];
procedure fl_drawmode(p1 : Longint {was int}); [ C ];
procedure fl_linewidth(p1 : Longint {was int}); [ C ];
procedure fl_linestyle(p1 : Longint {was int}); [ C ];
function fl_get_linewidth : longint; [ C ];
function fl_get_linestyle : longint; [ C ];

procedure fl_oval(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : TFL_COLOR); [ C ];
procedure fl_ovalbound(p1,p2,p3,p4 : TFL_Coord; p5 : TFL_COLOR); [ C ];
procedure fl_ovalarc(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6,p7 : Longint; p8 : TFL_COLOR);[ C ];

procedure fl_pieslice(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6,p7 :Longint {was int}; p8 : TFL_COLOR); [ C ];
procedure fl_add_vertex(p1,p2 :  TFL_Coord); [ C ];
procedure fl_add_float_vertex(p1,p2 :  real); [ C ];
procedure fl_reset_vertex; [ C ];
procedure fl_endline; [ C ];
procedure fl_endpolygon; [ C ];
procedure fl_endclosedline; [ C ];
procedure fl_drw_frame(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR; p7 : Longint {was int}); [ C ];
procedure fl_drw_checkbox(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : TFL_COLOR;p7 : Longint {was int}); [ C ];
function fl_get_fontstruct(p1,p2 :  Longint {was int}) : PXFontStruct; [ C ];
function fl_get_mouse(p1,p2 : PFL_COORD; p3 : pword) : TWindow; [ C ];
procedure fl_set_mouse(p1,p2 :  TFL_Coord); [ C ];
function fl_get_win_mouse(p1 : TWindow; p2,p3 : PFL_COORD; p4 : pword) : TWindow; [ C ];
function fl_get_form_mouse(p1 : PFL_FORM; p2,p3 : PFL_COORD; p4 : pWord) : TWindow; [ C ];
function fl_win_to_form(p1 : TWindow) : PFL_form; [ C ];
 procedure fl_set_form_icon(p1 : PFL_FORM; p2 : TPixmap; p3 :TPixmap); [ C ];
function fl_wincreate(p1 : pchar) : TWindow; [ C ];
 function fl_winshow(p1 : TWindow) : TWindow; [ C ];
 function fl_winopen(p1 : pchar) : TWindow; [ C ];
 procedure fl_winhide(p1 : TWindow); [ C ];
 procedure fl_winclose(p1 : TWindow); [ C ];
 procedure fl_winset(p1 : TWindow); [ C ];
 function fl_winget : Twindow; [ C ];
 procedure fl_winresize(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_winmove(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_winreshape(p1 : TWindow; p2,p3,p4,p5 : TFL_Coord); [ C ];
procedure fl_winicon(p1 : TWindow; p2 : TPixmap; p3 :TPixmap); [ C ];
procedure fl_winbackground(p1 : TWindow; p2 :  cardinal); [ C ];
procedure fl_winstepunit(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_winisvalid(p1 : TWindow); [ C ];
 procedure fl_wintitle(p1 : TWindow; p2 :  pchar); [ C ];
procedure fl_winposition(p1,p2 :  TFL_Coord); [ C ];
procedure fl_winminsize(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_winmaxsize(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_winaspect(p1 : TWindow; p2,p3 :TFL_Coord); [ C ];
procedure fl_reset_winconstraints(p1 : TWindow); [ C ];
 procedure fl_winsize(p1,p2 :  TFL_Coord); [ C ];
procedure fl_initial_winsize(p1,p2 :  TFL_Coord); [ C ];
procedure fl_initial_winstate(p1 : Longint {was int}); [ C ];
 function fl_create_colormap(p1 : PXVisualInfo; p2 :  Longint {was int}) : TColormap; [ C ];
procedure fl_wingeometry(p1,p2,p3,p4 : TFL_Coord); [ C ];
procedure fl_initial_wingeometry(p1,p2,p3,p4 : TFL_Coord); [ C ];
procedure fl_get_winsize(p1 : TWindow; p2,p3 :PFL_COORD); [ C ];
procedure fl_get_winorigin(p1 : TWindow; p2,p3 :PFL_COORD); [ C ];
procedure fl_get_wingeometry(p1 : TWindow; p2,p3,p4,p5 : PFL_COORD); [ C ];
function fl_XNextEvent(p1 : PXEvent) : Longint; [ C ];
function fl_XPeekEvent(p1 : PXEvent) : Longint; [ C ];
function fl_XEventsQueued(p1 : Longint {was int}) : Longint; [ C ];
procedure fl_XPutBackEvent(p1 : PXEvent); [ C ];
function fl_last_event : PXEvent ; [ C ];
function fl_set_event_callback(p1 : PFL_APPEVENT_CB; p2 : pointer) : PFL_APPEVENT_CB ; [ C ];
function fl_set_idle_callback(p1 : PFL_APPEVENT_CB; p2 :  pointer) : PFL_APPEVENT_CB ; [ C ];
function fl_addto_selected_xevent(p1 : TWindow; p2 :  longint) : Cardinal; [ C ];
function fl_remove_selected_xevent(p1 : TWindow; p2 :  longint) : cardinal; [ C ];
function fl_add_event_callback(p1 : TWindow; p2 :  Longint {was int}; p3 : PFL_APPEVENT_CB; p4 : pointer) : PFL_APPEVENT_CB ; [ C ];
procedure fl_remove_event_callback(p1 : TWindow; p2 :  Longint {was int}); [ C ];
procedure fl_activate_event_callbacks(p1 : TWindow); [ C ];
function fl_print_xevent_name(p1 : pchar; p2 : PXEvent) : PXEvent; [ C ];
function fl_initialize(p1 : pointer; p2 : ppchar; p3 : pchar; p4 : PFL_CMD_OPT; p5 : Longint {was int}) : Pdisplay; [ C ];
procedure fl_finish; [ C ];

function fl_get_resource(p1 : pchar; p2 :  pchar; p3 : TFL_RTYPE; p4 : pchar; p5 : pointer; p6 :Longint {was int}) : pchar;[ C ];
procedure fl_set_resource(p1 : pchar; p2 :  pchar);[ C ];
procedure fl_get_app_resources(p1 : PFL_resource; p2 : Longint {was int});[ C ];
procedure fl_set_graphics_mode(p1 : Longint {was int}; p2 :  Longint {was int});[ C ];
procedure fl_set_visualID(p1 : longint);  [ C ];
function fl_keysym_pressed(p1 : TKeySym) : Longint; [ C ];
procedure fl_set_defaults(p1 : cardinal; p2 :  PFL_IOPT);[ C ];
procedure fl_set_tabstop(p1 : pchar); [ C ];
procedure fl_get_defaults(p1 : PFL_IOPT); [ C ];
function fl_get_visual_depth : Longint; [ C ];
function fl_vclass_name(p1 : Longint {was int}) : pchar; [ C ];
function fl_vclass_val(p1 : pchar) : Longint {was int}; [ C ];
procedure fl_set_ul_property(p1 : Longint {was int}; p2 :  Longint {was int});[ C ];
procedure fl_set_clipping(p1 : TFL_Coord; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord);[ C ];
procedure fl_set_gc_clipping(p1 : TGC; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord);[ C ];
procedure fl_unset_gc_clipping(p1 : TGC); [ C ];
procedure fl_set_clippings(p1 : PXRectangle; p2 :  Longint {was int});[ C ];
procedure fl_unset_clipping; [ C ];
function fl_textgc : TGC;[ C ];
function fl_create_bitmap(p1 : Longint {was int}; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;[ C ];
function fl_add_bitmap(p1 : Longint {was int}; p2 : TFL_Coord; p3 : TFL_Coord; p4 : TFL_Coord; p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;[ C ];
procedure fl_set_bitmap_data(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : pointer);[ C ];
procedure fl_set_bitmap_file(p1 : PFL_OBJECT; p2 :  pchar);[ C ];
function fl_read_bitmapfile(p1 : TWindow; p2 :  pchar; p3,p4 : pword; p5,p6 : pointer) : TPixmap ;[ C ];
function fl_create_pixmap(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6  :pchar) : PFL_OBJECT;[ C ];
function fl_add_pixmap(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
procedure fl_set_pixmap_data(p1 : PFL_OBJECT; p2 :  ppchar);[ C ];
procedure fl_set_pixmap_file(p1 : PFL_OBJECT; p2 :  pchar);[ C ];
procedure fl_set_pixmap_align(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : Longint {was int});[ C ];
procedure fl_set_pixmap_pixmap(p1 : PFL_OBJECT; p2 : TPixmap; p3 :TPixmap);[ C ];
procedure fl_set_pixmap_colorcloseness(p1 : Longint {was int}; p2 : Longint {was int}; p3 :Longint {was int});[ C ];
procedure fl_free_pixmap_pixmap(p1 : PFL_OBJECT); [ C ];
function fl_get_pixmap_pixmap(p1 : PFL_OBJECT; p2 : PPixmap; p3 : PPixmap) : TPixmap ;[ C ];
function fl_read_pixmapfile(p1 : TWindow; p2 :  pchar;p3,p4 : pword; p5 : PPixmap;p6,p7 : pointer; p8 : TFL_COLOR)  : TPixmap ;[ C ];
function fl_create_from_pixmapdata(p1 : TWindow; p2 :  ppchar;p3,p4 : pword; p5 : PPixmap;p6,p7 : pointer; p8 : TFL_COLOR) : TPixmap ;[ C ];
function fl_create_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;[ C ];
function fl_add_box(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_browser(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_add_browser(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6:  pchar) : PFL_OBJECT;[ C ];
procedure fl_clear_browser(p1 : PFL_OBJECT);[ C ];
procedure fl_add_browser_line(p1 : PFL_OBJECT; p2 :  pchar);[ C ];
procedure fl_addto_browser(p1 : PFL_OBJECT; p2 :  pchar);[ C ];
procedure fl_addto_browserchars(p1 : PFL_OBJECT; p2 :  pchar);[ C ];

procedure fl_insert_browser_line(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);[ C ];
procedure fl_delete_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_replace_browser_line(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar);[ C ];
function fl_get_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar;[ C ];
function fl_load_browser(p1 : PFL_OBJECT; p2 :  pchar ) : Longint ;[ C ];
procedure fl_select_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_deselect_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_deselect_browser(p1 : PFL_OBJECT); [ C ];
function fl_isselected_browser_line(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : Longint ;[ C ];
function fl_get_browser_topline(p1 : PFL_OBJECT ) : Longint ; [ C ];
function fl_get_browser(p1 : PFL_OBJECT ) : Longint ;[ C ];
function fl_get_browser_maxline(p1 : PFL_OBJECT ) : Longint ; [ C ];
function fl_get_browser_screenlines(p1 : PFL_OBJECT ) : Longint ;[ C ];
procedure fl_set_browser_topline(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_specialkey(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_vscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_hscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];

procedure fl_set_browser_leftslider(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_browser_line_selectable(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int});[ C ];
procedure fl_get_browser_dimension(p1 : PFL_OBJECT; p2,p3,p4,p5 : PFL_COORD);[ C ];
procedure fl_set_browser_dblclick_callback(p1 : PFL_OBJECT; p2 : PFL_CALLBACKPTR; p3 :longint);[ C ];
procedure fl_set_browser_xoffset(p1 : PFL_OBJECT; p2 :  TFL_Coord);[ C ];
procedure fl_set_browser_scrollbarsize (p1 : PFL_OBJECT;p2,p3 : longint);[ C ];


function fl_create_button(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_roundbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_round3dbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];

function fl_create_lightbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_checkbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_bitmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];
function fl_create_pixmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 :  pchar) : PFL_OBJECT;[ C ];
function fl_add_roundbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;[ C ];
function fl_add_round3dbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;[ C ];
function fl_add_lightbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;[ C ];
function fl_add_checkbutton(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;[ C ];
function fl_add_button(p1 : Longint {was int}; p2,p3,p4,p5 :TFL_Coord; p6:  pchar) : PFL_OBJECT;[ C ];
procedure fl_set_bitmapbutton_data (p1 : PFL_OBJECT; p2,p3 : Longint {was int};   p4 : pchar);[ C ];
procedure fl_set_bitmapbutton_file (p1 : PFL_OBJECT; p2: pchar);[ C ];
function  fl_add_bitmapbutton (p1 : longint; p2,p3,p4,p5: TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];

function fl_add_pixmapbutton(p1 : Longint {was int}; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ];

function fl_get_button(p1 : PFL_OBJECT ) : Longint ;  [ C ];
procedure fl_set_button(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
function fl_get_button_numb(p1 : PFL_OBJECT ) : Longint ;[ C ];
function fl_create_generic_button(p1,p2 : Longint {was int}; p3,p4,p5,p6 : TFL_Coord;p7 : pchar) : PFL_OBJECT;[ C ];
procedure fl_add_button_class(p1 : Longint {was int}; p2 : PFL_DRAWBUTTON; p3 : PFL_CLEANUPBUTTON);[ C ];

function fl_create_generic_canvas(p1 : Longint; p2 : Longint {was int}; p3,p4,p5,p6 : TFL_Coord; p7 : pchar) : PFL_OBJECT;[ C ] ;
function fl_add_canvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar): PFL_OBJECT;[ C ] ;
function fl_create_canvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;[ C ] ;
function fl_create_mesacanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord;p6 :  pchar) : PFL_OBJECT;[ C ] ;
function fl_add_mesacanvas(p1 : Longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ] ;
procedure fl_set_canvas_decoration(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ] ;
procedure fl_set_canvas_colormap(p1 : PFL_OBJECT; p2 :  TColormap);[ C ] ;
procedure fl_set_canvas_visual(p1 : PFL_OBJECT; p2 :  PVisual);[ C ] ;
procedure fl_set_canvas_depth(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ] ;
procedure fl_set_canvas_attributes(p1 : PFL_OBJECT; p2 : word;  p3 : PXSetWindowAttributes);[ C ] ;
function fl_add_canvas_handler(p1 : PFL_OBJECT; p2 :  Longint {was int}; p3 : PFL_HANDLE_CANVAS; p4 : pointer) : PFL_HANDLE_CANVAS ;[ C ] ;
function fl_get_canvas_id(p1 : PFL_OBJECT ) : TWindow ;[ C ] ;
function fl_get_canvas_colormap(p1 : PFL_OBJECT ) : TColormap;[ C ] ; 
function fl_get_canvas_depth(p1 : PFL_OBJECT ) : Longint ; [ C ] ;
procedure fl_remove_canvas_handler(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : PFL_HANDLE_CANVAS);[ C ] ;
procedure fl_hide_canvas(p1 : PFL_OBJECT); 	{ internal use only }[ C ] ;
procedure fl_canvas_yield_to_shortcut(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ] ;
procedure fl_modify_canvas_prop(p1 : PFL_OBJECT; 
				  P2 : PFL_MODIFY_CANVAS_PROP;
				  p3 : PFL_MODIFY_CANVAS_PROP;
				  p4 : PFL_MODIFY_CANVAS_PROP);[ C ] ;
function fl_create_glcanvas(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT;[ C ] ;
function fl_add_glcanvas(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT;[ C ] ;
procedure fl_set_glcanvas_defaults(p1 : pointer); [ C ] ;
procedure fl_get_glcanvas_defaults(p1 : pointer); [ C ] ;
procedure fl_set_glcanvas_attributes(p1 : PFL_OBJECT; p2 : pointer);[ C ] ;
procedure fl_get_glcanvas_attributes(p1 : PFL_OBJECT; p2 : pointer);[ C ] ;
procedure fl_set_glcanvas_direct(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ] ;
procedure fl_activate_glcanvas(p1 : PFL_OBJECT);[ C ];

function fl_get_glcanvas_xvisualinfo(p1 : PFL_OBJECT ) : PXVisualInfo; [ C ] ;

function fl_create_chart(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 :  pchar) : PFL_OBJECT; [ C ];
function fl_add_chart(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
procedure fl_clear_chart(p1 : PFL_OBJECT); [ C ];
procedure fl_add_chart_value(p1 : PFL_OBJECT; p2 : double; p3 : pchar; p4 : Longint {was int}); [ C ];
procedure fl_insert_chart_value(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : pchar; p5 : Longint {was int}); [ C ];
procedure fl_replace_chart_value(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : pchar; p5 : Longint {was int}); [ C ];
procedure fl_set_chart_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_set_chart_maxnumb(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_chart_autosize(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_chart_lstyle(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_chart_lsize(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_chart_lcolor(p1 : PFL_OBJECT; p2 :  TFL_COLOR);[ C ];


function fl_create_choice(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) :  PFL_OBJECT; [ C ];
function fl_add_choice(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar ) : PFL_OBJECT; [ C ];
procedure fl_clear_choice(p1 : PFL_OBJECT); [ C ];
procedure fl_addto_choice(p1 : PFL_OBJECT; p2 :  pchar); [ C ];
procedure fl_replace_choice(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar); [ C ];
procedure fl_delete_choice(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_choice(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_choice_text(p1 : PFL_OBJECT; p2 :  pchar); [ C ];
function fl_get_choice(p1 : PFL_OBJECT ) : Longint ; [ C ];
function fl_get_choice_item_text(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar; [ C ];
function fl_get_choice_maxitems(p1 : PFL_OBJECT ) : Longint ; [ C ];
function fl_get_choice_text(p1 : PFL_OBJECT ) : pchar; [ C ];
procedure fl_set_choice_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_choice_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_choice_align(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_choice_item_mode(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : word); [ C ];
procedure fl_set_choice_item_shortcut(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar); [ C ];


function fl_create_clock(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_clock(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
procedure fl_get_clock(p1 : PFL_OBJECT; p2,p3,p4 : pointer); [ C ];

function fl_create_counter(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_counter(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT; [ C ];
procedure fl_set_counter_value(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_counter_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_set_counter_step(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_set_counter_precision(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
function fl_get_counter_value(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_get_counter_bounds(p1 : PFL_OBJECT; p2,p3 : pdouble);[ C ];

procedure fl_set_counter_return(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_counter_filter(p1 : PFL_OBJECT;p2 : pointer); [ C ];

procedure fl_set_cursor(p1 : TWindow; p2 :  Longint {was int}); [ C ];
procedure fl_set_cursor_color(p1 : longint; p2 : TFL_COLOR; p3 :TFL_COLOR); [ C ];
function fl_create_bitmap_cursor(p1 : pchar; p2 :  pchar; p3,p4,p5,p6 : Longint {was int}) : TCursor; [ C ];
function fl_get_cursor_byname(p1 : longint ) : TCursor; [ C ];

function fl_create_dial(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 :pchar) : PFL_OBJECT; [ C ];
function fl_add_dial(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT; [ C ];
procedure fl_set_dial_value(p1 : PFL_OBJECT; p2 :  double); [ C ];
function fl_get_dial_value(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_set_dial_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_get_dial_bounds(p1 : PFL_OBJECT; p2 : pdouble; p3 :pdouble); [ C ];
procedure fl_set_dial_step(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_dial_return(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_dial_angles(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_set_dial_cross(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_dial_direction (p1 : PFL_OBJECT; p2 : Longint);[ C ];

function fl_get_dirlist(p1 : pchar; p2 : pchar;	p3 : pointer;p4	:longint) : PFL_Dirlist; [ C ];
function fl_set_dirlist_filter (p1 : PFL_DIRLIST_FILTER ) : PFL_DIRLIST_FILTER ; [ C ];
procedure fl_set_dirlist_sort ( p1 : longint);[ C ];

procedure fl_free_dirlist(p1 : PFL_Dirlist); [ C ];
procedure fl_free_all_dirlist; [ C ];

function fl_is_valid_dir(p1 : pchar ) : Longint; [ C ];
function fl_fmtime(p1 : pchar ) : cardinal ; [ C ];
function fl_fix_dirname(p1 : pchar) : pchar; [ C ];

function fl_create_frame(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_frame(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT; [ C ];
function fl_create_labelframe(p1 : longint; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_labelframe(p1 : longint; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT; [ C ];

function fl_create_free(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar; p7 : PFL_HANDLEPTR) : PFL_OBJECT; [ C ];
function fl_add_free(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6 : pchar; P7 : PFL_HANDLEPTR) : PFL_OBJECT; [ C ];

procedure fl_set_goodies_font(p1 : LongInt; p2 :  Longint {was int}); [ C ];
procedure fl_show_message(p1 : pchar; p2 : pchar; p3 :pchar); [ C ];
procedure fl_show_messages(p1 : pchar);[ C ];

procedure fl_show_alert(p1 : pchar; p2 : pchar; p3 : pchar; p4 : Longint {was int}); [ C ];
function fl_show_question(p1 : pchar; p2 : longint) : Longint ; [ C ];
function fl_show_input(p1 : pchar; p2 :  pchar ) : pchar; [ C ];
function fl_show_simple_input(p1 : pchar; p2 :  pchar ) : pchar; [ C ];
function fl_show_colormap(p1 : Longint ) : Longint ; [ C ];
function fl_show_choice(p1,p2,p3 : pchar; p4 : Longint {was int}; p5,p6,p7 : pchar; p8 : Longint) : Longint;[ C ];
function fl_show_choices(p1 : pchar; p4 : Longint {was int}; p5,p6,p7 : pchar; p8 : Longint) : Longint;[ C ];
procedure fl_set_choices_shortcut(p1,p2,p3 :pchar); [ C ];

procedure fl_show_oneliner(p1 : pchar; p2 : TFL_Coord; p3 :TFL_Coord); [ C ];
procedure fl_hide_oneliner; [ C ];
procedure fl_set_oneliner_font(p1 : LongInt; p2 :  Longint {was int}); [ C ];
procedure fl_set_oneliner_color(p1 : TFL_COLOR; p2 :  TFL_COLOR); [ C ];

function fl_exe_command(p1 : pchar; p2 : Longint) : Longint; [ C ];
function fl_end_command(p1 : longint) : Longint; [ C ];
Function fl_end_all_command : Longint; [ C ];
procedure fl_show_command_log(p1 : Longint); [ C ];
procedure fl_hide_command_log; [ C ];
procedure fl_clear_command_log; [ C ];
procedure fl_addto_command_log(p1 : pchar);[ C ];
procedure fl_set_command_log_position(p1,p2 :longint);[ C ];
Function fl_get_command_log_fdstruct : PFD_CMDLOG; [ C ];

function fl_use_fselector(p1 : LongInt ) : LongInt; [ C ];
function fl_show_fselector(p1,p2,p3,p4 :  pchar) : pchar; [ C ];
procedure fl_set_fselector_fontsize (p1 : Longint); [ C ];
procedure fl_set_fselector_fontstyle (p1 : longint); [ C ];

procedure fl_set_fselector_placement(p1 : LongInt); [ C ];
procedure fl_set_fselector_border(p1 : LongInt); [ C ];
procedure fl_set_fselector_callback( p1 : PFSelector_Callback; p2 : pointer); [ C ];
function fl_get_filename : pchar; [ C ];
function fl_get_directory : pchar; [ C ];
function fl_get_pattern : pchar; [ C ];
function fl_set_directory (p1 : pchar ) : LongInt; [ C ];
procedure fl_set_pattern (p1 : pchar); [ C ];
procedure fl_refresh_fselector; [ C ];
procedure fl_add_fselector_appbutton(p1 : pchar; p2 : PFL_Procedure; p3 : pointer); [ C ];
procedure fl_remove_fselector_appbutton(p1 : pchar); [ C ];
procedure fl_disable_fselector_cache(p1 : LongInt); [ C ];
procedure fl_invalidate_fselector_cache; [ C ]; 
function fl_get_fselector_form : PFL_FORM; [ C ];
function fl_get_fselector_fdstruct  : PFD_FSELECTOR; [ C ]; 
procedure fl_hide_fselector;  [ C ];
procedure fl_set_fselector_filetype_marker(p1,p2,p3,p4,p5 : Longint); [ C ];

function fl_create_input(p1 : Longint; p2,p3,p4,p5 : TFL_Coord; p6: pchar)  : PFL_OBJECT ; [ C ];
function fl_add_input(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar )  : PFL_OBJECT ; [ C ];
procedure fl_set_input(p1 : PFL_OBJECT; p2 :  pchar)  ; [ C ];
procedure fl_set_input_color(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int})  ; [ C ];
function fl_get_input(p1 : PFL_OBJECT )  : pchar ; [ C ];
procedure fl_set_input_return(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_input_scroll(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_input_cursorpos(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int})  ; [ C ];
function fl_get_input_cursorpos(p1 : PFL_OBJECT; p2,p3 : Pointer )  : LongInt ; [ C ];
procedure fl_set_input_selected(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_input_selected_range(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int})  ; [ C ];
procedure fl_set_input_maxchars(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_input_format(p1 : PFL_OBJECT; p2,p3 :  Longint {was int});[ C ];
procedure fl_set_input_hscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_input_vscrollbar(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_input_xoffset(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_input_topline(p1 : PFL_OBJECT; p2 :  Longint {was int});[ C ];
procedure fl_set_input_scrollbarsize(p1 : PFL_OBJECT; p2,p3 :  Longint {was int});[ C ];
function fl_get_input_topline(p1 : PFL_OBJECT;p2,p3 : PLongint) : Longint;[ C ];
function fl_get_input_screenlines(p1 : PFL_OBJECT) : Longint;[ C ];
function fl_get_input_numberoflines(p1 : PFL_OBJECT) : Longint;[ C ];
procedure fl_get_input_format(p1 : PFL_OBJECT;p2,p3 : PLongint);[ C ];
function fl_set_input_filter(p1 : PFL_OBJECT; p2 :  PFL_INPUTVALIDATOR )  : PFL_INPUTVALIDATOR  ; [ C ];
procedure fl_set_input_editkeymap(PFL_EditKeymap);[ C ];

function fl_create_menu(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar) : PFL_OBJECT; [ C ] ;
function fl_add_menu(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar) : PFL_OBJECT; [ C ] ;
procedure fl_clear_menu(p1 : PFL_OBJECT); [ C ] ;
procedure fl_set_menu(p1 : PFL_OBJECT; p2 :  pchar); [ C ] ;
procedure fl_addto_menu(p1 : PFL_OBJECT; p2 :  pchar); [ C ] ;
procedure fl_replace_menu_item(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar); [ C ] ;
procedure fl_delete_menu_item(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ] ;
procedure fl_set_menu_item_shortcut(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :pchar); [ C ] ;
procedure fl_set_menu_item_mode(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Cardinal); [ C ] ;
procedure fl_show_menu_symbol(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ] ;
procedure fl_set_menu_popup(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ] ;
function fl_get_menu(p1 : PFL_OBJECT ) : Longint ; [ C ] ;
function fl_get_menu_item_text(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : pchar; [ C ] ;
function fl_get_menu_maxitems(p1 : PFL_OBJECT ) : Longint ; [ C ] ;
function fl_get_menu_item_mode(p1 : PFL_OBJECT; p2 :  Longint {was int} ) : Cardinal ; [ C ] ;
function fl_get_menu_text(p1 : PFL_OBJECT ) : pchar; [ C ] ;

function fl_newpup(p1 : TWindow ) : LongInt; [ C ];
function fl_defpup(p1 : TWindow; p2 :  pchar) : LongInt; [ C ] ;
function fl_addtopup(p1 : longint; p2 :  pchar) : LongInt; [ C ] ;

function fl_setpup_mode(p1 : Longint; p2 : Longint {was int}; p3 : Cardinal) : Longint; [ C ];
procedure fl_freepup(p1 : Longint); [ C ];
function fl_dopup(p1 : Longint ) : LongInt; [ C ];
procedure fl_setpup_shortcut(p1 : Longint; p2 : Longint {was int}; p3 :pchar); [ C ];
procedure fl_setpup_position(p1 : Longint; p2 :  Longint {was int}); [ C ];
procedure fl_setpup_selection(p1 : Longint; p2 :  Longint {was int}); [ C ];
function fl_setpup_fontsize(p1 : LongInt): Longint; [ C ];
function fl_setpup_fontstyle(p1 : LongInt): longint; [ C ];
procedure fl_setpup_shadow(p1 : LongInt; p2 :  Longint {was LongInt}); [ C ];
procedure fl_setpup_softedge(p1 : LongInt; p2 :  Longint {was int}); [ C ];
procedure fl_setpup_color(p1 : TFL_COLOR; p2 :  TFL_COLOR); [ C ];
procedure fl_setpup_checkcolor(p1 : TFL_COLOR); [ C ];
procedure fl_setpup_title(p1 : LongInt; p2 :  pchar); [ C ];
procedure fl_setpup_bw(p1 : LongInt; p2 :  Longint {was int}); [ C ];
procedure fl_setpup_pad(p1 : LongInt; p2 : Longint {was int}; p3 :Longint {was int}); [ C ];
function fl_setpup_cursor(p1 : LongInt; p2 :  Longint {was int} ) : TCursor ; [ C ];
function fl_setpup_default_cursor(p1 : LongInt ) : TCursor ; [ C ];
function fl_setpup_maxpup(p1 : LongInt ) : LongInt; [ C ];
function fl_getpup_mode(p1 : LongInt; p2 :  Longint {was int} ) : Cardinal; [ C ];
function fl_getpup_text(p1 : LongInt; p2 :  Longint {was int} ) : pchar; [ C ];
procedure fl_showpup(p1 : LongInt); [ C ];
procedure fl_hidepup(p1 : LongInt); [ C ];
function fl_setpup_itemcb(p1 : LongInt; p2 : Longint {was int}; p3 :PFL_PUP_CB ) : PFL_PUP_CB ; [ C ];
function fl_setpup_menucb(p1 : LongInt; p2 :  PFL_PUP_CB ) : PFL_PUP_CB ; [ C ];
procedure fl_setpup_submenu(p1 : LongInt; p2 : Longint {was int}; p3 :Longint {was int}); [ C ];

function fl_create_positioner(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_positioner(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
procedure fl_set_positioner_xvalue(p1 : PFL_OBJECT; p2 :  double); [ C ];
function fl_get_positioner_xvalue(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_set_positioner_xbounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_get_positioner_xbounds(p1 : PFL_OBJECT; p2 : pdouble ; p3 : pdouble); [ C ];
procedure fl_set_positioner_yvalue(p1 : PFL_OBJECT; p2 :  double); [ C ];
function fl_get_positioner_yvalue(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_set_positioner_ybounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_get_positioner_ybounds(p1 : PFL_OBJECT; p2 ,p3 :pdouble); [ C ];
procedure fl_set_positioner_xstep(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_positioner_ystep(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_positioner_return(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];

function fl_create_slider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT; [ C ];
function fl_add_slider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT; [ C ];
function fl_create_valslider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar): PFL_OBJECT; [ C ];
function fl_add_valslider(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;P6: pchar) : PFL_OBJECT; [ C ];
procedure fl_set_slider_value(p1 : PFL_OBJECT; p2 :  double); [ C ];
function fl_get_slider_value(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_set_slider_bounds(p1 : PFL_OBJECT; p2 : double; p3 :double); [ C ];
procedure fl_get_slider_bounds(p1 : PFL_OBJECT; p2,p3 : pdouble); [ C ];
procedure fl_set_slider_return(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_slider_step(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_slider_increment(p1 : PFL_OBJECT; p2,p3 :  double);[ C ];
procedure fl_set_slider_size(p1 : PFL_OBJECT; p2 :  double); [ C ];
procedure fl_set_slider_precision(p1 : PFL_OBJECT; p2 :  Longint {was int}); [ C ];
procedure fl_set_slider_filter(p1 : PFL_OBJECT; p2 : PTFL_SLIDER_FILTER); [ C ];

function fl_create_text(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_text(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6 : pchar): PFL_OBJECT; [ C ];

function fl_create_timer(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6 : pchar) : PFL_OBJECT; [ C ];
function fl_add_timer(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord;p6: pchar) : PFL_OBJECT; [C ];
procedure fl_set_timer(p1 : PFL_OBJECT; p2 :  double); [ C ];
function fl_get_timer(p1 : PFL_OBJECT ) : double ; [ C ];
procedure fl_set_timer_countup(p1 : PFL_OBJECT; p2 :  Longint); [ C ];
function fl_set_timer_filter (p1 : PFL_OBJECT; p2 : PFL_TIMER_FILTER) : PFL_TIMER_FILTER; [ C ];
procedure fl_suspend_timer(p1 : PFL_OBJECT); [ C ];
procedure fl_resume_timer(p1 : PFL_OBJECT); [ C ];


procedure fl_set_xyplot_return(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_xyplot_xtics(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int})  ; [ C ];
procedure fl_set_xyplot_ytics(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :Longint {was int})  ; [ C ];
procedure fl_set_xyplot_xbounds(p1 : PFL_OBJECT; p2 : double; p3 :double)  ; [ C ];
procedure fl_set_xyplot_ybounds(p1 : PFL_OBJECT; p2 : double; p3 :double)  ; [ C ];
procedure fl_get_xyplot_xbounds(p1 : PFL_OBJECT; p2,p3 : pfloat )  ; [ C ];
procedure fl_get_xyplot_ybounds(p1 : PFL_OBJECT; p2,p3 : pfloat )  ; [ C ];
procedure fl_get_xyplot(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : pointer)  ; [ C ];
procedure fl_get_xyplot_data(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : pointer)  ; [ C ];

function fl_create_xyplot(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; p6: pchar)  : PFL_OBJECT ; [ C ];

function fl_add_xyplot(p1 : LongInt; p2,p3,p4,p5 : TFL_Coord; P6 : pchar )  : PFL_OBJECT ; [ C ];

procedure fl_set_xyplot_data(p1 : PFL_OBJECT; p2,p3 : pfloat; p4 : Longint {was int}; p5,p6,p7 : pchar)  ; [ C ];
procedure fl_set_xyplot_file(p1 : PFL_OBJECT; p2,p3,p4,p5 :pchar)  ; [ C ];

{
#define fl_set_xyplot_datafile fl_set_xyplot_file
}
procedure fl_add_xyplot_text(p1 : PFL_OBJECT; p2,p3 : double; p4 : pchar;p5 : Longint {was int}; p6 :TFL_COLOR)  ; [ C ];

procedure fl_delete_xyplot_text(p1 : PFL_OBJECT; p2 :  pchar)  ; [ C ];

function fl_set_xyplot_maxoverlays(p1 : PFL_OBJECT; p2 :  Longint {was int} )  : Longint  ; [ C ];
procedure fl_add_xyplot_overlay(p1 : PFL_OBJECT; p2 : Longint {was int}; p3,p4 : pfloat; p5 : Longint {was int}; p6 :TFL_COLOR)  ; [ C ];
procedure fl_set_xyplot_overlay_type(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int})  ; [ C ];
procedure fl_delete_xyplot_overlay(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_xyplot_interpolate(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : double)  ; [ C ];
procedure fl_set_xyplot_fontsize(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_xyplot_fontstyle(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_xyplot_inspect(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_set_xyplot_symbolsize(p1 : PFL_OBJECT; p2 :  Longint {was int})  ; [ C ];
procedure fl_replace_xyplot_point(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : double; p4 : double)  ; [ C ];
procedure fl_get_xyplot_xmapping(p1 : PFL_OBJECT; p2,p3 : pfloat)  ; [ C ];
procedure fl_get_xyplot_ymapping(p1 : PFL_OBJECT; p2,p3 : pfloat)  ; [ C ];
function fl_interpolate(p1,p2 : pfloat; p3 :Longint {was int};p4,p5 : pfloat; p6 : double; p7 : Longint {was int})  : Longint ; [ C ];

procedure fl_xyplot_s2w(p1 : PFL_OBJECT; p2 : double; p3 : double; p4,p5 : pfloat)  ; [ C ];
procedure fl_xyplot_w2s(p1 : PFL_OBJECT; p2 : double; p3 : double; p4,p5 : pfloat)  ; [ C ];
procedure fl_set_xyplot_xscale(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :double)  ; [ C ];
procedure fl_set_xyplot_yscale(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 :double)  ; [ C ];

procedure fl_set_xyplot_linewidth(p1 : PFL_OBJECT; p2,p3 : Longint);[ C ];
procedure fl_set_xyplot_xgrid(p1 : PFL_OBJECT; p2 : Longint);[ C ];
procedure fl_set_xyplot_ygrid(p1 : PFL_OBJECT; p2 : Longint);[ C ];
procedure fl_set_xyplot_alphaxtics(p1 : PFL_OBJECT; p2,p3 : pchar);[ C ];
procedure fl_set_xyplot_alphaytics(p1 : PFL_OBJECT; p2,p3 : pchar);[ C ];
procedure fl_set_xyplot_fixed_xaxis(p1 : PFL_OBJECT; p2,p3 : pchar);[ C ];
procedure fl_set_xyplot_fixed_yaxis(p1 : PFL_OBJECT; p2,p3 : pchar);[ C ];

{ Added later : }
function fl_mode_capable(p1,p2 : longint ) : longint;[ C ];
procedure fl_enumerate_fonts(p1 : PFL_ENUMERATEPTR; p2 :  longint);[ C ];


{ From here we implement function aliases which were #defined }

procedure fl_set_button_shortcut(p1 : PFL_OBJECT; p2 : pchar;p3 : Longint {was int});

begin
  fl_set_object_shortcut (p1,p2,p3);
end;

procedure fl_set_pixmapbutton_data(p1 : PFL_OBJECT; p2 :  ppchar);

begin
 fl_set_pixmap_data(p1,p2);
end;

procedure fl_set_pixmapbutton_file(p1 : PFL_OBJECT; p2 :  pchar);

begin
  fl_set_pixmap_file(p1,p2);
end;

procedure fl_set_pixmapbutton_align(p1 : PFL_OBJECT; p2 : Longint {was int}; p3 : Longint {was int}; p4 : Longint {was int});
begin
  fl_set_pixmap_align(p1,p2,p3,p4);
end;

procedure fl_set_pixmapbutton_pixmap(p1 : PFL_OBJECT; p2 : TPixmap; p3 :TPixmap);
begin
 fl_set_pixmap_pixmap(p1,p2,p3);
end;

procedure fl_set_pixmapbutton_colorcloseness(p1 : Longint {was int}; p2 : Longint {was int}; p3 :Longint {was int});
begin
fl_set_pixmap_colorcloseness(p1,p2,p3);
end;

procedure fl_free_pixmapbutton_pixmap(p1 : PFL_OBJECT); 
begin
  fl_free_pixmap_pixmap(p1); 
end;

function fl_get_pixmapbutton_pixmap(p1 : PFL_OBJECT; p2 : PPixmap; p3 : PPixmap) : TPixmap ;

begin
 fl_get_pixmapbutton_pixmap:=fl_get_pixmap_pixmap(p1,p2,p3);
end;

Function FL_ObjWin ( P : PFL_Object) : TWindow;

begin
if P^.objclass <> FL_CANVAS then
    FL_ObjWin :=P^.form^.window
else
    FL_objWin := fl_get_canvas_id (p)
end;

procedure fl_rectf(x,y,w,h : TFL_COORD;c : TFL_COLOR);
begin
   fl_rectangle(1,x,y,w,h,c)
end;

procedure fl_rect(x,y,w,h : TFL_COORD;c : TFL_COLOR);
begin
    fl_rectangle(0,x,y,w,h,c)
end;

function fl_current_form : PFL_FORM;

begin
  asm
  movl fl_current_form,%eax
  movl %eax,__RESULT
  end
end;

end.
