{$MODE OBJFPC}
unit menu;

interface

uses
  ncurses;

{$LINKLIB menuw}
{$PACKRECORDS C}
{$INCLUDE eti.inc}

const
   libmenu = 'menuw';

type
   cuchar = Byte;
   Menu_Options = Longint;
   Item_Options = Longint;

const
//    TEXT = TEXT_ncurses;
{ Menu options:  }
   O_ONEVALUE = $01;   { Only one item can be selected for this menu. }
   O_SHOWDESC = $02;   { Display the item descriptions when the menu is posted.  }
   O_ROWMAJOR = $04;   { Display the menu in row-major order.  }
   O_IGNORECASE = $08; { Ignore the case when pattern-matching. }
   O_SHOWMATCH = $10;  { Move the cursor to within the item name while pattern-matching.  }
   O_NONCYCLIC = $20;  { Don't wrap around next-item and previous-item, requests to the other end of the menu. }
{ Item options:  }
   O_SELECTABLE = $01;

type
   TTEXT = record
        str : PChar;
        length : Word;
     end;

   ppITEM = ^pITEM;
   pITEM  = ^tITEM;

   pMENU = ^tMENU;
   ppMENU = ^pMENU;

   tITEM = record
        name : TTEXT;         { name of menu item                          }
        description : TTEXT;  { description of item, optional in display   }
        imenu : pMENU;        { Pointer to parent menu                     }
        userptr : Pointer;    { Pointer to user defined per item data      }
        opt : Item_Options;   { Item options                               }
        index : Smallint;     { Item number if connected to a menu         }
        y : Smallint;         { y and x location of item in menu           }
        x : Smallint;
        value : Bool;         { Selection value                            }
        left : pITEM;         { neighbor items                             }
        right : pITEM;
        up : pITEM;
        down : pITEM;
     end;



   tagITEM = tITEM;

   Menu_Hook = procedure (_para1:pMENU);cdecl;

   tMENU = record
        height : Smallint;      { Nr. of chars high                }
        width : Smallint;       { Nr. of chars wide                }
        rows : Smallint;        { Nr. of items high                }
        cols : Smallint;        { Nr. of items wide                }
        frows : Smallint;       { Nr. of formatted items high      }
        fcols : Smallint;       { Nr. of formatted items wide      }
        arows : Smallint;       { Nr. of items high (actual)       }
        namelen : Smallint;     { Max. name length                 }
        desclen : Smallint;     { Max. description length          }
        marklen : Smallint;     { Length of mark, if any           }
        itemlen : Smallint;     { Length of one item               }
        spc_desc : Smallint;    { Spacing for descriptor           }
        spc_cols : Smallint;    { Spacing for columns              }
        spc_rows : Smallint;    { Spacing for rows                 }
        pattern : PChar;        { Buffer to store match chars      }
        pindex : Smallint;      { Index into pattern buffer        }
        win : PWINDOW;          { Window containing menu           }
        sub : PWINDOW;          { Subwindow for menu display       }
        userwin : PWINDOW;      { User's window                    }
        usersub : PWINDOW;      { User's subwindow                 }
        items : ppITEM;         { array of items                   }
        nitems : Smallint;      { Nr. of items in menu             }
        curitem : pITEM;        { Current item                     }
        toprow : Smallint;      { Top row of menu                  }
        fore : chtype;          { Selection attribute              }
        back : chtype;          { Nonselection attribute           }
        grey : chtype;          { Inactive attribute               }
        pad : cuchar;           { Pad character                    }
        menuinit : Menu_Hook;   { User hooks                       }
        menuterm : Menu_Hook;
        iteminit : Menu_Hook;
        itemterm : Menu_Hook;
        userptr : Pointer;      { Pointer to menus user data       }
        mark : PChar;           { Pointer to marker string         }
        opt : Menu_Options;     { Menu options                     }
        status : Word;          { Internal state of menu           }
     end;




   tagMENU = tMENU;

{ Define keys  }

const
   REQ_LEFT_ITEM     = KEY_MAX + 1;
   REQ_RIGHT_ITEM    = KEY_MAX + 2;
   REQ_UP_ITEM       = KEY_MAX + 3;
   REQ_DOWN_ITEM     = KEY_MAX + 4;
   REQ_SCR_ULINE     = KEY_MAX + 5;
   REQ_SCR_DLINE     = KEY_MAX + 6;
   REQ_SCR_DPAGE     = KEY_MAX + 7;
   REQ_SCR_UPAGE     = KEY_MAX + 8;
   REQ_FIRST_ITEM    = KEY_MAX + 9;
   REQ_LAST_ITEM     = KEY_MAX + 10;
   REQ_NEXT_ITEM     = KEY_MAX + 11;
   REQ_PREV_ITEM     = KEY_MAX + 12;
   REQ_TOGGLE_ITEM   = KEY_MAX + 13;
   REQ_CLEAR_PATTERN = KEY_MAX + 14;
   REQ_BACK_PATTERN  = KEY_MAX + 15;
   REQ_NEXT_MATCH    = KEY_MAX + 16;
   REQ_PREV_MATCH    = KEY_MAX + 17;
   MIN_MENU_COMMAND  = KEY_MAX + 1;
   MAX_MENU_COMMAND  = KEY_MAX + 17;

{
 * Some AT&T code expects MAX_COMMAND to be out-of-band not
 * just for menu commands but for forms ones as well.
  }
{ --------- prototypes for libmenu functions -----------------------------  }

function menu_items(_para1:PMENU):ppITEM; cdecl;external libncurses;
function current_item(_para1:PMENU):pITEM; cdecl;external libncurses;
function new_item(_para1:PChar; _para2:PChar):pITEM; cdecl;external libncurses;
function new_menu(_para1:PPITEM):pMENU; cdecl;external libncurses;
function item_opts(_para1:PITEM):Item_Options; cdecl;external libncurses;
function menu_opts(_para1:PMENU):Menu_Options; cdecl;external libncurses;
function item_init(_para1:PMENU):Menu_Hook; cdecl;external libncurses;
function item_term(_para1:PMENU):Menu_Hook; cdecl;external libncurses;
function menu_init(_para1:PMENU):Menu_Hook; cdecl;external libncurses;
function menu_term(_para1:PMENU):Menu_Hook; cdecl;external libncurses;
function menu_sub(_para1:PMENU):PWINDOW; cdecl;external libncurses;
function menu_win(_para1:PMENU):PWINDOW; cdecl;external libncurses;
function item_description(_para1:PITEM):PChar; cdecl;external libncurses;
function item_name(_para1:PITEM):PChar; cdecl;external libncurses;
function menu_mark(_para1:PMENU):PChar; cdecl;external libncurses;
function menu_request_name(_para1:Longint):PChar; cdecl;external libncurses;
function menu_pattern(_para1:PMENU):PChar; cdecl;external libncurses;
function menu_userptr(_para1:PMENU):Pointer; cdecl;external libncurses;
function item_userptr(_para1:PITEM):Pointer; cdecl;external libncurses;
function menu_back(_para1:PMENU):chtype; cdecl;external libncurses;
function menu_fore(_para1:PMENU):chtype; cdecl;external libncurses;
function menu_grey(_para1:PMENU):chtype; cdecl;external libncurses;
function free_item(_para1:PITEM):Longint; cdecl;external libncurses;
function free_menu(_para1:PMENU):Longint; cdecl;external libncurses;
function item_count(_para1:PMENU):Longint; cdecl;external libncurses;
function item_index(_para1:PITEM):Longint; cdecl;external libncurses;
function item_opts_off(_para1:PITEM; _para2:Item_Options):Longint; cdecl;external libncurses;
function item_opts_on(_para1:PITEM; _para2:Item_Options):Longint; cdecl;external libncurses;
function menu_driver(_para1:PMENU; _para2:Longint):Longint; cdecl;external libncurses;
function menu_opts_off(_para1:PMENU; _para2:Menu_Options):Longint; cdecl;external libncurses;
function menu_opts_on(_para1:PMENU; _para2:Menu_Options):Longint; cdecl;external libncurses;
function menu_pad(_para1:PMENU):Longint; cdecl;external libncurses;
function pos_menu_cursor(_para1:PMENU):Longint; cdecl;external libncurses;
function post_menu(_para1:PMENU):Longint; cdecl;external libncurses;
function scale_menu(_para1:PMENU; _para2:PLongint; _para3:PLongint):Longint; cdecl;external libncurses;
function set_current_item(menu:PMENU; item:PITEM):Longint; cdecl;external libncurses;
//function set_item_init(_para1:PMENU; _para2:procedure (_para1:PMENU)):Longint; cdecl;external libncurses;
function set_item_init(_para1:PMENU; _para2:Menu_Hook):Longint; cdecl;external libncurses;
function set_item_opts(_para1:PITEM; _para2:Item_Options):Longint; cdecl;external libncurses;
//function set_item_term(_para1:PMENU; _para2:procedure (_para1:PMENU)):Longint; cdecl;external libncurses;
function set_item_term(_para1:PMENU; _para2:Menu_Hook):Longint; cdecl;external libncurses;
function set_item_userptr(_para1:PITEM; _para2:Pointer):Longint; cdecl;external libncurses;
function set_item_value(_para1:PITEM; _para2:Bool):Longint; cdecl;external libncurses;
function set_menu_back(_para1:PMENU; _para2:chtype):Longint; cdecl;external libncurses;
function set_menu_fore(_para1:PMENU; _para2:chtype):Longint; cdecl;external libncurses;
function set_menu_format(_para1:PMENU; _para2:Longint; _para3:Longint):Longint; cdecl;external libncurses;
function set_menu_grey(_para1:PMENU; _para2:chtype):Longint; cdecl;external libncurses;
//function set_menu_init(_para1:PMENU; _para2:procedure (_para1:PMENU)):Longint; cdecl;external libncurses;
function set_menu_init(_para1:PMENU; _para2:Menu_Hook):Longint; cdecl;external libncurses;
function set_menu_items(_para1:PMENU; _para2:PPITEM):Longint; cdecl;external libncurses;
function set_menu_mark(_para1:PMENU; _para2:PChar):Longint; cdecl;external libncurses;
function set_menu_opts(_para1:PMENU; _para2:Menu_Options):Longint; cdecl;external libncurses;
function set_menu_pad(_para1:PMENU; _para2:Longint):Longint; cdecl;external libncurses;
function set_menu_pattern(_para1:PMENU; _para2:PChar):Longint; cdecl;external libncurses;
function set_menu_sub(_para1:PMENU; _para2:PWINDOW):Longint; cdecl;external libncurses;
//function set_menu_term(_para1:PMENU; _para2:procedure (_para1:PMENU)):Longint; cdecl;external libncurses;
function set_menu_term(_para1:PMENU; _para2:Menu_Hook):Longint; cdecl;external libncurses;
function set_menu_userptr(_para1:PMENU; _para2:Pointer):Longint; cdecl;external libncurses;
function set_menu_win(_para1:PMENU; _para2:PWINDOW):Longint; cdecl;external libncurses;
function set_top_row(_para1:PMENU; _para2:Longint):Longint; cdecl;external libncurses;
function top_row(_para1:PMENU):Longint; cdecl;external libncurses;
function unpost_menu(_para1:PMENU):Longint; cdecl;external libncurses;
function menu_request_by_name(_para1:PChar):Longint; cdecl;external libncurses;
function set_menu_spacing(_para1:PMENU; _para2:Longint; _para3:Longint; _para4:Longint):Longint; cdecl;external libncurses;
function menu_spacing(_para1:PMENU; _para2:PLongint; _para3:PLongint; _para4:PLongint):Longint; cdecl;external libncurses;
function item_value(_para1:PITEM):Bool; cdecl;external libncurses;
function item_visible(_para1:PITEM):Bool; cdecl;external libncurses;
procedure menu_format(_para1:PMENU; _para2:PLongint; _para3:PLongint); cdecl;external libncurses;

implementation


end.
