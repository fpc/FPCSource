unit menu;
{---------------------------------------------------------------------------
                                 CncWare
----------------------------------------------------------------------------
  Filename..: menu.pp
  Programmer: Ken J. Wright
  Date......: 07/12/2000

  Purpose - Link to the Linux 'menu' library for ncurses menuing
            functions.

-------------------------------< Revisions >---------------------------------
 Revision|   Date   | Prog| Description
-----------------------------------------------------------------------------
 1.00    | 07/12/00 | kjw | Initial release.
-----------------------------------------------------------------------------
}
{  Automatically converted by H2PAS.EXE from menu.h
   Utility made by Florian Klaempfl 25th-28th september 96
   Improvements made by Mark A. Malakanov 22nd-25th may 97
   Further improvements by Michael Van Canneyt, April 1998
   define handling and error recovery by Pierre Muller, June 1998 }


  interface

  { C default packing is dword }

{$PACKRECORDS 4}
  {
     Copyright (c) 1998 Free Software Foundation, Inc.

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the
     "Software"), to deal in the Software without restriction, including
     without limitation the rights to use, copy, modify, merge, publish,
     distribute, distribute with modifications, sublicense, and/or sell
     copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included
     in all copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
     IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
     DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
     OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
     THE USE OR OTHER DEALINGS IN THE SOFTWARE.

     Except as contained in this notice, the name(s) of the above copyright
     holders shall not be used in advertising or otherwise to promote the
     sale, use or other dealings in this Software without prior written
     authorization.
                                                                               }
  {
       Author: Juergen Pfeifer <Juergen.Pfeifer@T-Online.de> 1995,1997
  }

{$linklib menu}

uses ncurses;

{$include eti.inc}

  const
    libmenu = 'menu';

  type
     Menu_Options = longint;
     Item_Options = longint;


  const
  { Menu options:  }
     O_ONEVALUE = $01;
     O_SHOWDESC = $02;
     O_ROWMAJOR = $04;
     O_IGNORECASE = $08;
     O_SHOWMATCH = $10;
     O_NONCYCLIC = $20;
  { Item options:  }
     O_SELECTABLE = $01;

  type

     tTEXT = record
          str : pchar;
          length : word;
       end;

     tITEM = record
          name : tTEXT;        { name of menu item                          }
          description : tTEXT; { description of item, optional in display   }
          imenu : ^tagMENU;    { Pointer to parent menu                     }
          userptr : pointer;   { Pointer to user defined per item data      }
          opt : Item_Options;  { Item options                               }
          index : integer;     { Item number if connected to a menu         }
          y : integer;         { y and x location of item in menu           }
          x : integer;
          value : bool;        { Selection value                            }
          left : ^tagITEM;     { neighbour items                            }
          right : ^tagITEM;
          up : ^tagITEM;
          down : ^tagITEM;
       end;

     pITEM = ^tITEM;
     ppITEM = ^pITEM;

     tagITEM = tITEM;

     Menu_Hook = procedure;cdecl;

     tMENU = record
          height : integer;       { Nr. of chars high                }
          width : integer;        { Nr. of chars wide                }
          rows : integer;         { Nr. of items high                }
          cols : integer;         { Nr. of items wide                }
          frows : integer;        { Nr. of formatted items high      }
          fcols : integer;        { Nr. of formatted items wide      }
          arows : integer;        { Nr. of items high (actual)       }
          namelen : integer;      { Max. name length                 }
          desclen : integer;      { Max. description length          }
          marklen : integer;      { Length of mark, if any           }
          itemlen : integer;      { Length of one item               }
          spc_desc : integer;     { Spacing for descriptor           }
          spc_cols : integer;     { Spacing for columns              }
          spc_rows : integer;     { Spacing for rows                 }
          pattern : ^char;        { Buffer to store match chars      }
          pindex : integer;       { Index into pattern buffer        }
          win : ^WINDOW;          { Window containing menu           }
          sub : ^WINDOW;          { Subwindow for menu display       }
          userwin : ^WINDOW;      { User's window                    }
          usersub : ^WINDOW;      { User's subwindow                 }
          items : ^pITEM;         { array of items                   }
          nitems : integer;       { Nr. of items in menu             }
          curitem : pITEM;        { Current item                     }
          toprow : integer;       { Top row of menu                  }
          fore : chtype;          { Selection attribute              }
          back : chtype;          { Nonselection attribute           }
          grey : chtype;          { Inactive attribute               }
          pad : byte;             { Pad character                    }
          menuinit : Menu_Hook;   { User hooks                       }
          menuterm : Menu_Hook;
          iteminit : Menu_Hook;
          itemterm : Menu_Hook;
          userptr : pointer;      { Pointer to menus user data       }
          mark : pchar;           { Pointer to marker string         }
          opt : Menu_Options;     { Menu options                     }
          status : word;          { Internal state of menu           }
       end;

     pMENU = ^tMENU;
     ppMENU = ^pMENU;

     tagMENU = tMENU;

  const
  { Define keys  }
     REQ_LEFT_ITEM      = KEY_MAX + 1;
     REQ_RIGHT_ITEM     = KEY_MAX + 2;
     REQ_UP_ITEM        = KEY_MAX + 3;
     REQ_DOWN_ITEM      = KEY_MAX + 4;
     REQ_SCR_ULINE      = KEY_MAX + 5;
     REQ_SCR_DLINE      = KEY_MAX + 6;
     REQ_SCR_DPAGE      = KEY_MAX + 7;
     REQ_SCR_UPAGE      = KEY_MAX + 8;
     REQ_FIRST_ITEM     = KEY_MAX + 9;
     REQ_LAST_ITEM      = KEY_MAX + 10;
     REQ_NEXT_ITEM      = KEY_MAX + 11;
     REQ_PREV_ITEM      = KEY_MAX + 12;
     REQ_TOGGLE_ITEM    = KEY_MAX + 13;
     REQ_CLEAR_PATTERN  = KEY_MAX + 14;
     REQ_BACK_PATTERN   = KEY_MAX + 15;
     REQ_NEXT_MATCH     = KEY_MAX + 16;
     REQ_PREV_MATCH     = KEY_MAX + 17;
     MIN_MENU_COMMAND   = KEY_MAX + 1;
     MAX_MENU_COMMAND   = KEY_MAX + 17;
  {
     Some AT&T code expects MAX_COMMAND to be out-of-band not
     just for menu commands but for forms ones as well.
     /
  #if defined(MAX_COMMAND)
  #  if (MAX_MENU_COMMAND > MAX_COMMAND)
  #    error Something is wrong -- MAX_MENU_COMMAND is greater than MAX_COMMAND
  #  elif (MAX_COMMAND != (KEY_MAX + 128))
  #    error Something is wrong -- MAX_COMMAND is already inconsistently defined.
  #  endif
  #else
  #  define MAX_COMMAND (KEY_MAX + 128)
  #endif
   }
  { --------- prototypes for libmenu functions -----------------------------  }

  function menu_items(_para1:pMENU):ppITEM;cdecl;external libmenu;
  function current_item(_para1:pMENU):pITEM;cdecl;external libmenu;
  function new_item(_para1:pchar; _para2:pchar):pITEM;cdecl;external libmenu;
  function new_menu(_para1:ppITEM):pMENU;cdecl;external libmenu;
  function item_opts(_para1:pITEM):Item_Options;cdecl;external libmenu;
  function menu_opts(_para1:pMENU):Menu_Options;cdecl;external libmenu;
(*
  function item_init(_para1:pMENU):Menu_Hook;
    begin
       { You must implemented this function }
    end;
  function item_term(_para1:pMENU):Menu_Hook;
    begin
       { You must implemented this function }
    end;
  function menu_init(_para1:pMENU):Menu_Hook;
    begin
       { You must implemented this function }
    end;
  function menu_term(_para1:pMENU):Menu_Hook;
    begin
       { You must implemented this function }
    end;
*)
  function menu_sub(_para1:pMENU):pWINDOW;cdecl;external libmenu;
  function menu_win(_para1:pMENU):pWINDOW;cdecl;external libmenu;
  function item_description(_para1:pITEM):pchar;cdecl;external libmenu;
  function item_name(_para1:pITEM):pchar;cdecl;external libmenu;
  function menu_mark(_para1:pMENU):pchar;cdecl;external libmenu;
  function menu_request_name(_para1:longint):pchar;cdecl;external libmenu;
  function menu_pattern(_para1:pMENU):pchar;cdecl;external libmenu;
  function menu_userptr(_para1:pMENU):pointer;cdecl;external libmenu;
  function item_userptr(_para1:pITEM):pointer;cdecl;external libmenu;
  function menu_back(_para1:pMENU):chtype;cdecl;external libmenu;
  function menu_fore(_para1:pMENU):chtype;cdecl;external libmenu;
  function menu_grey(_para1:pMENU):chtype;cdecl;external libmenu;
  function free_item(_para1:pITEM):longint;cdecl;external libmenu;
  function free_menu(_para1:pMENU):longint;cdecl;external libmenu;
  function item_count(_para1:pMENU):longint;cdecl;external libmenu;
  function item_index(_para1:pITEM):longint;cdecl;external libmenu;
  function item_opts_off(_para1:pITEM; _para2:Item_Options):longint;cdecl;external libmenu;
  function item_opts_on(_para1:pITEM; _para2:Item_Options):longint;cdecl;external libmenu;
  function menu_driver(_para1:pMENU; _para2:longint):longint;cdecl;external libmenu;
  function menu_opts_off(_para1:pMENU; _para2:Menu_Options):longint;cdecl;external libmenu;
  function menu_opts_on(_para1:pMENU; _para2:Menu_Options):longint;cdecl;external libmenu;
  function menu_pad(_para1:pMENU):longint;cdecl;external libmenu;
  function pos_menu_cursor(_para1:pMENU):longint;cdecl;external libmenu;
  function post_menu(_para1:pMENU):longint;cdecl;external libmenu;
  function scale_menu(_para1:pMENU; _para2:plongint; _para3:plongint):longint;cdecl;external libmenu;
  function set_current_item(menu:pMENU; item:pITEM):longint;cdecl;external libmenu;
{  function set_item_init(_para1:pMENU; _para2:Menu_Hook):longint;cdecl;external libmenu;}
  function set_item_opts(_para1:pITEM; _para2:Item_Options):longint;cdecl;external libmenu;
{  function set_item_term(_para1:pMENU; _para2:Menu_Hook):longint;cdecl;external libmenu;}
  function set_item_userptr(_para1:pITEM; _para2:pointer):longint;cdecl;external libmenu;
  function set_item_value(_para1:pITEM; _para2:bool):longint;cdecl;external libmenu;
  function set_menu_back(_para1:pMENU; _para2:chtype):longint;cdecl;external libmenu;
  function set_menu_fore(_para1:pMENU; _para2:chtype):longint;cdecl;external libmenu;
  function set_menu_format(_para1:pMENU; _para2:longint; _para3:longint):longint;cdecl;external libmenu;
  function set_menu_grey(_para1:pMENU; _para2:chtype):longint;cdecl;external libmenu;
{  function set_menu_init(_para1:pMENU; _para2:Menu_Hook):longint;cdecl;external libmenu;}
  function set_menu_items(_para1:pMENU; _para2:ppITEM):longint;cdecl;external libmenu;
  function set_menu_mark(_para1:pMENU; _para2:pchar):longint;cdecl;external libmenu;
  function set_menu_opts(_para1:pMENU; _para2:Menu_Options):longint;cdecl;external libmenu;
  function set_menu_pad(_para1:pMENU; _para2:longint):longint;cdecl;external libmenu;
  function set_menu_pattern(_para1:pMENU; _para2:pchar):longint;cdecl;external libmenu;
  function set_menu_sub(_para1:pMENU; _para2:pWINDOW):longint;cdecl;external libmenu;
{  function set_menu_term(_para1:pMENU; _para2:Menu_Hook):longint;cdecl;external libmenu;}
  function set_menu_userptr(_para1:pMENU; _para2:pointer):longint;cdecl;external libmenu;
  function set_menu_win(_para1:pMENU; _para2:pWINDOW):longint;cdecl;external libmenu;
  function set_top_row(_para1:pMENU; _para2:longint):longint;cdecl;external libmenu;
  function top_row(_para1:pMENU):longint;cdecl;external libmenu;
  function unpost_menu(_para1:pMENU):longint;cdecl;external libmenu;
  function menu_request_by_name(_para1:pchar):longint;cdecl;external libmenu;
  function set_menu_spacing(_para1:pMENU; _para2:longint; _para3:longint; _para4:longint):longint;cdecl;external libmenu;
  function menu_spacing(_para1:pMENU; _para2:plongint; _para3:plongint; _para4:plongint):longint;cdecl;external libmenu;
  function item_value(_para1:pITEM):bool;cdecl;external libmenu;
  function item_visible(_para1:pITEM):bool;cdecl;external libmenu;
(*
  procedure menu_format(_para1:pMENU; _para2:plongint; _para3:plongint);
    begin
       { You must implemented this function }
    end;
*)

  implementation

begin
end.
