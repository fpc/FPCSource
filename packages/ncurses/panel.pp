unit panel;
{---------------------------------------------------------------------------
                                 CncWare
                            (c) Copyright 1999
----------------------------------------------------------------------------
  Filename..: panel.pp
  Programmer: Ken J. Wright
  Date......: 12/08/1999

  Purpose - Link to the Linux 'panel' library for ncurses windowing 
            functions. The panel library handles overlapping windows,
            whereas, native ncurses windowing is only tiled.

-------------------------------< Revisions >---------------------------------
 Revision|   Date   | Prog| Description
-----------------------------------------------------------------------------
 1.00    | 12/08/99 | kjw | Initial release.
-----------------------------------------------------------------------------
}
interface
uses ncurses;

{$PACKRECORDS 4}
{$linklib panel}

  type

     pPANEL = ^_PANEL;

     _PANEL = record
          win : ^WINDOW;
          wstarty : longint;
          wendy : longint;
          wstartx : longint;
          wendx : longint;
          below : ^_panel;
          above : ^_panel;
          user : longint; { NCURSES_CONST void  user; }
          obscure : pointer;
       end;

  function panel_window(_para1:pPANEL):pWINDOW;cdecl;
  procedure update_panels;cdecl;
  function hide_panel(_para1:pPANEL):longint;cdecl;
  function show_panel(_para1:pPANEL):longint;cdecl;
  function del_panel(_para1:pPANEL):longint;cdecl;
  function top_panel(_para1:pPANEL):longint;cdecl;
  function bottom_panel(_para1:pPANEL):longint;cdecl;
  function new_panel(_para1:pWINDOW):pPANEL;cdecl;
  function panel_above(_para1:pPANEL):pPANEL;cdecl;
  function panel_below(_para1:pPANEL):pPANEL;cdecl;

  { extern  int set_panel_userptr(PANEL  , NCURSES_CONST void  );  }
  { extern  NCURSES_CONST void  panel_userptr(const PANEL  );  }

  function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl;
  function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl;
  function panel_hidden(_para1:pPANEL):longint;cdecl;

implementation

const External_library='';

  function panel_window(_para1:pPANEL):pWINDOW;cdecl;External;
  procedure update_panels;cdecl;External;
  function hide_panel(_para1:pPANEL):longint;cdecl;External;
  function show_panel(_para1:pPANEL):longint;cdecl;External;
  function del_panel(_para1:pPANEL):longint;cdecl;External;
  function top_panel(_para1:pPANEL):longint;cdecl;External;
  function bottom_panel(_para1:pPANEL):longint;cdecl;External;
  function new_panel(_para1:pWINDOW):pPANEL;cdecl;External;
  function panel_above(_para1:pPANEL):pPANEL;cdecl;External;
  function panel_below(_para1:pPANEL):pPANEL;cdecl;External;

  { extern  int set_panel_userptr(PANEL  , NCURSES_CONST void  );  }
  { extern  NCURSES_CONST void  panel_userptr(const PANEL  );  }

  function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl;External;
  function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl;External;
  function panel_hidden(_para1:pPANEL):longint;cdecl;External;

end.
