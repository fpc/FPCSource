unit panel;
{---------------------------------------------------------------------------
                                 CncWare
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

  const
    libpanel = 'panel';

  type

     pPANEL = ^_PANEL;

     _PANEL = record
          win : ^WINDOW;
          wstarty : longint;
          wendy : longint;
          wstartx : longint;
          wendx : longint;
          below : ppanel;
          above : ppanel;
          user : longint; { NCURSES_CONST void  user; }
          obscure : pointer;
       end;

  function panel_window(_para1:pPANEL):pWINDOW;cdecl;external libpanel;
  procedure update_panels;cdecl;external libpanel;
  function hide_panel(_para1:pPANEL):longint;cdecl;external libpanel;
  function show_panel(_para1:pPANEL):longint;cdecl;external libpanel;
  function del_panel(_para1:pPANEL):longint;cdecl;external libpanel;
  function top_panel(_para1:pPANEL):longint;cdecl;external libpanel;
  function bottom_panel(_para1:pPANEL):longint;cdecl;external libpanel;
  function new_panel(_para1:pWINDOW):pPANEL;cdecl;external libpanel;
  function panel_above(_para1:pPANEL):pPANEL;cdecl;external libpanel;
  function panel_below(_para1:pPANEL):pPANEL;cdecl;external libpanel;

  { extern  int set_panel_userptr(PANEL  , NCURSES_CONST void  );  }
  { extern  NCURSES_CONST void  panel_userptr(const PANEL  );  }

  function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl;external libpanel;
  function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl;external libpanel;
  function panel_hidden(_para1:pPANEL):longint;cdecl;external libpanel;

implementation

end.
