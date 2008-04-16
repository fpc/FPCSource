
unit panel;
interface

uses
  ncurses;


{$PACKRECORDS C}
{$linklib panelw}

const
   libpanel = 'panelw';

(* Const before type ignored *)
type
   PPANEL  = ^TPANEL;
   TPANEL = record
        win : PWINDOW;
        below : PPANEL;
        above : PPANEL;
        user : Pointer;
     end;

(* Const before type ignored *)

function panel_window(_para1:PPANEL):PWINDOW; cdecl;external libpanel;
procedure update_panels; cdecl;external libpanel;
function hide_panel(_para1:PPANEL):Longint; cdecl;external libpanel;
function show_panel(_para1:PPANEL):Longint; cdecl;external libpanel;
function del_panel(_para1:PPANEL):Longint; cdecl;external libpanel;
function top_panel(_para1:PPANEL):Longint; cdecl;external libpanel;
function bottom_panel(_para1:PPANEL):Longint; cdecl;external libpanel;
function new_panel(_para1:PWINDOW):PPANEL; cdecl;external libpanel;
(* Const before type ignored *)
function panel_above(_para1:PPANEL):PPANEL; cdecl;external libpanel;
(* Const before type ignored *)
function panel_below(_para1:PPANEL):PPANEL; cdecl;external libpanel;
(* Const before type ignored *)
function set_panel_userptr(_para1:PPANEL; _para2:pointer):Longint; cdecl;external libpanel;
(* Const before type ignored *)
(* Const before type ignored *)
function panel_userptr(_para1:PPANEL):pointer; cdecl;external libpanel;
function move_panel(_para1:PPANEL; _para2:Longint; _para3:Longint):Longint; cdecl;external libpanel;
function replace_panel(_para1:PPANEL; _para2:PWINDOW):Longint; cdecl;external libpanel;
(* Const before type ignored *)
function panel_hidden(_para1:PPANEL):Longint; cdecl;external libpanel;

implementation


end.
