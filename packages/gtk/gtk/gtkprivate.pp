{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

type
     TGtkPrivateFlags = longint;
const
     PRIVATE_GTK_USER_STYLE = 1 shl 0;
     PRIVATE_GTK_REDRAW_PENDING = 1 shl 1;
     PRIVATE_GTK_RESIZE_PENDING = 1 shl 2;
     PRIVATE_GTK_RESIZE_NEEDED = 1 shl 3;
     PRIVATE_GTK_LEAVE_PENDING = 1 shl 4;
     PRIVATE_GTK_HAS_SHAPE_MASK = 1 shl 5;
     PRIVATE_GTK_IN_REPARENT = 1 shl 6;
     PRIVATE_GTK_IS_OFFSCREEN = 1 shl 7;

function  GTK_PRIVATE_FLAGS(wid : PGtkWidget) : longint;
function  GTK_WIDGET_USER_STYLE(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_REDRAW_PENDING(obj : PGtkWidget) : boolean;
function  GTK_CONTAINER_RESIZE_PENDING(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_RESIZE_NEEDED(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_LEAVE_PENDING(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_HAS_SHAPE_MASK(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_IN_REPARENT(obj : PGtkWidget) : boolean;
function  GTK_WIDGET_IS_OFFSCREEN(obj : PGtkWidget) : boolean;

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_PRIVATE_FLAGS(wid : PGtkWidget) : longint;
    begin
       GTK_PRIVATE_FLAGS:=wid^.private_flags;
    end;

function  GTK_WIDGET_USER_STYLE(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_USER_STYLE:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_USER_STYLE)) <> 0;
    end;

function  GTK_WIDGET_REDRAW_PENDING(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_REDRAW_PENDING:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_REDRAW_PENDING)) <> 0;
    end;

function  GTK_CONTAINER_RESIZE_PENDING(obj : PGtkWidget) : boolean;
    begin
       GTK_CONTAINER_RESIZE_PENDING:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_RESIZE_PENDING)) <> 0;
    end;

function  GTK_WIDGET_RESIZE_NEEDED(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_RESIZE_NEEDED:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_RESIZE_NEEDED)) <> 0;
    end;

function  GTK_WIDGET_LEAVE_PENDING(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_LEAVE_PENDING:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_LEAVE_PENDING)) <> 0;
    end;

function  GTK_WIDGET_HAS_SHAPE_MASK(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_HAS_SHAPE_MASK:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_HAS_SHAPE_MASK)) <> 0;
    end;

function  GTK_WIDGET_IN_REPARENT(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_IN_REPARENT:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_IN_REPARENT)) <> 0;
    end;

function  GTK_WIDGET_IS_OFFSCREEN(obj : PGtkWidget) : boolean;
    begin
       GTK_WIDGET_IS_OFFSCREEN:=((GTK_PRIVATE_FLAGS(obj)) and longint(PRIVATE_GTK_IS_OFFSCREEN)) <> 0;
    end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.4  1999/06/29 23:46:56  peter
    * changed enums to constants

  Revision 1.3  1999/06/22 15:32:12  daniel
  *** empty log message ***

  Revision 1.2  1999/05/10 15:20:07  peter
    * cdecl fixes

  Revision 1.1  1999/05/10 09:14:00  peter
    + new gtk 1.2 files

}

