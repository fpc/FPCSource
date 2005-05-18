Program Sysmousetest;
{

    This program is part of the FPC demoes.
    Copyright (C) 2000 by Marco van de Voort
    Originally for a FPC on FreeBSD article in a 2000 edition of
    the German magazine FreeX
  
    A test for sysmouse. Moused must be loaded. Maybe works in xterm too if
    X Windows is configured to use sysmouse.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses BaseUnix,Unix,Console;

CONST STDIN=0;

TYPE ActionType=(click,paste);

procedure Window1Handler(X,Y:LONGINT;Action:ActionType);

begin
  IF Action=Click THEN
   Writeln('Click in Window 1, relative coordinates: ',X,':',Y)
  ELSE
   Writeln('Paste in Window 1, relative coordinates: ',X,':',Y);
end;

procedure Window2Handler(X,Y:LONGINT;Action:ActionType);

begin
  IF Action=Click THEN
   Writeln('Click in Window 2, relative coordinates: ',X,':',Y)
  ELSE
   Writeln('Paste in Window 2, relative coordinates: ',X,':',Y);
end;

procedure Window3Handler(X,Y:LONGINT;Action:ActionType);

begin
  IF Action=Click THEN
   Writeln('Click in Window 3, relative coordinates: ',X,':',Y)
  ELSE
   Writeln('Paste in Window 3, relative coordinates: ',X,':',Y);
end;

procedure Window4Handler(X,Y:LONGINT;Action:ActionType);

begin
  IF Action=Click THEN
   Writeln('Click in Window 4, relative coordinates: ',X,':',Y)
  ELSE
   Writeln('Paste in Window 4, relative coordinates: ',X,':',Y);
end;

{Of course in a real window manager, all this would be more dynamic (so you
can change windows, and have them stacked. }

TYPE WindowHandlerProc = procedure (X,Y:longint;Action:ActionType);
     WindowListType    = ARRAY[1..4] OF WindowHandlerProc;

CONST WindowList : WindowListType=(@Window1Handler,@Window2Handler,
  	  	   @Window3Handler,@Window4Handler);

var cwidth,cheight    : longint; {Dimensions of a char cell. 
				   For pixels to chars}
    xpos,ypos,buttons : longint; {Location and type of last mouseclick}
    Focus	      : Longint; {Quarter of screen that has focus now}
    TermApp	      : Boolean;

{
 * Signal handler for SIGUSR2: Retrieves mouse coordinates; converts pixels
 * to rows and columns.
 }
procedure Sysmousehandler(Sig:Longint);cdecl;	{IMPORTANT!  call back has C calling convention}

var mi : MOUSE_INFO;
    fd : longint;
begin
        fd:=STDIN;
	mi.operation := MOUSE_GETINFO;
	IF NOT CONS_MOUSECTL(fd, mi) THEN 
        {Mouse call failed, don't update vars}
	 exit;			
		
	xpos := mi.u.data.x;
	ypos := mi.u.data.y;
	buttons := mi.u.data.buttons and 7;
end;	

procedure StartMouse;
{initialise the mouse and determine the sizes of a standard character cell}

var
	mi : mouse_info_t;
	vi : video_info_t;
	fd : longint;
	
begin 
  fd:=longint(stdin);	
  if FBIO_GETMODE(fd,vi.vi_mode) AND FBIO_MODEINFO(fd,vi) then
   begin   
    cwidth:=vi.vi_cwidth;   
    cheight:=vi.vi_cheight;
    Writeln('Dimensions of a character cell (width :height) :',Cwidth,':',cheight);
   end;

  {ignore SIGUSR2 for a while, otherwise moving the mouse before handler
    installation will terminate the application}
  
  fpSignal(SIGUSR2,SignalHandler(SIG_IGN));
  
 { Have sysmouse send us SIGUSR2 for mouse state changes. }

  mi.operation := _MOUSE_MODE; { Note: underscore added!}
  mi.u.mode.mode := 0;
  mi.u.mode.signal := SIGUSR2;

  {If successful, register signal handler}

  if CONS_MOUSECTL(fd,mi) then
   begin 
    { Connect SIGUSR2 to our (C calling convention!) mousehandler}

    fpsignal(SIGUSR2, @SysmouseHandler);

    {show mouse}
    mi.operation := MOUSE_SHOW;
    CONS_MOUSECTL(fd, mi);
    exit;
   end;
end;
	

procedure myhandler(x,y,but :longint);

VAR WindowNr : Longint;

begin
 {Upper left 2x2 character cell block terminates program}
 if (X<3) AND (Y<3) then
  begin
   TermApp:=TRUE;
   EXIT;
  end;
    {The screen is divided in four windows and are numbered as follows:

		   1|2
		   ---
		   3|4}

   if (x<=40) then
    WindowNr:=1
   else
    WindowNr:=2;
  IF Y>12 THEN
   INC(WindowNr,2);

  IF WindowNr=Focus THEN
   BEGIN
      {This window already has focus. Normalise coordinates and
	pass the event to the window}
     IF X>40 THEN Dec(X,40);
     IF Y>12 THEN Dec(Y,12);
    IF (But and 1)=1 THEN
     WindowList[WindowNr](x,y,click)
    else
     IF (But and 4)=4 THEN
      WindowList[WindowNr](x,y,paste)
    else
     Writeln('I only have a two button mouse, so this one does nothing');
  END
 else
  BEGIN
   Writeln('Main handler is changing focus from to window',WindowNr);
   Focus:=WindowNr;
  end;
end;

procedure WaitForEvent; 
{
 * Wait in select() loop.  If interrupted, check for mouse button press and
 * construct a minimal gpm pseudo-event and call MouseHandler(). Otherwise
 * hand over to wgetch().
}

var rfds : tsigset;
    
begin
  fpsigemptyset(rfds);
  fpsigaddset(rfds,STDIN);
  while fpselect(1, @rfds,nil,nil,nil)<=0 DO
    begin
      IF TermApp THEN Exit;
      if (fpgeterrno= ESYSEINTR) AND (buttons<>0) THEN
       MyHandler ((xpos DIV cwidth)+1,(ypos DIV cheight)+1,buttons);
    end;
end;

begin 
// if physicalconsole(0) then
// begin
 {Don't want to use ncurses, to easier link static}

  Write(#27'[?1001s'); { save old hilight tracking }
  Write(#27'[?1000h'); { enable mouse tracking }
  for cwidth:=1 to 25 DO Writeln; 
  Writeln('Sysmouse demo, click upper-left corner to exit this program');
  Writeln;
  Writeln('Sysmouse implements a very simple mouse event driven windowing program');
  Writeln('The 4 quadrants of the screen act as windows, and focus is implemented');
  Writeln('Try to click (left or right) the different quadrants, and see what happens');
  Writeln;
  cwidth := 8; cheight := 16;
  Focus:=0;
  StartMouse;
  TermApp:=FALSE;
  while not TermApp do WaitForEvent;
// end
//else 
//  Writeln('This program must be run from the physical console, not over telnet or under X');
end.
