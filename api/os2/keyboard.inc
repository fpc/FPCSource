{
  System independent keyboard interface for os2

  $Id$
}
procedure InitKeyboard;
begin
end;

procedure DoneKeyboard;
begin
end;

function GetKeyEvent: TKeyEvent;
begin
end;

function PollKeyEvent: TKeyEvent;
begin
end;

function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
begin
end;

function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;
begin
end;

{
  $Log$
  Revision 1.1  2000-01-06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:32  peter
    * moved some dirs

  Revision 1.1  1998/10/26 11:31:49  peter
    + inital include files

}
