{ %NORUN }
{ %OPT=-g }

program tw25600;

{$MODE DELPHI}

type
  // maybe related to
  // http://bugs.freepascal.org/view.php?id=25599
  // http://bugs.freepascal.org/view.php?id=24848
  TC<T> = class
  public
    class var F: array[0..T.X] of byte; // for normal fields all is ok
  end;

begin // Fatal: Internal error 200601281
end.

