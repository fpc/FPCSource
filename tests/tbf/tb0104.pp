{ %version=1.1 }
{ %fail }
{ Old file: tbs0293.pp }
{ no error with variable name = type name }

{$ifdef fpc}{$mode objfpc}{$endif}

TYPE  Ttype = class
              field :LONGINT;
              CONSTRUCTOR DOSOMETHING;
              END;

CONSTRUCTOR TTYPE.DOSOMETHING;
BEGIN
END;

var
  longint : longint;

procedure p;
VAR
  TTYPE : TTYPE;
BEGIn
  ttype:=ttype.dosomething;
END;

begin
  p;
end.
