{ Old file: tbs0021.pp }
{  tests compatibility of empty sets with other set and the evalution of constant sets                  OK 0.9.3 }

{ tests constant set evalution }

var
   a : set of byte;

const
   b : set of byte = [0..255]+[9];

type
   tcommandset = set of byte;

const
cmZoom = 10;
cmClose = 5;
cmResize = 8;
cmNext = 12;
cmPrev = 15;

CONST
   CurCommandSet : TCommandSet = ([0..255] -
        [cmZoom, cmClose, cmResize, cmNext, cmPrev]);
   commands : tcommandset = [];

var
   CommandSetChanged : boolean;

PROCEDURE DisableCommands (Commands: TCommandSet);

   BEGIN
      {$IFNDEF PPC_FPK}                                  { FPK bug }
      CommandSetChanged := CommandSetChanged OR
        (CurCommandSet * Commands <> []);                { Set changed flag }
      {$ENDIF}
      CurCommandSet := CurCommandSet - Commands;         { Update command set }
   END;

begin
   a:=[byte(1)]+[byte(2)];
end.
