{ problem of conversion between
  smallsets and long sets }
type

{ Command sets }

  PCommandSet = ^TCommandSet;
  TCommandSet = set of Byte;

Const
  cmValid   = 0;
  cmQuit    = 1;
  cmError   = 2;
  cmMenu    = 3;
  cmClose   = 4;
  cmZoom    = 5;
  cmResize  = 6;
  cmNext    = 7;
  cmPrev    = 8;
  cmHelp    = 9;

{ Application command codes }

  cmCut     = 20;
  cmCopy    = 21;
  cmPaste   = 22;
  cmUndo    = 23;
  cmClear   = 24;
  cmTile    = 25;
  cmCascade = 26;

  CurCommandSet: TCommandSet =
    [0..255] - [cmZoom, cmClose, cmResize, cmNext, cmPrev];


  begin
  end.
