{
    $Id: openlib.pas,v 1.2 2005/02/14 17:13:10 peter Exp $

    Two ways of opening and using libraries
    Free Pascal for MorphOS example

    Copyright (C) 2004 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * 2004.12.10 * }

program openlib;

uses exec, intuition, graphics, utility;

{ * You can enable this to manually open needed libraries, * }
{ * else it will use functions built into the units;       * }
{ * _DO NOT_ open/close DOS and Utility libraries manually * }
{ * since that's handled by the default startup/shutdown code. * }
{ DEFINE USEOPENLIB}


const
  ERRMSG_NOINTUI = 'Unable to open intuition.library V50!';
  ERRMSG_NOGFX   = 'Unable to open graphics.library V50!';

const
  MSG_INTUIOK = 'intuition.library V50 opened successfully.';
  MSG_GFXOK   = 'graphics.library V50 opened successfully.';


procedure ShutDown(exitString: String; code: LongInt);
begin

  { * When using opening functions built into the units, * }
  { * it's not needed to close libs manually, since unit exit  *}
  { * code will do it for you. * }
{$IFDEF USEOPENLIB}
  if assigned(intuitionBase) then CloseLibrary(PLibrary(intuitionBase));
  if assigned(gfxBase) then CloseLibrary(gfxBase);
{$ENDIF}

  if exitString<>'' then writeln(exitString);
  Halt(code);
end;

procedure Init;
begin

  { * Using built-in or custom library opening functions. * }
  { * It's recommended not to mix up the two ways. * }
  { * It's not needed to implement both of them in your * }
  { * programs, it's just an example to show it. * }
{$IFDEF USEOPENLIB}

  IntuitionBase:=OpenLibrary(INTUITIONNAME,50);
  if IntuitionBase=NIL then
    ShutDown(ERRMSG_NOINTUI,20)
  else
    writeln(MSG_INTUIOK);

  GfxBase:=OpenLibrary(GRAPHICSNAME,50);
  if GfxBase=NIL then
    ShutDown(ERRMSG_NOGFX,20)
  else
    writeln(MSG_GFXOK);

{$ELSE}

  if Not InitIntuitionLibrary then
    ShutDown(ERRMSG_NOINTUI,20)
  else
    writeln(MSG_INTUIOK);

  if Not InitGraphicsLibrary then
    ShutDown(ERRMSG_NOGFX,20)
  else
    writeln(MSG_GFXOK);

{$ENDIF}
end;


begin
  Init;
  ShutDown('',0);
end.

{
  $Log: openlib.pas,v $
  Revision 1.2  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.1  2004/12/14 22:00:17  karoly
    * initial revision

}
