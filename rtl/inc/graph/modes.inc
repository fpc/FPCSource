{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This include implements video mode management.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{-----------------------------------------------------------------------}
{                          Internal routines                            }
{-----------------------------------------------------------------------}

 procedure addmode(mode: TModeInfo);
  {********************************************************}
  { Procedure AddMode()                                    }
  {--------------------------------------------------------}
  { This routine adds <mode> to the list of recognized     }
  { modes. Duplicates are allowed.                         }
  {********************************************************}
  var
    list: PModeInfo;
    newlst : PModeInfo;
  begin
    if not assigned(ModeList) then
      begin
        new(ModeList);
        move(mode, ModeList^, sizeof(Mode));
      end
    else
      begin
        list := ModeList;
        { go to the end of the list }
        while assigned(list^.next) do
          list:=list^.next;
        new(NewLst);
        list^.next := NewLst;
        move(mode, NewLst^, sizeof(Mode));
      end;
  end;


  procedure initmode(var mode: TModeInfo);
  {********************************************************}
  { Procedure InitMode()                                   }
  {--------------------------------------------------------}
  { This routine initialized the mode to default values.   }
  {********************************************************}
   begin
     FillChar(mode,sizeof(Mode),#0);
   end;


   function searchmode(ReqDriver : smallint; var reqmode: smallint): PModeInfo;
  {********************************************************}
  { Procedure SearchMode()                                 }
  {--------------------------------------------------------}
  { This routine searches the list of recognized modes,    }
  { and tries to find the <reqmode> in the <reqdriver>     }
  { return nil if not found, otherwise returns the found   }
  { structure.                                             }
  { note: if reqmode = -32768, the first mode available    }
  {   for reqdriver is returned (JM)                       }
  {********************************************************}
    var
     list, lastModeInfo: PModeInfo;
    begin
{$ifdef logging}
      LogLn('Searching for driver '+strf(reqdriver)+' and mode '+strf(reqmode));
{$endif logging}
        searchmode := nil;
        list := ModeList;
        If assigned(list) then
          lastModeInfo := list;
        { go to the end of the list }
        while assigned(list) do
          begin
{$ifdef logging}
           Log('Found driver '+strf(list^.DriverNumber)+
               ' and mode $'+hexstr(list^.ModeNumber,4)+'... ');
{$endif logging}
             if ((list^.DriverNumber = ReqDriver) and
                 ((list^.ModeNumber = ReqMode) or
                  { search for lowest mode }
                  (reqMode = -32768))) or
                 { search for highest mode }
                ((reqMode = -32767) and
                 (lastModeInfo^.driverNumber = reqDriver) and
                 ((list^.driverNumber <> lastModeInfo^.driverNumber) or
                   not(assigned(list^.next)))) then
               begin
{$ifdef logging}
                 LogLn('Accepted!');
{$endif logging}
                 searchmode := list;
                 If reqMode = -32768 then
                   reqMode := list^.modeNumber
                 else if reqMode = -32767 then
                   begin
                     reqMode := lastModeInfo^.modeNumber;
                     searchMode := lastModeInfo;
                   end;
                 exit;
               end;
{$ifdef logging}
             LogLn('Rejected.');
{$endif logging}
             lastModeInfo := list;
             list:=list^.next;
          end;
    end;


{-----------------------------------------------------------------------}
{                          External routines                            }
{-----------------------------------------------------------------------}

   function GetModeName(ModeNumber: smallint): string;
  {********************************************************}
  { Function GetModeName()                                 }
  {--------------------------------------------------------}
  { Checks  the known video list, and returns ModeName     }
  { string. On error returns an empty string.              }
  {********************************************************}
    var
     mode: PModeInfo;
    begin
      mode:=nil;
      GetModeName:='';
      { only search in the current driver modes ... }
      mode:=SearchMode(IntCurrentDriver,ModeNumber);
      if assigned(mode) then
          GetModeName:=Mode^.ModeName
      else
         _GraphResult := grInvalidMode;
    end;

   function GetGraphMode: smallint;
     begin
      GetGraphMode := IntCurrentMode;
     end;

   function GetMaxMode: word;
   { I know , i know, this routine is very slow, and it would }
   { be much easier to sort the linked list of possible modes }
   { instead of doing this, but I'm lazy!! And anyways, the   }
   { speed of the routine here is not that important....      }
    var
     i: word;
     mode: PModeInfo;
    begin
      mode:=nil;
      i:=0;
      repeat
        inc(i);
        { mode 0 always exists... }
        { start search at 1..     }
        mode:=SearchMode(IntCurrentDriver,i);
      until not assigned(mode);
      GetMaxMode:=i;
    end;


    procedure GetModeRange(GraphDriver: smallint; var LoMode,
      HiMode: smallint);
      var
       mode : PModeInfo;
     begin
       {$ifdef logging}
       LogLn('GetModeRange : Enter ('+strf(GraphDriver)+')');
       {$endif}
       HiMode:=-1;
       mode := nil;
       { First search if the graphics driver is supported ..  }
       { since mode zero is always supported.. if that driver }
       { is supported it should return something...           }

       { not true, e.g. VESA doesn't have a mode 0. Changed so}
       { -32768 means "return lowest mode in second parameter }
       { also, under VESA some modes may not be supported     }
       { (e.g. $108 here) while some with a higher number can }
       { be supported ($112 and onward), so I also added that }
       { -32767 means "return highest mode in second parameter}
       { This whole system should be overhauled though to work}
       { without such hacks (JM)                              }
       loMode := -32768;
       mode := SearchMode(GraphDriver, loMode);
       { driver not supported...}
       if not assigned(mode) then
         begin
           loMode := -1;
           exit;
         end;
       {$ifdef logging}
       LogLn('GetModeRange : Mode 0 found');
       {$endif}
       { now it exists... find highest available mode... }
       hiMode := -32767;
       mode:=SearchMode(GraphDriver,hiMode);
     end;


  procedure SetGraphMode(mode: smallint);
    var
     modeinfo: PModeInfo;
    begin
      { check if the mode exists... }
      modeinfo := searchmode(IntcurrentDriver,mode);
      if not assigned(modeinfo) then
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 1');
{$endif logging}
          _GraphResult := grInvalidMode;
          exit;
       end;
    { reset all hooks...}
    DefaultHooks;
    { arccall not reset - tested against VGA BGI driver }
    { Setup all hooks if none, keep old defaults...}

      { required hooks - returns error if no hooks to these }
      { routines.                                           }
      if assigned(modeinfo^.DirectPutPixel) then
         DirectPutPixel := modeinfo^.DirectPutPixel
      else
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 2');
{$endif logging}
         _Graphresult := grInvalidMode;
         exit;
       end;

      if assigned(modeinfo^.PutPixel) then
         PutPixel := modeinfo^.PutPixel
      else
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 3');
{$endif logging}
         _Graphresult := grInvalidMode;
         exit;
       end;

      if assigned(modeinfo^.GetPixel) then
         GetPixel := modeinfo^.GetPixel
      else
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 4');
{$endif logging}
         _Graphresult := grInvalidMode;
         exit;
       end;

     if assigned(modeinfo^.SetRGBPalette) then
         SetRGBPalette := modeinfo^.SetRGBPalette
     else
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 5');
{$endif logging}
         _Graphresult := grInvalidMode;
         exit;
       end;

     if assigned(modeinfo^.GetRGBPalette) then
         GetRGBPalette := modeinfo^.GetRGBPalette
     else
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 6');
{$endif logging}
         _Graphresult := grInvalidMode;
         exit;
       end;

      { optional hooks. }
      if assigned(modeinfo^.ClearViewPort) then
         ClearViewPort := modeinfo^.ClearViewPort;
      if assigned(modeinfo^.PutImage) then
         PutImage := modeinfo^.PutImage;
      if assigned(modeinfo^.GetImage) then
         GetImage := modeinfo^.GetImage;
      if assigned(modeinfo^.ImageSize) then
         ImageSize := modeinfo^.ImageSize;
      if assigned(modeinfo^.GetScanLine) then
         GetScanLine := modeinfo^.GetScanLine;
      if assigned(modeinfo^.Line) then
         Line := modeinfo^.Line;
      if assigned(modeinfo^.InternalEllipse) then
         InternalEllipse := modeinfo^.InternalEllipse;
      if assigned(modeinfo^.PatternLine) then
         PatternLine := modeinfo^.PatternLine;
      if assigned(modeinfo^.HLine) then
         Hline := modeinfo^.Hline;
      if assigned(modeinfo^.Vline) then
         VLine := modeinfo^.VLine;
      if assigned(modeInfo^.SetVisualPage) then
         SetVisualPage := modeInfo^.SetVisualPage;
      if assigned(modeInfo^.SetActivePage) then
         SetActivePage := modeInfo^.SetActivePage;


      IntCurrentMode := modeinfo^.ModeNumber;
      IntCurrentDriver := modeinfo^.DriverNumber;
      XAspect := modeinfo^.XAspect;
      YAspect := modeinfo^.YAspect;
      MaxX := modeinfo^.MaxX;
      MaxY := modeinfo^.MaxY;
      HardwarePages := modeInfo^.HardwarePages;
      MaxColor := modeinfo^.MaxColor;
      PaletteSize := modeinfo^.PaletteSize;
      { is this a direct color mode? }
      DirectColor := modeinfo^.DirectColor;
      { now actually initialize the video mode...}
      { check first if the routine exists        }
      if not assigned(modeinfo^.InitMode) then
        begin
{$ifdef logging}
          LogLn('Mode setting failed in setgraphmode pos 7');
{$endif logging}
          _GraphResult := grInvalidMode;
          exit;
        end;
      modeinfo^.InitMode;
      if _GraphResult <> grOk then exit;
      isgraphmode := true;
      { It is very important that this call be made }
      { AFTER the other variables have been setup.  }
      { Since it calls some routines which rely on  }
      { those variables.                            }
      SetActivePage(0);
      SetVisualPage(0);
      GraphDefaults;
      SetViewPort(0,0,MaxX,MaxY,TRUE);
    end;

    procedure RestoreCrtMode;
  {********************************************************}
  { Procedure RestoreCRTMode()                             }
  {--------------------------------------------------------}
  { Returns to the video mode which was set before the     }
  { InitGraph. Hardware state is set to the old values.    }
  {--------------------------------------------------------}
  { NOTE: -                                                }
  {       -                                                }
  {********************************************************}
     begin
       isgraphmode := false;
       RestoreVideoState;
     end;

{
$Log$
Revision 1.20  2000-03-24 13:01:15  florian
  * ClearViewPort fixed

Revision 1.19  2000/01/07 16:41:39  daniel
  * copyright 2000

Revision 1.18  2000/01/07 16:32:26  daniel
  * copyright 2000 added

Revision 1.17  2000/01/02 19:02:39  jonas
  * removed/commented out (inited but) unused vars and unused types

Revision 1.16  1999/12/21 17:42:18  jonas
  * changed vesa.inc do it doesn't try to use linear modes anymore (doesn't work
    yet!!)
  * fixed mode detection so the low modenumber of a driver doesn't have to be zero
    anymore (so VESA autodetection now works)

Revision 1.15  1999/12/20 11:22:36  peter
  * integer -> smallint to overcome -S2 switch needed for ggi version

Revision 1.14  1999/12/04 21:20:04  michael
+ Additional logging

Revision 1.13  1999/11/28 16:13:55  jonas
  * corrected misplacement of call to initvars in initgraph
  + some extra debugging commands (for -dlogging) in the mode functions

Revision 1.12  1999/09/28 13:56:31  jonas
  * reordered some local variables (first 4 byte vars, then 2 byte vars
    etc)
  * font data is now disposed in exitproc, exitproc is now called
    GraphExitProc (was CleanModes) and resides in graph.pp instead of in
    modes.inc

Revision 1.11  1999/09/26 13:31:07  jonas
  * changed name of modeinfo variable to vesamodeinfo and fixed
    associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
    of sizeof(TVesamodeinfo) etc)
  * changed several sizeof(type) to sizeof(varname) to avoid similar
    errors in the future

Revision 1.10  1999/09/24 22:52:39  jonas
  * optimized patternline a bit (always use hline when possible)
  * isgraphmode stuff cleanup
  * vesainfo.modelist now gets disposed in cleanmode instead of in
    closegraph (required moving of some declarations from vesa.inc to
    new vesah.inc)
  * queryadapter gets no longer called from initgraph (is called from
    initialization of graph unit)
  * bugfix for notput in 32k and 64k vesa modes
  * a div replaced by / in fillpoly

Revision 1.9  1999/09/22 13:13:36  jonas
  * renamed text.inc -> gtext.inc to avoid conflict with system unit
  * fixed textwidth
  * isgraphmode now gets properly updated, so mode restoring works
    again

Revision 1.8  1999/09/18 22:21:11  jonas
  + hlinevesa256 and vlinevesa256
  + support for not/xor/or/andput in vesamodes with 32k/64k colors
  * lots of changes to avoid warnings under FPC

Revision 1.7  1999/07/12 13:27:14  jonas
  + added Log and Id tags
  * added first FPC support, only VGA works to some extend for now
  * use -dasmgraph to use assembler routines, otherwise Pascal
    equivalents are used
  * use -dsupportVESA to support VESA (crashes under FPC for now)
  * only dispose vesainfo at closegrph if a vesa card was detected
  * changed int32 to longint (int32 is not declared under FPC)
  * changed the declaration of almost every procedure in graph.inc to
    "far;" becquse otherwise you can't assign them to procvars under TP
    real mode (but unexplainable "data segnment too large" errors prevent
    it from working under real mode anyway)

}