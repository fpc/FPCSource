{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by the Free Pascal development team

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
        move(mode, ModeList^, sizeof(TModeInfo));
      end
    else
      begin
        list := ModeList;
        { go to the end of the list }
        while assigned(list^.next) do
          list:=list^.next;
        new(NewLst);
        list^.next := NewLst;
        move(mode, NewLst^, sizeof(TModeInfo));
      end;
  end;


  procedure initmode(var mode: TModeInfo);
  {********************************************************}
  { Procedure InitMode()                                   }
  {--------------------------------------------------------}
  { This routine initialized the mode to default values.   }
  {********************************************************}
   begin
     FillChar(mode,sizeof(TModeInfo),#0);
   end;


   function searchmode(ReqDriver : integer; reqmode: integer): PModeInfo;
  {********************************************************}
  { Procedure SearchMode()                                 }
  {--------------------------------------------------------}
  { This routine searches the list of recognized modes,    }
  { and tries to find the <reqmode> in the <reqdriver>     }
  { return nil if not found, otherwise returns the found   }
  { structure.                                             }
  {********************************************************}
    var
     list: PModeInfo;
    begin
        searchmode := nil;
        list := ModeList;
        { go to the end of the list }
        while assigned(list) do
          begin
             if (list^.DriverNumber = ReqDriver) and
               (list^.ModeNumber = ReqMode) then
               begin
                 searchmode := list;
                 exit;
               end;
             list:=list^.next;
          end;
    end;


   procedure cleanmode;far;
  {********************************************************}
  { Procedure CleanMode()                                  }
  {--------------------------------------------------------}
  { This routine deallocates the mode list.                }
  { It is called as an exit procedure ONLY.                }
  {********************************************************}
    var
      list: PModeInfo;
      tmp : PModeInfo;
    begin
        list := ModeList;
        { go to the end of the list }
        while assigned(list) do
        begin
          tmp := list;
          list:=list^.next;
          dispose(tmp);
        end;
    end;

{-----------------------------------------------------------------------}
{                          External routines                            }
{-----------------------------------------------------------------------}

   function GetModeName(ModeNumber: integer): string;
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

   function GetGraphMode: Integer;
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


    procedure GetModeRange(GraphDriver: Integer; var LoMode,
      HiMode: Integer);
      var
       i : integer;
       mode : PModeInfo;
     begin
       LoMode:=-1;
       HiMode:=-1;
       mode := nil;
       { First search if the graphics driver is supported ..  }
       { since mode zero is always supported.. if that driver }
       { is supported it should return something...           }
       mode := SearchMode(GraphDriver, 0);
       { driver not supported...}
       if not assigned(mode) then exit;
       { now it exists... find highest available mode... }
       LoMode := 0;
       mode:=nil;
       i:=-1;
       repeat
         inc(i);
         { start search at 0..     }
         mode:=SearchMode(GraphDriver,i);
       until not assigned(mode);
       HiMode := i-1;
     end;


  procedure SetGraphMode(mode: Integer);
    var
     modeinfo: PModeInfo;
    begin
      { check if the mode exists... }
      modeinfo := searchmode(IntcurrentDriver,mode);
      if not assigned(modeinfo) then
        begin
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
         _Graphresult := grInvalidMode;
         exit;
       end;

      if assigned(modeinfo^.PutPixel) then
         PutPixel := modeinfo^.PutPixel
      else
        begin
         _Graphresult := grInvalidMode;
         exit;
       end;

      if assigned(modeinfo^.GetPixel) then
         GetPixel := modeinfo^.GetPixel
      else
        begin
         _Graphresult := grInvalidMode;
         exit;
       end;

     if assigned(modeinfo^.SetRGBPalette) then
         SetRGBPalette := modeinfo^.SetRGBPalette
     else
        begin
         _Graphresult := grInvalidMode;
         exit;
       end;

     if assigned(modeinfo^.GetRGBPalette) then
         GetRGBPalette := modeinfo^.GetRGBPalette
     else
        begin
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
      MaxColor := modeinfo^.MaxColor;
      PaletteSize := modeinfo^.PaletteSize;
      { is this a direct color mode? }
      DirectColor := modeinfo^.DirectColor;
      { now actually initialize the video mode...}
      { check first if the routine exists        }
      if not assigned(modeinfo^.InitMode) then
        begin
          _GraphResult := grInvalidMode;
          exit;
        end;
      modeinfo^.InitMode;
      if _GraphResult <> grOk then exit;
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
       RestoreVideoState;
     end;



