{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Window menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.TileVertical;
begin
  if not assigned(Desktop) then exit;
  Desktop^.TileColumnsFirst:=True;
  Tile;
  Desktop^.TileColumnsFirst:=False;
end;

FUNCTION Tileable (P: PView): Boolean;
BEGIN
   Tileable := (P^.Options AND ofTileable <> 0) AND   { View is tileable }
     (P^.State AND sfVisible <> 0);                   { View is visible }
END;

{ This pretty much copy of window cascade }
{ added extra variable `Count` }
{ calculate one of `NR.A.Y` or `NR.A.X` in reverse }
procedure TIDEApp.Stepped(aDirection:boolean);
var R: TRect;
VAR CascadeNum, Count: SmallInt; LastView: PView; Min, Max: TPoint;

   PROCEDURE DoCount (P: PView);
   BEGIN
     If Tileable(P) Then Begin
       Inc(CascadeNum); LastView := P;                { Count tileable }
     End;
   END;

   PROCEDURE DoCascade (P: PView);
   VAR PState: Word; NR: TRect;
   BEGIN
     If Tileable(P) AND (CascadeNum >= 0) Then Begin  { View tileable }
       NR.Copy(R);                                    { Copy rect area }
       if aDirection then
       begin
         {option 1 active window in lowest position, window titles visible}
         Inc(NR.A.X, Count*1-CascadeNum*1-1);         { Inc x position }
         Inc(NR.A.Y, CascadeNum);                     { Inc y position }
       end else
       begin
         {option 2 active window in highest position, window titles not visible}
         Inc(NR.A.X, CascadeNum*1);                     { Inc x position }
         Inc(NR.A.Y, Count-CascadeNum-1);               { Inc y position }
       end;
       PState := P^.State;                            { Hold view state }
       P^.State := P^.State AND NOT sfVisible;        { Temp stop draw }
       P^.Locate(NR);                                 { Locate the view }
       P^.State := PState;                            { Now allow draws }
       Dec(CascadeNum);                               { Dec count }
     End;
   END;

begin
   if not assigned(Desktop) then exit;
   GetTileRect(R);                                    { Tileable area }
   CascadeNum := 0;                                   { Zero step count }
   Desktop^.ForEach(TCallbackProcParam(@DoCount));    { Count tileable }
   Count:=CascadeNum;                                 { Keep count in Count }
   If (CascadeNum>0) Then Begin
     LastView^.SizeLimits(Min, Max);                  { Check size limits }
     If (Min.X > R.B.X - R.A.X - CascadeNum) OR
     (Min.Y > R.B.Y - R.A.Y - CascadeNum) Then
     Desktop^.TileError Else Begin                    { Check for error }
       Dec(CascadeNum);                               { One less view }
       Desktop^.ForEach(TCallbackProcParam(@DoCascade)); { Stepped view }
       Desktop^.DrawView;                             { Redraw now }
     End;
   End;
end;

procedure TIDEApp.CloseAll;

  procedure SendClose(P: PView);
  begin
    Message(P,evCommand,cmClose,nil);
  end;

begin
  Desktop^.ForEach(TCallbackProcParam(@SendClose));
end;

procedure TIDEApp.ResizeApplication(x, y : longint);
var
  OldR : TRect;
  Mode : TVideoMode;
begin
  GetBounds(OldR);
  { adapt to new size }
  if (OldR.B.Y-OldR.A.Y<>y) or
     (OldR.B.X-OldR.A.X<>x) then
    begin
      Mode.color:=ScreenMode.Color;
      Mode.col:=x;
      Mode.row:=y;
      SetScreenVideoMode(Mode);
      UpdateRecentFileList; {ensure file menu not go over screen}
      Redraw;
    end;
end;

type
    PWindowListBox = ^TWindowListBox;
    TWindowListBox = object(TAdvancedListBox)
      constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
    end;

    PWindowListDialog = ^TWindowListDialog;
    TWindowListDialog = object(TCenterDialog)
      constructor Init;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      LB: PWindowListBox;
      C : PCollection;
      BtnShow,BtnHide: PNoUpdateButton;
      procedure  UpdateList;
      procedure  UpdateButtons;
    end;

constructor TWindowListBox.Init(var Bounds: TRect; AScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AScrollBar);
end;

function TWindowListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PView;
    S: string;
    savMaxLen: Sw_Integer;
begin
  P:=List^.At(Item);
  savMaxLen:=MaxLen;
  MaxLen:=MaxLen-9; { have to have less to actually fit in long file paths }
  case P^.HelpCtx of
    hcSourceWindow : S:=PSourceWindow(P)^.GetTitle(MaxLen);
    hcHelpWindow   : S:=PHelpWindow(P)^.GetTitle(MaxLen);
    hcCalcWindow   : S:=PCalculator(P)^.GetTitle(MaxLen);
    hcBrowserWindow: S:=PBrowserWindow(P)^.GetTitle(MaxLen);
    hcCompilerMessagesWindow,
    hcMessagesWindow:S:=PFPWindow(P)^.GetTitle(MaxLen);
    hcGDBWindow,
    hcDisassemblyWindow,
    hcWatchesWindow,
    hcStackWindow,
    hcRegistersWindow,
    hcFPURegisters,
    hcVectorRegisters,
    hcClipboardWindow,
    hcASCIITableWindow,
    hcUserScreenWindow,
    hcBreakpointListWindow :
      S:=PWindow(P)^.GetTitle(MaxLen);
  else S:='???? - '+PWindow(P)^.GetTitle(MaxLen);
  end;
  MaxLen:=savMaxLen;
  if PWindow(P)^.Number<>0 then
    S:=S+'('+IntToStr(PWindow(P)^.Number)+')';
  if P^.GetState(sfVisible) then S:=' '+S else
    begin
      S:='*'+S+' - '+msg_windowlist_hidden;
    end;
  GetText:=copy(S,1,MaxLen);
end;

constructor TWindowListDialog.Init;
var R,R2: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,Max(60,round(ScreenWidth*5/8)),Max(13,round(ScreenHeight*3/5)));
  inherited Init(R, dialog_windowlist);
  HelpCtx:=hcWindowList;
  New(C, Init(20,10));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.X:=R.B.X-14;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(LB, Init(R, SB));
  LB^.Default:=true;
  LB^.NewList(C);
  UpdateList;
  if C^.Count>=2 then
   if PWindow(C^.At(1))^.GetState(sfVisible) then
    LB^.FocusItem(1); { focus the 2nd one }
  Insert(LB);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_wndlist_windows, LB)));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.A.X:=R.B.X-13+1; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, button_OK, cmOK, bfDefault)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Delete, cmDeleteItem, bfNormal)));
  R.Move(0,2);
  New(BtnShow, Init(R, button_Show, cmShowItem, bfNormal));
  Insert(BtnShow);
  R.Move(0,2);
  New(BtnHide, Init(R, button_Hide, cmHideItem, bfNormal));
  Insert(BtnHide);
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));

  LB^.Select;
  PutCommand(@Self,evBroadcast,cmListFocusChanged,LB);
end;

procedure TWindowListDialog.UpdateList;
var VisState: boolean;
procedure AddIt(P: PView);
begin
  if (P<>pointer(Desktop^.Background)) and
     (P^.GetState(sfDisabled)=false) and
     ((P^.Options and ofSelectable)<>0) and
     (P^.GetState(sfVisible)=VisState) then
     C^.Insert(P);
end;
begin
  C^.DeleteAll;
  VisState:=true; Desktop^.ForEach(TCallbackProcParam(@AddIt)); { add visible windows to list }
  VisState:=false; Desktop^.ForEach(TCallbackProcParam(@AddIt)); { add hidden windows }
  LB^.SetRange(C^.Count);
  UpdateButtons;
  ReDraw;
end;

procedure TWindowListDialog.UpdateButtons;
var W: PView;
begin
  if LB^.Range>0 then
    begin
      W:=LB^.List^.At(LB^.Focused);
      if Assigned(BtnShow) then
        BtnShow^.SetState(sfDisabled,W^.GetState(sfVisible));
      if Assigned(BtnHide) then
        BtnHide^.SetState(sfDisabled,not W^.GetState(sfVisible));
    end
  else
    begin
      BtnShow^.SetState(sfDisabled,true);
      BtnHide^.SetState(sfDisabled,true);
    end;
  ReDraw;
end;

procedure TWindowListDialog.HandleEvent(var Event: TEvent);
var W: PWindow;
    KeePOwner : PGroup;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbDel :
          begin
            Message(@Self,evCommand,cmDeleteItem,nil);
            ClearEvent(Event);
          end;
      end;
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
          if Event.InfoPtr=LB then
            UpdateButtons;
      end;
    evCommand :
      case Event.Command of
        cmDeleteItem :
          if C^.Count>0 then
          begin
            W:=PWindow(C^.At(LB^.Focused));
            { we need to remove the window from the list
              because otherwise
              IDEApp.SourceWindowClosed
              is called after the object has been freed
              but the ListBox.Redraw will still try to
              read the title PM }
            KeepOwner:=W^.Owner;
            if assigned(KeepOwner) then
              KeepOwner^.Delete(W);
            UpdateList;
            { But reinsert it as Close might only
              trigger Hide in some cases }
            if assigned(KeepOwner) then
              KeepOwner^.Insert(W);
            Message(W,evCommand,cmClose,nil);
            UpdateList;
            ClearEvent(Event);
          end;
        cmShowItem :
          if C^.Count>0 then
          begin
            PWindow(C^.At(LB^.Focused))^.Show;
            UpdateList;
            ClearEvent(Event);
          end;
        cmHideItem :
          if C^.Count>0 then
          begin
            PWindow(C^.At(LB^.Focused))^.Hide;
            UpdateList;
            ClearEvent(Event);
          end;
        cmOK :
          if C^.Count>0 then
          begin
            W:=PWindow(C^.At(LB^.Focused));
            if W^.GetState(sfVisible)=false then
              W^.Show;
            W^.MakeFirst;
          end;
      end;
  end;
  inherited HandleEvent(Event);
end;

destructor TWindowListDialog.Done;
begin
  if C<>nil then begin C^.DeleteAll; Dispose(C, Done); end;
  inherited Done;
end;

procedure TIDEApp.WindowList;
var W: PWindowListDialog;
begin
  New(W,Init);
  ExecView(W);
  Dispose(W,Done);
  Desktop^.Lock;
  { force correct commands to be enabled }
  Desktop^.SetState(sfActive,true);  { activate everything }
  Desktop^.SetState(sfActive,false); { deactivate everything }
  if assigned(Desktop^.Current) then
    Desktop^.Current^.SetState(sfActive,true); { set active only current }
  Desktop^.UnLock;
end;

