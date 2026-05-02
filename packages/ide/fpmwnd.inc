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

procedure TIDEApp.CloseAll;

  procedure SendClose(P: PView);
  begin
    Message(P,evCommand,cmClose,nil);
  end;

begin
  Desktop^.ForEach(@SendClose);
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
      SearchPos : Sw_Word;
      CurString : String;
      constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetKey(var S: String): Pointer; virtual;
      procedure   NewList(AList: PCollection); virtual;
    end;

    PWindowListDialog = ^TWindowListDialog;
    TWindowListDialog = object(TCenterDialog)
      constructor Init;
      procedure   SizeLimits (Var Min, Max: TPoint); Virtual;
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
  SearchPos:=0;
  CurString:='';
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

procedure TWindowListBox.HandleEvent(var Event: TEvent);
const
  SpecialChars: set of AnsiChar = [#0,#9,#27];
var
  K: Pointer;
  Value : Sw_integer;
  OldPos, OldValue: Sw_Integer;
  T: Boolean;
  nX : Sw_Integer;
  NewString: String;//Sw_String;

  function UnsortedListSearch(AKey : PString; var AValue, nX : Sw_Integer):boolean;
  var Count, k,n,z : Sw_Integer;
      Found : boolean;
      S : String;
  begin
    UnsortedListSearch:=false;
    nX:=1;
    if List=nil then exit;
    if AKey=nil then exit;
    if Length(AKey^)=0 then exit; {make no sense to search for empty Key}
    Count:= List^.Count;
    Found:=false;
    for k:=0 to Count-1 do
    begin
      S:=GetText(K,Min(Size.X+1,255));
      {locate beginning of file name, skip path}
      N:=Length(S);
      While (N>2) do
      begin
        if S[N-1] = DirectorySeparator then break;
        dec(N);
      end;
      {compare with Key byte by byte}
      for z:=1 to Length(AKey^) do
      begin
        if Upcase(S[n-1+z])<>Upcase(AKey^[z]) then break;
        if z = Length(AKey^) then
        begin
          nX:=N; { mark Cursor.X start position }
          AValue:=k;
          Found:=true;
        end;
      end;
      if Found then break;
    end;
    UnsortedListSearch:=Found;
  end;

begin
  OldValue := Focused;
  T:=false;
  if not ((Event.What = evKeyDown) and (Event.CharCode=' ')) then {skip space key}
    Inherited HandleEvent(Event);
  if (OldValue <> Focused) or
     ((Event.What = evBroadcast) and (Event.InfoPtr = @Self) and
      (Event.Command = cmReleasedFocus)) then
  begin
    SearchPos := 0;
    CurString:='';
    HideCursor; { no search, no cursor shown }
  end;
  if Event.What = evKeyDown then
  begin
    { patched to prevent error when no or empty list or Escape pressed }
    if (not (Event.CharCode in SpecialChars)) and
       (List <> nil) and (List^.Count > 0) then
    begin
      Value := Focused;
      ShowCursor;  {Show cursor only when in search mode }
      OldPos := SearchPos;
      if Event.KeyCode = kbBack then
        begin
          if SearchPos = 0 then Exit;
          Dec(SearchPos);
          OldPos:=SearchPos;
          SetLength(CurString,SearchPos);
        end
      else
        begin
          Inc(SearchPos);
          SetLength(CurString,SearchPos);
          CurString[SearchPos] := Event.CharCode;
        end;

      K := GetKey(CurString);
      T := UnsortedListSearch( K, Value, nX);

      if CurString='' then
        SetCursor(2, Cursor.Y);
      if Value < Range then
      begin
          if Value < Range then
            NewString := GetText(Value, 255)
          else
            NewString := '';
          if T then
          begin
            if Value <> OldValue then
              FocusItem(Value);
            SetCursor({Cursor.X}nX+SearchPos, Cursor.Y);
          end
            else
              SearchPos := OldPos;
      end
        else SearchPos := OldPos;

      if (SearchPos <> OldPos) or (Event.CharCode in [#08,'0'..'1','A'..'Z','a'..'z']) then
         ClearEvent(Event);
    end;
  end;

  if not T and (Event.What = evKeyDown) and (Event.CharCode=' ') then
    Inherited HandleEvent(Event); {now handel space key}
end;

function TWindowListBox.GetKey(var S: String): Pointer;
begin
//{$ifdef FV_UNICODE}
//  GetKey := Pointer(S);
//{$else}
  GetKey := @S;
//{$endif}
end;

procedure TWindowListBox.NewList(AList: PCollection);
begin
  inherited NewList(AList);
  SearchPos:=0;
  CurString:='';
end;

constructor TWindowListDialog.Init;
var R,R2: TRect;
    SB: PScrollBar;
    Lab : PLabel;
    Btn : PButton;
begin
  R.Assign(0,0,Max(60,round(ScreenWidth*5/8)),Max(14,round(ScreenHeight*3/5)));
  inherited Init(R, dialog_windowlist);
  GrowMode := gfGrowAll + gfGrowRel;                 { Set window growmodes }
  HelpCtx:=hcWindowList;
  New(C, Init(20,10));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.X:=R.B.X-14;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(LB, Init(R, SB));
  LB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  LB^.Default:=true;
  LB^.NewList(C);
  UpdateList;
  if C^.Count>=2 then
   if PWindow(C^.At(1))^.GetState(sfVisible) then
    LB^.FocusItem(1); { focus the 2nd one }
  Insert(LB);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Lab:=New(PLabel, Init(R2, label_wndlist_windows, LB));
  Lab^.GrowMode:=gfGrowHiX;
  Insert(Lab);

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.A.X:=R.B.X-13+1; R.B.Y:=R.A.Y+2;
  Btn:=New(PButton, Init(R, button_OK, cmOK, bfDefault));
  Btn^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(Btn);
  R.Move(0,2);
  Btn:=New(PButton, Init(R, button_Close, cmDeleteItem, bfNormal));
  Btn^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(Btn);
  R.Move(0,2);
  New(BtnShow, Init(R, button_Show, cmShowItem, bfNormal));
  BtnShow^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(BtnShow);
  R.Move(0,2);
  New(BtnHide, Init(R, button_Hide, cmHideItem, bfNormal));
  BtnHide^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(BtnHide);
  R.Move(0,2);
  Btn:=New(PButton, Init(R, button_Cancel, cmCancel, bfNormal));
  Btn^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(Btn);

  LB^.Select;
  PutCommand(@Self,evBroadcast,cmListFocusChanged,LB);
end;

procedure TWindowListDialog.SizeLimits (Var Min, Max: TPoint);
begin
  Min.X:=60;
  Min.Y:=14;
  Max.X:=WUtils.Max(60,((ScreenWidth*5) div 8)+1);
  Max.Y:=WUtils.Max(14,((ScreenHeight*3) div 5)+1);
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
  VisState:=true; Desktop^.ForEach(@AddIt); { add visible windows to list }
  VisState:=false; Desktop^.ForEach(@AddIt); { add hidden windows }
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

