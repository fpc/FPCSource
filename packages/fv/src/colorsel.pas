{
   This unit is part of the Free Vision package

   Copyright 2008,2024 by Marco van de Voort, Andreas Jakobsche and Margers

   Color select dialog.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.

 ****************************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit ColorSel;
{$ENDIF FPC_DOTTEDUNITS}
{====Include file to sort compiler platform out =====================}
{$I platform.inc}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Objects, FreeVision.Drivers, FreeVision.Dialogs, FreeVision.Views, 
  FreeVision.Fvconsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  Objects, Drivers, Dialogs, Views, fvconsts;
{$ENDIF FPC_DOTTEDUNITS}

const dialog_colorsel_colors        = 'Colors';
      label_colorsel_group          = '~G~roup';
      label_colorsel_item           = '~I~tem';
      label_colorsel_foreground     = '~F~oreground';
      label_colorsel_background     = '~B~ackground';
      label_colorsel_displaytext    = 'Text Text Text';

      label_colors_framepassive     = 'Frame passive';
      label_colors_frameactive      = 'Frame active';
      label_colors_frameicon        = 'Frame icons';
      label_colors_scrollbarpage    = 'Scroll bar page';
      label_colors_scrollbaricons   = 'Scroll bar icons';
      label_colors_normaltext       = 'Normal text';
      label_colors_selectedtext     = 'Selected text';
      label_colors_activeitem       = 'Active item';
      label_colors_inactiveitem     = 'Inactive item';
      label_colors_focuseditem      = 'Focused item';
      label_colors_selecteditem     = 'Selected item';
      label_colors_divider          = 'Divider';
      label_colors_normal           = 'Normal';
      label_colors_selected         = 'Selected';
      label_colors_disabled         = 'Disabled';
      label_colors_shortcut         = 'Shortcut';
      label_colors_selecteddisabled = 'Selected disabled';
      label_colors_shortcutselected = 'Shortcut selected';
      label_colors_color            = 'Color';
      label_colors_framebackground  = 'Frame/background';
      label_colors_statictext       = 'Static text';
      label_colors_normallabel      = 'Label normal';
      label_colors_selectedlabel    = 'Label selected';
      label_colors_shortcutlabel    = 'Label shortcut';
      label_colors_normalbutton     = 'Button normal';
      label_colors_defaultbutton    = 'Button default';
      label_colors_selectedbutton   = 'Button selected';
      label_colors_disabledbutton   = 'Button disabled';
      label_colors_buttonshadow     = 'Button shadow';
      label_colors_shortcutbutton   = 'Button shortcut';
      label_colors_normalcluster    = 'Cluster normal';
      label_colors_selectedcluster  = 'Cluster selected';
      label_colors_shortcutcluster  = 'Cluster shortcut';
      label_colors_normalinput      = 'Input normal';
      label_colors_selectedinput    = 'Input selected';
      label_colors_inputarrow       = 'Input arrow';
      label_colors_historybutton    = 'History button';
      label_colors_historysides     = 'History sides';
      label_colors_historybarpage   = 'History bar page';
      label_colors_historybaricon   = 'History bar icons';
      label_colors_normallist       = 'List normal';
      label_colors_selectedlist     = 'List selected';
      label_colors_focusedlist      = 'List focused';
      label_colors_listdivider      = 'List divider';
      label_colors_infopane         = 'Information pane';

type
  TColorSel = (csBackground, csForeground);

  PColorItem = ^TColorItem;
  TColorItem = record
    Name: PString;
    Index: Byte;
    Next: PColorItem;
  end;

  PColorGroup = ^TColorGroup;
  TColorGroup = record
    Name: PString;
    Index: Byte;
    Items: PColorItem;
    Next: PColorGroup;
  end;

  PColorIndex = ^TColorIndex;
  TColorIndex = record
    GroupIndex: Byte;
    ColorSize: Byte;
    ColorIndex: array[0 .. 255] of Byte;
  end;

  PColorGroupList = ^TColorGroupList;
  TColorGroupList = object(TListViewer)
    Groups: PColorGroup;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar; AGroups: PColorGroup);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure FocusItem(Item: Sw_Integer); virtual;
    function GetText(Item: Sw_Integer; MaxLen: Sw_Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetGroupIndex(GroupNum, ItemNum: Byte);
    function GetGroup(GroupNum: Byte): PColorGroup;
    function GetGroupIndex(GroupNum: Byte): Byte;
    function GetNumGroups: byte;
    destructor Done; Virtual;
  end;

  PColorItemList = ^TColorItemList;
  TColorItemList = object(TListViewer)
    Items: PColorItem;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar; AItems: PColorItem);
    procedure FocusItem(Item: Sw_Integer); virtual;
    function GetText(Item: Sw_Integer; MaxLen: Sw_Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetItems(AItems: PColorItem);
    function GetColorIndex(Item: Sw_integer): Sw_integer;
  end;

  PColorSelector = ^TColorSelector;
  TColorSelector = object(TView)
    Color: Byte;
    SelType: TColorSel;
    constructor Init(var Bounds: TRect; ASelType: TColorSel);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; Virtual;
  end;

  PColorDisplay = ^TColorDisplay;
  TColorDisplay = object(TView)
    Color: PByte;
    Text: PString;
    constructor Init(var Bounds: TRect; AText: PString);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetColor(var AColor: Byte); virtual;
    procedure Draw; Virtual;
    destructor Done; Virtual;
  end;

  PColorDialog = ^TColorDialog;
  TColorDialog = object(TDialog)
    BakLabel: PLabel;
    BakSel: PColorSelector;
    ForLabel: PLabel;
    ForSel: PColorSelector;
    Groups: PColorGroupList;
    GroupIndex: Byte;
    Items: PColorItemList;
    Display: PColorDisplay;
    Pal: TPalette;
    constructor Init(APalette: TPalette; AGroups: PColorGroup);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent (var Event: TEvent); virtual;
    function DataSize: sw_word; virtual;
    procedure GetData (var Rec); virtual;
    procedure SetData (var Rec); virtual;
    {procedure GetIndexes (var Colors: PColorIndex);
    procedure SetIndexes (var Colors: PColorIndex);}
    private
    procedure ItemChanged;
  end;


const ColorIndexes : PColorIndex = nil;

procedure RegisterColorsel;

function ColorGroup(Name: string; Items: PColorItem; Next: PColorGroup): PColorGroup;
function ColorItem(Name: string; Index: Byte; Next: PColorItem): PColorItem;

procedure LoadIndexes(var S: TStream);
procedure StoreIndexes(var S: TStream);

function WindowColorItems (Palette: Sw_Word; const Next: PColorItem): PColorItem;
function DialogColorItems (Palette: Sw_Word; const Next: PColorItem): PColorItem;
function MenuColorItems(const Next: PColorItem): PColorItem;
function DesktopColorItems(const Next: PColorItem): PColorItem;


{--------------------------------------------------------------------}
{                      implementation                                }
{--------------------------------------------------------------------}
implementation

const
  RColorItemList: TStreamRec = (
     ObjType: idColorItemList;
     VmtLink: Ofs(TypeOf(TColorItemList)^);
     Load:    @TColorItemList.Load;
     Store:   @TColorItemList.Store
  );
  RColorGroupList: TStreamRec = (
     ObjType: idColorGroupList;
     VmtLink: Ofs(TypeOf(TColorGroupList)^);
     Load:    @TColorGroupList.Load;
     Store:   @TColorGroupList.Store
  );
  RColorSelector: TStreamRec = (
     ObjType: idColorSelector;
     VmtLink: Ofs(TypeOf(TColorSelector)^);
     Load:    @TColorSelector.Load;
     Store:   @TColorSelector.Store
  );
  RColorDisplay: TStreamRec = (
     ObjType: idColorDisplay;
     VmtLink: Ofs(TypeOf(TColorDisplay)^);
     Load:    @TColorDisplay.Load;
     Store:   @TColorDisplay.Store
  );
  RColorDialog: TStreamRec = (
     ObjType: idColorDialog;
     VmtLink: Ofs(TypeOf(TColorDialog)^);
     Load:    @TColorDialog.Load;
     Store:   @TColorDialog.Store
  );

procedure RegisterColorsel;
begin
 // according to help should register TColorSelector,     TMonoSelector, TColorDisplay, TColorGroupList, TColorItemList,     TColorDialog
 // probably don't bother with the mono variants. Except for (P/T)colordialog, these don't grep in FV/IDE src.

 // TColorSelector -> the square colorselection widget (instantiated twice once for front, once for back?)
 // TColorGrouplist-> the selection of the color group (left list)  (TListbox or whatever the TV eq is?)
 // TColorItemList -> the selection of the color identifier (right list)  (TListbox or whatever the TV eq is?)

 RegisterType(RColorItemList);
 RegisterType(RColorGroupList);
 RegisterType(RColorSelector);
 RegisterType(RColorDisplay);
 RegisterType(RColorDialog);
end;


function ColorGroup(Name: string; Items: PColorItem; Next: PColorGroup): PColorGroup;
var
  R: PColorGroup;
begin
  New(R);
  R^.Name := NewStr(Name);
  R^.Items := Items;
  R^.Next := Next;
  ColorGroup := R;
end;

function ColorItem(Name: string; Index: Byte; Next: PColorItem): PColorItem;
var R: PColorItem;
begin
  New(R);
  R^.Name := NewStr(Name);
  R^.Index := Index;
  R^.Next := Next;
  ColorItem := R
end;

procedure LoadIndexes(var S: TStream);
var A: TColorIndex;
begin
  fillchar(A,sizeof(A),0);
  S.Read(A.GroupIndex, SizeOf(A.GroupIndex));
  S.Read(A.ColorSize, SizeOf(A.ColorSize));
  if A.ColorSize > 0 then
    S.Read(A.ColorIndex, SizeOf(A.ColorSize));
  if ColorIndexes<>nil then ColorIndexes^:=A;
end;

procedure StoreIndexes(var S: TStream);
var A: TColorIndex;
begin
  fillchar(A,sizeof(A),0);
  if ColorIndexes<>nil then A:=ColorIndexes^;
  S.Write(A.GroupIndex, SizeOf(A.GroupIndex));
  S.Write(A.ColorSize, SizeOf(A.ColorSize));
  if A.ColorSize>0 then
    S.Write(A.ColorIndex, SizeOf(A.ColorSize));
end;

{--------------------------------------------------------------------}
{                      TColorSelector                                }
{--------------------------------------------------------------------}

constructor TColorSelector.Init(var Bounds: TRect; ASelType: TColorSel);
begin
  inherited init(Bounds);
  Options:=Options or ofSelectable or ofFirstClick or ofFramed;
  EventMask := EventMask OR evBroadcast;
  SelType:=ASelType;
end;

constructor TColorSelector.Load(var S: TStream);
begin
  inherited Load(S);
  S.Read(Color, SizeOf(Color));
  S.Read(SelType, SizeOf(SelType));
end;

procedure TColorSelector.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(Color, SizeOf(Color));
  S.Write(SelType, SizeOf(SelType));
end;

procedure TColorSelector.HandleEvent(var Event: TEvent);
var newColor,oldColor : sw_integer;
   sColor : byte;
   NeedClear : boolean;
   Mouse: TPoint;
   Mx : sw_integer;
   n15, n11, n16 : sw_integer;
begin
  inherited HandleEvent(Event);
  NeedClear:=false;
  oldColor:=Color;
  newColor:=Color;
  Case Event.What Of
    evKeyDown: Begin                                 { Key down event }
      NeedClear:=true;
      n15:=15; n11:=11; n16:=16;
      if (SelType = csBackground) then
      begin
        n15:=7; n11:=3; n16:=8;
      end;
      Case CtrlToArrow(Event.KeyCode) Of
         kbUp: begin
           newColor:=newColor-4;
           if newColor<0 then
           begin
              newColor:=(newColor+n15) ;
              if NewColor=n11 then NewColor:=n15;
           end;
         end;
         kbDown: begin
           newColor:=newColor+4;
           if newColor>=n16 then
           begin
             newColor:=(newColor+1) - n16;
             if newColor=4 then newColor:=0;
           end;
         end;
         kbRight: begin newColor:=newColor+1; if newColor>=n16 then newColor:=0; end;
         kbLeft: begin newColor:=newColor-1; if newColor=-1 then newColor:=n15; end;
      otherwise
        NeedClear:=false;
      end;
    end;
    evMouseDown: Begin                               { Mouse down event }
      if (Event.Buttons=mbLeftButton ) and MouseInView(Event.Where) Then
      begin
        NeedClear:=true;
        MakeLocal(Event.Where, Mouse);               { Localize mouse }
        mx:=Mouse.X;
        if (mx>=0) and (mx<=12) then
          if ((Mouse.Y >= 0) and (Mouse.Y <= 3) and (SelType = csForeground))
          or ((Mouse.Y >= 0) and (Mouse.Y <= 1) and (SelType = csBackground)) then
          begin
            mx:=mx div 3;
            newColor:=(Mouse.Y)*4+mx;
         end;
      end;
    end;
    evBroadcast: Begin                               { Broadcast event }
      case Event.Command of
        cmColorSet:begin
          if SelType = csForeground then
            sColor:= Event.InfoByte and $0f
          else
            sColor:= (Event.InfoByte shr 4) and $0f;
          if Color<>sColor then
          begin
            Color:=sColor;
            DrawView;
          end;
        end;
      end;
    End;
  end;
  if NeedClear then
    ClearEvent(Event);
  if oldColor<>newColor then
  begin
    Color:=newColor;
    if SelType = csForeground then          {intended type cast byte to pointer}
      Message(Owner, evBroadcast, cmColorForegroundChanged, pointer(byte(Color)))
    else
      Message(Owner, evBroadcast, cmColorBackgroundChanged, pointer(byte(Color)));
    Owner^.DrawView;
  end;
end;

procedure TColorSelector.Draw;
var
  FrameColor : Byte;
  SelColor,CurColor : Byte;
  Frame : AnsiChar;
  B     : TDrawBuffer;

procedure DrawColorLine (LineNr : sw_word);
var k: sw_integer;
begin
  for k:=0 to 3 do
  begin
    MoveChar (B[0+k*3], #219, SelColor, 3);
    if CurColor = Color then
    begin
      if ((k = 0) or (k = 1)) and (CurColor<2) then
        SelColor:=SelColor or $70
      else
        SelColor:=SelColor and $f;
      MoveChar (B[0+k*3+1], #8, SelColor, 1);
      SelColor:=SelColor or $10;
      SelColor:=SelColor and $1f;
    end;
    inc(CurColor);
    inc(SelColor,1);
  end;
  WriteBuf (0, LineNr, Size.X, 1, B);
end;

begin
  FrameColor := GetColor (2);
  MoveChar (B, ' ', FrameColor, Size.X);
  SelColor:=$10;
  CurColor:=0;
  DrawColorLine(0);
  DrawColorLine(1);
  if SelType = csForeground then
  begin
    DrawColorLine(2);
    DrawColorLine(3);
  end;
end;

{--------------------------------------------------------------------}
{                      TColorDisplay                                 }
{--------------------------------------------------------------------}

constructor TColorDisplay.Init(var Bounds: TRect; AText: PString);
begin
  inherited init(Bounds);
  EventMask := EventMask OR evBroadcast;
  Text:=AText;
end;

constructor TColorDisplay.Load(var S: TStream);
begin
  inherited Load(S);
  if not Assigned(Color) then
    GetMem(Color,1);
  S.Read(Color^, SizeOf(Color^));
  Text:=S.ReadStr;
end;

procedure TColorDisplay.Store(var S: TStream);
var vColor : byte;
begin
  inherited Store(S);
  vColor:=0;
  if Assigned(Color) then vColor:=Color^;
  S.Write(vColor, SizeOf(vColor));
  S.WriteStr(Text);
end;

procedure TColorDisplay.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  Case Event.What Of
    evBroadcast:                                { Broadcast event }
      case Event.Command of
        cmColorForegroundChanged:if assigned(Color) then begin
          Color^:=(Color^ and  $f0) or (Event.InfoByte and $0f);
          DrawView;
          ClearEvent(Event);                             { Event was handled }
        end;
        cmColorBackgroundChanged:if assigned(Color) then begin
          Color^:= (Color^ and $0f) or((Event.InfoByte shl 4) and $f0);
          DrawView;
          ClearEvent(Event);                             { Event was handled }
        end;
      end;
  end;
end;

procedure TColorDisplay.SetColor(var AColor: Byte);
begin
  Color:=@AColor;
  Message(Owner, evBroadcast, cmColorSet, pointer(byte(Color^))); {intended type cast byte to pointer}
  DrawView;
end;

procedure TColorDisplay.Draw;
var
  FColor : Byte;
  Y      : sw_integer;
  S      : String;
  B      : TDrawBuffer;
begin
  if Assigned(Color) then
  begin
    FColor:=Color^;
    if FColor = 0 then
       FColor:=128;
  end else
    FColor := GetColor (2);
  MoveChar (B, ' ', FColor, Size.X);
  if Assigned(Text) then
    S:=Text^
  else
    S:='';
  if length(S) < Size.X then
    MoveStr (B[(Size.X-length(S)) div 2], S, FColor)
  else
    MoveStr (B, S, FColor);
  if Size.Y > 0 then
    for Y :=0 to Size.Y-1 do
    begin
      WriteBuf (0, Y, Size.X, 1, B);
    end;
end;

destructor TColorDisplay.Done;
begin
  if assigned(Text) then
  begin
    Dispose(Text);
    Text:=nil;
  end;
  inherited Done;
end;

{--------------------------------------------------------------------}
{                      TColorGroupList                               }
{--------------------------------------------------------------------}

constructor TColorGroupList.Init(var Bounds: TRect; AScrollBar: PScrollBar; AGroups: PColorGroup);
var
  x: PColorGroup;
begin
  inherited Init(Bounds, 1, nil, AScrollBar);
  EventMask := EventMask OR evBroadcast;
  Range := 0;
  Groups := AGroups;
  x := AGroups;
  while Assigned(x) do begin
    x^.Index:=0;
    Inc(Range);
    x := x^.Next
  end;
  SetRange(Range);
end;

constructor TColorGroupList.Load(var S: TStream);
var x,z: PColorGroup;
  R,Q: PColorItem;
  num,numItems,iG,iI : word;
begin
  inherited Load(S);
  S.Read(num, SizeOf(num));
  Groups:=nil;
  { read PColorGroup linked list }
  z:=nil;
  for iG:=1 to num do
  begin
    S.Read(numItems, SizeOf(numItems));
    new(x);
    x^.Items:=nil;
    Q:=nil;
    {read items}
    for iI:=1 to numItems do
    begin
      New(R);
      R^.Name:=S.ReadStr;
      S.Read(R^.Index, SizeOf(R^.Index));
      R^.Next:=nil;
      if assigned(Q) then
        Q^.Next:=R;
      Q:=R;
      if iI=1 then x^.Items:=R;
    end;
    {read group}
    x^.Name:=S.ReadStr;
    S.Read(x^.Index, SizeOf(x^.Index));
    x^.Next:=nil;
    if assigned(z) then
      z^.Next:=x;
    z:=x;
    if iG = 1 then
      Groups:=x; { Group starts with first entry }
  end;
end;

procedure TColorGroupList.Store(var S: TStream);
var x,z: PColorGroup;
  R,Q: PColorItem;
  num,numItems : word;
begin
  inherited Store(S);
  num:=GetNumGroups;
  S.Write(num,Sizeof(num));
  x := Groups;
  {Write PColorGroup linked list}
  while Assigned(x) do begin
    R:=x^.Items;
    {count items}
    Q:=R;
    numItems:=0;
    while Assigned(Q) do begin
      inc(numItems);
      Q:=Q^.Next;
    end;
    S.Write(numItems,Sizeof(numItems)); {  write Item count }
    {write items}
    while Assigned(R) do begin
      S.WriteStr(R^.Name);
      S.Write(R^.Index,Sizeof(R^.Index));
      R := R^.Next;
    end;
    {write gropu}
    S.WriteStr(x^.Name);
    S.Write(x^.Index,Sizeof(x^.Index));
    x := x^.Next;
  end;
end;

procedure TColorGroupList.FocusItem(Item: Sw_Integer);
var oFocus : sw_integer;
begin
  oFocus:=Focused;
  inherited FocusItem(Item);
  if Item < Range then
  begin
    if oFocus<>Focused then
      Message(Owner, evBroadcast, cmNewColorItem, @Self);{ Send message }
  end;
end;

function TColorGroupList.GetText(Item: Sw_Integer; MaxLen: Sw_Integer): String;
var x: PColorGroup;
    Num : sw_integer;
begin
  GetText:='';
  x := Groups;
  Num:=0;
  while Assigned(x) do begin
    if Num = Item then
    begin
      GetText:=x^.Name^;
      exit;
    end;
    inc(Num);
    x := x^.Next;
  end;
end;

procedure TColorGroupList.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  Case Event.What Of
    evBroadcast:                                { Broadcast event }
      case Event.Command of
        cmSaveColorIndex: begin
          SetGroupIndex(Focused,Event.InfoByte);
          ClearEvent(Event);                    { Event was handled }
        end;
      end;
  end;
end;

procedure TColorGroupList.SetGroupIndex(GroupNum, ItemNum: Byte);
var x: PColorGroup;
begin
  x:=GetGroup(GroupNum);
  if Assigned(x) then
    x^.Index:=ItemNum;
end;

function TColorGroupList.GetGroup(GroupNum: Byte): PColorGroup;
var x: PColorGroup;
    Num : sw_integer;
begin
  GetGroup:=nil;
  x := Groups;
  Num:=0;
  while Assigned(x) do begin
    if Num = GroupNum then
    begin
      GetGroup:=x;
      exit;
    end;
    inc(Num);
    x := x^.Next;
  end;
end;

function TColorGroupList.GetGroupIndex(GroupNum: Byte): Byte;
var x: PColorGroup;
begin
  GetGroupIndex:=0;
  x:=GetGroup(GroupNum);
  if Assigned(x) then
    GetGroupIndex:=x^.Index;
end;

function TColorGroupList.GetNumGroups: byte;
var x: PColorGroup;
    Num : sw_integer;
begin
  x := Groups;
  Num:=0;
  while Assigned(x) do begin
    inc(Num);
    x := x^.Next;
  end;
  GetNumGroups:=Num;
end;

destructor TColorGroupList.Done;
var x,z: PColorGroup;
  R,Q: PColorItem;
begin
  x := Groups;
  inherited Done;
  Groups:=nil;
  {Disopse PColorGroup linked list}
  while Assigned(x) do begin
    R:=x^.Items;
    while Assigned(R) do begin
      Dispose(R^.Name);
      Q:=R;
      R := R^.Next;
      Dispose(Q);
    end;
    Dispose(x^.Name);
    z:=x;
    x := x^.Next;
    Dispose(z);
  end;
end;

{--------------------------------------------------------------------}
{                      TColorItemList                                }
{--------------------------------------------------------------------}

constructor TColorItemList.Init(var Bounds: TRect; AScrollBar: PScrollBar; AItems: PColorItem);
begin
  inherited Init(Bounds, 1, nil, AScrollBar);
  EventMask := EventMask OR evBroadcast;
  SetItems(AItems);
end;

procedure TColorItemList.FocusItem(Item: Sw_Integer);
var oFocus:sw_integer;
begin
  oFocus:=Focused;
  inherited FocusItem(Item);
  if Item < Range then
  begin
    if oFocus<>Focused then
    begin
      {one for TColorGroupList and other for TColorDialog }
      Message(Owner, evBroadcast, cmSaveColorIndex, pointer(byte(Focused)));{intended type cast byte to pointer}
      Message(Owner, evBroadcast, cmNewColorIndex, @Self);{ Send message }
    end;
  end;
end;

function TColorItemList.GetText(Item: Sw_Integer; MaxLen: Sw_Integer): String;
var count : sw_integer;
    x :PColorItem;
begin
  GetText:= '';
  count:=0;
  if Assigned(Items) then
  begin
    x:=Items;
    while Assigned(x) do
    begin
      if count=Item then
      begin
        GetText:=x^.Name^;
        {SetLength(GetText,Min(Length(GetText),MaxLen));}
        exit;
      end;
      inc(count);
      x:=x^.Next;
    end;
  end;
end;

procedure TColorItemList.HandleEvent(var Event: TEvent);
var Groups : PColorGroupList;
    x:PColorGroup;
    GroupIndex,ItemIndex:sw_integer;
begin
  inherited HandleEvent(Event);
  Case Event.What Of
    evBroadcast: Begin                               { Broadcast event }
      case Event.Command of
        cmNewColorItem: begin
          Groups:=PColorGroupList(Event.Infoptr);
          GroupIndex:=Groups^.Focused;
          ItemIndex:=Groups^.GetGroupIndex(GroupIndex);
          x:=Groups^.GetGroup(GroupIndex);
          if assigned(x) then
            SetItems(x^.Items)
          else SetItems(nil);
          FocusItem(ItemIndex);
          DrawView;
          ClearEvent(Event);                             { Event was handled }
        end;
      end;
    end;
  end;
end;

procedure TColorItemList.SetItems(AItems: PColorItem);
var count : sw_integer;
    x :PColorItem;
begin
  Items:=AItems;
  Focused:=-1;
  count:=0;
    x:=Items;
    while Assigned(x) do
    begin
      inc(count);
      x:=x^.Next;
    end;
  SetRange(count);
end;

function TColorItemList.GetColorIndex(Item: Sw_integer): Sw_integer;
var count : sw_integer;
    x :PColorItem;
begin
  GetColorIndex:=0;
  count:=0;
  if Assigned(Items) then
  begin
    x:=Items;
    while Assigned(x) do
    begin
      if count=Item then
      begin
        GetColorIndex:=x^.Index;
        exit;
      end;
      inc(count);
      x:=x^.Next;
    end;
  end;
end;

{--------------------------------------------------------------------}
{                      TColorDialog                                  }
{--------------------------------------------------------------------}

constructor TColorDialog.Init(APalette: TPalette; AGroups: PColorGroup);
var
  R,R2: TRect;
  OkButton,CancelButton : PButton;
  VScrollBar: PScrollBar;
  x:PColorGroup;
  xItems: PColorItem;
begin
  R.Assign(0, 0, 62, 19);
  inherited Init(R, dialog_colorsel_colors);
  Options := Options or ofCentered;
  EventMask := EventMask OR evBroadcast;             { Set event mask }
  Pal := APalette;
  {-- Groups list --}
  R.Assign(3, 3, 18, 15);
  R2.Copy(R); R2.A.X:=R2.B.X; Inc(R2.B.X);
  VScrollBar := New(PScrollBar, Init(R2));
  Insert(VScrollBar);
  Groups := New(PColorGroupList, Init(R, VScrollBar, AGroups));
  Insert(Groups);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_colorsel_group, Groups)));
  {-- Item list --}
  GroupIndex:=Groups^.Focused;
  x:=Groups^.GetGroup(GroupIndex);
  if assigned(x) then
    xItems:=x^.Items
  else xItems:=nil;
  R.Assign(21, 3, 42, 15);
  R2.Copy(R); R2.A.X:=R2.B.X; Inc(R2.B.X);
  VScrollBar := New(PScrollBar, Init(R2));
  Insert(VScrollBar);
  Items := New(PColorItemList, Init(R, VScrollBar,xItems));
  Insert(Items);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_colorsel_item, Items)));
  {-- Color selector foreground --}
  R.Assign(46, 3, 58, 7);
  R2.Copy(R); R2.B.Y:=R2.A.Y; Dec(R2.A.Y);
  ForSel:=New(PColorSelector, Init(R, csForeground));
  ForLabel:=New(PLabel, Init(R2, label_colorsel_foreground, ForSel));
  Insert(ForSel);
  Insert(ForLabel);
  {-- Color selector background --}
  R.Assign(46, 9, 58, 11);
  R2.Copy(R); R2.B.Y:=R2.A.Y; Dec(R2.A.Y);
  BakSel:=New(PColorSelector, Init(R, csBackground));
  BakLabel:=New(PLabel, Init(R2, label_colorsel_background, BakSel));
  Insert(BakSel);
  Insert(BakLabel);
  {-- Color Display --}
  R.Assign(45, 12, 59, 15);
  Display:=New(PColorDisplay,Init(R,NewStr(label_colorsel_displaytext)));
  Insert(Display);
  {-- Buttons --}
  R.Assign(37, 16, 47, 18);
  OkButton := New(PButton, Init(R, slOk, cmOK, bfDefault));
  Insert(OkButton);
  Inc(R.A.X,12); Inc(R.B.X,12);
  CancelButton := New(PButton, Init(R, slCancel, cmCancel, bfDefault));
  Insert(CancelButton);
  {--set focus--}
  Items^.FocusItem(0);
  SelectNext(False);
end;

constructor TColorDialog.Load(var S: TStream);
begin
  if not TDialog.Load(S) then
    Fail;
  S.Read(GroupIndex, SizeOf(GroupIndex));
  S.Read(Pal, SizeOf(Pal));
  if (S.Status <> stOk) then
  begin
    TDialog.Done;
    Fail;
  end;
  GetSubViewPtr(S,BakLabel);
  GetSubViewPtr(S,BakSel);
  GetSubViewPtr(S,ForLabel);
  GetSubViewPtr(S,ForSel);
  GetSubViewPtr(S,Groups);
  GetSubViewPtr(S,Items);
  GetSubViewPtr(S,Display);
  if assigned(Items) then
    if assigned(Groups) then
      Items^.Items:=Groups^.GetGroup(Groups^.Focused)^.Items;
end;

procedure TColorDialog.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(GroupIndex, SizeOf(GroupIndex));
  S.Write(Pal, SizeOf(Pal));
  PutSubViewPtr(S,BakLabel);
  PutSubViewPtr(S,BakSel);
  PutSubViewPtr(S,ForLabel);
  PutSubViewPtr(S,ForSel);
  PutSubViewPtr(S,Groups);
  PutSubViewPtr(S,Items);
  PutSubViewPtr(S,Display);
end;

procedure TColorDialog.ItemChanged;
const NoItemSelectedColor : byte = $7f;
var ColorIndex,ItemIndex : sw_integer;
begin
  GroupIndex:=Groups^.Focused;
  if Items^.Range > 0 then
    begin
      ItemIndex:=Items^.Focused;
      ColorIndex:=Items^.GetColorIndex(ItemIndex);
      Display^.SetColor(byte(Pal[ColorIndex]));
    end
  else
    Display^.SetColor(NoItemSelectedColor);
end;

procedure TColorDialog.HandleEvent (var Event: TEvent);
begin
  inherited HandleEvent(Event);
  Case Event.What Of
    evBroadcast:                                     { Broadcast event }
      case Event.Command of
        cmNewColorIndex: begin
          ItemChanged;
          ClearEvent(Event);                         { Event was handled }
        end;
      end;
  end;
end;

function TColorDialog.DataSize: sw_Word;
begin
  DataSize:=Sizeof(Pal);
end;

procedure TColorDialog.GetData (var Rec);
begin
  TPalette(Rec):=Pal;
end;

procedure TColorDialog.SetData (var Rec);
begin
  Pal:=TPalette(Rec);
  ItemChanged;
end;

function WindowColorItems (Palette: Sw_Word; const Next: PColorItem): PColorItem;
const
  COffset: array[wpBlueWindow..wpGrayWindow] of Byte =
    (8, 16, 24);
var
  Offset: Word;
begin
  //WindowColorItems:=ColorItem('Normal',12,Next); {place holder}
  Offset := COffset[Palette];
  WindowColorItems :=
    ColorItem(label_colors_framepassive,     Offset + 0,
    ColorItem(label_colors_frameactive,      Offset + 1,
    ColorItem(label_colors_frameicon,        Offset + 2,
    ColorItem(label_colors_scrollbarpage,    Offset + 3,
    ColorItem(label_colors_scrollbaricons,   Offset + 4,
    ColorItem(label_colors_normaltext,       Offset + 5,
    Next))))));
end;

function DialogColorItems(Palette: Sw_Word; const Next: PColorItem): PColorItem;
const
  COffset: array[dpBlueDialog..dpGrayDialog] of Byte =
    (64, 96, 32);
  var
    Offset: Byte;
begin
  //DialogColorItems:=ColorItem('Normal',13,Next); {place holder}
  Offset := COffset[Palette];
  DialogColorItems :=
    ColorItem(label_colors_framebackground,  Offset + 1,
    ColorItem(label_colors_frameicon,        Offset + 2,
    ColorItem(label_colors_scrollbarpage,    Offset + 3,
    ColorItem(label_colors_scrollbaricons,   Offset + 4,
    ColorItem(label_colors_statictext,       Offset + 5,

    ColorItem(label_colors_normallabel,      Offset + 6,
    ColorItem(label_colors_selectedlabel,    Offset + 7,
    ColorItem(label_colors_shortcutlabel,    Offset + 8,

    ColorItem(label_colors_normalbutton,     Offset + 9,
    ColorItem(label_colors_defaultbutton,    Offset + 10,
    ColorItem(label_colors_selectedbutton,   Offset + 11,
    ColorItem(label_colors_disabledbutton,   Offset + 12,
    ColorItem(label_colors_shortcutbutton,   Offset + 13,
    ColorItem(label_colors_buttonshadow,     Offset + 14,

    ColorItem(label_colors_normalcluster,    Offset + 15,
    ColorItem(label_colors_selectedcluster,  Offset + 16,
    ColorItem(label_colors_shortcutcluster,  Offset + 17,

    ColorItem(label_colors_normalinput,      Offset + 18,
    ColorItem(label_colors_selectedinput,    Offset + 19,
    ColorItem(label_colors_inputarrow,       Offset + 20,

    ColorItem(label_colors_historybutton,    Offset + 21,
    ColorItem(label_colors_historysides,     Offset + 22,
    ColorItem(label_colors_historybarpage,   Offset + 23,
    ColorItem(label_colors_historybaricon,   Offset + 24,

    ColorItem(label_colors_normallist,       Offset + 25,
    ColorItem(label_colors_selectedlist,     Offset + 26,
    ColorItem(label_colors_focusedlist,      Offset + 27,
    ColorItem(label_colors_listdivider,      Offset + 28,

    ColorItem(label_colors_infopane,         Offset + 29,
    Next)))))))))))))))))))))))))))));
end;

function MenuColorItems(const Next: PColorItem): PColorItem;
begin
  MenuColorItems :=
    ColorItem(label_colors_normal,2,
    ColorItem(label_colors_disabled,3,
    ColorItem(label_colors_shortcut,4,
    ColorItem(label_colors_selected,5,
    ColorItem(label_colors_selecteddisabled, 6,
    ColorItem(label_colors_shortcutselected, 7,
    Next)))))) ;
end;

function DesktopColorItems(const Next: PColorItem): PColorItem;
begin
  DesktopColorItems := ColorItem(label_colors_color,1,Next);
end;

end.

{

fvconsts.pas:  idColorSelector = 92;
fvconsts.pas:  idMonoSelector = 93;

}
