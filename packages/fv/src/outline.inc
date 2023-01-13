{
    This file is part of the Free Vision package

    Outline viewer

    Copyright (c) 1999-2022 by Peter Vreman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$H-}
{$ifdef FV_UNICODE}
unit uoutline;
{$else FV_UNICODE}
unit outline;
{$endif FV_UNICODE}

{$CODEPAGE cp437}

{***************************************************************************}
                                  interface
{***************************************************************************}

{$ifdef FV_UNICODE}
uses  udrivers,objects,uviews;
{$else FV_UNICODE}
uses  drivers,objects,views;
{$endif FV_UNICODE}

type  Pnode=^Tnode;
      Tnode=record
        next:Pnode;
{$ifdef FV_UNICODE}
        text:UnicodeString;
{$else FV_UNICODE}
        text:Pstring;
{$endif FV_UNICODE}
        childlist:Pnode;
        expanded:boolean;
      end;

      Poutlineviewer=^Toutlineviewer;
      Toutlineviewer=object(Tscroller)
        foc:sw_integer;
        constructor init(var bounds:Trect;
                         AHscrollbar,AVscrollbar:Pscrollbar);
        procedure adjust(node:pointer;expand:boolean);virtual;
{$ifdef FV_UNICODE}
        function creategraph(level:SmallInt;lines:longint;
                             flags:word;levwidth,endwidth:SmallInt;
                             const chars:UnicodeString):UnicodeString;
{$else FV_UNICODE}
        function creategraph(level:SmallInt;lines:longint;
                             flags:word;levwidth,endwidth:SmallInt;
                             const chars:string):string;
{$endif FV_UNICODE}
        procedure draw;virtual;
        procedure expandall(node:pointer);
        function firstthat(test:codepointer):pointer;
        procedure focused(i:sw_integer);virtual;
        procedure foreach(action:codepointer);
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
{$ifdef FV_UNICODE}
        function getgraph(level:SmallInt;lines:longint;flags:word):UnicodeString;
{$else FV_UNICODE}
        function getgraph(level:SmallInt;lines:longint;flags:word):string;
{$endif FV_UNICODE}
        function getnode(i:sw_integer):pointer;virtual;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getpalette:Ppalette;virtual;
        function getroot:pointer;virtual;
{$ifdef FV_UNICODE}
        function gettext(node:pointer):UnicodeString;virtual;
{$else FV_UNICODE}
        function gettext(node:pointer):string;virtual;
{$endif FV_UNICODE}
        procedure handleevent(var event:Tevent);virtual;
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        function isselected(i:sw_integer):boolean;virtual;
        procedure selected(i:sw_integer);virtual;
        procedure setstate(Astate:word;enable:boolean);virtual;
        procedure update;
      private
        procedure set_focus(Afocus:sw_integer);
        function do_recurse(action:codepointer;callerframe:pointer;
                            stop_if_found:boolean):pointer;
      end;

      Poutline=^Toutline;
      Toutline=object(Toutlineviewer)
        root:Pnode;
        constructor init(var bounds:Trect;
                         AHscrollbar,AVscrollbar:Pscrollbar;
                         Aroot:Pnode);
        procedure adjust(node:pointer;expand:boolean);virtual;
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getroot:pointer;virtual;
{$ifdef FV_UNICODE}
        function gettext(node:pointer):UnicodeString;virtual;
{$else FV_UNICODE}
        function gettext(node:pointer):string;virtual;
{$endif FV_UNICODE}
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        destructor done;virtual;
      end;

const ovExpanded = $1;
      ovChildren = $2;
      ovLast     = $4;

      Coutlineviewer=Cscroller+#8#8;

{$ifdef FV_UNICODE}
function newnode(const Atext:UnicodeString;Achildren,Anext:Pnode):Pnode;
{$else FV_UNICODE}
function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;
{$endif FV_UNICODE}
procedure disposenode(node:Pnode);


{***************************************************************************}
                                implementation
{***************************************************************************}

type TMyFunc = function(_EBP: Pointer; Cur: Pointer;
                        Level, Position: sw_integer; Lines: LongInt;
                        Flags: Word): Boolean;


{$ifdef FV_UNICODE}
function newnode(const Atext:UnicodeString;Achildren,Anext:Pnode):Pnode;
{$else FV_UNICODE}
function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;
{$endif FV_UNICODE}

begin
  newnode:=new(Pnode);
  with newnode^ do
    begin
      next:=Anext;
{$ifdef FV_UNICODE}
      text:=Atext;
{$else FV_UNICODE}
      text:=newstr(Atext);
{$endif FV_UNICODE}
      childlist:=Achildren;
      expanded:=true;
    end;
end;

procedure disposenode(node:Pnode);

var next:Pnode;

begin
  while node<>nil do
    begin
      disposenode(node^.childlist);
{$ifndef FV_UNICODE}
      disposestr(node^.text);
{$endif FV_UNICODE}
      next:=node^.next;
      dispose(node);
      node:=next;
    end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        Toutlineviewer object methods                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor Toutlineviewer.init(var bounds:Trect;
                 AHscrollbar,AVscrollbar:Pscrollbar);

begin
  inherited init(bounds,AHscrollbar,AVscrollbar);
  foc:=0;
  growmode:=gfGrowHiX+gfGrowHiY;
end;

procedure Toutlineviewer.adjust(node:pointer;expand:boolean);

begin
  abstract;
end;

{$ifdef FV_UNICODE}
function TOutlineViewer.CreateGraph(Level: SmallInt; Lines: LongInt;
  Flags: Word; LevWidth, EndWidth: SmallInt;
  const Chars: UnicodeString): UnicodeString;
{$else FV_UNICODE}
function TOutlineViewer.CreateGraph(Level: SmallInt; Lines: LongInt;
  Flags: Word; LevWidth, EndWidth: SmallInt;
  const Chars: String): String;
{$endif FV_UNICODE}
const
  FillerOrBar  = 0;
  YorL         = 2;
  StraightOrTee= 4;
  Retracted    = 6;
var
  Last, Children, Expanded: Boolean;
  I , J : Byte;
{$ifdef FV_UNICODE}
  Graph : UnicodeString;
{$else FV_UNICODE}
  Graph : String;
{$endif FV_UNICODE}

begin
  { Load registers }
  graph:=space(Level*LevWidth+EndWidth+1);

  { Write bar characters }
  J := 1;
  while (Level > 0) do
  begin
    Inc(J);
    if (Lines and 1) <> 0 then
      Graph[J] := Chars[FillerOrBar+2]
    else
      Graph[J] := Chars[FillerOrBar+1];
    for I := 1 to LevWidth - 1 do
      Graph[I]:= Chars[FillerOrBar+1];
    J := J + LevWidth - 1;
    Dec(Level);
    Lines := Lines shr 1;
  end;

  { Write end characters }
  Dec(EndWidth);
  if EndWidth > 0 then
  begin
    Inc(J);
    if Flags and ovLast <> 0 then
      Graph[J] := Chars[YorL+2]
    else
      Graph[J] := Chars[YorL+1];
    Dec(EndWidth);
    if EndWidth > 0 then
    begin
      Dec(EndWidth);
      for I := 1 to EndWidth do
        Graph[I]:= Chars[StraightOrTee+1];
      J := J + EndWidth;
      Inc(J);
      if (Flags and ovChildren) <> 0 then
        Graph[J] := Chars[StraightOrTee+2]
      else
        Graph[J] := Chars[StraightOrTee+1];
    end;
    Inc(J);
    if Flags and ovExpanded <> 0 then
      Graph[J] := Chars[Retracted+2]
    else
      Graph[J] := Chars[Retracted+1];
  end;
  SetLength(Graph, J);

  CreateGraph := Graph;
end;

function Toutlineviewer.do_recurse(action:codepointer;callerframe:pointer;
                                   stop_if_found:boolean):pointer;

var position:sw_integer;
    r:pointer;

  function recurse(cur:pointer;level:SmallInt;lines:longint;lastchild:boolean):pointer;

  var i,childcount:sw_integer;
      child:pointer;
      flags:word;
      children,expanded,found:boolean;

  begin
    inc(position);
    recurse:=nil;

    children:=haschildren(cur);
    expanded:=isexpanded(cur);

    {Determine flags.}
    flags:=0;
    if not children or expanded then
      inc(flags,ovExpanded);
    if children and expanded then
      inc(flags,ovChildren);
    if lastchild then
      inc(flags,ovLast);

    {Call the function.}
    found:=TMyFunc(action)(callerframe,cur,level,position,lines,flags);

    if stop_if_found and found then
      recurse:=cur
    else if children and expanded then {Recurse children?}
      begin
        if not lastchild then
          lines:=lines or (1 shl level);
        {Iterate all childs.}
        childcount:=getnumchildren(cur);
        for i:=0 to childcount-1 do
          begin
            child:=getchild(cur,i);
            if (child<>nil) and (level<31) then
              recurse:=recurse(child,level+1,lines,i=childcount-1);
            {Did we find a node?}
            if recurse<>nil then
              break;
          end;
      end;
  end;

begin
  position:=-1;
  r:=getroot;
  if r<>nil then
    do_recurse:=recurse(r,0,0,true)
  else
    do_recurse:=nil;
end;

procedure Toutlineviewer.draw;

var c_normal,c_normal_x,c_select,c_focus:byte;
    maxpos:sw_integer;
    b:Tdrawbuffer;

  function draw_item(cur:pointer;level,position:sw_integer;
                     lines:longint;flags:word):boolean;

  var c,i:byte;
{$ifdef FV_UNICODE}
      s,t:UnicodeString;
{$else FV_UNICODE}
      s,t:string;
{$endif FV_UNICODE}

  begin
    draw_item:=position>=delta.y+size.y;
    if (position<delta.y) or draw_item then
      exit;

    maxpos:=position;
    s:=getgraph(level,lines,flags);
    t:=gettext(cur);

    {Determine text colour.}
    if (foc=position) and (state and sffocused<>0) then
      c:=c_focus
    else if isselected(position) then
      c:=c_select
    else if flags and ovexpanded<>0 then
      c:=c_normal_x
    else
      c:=c_normal;

    {Fill drawbuffer with graph and text to draw.}
    for i:=0 to size.x-1 do
      begin
{$ifdef FV_UNICODE}
        b[i].Attribute:=c;
{$else FV_UNICODE}
        wordrec(b[i]).hi:=c;
{$endif FV_UNICODE}
        if i+delta.x<length(s) then
{$ifdef FV_UNICODE}
          b[i].ExtendedGraphemeCluster:=s[1+i+delta.x]
{$else FV_UNICODE}
          wordrec(b[i]).lo:=byte(s[1+i+delta.x])
{$endif FV_UNICODE}
        else if 1+i+delta.x-length(s)<=length(t) then
{$ifdef FV_UNICODE}
          b[i].ExtendedGraphemeCluster:=t[1+i+delta.x-length(s)]
{$else FV_UNICODE}
          wordrec(b[i]).lo:=byte(t[1+i+delta.x-length(s)])
{$endif FV_UNICODE}
        else
{$ifdef FV_UNICODE}
          b[i].ExtendedGraphemeCluster:=' ';
{$else FV_UNICODE}
          wordrec(b[i]).lo:=byte(' ');
{$endif FV_UNICODE}
      end;

    {Draw!}
    writeline(0,position-delta.y,size.x,1,b);
  end;

begin
  c_normal:=getcolor(4);
  c_normal_x:=getcolor(1);
  c_focus:=getcolor(2);
  c_select:=getcolor(3);
  maxpos:=-1;
  foreach(@draw_item);
  movechar(b,' ',c_normal,size.x);
  writeline(0,maxpos+1,size.x,size.y-(maxpos-delta.y),b);
end;

procedure Toutlineviewer.expandall(node:pointer);

var i:sw_integer;

begin
  if haschildren(node) then
    begin
      for i:=0 to getnumchildren(node)-1 do
        expandall(getchild(node,i));
      adjust(node,true);
    end;
end;

function Toutlineviewer.firstthat(test:codepointer):pointer;

begin
  firstthat:=do_recurse(test,
      { On most systems, locals are accessed relative to base pointer,
        but for MIPS cpu, they are accessed relative to stack pointer.
        This needs adaptation for so low level routines,
        like MethodPointerLocal and related objects unit functions. }
{$ifndef FPC_LOCALS_ARE_STACK_REG_RELATIVE}
      get_caller_frame(get_frame,get_pc_addr)
{$else}
      get_frame
{$endif}
      ,true);
end;

procedure Toutlineviewer.focused(i:sw_integer);

begin
  foc:=i;
end;

procedure Toutlineviewer.foreach(action:codepointer);

begin
  do_recurse(action,
      { On most systems, locals are accessed relative to base pointer,
        but for MIPS cpu, they are accessed relative to stack pointer.
        This needs adaptation for so low level routines,
        like MethodPointerLocal and related objects unit functions. }
{$ifndef FPC_LOCALS_ARE_STACK_REG_RELATIVE}
      get_caller_frame(get_frame,get_pc_addr)
{$else}
      get_frame
{$endif}
      ,false);
end;

function Toutlineviewer.getchild(node:pointer;i:sw_integer):pointer;

begin
  abstract;
end;

{$ifdef FV_UNICODE}
function Toutlineviewer.getgraph(level:SmallInt;lines:longint;
                                 flags:word):UnicodeString;

begin
  getgraph:=creategraph(level,lines,flags,3,3,#$0020#$2502#$251C#$2514#$2500#$2500#$002B#$2500);
end;
{$else FV_UNICODE}
function Toutlineviewer.getgraph(level:SmallInt;lines:longint;
                                 flags:word):string;

begin
  getgraph:=creategraph(level,lines,flags,3,3,#32#179#195#192#196#196#43#196);
end;
{$endif FV_UNICODE}

function Toutlineviewer.getnode(i:sw_integer):pointer;

  function test_position(node:pointer;level,position:sw_integer;lines:longInt;
                         flags:word):boolean;

  begin
    test_position:=position=i;
  end;

begin
  getnode:=firstthat(@test_position);
end;

function Toutlineviewer.getnumchildren(node:pointer):sw_integer;

begin
  abstract;
end;

function Toutlineviewer.getpalette:Ppalette;

const p:string[length(Coutlineviewer)]=Coutlineviewer;

begin
  getpalette:=@p;
end;

function Toutlineviewer.getroot:pointer;

begin
  abstract;
end;

{$ifdef FV_UNICODE}
function Toutlineviewer.gettext(node:pointer):UnicodeString;
{$else FV_UNICODE}
function Toutlineviewer.gettext(node:pointer):string;
{$endif FV_UNICODE}

begin
  abstract;
end;

procedure Toutlineviewer.handleevent(var event:Tevent);

var mouse:Tpoint;
    cur:pointer;
    new_focus:sw_integer;
    count:byte;
    handled,m,mouse_drag:boolean;
{$ifdef FV_UNICODE}
    graph:UnicodeString;
{$else FV_UNICODE}
    graph:string;
{$endif FV_UNICODE}

{$ifdef FV_UNICODE}
  function graph_of_focus(var graph:UnicodeString):pointer;
{$else FV_UNICODE}
  function graph_of_focus(var graph:string):pointer;
{$endif FV_UNICODE}

  var _level:sw_integer;
      _lines:longInt;
      _flags:word;

    function find_focused(cur:pointer;level,position:sw_integer;
                          lines:longint;flags:word):boolean;

    begin
      find_focused:=position=foc;
      if find_focused then
        begin
          _level:=level;
          _lines:=lines;
          _flags:=flags;
        end;
    end;

  begin
    graph_of_focus:=firstthat(@find_focused);
    graph:=getgraph(_level,_lines,_flags);
  end;

const skip_mouse_events=3;

begin
  inherited handleevent(event);
  case event.what of
    evKeyboard:
      begin
        new_focus:=foc;
        handled:=true;
        case ctrltoarrow(event.keycode) of
          kbUp,kbLeft:
            dec(new_focus);
          kbDown,kbRight:
            inc(new_focus);
          kbPgDn:
            inc(new_focus,size.y-1);
          kbPgUp:
            dec(new_focus,size.y-1);
          kbCtrlPgUp:
            new_focus:=0;
          kbCtrlPgDn:
            new_focus:=limit.y-1;
          kbHome:
            new_focus:=delta.y;
          kbEnd:
            new_focus:=delta.y+size.y-1;
          kbCtrlEnter,kbEnter:
            selected(new_focus);
        else
          case event.charcode of
            '-','+':
              begin
                adjust(getnode(new_focus),event.charcode='+');
                update;
              end;
            '*':
              begin
                expandall(getnode(new_focus));
                update;
              end;
            else
              handled:=false;
          end;
        end;
        if new_focus<0 then
          new_focus:=0;
        if new_focus>=limit.y then
          new_focus:=limit.y-1;
        if foc<>new_focus then
          set_focus(new_focus);
        if handled then
          clearevent(event);
      end;
    evMouseDown:
      begin
        count:=1;
        mouse_drag:=false;
        repeat
          makelocal(event.where,mouse);
          if mouseinview(event.where) then
            new_focus:=delta.y+mouse.y
          else
            begin
              inc(count,byte(event.what=evMouseAuto));
              if count and skip_mouse_events=0 then
                begin
                  if mouse.y<0 then
                    dec(new_focus);
                  if mouse.y>=size.y then
                    inc(new_focus);
                end;
            end;
          if new_focus<0 then
            new_focus:=0;
          if new_focus>=limit.y then
            new_focus:=limit.y-1;
          if foc<>new_focus then
            set_focus(new_focus);
          m:=mouseevent(event,evMouseMove+evMouseAuto);
          if m then
            mouse_drag:=true;
        until not m;
        if event.double then
          selected(foc)
        else if not mouse_drag then
          begin
            cur:=graph_of_focus(graph);
            if mouse.x<length(graph) then
              begin
                adjust(cur,not isexpanded(cur));
                update;
              end;
          end;
      end;
  end;
end;


function Toutlineviewer.haschildren(node:pointer):boolean;

begin
  abstract;
end;

function Toutlineviewer.isexpanded(node:pointer):boolean;

begin
  abstract;
end;

function Toutlineviewer.isselected(i:sw_integer):boolean;

begin
  isselected:=foc=i;
end;

procedure Toutlineviewer.selected(i:sw_integer);

begin
  {Does nothing by default.}
end;

procedure Toutlineviewer.set_focus(Afocus:sw_integer);

begin
  assert((Afocus>=0) and (Afocus<limit.y));
  focused(Afocus);
  if Afocus<delta.y then
    scrollto(delta.x,Afocus)
  else if Afocus-size.y>=delta.y then
    scrollto(delta.x,Afocus-size.y+1);
  drawview;
end;

procedure Toutlineviewer.setstate(Astate:word;enable:boolean);

begin
  if Astate and sffocused<>0 then
    drawview;
  inherited setstate(Astate,enable);
end;

procedure Toutlineviewer.update;

var count:sw_integer;
    maxwidth:byte;

  procedure check_item(cur:pointer;level,position:sw_integer;
                       lines:longint;flags:word);

  var width:word;

  begin
    inc(count);
    width:=length(gettext(cur))+length(getgraph(level,lines,flags));
    if width>maxwidth then
      maxwidth:=width;
  end;

begin
  count:=0;
  maxwidth:=0;
  foreach(@check_item);
  setlimit(maxwidth,count);
  set_focus(foc);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          Toutline object methods                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor Toutline.init(var bounds:Trect;
                          AHscrollbar,AVscrollbar:Pscrollbar;
                          Aroot:Pnode);

begin
  inherited init(bounds,AHscrollbar,AVscrollbar);
  root:=Aroot;
  update;
end;

procedure Toutline.adjust(node:pointer;expand:boolean);

begin
  assert(node<>nil);
  Pnode(node)^.expanded:=expand;
end;

function Toutline.getnumchildren(node:pointer):sw_integer;

var p:Pnode;

begin
  assert(node<>nil);
  p:=Pnode(node)^.childlist;
  getnumchildren:=0;
  while p<>nil do
    begin
      inc(getnumchildren);
      p:=p^.next;
    end;
end;

function Toutline.getchild(node:pointer;i:sw_integer):pointer;

begin
  assert(node<>nil);
  getchild:=Pnode(node)^.childlist;
  while i<>0 do
    begin
      dec(i);
      getchild:=Pnode(getchild)^.next;
    end;
end;

function Toutline.getroot:pointer;

begin
  getroot:=root;
end;

{$ifdef FV_UNICODE}
function Toutline.gettext(node:pointer):UnicodeString;
{$else FV_UNICODE}
function Toutline.gettext(node:pointer):string;
{$endif FV_UNICODE}

begin
  assert(node<>nil);
{$ifdef FV_UNICODE}
  gettext:=Pnode(node)^.text;
{$else FV_UNICODE}
  gettext:=Pnode(node)^.text^;
{$endif FV_UNICODE}
end;

function Toutline.haschildren(node:pointer):boolean;

begin
  assert(node<>nil);
  haschildren:=Pnode(node)^.childlist<>nil;
end;

function Toutline.isexpanded(node:pointer):boolean;

begin
  assert(node<>nil);
  isexpanded:=Pnode(node)^.expanded;
end;

destructor Toutline.done;

begin
  disposenode(root);
  inherited done;
end;

end.
