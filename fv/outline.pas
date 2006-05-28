unit outline;

{***************************************************************************}
                                  interface
{***************************************************************************}

uses  objects,views;

type  Pnode=^Tnode;
      Tnode=record
        next:Pnode;
        text:Pstring;
        childlist:Pnode;
        expanded:boolean;
      end;

      Poutlineviewer=^Toutlineviewer;
      Toutlineviewer=object(Tscroller)
        foc:sw_integer;
        constructor init(var bounds:Trect;
                         AHscrollbar,AVscrollbar:Pscrollbar);
        procedure adjust(node:pointer;expand:boolean);virtual;
        function creategraph(level:integer;lines:longint;
                             flags:word;levwidth,endwidth:integer;
                             const chars:string):string;
        procedure draw;virtual;
        procedure expandall(node:pointer);
        function firstthat(test:pointer):pointer;
        procedure focused(i:sw_integer);virtual;
        procedure foreach(action:pointer);
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
        function getgraph(level:integer;lines:longint;flags:word):string;
        function getnode(i:sw_integer):pointer;virtual;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getroot:pointer;virtual;
        function gettext(node:pointer):string;virtual;
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        function isselected(i:sw_integer):boolean;virtual;
        procedure selected(i:sw_integer);virtual;
        procedure setstate(Astate:word;enable:boolean);virtual;
        procedure update;
      private
        procedure set_focus(Afocus:sw_integer);
        function do_recurse(action,callerframe:pointer;
                            stop_if_found:boolean):pointer;
      end;

      Toutline=object(Toutlineviewer)
        root:Pnode;
        constructor init(var bounds:Trect;
                         AHscrollbar,AVscrollbar:Pscrollbar;
                         Aroot:Pnode);
        procedure adjust(node:pointer;expand:boolean);virtual;
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getroot:pointer;virtual;
        function gettext(node:pointer):string;virtual;
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        destructor done;virtual;
      end;

const ovExpanded = $1;
      ovChildren = $2;
      ovLast     = $4;

function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;
procedure disposenode(node:Pnode);


{***************************************************************************}
                                implementation
{***************************************************************************}

type TMyFunc = function(_EBP: Pointer; Cur: Pointer;
                        Level, Position: sw_integer; Lines: LongInt;
                        Flags: Word): Boolean;


function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;

begin
  newnode:=new(Pnode);
  with newnode^ do
    begin
      next:=Anext;
      text:=newstr(Atext);
      childlist:=Achildren;
      expanded:=false;
    end;
end;

procedure disposenode(node:Pnode);

begin
  while node<>nil do
    begin
      disposenode(node^.childlist);
      disposestr(node^.text);
      dispose(node);
      node:=node^.next;
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

function TOutlineViewer.CreateGraph(Level: Integer; Lines: LongInt;
  Flags: Word; LevWidth, EndWidth: Integer;
  const Chars: String): String;
const
  FillerOrBar  = 0;
  YorL         = 2;
  StraightOrTee= 4;
  Retracted    = 6;
var
  Last, Children, Expanded: Boolean;
  I , J : Byte;
  Graph : String;

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
  Graph[0] := Char(J);

  CreateGraph := Graph;
end;

procedure Toutlineviewer.draw;

var c_normal,c_normal_x,c_select,c_focus:byte;
    maxpos:sw_integer;
    b:Tdrawbuffer;

  function draw_item(cur:pointer;level,position:sw_integer;
                     lines:longint;flags:word):boolean;

  var c,i:byte;
      s,t:string;

  begin
    draw_item:=position>=delta.y+size.y;
    if (position<delta.y) or draw_item then
      exit;

    maxpos:=position;
    s:=getgraph(level,lines,flags);
    t:=gettext(cur);

    {Determine text colour.}
    if isselected(position) then
      c:=c_select
    else if (foc=position) and (state and sffocused<>0) then
      c:=c_focus
    else if flags and ovexpanded<>0 then
      c:=c_normal_x
    else
      c:=c_normal;

    {Fill drawbuffer with graph and text to draw.}
    for i:=0 to size.x-1 do
      begin
        wordrec(b[i]).hi:=c;
        if i+delta.x<=length(s) then
          wordrec(b[i]).lo:=byte(s[i+delta.x])
        else if i+delta.x-length(s)<length(t) then
          wordrec(b[i]).lo:=byte(s[i+delta.x-length(s)])
        else
          wordrec(b[i]).lo:=byte(' ');
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

function Toutlineviewer.firstthat(test:pointer):pointer;

begin
  firstthat:=do_recurse(test,get_caller_frame(get_frame),true);
end;

procedure Toutlineviewer.focused(i:sw_integer);

begin
  foc:=i;
end;

procedure Toutlineviewer.foreach(action:pointer);

begin
  do_recurse(action,get_caller_frame(get_frame),false);
end;

function Toutlineviewer.getchild(node:pointer;i:sw_integer):pointer;

begin
  abstract;
end;

function Toutlineviewer.getgraph(level:integer;lines:longint;
                                 flags:word):string;

begin
  getgraph:=creategraph(level,lines,flags,3,3,' ³ÃÀÄÄ+Ä');
end;

function Toutlineviewer.getnode(i:sw_integer):pointer;

  function test_position(node:pointer;level,position:sw_integer;lines:longInt;
                         flags:word):boolean;

  begin
    test_position:=position=i;
  end;

begin
  foreach(@test_position);
end;

function Toutlineviewer.getnumchildren(node:pointer):sw_integer;

begin
  abstract;
end;

function Toutlineviewer.getroot:pointer;

begin
  abstract;
end;

function Toutlineviewer.gettext(node:pointer):string;

begin
  abstract;
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

function Toutlineviewer.do_recurse(action,callerframe:pointer;
                                   stop_if_found:boolean):pointer;

var position:sw_integer;

  function recurse(cur:pointer;level:integer;lines:longint;lastchild:boolean):pointer;

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
  do_recurse:=recurse(getroot,0,0,true);
end;

procedure Toutlineviewer.selected(i:sw_integer);

begin
  {Does nothing by default.}
end;

procedure Toutlineviewer.set_focus(Afocus:sw_integer);

begin
  {}
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
  Pnode(node)^.expanded:=expand;
end;

function Toutline.getnumchildren(node:pointer):sw_integer;

var p:Pnode;

begin
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

function Toutline.gettext(node:pointer):string;

begin
  gettext:=Pnode(node)^.text^;
end;

function Toutline.haschildren(node:pointer):boolean;

begin
  haschildren:=Pnode(node)^.childlist<>nil;
end;

function Toutline.isexpanded(node:pointer):boolean;

begin
  isexpanded:=Pnode(node)^.expanded;
end;

destructor Toutline.done;

begin
  disposenode(root);
  inherited done;
end;

end.
