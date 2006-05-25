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
        procedure foreach(node:pointer);
        function getgraph(level:integer;lines:longint;flags:word):string;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
        function gettext(node:pointer):string;virtual;
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        destructor done;virtual;
      end;

      Toutline=object(Toutlineviewer)
        root:Pnode;
        constructor init(var bounds:Trect;
                         AHscrollbar,AVscrollbar:Pscrollbar;
                         Aroot:Pnode);
        procedure adjust(node:pointer;expand:boolean);virtual;
        function getnumchildren(node:pointer):sw_integer;virtual;
        function getchild(node:pointer;i:sw_integer):pointer;virtual;
        function gettext(node:pointer):string;virtual;
        function haschildren(node:pointer):boolean;virtual;
        function isexpanded(node:pointer):boolean;virtual;
        destructor done;virtual;
      end;

function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;
procedure disposenode(node:Pnode);


{***************************************************************************}
                                implementation
{***************************************************************************}

function newnode(const Atext:string;Achildren,Anext:Pnode):Pnode;

begin
  new(newnode);
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
  with node^ do
    begin
      if childlist<>nil then
        disposenode(childlist);
      if next<>nil then
        disposenode(next)
    end;
  dispose(node);
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

procedure Toutlineviewer.adjust(node:pointer;expand:boolean);virtual;

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
  { Break out flags }
  Expanded := Boolean((Flags and ovExpanded) <> 0);
  Children := Boolean((Flags and ovChildren) <> 0);
  Last    := Boolean((Flags and ovLast) <> 0);

  { Load registers }
  J := Level*LevWidth+EndWidth+1;
  Graph[0] := Char(J);
  for I := 1 to J do
    Graph[I] := ' ';

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
    begin
      Graph[I]:= Chars[FillerOrBar+1];
    end;
    J := J + LevWidth - 1;
    Dec(Level);
    Lines := Lines shr 1;
  end;

  { Write end characters }
  Dec(EndWidth);
  if EndWidth > 0 then
  begin
    Inc(J);
    if Last <> False then
      Graph[J] := Chars[YorL+2]
    else
      Graph[J] := Chars[YorL+1];
    Dec(EndWidth);
    if EndWidth > 0 then
    begin
      Dec(EndWidth);
      if EndWidth > 0 then
      begin
        for I := 1 to EndWidth do
        begin
          Graph[I]:= Chars[StraightOrTee+1];
        end;
        J := J + EndWidth;
      end;
      Inc(J);
      if Children <> False then
        Graph[J] := Chars[StraightOrTee+2]
      else
        Graph[J] := Chars[StraightOrTee+1];
    end;
    Inc(J);
    if Expanded <> False then
      Graph[J] := Chars[Retracted+2]
    else
      Graph[J] := Chars[Retracted+1];
  end;
  Graph[0] := Char(J);

  CreateGraph := Graph;
end;

procedure Toutlineviewer.draw;

begin
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
end;

procedure Toutlineviewer.focused(i:sw_integer);

begin
  foc:=i;
end;

procedure Toutlineviewer.foreach(action:pointer);

begin
end;

function Toutlineviewer.getgraph(level:integer;lines:longint;
                                 flags:word):string;

begin
end;

function Toutlineviewer.getnumchildren(node:pointer):sw_integer;

begin
  abstract;
end;

function Toutlineviewer.getchild(node:pointer;i:sw_integer):pointer;

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
end;

destructor Toutlineviewer.done;virtual;

begin
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          Toutline object methods                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor Toutline.init(var bounds:Trect;
                          AHscrollbar,AVscrollbar:Pscrollbar;
                          Aroot:Pnode);

begin
  inherited init(bounds,AHscrollbar,AVscrollbar);
  root:=Pnode;
  update;
end;

procedure Toutline.adjust(node:pointer;expand:boolean);

begin
  Pnode(node)^.expanded:=expand;
end;

function Toutline.getroot:pointer;virtual;

begin
  getroot:=root;
end;

function Toutline.getnumchildren(node:pointer):sw_integer;virtual;

var p:Pnode;

begin
  p:=Pnode(node)^.childlist;
  get_num_children:=0;
  while p<>nil do
    begin
      inc(get_num_children);
      p:=p^.next;
    end;
end;

function Toutline.getchild(node:pointer;i:sw_integer):pointer;virtual;

begin
  get_child:=Pnode(node)^.childlist;
  while i<>0 do
    begin
      dec(i);
      get_child:=get_child^.next;
    end;
end;

function Toutline.gettext(node:pointer):string;

begin
  gettext:=Pnode(node)^.text;
end;

function Toutline.haschildren(node:pointer):boolean;

begin
  haschildren:=Pnode(node)^.childlist<>nil;
end;

function isexpanded(node:pointer):boolean;

begin
  isexpanded:=Pnode(node)^.expanded;
end;

destructor Toutline.done;

begin
  dispose(root,done);
end;

end.
