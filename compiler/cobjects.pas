{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This module provides some basic objects

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

{$ifdef tp}
  {$E+,N+,D+,F+}
{$endif}
{$I-}
{$R-}{ necessary for crc calculation }

unit cobjects;

{ define OLDSPEEDVALUE}

  interface

    uses
{$ifdef DELPHI4}
       dmisc,
       sysutils
{$else DELPHI4}
       strings
{$ifndef linux}
       ,dos
{$else}
       ,linux
{$endif}
{$endif DELPHI4}
      ;

    const
       { the real size will be [-hasharray..hasharray] ! }
{$ifdef TP}
       hasharraysize = 127;
{$else}
       hasharraysize = 2047;
{$endif}

    type
       pstring = ^string;

{$ifdef TP}
       { redeclare dword only in case of emergency, some small things
         of the compiler won't work then correctly (FK)
       }
       dword = longint;
{$endif TP}

       pfileposinfo = ^tfileposinfo;
       tfileposinfo = record
         line      : longint;
         column    : word;
         fileindex : word;
       end;

       pmemdebug = ^tmemdebug;
       tmemdebug = object
          constructor init(const s:string);
          destructor  done;
          procedure show;
       private
          startmem : longint;
          infostr  : string[40];
       end;

       plinkedlist_item = ^tlinkedlist_item;
       tlinkedlist_item = object
          next,previous : plinkedlist_item;
          { does nothing }
          constructor init;
          destructor done;virtual;
          function getcopy:plinkedlist_item;virtual;
       end;

       pstring_item = ^tstring_item;
       tstring_item = object(tlinkedlist_item)
          str : pstring;
          constructor init(const s : string);
          destructor done;virtual;
       end;


       { this implements a double linked list }
       plinkedlist = ^tlinkedlist;
       tlinkedlist = object
          first,last : plinkedlist_item;
          constructor init;
          destructor done;
          { destructors the linkedlist without cleaning the items up }
          destructor done_noclear;

          { disposes the items of the list }
          procedure clear;

          { concats a new item at the end }
          procedure concat(p : plinkedlist_item);

          { inserts a new item at the begin }
          procedure insert(p : plinkedlist_item);

          { inserts another list at the begin and make this list empty }
          procedure insertlist(p : plinkedlist);

          { concats another list at the end and make this list empty }
          procedure concatlist(p : plinkedlist);

          procedure concatlistcopy(p : plinkedlist);

          { removes p from the list (p isn't disposed) }
          { it's not tested if p is in the list !      }
          procedure remove(p : plinkedlist_item);

          { is the linkedlist empty ? }
          function  empty:boolean;

          { items in the list }
          function  count:longint;
       end;

       { some help data types }
       pstringqueueitem = ^tstringqueueitem;
       tstringqueueitem = object
          data : pstring;
          next : pstringqueueitem;
       end;

       { String Queue}
       PStringQueue=^TStringQueue;
       TStringQueue=object
         first,last : PStringqueueItem;
         constructor Init;
         destructor Done;
         function Empty:boolean;
         function Get:string;
         function Find(const s:string):PStringqueueItem;
         function Delete(const s:string):boolean;
         procedure Insert(const s:string);
         procedure Concat(const s:string);
         procedure Clear;
       end;

       { containeritem }
       pcontaineritem = ^tcontaineritem;
       tcontaineritem = object
          next : pcontaineritem;
          constructor init;
          destructor  done;virtual;
       end;

       { container }
       pcontainer = ^tcontainer;
       tcontainer = object
          root,
          last    : pcontaineritem;
          constructor init;
          destructor  done;
          { true when the container is empty }
          function  empty:boolean;
          { inserts a string }
          procedure insert(item:pcontaineritem);
          { gets a string }
          function  get:pcontaineritem;
          { deletes all items }
          procedure clear;
       end;

       { containeritem }
       pstringcontaineritem = ^tstringcontaineritem;
       tstringcontaineritem = object(tcontaineritem)
          data : pstring;
          file_info : tfileposinfo;
          constructor init(const s:string);
          constructor Init_TokenInfo(const s:string;const pos:tfileposinfo);
          destructor  done;virtual;
       end;

       { string container }
       pstringcontainer = ^tstringcontainer;
       tstringcontainer = object(tcontainer)
          doubles : boolean;  { if this is set to true, doubles are allowed }
          constructor init;
          constructor init_no_double;
          procedure insert(const s : string);
          procedure insert_with_tokeninfo(const s : string;const file_info : tfileposinfo);
          { gets a string }
          function get : string;
          function get_with_tokeninfo(var file_info : tfileposinfo) : string;
          { true if string is in the container }
          function find(const s:string):boolean;
       end;


       { namedindexobject for use with dictionary and indexarray }
       Pnamedindexobject=^Tnamedindexobject;
       Tnamedindexobject=object
         indexnr    : longint;
         _name      : Pstring;
         next,
         left,right : Pnamedindexobject;
         speedvalue : longint;
         constructor init;
         constructor initname(const n:string);
         destructor  done;virtual;
         procedure setname(const n:string);virtual;
         function  name:string;virtual;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[-hasharraysize..hasharraysize] of Pnamedindexobject;

       Tnamedindexcallback = procedure(p:Pnamedindexobject);

       Pdictionary=^Tdictionary;
       Tdictionary=object
         noclear   : boolean;
         replace_existing : boolean;
         constructor init;
         destructor  done;virtual;
         procedure usehash;
         procedure clear;
         function delete(const s:string):Pnamedindexobject;
         function  empty:boolean;
         procedure foreach(proc2call:Tnamedindexcallback);
         function  insert(obj:Pnamedindexobject):Pnamedindexobject;
         function  rename(const olds,news : string):Pnamedindexobject;
         function  search(const s:string):Pnamedindexobject;
         function  speedsearch(const s:string;speedvalue:longint):Pnamedindexobject;
       private
         root      : Pnamedindexobject;
         hasharray : Pdictionaryhasharray;
         procedure cleartree(obj:Pnamedindexobject);
         function  insertnode(newnode:Pnamedindexobject;var currnode:Pnamedindexobject):Pnamedindexobject;
         procedure inserttree(currtree,currroot:Pnamedindexobject);
       end;

       pdynamicarray = ^tdynamicarray;
       tdynamicarray = object
         posn,
         count,
         limit,
         elemlen,
         growcount : longint;
         data      : pchar;
         constructor init(Aelemlen,Agrow:longint);
         destructor  done;
         function  size:longint;
         function  usedsize:longint;
         procedure grow;
         procedure align(i:longint);
         procedure seek(i:longint);
         procedure write(var d;len:longint);
         procedure read(var d;len:longint);
         procedure writepos(pos:longint;var d;len:longint);
         procedure readpos(pos:longint;var d;len:longint);
       end;

      tindexobjectarray=array[1..16000] of Pnamedindexobject;
      Pnamedindexobjectarray=^tindexobjectarray;

      pindexarray=^tindexarray;
      tindexarray=object
        first : Pnamedindexobject;
        count : longint;
        constructor init(Agrowsize:longint);
        destructor  done;
        procedure clear;
        procedure foreach(proc2call : Tnamedindexcallback);
        procedure deleteindex(p:Pnamedindexobject);
        procedure delete(p:Pnamedindexobject);
        procedure insert(p:Pnamedindexobject);
        function  search(nr:longint):Pnamedindexobject;
      private
        growsize,
        size  : longint;
        data  : Pnamedindexobjectarray;
        procedure grow(gsize:longint);
      end;

{$ifdef BUFFEREDFILE}
       { this is implemented to allow buffered binary I/O }
       pbufferedfile = ^tbufferedfile;
       tbufferedfile = object
           f : file;
           buf : pchar;
           bufsize,buflast,bufpos : longint;

           { 0 closed, 1 input, 2 output }
           iomode : byte;

           { true, if the compile should change the endian of the output }
           change_endian : boolean;

           { calcules a crc for the file,                                    }
           { but it's assumed, that there no seek while do_crc is true       }
           do_crc : boolean;
           crc : longint;
           { temporary closing feature }
           tempclosed : boolean;
           tempmode : byte;
           temppos : longint;

           { inits a buffer with the size bufsize which is assigned to }
           { the file  filename                                        }
           constructor init(const filename : string;_bufsize : longint);

           { closes the file, if needed, and releases the memory }
           destructor done;virtual;

           { opens the file for input, other accesses are rejected }
           function  reset:boolean;

           { opens the file for output, other accesses are rejected }
           procedure rewrite;

           { reads or writes the buffer from or to disk }
           procedure flush;

           { writes a string to the file }
           { the string is written without a length byte }
           procedure write_string(const s : string);

           { writes a zero terminated string }
           procedure write_pchar(p : pchar);

           { write specific data types, takes care of }
           { byte order                               }
           procedure write_byte(b : byte);
           procedure write_word(w : word);
           procedure write_long(l : longint);
           procedure write_double(d : double);

           { writes any data }
           procedure write_data(var data;count : longint);

           { reads any data }
           procedure read_data(var data;bytes : longint;var count : longint);

           { closes the file and releases the buffer }
           procedure close;

           { temporary closing }
           procedure tempclose;
           procedure tempreopen;

           { goto the given position }
           procedure seek(l : longint);

           { installes an user defined buffer      }
           { and releases the old one, but be      }
           { careful, if the old buffer contains   }
           { data, this data is lost               }
           procedure setbuf(p : pchar;s : longint);

           { reads the file time stamp of the file, }
           { the file must be opened                }
           function getftime : longint;

           { returns filesize }
           function getsize : longint;

           { returns the path }
           function getpath : string;

           { resets the crc }
           procedure clear_crc;

           { returns the crc }
           function getcrc : longint;
       end;
{$endif BUFFEREDFILE}

    function getspeedvalue(const s : string) : longint;

    { releases the string p and assignes nil to p }
    { if p=nil then freemem isn't called          }
    procedure stringdispose(var p : pstring);

    { idem for ansistrings }
    procedure ansistringdispose(var p : pchar;length : longint);

    { allocates mem for a copy of s, copies s to this mem and returns }
    { a pointer to this mem                                           }
    function stringdup(const s : string) : pstring;

    { allocates memory for s and copies s as zero terminated string
      to that mem and returns a pointer to that mem }
    function  strpnew(const s : string) : pchar;
    procedure strdispose(var p : pchar);

    { makes a char lowercase, with spanish, french and german char set }
    function lowercase(c : char) : char;

    { makes zero terminated string to a pascal string }
    { the data in p is modified and p is returned     }
    function pchar2pstring(p : pchar) : pstring;

    { ambivalent to pchar2pstring }
    function pstring2pchar(p : pstring) : pchar;

  implementation

    uses
      comphook;

{*****************************************************************************
                                    Memory debug
*****************************************************************************}

    constructor tmemdebug.init(const s:string);
      begin
        infostr:=s;
        startmem:=memavail;
      end;

    procedure tmemdebug.show;
      var
        l : longint;
      begin
        write('memory [',infostr,'] ');
        l:=memavail;
        if l>startmem then
         writeln(l-startmem,' released')
        else
         writeln(startmem-l,' allocated');
      end;

    destructor tmemdebug.done;
      begin
        show;
      end;


{$ifndef OLDSPEEDVALUE}

{*****************************************************************************
                                   Crc 32
*****************************************************************************}

var
{$ifdef Delphi}
  Crc32Tbl : array[0..255] of longword;
{$else Delphi}
  Crc32Tbl : array[0..255] of longint;
{$endif Delphi}

procedure MakeCRC32Tbl;
var
{$ifdef Delphi}
  crc : longword;
{$else Delphi}
  crc : longint;
{$endif Delphi}
  i,n : byte;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor $edb88320
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}

{$R- needed here }
{CRC 32}
Function GetSpeedValue(Const s:String):longint;
var
  i,InitCrc : longint;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  InitCrc:=$ffffffff;
  for i:=1to Length(s) do
   InitCrc:=Crc32Tbl[byte(InitCrc) xor ord(s[i])] xor (InitCrc shr 8);
  GetSpeedValue:=InitCrc;
end;

{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}

{$else}

{$ifndef TP}
    function getspeedvalue(const s : string) : longint;
      var
        p1,p2:^byte;
        i : longint;

      begin
        p1:=@s;
        longint(p2):=longint(p1)+p1^+1;
        inc(longint(p1));
        i:=0;
        while p1<>p2 do
         begin
           i:=i + ord(p1^);
           inc(longint(p1));
         end;
        getspeedvalue:=i;
      end;
{$else}
    function getspeedvalue(const s : string) : longint;
      type
        ptrrec=record
          ofs,seg:word;
        end;
      var
        l,w   : longint;
        p1,p2 : ^byte;
      begin
        p1:=@s;
        ptrrec(p2).seg:=ptrrec(p1).seg;
        ptrrec(p2).ofs:=ptrrec(p1).ofs+p1^+1;
        inc(p1);
        l:=0;
        while p1<>p2 do
         begin
           l:=l + ord(p1^);
           inc(p1);
         end;
        getspeedvalue:=l;
      end;
{$endif}

{$endif OLDSPEEDVALUE}


    function pchar2pstring(p : pchar) : pstring;
      var
         w,i : longint;
      begin
         w:=strlen(p);
         for i:=w-1 downto 0 do
           p[i+1]:=p[i];
         p[0]:=chr(w);
         pchar2pstring:=pstring(p);
      end;


    function pstring2pchar(p : pstring) : pchar;
      var
         w,i : longint;
      begin
         w:=length(p^);
         for i:=1 to w do
           p^[i-1]:=p^[i];
         p^[w]:=#0;
         pstring2pchar:=pchar(p);
      end;


    function lowercase(c : char) : char;
       begin
          case c of
             #65..#90 : c := chr(ord (c) + 32);
             #154 : c:=#129;  { german }
             #142 : c:=#132;  { german }
             #153 : c:=#148;  { german }
             #144 : c:=#130;  { french }
             #128 : c:=#135;  { french }
             #143 : c:=#134;  { swedish/norge (?) }
             #165 : c:=#164;  { spanish }
             #228 : c:=#229;  { greek }
             #226 : c:=#231;  { greek }
             #232 : c:=#227;  { greek }
          end;
          lowercase := c;
       end;


    function strpnew(const s : string) : pchar;
      var
         p : pchar;
      begin
         getmem(p,length(s)+1);
         strpcopy(p,s);
         strpnew:=p;
      end;


    procedure strdispose(var p : pchar);
      begin
        if assigned(p) then
         begin
           freemem(p,strlen(p)+1);
           p:=nil;
         end;
      end;


    procedure stringdispose(var p : pstring);
      begin
         if assigned(p) then
           freemem(p,length(p^)+1);
         p:=nil;
      end;


    procedure ansistringdispose(var p : pchar;length : longint);
      begin
         if assigned(p) then
           freemem(p,length+1);
         p:=nil;
      end;


    function stringdup(const s : string) : pstring;
      var
         p : pstring;
      begin
         getmem(p,length(s)+1);
         p^:=s;
         stringdup:=p;
      end;


{****************************************************************************
                                  TStringQueue
****************************************************************************}

constructor TStringQueue.Init;
begin
  first:=nil;
  last:=nil;
end;


function TStringQueue.Empty:boolean;
begin
  Empty:=(first=nil);
end;


function TStringQueue.Get:string;
var
  newnode : pstringqueueitem;
begin
  if first=nil then
   begin
     Get:='';
     exit;
   end;
  Get:=first^.data^;
  stringdispose(first^.data);
  newnode:=first;
  first:=first^.next;
  dispose(newnode);
end;


function TStringQueue.Find(const s:string):PStringqueueItem;
var
  p : PStringqueueItem;
begin
  p:=first;
  while assigned(p) do
   begin
     if p^.data^=s then
      break;
     p:=p^.next;
   end;
  Find:=p;
end;


function TStringQueue.Delete(const s:string):boolean;
var
  prev,p : PStringqueueItem;
begin
  Delete:=false;
  prev:=nil;
  p:=first;
  while assigned(p) do
   begin
     if p^.data^=s then
      begin
        if p=last then
          last:=prev;
        if assigned(prev) then
         prev^.next:=p^.next
        else
         first:=p^.next;
        dispose(p);
        Delete:=true;
        exit;
      end;
     prev:=p;
     p:=p^.next;
   end;
end;


procedure TStringQueue.Insert(const s:string);
var
  newnode : pstringqueueitem;
begin
  new(newnode);
  newnode^.next:=first;
  newnode^.data:=stringdup(s);
  first:=newnode;
  if last=nil then
   last:=newnode;
end;


procedure TStringQueue.Concat(const s:string);
var
  newnode : pstringqueueitem;
begin
  new(newnode);
  newnode^.next:=nil;
  newnode^.data:=stringdup(s);
  if first=nil then
   first:=newnode
  else
   last^.next:=newnode;
  last:=newnode;
end;


procedure TStringQueue.Clear;
var
  newnode : pstringqueueitem;
begin
  while (first<>nil) do
   begin
     newnode:=first;
     stringdispose(first^.data);
     first:=first^.next;
     dispose(newnode);
   end;
  last:=nil;
end;


destructor TStringQueue.Done;
begin
  Clear;
end;


{****************************************************************************
                                TContainerItem
 ****************************************************************************}

constructor TContainerItem.Init;
begin
end;


destructor TContainerItem.Done;
begin
end;


{****************************************************************************
                             TStringContainerItem
 ****************************************************************************}

constructor TStringContainerItem.Init(const s:string);
begin
  inherited Init;
  data:=stringdup(s);
  file_info.fileindex:=0;
  file_info.line:=0;
  file_info.column:=0;
end;


constructor TStringContainerItem.Init_TokenInfo(const s:string;const pos:tfileposinfo);
begin
  inherited Init;
  data:=stringdup(s);
  file_info:=pos;
end;


destructor TStringContainerItem.Done;
begin
  stringdispose(data);
end;



{****************************************************************************
                                   TCONTAINER
 ****************************************************************************}

    constructor tcontainer.init;
      begin
         root:=nil;
         last:=nil;
      end;


    destructor tcontainer.done;
      begin
         clear;
      end;


    function tcontainer.empty:boolean;
      begin
        empty:=(root=nil);
      end;


    procedure tcontainer.insert(item:pcontaineritem);
      begin
         item^.next:=nil;
         if root=nil then
          root:=item
         else
          last^.next:=item;
         last:=item;
      end;


    procedure tcontainer.clear;
      var
         newnode : pcontaineritem;
      begin
         newnode:=root;
         while assigned(newnode) do
           begin
              root:=newnode^.next;
              dispose(newnode,done);
              newnode:=root;
           end;
         last:=nil;
         root:=nil;
      end;


    function tcontainer.get:pcontaineritem;
      begin
         if root=nil then
          get:=nil
         else
          begin
            get:=root;
            root:=root^.next;
          end;
      end;


{****************************************************************************
                           TSTRINGCONTAINER
 ****************************************************************************}

    constructor tstringcontainer.init;
      begin
         inherited init;
         doubles:=true;
      end;


    constructor tstringcontainer.init_no_double;
      begin
         inherited init;
         doubles:=false;
      end;


    procedure tstringcontainer.insert(const s : string);
      var
        newnode : pstringcontaineritem;
      begin
         if (s='') or
            ((not doubles) and find(s)) then
          exit;
         new(newnode,init(s));
         inherited insert(newnode);
      end;


    procedure tstringcontainer.insert_with_tokeninfo(const s : string; const file_info : tfileposinfo);
      var
        newnode : pstringcontaineritem;
      begin
         if (not doubles) and find(s) then
          exit;
         new(newnode,init_tokeninfo(s,file_info));
         inherited insert(newnode);
      end;


    function tstringcontainer.get : string;
      var
         p : pstringcontaineritem;
      begin
         p:=pstringcontaineritem(inherited get);
         if p=nil then
          get:=''
         else
          begin
            get:=p^.data^;
            dispose(p,done);
          end;
      end;


    function tstringcontainer.get_with_tokeninfo(var file_info : tfileposinfo) : string;
      var
         p : pstringcontaineritem;
      begin
         p:=pstringcontaineritem(inherited get);
         if p=nil then
          begin
            get_with_tokeninfo:='';
            file_info.fileindex:=0;
            file_info.line:=0;
            file_info.column:=0;
          end
         else
          begin
            get_with_tokeninfo:=p^.data^;
            file_info:=p^.file_info;
            dispose(p,done);
          end;
      end;


    function tstringcontainer.find(const s:string):boolean;
      var
        newnode : pstringcontaineritem;
      begin
        find:=false;
        newnode:=pstringcontaineritem(root);
        while assigned(newnode) do
         begin
           if newnode^.data^=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=pstringcontaineritem(newnode^.next);
         end;
      end;


{****************************************************************************
                            TLINKEDLIST_ITEM
 ****************************************************************************}

    constructor tlinkedlist_item.init;
      begin
        previous:=nil;
        next:=nil;
      end;


    destructor tlinkedlist_item.done;
      begin
      end;


    function tlinkedlist_item.getcopy:plinkedlist_item;
      var
        l : longint;
        p : plinkedlist_item;
      begin
        l:=sizeof(self);
        getmem(p,l);
        move(self,p^,l);
        getcopy:=p;
      end;


{****************************************************************************
                            TSTRING_ITEM
 ****************************************************************************}

    constructor tstring_item.init(const s : string);
      begin
         str:=stringdup(s);
      end;


    destructor tstring_item.done;
      begin
         stringdispose(str);
         inherited done;
      end;


{****************************************************************************
                               TLINKEDLIST
 ****************************************************************************}

    constructor tlinkedlist.init;
      begin
         first:=nil;
         last:=nil;
      end;


    destructor tlinkedlist.done;

      begin
         clear;
      end;

    destructor tlinkedlist.done_noclear;

      begin
      end;

    procedure tlinkedlist.clear;
      var
         newnode : plinkedlist_item;
      begin
         newnode:=first;
         while assigned(newnode) do
           begin
              first:=newnode^.next;
              dispose(newnode,done);
              newnode:=first;
           end;
      end;


    procedure tlinkedlist.insertlist(p : plinkedlist);
      begin
         { empty list ? }
         if not(assigned(p^.first)) then
           exit;

         p^.last^.next:=first;

         { we have a double linked list }
         if assigned(first) then
           first^.previous:=p^.last;

         first:=p^.first;

         if not(assigned(last)) then
           last:=p^.last;

         { p becomes empty }
         p^.first:=nil;
         p^.last:=nil;
      end;


    procedure tlinkedlist.concat(p : plinkedlist_item);
      begin
        if not(assigned(first)) then
         begin
           first:=p;
           p^.previous:=nil;
           p^.next:=nil;
         end
        else
         begin
           last^.next:=p;
           p^.previous:=last;
           p^.next:=nil;
         end;
        last:=p;
      end;


    procedure tlinkedlist.insert(p : plinkedlist_item);
      begin
         if not(assigned(first)) then
          begin
            last:=p;
            p^.previous:=nil;
            p^.next:=nil;
          end
         else
          begin
            first^.previous:=p;
            p^.previous:=nil;
            p^.next:=first;
          end;
         first:=p;
      end;


    procedure tlinkedlist.remove(p : plinkedlist_item);
      begin
         if not(assigned(p)) then
           exit;
         if (first=p) and (last=p) then
           begin
              first:=nil;
              last:=nil;
           end
         else if first=p then
           begin
              first:=p^.next;
              if assigned(first) then
                first^.previous:=nil;
           end
         else if last=p then
           begin
              last:=last^.previous;
              if assigned(last) then
                last^.next:=nil;
           end
         else
           begin
              p^.previous^.next:=p^.next;
              p^.next^.previous:=p^.previous;
           end;
         p^.next:=nil;
         p^.previous:=nil;
      end;


    procedure tlinkedlist.concatlist(p : plinkedlist);
     begin
         if not(assigned(p^.first)) then
           exit;

         if not(assigned(first)) then
           first:=p^.first
           else
             begin
                last^.next:=p^.first;
                p^.first^.previous:=last;
             end;

         last:=p^.last;

         { make p empty }
         p^.last:=nil;
         p^.first:=nil;
      end;


    procedure tlinkedlist.concatlistcopy(p : plinkedlist);
      var
        newnode,newnode2 : plinkedlist_item;
      begin
         newnode:=p^.first;
         while assigned(newnode) do
          begin
            newnode2:=newnode^.getcopy;
            if assigned(newnode2) then
             begin
               if not(assigned(first)) then
                begin
                  first:=newnode2;
                  newnode2^.previous:=nil;
                  newnode2^.next:=nil;
                end
               else
                begin
                  last^.next:=newnode2;
                  newnode2^.previous:=last;
                  newnode2^.next:=nil;
                end;
               last:=newnode2;
             end;
            newnode:=newnode^.next;
          end;
      end;


    function tlinkedlist.empty:boolean;
      begin
        empty:=(first=nil);
      end;


    function tlinkedlist.count:longint;
      var
        i : longint;
        hp : plinkedlist_item;
      begin
        hp:=first;
        i:=0;
        while assigned(hp) do
         begin
           inc(i);
           hp:=hp^.next;
         end;
        count:=i;
      end;


{****************************************************************************
                               Tnamedindexobject
 ****************************************************************************}

constructor Tnamedindexobject.init;
begin
  { index }
  indexnr:=-1;
  next:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  _name:=nil;
  speedvalue:=-1;
end;

constructor Tnamedindexobject.initname(const n:string);
begin
  { index }
  indexnr:=-1;
  next:=nil;
  { dictionary }
  left:=nil;
  right:=nil;
  speedvalue:=-1;
  _name:=stringdup(n);
end;

destructor Tnamedindexobject.done;
begin
  stringdispose(_name);
end;

procedure Tnamedindexobject.setname(const n:string);
begin
  if speedvalue=-1 then
   begin
     if assigned(_name) then
       stringdispose(_name);
     _name:=stringdup(n);
   end;
end;

function Tnamedindexobject.name:string;
begin
  if assigned(_name) then
   name:=_name^
  else
   name:='';
end;


{****************************************************************************
                               TDICTIONARY
****************************************************************************}

    constructor Tdictionary.init;
      begin
        root:=nil;
        hasharray:=nil;
        noclear:=false;
        replace_existing:=false;
      end;


    procedure Tdictionary.usehash;
      begin
        if not(assigned(root)) and
           not(assigned(hasharray)) then
         begin
           new(hasharray);
           fillchar(hasharray^,sizeof(hasharray^),0);
         end;
      end;


    destructor Tdictionary.done;
      begin
        if not noclear then
         clear;
        if assigned(hasharray) then
         dispose(hasharray);
      end;


    procedure Tdictionary.cleartree(obj:Pnamedindexobject);
      begin
        if assigned(obj^.left) then
          cleartree(obj^.left);
        if assigned(obj^.right) then
          cleartree(obj^.right);
        dispose(obj,done);
        obj:=nil;
      end;


    procedure Tdictionary.clear;
      var
        w : longint;
      begin
        if assigned(root) then
          cleartree(root);
        if assigned(hasharray) then
         for w:=-hasharraysize to hasharraysize do
          if assigned(hasharray^[w]) then
           cleartree(hasharray^[w]);
      end;

    function Tdictionary.delete(const s:string):Pnamedindexobject;

    var p,speedvalue:longint;
        n:Pnamedindexobject;

        procedure insert_right_bottom(var root,Atree:Pnamedindexobject);

        begin
            while root^.right<>nil do
                root:=root^.right;
            root^.right:=Atree;
        end;

        function delete_from_tree(root:Pnamedindexobject):Pnamedindexobject;

        type    leftright=(left,right);

        var lr:leftright;
            oldroot:Pnamedindexobject;

        begin
            oldroot:=nil;
            while (root<>nil) and (root^.speedvalue<>speedvalue) do
                begin
                    oldroot:=root;
                    if speedvalue<root^.speedvalue then
                        begin
                            root:=root^.right;
                            lr:=right;
                        end
                    else
                        begin
                            root:=root^.left;
                            lr:=left;
                        end;
                end;
            while (root<>nil) and (root^._name^<>s) do
                begin
                    oldroot:=root;
                    if s<root^._name^ then
                        begin
                            root:=root^.right;
                            lr:=right;
                        end
                    else
                        begin
                            root:=root^.left;
                            lr:=left;
                        end;
                end;
            if (oldroot=nil) or (root=nil) then
                do_internalerror(218); {Internalerror is not available...}
            if root^.left<>nil then
                begin
                    {Now the node pointing to root must point to the left
                     subtree of root. The right subtree of root must be
                     connected to the right bottom of the left subtree.}
                    if lr=left then
                        oldroot^.left:=root^.left
                    else
                        oldroot^.right:=root^.left;
                    if root^.right<>nil then
                        insert_right_bottom(root^.left,root^.right);
                end
            else
                {There is no left subtree. So we can just replace the node to
                 delete with the right subtree.}
                if lr=left then
                    oldroot^.left:=root^.right
                else
                    oldroot^.right:=root^.right;
            delete_from_tree:=root;
        end;

    begin
        speedvalue:=getspeedvalue(s);
        n:=root;
        if assigned(hasharray) then
            begin
                {First, check if the node to delete directly located under
                 the hasharray.}
                p:=speedvalue mod hasharraysize;
                n:=hasharray^[p];
                if (n<>nil) and (n^.speedvalue=speedvalue) and
                 (n^._name^=s) then
                    begin
                        {The node to delete is directly located under the
                         hasharray. Make the hasharray point to the left
                         subtree of the node and place the right subtree on
                         the right-bottom of the left subtree.}
                        if n^.left<>nil then
                            begin
                                hasharray^[p]:=n^.left;
                                if n^.right<>nil then
                                    insert_right_bottom(n^.left,n^.right);
                            end
                        else
                            hasharray^[p]:=n^.right;
                        delete:=n;
                        exit;
                    end;
            end
        else
            begin
                {First check if the node to delete is the root.}
                if (root<>nil) and (n^.speedvalue=speedvalue)
                 and (n^._name^=s) then
                    begin
                        if n^.left<>nil then
                            begin
                                root:=n^.left;
                                if n^.right<>nil then
                                    insert_right_bottom(n^.left,n^.right);
                            end
                        else
                            root:=n^.right;
                        delete:=n;
                        exit;
                    end;
            end;
        delete:=delete_from_tree(n);
    end;

    function Tdictionary.empty:boolean;
      var
        w : longint;
      begin
        if assigned(hasharray) then
         begin
           empty:=false;
           for w:=-hasharraysize to hasharraysize do
            if assigned(hasharray^[w]) then
             exit;
           empty:=true;
         end
        else
         empty:=(root=nil);
      end;


    procedure Tdictionary.foreach(proc2call:Tnamedindexcallback);

        procedure a(p:Pnamedindexobject);
        begin
          proc2call(p);
          if assigned(p^.left) then
           a(p^.left);
          if assigned(p^.right) then
           a(p^.right);
        end;

      var
        i : longint;
      begin
        if assigned(hasharray) then
         begin
           for i:=-hasharraysize to hasharraysize do
            if assigned(hasharray^[i]) then
             a(hasharray^[i]);
         end
        else
         if assigned(root) then
          a(root);
      end;


    function Tdictionary.insert(obj:Pnamedindexobject):Pnamedindexobject;
      begin
        obj^.speedvalue:=getspeedvalue(obj^._name^);
        if assigned(hasharray) then
         insert:=insertnode(obj,hasharray^[obj^.speedvalue mod hasharraysize])
        else
         insert:=insertnode(obj,root);
      end;


    function tdictionary.insertnode(newnode:Pnamedindexobject;var currnode:Pnamedindexobject):Pnamedindexobject;
      begin
        if currnode=nil then
         begin
           currnode:=newnode;
           insertnode:=newnode;
         end
        { first check speedvalue, to allow a fast insert }
        else
         if currnode^.speedvalue>newnode^.speedvalue then
          insertnode:=insertnode(newnode,currnode^.right)
        else
         if currnode^.speedvalue<newnode^.speedvalue then
          insertnode:=insertnode(newnode,currnode^.left)
        else
         begin
           if currnode^._name^>newnode^._name^ then
            insertnode:=insertnode(newnode,currnode^.right)
           else
            if currnode^._name^<newnode^._name^ then
             insertnode:=insertnode(newnode,currnode^.left)
           else
            begin
              if replace_existing and
                 assigned(currnode) then
                begin
                  newnode^.left:=currnode^.left;
                  newnode^.right:=currnode^.right;
                  currnode:=newnode;
                  insertnode:=newnode;
                end
              else
               insertnode:=currnode;
             end;
         end;
      end;


    procedure tdictionary.inserttree(currtree,currroot:Pnamedindexobject);
      begin
        if assigned(currtree) then
         begin
           inserttree(currtree^.left,currroot);
           inserttree(currtree^.right,currroot);
           currtree^.right:=nil;
           currtree^.left:=nil;
           insertnode(currtree,currroot);
         end;
      end;


    function tdictionary.rename(const olds,news : string):Pnamedindexobject;
      var
        spdval : longint;
        lasthp,
        hp,hp2,hp3 : Pnamedindexobject;
      begin
        spdval:=getspeedvalue(olds);
        if assigned(hasharray) then
         hp:=hasharray^[spdval mod hasharraysize]
        else
         hp:=root;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp^.speedvalue then
             begin
               lasthp:=hp;
               hp:=hp^.left
             end
            else
             if spdval<hp^.speedvalue then
              begin
                lasthp:=hp;
                hp:=hp^.right
              end
            else
             begin
               if (hp^.name=olds) then
                begin
                  { get in hp2 the replacer for the root or hasharr }
                  hp2:=hp^.left;
                  hp3:=hp^.right;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp^.right;
                     hp3:=hp^.left;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp^.left=hp then
                      lasthp^.left:=hp2
                     else
                      lasthp^.right:=hp2;
                   end
                  else
                   begin
                     if assigned(hasharray) then
                      hasharray^[spdval mod hasharraysize]:=hp2
                     else
                      root:=hp2;
                   end;
                  { reinsert the hp3 in the tree from hp2 }
                  inserttree(hp3,hp2);
                  { reset node with new values }
                  stringdispose(hp^._name);
                  hp^._name:=stringdup(news);
                  hp^.speedvalue:=getspeedvalue(news);
                  hp^.left:=nil;
                  hp^.right:=nil;
                  { reinsert }
                  if assigned(hasharray) then
                   rename:=insertnode(hp,hasharray^[hp^.speedvalue mod hasharraysize])
                  else
                   rename:=insertnode(hp,root);
                  exit;
                end
               else
                if olds>hp^.name then
                 begin
                   lasthp:=hp;
                   hp:=hp^.left
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp^.right;
                 end;
             end;
          end;
      end;


    function Tdictionary.search(const s:string):Pnamedindexobject;
      begin
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function Tdictionary.speedsearch(const s:string;speedvalue:longint):Pnamedindexobject;
      var
        newnode:Pnamedindexobject;
      begin
        if assigned(hasharray) then
         newnode:=hasharray^[speedvalue mod hasharraysize]
        else
         newnode:=root;
        while assigned(newnode) do
         begin
           if speedvalue>newnode^.speedvalue then
            newnode:=newnode^.left
           else
            if speedvalue<newnode^.speedvalue then
             newnode:=newnode^.right
           else
            begin
              if (newnode^._name^=s) then
               begin
                 speedsearch:=newnode;
                 exit;
               end
              else
               if s>newnode^._name^ then
                newnode:=newnode^.left
              else
               newnode:=newnode^.right;
            end;
         end;
        speedsearch:=nil;
      end;


{****************************************************************************
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.init(Aelemlen,Agrow:longint);
      begin
        posn:=0;
        count:=0;
        limit:=0;
        data:=nil;
        elemlen:=Aelemlen;
        growcount:=Agrow;
        grow;
      end;

    function  tdynamicarray.size:longint;
      begin
        size:=limit*elemlen;
      end;

    function  tdynamicarray.usedsize:longint;
      begin
        usedsize:=count*elemlen;
      end;

    procedure tdynamicarray.grow;
      var
        osize : longint;
        odata : pchar;
      begin
        osize:=size;
        odata:=data;
        inc(limit,growcount);
        getmem(data,size);
        if assigned(odata) then
         begin
           move(odata^,data^,osize);
           freemem(odata,osize);
         end;
        fillchar(data[osize],growcount*elemlen,0);
      end;

    procedure tdynamicarray.align(i:longint);
      var
        j : longint;
      begin
        j:=(posn*elemlen mod i);
        if j<>0 then
         begin
           j:=i-j;
           while limit<(posn+j) do
            grow;
           inc(posn,j);
           if (posn>count) then
            count:=posn;
         end;
      end;

    procedure tdynamicarray.seek(i:longint);
      begin
        while limit<i do
         grow;
        posn:=i;
        if (posn>count) then
         count:=posn;
      end;

    procedure tdynamicarray.write(var d;len:longint);
      begin
        while limit<(posn+len) do
         grow;
        move(d,data[posn*elemlen],len*elemlen);
        inc(posn,len);
        if (posn>count) then
         count:=posn;
      end;

    procedure tdynamicarray.read(var d;len:longint);
      begin
        move(data[posn*elemlen],d,len*elemlen);
        inc(posn,len);
        if (posn>count) then
         count:=posn;
      end;

    procedure tdynamicarray.writepos(pos:longint;var d;len:longint);
      begin
        while limit<(pos+len) do
         grow;
        move(d,data[pos*elemlen],len*elemlen);
        posn:=pos+len;
        if (posn>count) then
         count:=posn;
      end;

    procedure tdynamicarray.readpos(pos:longint;var d;len:longint);
      begin
        while limit<(pos+len) do
         grow;
        move(data[pos*elemlen],d,len*elemlen);
        posn:=pos+len;
        if (posn>count) then
         count:=posn;
      end;

    destructor tdynamicarray.done;
      begin
        if assigned(data) then
         freemem(data,size);
      end;


{****************************************************************************
                               tindexarray
 ****************************************************************************}


    constructor tindexarray.init(Agrowsize:longint);
      begin
        growsize:=Agrowsize;
        size:=0;
        count:=0;
        data:=nil;
        first:=nil;
      end;

    destructor tindexarray.done;
      begin
        if assigned(data) then
          begin
             clear;
             freemem(data,size*4);
             data:=nil;
          end;
      end;

    function tindexarray.search(nr:longint):Pnamedindexobject;
      begin
        if nr<=count then
         search:=data^[nr]
        else
         search:=nil;
      end;


    procedure tindexarray.clear;
      var
        i : longint;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          begin
            dispose(data^[i],done);
            data^[i]:=nil;
          end;
        count:=0;
        first:=nil;
      end;


    procedure tindexarray.foreach(proc2call : Tnamedindexcallback);
      var
        i : longint;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i]);
      end;


    procedure tindexarray.grow(gsize:longint);
      var
        osize : longint;
        odata : Pnamedindexobjectarray;
      begin
        osize:=size;
        odata:=data;
        inc(size,gsize);
        getmem(data,size*4);
        if assigned(odata) then
         begin
           move(odata^,data^,osize*4);
           freemem(odata,osize*4);
         end;
        fillchar(data^[osize+1],gsize*4,0);
      end;


    procedure tindexarray.deleteindex(p:Pnamedindexobject);
      var
        i : longint;
      begin
        i:=p^.indexnr;
        { update counter }
        if i=count then
         dec(count);
        { update linked list }
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i]^.next:=data^[p^.indexnr]^.next;
              break;
            end;
         end;
        if i=0 then
         first:=p^.next;
        data^[p^.indexnr]:=nil;
        { clear entry }
        p^.indexnr:=-1;
        p^.next:=nil;
      end;


    procedure tindexarray.delete(p:Pnamedindexobject);
      begin
        deleteindex(p);
        dispose(p,done);
        p:=nil;
      end;


    procedure tindexarray.insert(p:Pnamedindexobject);
      var
        i  : longint;
      begin
        if p^.indexnr=-1 then
         begin
           inc(count);
           p^.indexnr:=count;
         end;
        if p^.indexnr>count then
         count:=p^.indexnr;
        if count>size then
         grow(((count div growsize)+1)*growsize);
        data^[p^.indexnr]:=p;
        { update linked list backward }
        i:=p^.indexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i]^.next:=p;
              break;
            end;
         end;
        if i=0 then
         first:=p;
        { update linked list forward }
        i:=p^.indexnr;
        while (i<=count) do
         begin
           inc(i);
           if (i<=count) and assigned(data^[i]) then
            begin
              p^.next:=data^[i];
              exit;
            end;
         end;
        if i>count then
         p^.next:=nil;
      end;


{$ifdef BUFFEREDFILE}

{****************************************************************************
                               TBUFFEREDFILE
 ****************************************************************************}

    Const
       crcseed = $ffffffff;

       crctable : array[0..255] of longint = (
          $00000000,$77073096,$ee0e612c,$990951ba,$076dc419,$706af48f,
          $e963a535,$9e6495a3,$0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988,
          $09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91,$1db71064,$6ab020f2,
          $f3b97148,$84be41de,$1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,
          $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,
          $fa0f3d63,$8d080df5,$3b6e20c8,$4c69105e,$d56041e4,$a2677172,
          $3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,$35b5a8fa,$42b2986c,
          $dbbbc9d6,$acbcf940,$32d86ce3,$45df5c75,$dcd60dcf,$abd13d59,
          $26d930ac,$51de003a,$c8d75180,$bfd06116,$21b4f4b5,$56b3c423,
          $cfba9599,$b8bda50f,$2802b89e,$5f058808,$c60cd9b2,$b10be924,
          $2f6f7c87,$58684c11,$c1611dab,$b6662d3d,$76dc4190,$01db7106,
          $98d220bc,$efd5102a,$71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433,
          $7807c9a2,$0f00f934,$9609a88e,$e10e9818,$7f6a0dbb,$086d3d2d,
          $91646c97,$e6635c01,$6b6b51f4,$1c6c6162,$856530d8,$f262004e,
          $6c0695ed,$1b01a57b,$8208f4c1,$f50fc457,$65b0d9c6,$12b7e950,
          $8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65,
          $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,$4adfa541,$3dd895d7,
          $a4d1c46d,$d3d6f4fb,$4369e96a,$346ed9fc,$ad678846,$da60b8d0,
          $44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9,$5005713c,$270241aa,
          $be0b1010,$c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
          $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,$2eb40d81,
          $b7bd5c3b,$c0ba6cad,$edb88320,$9abfb3b6,$03b6e20c,$74b1d29a,
          $ead54739,$9dd277af,$04db2615,$73dc1683,$e3630b12,$94643b84,
          $0d6d6a3e,$7a6a5aa8,$e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,
          $f00f9344,$8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,
          $196c3671,$6e6b06e7,$fed41b76,$89d32be0,$10da7a5a,$67dd4acc,
          $f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,$d6d6a3e8,$a1d1937e,
          $38d8c2c4,$4fdff252,$d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b,
          $d80d2bda,$af0a1b4c,$36034af6,$41047a60,$df60efc3,$a867df55,
          $316e8eef,$4669be79,$cb61b38c,$bc66831a,$256fd2a0,$5268e236,
          $cc0c7795,$bb0b4703,$220216b9,$5505262f,$c5ba3bbe,$b2bd0b28,
          $2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d,
          $9b64c2b0,$ec63f226,$756aa39c,$026d930a,$9c0906a9,$eb0e363f,
          $72076785,$05005713,$95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,
          $92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21,$86d3d2d4,$f1d4e242,
          $68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,$18b74777,
          $88085ae6,$ff0f6a70,$66063bca,$11010b5c,$8f659eff,$f862ae69,
          $616bffd3,$166ccf45,$a00ae278,$d70dd2ee,$4e048354,$3903b3c2,
          $a7672661,$d06016f7,$4969474d,$3e6e77db,$aed16a4a,$d9d65adc,
          $40df0b66,$37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
          $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,$cdd70693,
          $54de5729,$23d967bf,$b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94,
          $b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d);

    constructor tbufferedfile.init(const filename : string;_bufsize : longint);

      begin
         assign(f,filename);
         bufsize:=_bufsize;
         bufpos:=0;
         buflast:=0;
         do_crc:=false;
         iomode:=0;
         tempclosed:=false;
         change_endian:=false;
         clear_crc;
      end;

    destructor tbufferedfile.done;

      begin
         close;
      end;

    procedure tbufferedfile.clear_crc;

      begin
         crc:=crcseed;
      end;

    procedure tbufferedfile.setbuf(p : pchar;s : longint);

      begin
         flush;
         freemem(buf,bufsize);
         bufsize:=s;
         buf:=p;
      end;

    function tbufferedfile.reset:boolean;

      var
         ofm : byte;
      begin
         ofm:=filemode;
         iomode:=1;
         getmem(buf,bufsize);
         filemode:=0;
         {$I-}
          system.reset(f,1);
         {$I+}
         reset:=(ioresult=0);
         filemode:=ofm;
      end;

    procedure tbufferedfile.rewrite;

      begin
         iomode:=2;
         getmem(buf,bufsize);
         system.rewrite(f,1);
      end;

    procedure tbufferedfile.flush;

      var
{$ifdef FPC}
         count : longint;
{$else}
         count : integer;
{$endif}

      begin
         if iomode=2 then
           begin
              if bufpos=0 then
                exit;
              blockwrite(f,buf^,bufpos)
           end
         else if iomode=1 then
            if buflast=bufpos then
              begin
                 blockread(f,buf^,bufsize,count);
                 buflast:=count;
              end;
         bufpos:=0;
      end;

    function tbufferedfile.getftime : longint;

      var
         l : longint;
{$ifdef linux}
         Info : Stat;
{$endif}
      begin
{$ifndef linux}
         { this only works if the file is open !! }
         dos.getftime(f,l);
{$else}
         Fstat(f,Info);
         l:=info.mtime;
{$endif}
         getftime:=l;
      end;

    function tbufferedfile.getsize : longint;

      begin
        getsize:=filesize(f);
      end;

    procedure tbufferedfile.seek(l : longint);

      begin
         if iomode=2 then
           begin
              flush;
              system.seek(f,l);
           end
         else if iomode=1 then
           begin
              { forces a reload }
              bufpos:=buflast;
              system.seek(f,l);
              flush;
           end;
      end;

    type
{$ifdef tp}
       bytearray1 = array [1..65535] of byte;
{$else}
       bytearray1 = array [1..10000000] of byte;
{$endif}

    procedure tbufferedfile.read_data(var data;bytes : longint;var count : longint);

      var
         p : pchar;
         c,i : longint;

      begin
         p:=pchar(@data);
         count:=0;
         while bytes-count>0 do
           begin
              if bytes-count>buflast-bufpos then
                begin
                   move((buf+bufpos)^,(p+count)^,buflast-bufpos);
                   inc(count,buflast-bufpos);
                   bufpos:=buflast;
                   flush;
                   { can't we read anything ? }
                   if bufpos=buflast then
                     break;
                end
              else
                begin
                   move((buf+bufpos)^,(p+count)^,bytes-count);
                   inc(bufpos,bytes-count);
                   count:=bytes;
                   break;
                end;
           end;
         if do_crc then
           begin
              c:=crc;
              for i:=1 to bytes do
              c:=(c shr 8) xor crctable[byte(c) xor (bytearray1(data)[i])];
              crc:=c;
           end;
      end;

    procedure tbufferedfile.write_data(var data;count : longint);

      var
         c,i : longint;

      begin
         if bufpos+count>bufsize then
           flush;
         move(data,(buf+bufpos)^,count);
         inc(bufpos,count);
         if do_crc then
           begin
              c:=crc;
              for i:=1 to count do
                c:=(c shr 8) xor crctable[byte(c) xor (bytearray1(data)[i])];
              crc:=c;
           end;
      end;

    function tbufferedfile.getcrc : longint;

      begin
         getcrc:=crc xor crcseed;
      end;

    procedure tbufferedfile.write_string(const s : string);

      begin
        if bufpos+length(s)>bufsize then
          flush;
        { why is there not CRC here ??? }
        move(s[1],(buf+bufpos)^,length(s));
        inc(bufpos,length(s));
         { should be
        write_data(s[1],length(s)); }
      end;

    procedure tbufferedfile.write_pchar(p : pchar);

      var
         l : longint;

      begin
        l:=strlen(p);
        if l>=bufsize then
          do_internalerror(222);
        { why is there not CRC here ???}
        if bufpos+l>bufsize then
          flush;
        move(p^,(buf+bufpos)^,l);
        inc(bufpos,l);
         { should be
        write_data(p^,l); }
      end;

    procedure tbufferedfile.write_byte(b : byte);

      begin
         write_data(b,sizeof(byte));
      end;

    procedure tbufferedfile.write_long(l : longint);

      var
         w1,w2 : word;

      begin
         if change_endian then
           begin
              w1:=l and $ffff;
              w2:=l shr 16;
              l:=swap(w2)+(longint(swap(w1)) shl 16);
           end;
         write_data(l,sizeof(longint));
      end;

    procedure tbufferedfile.write_word(w : word);

      begin
         if change_endian then
           begin
              w:=swap(w);
           end;
         write_data(w,sizeof(word));
      end;

    procedure tbufferedfile.write_double(d : double);

      begin
         write_data(d,sizeof(double));
      end;

    function tbufferedfile.getpath : string;

      begin
{$ifdef dummy}
         getpath:=strpas(filerec(f).name);
{$endif}
         getpath:='';
      end;

    procedure tbufferedfile.close;

      begin
         if iomode<>0 then
           begin
              flush;
              system.close(f);
              freemem(buf,bufsize);
              buf:=nil;
              iomode:=0;
           end;
      end;

    procedure tbufferedfile.tempclose;

      begin
        if iomode<>0 then
         begin
           temppos:=system.filepos(f);
           tempmode:=iomode;
           tempclosed:=true;
           system.close(f);
           iomode:=0;
         end
        else
         tempclosed:=false;
      end;

    procedure tbufferedfile.tempreopen;

      var
         ofm : byte;

      begin
         if tempclosed then
           begin
              case tempmode of
               1 : begin
                     ofm:=filemode;
                     iomode:=1;
                     filemode:=0;
                     system.reset(f,1);
                     filemode:=ofm;
                   end;
               2 : begin
                     iomode:=2;
                     system.rewrite(f,1);
                   end;
              end;
              system.seek(f,temppos);
              tempclosed:=false;
           end;
      end;

{$endif BUFFEREDFILE}

end.
{
  $Log$
  Revision 1.49  1999-12-22 01:01:48  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.48  1999/12/06 18:21:03  peter
    * support !ENVVAR for long commandlines
    * win32/go32v2 write short pathnames to link.res so c:\Program Files\ is
      finally supported as installdir.

  Revision 1.47  1999/11/15 14:59:55  pierre
   * last was not handled correctly in TStringQueue

  Revision 1.46  1999/11/14 15:56:36  peter
    * fixed stringqueue.delete

  Revision 1.45  1999/11/12 11:03:49  peter
    * searchpaths changed to stringqueue object

  Revision 1.44  1999/11/06 14:34:20  peter
    * truncated log to 20 revs

  Revision 1.43  1999/10/26 12:30:41  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.42  1999/09/07 15:08:51  pierre
   * runerror => do_internalerror

  Revision 1.41  1999/08/24 13:13:57  peter
    * MEMDEBUG to see the sizes of asmlist,asmsymbols,symtables

  Revision 1.40  1999/08/12 23:19:05  pierre
   * added inherited init call to tstringcontainer.init_no_double for Peter

  Revision 1.39  1999/08/05 14:58:07  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.38  1999/07/18 10:19:46  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.37  1999/07/03 00:29:45  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.36  1999/06/23 11:13:20  peter
    * fixed linebreak

  Revision 1.35  1999/06/23 11:07:23  daniel
  * Tdictionary.delete

  Revision 1.33.2.1  1999/06/15 10:12:22  peter
    * fixed inserttree which didn't reset left,right

  Revision 1.33  1999/05/31 23:33:21  peter
    * fixed tdictionary rename which didn't reset left,right when
      reinserting

  Revision 1.32  1999/05/27 19:44:23  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.31  1999/05/21 13:54:59  peter
    * NEWLAB for label as symbol

  Revision 1.30  1999/05/21 10:38:59  peter
    * fixed deleteindex which didn't reset indexnr and set first wrong

  Revision 1.29  1999/05/08 19:47:27  peter
    * indexarray.delete resets pointer after dispose

  Revision 1.28  1999/05/05 10:05:48  florian
    * a delphi compiled compiler recompiles ppc

  Revision 1.27  1999/05/05 09:19:03  florian
    * more fixes to get it with delphi running

  Revision 1.26  1999/04/21 09:43:31  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.25  1999/04/15 10:01:44  peter
    * small update for storenumber

  Revision 1.24  1999/04/14 09:14:47  peter
    * first things to store the symbol/def number in the ppu

}
