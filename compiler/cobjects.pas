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

  interface

    uses
       strings
{$ifndef linux}
       ,dos
{$else}
       ,linux
{$endif}
      ;

    type
       pstring = ^string;

       tfileposinfo = record
         line : longint; { could be changed to abspos }
         fileindex,column : word;
       end;
       pfileposinfo = ^tfileposinfo;

       { some help data types }
       pstringitem = ^tstringitem;

       tstringitem = record
          data : pstring;
          next : pstringitem;
{$ifdef UseTokenInfo}
          fileinfo : tfileposinfo; { pointer to tinputfile }
{$endif UseTokenInfo}
       end;

       plinkedlist_item = ^tlinkedlist_item;

       tlinkedlist_item = object
          next,previous : plinkedlist_item;
          { does nothing }
          constructor init;
          destructor done;virtual;
       end;

       pstring_item = ^tstring_item;

       tstring_item = object(tlinkedlist_item)
          str : pstring;
          constructor init(const s : string);
          destructor done;virtual;
       end;

       plinkedlist = ^tlinkedlist;

       { this implements a double linked list }
       tlinkedlist = object
          first,last : plinkedlist_item;
          constructor init;
          destructor done;

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

          { removes p from the list (p isn't disposed) }
          { it's not tested if p is in the list !      }
          procedure remove(p : plinkedlist_item);
       end;

       { String Queue}
       PStringQueue=^TStringQueue;
       TStringQueue=object
         first,last : PStringItem;
         constructor Init;
         destructor Done;
         function Empty:boolean;
         function Get:string;
         procedure Insert(const s:string);
         procedure Concat(const s:string);
         procedure Clear;
       end;

       { string container }
       pstringcontainer = ^tstringcontainer;

       tstringcontainer = object
          root,last : pstringitem;

          { if this is set to true, doubles are allowed }
          { true is default                             }
          doubles : boolean;
          constructor init;
          destructor done;

          { true is the container empty }
          function empty:boolean;


          { inserts a string }
          procedure insert(const s : string);
{$ifdef UseTokenInfo}
          procedure insert_with_tokeninfo(const s : string;const file_info : tfileposinfo);
{$endif UseTokenInfo}

          { gets a string }
          function get : string;
{$ifdef UseTokenInfo}
    function get_with_tokeninfo(var file_info : tfileposinfo) : string;
{$endif UseTokenInfo}

          { deletes all strings }
          procedure clear;
       end;

       pbufferedfile = ^tbufferedfile;

       { this is implemented to allow buffered binary I/O }
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

           { inits a buffer with the size bufsize which is assigned to }
           { the file  filename                                        }
           constructor init(const filename : string;_bufsize : longint);

           { closes the file, if needed, and releases the memory }
           destructor done;virtual;

           { opens the file for input, other accesses are rejected }
           procedure reset;

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
    function strpnew(const s : string) : pchar;

    { makes a char lowercase, with spanish, french and german char set }
    function lowercase(c : char) : char;

    { makes zero terminated string to a pascal string }
    { the data in p is modified and p is returned     }
    function pchar2pstring(p : pchar) : pstring;

    { ambivalent to pchar2pstring }
    function pstring2pchar(p : pstring) : pchar;

  implementation

    function pchar2pstring(p : pchar) : pstring;

      var
         w : word;
         i : longint;

      begin
         w:=strlen(p);
         for i:=w-1 downto 0 do
           p[i+1]:=p[i];
         p[0]:=chr(w);
         pchar2pstring:=pstring(p);
      end;

    function pstring2pchar(p : pstring) : pchar;

      var
         w : word;
         i : longint;

      begin
         w:=ord(p^[0]);
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
end;


function TStringQueue.Empty:boolean;
begin
  Empty:=(first=nil);
end;


function TStringQueue.Get:string;
var
  hp : pstringitem;
begin
  if first=nil then
   begin
     Get:='';
     exit;
   end;
  Get:=first^.data^;
  stringdispose(first^.data);
  hp:=first;
  first:=first^.next;
  dispose(hp);
end;


procedure TStringQueue.Insert(const s:string);
var
  hp : pstringitem;
begin
  new(hp);
  hp^.next:=first;
  hp^.data:=stringdup(s);
  first:=hp;
  if last=nil then
   last:=hp;
end;


procedure TStringQueue.Concat(const s:string);
var
  hp : pstringitem;
begin
  new(hp);
  hp^.next:=nil;
  hp^.data:=stringdup(s);
  if first=nil then
   first:=hp
  else
   last^.next:=hp;
  last:=hp;
end;


procedure TStringQueue.Clear;
var
  hp : pstringitem;
begin
  while (first<>nil) do
   begin
     hp:=first;
     stringdispose(first^.data);
     first:=first^.next;
     dispose(hp);
   end;
end;


destructor TStringQueue.Done;
begin
  Clear;
end;

{****************************************************************************
                           TSTRINGCONTAINER
 ****************************************************************************}

    constructor tstringcontainer.init;

      begin
         root:=nil;
         last:=nil;
         doubles:=true;
      end;

    destructor tstringcontainer.done;

      begin
         clear;
      end;

    function tstringcontainer.empty:boolean;


      begin
        empty:=(root=nil);
      end;


    procedure tstringcontainer.insert(const s : string);

      var
         hp : pstringitem;

      begin
         if not(doubles) then
           begin
              hp:=root;
              while assigned(hp) do
                begin
                   if hp^.data^=s then exit;
                   hp:=hp^.next;
                end;
           end;
         new(hp);
         hp^.next:=nil;
         hp^.data:=stringdup(s);
         if root=nil then root:=hp
           else last^.next:=hp;
         last:=hp;
      end;

{$ifdef UseTokenInfo}
          procedure tstringcontainer.insert_with_tokeninfo
            (const s : string; const file_info : tfileposinfo);

      var
         hp : pstringitem;

      begin
         if not(doubles) then
           begin
              hp:=root;
              while assigned(hp) do
                begin
                   if hp^.data^=s then exit;
                   hp:=hp^.next;
                end;
           end;
         new(hp);
         hp^.next:=nil;
         hp^.data:=stringdup(s);
         hp^.fileinfo:=file_info;
         if root=nil then root:=hp
           else last^.next:=hp;
         last:=hp;
      end;

{$endif UseTokenInfo}
    procedure tstringcontainer.clear;

      var
         hp : pstringitem;

      begin
         hp:=root;
         while assigned(hp) do
           begin
              stringdispose(hp^.data);
              root:=hp^.next;
              dispose(hp);
              hp:=root;
           end;
         last:=nil;
         root:=nil;
      end;

    function tstringcontainer.get : string;

      var
         hp : pstringitem;

      begin
         if root=nil then
          get:=''
         else
          begin
            get:=root^.data^;
            hp:=root;
            root:=root^.next;
            stringdispose(hp^.data);
            dispose(hp);
          end;
      end;

{$ifdef UseTokenInfo}
    function tstringcontainer.get_with_tokeninfo(var file_info : tfileposinfo) : string;

      var
         hp : pstringitem;

      begin
         if root=nil then
          begin
             get_with_tokeninfo:='';
             file_info.fileindex:=0;
             file_info.line:=0;
             file_info.column:=0;
          end
         else
          begin
            get_with_tokeninfo:=root^.data^;
            hp:=root;
            root:=root^.next;
            stringdispose(hp^.data);
            file_info:=hp^.fileinfo;
            dispose(hp);
          end;
      end;
{$endif UseTokenInfo}

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

    procedure tlinkedlist.clear;

      var
         hp : plinkedlist_item;

      begin
         hp:=first;
         while assigned(hp) do
           begin
              first:=hp^.next;
              dispose(hp,done);
              hp:=first;
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
         p^.previous:=nil;
         p^.next:=nil;
         if not(assigned(first)) then
           first:=p
           else
             begin
                last^.next:=p;
                p^.previous:=last;
             end;
         last:=p;
      end;

    procedure tlinkedlist.insert(p : plinkedlist_item);

      begin
         p^.previous:=nil;
         p^.next:=nil;
         if not(assigned(first)) then
           last:=p
         else
           begin
              first^.previous:=p;
              p^.next:=first;
              first:=p;
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

    procedure tbufferedfile.reset;

      var
         ofm : byte;
      begin
         ofm:=filemode;
         iomode:=1;
         getmem(buf,bufsize);
         filemode:=0;
         system.reset(f,1);
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
        move(s[1],(buf+bufpos)^,length(s));
        inc(bufpos,length(s));
      end;

    procedure tbufferedfile.write_pchar(p : pchar);

      var
         l : longint;

      begin
        l:=strlen(p);
        if l>=bufsize then
          runerror(222);
        if bufpos+l>bufsize then
          flush;
        move(p^,(buf+bufpos)^,l);
        inc(bufpos,l);
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
              write_data(l,sizeof(longint))
           end
         else
           write_data(l,sizeof(longint))
      end;

    procedure tbufferedfile.write_word(w : word);

      begin
         if change_endian then
           begin
              w:=swap(w);
              write_data(w,sizeof(word))
           end
         else
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
              iomode:=0;
           end;
      end;

end.
{
  $Log$
  Revision 1.5  1998-04-30 15:59:40  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/29 10:33:50  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.2  1998/04/07 11:09:04  peter
    + filemode is set correct in tbufferedfile.reset

  Revision 1.1.1.1  1998/03/25 11:18:15  root
  * Restored version

  Revision 1.15  1998/03/10 16:27:38  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.14  1998/03/10 01:17:18  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.13  1998/03/04 17:33:42  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.12  1998/03/02 01:48:31  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.11  1998/02/28 00:20:22  florian
    * more changes to get import libs for Win32 working

  Revision 1.10  1998/02/24 14:20:50  peter
    + tstringcontainer.empty
    * ld -T option restored for linux
    * libraries are placed before the objectfiles in a .PPU file
    * removed 'uses link' from files.pas

  Revision 1.9  1998/02/18 13:48:17  michael
  + Implemented an OS independent AsmRes object.

  Revision 1.8  1998/02/17 21:20:45  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.7  1998/02/13 10:34:55  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.6  1998/02/12 11:50:01  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.5  1998/02/06 23:08:32  florian
    + endian to targetinfo and sourceinfo added
    + endian independed writing of ppu file (reading missed), a PPU file
      is written with the target endian

  Revision 1.4  1998/01/13 17:11:34  michael
  * Changed getftime method to work faster under linux.

  Revision 1.3  1997/12/05 13:45:34  daniel
  - Removed overlay init. This is done by PPOVIN.PAS.

  Revision 1.2  1997/11/28 18:14:28  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:32:55  michael
  FPC Compiler CVS start

 Pre-CVS log:

 History:
      30th september 1996:
         + english comments (FK)
         + _2pchar renamed to pstring2pchar (FK)
         + _2pstring renamed to pchar2pstring (FK)
      15th october 1996:
         + tstringcontainer is compilable (FK)
         + full compilable (FK)
       4th january 1996:
         + tstring_item added (FK)
      19th november 1997:
         + call of overlay init (FK)
}
