{
    $Id$
    Copyright (c) 1998-2000 by Michael van Canneyt

    Handles resourcestrings

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
unit cresstr;

interface

  Procedure ResetResourceStrings;
  Procedure InsertResourceStrings;
  Function registerresourcestring(Const name : string;p : pchar;len : longint) : longint;
  Function calc_resstring_hashvalue(P : Pchar; Len : longint) : longint;
  Procedure WriteResourceFile(FileName : String);

implementation

uses
   globals,aasm,verbose,files;

Type
  { These are used to form a singly-linked list, ordered by hash value }
  PResourcestring = ^TResourceString;
  TResourceString = record
    Name : String;
    Value : Pchar;
    Len,hash : longint;
    Next : PResourcestring;
    end;

const
   { we can use a static constant because we compile a program only once }
   { per compiler call                                                   }
   resstrcount : longint = 0;
   resourcefilename = 'resource.rst';

Var
  ResourceListRoot : PResourceString;
  ResourceListCurrent : PResourceString;

{ ---------------------------------------------------------------------
   Calculate hash value, based on the string
  ---------------------------------------------------------------------}

function calc_resstring_hashvalue(P : Pchar; Len : longint) : longint;

  Var hash,g,I : longint;

  begin
     hash:=0;
     For I:=0 to Len-1 do { 0 terminated }
       begin
       hash:=hash shl 4;
       inc(Hash,Ord(P[i]));
       g:=hash and ($f shl 28);
       if g<>0 then
         begin
         hash:=hash xor (g shr 24);
         hash:=hash xor g;
         end;
       end;
     If Hash=0 then
       Calc_resstring_hashvalue:=Not(0)
     else
       calc_resstring_hashvalue:=Hash;
  end;


{ ---------------------------------------------------------------------
    Append 1 resourcestring to the linked list of resource strings.
  ---------------------------------------------------------------------}

Function AppendToResourceList(const name : string;p : pchar;len,hash : longint) : longint;
begin
  If ResourceListCurrent<>Nil then
    begin
    New(ResourceListCurrent^.Next);
    ResourceListCurrent:=ResourceListCurrent^.Next;
    end
  else
    begin
    New(ResourceListCurrent);
    ResourceListRoot:=ResourceListCurrent;
    end;
  ResourceListCurrent^.Next:=Nil;
  { name is lower case... }
  ResourceListCurrent^.Name:=Name;
  ResourceListCurrent^.Len:=Len;
  ResourceListCurrent^.Hash:=hash;
  GetMem(ResourceListCurrent^.Value,Len);
  Move(P^,ResourceListCurrent^.Value^,Len);
  AppendToResourceList:=ResStrCount;
  inc(Resstrcount);
end;

{ ---------------------------------------------------------------------
    Append 1 resource string to the resourcestring asm list
  ---------------------------------------------------------------------}

Procedure AppendToAsmResList (P : PResourceString);

Var
 l1 : pasmlabel;
 s : pchar;
 l : longint;

begin
 With P^ Do
   begin
   if (Value=nil) or (len=0) then
     resourcestringlist^.concat(new(pai_const,init_32bit(0)))
   else
     begin
        getdatalabel(l1);
        resourcestringlist^.concat(new(pai_const_symbol,init(l1)));
        consts^.concat(new(pai_const,init_32bit(len)));
        consts^.concat(new(pai_const,init_32bit(len)));
        consts^.concat(new(pai_const,init_32bit(-1)));
        consts^.concat(new(pai_label,init(l1)));
        getmem(s,len+1);
        move(Value^,s^,len);
        s[len]:=#0;
        consts^.concat(new(pai_string,init_length_pchar(s,len)));
        consts^.concat(new(pai_const,init_8bit(0)));
     end;
   { append Current value (nil) and hash...}
   resourcestringlist^.concat(new(pai_const,init_32bit(0)));
   resourcestringlist^.concat(new(pai_const,init_32bit(hash)));
   { Append the name as a ansistring. }
   getdatalabel(l1);
   L:=Length(Name);
   resourcestringlist^.concat(new(pai_const_symbol,init(l1)));
   consts^.concat(new(pai_const,init_32bit(l)));
   consts^.concat(new(pai_const,init_32bit(l)));
   consts^.concat(new(pai_const,init_32bit(-1)));
   consts^.concat(new(pai_label,init(l1)));
   getmem(s,l+1);
   move(Name[1],s^,l);
   s[l]:=#0;
   consts^.concat(new(pai_string,init_length_pchar(s,l)));
   consts^.concat(new(pai_const,init_8bit(0)));
   end;
end;


{ ---------------------------------------------------------------------
    Create the full asmlist for resourcestrings.
  ---------------------------------------------------------------------}

procedure insertresourcestrings;

  Var R : PresourceString;

  begin
  if not(assigned(resourcestringlist)) then
    resourcestringlist:=new(paasmoutput,init);
  resourcestringlist^.insert(new(pai_const,init_32bit(resstrcount)));
  resourcestringlist^.insert(new(pai_symbol,initname_global(current_module^.modulename^+'_'+'RESOURCESTRINGLIST',0)));
  R:=ResourceListRoot;
  While R<>Nil do
    begin
    AppendToAsmResList(R);
    R:=R^.Next;
    end;
  resourcestringlist^.concat(new(pai_symbol_end,initname(current_module^.modulename^+'_'+'RESOURCESTRINGLIST')));

  end;

{ ---------------------------------------------------------------------
    Insert 1 resource string in all tables.
  ---------------------------------------------------------------------}

function  registerresourcestring(const name : string;p : pchar;len : longint) : longint;

var
   fullname : string;
   hash : longint;
begin
   { Calculate result }
   fullname:=lower(current_module^.modulename^+'.'+Name);
   hash:=calc_resstring_hashvalue(p,len);
   if not(assigned(resourcestringlist)) then
     resourcestringlist:=new(paasmoutput,init);
   registerresourcestring:=AppendToResourceList(fullname,P,Len,Hash);
end;

Procedure WriteResourceFile(Filename : String);

Type
   TMode = (quoted,unquoted);

Var F : Text;
    Mode : TMode;
    old : PresourceString;
    C : char;
    Col,i : longint;

   Procedure Add(Const S : String);

   begin
     Write(F,S);
     Col:=Col+length(s);
   end;

begin
  If (ResourceListRoot=Nil) then
    exit;
  FileName:=ForceExtension(lower(FileName),'.rst');
  message1 (general_i_writingresourcefile,filename);
  Assign(F,Filename);
  {$i-}
  Rewrite(f);
  {$i+}
  If IOresult<>0 then
    begin
    message(general_e_errorwritingresourcefile);
    exit;
    end;
  While ResourceListRoot<>Nil do
    With ResourceListRoot^ do
      begin
      writeln(f);
      Writeln (f,'# hash value = ',hash);
     col:=0;
     Add(Name+'=');
     Mode:=unquoted;
     For I:=0 to Len-1 do
       begin
       C:=Value[i];
       If (ord(C)>31) and (Ord(c)<=128) and (c<>'''') then
         begin
         If mode=Quoted then
           Add(c)
         else
           begin
           Add(''''+c);
           mode:=quoted
           end
         end
       else
         begin
         If Mode=quoted then
           begin
           Add('''');
           mode:=unquoted;
           end;
         Add('#'+tostr(ord(c)));
         end;
       If Col>72 then
         begin
         if mode=quoted then
           Write (F,'''');
         Writeln(F,'+');
         Col:=0;
         Mode:=unQuoted;
         end;
       end;
     if mode=quoted then writeln (f,'''');
     Writeln(f);
     Old :=ResourceListRoot;
     ResourceListRoot:=old^.Next;
     FreeMem(Old^.Value,Len);
     Dispose(Old);
     end;
   close(f);
end;


Procedure ResetResourceStrings;

Var R,T : PResourceString;

begin
  If ResourceStringList<>Nil then
    begin
    Dispose(ResourceStringlist,Done);
    ResourceStringList:=Nil;
    end;
  R:=ResourceListRoot;
  While R<>Nil do
    begin
    FreeMem(R^.Value,R^.Len);
    T:=R^.Next;
    Dispose(R);
    R:=T;
    end;
  ResStrCount:=0;
  ResourceListCurrent:=Nil;
  ResourceListRoot:=Nil;
end;

end.
{
  $Log$
  Revision 1.16  2000-01-07 01:14:23  peter
    * updated copyright to 2000

  Revision 1.15  1999/11/06 14:34:20  peter
    * truncated log to 20 revs

  Revision 1.14  1999/08/27 15:55:36  michael
  * Fixed small bug: next field in resourcelist was not initialized

  Revision 1.13  1999/08/26 20:24:39  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.12  1999/08/25 16:41:07  peter
    * resources are working again

  Revision 1.11  1999/08/23 11:48:23  michael
  * resourcestrings ams list needs unitname prepended

  Revision 1.10  1999/08/23 11:45:41  michael
  * Hopefully final attempt at resourcestrings

  Revision 1.9  1999/08/15 21:57:59  michael
  Changes for resource strings

  Revision 1.8  1999/07/29 20:54:01  peter
    * write .size also

  Revision 1.7  1999/07/26 09:42:00  florian
    * bugs 494-496 fixed

  Revision 1.6  1999/07/25 19:27:15  michael
  + Fixed hash computing, now compatible with gnu .mo file

  Revision 1.5  1999/07/24 18:35:41  michael
  * Forgot to add unitname to resourcestring data

  Revision 1.4  1999/07/24 16:22:10  michael
  + Improved resourcestring handling

  Revision 1.3  1999/07/24 15:12:58  michael
  changes for resourcestrings

  Revision 1.2  1999/07/22 20:04:58  michael
  + Added computehashvalue

  Revision 1.1  1999/07/22 09:34:04  florian
    + initial revision

}
