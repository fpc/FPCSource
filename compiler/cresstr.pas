{
    $Id$
    Copyright (c) 1999 by the Free Pascal development team

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

    procedure insertresourcestrings;
    procedure registerresourcestring(Const name : string;p : pchar;len,hash : longint);
    function calc_resstring_hashvalue(p : pchar;len : longint) : longint;
    Procedure WriteResourceFile(FileName : String);

  implementation

    uses
       globals,aasm,verbose,files;

    Type
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

    { calcs the hash value for a give resourcestring, len is }
    { necessary because the resourcestring can contain #0    }

    function calc_resstring_hashvalue(p : pchar;len : longint) : longint;

      Var hash,g,I : longint;

      begin
         hash:=0;
         For I:=0 to Len-1 do { 0 terminated }
           begin
           hash:=hash shl 4;
           inc(Hash,Ord(p[i]));
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

    procedure insertresourcestrings;

      begin
         if not(assigned(resourcestringlist)) then
           resourcestringlist:=new(paasmoutput,init);
         resourcestringlist^.insert(new(pai_const,init_32bit(resstrcount)));
         resourcestringlist^.insert(new(pai_symbol,initname_global('RESOURCESTRINGLIST',0)));
         resourcestringlist^.concat(new(pai_symbol_end,initname('RESOURCESTRINGLIST')));
      end;


    Procedure AppendToResourceList(const name : string;p : pchar;len,hash : longint);

    Var R : PResourceString;

    begin
      inc(resstrcount);
      New(R);
      R^.Name:=Lower(Name);
      r^.Len:=Len;
      R^.Hash:=hash;
      GetMem(R^.Value,Len);
      Move(P^,R^.Value^,Len);
      R^.Next:=ResourceListRoot;
      ResourceListRoot:=R;
    end;

    procedure registerresourcestring(const name : string;p : pchar;len,hash : longint);

      var
         l1 : pasmlabel;
         s : pchar;

      begin
         { we don't need to generate consts in units }
         if (main_module^.is_unit) then
           exit;

         if not(assigned(resourcestringlist)) then
           resourcestringlist:=new(paasmoutput,init);

         AppendToResourceList(current_module^.modulename^+'.'+Name,P,Len,Hash);

         { an empty ansi string is nil! }
         if (p=nil) or (len=0) then
           resourcestringlist^.concat(new(pai_const,init_32bit(0)))
         else
           begin
              getdatalabel(l1);
              resourcestringlist^.concat(new(pai_const_symbol,init(l1)));
              { first write the maximum size }
              consts^.concat(new(pai_const,init_32bit(len)));
              { second write the real length }
              consts^.concat(new(pai_const,init_32bit(len)));
              { redondent with maxlength but who knows ... (PM) }
              { third write use count (set to -1 for safety ) }
              consts^.concat(new(pai_const,init_32bit(-1)));
              consts^.concat(new(pai_label,init(l1)));
              getmem(s,len+1);
              move(p^,s^,len);
              s[len]:=#0;
              consts^.concat(new(pai_string,init_length_pchar(s,len)));
              consts^.concat(new(pai_const,init_8bit(0)));
           end;
         resourcestringlist^.concat(new(pai_const,init_32bit(0)));
         resourcestringlist^.concat(new(pai_const,init_32bit(hash)));
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
      If resstrCount=0 then
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

end.
{
  $Log$
  Revision 1.8  1999-07-29 20:54:01  peter
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