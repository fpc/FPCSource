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

uses
  cobjects;

Type
  { These are used to form a singly-linked list, ordered by hash value }
  PResourceStringItem = ^TResourceStringItem;
  TResourceStringItem = object(TLinkedList_Item)
    Name  : String;
    Value : Pchar;
    Len,
    hash  : longint;
    constructor Init(const AName:string;AValue:pchar;ALen:longint);
    destructor  Done;virtual;
    procedure CalcHash;
  end;

  PResourceStrings=^TResourceStrings;
  TResourceStrings=object
  private
    List : TLinkedList;
  public
    ResStrCount : longint;
    constructor Init;
    destructor  Done;
    function  Register(Const name : string;p : pchar;len : longint) : longint;
    procedure CreateResourceStringList;
    Procedure WriteResourceFile(FileName : String);
  end;

var
  ResourceStrings : PResourceStrings;


implementation

uses
   globals,aasm,verbose,files;


{ ---------------------------------------------------------------------
   Calculate hash value, based on the string
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
                          TRESOURCESTRING_ITEM
  ---------------------------------------------------------------------}

constructor TResourceStringItem.Init(const AName:string;AValue:pchar;ALen:longint);
begin
  inherited Init;
  Name:=AName;
  Len:=ALen;
  GetMem(Value,Len);
  Move(AValue^,Value^,Len);
  CalcHash;
end;


destructor TResourceStringItem.Done;
begin
  FreeMem(Value,Len);
end;


procedure TResourceStringItem.CalcHash;
Var
  g,I : longint;
begin
  hash:=0;
  For I:=0 to Len-1 do { 0 terminated }
   begin
     hash:=hash shl 4;
     inc(Hash,Ord(Value[i]));
     g:=hash and ($f shl 28);
     if g<>0 then
      begin
        hash:=hash xor (g shr 24);
        hash:=hash xor g;
      end;
   end;
  If Hash=0 then
    Hash:=Not(0);
end;


{ ---------------------------------------------------------------------
                          TRESOURCESTRINGS
  ---------------------------------------------------------------------}

Constructor TResourceStrings.Init;
begin
  List.Init;
  ResStrCount:=0;
end;


Destructor TResourceStrings.Done;
begin
  List.Done;
end;


{ ---------------------------------------------------------------------
    Create the full asmlist for resourcestrings.
  ---------------------------------------------------------------------}

procedure TResourceStrings.CreateResourceStringList;

  Procedure AppendToAsmResList (P : PResourceStringItem);
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

Var
  R : PresourceStringItem;
begin
  if not(assigned(resourcestringlist)) then
    resourcestringlist:=new(paasmoutput,init);
  resourcestringlist^.insert(new(pai_const,init_32bit(resstrcount)));
  resourcestringlist^.insert(new(pai_symbol,initdataname_global(current_module^.modulename^+'_'+'RESOURCESTRINGLIST',0)));
  R:=PResourceStringItem(List.First);
  While assigned(R) do
   begin
     AppendToAsmResList(R);
     R:=PResourceStringItem(R^.Next);
   end;
  resourcestringlist^.concat(new(pai_symbol_end,initname(current_module^.modulename^+'_'+'RESOURCESTRINGLIST')));
end;


{ ---------------------------------------------------------------------
    Insert 1 resource string in all tables.
  ---------------------------------------------------------------------}

function  TResourceStrings.Register(const name : string;p : pchar;len : longint) : longint;
begin
  List.Concat(new(PResourceStringItem,Init(lower(current_module^.modulename^+'.'+Name),p,len)));
  Register:=ResStrCount;
  inc(ResStrCount);
end;


Procedure TResourceStrings.WriteResourceFile(Filename : String);
Type
  TMode = (quoted,unquoted);
Var
  F : Text;
  Mode : TMode;
  R : PResourceStringItem;
  C : char;
  Col,i : longint;

  Procedure Add(Const S : String);
  begin
    Write(F,S);
    Col:=Col+length(s);
  end;

begin
  If List.Empty then
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
  R:=PResourceStringItem(List.First);
  While assigned(R) do
   begin
     writeln(f);
     Writeln(f,'# hash value = ',R^.hash);
     col:=0;
     Add(R^.Name+'=');
     Mode:=unquoted;
     For I:=0 to R^.Len-1 do
      begin
        C:=R^.Value[i];
        If (ord(C)>31) and (Ord(c)<=128) and (c<>'''') then
         begin
           If mode=Quoted then
            Add(c)
           else
            begin
              Add(''''+c);
              mode:=quoted
            end;
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
     if mode=quoted then
      writeln (f,'''');
     Writeln(f);
     R:=PResourceStringItem(R^.Next);
   end;
  close(f);
end;


end.
{
  $Log$
  Revision 1.3  2000-07-13 12:08:25  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:39  michael
  + removed logs

}
