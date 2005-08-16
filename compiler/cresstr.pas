{
    Copyright (c) 1998-2002 by Michael van Canneyt

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

{$i fpcdefs.inc}

interface

uses
  cclasses;

Type
  { These are used to form a singly-linked list, ordered by hash value }
  TResourceStringItem = class(TLinkedListItem)
    Name  : String;
    Value : Pchar;
    Len   : Longint;
    hash  : Cardinal;
    constructor Create(const AName:string;AValue:pchar;ALen:longint);
    destructor  Destroy;override;
    procedure CalcHash;
  end;

  TResourceStrings=class
  private
    List : TLinkedList;
  public
    ResStrCount : longint;
    constructor Create;
    destructor  Destroy;override;
    function  Register(Const name : string;p : pchar;len : longint) : longint;
    procedure CreateResourceStringList;
    Procedure WriteResourceFile(const FileName : String);
  end;

var
  ResourceStrings : TResourceStrings;


implementation

uses
   cutils,globtype,globals,
   symdef,
   verbose,fmodule,
   aasmbase,aasmtai,
   aasmcpu;


{ ---------------------------------------------------------------------
   Calculate hash value, based on the string
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
                          TRESOURCESTRING_ITEM
  ---------------------------------------------------------------------}

constructor TResourceStringItem.Create(const AName:string;AValue:pchar;ALen:longint);
begin
  inherited Create;
  Name:=AName;
  Len:=ALen;
  GetMem(Value,Len);
  Move(AValue^,Value^,Len);
  CalcHash;
end;


destructor TResourceStringItem.Destroy;
begin
  FreeMem(Value,Len);
end;

procedure TResourceStringItem.CalcHash;
Var
  g : Cardinal;
  I : longint;
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
    Hash:=$ffffffff;
end;


{ ---------------------------------------------------------------------
                          TRESOURCESTRINGS
  ---------------------------------------------------------------------}

Constructor TResourceStrings.Create;
begin
  List:=TStringList.Create;
  ResStrCount:=0;
end;


Destructor TResourceStrings.Destroy;
begin
  List.Free;
end;


{ ---------------------------------------------------------------------
    Create the full asmlist for resourcestrings.
  ---------------------------------------------------------------------}

procedure TResourceStrings.CreateResourceStringList;

  Procedure AppendToAsmResList (P : TResourceStringItem);
  Var
    l1 : tasmlabel;
    s : pchar;
    l : longint;
  begin
    With P Do
     begin
       if (Value=nil) or (len=0) then
         resourcestringlist.concat(tai_const.create_sym(nil))
       else
         begin
            objectlibrary.getdatalabel(l1);
            resourcestringlist.concat(tai_const.create_sym(l1));
            consts.concat(tai_align.Create(const_align(sizeof(aint))));
            consts.concat(tai_const.create_aint(-1));
            consts.concat(tai_const.create_aint(len));
            consts.concat(tai_label.create(l1));
            getmem(s,len+1);
            move(Value^,s^,len);
            s[len]:=#0;
            consts.concat(tai_string.create_length_pchar(s,len));
            consts.concat(tai_const.create_8bit(0));
         end;
       { append Current value (nil) and hash...}
       resourcestringlist.concat(tai_const.create_sym(nil));
       resourcestringlist.concat(tai_const.create_32bit(longint(hash)));
       { Append the name as a ansistring. }
       objectlibrary.getdatalabel(l1);
       L:=Length(Name);
       resourcestringlist.concat(tai_const.create_sym(l1));
       consts.concat(tai_align.Create(const_align(sizeof(aint))));
       consts.concat(tai_const.create_aint(-1));
       consts.concat(tai_const.create_aint(l));
       consts.concat(tai_label.create(l1));
       getmem(s,l+1);
       move(Name[1],s^,l);
       s[l]:=#0;
       consts.concat(tai_string.create_length_pchar(s,l));
       consts.concat(tai_const.create_8bit(0));
     end;
  end;

Var
  R : tresourceStringItem;
begin
  if not(assigned(resourcestringlist)) then
    resourcestringlist:=taasmoutput.create;
  resourcestringlist.insert(tai_const.create_32bit(resstrcount));
  resourcestringlist.insert(tai_symbol.createname_global(make_mangledname('RESOURCESTRINGLIST',current_module.localsymtable,''),AT_DATA,0));
  resourcestringlist.insert(tai_align.Create(const_align(sizeof(aint))));
  R:=TResourceStringItem(List.First);
  While assigned(R) do
   begin
     AppendToAsmResList(R);
     R:=TResourceStringItem(R.Next);
   end;
  resourcestringlist.concat(tai_symbol_end.createname(current_module.modulename^+'_'+'RESOURCESTRINGLIST'));
end;


{ ---------------------------------------------------------------------
    Insert 1 resource string in all tables.
  ---------------------------------------------------------------------}

function  TResourceStrings.Register(const name : string;p : pchar;len : longint) : longint;
begin
  List.Concat(tResourceStringItem.Create(lower(current_module.modulename^+'.'+Name),p,len));
  Register:=ResStrCount;
  inc(ResStrCount);
end;


Procedure TResourceStrings.WriteResourceFile(const FileName : String);
Type
  TMode = (quoted,unquoted);
Var
  F : Text;
  Mode : TMode;
  R : TResourceStringItem;
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
  message1 (general_i_writingresourcefile,SplitFileName(filename));
  Assign(F,Filename);
  {$i-}
  Rewrite(f);
  {$i+}
  If IOresult<>0 then
    begin
      message1(general_e_errorwritingresourcefile,filename);
      exit;
    end;
  R:=TResourceStringItem(List.First);
  While assigned(R) do
   begin
     writeln(f);
     Writeln(f,'# hash value = ',R.hash);
     col:=0;
     Add(R.Name+'=');
     Mode:=unquoted;
     For I:=0 to R.Len-1 do
      begin
        C:=R.Value[i];
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
     R:=TResourceStringItem(R.Next);
   end;
  close(f);
end;


end.
