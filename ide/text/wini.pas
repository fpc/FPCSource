{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by B‚rczi G bor

    Reading and writing .INI files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WINI;

interface

uses Objects;

type
    PINIEntry = ^TINIEntry;
    TINIEntry = object(TObject)
      constructor Init(ALine: string);
      function    GetText: string;
      function    GetTag: string;
      function    GetComment: string;
      function    GetValue: string;
      procedure   SetValue(S: string);
      destructor  Done; virtual;
    private
      Tag      : PString;
      Value    : PString;
      Comment  : PString;
      Text     : PString;
      Modified : boolean;
      procedure Split;
    end;

    PINISection = ^TINISection;
    TINISection = object(TObject)
      constructor Init(AName: string);
      function    GetName: string;
      procedure   AddEntry(S: string);
      destructor  Done; virtual;
    private
      Name     : PString;
      Entries  : PCollection;
    end;

    PINIFile = ^TINIFile;
    TINIFile = object(TObject)
      constructor Init(AFileName: string);
      function    Read: boolean; virtual;
      function    Update: boolean; virtual;
      function    IsModified: boolean; virtual;
      destructor  Done; virtual;
    private
      ReadOnly: boolean;
      Sections: PCollection;
      FileName: PString;
    end;

const MainSectionName : string[40] = 'MainSection';
      CommentChar     : char = ';';

implementation


function LTrim(S: string): string;
begin
  while copy(S,1,1)=' ' do Delete(S,1,1);
  LTrim:=S;
end;


function RTrim(S: string): string;
begin
  while copy(S,length(S),1)=' ' do Delete(S,length(S),1);
  RTrim:=S;
end;


function Trim(S: string): string;
begin
  Trim:=RTrim(LTrim(S));
end;


function GetStr(P: PString): string;
begin
  if P=nil then GetStr:='' else GetStr:=P^;
end;


function EatIO: integer;
begin
  EatIO:=IOResult;
end;


constructor TINIEntry.Init(ALine: string);
begin
  inherited Init;
  Text:=NewStr(ALine);
  Split;
end;


function TINIEntry.GetText: string;
var S,CoS: string;
begin
  if Text=nil then
    begin
      CoS:=GetComment;
      S:=GetTag+'='+GetValue;
      if Trim(S)='=' then S:=CoS else
        if CoS<>'' then S:=S+' '+CommentChar+' '+CoS;
    end
    else S:=Text^;
  GetText:=S;
end;


function TINIEntry.GetTag: string;
begin
  GetTag:=GetStr(Tag);
end;


function TINIEntry.GetComment: string;
begin
  GetComment:=GetStr(Comment);
end;


function TINIEntry.GetValue: string;
begin
  GetValue:=GetStr(Value);
end;


procedure TINIEntry.SetValue(S: string);
begin
  if GetValue<>S then
  begin
    if Text<>nil then DisposeStr(Text); Text:=nil;
    Value:=NewStr(S);
    Modified:=true;
  end;
end;


procedure TINIEntry.Split;
var S,ValueS: string;
    P,P2: byte;
    C: char;
    InString: boolean;
begin
  S:=GetText;
  P:=Pos('=',S); P2:=Pos(CommentChar,S);
  if (P2<>0) and (P2<P) then P:=0;
  if P<>0 then
    begin
      Tag:=NewStr(copy(S,1,P-1));
      P2:=P+1; InString:=false; ValueS:='';
      while (P2<=length(S)) do
        begin
          C:=S[P2];
          if C='"' then InString:=not InString else
          if (C=CommentChar) and (InString=false) then Break else
          ValueS:=ValueS+C;
          Inc(P2);
        end;
      Value:=NewStr(ValueS);
      Comment:=NewStr(copy(S,P2+1,255));
    end else
    begin
      Tag:=nil;
      Value:=nil;
      Comment:=NewStr(S);
    end;
end;


destructor TINIEntry.Done;
begin
  inherited Done;
  if Text<>nil then DisposeStr(Text);
  if Tag<>nil then DisposeStr(Tag);
  if Value<>nil then DisposeStr(Value);
  if Comment<>nil then DisposeStr(Comment);
end;


constructor TINISection.Init(AName: string);
begin
  inherited Init;
  Name:=NewStr(AName);
  New(Entries, Init(50,500));
end;


function TINISection.GetName: string;
begin
  GetName:=GetStr(Name);
end;

procedure TINISection.AddEntry(S: string);
var E: PINIEntry;
begin
  New(E, Init(S));
  Entries^.Insert(E);
end;


destructor TINISection.Done;
begin
  inherited Done;
  if Name<>nil then DisposeStr(Name);
  Dispose(Entries, Done);
end;


constructor TINIFile.Init(AFileName: string);
begin
  inherited Init;
  FileName:=NewStr(AFileName);
  New(Sections, Init(50,50));
  Read;
end;

function TINIFile.Read: boolean;
var f: text;
    OK: boolean;
    S,TS: string;
    P: PINISection;
    I: integer;
begin
  New(P, Init(MainSectionName));
  Sections^.Insert(P);
  Assign(f,FileName^);
{$I-}
  Reset(f);
  OK:=EatIO=0;
  while OK and (Eof(f)=false) do
    begin
      readln(f,S);
      TS:=Trim(S);
      OK:=EatIO=0;
      if OK then
      if TS<>'' then
      if copy(TS,1,1)='[' then
      begin
        I:=Pos(']',TS); if I=0 then I:=length(TS)+1;
        New(P, Init(copy(TS,2,I-2)));
        Sections^.Insert(P);
      end else
      begin
        P^.AddEntry(S);
      end;
    end;
  Close(f);
  EatIO;
{$I+}
  Read:=true;
end;

function TINIFile.IsModified: boolean;

  function SectionModified(P: PINISection): boolean; {$ifndef FPC}far;{$endif}

    function EntryModified(E: PINIEntry): boolean; {$ifndef FPC}far;{$endif}
    begin
      EntryModified:=E^.Modified;
    end;

  begin
    SectionModified:=(P^.Entries^.FirstThat(@EntryModified)<>nil);
  end;

begin
  IsModified:=(Sections^.FirstThat(@SectionModified)<>nil);
end;


function TINIFile.Update: boolean;
var f: text;
    OK: boolean;
    P: PINISection;
    E: PINIEntry;
    I,J: integer;
begin
  Assign(f,FileName^);
{$I-}
  Rewrite(f);
  OK:=EatIO=0;
  if OK then
  for I:=0 to Sections^.Count-1 do
    begin
      P:=Sections^.At(I);
      if I<>0 then writeln(f,'['+P^.GetName+']');
      for J:=0 to P^.Entries^.Count-1 do
        begin
          E:=P^.Entries^.At(J);
          writeln(f,E^.GetText);
          OK:=EatIO=0;
          if OK=false then Break;
        end;
      if OK and ((I>0) or (P^.Entries^.Count>0)) and (I<Sections^.Count-1) then
        writeln(f,'');
      OK:=OK and (EatIO=0);
      if OK=false then Break;
    end;
  Close(f);
  EatIO;
{$I+}
  Update:=true;
end;

destructor TINIFile.Done;
begin
  if IsModified then
    Update;
  inherited Done;
  if FileName<>nil then
    DisposeStr(FileName);
  Dispose(Sections, Done);
end;


END.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:55  peter
    * moved

  Revision 1.1  1998/12/22 10:39:57  peter
    + options are now written/read
    + find and replace routines

}
