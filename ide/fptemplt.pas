{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Template support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPTemplt;

interface

uses FPViews;

const
      tsDate         = '$DATE';
      tsDateCustom   = '$DATE(';
      tsTime         = '$TIME';
      tsPrompt       = '$PROMPT(';

function  GetTemplateCount: integer;
function  GetTemplateName(Index: integer): string;
function  StartTemplate(Index: integer; Editor: PSourceEditor): boolean;

procedure InitTemplates;
procedure DoneTemplates;

implementation

uses
  Dos,Objects,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,
{$endif}
  MsgBox,
  WUtils,
  WEditor,
  FPConst,FPVars,FPString,FPUtils;

type
    PTemplate = ^TTemplate;
    TTemplate = record
      Name : PString;
      Path : PString;
    end;

    PTemplateCollection = ^TTemplateCollection;
    TTemplateCollection = object(TSortedCollection)
      function  At(Index: Integer): PTemplate;
      procedure FreeItem(Item: Pointer); virtual;
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
    end;

const Templates : PTemplateCollection = nil;

function NewTemplate(const Name, Path: string): PTemplate;
var P: PTemplate;
begin
  New(P);
  FillChar(P^,SizeOf(P^),0);
  P^.Name:=NewStr(Name);
  P^.Path:=NewStr(Path);
  NewTemplate:=P;
end;

procedure DisposeTemplate(P: PTemplate);
begin
  if assigned(P) then
   begin
     if assigned(P^.Name) then
       DisposeStr(P^.Name);
     if assigned(P^.Path) then
       DisposeStr(P^.Path);
     Dispose(P);
   end;
end;

function TTemplateCollection.At(Index: Integer): PTemplate;
begin
  At:=inherited At(Index);
end;

procedure TTemplateCollection.FreeItem(Item: Pointer);
begin
  if assigned(Item) then
    DisposeTemplate(Item);
end;

function TTemplateCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var R: Sw_integer;
    K1: PTemplate absolute Key1;
    K2: PTemplate absolute Key2;
begin
  if K1^.Name^<K2^.Name^ then R:=-1 else
  if K1^.Name^>K2^.Name^ then R:= 1 else
  R:=0;
  Compare:=R;
end;

function GetTemplateCount: integer;
var Count: integer;
begin
  if Templates=nil then Count:=0 else Count:=Templates^.Count;
  GetTemplateCount:=Count;
end;

function GetTemplateName(Index: integer): string;
begin
  GetTemplateName:=Templates^.At(Index)^.Name^;
end;

function SearchStr(const InS, SubS: string; var P: sw_integer): boolean;
begin
  P:=Pos(SubS,InS);
  SearchStr:=(P<>0);
end;

procedure ReplaceStr(var S: string; StartP,Len: sw_integer; const NewS: string);
begin
  Delete(S,StartP,Len);
  Insert(NewS,S,StartP);
end;

function ReadStringPos(const InS: string; StartP: sw_integer; var Expr: string; var EndPos: sw_integer): sw_integer;
const Enclosers : string[2] = '''"';
var OK: boolean;
    Encloser: char;
    P: sw_integer;
begin
  OK:=false; Expr:=''; P:=StartP; EndPos:=-1;
  if length(InS)>=P then
  begin
    P:=Pos(InS[P],Enclosers);
    OK:=(P<>0);
    if OK then
    begin
      OK:=false;
      Encloser:=Enclosers[P];
      P:=StartP;
      Inc(P);
      while (P<=length(InS)) do
      begin
        if InS[P]<>Encloser then
          Expr:=Expr+InS[P]
        else
          if (P+1<=length(InS)) and (InS[P+1]=Encloser) then
            Expr:=Expr+InS[P]
          else
            begin
              OK:=true;
              Break;
            end;
        Inc(P);
      end;
      EndPos:=P;
    end;
  end;
  if OK then
    ReadStringPos:=length(Expr)
  else
    ReadStringPos:=-1;
end;

{function ReadString(const InS: string; StartP: sw_integer; var Expr: string): sw_integer;
var P: sw_integer;
begin
  ReadString:=ReadStringPos(InS,StartP,Expr,P);
end;}

function ProcessTemplateLine(var S: string): boolean;
var OK: boolean;
    P,EndP: sw_integer;
    Name,Expr: string;
begin
  OK:=true;
  repeat
    P:=0; Expr:='';
    if OK and SearchStr(S,tsPrompt,P) then
      if ReadStringPos(S,P+length(tsPrompt),Name,EndP)>=0 then
        if copy(S,EndP+1,1)=')' then
         begin
           OK:=InputBox(dialog_fillintemplateparameter,Name,Expr,255)=cmOK;
           if OK then
             ReplaceStr(S,P,EndP-P+1+1,Expr);
         end;
    if OK and SearchStr(S,tsDateCustom,P) then
      if ReadStringPos(S,P+length(tsDateCustom),Expr,EndP)>=0 then
        if copy(S,EndP+1,1)=')' then
           ReplaceStr(S,P,EndP-P+1+1,FormatDateTimeL(Now,Expr));
    if OK and SearchStr(S,tsDate,P) then
      ReplaceStr(S,P,length(tsDate),FormatDateTimeL(Now,'yyyy/mm/dd'));
    if OK and SearchStr(S,tsTime,P) then
      ReplaceStr(S,P,length(tsTime),FormatDateTimeL(Now,'hh:nn:ss'));
  until P=0;
  ProcessTemplateLine:=OK;
end;

function ProcessTemplate(Editor: PSourceEditor): boolean;
var OK: boolean;
    I: sw_integer;
    S,OrigS: string;
begin
  OK:=true;
  with Editor^ do
  for I:=0 to GetLineCount-1 do
  begin
    S:=GetDisplayText(I); OrigS:=S;
    OK:=ProcessTemplateLine(S);
    if OK=false then Break;
    if S<>OrigS then
    begin
      SetDisplayText(I,S);
      UpdateAttrs(I,attrAll);
     end;
  end;
  ProcessTemplate:=OK;
end;

function StartTemplate(Index: integer; Editor: PSourceEditor): boolean;
var
    T: PTemplate;
    OK: boolean;
begin
  T:=Templates^.At(Index);
  OK:=StartEditor(Editor,T^.Path^);
  if OK then
  begin
    ProcessTemplate(Editor);
  end;
  StartTemplate:=OK;
end;


{*****************************************************************************
                                 InitTemplates
*****************************************************************************}

procedure InitTemplates;

  procedure ScanDir(Dir: PathStr);
  var SR: SearchRec;
      S: string;
      PT : PTemplate;
      i : sw_integer;
  begin
    if copy(Dir,length(Dir),1)<>DirSep then Dir:=Dir+DirSep;
    FindFirst(Dir+'*'+TemplateExt,AnyFile,SR);
    while (DosError=0) do
    begin
      S:=NameOf(SR.Name);
      S:=LowerCaseStr(S);
      S[1]:=Upcase(S[1]);
      PT:=NewTemplate(S,FExpand(Dir+SR.Name));
      if not Templates^.Search(PT,i) then
        Templates^.Insert(PT)
      else
        DisposeTemplate(PT);
      FindNext(SR);
    end;
  {$ifdef FPC}
    FindClose(SR);
  {$endif def FPC}
  end;

begin
  New(Templates, Init(10,10));
  ScanDir('.');
  ScanDir(IDEDir);
end;


procedure DoneTemplates;
begin
  if assigned(Templates) then
    begin
      Dispose(Templates, Done);
      Templates:=nil;
    end;
end;

END.
{
  $Log$
  Revision 1.2  2001-08-05 02:01:48  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.10  2000/06/22 09:07:12  pierre
   * Gabor changes: see fixes.txt

  Revision 1.9  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.8  1999/06/25 00:33:40  pierre
   * avoid lost memory on duplicate Template Items

  Revision 1.7  1999/03/08 14:58:11  peter
    + prompt with dialogs for tools

  Revision 1.6  1999/03/01 15:42:03  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.5  1999/02/18 13:44:35  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.4  1999/02/16 17:13:56  pierre
   + findclose added for FPC

  Revision 1.3  1999/01/21 11:54:24  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.2  1998/12/28 15:47:52  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.2  1998/12/22 10:39:51  peter
    + options are now written/read
    + find and replace routines

}
