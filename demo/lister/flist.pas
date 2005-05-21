{

    This file is a Free Pascal example
    Copyright (C) 2005 by Marco van de Voort
        member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    A set of simple dirscanning routines for the lister.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit FList;

{$Mode Delphi}

Interface

Uses Classes{$ifdef Win32},Registry {$endif};

Const NoMarkNoSelect=0;
      MarkNoSelect  =1;
      NoMarkSelect  =2;
      MarkSelect    =3;

Type

     TDirList  = Class
                  private
                   ACursor,                 // Entry (0-based) the cursor is on.
                   APosition,               // Entry (0-based) the homespot occupies
                   ATotalEnt,               // Total number of entries (dirs.count+files.count)
                   wdth,                    // width of a column (width of screen div col)+1 in chars
                   Amaxent,                 // number of entries that fit on the screen
                   Acolh,                   // height of a column in entries.
                   botRightCol : Integer;   // When past this char, we are in bottombar
                   TopBar,
                   BottomBar   : Integer;   // lines below/above not for display

                   AColumns    : Integer;  // amount of columns
                   TheDirs     : TStringList;   // \
                   TheFiles    : TStringList;   // The current entries
                   Marked      : TBits;     // Which entries are selected?
                   DirMax      : Integer;   // Can be used for autoscaling

                   Filemax     : Integer;   // likewise
                   ScrWidth,
                   ScrHeight   : Integer;
                   function  GetEntry(I:Integer;index:Integer):String;
                   function  GetDirCount:Integer;
                  public
                   Directory   : String;    // Current dir we are looking at.
                   Constructor Create;
                   Destructor Destroy; override;
                   Procedure PopulateList(Const FileSpec:String);
                   Procedure CalcScreenStats;
                   Procedure Mark(x:integer);
                   Procedure UnMark(x:integer);
                   Procedure Toggle(x:integer);
                   {$ifdef Debug}
                   Procedure PrintDirs;
                   Procedure PrintFiles;
                   {$endif}
                   property Entries[i:Integer]:String index 0 read GetEntry; Default;
                   property Directories[i:Integer]:String index 1 Read GetEntry;
                   property Files[i:Integer]:String index 2 Read GetEntry;
                   property DirCount:Integer read GetDirCount;
                   property Position: Integer read APosition write APosition;
                   property Columns: Integer read AColumns write AColumns;
                   property Cursor: Integer read ACursor write ACursor;
                   property Colh: Integer read AColh write AColh;
                   property MaxEnt: Integer read AMaxEnt write AMaxEnt;
                   property TotalEnt: Integer read ATotalEnt write ATotalEnt;
                   End;


    TVidDirList = Class(TDirList)
                   {$ifdef Win32}
                    Reg:TRegistry;
                   {$endif}
                   Attributes : Array[0..3] Of Integer;
                   Constructor Create();
                   Destructor Destroy; override;
                   Procedure BuildDisplay;
                   procedure ClearArea;
                   Procedure Hilight(Curs,Home,Onx:Integer);
                  {$ifdef Win32}
                    Function CheckAssociation(ext:String):String;
                  {$endif}
                   End;


Procedure TextOut(X,Y : Integer;Const S : String);
Procedure textclear(x,y,Count :Integer);

{$ifdef debug}
var  f : Text;
{$endif}


Implementation

Uses SysUtils,Video;

// comes from vidutl in the video examples area.
Procedure TextOut(X,Y : Integer;Const S : String);

Var
  P,I,M : Integer;

begin
  P:=((X-1)+(Y-1)*ScreenWidth);
  M:=Length(S);
  If P+M>ScreenWidth*ScreenHeight then
    M:=ScreenWidth*ScreenHeight-P;
  For I:=1 to M do
    VideoBuf^[P+I-1]:=Ord(S[i])+($07 shl 8);
end;

Procedure textclear(x,y,Count :Integer);

begin
  FillWord(VideoBuf[((X-1)+(Y-1)*ScreenWidth)],count,$07 shl 8);
end;


Constructor TDirList.Create;

Begin
  TheDirs:=TStringList.Create;
  TheFiles:=TStringList.Create;
  Marked:=TBits.Create(1000);
  TheDirs.Sorted:=True;
  TheFiles.Sorted:=True;
  TopBar:=1;
  BottomBar:=1;
  Columns:=4;
  Inherited Create;
End;

function  TDirList.GetDirCount:Integer;

Begin
 Result:=TheDirs.Count;
End;

Destructor TDirList.Destroy;

Begin
 TheDirs.Free;
 TheFiles.Free;
 Marked.Free;
 inherited destroy;
End;

Procedure TDirList.Mark(x:integer);

Begin
 Marked.Seton(x);
End;

Procedure TDirList.UnMark(x:integer);

Begin
 Marked.Clear(x);
End;

Procedure TDirList.Toggle(x:integer);

{$ifdef Debug}
var s:String;
    I:longint;
{$endif}

Begin
 Marked[x]:=NOT Marked[x];
 {$ifdef Debug}
 Writeln(F,'after marked:',marked.size);
 SetLength(S,51);
 For I:=0 To 50 Do
  If Marked[i] Then
   S[i+1]:=#49
  else
   S[I+1]:=#48;
 TextOut(1,1,S);
  Writeln(F,'after textout:',marked.size);
 {$endif}
End;

Function TDirList.GetEntry(I:Integer;Index:Integer):String;

Begin
 {$ifdef Debug}
  Writeln(F,'i:',i,' ',index);
  {$endif}
  Case Index Of
  0 : If I<TheDirs.Count Then
        Result:=TheDirs[I]
      Else
        Result:=TheFiles[I-TheDirs.Count];
  1 : Result:=TheDirs[I];
  2 : Result:=TheFiles[I];
  End;
End;

Procedure TDirList.PopulateList;

Var  Info : TSearchRec;
     Len  : Integer;

Procedure DoSearch(Const fs:String; Attr : Integer;AddFiles:Boolean);

Begin
    If FindFirst (Directory+FS,Attr,Info)=0 then
      Repeat
        Len:=Length(Info.Name);
        If (Info.Attr and faDirectory) = faDirectory then
          Begin
            TheDirs.Add(Info.Name);
            If Len>DirMax Then
              DirMax:=Len;
          End
        Else
          Begin
            If AddFiles Then
              Begin
                TheFiles.Add(Info.Name);
                If Len>FileMax Then
                  FileMax:=Len;
              End;
          End;
      Until FindNext(info)<>0;
     FindClose(Info);
End;

Begin

  DirMax:=0;
  FileMax:=0;
  TheDirs.Clear;
  TheFiles.Clear;
  Directory:=IncludeTrailingPathDelimiter(Directory);
  If FileSpec='*.*' Then
    Begin
      DoSearch(FileSpec,faAnyFile and faDirectory,True);
    End
  Else
    Begin
      DoSearch('*.*',faDirectory,False);
      DoSearch(FileSpec,faAnyFile,True);
    End;
  If (TheDirs.Count>0) And (TheDirs[0]='.') Then
    TheDirs.Delete(0);
  TotalEnt:=TheDirs.count+TheFiles.count;
  Position:=0;
  Cursor:=0;
  If Marked.Size<TotalEnt THEN
    Marked.Grow(TotalEnt);
  Marked.ClearAll;
End;

{$ifdef debug}
Procedure TDirList.PrintDirs;

Var I:Integer;

Begin
  Writeln(f,Thedirs.count, ' ', thefiles.count, ' ',thedirs.count+thefiles.count);
  If theDirs.Count>0 Then
     For I:=0 To theDirs.Count-1 DO
        Writeln(f,theDirs[I]);
End;

Procedure TDirList.PrintFiles;

Var I:Integer;

Begin
  If TheFiles.Count>0 Then
     For I:=0 To TheFiles.Count-1 DO
        Writeln(f,TheFiles[I]);
  Writeln(f,'----');
End;
{$endif}

Procedure TDirList.CalcScreenStats;

Begin
 // Calc width of columns, minus one for the space inbetween

 wdth:=(ScrWidth DIV Columns)-1;

 // effective height of a column

 colh:=(ScrHeight-TopBar-BottomBar);

 // Max amount Filenames we can store in one screen;

 maxent:=colh*Columns;

 // If we write beyond this character, we would be wrong.

 BotRightCol:=(ScrHeight-BottomBar)*ScrWidth;
End;


Constructor TVidDirList.Create;

Begin
 inherited Create;
 ScrWidth:=ScreenWidth;
 ScrHeight:=ScreenHeight;
 CalcScreenStats;
 {$Ifdef Win32}
  Reg:=TRegistry.Create;
  Reg.RootKey:=HKEY_CLASSES_ROOT;
 {$endif}
End;

Destructor TVidDirList.Destroy;

Begin
 {$ifdef Win32}
  Reg.Free;
 {$endif}
End;


Procedure TVidDirList.BuildDisplay;

Var
  O,I,M,X,
  TopLeftCol,
  totalc,
  lpos,
  dirc       : Integer;
  S       : String;

begin
 {$ifdef debug}
//  Writeln(f,'entering');
 {$endif}
  dirc:=Thedirs.count;

  totalc:=TotalEnt;
  TopLeftCol:=TopBar*ScreenWidth;

  X:=TopLeftCol;
  lpos:=position+maxent;
  // First the dirs;
  i:=Position;
  If I<Totalc THen
   Begin
     REPEAT
       If I<dirc Then
        S:=TheDirs[I]
       Else
        S:=TheFiles[I-dirc];
       m:=Length(s);
       if m>wdth Then
        m:=wdth;

       For o:=0 to m-1 do
        VideoBuf^[X+O]:=Ord(S[o+1])+(Attributes[ORD(Marked[I])] shl 8);

       inc(X,screenwidth);
       If X>=botrightcol Then
        Begin
         TopLeftCol:=TopLeftCol+wdth+1;
         x:=TopLeftCol;
        End;
     Inc(I);
     Until (i>=lpos) or (I>=totalc);
     {$ifdef debug}
//     Writeln(F,'lpos  :',lpos);
//     writeln(F,'i     :',i);
//     writeln(F,'totalc:',totalc);
     {$endif}
   End;
end;

Procedure TVidDirList.Hilight(Curs,Home:Integer;Onx:Integer);

Var I    : Integer;
    Posx : Integer;
    L    : PWord;
    Attr : Integer;

Begin
 Attr:=Attributes[ORD(Marked[Curs])+Onx shl 1] shl 8;
 Posx:=Curs-Home;
 L:=@VideoBuf[(Posx DIV Colh)*(wdth+1) + (TopBar+ Posx MOD Colh)*screenWidth];
 For I:= 0 TO wdth-1 DO
  Begin
   L^:=(L^ And 255) + Attr;
   Inc(L);
  End;
End;

procedure TVidDirList.ClearArea;
Begin
    TextClear(1,2,screenwidth*(screenheight-topbar-bottombar));
End;

{$ifdef Win32}
 Function TVidDirList.CheckAssociation(ext:String):String;

 Var S : String;
 Begin
  if Reg.OpenKey(ext, false) then
   begin
     Reg.CloseKey;
//     Reg.Free;
     Exit('');
   end;

  Reg.OpenKey('\'+ext, True);
  S:=Reg.readString('');
  Reg.closekey;
  Reg.OpenKey('\'+S+'\Shell\Open\Command', True);
  Result:=Reg.ReadString('');
  reg.closekey;
 End;
{$endif}
end.
