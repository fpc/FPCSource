{

    This file is a Free Pascal example
    Copyright (C) 2005 by Marco van de Voort
        member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    An filebrowser inspired by Vernon D. Buerg's list.com, designed
    to be a shell to less under Unix, but works fine under Windows too.
    (using any less and file in the path)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode Delphi}

Uses Process,SysUtils,Video,Keyboard,FList
     {$IFDEF UNIX}, BaseUnix{$ENDIF};

Function Do_File_cmd(path:String):String;

Const BufSize = 1024;
      TheProgram = 'file' {$IFDEF Win32}+'.exe' {$ENDIF};


Var S : TProcess;
    Buf : Array[1..BUFSIZE] of char;
    I,Count : longint;

begin
  S:=TProcess.Create(Nil);
  S.Commandline:=theprogram+' '+path;
  S.Options:=[poUsePipes,poNoConsole];
  S.execute;
  Result:='';
  Count:=s.output.read(buf,BufSize);
  If Count>0 Then
    Begin
      SetLength(Result,Count);
      Move(buf[1],Result[1],Count);
    End;
  S.Free;
  {$ifdef win32}
  If Length(Result)>2 Then
   Begin
    If Result[2]=':' Then
     Result[2]:=' ';
   End;
  {$endif}
  i:=Pos(':',Result);
  If I>0 Then
    Delete(Result,1,I);
  Result:=Trim(Result);
  I:=Length(Result);
  While (I>0) and (Result[I]=#10) DO
   Dec(I);
  If I>ScreenWidth Then
    I:=ScreenWidth;
  SetLength(Result,I);
end;


Var
  FileSpec : String;
  D        : TVidDirList;
  ExitNow  : Boolean;
  K        : TKeyEvent;
  OldHome,
  OldCursor: Integer;
  S,S2     : String;
  Forced,
  ForcedFull: Boolean;
  C        : Char;
  Editor,
  Pager : AnsiString;
Procedure ReDraw;

Begin
    D.ClearArea;
    D.BuildDisplay;
    D.HiLight(D.Cursor,D.Position,1);
    // Probably so much changed that diffing won't help?
    UpdateScreen(true);
End;

procedure loadutil(const envvar,default : string;var symbol : string);

begin
   Symbol:=GetEnvironmentVariable(envvar); 
   if Symbol='' Then
     Symbol:=default;
   if Pos('/',Symbol)=0 Then
     Symbol:=FileSearch(Symbol,GetEnvironmentVariable('PATH'));
end;


Begin
  InitVideo;
  InitKeyboard;
  {$ifdef Unix}
   FileSpec:='*';
  {$else}
   FileSpec:='*.*';
  {$endif}
  ExitNow:=False;
  {$ifdef win32}
   Pager:='notepad.exe';
   Editor:='notepad.exe';
  {$else}
   loadutil('EDITOR','joe' ,editor);
   loadutil('PAGER' ,'less',pager);
  {$endif}
  If ParamCount()>0 Then
    FileSpec:=ParamStr(1);
  {$ifdef debug}
  assign(f,'log.txt');
  rewrite(F);
  {$endif}
  D:=TVidDirList.Create;
  D.Columns:=5;                              // default
  D.Directory:=GetCurrentDir;
  D.PopulateList(FileSpec);
  D.Attributes[NoMarkNoSelect]:=$07;
  D.Attributes[MarkNoSelect]  :=$17;
  D.Attributes[NoMarkSelect]  :=$0F;
  D.Attributes[MarkSelect]    :=$1F;

  ReDraw;
  Repeat
   {$ifdef debug}
    Writeln(F,'Cursor  :',D.Cursor);
    Writeln(F,'Position:',D.Position);
    Writeln(F,'Totalent:',D.TotalEnt);
//    Writeln(F,'wdth    :',D.wdth);
    Writeln(F,'maxent  :',D.maxent);
    Writeln(F,'colh    :',D.colh);
    Writeln(F,'columns :',D.columns);
    Writeln(F);
   {$endif}

    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    OldCursor:=D.Cursor;
    OldHome:=D.Position;
    Forced:=False;
    ForcedFull:=False;
    IF IsFunctionKey(K) Then
      Begin
        K:=TKeyRecord(K).KeyCode;
        Case K Of
          kbdRight: Begin
                     If D.Cursor<(D.TotalEnt-D.Colh) Then
                       D.Cursor:=D.Cursor+D.colh
                      else
                       D.Cursor:=D.TotalEnt-1;
                     If D.Cursor>=(D.Position+D.MaxEnt) Then
                      D.Position:=D.Position+d.colh;
                    End;
          kbdDown : Begin
                     If D.Cursor<(D.TotalEnt-1) Then
                      D.Cursor:=D.Cursor+1;
                     If D.Cursor>(D.Position+D.MaxEnt-1) Then
                      D.Position:=D.Position+D.Colh;
                    End;
          kbdUp   : Begin
                     If D.Cursor>0 Then
                      D.Cursor:=D.Cursor-1;
                     If D.Cursor<D.Position Then
                       Begin
                         D.Position:=D.Position-D.Colh;
                         If D.Position<0 Then
                          D.Position:=0;
                       End;
                    End;
          kbdLeft : Begin
                     If D.Cursor>=(D.Colh) Then
                        D.Cursor:=D.Cursor-D.colh
                      else
                        D.Cursor:=0;
                     If D.Cursor<D.Position Then
                       D.Position:=D.Position-D.Colh;
                     If D.Position<0 Then
                       D.Position:=0;
                    End;
          End;
      End
    Else
      Begin
        C:=GetKeyEventChar(K);
        if C<>#0 Then
        Case C Of
          #13      : Begin
                       If D.Cursor>=D.DirCount Then
                         Begin
                           {$ifdef win32}  // try to get "open" action ?
                             S:=ExtractFileExt(D[D.Cursor]);
                             Delete(S,1,1);
			   {$endif}
                           ExecuteProcess(Pager,[D.Directory+D[D.Cursor]]);
                           // TextOut(10,1,'                        ');
                           //TextOut(10,1,D[D.Cursor]);
                           ForcedFull:=True;
                         End
                       Else
                         Begin
                           S:=D.Directories[D.Cursor];
                           S2:=D.Directory+S;
                           If S='..' Then
                             S2:=ExpandFileName(S2);
			   {$IFDEF UNIX}
                           IF FPAccess(pchar(s2),X_OK)=0 Then
                             begin
		           {$ENDIF}
                               D.Directory:=S2;
                               D.PopulateList(FileSpec);
                               ForcedFull:=True;
 			   {$IFDEF UNIX}
                             end;
			   {$ENDIF}
                         End;
                     End;
          'e','E'  : begin
		       If D.Cursor>=D.DirCount Then
                         Begin
                           {$ifdef win32}  // try to get "edit" action ?
                             S:=ExtractFileExt(D[D.Cursor]);
                             Delete(S,1,1);
			   {$endif}
                           ExecuteProcess(Editor,[D.Directory+D[D.Cursor]]);
                           // TextOut(10,1,'                        ');
                           //TextOut(10,1,D[D.Cursor]);
                           ForcedFull:=True;
                         End	
		     end;
          'd','D'  : Begin
		       If D.Cursor>=D.DirCount Then
                         Begin
                           s:=ExpandFileName(D.Directory+D[D.Cursor]);
		           DeleteFile(S);
			   D.PopulateList(FileSpec);
			   ForcedFull:=True;
			 End;                	 
		     End;
          #27,'q'  : exitnow:=True;
          ' '      : Begin
                       D.Toggle(D.Cursor);
                       Forced:=True;
                     End;
          'i'      : Begin
                       TextClear(1,1,ScreenWidth);
                       If D.Cursor>=D.DirCount Then
                         Begin
                           TextOut(1,1,do_file_cmd(' '+D.Directory+D[D.Cursor]));
                         End
                       Else
                         Begin
                           textout(1,1,+' is a directory');
                         End;
                      Forced:=True;
                     End;

          End;
      End;

   // Determine if, and what kind of updating necessary;
   If (OldHome<>D.Position) OR ForcedFull Then
     Redraw
   Else
     Begin
      If (OldCursor<>D.Cursor) Or Forced Then
        Begin
          D.Hilight(OldCursor,OldHome,0);
          D.HiLight(D.Cursor,D.Position,1);
          UpdateScreen(False);
        End;
     End;
  Until ExitNow;

  DoneKeyboard;
  DoneVideo;
  {$ifdef Debug}
  d.printdirs;
  d.printfiles;
  {$endif}
  D.Free;

  {$ifdef debug}
  Close(F);
  {$endif}
End.
