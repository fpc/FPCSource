{
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

program h2pas;
{$H+}

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  classes, h2poptions, scan, h2pconst, scanbase,
  h2pbase, h2pparse, h2pout, h2ptypes, h2pCpp;

var
  SS : string;
  headerfile: Text;
  finaloutfile: Text;
  Lines : TStringList;
  I : Integer;

begin
  pointerprefix:=false;
{ Initialize }
  InitGlobals;
  EnableDebug;
  aktspace:='';
  block_type:=bt_no;
{ Read commandline options }
  ProcessOptions;
  if not CompactMode then
   aktspace:='  ';
{ open input and output files }
  OpenInputFile;
  OpenOutputFiles;
{ Parse! }
  yyparse;
{ Write implementation if needed }
  WriteSectionMarker(outfile,'I');
   if not(includefile) then
    begin
      writeln(outfile);
      writeln(outfile,'implementation');
      writeln(outfile);
    end;
  if createdynlib then
    WriteLibraryUses;
   { here we have a problem if a line is longer than 255 chars !! }
   reset(implemfile);
   while not eof(implemfile) do
    begin
      readln(implemfile,SS);
      writeln(outfile,SS);
    end;
  if createdynlib then
    WriteLibraryInitialization;

   { write end of file }
   writeln(outfile);
   if not(includefile) then
     writeln(outfile,'end.');
   { close and erase tempfiles }
  CloseTempFiles;
  RemovePreprocessedFiles;
  flush(outfile);

  {**** generate full file ****}
  assign(headerfile, 'ext4.tmp');
  {$I-}
  rewrite(headerfile);
  {$I+}
  if ioresult<>0 then
    begin
      writeln('file ext4.tmp could not be created!');
      halt(1);
  end;
  WriteFileHeader(HeaderFile);

  { Final output filename }
  assign(finaloutfile, outputfilename);
  {$I-}
  rewrite(finaloutfile);
  {$I+}
  if ioresult<>0 then
  begin
     writeln('file ',outputfilename,' could not be created!');
     halt(1);
  end;
  writeln(finaloutfile);

  { Read unit header file }
  reset(headerfile);
  while not eof(headerfile) do
    begin
      readln(headerfile,SS);
      writeln(finaloutfile,SS);
    end;
  { Read interface and implementation file }
  reset(outfile);
  Lines:=TStringList.Create;
  while not eof(outfile) do
    begin
      readln(outfile,SS);
      Lines.Add(SS);
    end;
  CollectMovedRecords(Lines);
  if OneTypeSection then
    ArrangeSections(Lines);
  for I:=0 to Lines.Count-1 do
    if not WriteMarkedPointers(finaloutfile,Lines[I]) then
      writeln(finaloutfile,Lines[I]);
  Lines.Free;

  close(HeaderFile);
  close(outfile);
  close(finaloutfile);
  erase(outfile);
  erase(headerfile);

  PTypeList.Free;
  freedynlibproc.free;
  loaddynlibproc.free;
end.