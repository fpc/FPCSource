
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

program h2pas;
{$H+}

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  classes, h2poptions, scan, h2pconst, scanbase,
  h2pbase, h2pparse, h2pout, h2ptypes;


var
  SS : string;
  headerfile: Text;
  finaloutfile: Text;

begin
  pointerprefix:=false;
{ Initialize }
  InitGlobals;
  EnableDebug;
  aktspace:='';
  block_type:=bt_no;
  IsExtern:=false;
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
   if not(includefile) then
    begin
      writeln(outfile);
      writeln(outfile,'implementation');
      writeln(outfile);
    end;
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
  while not eof(outfile) do
    begin
      readln(outfile,SS);
      writeln(finaloutfile,SS);
    end;

  close(HeaderFile);
  close(outfile);
  close(finaloutfile);
  erase(outfile);
  erase(headerfile);

  PTypeList.Free;
  freedynlibproc.free;
  loaddynlibproc.free;
end.
