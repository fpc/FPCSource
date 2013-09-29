{ Program to test OS-specific features of the system unit }
{ routines to test:                                       }
{   mkdir()                                               }
{   chdir()                                               }
{   rmdir()                                               }
{   getdir()                                              }
{ This program tests support for non-ASCII chaaracters in }
{ path names                                              }

{ %target=win32,win64,darwin,freebsd,openbsd,netbsd,linux,morphos,haiku,aix,nativent }

Program tdir;
{$codepage utf-8}
{$I-}

{$ifdef unix}
uses
  cwstring;
{$endif}

procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end;
end;


procedure testansi;
const
  dirname: utf8string = 'éż†®';
var
  orgdir, newdir: rawbytestring;
Begin
   Writeln('rawbytestring tests');
   Write('Getting current directory...');
   getdir(0,orgdir);
   test(IOResult,0);
   WriteLn('Passed');
   
   Write('creating new directory...');
   mkdir(dirname);
   test(IOResult,0);
   WriteLn('Passed');

   Write('changing to new directory...');
   chdir(dirname);
   test(IOResult, 0);
   WriteLn('Passed!');

   Write('Getting current directory again...');
   getdir(0,newdir);
   test(IOResult,0);
   WriteLn('Passed');

   Write('Checking whether the current directories are properly relative to each other...');
   if newdir[length(newdir)]=DirectorySeparator then
     setlength(newdir,length(newdir)-1);
   setcodepage(newdir,CP_UTF8);
   if copy(newdir,1,length(orgdir))<>orgdir then
     test(0,1);
   if copy(newdir,length(newdir)-length(dirname)+1,length(dirname))<>dirname then
     test(2,3);
   Writeln('Passed');

   Write('going directory up ...');
   chdir('..');
   test(IOResult, 0);
   WriteLn('Passed!');

   Write('removing directory ...');
   rmdir(dirname);
   test(IOResult, 0);
   WriteLn('Passed!');
end;


procedure testuni;
const
  dirname: unicodestring = 'éż†®';
var
  orgdir, newdir: unicodestring;
Begin
   Writeln('unicodestring tests');
   Write('Getting current directory...');
   getdir(0,orgdir);
   test(IOResult,0);
   WriteLn('Passed');
   
   Write('creating new directory...');
   mkdir(dirname);
   test(IOResult,0);
   WriteLn('Passed');

   Write('changing to new directory...');
   chdir(dirname);
   test(IOResult, 0);
   WriteLn('Passed!');

   Write('Getting current directory again...');
   getdir(0,newdir);
   test(IOResult,0);
   WriteLn('Passed');

   Write('Checking whether the current directories are properly relative to each other...');
   if newdir[length(newdir)]=DirectorySeparator then
     setlength(newdir,length(newdir)-1);
   if copy(newdir,1,length(orgdir))<>orgdir then
     test(0,1);
   if copy(newdir,length(newdir)-length(dirname)+1,length(dirname))<>dirname then
     test(2,3);
   Writeln('Passed');

   Write('going directory up ...');
   chdir('..');
   test(IOResult, 0);
   WriteLn('Passed!');

   Write('removing directory ...');
   rmdir(dirname);
   test(IOResult, 0);
   WriteLn('Passed!');
end;

begin
  { ensure that we get into trouble if at one point defaultsystemcodepage is used }
  SetMultiByteConversionCodePage(CP_ASCII);
  { this test only works in its current form on systems that either use a two byte file system OS API, or whose 1-byte API supports UTF-8 }
  SetMultiByteFileSystemCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  testansi;
  testuni;
end.
