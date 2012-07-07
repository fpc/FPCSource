program test;

{$ifdef FPC}
{$mode delphi}
{$endif}
{$ifdef windows}
{$apptype console}
{$endif}

uses
{$ifdef unix}
 cwstring,
{$endif}
 Classes,SysUtils,uw13015;

procedure writefile(const fn: string);
var
  f:TStream;
  tc:TTestClass;
begin
  writeln('Write component with widestring property to stream');
  tc:=TTestClass.Create(nil);
  writeln('tc.Wstr=',tc.Wstr);
  write('tc.DumpAndCheck()');
  tc.DumpAndCheck;
     
  f:=TFileStream.Create(fn,fmCreate);
  try
    f.WriteComponent(tc);
  finally
    f.Free;
  end;

  tc.free;
end;


procedure readfile(const fn: string);
var
  f:TStream;
  tc:TTestClass;
begin
  writeln('Reading component with widestring property');
  f:=TFileStream.Create(fn,fmOpenRead);
  try
    tc:=TTestClass(f.ReadComponent(nil));
    if Assigned(tc) then
      begin
        writeln('tc.Wstr=',tc.Wstr);
        write('tc.DumpAndCheck()');
        tc.DumpAndCheck;
      end;
  finally
    f.Free;
  end;

  tc.free;
end;


const utf8str : array[0..84] of char=(
  'T','P','F','0',#010,'T','T','e','s','t','C','l','a','s','s',
  #000,#004,'W','s','t','r',#020,'9',#000,#000,#000,#208,#191,#209,#128,
  #208,#184,#208,#178,#208,#181,#209,#130,',',' ',#208,#191,#209,#128,#209,
  #139,#208,#178,#209,#150,#209,#130,#208,#176,#208,#189,#209,#140,#208,#189,
  #208,#181,' ','-',' ','p','r',#195,#188,'f','u','n','g',' ','s',
  'p','a',#195,#159,' ','g','u','t',#000,#000);

var
  f: file;
begin
  RegisterClasses([TTestClass]);

  WriteFile('test.bin');
  ReadFile('test.bin');
  DeleteFile('test.bin');
  
  assign(f,'tw13015-utf8.bin');
  rewrite(f,1);
  blockwrite(f,utf8str,sizeof(utf8str));
  close(f);
  ReadFile('tw13015-utf8.bin');
  DeleteFile('tw13015-utf8.bin');
end.
