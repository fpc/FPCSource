{ Copyright (C) <2005> <Andrew Haines> chmls.lpr

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING, included in this distribution,
  for details about the copyright.
}
program chmls;

{$IFDEF MSWINDOWS}
{$apptype console}
{$ENDIF}

{$mode objfpc}{$H+}

uses
  Classes, GetOpts, SysUtils, Types,
  chmreader, chmbase;

type

  { TListObject }

  TListObject = class
    Section  : Integer;
    count    : integer;
    donotpage: boolean;
    procedure OnFileEntry(Name: String; Offset, UncompressedSize, ASection: Integer);
  end;

   TExtractAllObject = class
    basedir : string;
    r       : TChmReader;
    lastone_was_point : boolean;
    procedure OnFileEntry(Name: String; Offset, UncompressedSize, ASection: Integer);
  end;


  TCmdEnum = (cmdList,cmdExtract,cmdExtractall,cmdNone);        // One dummy element at the end avoids rangecheck errors.

Const
  CmdNames : array [TCmdEnum] of String = ('LIST','EXTRACT','EXTRACTALL','');

var
  theopts : array[1..2] of TOption;


Procedure Usage;

begin
  Writeln(StdErr,'Usage: chmls [switches] [command] [command specific parameters]');
  writeln(stderr);
  writeln(stderr,'Switches : ');
  writeln(stderr,' -h, --help  : this screen');
  writeln(stderr,' -n          : do not page list output');
  writeln(stderr);
  writeln(stderr,'Where command is one of the following or if omitted, equal to LIST.');
  writeln(stderr,' list     <filename> [section number] ');
  writeln(stderr,'            Shows contents of the archive''s directory');
  writeln(stderr,' extract  <chm filename> <filename to extract> [saveasname]');
  writeln(stderr,'            Extracts file "filename to get" from archive "filename",');
  writeln(stderr,'            and, if specified, saves it to [saveasname]');
  writeln(stderr,' extractall <chm filename> [directory]');
  writeln(stderr,'            Extracts all files from archive "filename" to directory ');
  writeln(stderr,'            "directory"');

  Halt(1);
end;

procedure WrongNrParam(cmd:string;number:integer);

begin
  writeln(stderr,' Wrong number of parameters for ',cmd,' ',number);
  usage;
  halt(1);
end;

procedure InitOptions;
begin
  with theopts[1] do
   begin
    name:='help';
    has_arg:=0;
    flag:=nil;
    value:=#0;
  end;
  with theopts[2] do
   begin
    name:='';
    has_arg:=0;
    flag:=nil;
  end;
end;

procedure WriteStrAdj(Str: String; CharWidth: Integer);
// Changed to WriteStrADJ (for adjust), since 2.4.0 writestr is a builtin
// Why doesn't Write() allow left aligned columns?, sigh.
  var
    OutString: String;
    Len: Integer;
  begin
    Len := Length(Str);
    SetLength(OutString, CharWidth-Len);
    FillChar(OutString[1], CharWidth-Len, ' ');
    Write(OutString + Str); // to stdout
  end;

{ TListObject }


function craftpath(pth:string;filename:String):string;

var lenpth,lenfn:integer;
    pthends,filenameends : Boolean;
begin
  lenpth:=length(pth); lenfn :=length(filename);

  if lenpth=0 then
    exit(filename);

  pthends:=false;  filenameends:=false;
  if (lenpth>0) and (pth[lenpth] in ['/','\']) then
    pthends:=true;

  if (lenfn>0) and (filename[1] in ['/','\']) then
    filenameends:=true;

  if pthends and filenameends then
      result:=copy(pth,1,lenpth-1)+filename
  else
    if pthends or filenameends then
        result:=pth+filename
    else
       result:=pth+pathsep+filename;
end;


procedure TListObject.OnFileEntry(Name: String; Offset, UncompressedSize,
  ASection: Integer);
begin
  Inc(Count);
  if (Section > -1) and (ASection <> Section) then Exit;
  if (Count = 1) or ((Count mod 40 = 0) and not donotpage) then
    WriteLn(StdErr, '<Section> <Offset> <UnCompSize>  <Name>');
  Write(' ');
  Write(ASection);
  Write('      ');
  WriteStrAdj(IntToStr(Offset), 10);
  Write('  ');
  WriteStrAdj(IntToStr(UncompressedSize), 11);
  Write('  ');
  WriteLn(Name);
end;

procedure TExtractAllObject.OnFileEntry(Name: String; Offset, UncompressedSize,
  ASection: Integer);
var mem : TMemoryStream;
    s   : String;
    len : integer;
procedure wrpoint;
begin
      if lastone_was_point then
        writeln;
      lastone_was_point:=false;
end;
begin
  len:=Length(Name);
  if ((Len>0) and (name[len]='/')) then
    exit; // directory or empty file

  if (UncompressedSize=0) Then
    begin
      WrPoint;
      Writeln(stderr,'Skipping empty file ',Name);
      exit;
    end;
  if ((Len>0) and (name[1]=':')) then
    begin
      WrPoint;
      Writeln(stderr,'Skipping internal file : ',Name);
      exit;
    end;
  mem:=r.getobject(name);
  if assigned(mem) then
    begin
      s:=craftpath(basedir,name);
      ForceDirectories(extractfiledir(s));
      try
         mem.savetofile(s);
         write('.');
         lastone_was_point:=true;
      except
        on e : exception do
          begin
            WrPoint;
            Writeln(Stderr,'Error saving ',name,' to ',s,'.'            );
          end;
       end;
    end
  else
    begin
      Writeln(Stderr,'Can''t extract ',name);
    end;
end;

var donotpage:boolean=false;

procedure ListChm(Const Name:string;Section:Integer);
var
  ITS: TITSFReader;
  Stream: TFileStream;
  JunkObject: TListObject;

begin
  if not Fileexists(name) then
    begin
      writeln(stderr,' Can''t find file ',name);
      halt(1);
    end;

  Stream := TFileStream.Create(name, fmOpenRead);
  JunkObject := TListObject.Create;
  JunkObject.Section:=Section;
  JunkObject.Count:=0;
  JunkObject.DoNotPage:=DoNotPage;

  ITS:= TITSFReader.Create(Stream, True);
  ITS.GetCompleteFileList(@JunkObject.OnFileEntry);

  WriteLn('Total Files in chm: ', JunkObject.Count);
  ITS.Free;
  JunkObject.Free;
end;

procedure ExtractFile(chm,readfrom,saveto:string);
var
  fs: TFileStream;
  m : TMemoryStream;
  r : TChmReader;
begin
  if not Fileexists(chm) then
    begin
      writeln(stderr,' Can''t find file ',chm);
      halt(1);
    end;

  if (length(readfrom)>1) and (readfrom[1]<>'/') then
    readfrom:='/'+readfrom;

  fs:=TFileStream.create(chm,fmOpenRead);
  r:=TChmReader.Create(fs,True);
  m:=r.getobject(readfrom);
  if assigned(m) then
    begin
      try
        Writeln('Extracting ms-its:/',chm,'::',readfrom,' to ',saveto);
        m.savetofile(saveto);
      except
        on e : exception do
          writeln('Can''t write to file ',saveto);
        end;
     end
  else
    begin
      writeln(stderr,'Can''t find file ',readfrom,' in ',chm);
      halt(1);
    end;
end;

procedure ExtractFileAll(chm,dirto2:string);
var
  fs: TFileStream;
  m : TMemoryStream;
  r : TChmReader;
  fl : boolean;
  ListAll : TExtractAllObject;
begin
  if not Fileexists(chm) then
    begin
      writeln(stderr,' Can''t find file ',chm);
      halt(1);
    end;


  if not directoryexists(dirto2) then
    begin
      fl:=false;
      try
        mkdir(dirto2);
        fl:=directoryexists(dirto2);
      except
       on e : exception do ;
       end;
      if not fl then
        begin
          writeln(stderr,'Directory ',dirto2,' doesn''t exist, and trying to create it fails');
          halt(1);
        end;
      end;


  fs:=TFileStream.create(chm,fmOpenRead);
  r:=TCHMReader.create(fs,true);
  Listall:= TExtractAllObject.Create;
  ListAll.basedir:=dirto2;
  ListAll.r:=r;
  ListAll.lastone_was_point:=false;
  r.GetCompleteFileList(@ListAll.OnFileEntry);

  r.free;
end;


procedure buildarglist(var params: TStringDynArray;var cmd :TCmdEnum);

var s           : ansistring;
    j,k         : Integer;

begin
  s:=uppercase(paramstr(optind));
  cmd:=Low(TCMDEnum);
  While (cmd<>high(TCmdEnum)) and (s<>CmdNames[cmd]) do
    inc(cmd);
  if cmd=CmdNone then
    begin
      writeln(stderr,' Using cmdls without command is deprecated, this may be removed in a future version');
      writeln(stderr,' Please consider using the "list" command');
      cmd:=CmdList;      // no cmd found -> list  In the future we can also do a name check here for the default (symlink on unix)
      k:=optind;
    end
  else
    begin
      k:=optind+1;
    end;
  setlength(params,paramcount-k+1);
  for j:=k to paramcount do
    params[j-k]:=paramstr(j);
end;

Var
  LocalParams : TStringDynArray;
  c:   char;
  i,
  Params,
  OptionIndex : Longint;
  cmd         : TCmdEnum;
  section     : Integer = -1;

// Start of program
begin
  InitOptions;
  Writeln(stderr,'chmls, a CHM utility. (c) 2010 Free Pascal core.');
  Writeln(Stderr);
  repeat
    c:=getlongopts('hn',@theopts[1],optionindex);
    case c of
      #0 : begin
             case optionindex-1 of
               0 : begin;
                     Usage;
                     Halt;
                   end;
                end;
           end;
      'n'     : donotpage:=true;
      '?','h' :
            begin
              writeln('unknown option',optopt);
              usage;
              halt;
            end;
   end; { case }
 until c=endofoptions;

 params:=Paramcount-optind+1;

 if params>0 then
  begin
    BuildArgList(localparams,cmd);
    case cmd of
      cmdlist : begin
                  case length(localparams) of
                    1 : ListChm(localparams[0],Section);
                    2 : begin
                          if not TryStrToInt(localparams[1],section) then
                            begin
                              writeln(stderr,' Invalid value for section ',localparams[2]);
                              usage;
                              halt(1);
                            end;
                          ListChm(localparams[0],Section);
                        end;
                  else
                    WrongNrParam(cmdnames[cmd],length(localparams));
                   end; {case}
                end; { cmdlist}
      cmdextract : begin
                     case length(localparams) of
                      2: ExtractFile(localparams[0],localparams[1],extractfilename(localparams[1]));
                      3: ExtractFile(localparams[0],localparams[1],localparams[2]);
                     else
                       WrongNrParam(cmdnames[cmd],length(localparams));
                     end;
                   end;
      cmdextractall: begin
                      if length(localparams)=2 then
                        ExtractFileall(localparams[0],localparams[1])
                      else
                        WrongNrParam(cmdnames[cmd],length(localparams));
                     end;
      end; {case cmd of}
  end
 else
   begin
     Usage;
     halt;
   end;

end.

