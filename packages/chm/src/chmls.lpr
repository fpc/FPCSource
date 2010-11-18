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

  { TJunkObject }

  TJunkObject = class
    Section  : Integer;
    count    : integer;
    donotpage: boolean;
    procedure OnFileEntry(Name: String; Offset, UncompressedSize, ASection: Integer);
  end;

  TCmdEnum = (cmdList,cmdExtract,cmdNone);        // One dummy element at the end avoids rangecheck errors.

Const
  CmdNames : array [TCmdEnum] of String = ('LIST','EXTRACT','');

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
  Halt(1);
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

{ TJunkObject }

procedure TJunkObject.OnFileEntry(Name: String; Offset, UncompressedSize,
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

var donotpage:boolean=false;

procedure ListChm(Const Name:string;Section:Integer);
var
  ITS: TITSFReader;
  Stream: TFileStream;
  JunkObject: TJunkObject;

begin
  if not Fileexists(name) then
    begin
      writeln(stderr,' Can''t find file ',name);
      halt(1);
    end;

  Stream := TFileStream.Create(name, fmOpenRead);
  JunkObject := TJunkObject.Create;
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
                    begin
                      writeln(stderr,' Wrong number of parameters for LIST ',length(localparams));
                      usage;
                      halt(1);
                    end
                   end; {case}
                end; { cmdlist}
      cmdextract : begin
                     case length(localparams) of
                      2: ExtractFile(localparams[0],localparams[1],extractfilename(localparams[1]));
                      3: ExtractFile(localparams[0],localparams[1],localparams[2]);
                     else
                      begin
                        writeln(stderr,' Wrong number of parameters for LIST ',length(localparams));
                        usage;
                        halt(1);
                      end
                     end;
                   end;
      end; {case cmd of}
  end
 else
   begin
     Usage;
     halt;
   end;

end.

