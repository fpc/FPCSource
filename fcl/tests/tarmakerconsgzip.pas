(**
 Copyright (c) 2001-2006 by Stefan Heymann

 See the file COPYING.DESTRUCTOR, included in this distribution,
 for details about the copyright.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

===============================================================================================
Name    : Main
===============================================================================================
Project : "TarMakerCons" Demo Application for LibTar.pas
===============================================================================================
Subject : Main Window
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
2001-06-19  HeySt  Start
2006-10-20  MvdV   Fork from GUI version to Console app for 
                    easier testing with FPC on various platforms
2006-10-21  MvdV   Fork again test gzipping.
*)

Program TarMakerConsGZip;

{$ifndef FPC}
  Sorry, the zstream unit is afaik FPC only. Maybe it can be gotten
  to work with Delphi easily though. See FPC website
{$endif}

Uses SysUtils,Classes,LibTar,zstream;

var p : string;
    d : TSearchRec;
    TarWriter : TTarWriter;   
    C : TGZFileStream;
begin
  if paramcount<1 then
    begin
      Writeln('TarMakerCons, compresses *.pas files into a .tar');
      writeln('Usage :  TarMakerCons <filename to create>');
      exit;
    end;
  
  C:=TGZFileStream.Create(paramstr(1),gzOpenWrite);
  TarWriter := TTarWriter.Create (C);
  if FindFirst('*.pas',faAnyFile-faDirectory,d)=0 Then
    begin
      repeat
        TarWriter.AddFile (d.name);
      until findnext(d)<>0;
      Findclose(d);
    end;
 TarWriter.free; 
 c.free;
end.
