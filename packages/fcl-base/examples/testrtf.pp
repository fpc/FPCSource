{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    This program demonstrates the RTF parser object.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Program testrtf;

{$mode objfpc}{$h+}

uses rtfpars,classes;

type
  TRTFDemo = class(TObject)
    FFileName : string;
    FParser : TRTFParser;
    Procedure DoDestination;
    procedure dospecial;
    procedure doctrl;
    Procedure Dowrite;
    Procedure Start;
    procedure handleerror ( s : shortstring);
  end;

Var
  RTFDemo : TRTFDemo;

procedure TRTFDemo.DoDestination;
{
  skip all special destinations.
}
begin
  FParser.skipgroup;
end;

procedure TRTFDemo.dospecial;
{
  Don't do anything special.
}
begin
  if FParser.RTFMinor=rtfpar then
    Writeln;
end;


procedure TRTFDemo.doctrl;

begin
  case Fparser.rtfmajor of
    rtfdestination         : dodestination;
    rtfspecialchar         : dospecial;
    end;
end;


Procedure TRTFDemo.Dowrite;

begin
  { RTFmajor contains the character ASCII Code, we just dump it }
  Write (chr(FParser.RTFMajor));
end;

procedure TRTFDemo.Start;

var Thestream : TFilestream;

begin
  Thestream:=TFileStream.Create(FFileName,fmopenread);
  FParser:=TRTFParser.Create(TheStream);
  FParser.classcallbacks[rtfText]:=@dowrite;
  FParser.classcallbacks[rtfcontrol]:=@doctrl;
  FParser.onrtferror:=@handleerror;
  FParser.StartReading;
  Fparser.Free;
  Thestream.free;
end;

procedure TRTFDemo.handleerror ( s : shortstring);

begin
  Writeln (stderr,s);
end;

VAr Name : String;

begin
  RTFDemo:=TRTFDemo.Create;
  If Paramstr(1)='' then
    begin
    Write ('Enter filename to process: ');
    Readln (name);
    end
  else
    Name:=Paramstr(1);
  RTFDemo.FFileName:=Name;
  RTFDemo.Start;
  RTFDemo.Free;
end.
