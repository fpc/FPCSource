program fpdocstripper;

{
  fpdocstripper  -  Free Pascal fpdoc file stripper
  Copyright (C) 2012-2013 by Reinier Olislagers

  * Takes an FPDoc XML file and removes all elements that have no documentation in them
  * Useful before submitting a documentation patch as it keeps file size down and
    makes it clearer what exactly is documented.

  See the file COPYING, included in this distribution,
  for details about the copyright and license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  To do: currently parses the raw XML; it may be possible to reuse the fpdoc format
  reading code in other units.
}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  DOM, xmlread, xmlwrite;
type

  { TFPDocStripper }

  TFPDocStripper = class(TCustomApplication)
  protected
    FInputFile: string;
    FOutputFile: string;
    FStripComments : Boolean;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure StripEmptyXML(Node:TDOMNode);
    procedure WriteHelp; virtual;
    Property StripComments : Boolean Read FStripComments Write FStripComments;
  end;


procedure TFPDocStripper.StripEmptyXML(Node:TDOMNode);

// Recursive function to process a node and all its child nodes
var
  i: integer;
  E : TDomElement;
  CN : TDomNode;
  B : Boolean;
  
begin
  // Exit procedure if no more nodes to process
  if Node = nil then Exit;

  for i:=Node.ChildNodes.Count-1 downto 0 do
    begin
    StripEmptyXML(Node.ChildNodes[i]);
    end;

  for i:=Node.ChildNodes.Count-1 downto 0 do
    begin
    CN:=Node.ChildNodes[i];
    // Remove all comments
    B:=StripComments and (CN.NodeType=COMMENT_NODE);
    if not B then
      begin
      // Remove children without children or attributes
      B:=(CN.HasChildNodes=false) and
         (CN.HasAttributes=false) and
         (CN.TextContent='');
      // Empty elements that do not link to others   
      if not B then
        begin
        if (CN is TDomElement) then
          begin
          E:=CN as TDomElement;
          B:=(E.NodeName='element') 
             and (E.HasChildNodes=false)
             and (E['name']<>'') and (E['link']='');
          end;
        end;   
      end;
    if B then
      Node.RemoveChild(CN);
    end;
end;

{ TFPDocStripper }

procedure TFPDocStripper.DoRun;
var
  ErrorMsg: String;
  Doc: TXMLDocument;
begin
  // check parameters
  ErrorMsg:=CheckOptions('h','help input: output: keepcomments');
  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    writeln();
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('input') then begin
    FInputFile:=ExpandFileName(GetOptionValue('input'));
  end else begin
    writeln('Error: no input file specified.');
    writeln();
    WriteHelp;
    Terminate;
    Exit;
  end;

  FStripComments:=not HasOption('keepcomments');
  
  if HasOption('output') then begin
    FOutputFile:=ExpandFileName(GetOptionValue('output'));
  end else begin
    writeln('Error: no output file specified.');
    writeln();
    WriteHelp;
    Terminate;
    Exit;
  end;

  if FInputFile=FOutputfile then
    raise Exception.CreateFmt('Input file %s must not be the same as output file.',[FInputFile]);

  if fileexists(FInputFile)=false then
    raise Exception.CreateFmt('Input file %s does not exist.',[FInputFile]);

  try
    ReadXMLFile(Doc,FInputFile);
    StripEmptyXML(Doc.DocumentElement);
    WriteXMLFile(Doc,FOutputFile);
  finally
    Doc.Free;
  end;
  Terminate;
end;

constructor TFPDocStripper.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFPDocStripper.Destroy;
begin
  inherited Destroy;
end;

procedure TFPDocStripper.WriteHelp;
begin
  writeln('Strips undocumented elements and comments');
  writeln('from an fpdoc XML (description/documentation) file.');
  writeln('');
  writeln('Useful before submitting a documentation patch as');
  writeln('it keeps file size down and makes it clear what exactly');
  writeln('is documented.');
  writeln('');
  writeln('Usage: ',ExeName,' -h');
  writeln('--keepcomments');
  writeln('  Do not strip comments');
  writeln('--input=file');
  writeln('  Read specified fpdoc XML file.');
  writeln('--output=file');
  writeln('  Write cleaned output to this file.');
end;

var
  Application: TFPDocStripper;
begin
  Application:=TFPDocStripper.Create(nil);
  Application.Run;
  Application.Free;
end.

