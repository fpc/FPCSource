{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi

    Output messages handler

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit msghandler;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils;

type

  { TMessages }

  TMessages = class
  private
    fVerboseSet : boolean;
    fVerbose : boolean;
    fVerbCache : TStringList;
    fStdOut : text;
    fStdErr : text;
    procedure SetVerbose(const aValue : boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoError(const aMsg : string);
    procedure DoVerbose(const aMsg : string);
    procedure Flush;
    property Verbose : boolean read fVerbose write SetVerbose;
  end;
  
var Messages : TMessages;

procedure Halt(errnum:Longint);

implementation

procedure Halt(errnum:Longint);
begin
  Messages.Flush;
  System.Halt(errnum);
end;

{ TMessages }

procedure TMessages.SetVerbose(const aValue: boolean);
var i : integer;
begin
  fVerbose:=aValue;
  if fVerboseSet then exit;

  fVerboseSet:=true;
  if fVerbose then //output all verbose messages we didn't output before
    for i:=0 to fVerbCache.Count-1 do
      writeln(fStdOut,'Debug: '+fVerbCache[i]);

  FreeAndNil(fVerbCache);
end;

constructor TMessages.Create;
begin
  fVerbose:=false;
  fVerboseSet:=false;
  fVerbCache:=TStringList.Create;
  fStdOut:=stdout;
  fStdErr:=stderr;
end;

destructor TMessages.Destroy;
begin
  if fVerbCache<>nil then
    fVerbCache.Free;
end;

procedure TMessages.DoError(const aMsg: string);
begin
  writeln(fStdErr,'Error: '+aMsg);
end;

procedure TMessages.DoVerbose(const aMsg: string);
begin
  if not fVerboseSet then
  begin
    fVerbCache.Add(aMsg);
    exit;
  end;
  if fVerbose then
    writeln(fStdOut,'Debug: '+aMsg);
end;

procedure TMessages.Flush;
begin
  System.Flush(fStdOut);
  System.Flush(fStdErr);
end;

initialization
  Messages:=TMessages.Create;


finalization
  Messages.Free;

end.
