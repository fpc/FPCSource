{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt
    member of the Free Pascal development team

    Chained Streams implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit ChainStream;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type
  { TChainedStream }

  // Stream, backed by several other streams.
  // A read operation will read bytes from the next stream in the chain if there is one, till the requested number of bytes is read.
  // When writing, the current size of the streams is kept.
  // i.e. the write operation overflows to the next stream, if any.

  TChainedStream = class(TProxyAggregateStream)
  private
    FOwnsStreams: Boolean;
    function GetStreamCount: Integer;
    procedure SetOwnsStreams(const aOwnsStreams: Boolean);
  Public
    Constructor Create(aChain : Array of TStream; aOwnsStreams : Boolean = False);
    property StreamCount : Integer Read GetStreamCount;
    Property OwnsStreams : Boolean Read FOwnsStreams Write SetOwnsStreams;
  end;

implementation

{ TChainedStream }

function TChainedStream.GetStreamCount: Integer;
begin
  Result:=Count;
end;

procedure TChainedStream.SetOwnsStreams(const aOwnsStreams: Boolean);
var
  I: Integer;
begin
  FOwnsStreams := aOwnsStreams;
  for I := 0 to Count-1 do
    OwnsStream[I] := FOwnsStreams;
end;

constructor TChainedStream.Create(aChain: array of TStream; aOwnsStreams: Boolean);
Var
  S: TStream;

begin
  inherited Create;

  FOwnsStreams := aOwnsStreams;
  for S in aChain do
    AddStream(S, aOwnsStreams);
end;

end.

