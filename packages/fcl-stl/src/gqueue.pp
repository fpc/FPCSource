{
   This file is part of the Free Pascal FCL library.
   BSD parts (c) 2011 Vlado Boza

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode objfpc}

{$IFNDEF FPC_DOTTEDUNITS}
unit gqueue;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Stl.Deque;
{$ELSE FPC_DOTTEDUNITS}
uses gdeque;
{$ENDIF FPC_DOTTEDUNITS}

type 
  generic TQueue<T>=class
  private
  type 
    TContainer = specialize TDeque<T>;
  var
    FData:TContainer;
  public
    procedure Push(value:T);inline;
    procedure Pop();inline;
    function Front():T;inline;
    function Size():SizeUInt;inline;
    function IsEmpty():boolean;inline;
    constructor Create;
    destructor Destroy;override;
end;

implementation

constructor TQueue.Create;
begin
  FData:=TContainer.Create;
end;

destructor TQueue.Destroy;
begin
  FData.Destroy;
end;

procedure TQueue.Push(value:T);inline;
begin
  FData.PushBack(value);
end;

procedure TQueue.Pop();inline;
begin
  FData.PopFront;
end;

function TQueue.Front:T;inline;
begin
  Front:=FData.Front;
end;

function TQueue.Size:SizeUInt;inline;
begin
  Size:=FData.Size;
end;

function TQueue.IsEmpty:boolean;inline;
begin
  IsEmpty:=FData.IsEmpty;
end;

end.
