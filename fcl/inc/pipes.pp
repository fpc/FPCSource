{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Michael Van Canneyt

    Implementation of pipe stream.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit Pipes;

Interface

Uses sysutils,Classes;

Type

  ENoReadPipe = Class(ESTreamError);
  ENoWritePipe = Class (EStreamError);
  EPipeSeek = Class (EStreamError);
  EPipeCreation = Class (Exception);

  TPipeStream = Class (THandleStream)
    public
      Function Seek (Offset : Longint;Origin : Word) : longint;override;
    end;

  TInputPipeStream = Class(TPipeStream)
    public
      Function Write (Const Buffer; Count : Longint) :Longint; Override;
    end;

  TOutputPipeStream = Class(TPipeStream)
    Public
      Function Read (Var Buffer; Count : Longint) : longint; Override;
    end;

Procedure CreatePipeStreams (Var InPipe : TInputPipeStream;
                             Var OutPipe : TOutputPipeStream);

Const EPipeMsg = 'Failed to create pipe.';
      ENoReadMSg = 'Cannot read from OuputPipeStream.';
      ENoWriteMsg = 'Cannot write to InputPipeStream.';
      ENoSeekMsg = 'Cannot seek on pipes';


Implementation

{$i pipes.inc}

Procedure CreatePipeStreams (Var InPipe : TInputPipeStream;
                             Var OutPipe : TOutputPipeStream);

Var InHandle,OutHandle : Longint;

begin
  if CreatePipeHandles (InHandle, OutHandle) then
    begin
    Inpipe:=TinputPipeStream.Create (InHandle);
    OutPipe:=ToutputPipeStream.Create (OutHandle);
    end
  Else
    Raise EPipeCreation.Create (EPipeMsg)
end;

Function TPipeStream.Seek (Offset : Longint;Origin : Word) : longint;

begin
  Raise EPipeSeek.Create (ENoSeekMsg);
end;

Function TInputPipeStream.Write (Const Buffer; Count : Longint) : longint;

begin
  Raise ENoWritePipe.Create (ENoWriteMsg);
end;

Function TOutputPipeStream.Read(Var Buffer; Count : Longint) : longint;

begin
  Raise ENoReadPipe.Create (ENoReadMsg);
end;

end.