{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ exceptions aren't implemented yet in the compiler }
{$define NoExceptions}

{ determine the type of the resource/form file }
{$define Win16Res}
unit Classes;

interface

uses
  objpas,dos;

{$i classesh.inc}

implementation

{$i classes.inc}

{****************************************************************************}
{*                         THandleStream                                    *}
{****************************************************************************}

  procedure THandleStream.SetSize(NewSize: Longint);

    begin
       inherited SetSize(NewSize);
       {!!!!!}
    end;

  constructor THandleStream.Create(AHandle: Integer);

    begin
       inherited Create;
       FHandle:=AHandle;
    end;

  function THandleStream.Read(var Buffer; Count: Longint): Longint;

    begin
       {!!!!!}
    end;

  function THandleStream.Write(const Buffer; Count: Longint): Longint;

    begin
       {!!!!!}
    end;

  function THandleStream.Seek(Offset: Longint; Origin: Word): Longint;

    var
       regs : registers;

    begin
       regs.ebx:=FHandle;
       regs.ecx:=pos shr 16;
       regs.edx:=Offset and $ffff;
       regs.eax:=$4200+Origin;
       Intr($21,regs);
       if (regs.realflags and carryflag) <> 0 then
         InOutRes:=lo(regs.realeax);
    end;

{****************************************************************************}
{*                          TFileStream                                     *}
{****************************************************************************}

  constructor TFileStream.Create(const FileName: string; Mode: Word);

    begin
       {!!!!!}
    end;

  destructor TFileStream.Destroy;

    begin
       {!!!!!}
    end;

end.

{
  $Log$
  Revision 1.1  1998-05-04 12:16:01  florian
    + Initial revisions after making a new directory structure

}