{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ *********************************************************************
  *                         TFiler                                    *
  *********************************************************************}

Constructor TFiler.Create(Stream: TStream; BufSize: Integer);

begin
  FStream:=Stream;
  GetMem(FBuffer,BufSize);
  FBufSize:=BufSize;
end;


Destructor TFiler.Destroy;

begin
  If Assigned(FBuffer) then 
   FreeMem (FBuffer,FBufSize);
end;

{
  $Log$
  Revision 1.2  1998-08-24 12:38:23  michael
  small fixes

  Revision 1.1  1998/05/04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
