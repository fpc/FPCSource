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
{****************************************************************************}
{*                               TBits                                      *}
{****************************************************************************}

  procedure TBits.Error;

    begin
         Raise EBitsError.Create('');
    end;

  procedure TBits.SetSize(Value: Integer);

    var
       hp : pointer;
       cvalue,csize : Integer;

    begin
       { ajust value to n*8 }
       cvalue:=Value;
       if cvalue mod 8<>0 then
         cvalue:=cvalue+(8-(cvalue mod 8));

       { store pointer to release it later }
       hp:=FBits;

       { ajust size to n*8 }
       csize:=FSize;
       if csize mod 8<>0 then
         csize:=csize+(8-(csize mod 8));

       if FSize>0 then
         begin
            { get new memory }
            GetMem(FBits,cvalue div 8);
            { clear the whole array }
            FillChar(FBits^,cvalue div 8,0);
            { copy old data }
            Move(hp^,FBits^,csize div 8);
         end
       else
         FBits:=nil;

       if assigned(hp) then
         FreeMem(hp,csize div 8);

       FSize:=Value;
    end;

  procedure TBits.SetBit(Index: Integer; Value: Boolean);

    type
       pbyte = ^byte;

    begin
       if (Index>=FSize) or (Index<0)  then
         Error
       else
         begin
            if Value then
              pbyte(FBits)[Index div 8]:=pbyte(FBits)[Index div 8] or
                (1 shl (Index mod 8))
            else
              pbyte(FBits)[Index div 8]:=pbyte(FBits)[Index div 8] and
                not(1 shl (Index mod 8));
         end;
    end;

  function TBits.GetBit(Index: Integer): Boolean;

    type
       pbyte = ^byte;

    begin
       if (Index>=FSize) or (Index<0) then
         Error
       else
         GetBit:=(pbyte(FBits)[Index div 8] and (1 shl (Index mod 8)))<>0;
    end;

  destructor TBits.Destroy;

    var
       csize : Integer;

    begin
       { ajust size to n*8 }
       csize:=FSize;
       if csize mod 8<>0 then
         csize:=csize+(8-(csize mod 8));
       if assigned(FBits) then
         FreeMem(FBits,csize);
       inherited Destroy;
    end;

  function TBits.OpenBit: Integer;

    type
       pbyte = ^byte;

    var
       i : Integer;

    begin
       for i:=0 to FSize-1 do
         if (pbyte(FBits)[i div 8] and (1 shl (i mod 8)))=0 then
           begin
              OpenBit:=i;
              exit;
           end;
       SetSize(FSize+1);
       OpenBit:=FSize-1;
    end;


{
  $Log$
  Revision 1.3  1998-11-04 14:36:29  michael
  Error handling always with exceptions

  Revision 1.2  1998/11/04 10:46:42  peter
    * exceptions work

  Revision 1.1  1998/05/04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
