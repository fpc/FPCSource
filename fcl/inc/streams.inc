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
{*                             TStream                                      *}
{****************************************************************************}

  function TStream.GetPosition: Longint;

    begin
       GetPosition:=Seek(0,soFromCurrent);
    end;

  procedure TStream.SetPosition(Pos: Longint);

    begin
       Seek(soFromBeginning,Pos);
    end;

  function TStream.GetSize: Longint;

    var
       p : longint;

    begin
       p:=GetPosition;
       GetSize:=Seek(soFromEnd,0);
       Seek(soFromBeginning,p);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
       SetPosition(NewSize);
    end;

  procedure TStream.ReadBuffer(var Buffer; Count: Longint);

    begin
       if Read(Buffer,Count)<Count then
{$ifdef NoExceptions}
         ;
{$else}
         Raise(EReadError);
{$endif}
    end;

  procedure TStream.WriteBuffer(const Buffer; Count: Longint);

    begin
       if Write(Buffer,Count)<Count then
{$ifdef NoExceptions}
         ;
{$else}
         Raise(EWriteError);
{$endif}
    end;

  function TStream.CopyFrom(Source: TStream; Count: Longint): Longint;

    var
       i : longint;
       buffer : array[0..1023] of byte;

    begin
       CopyFrom:=0;
       while Count>0 do
         begin
            if (Count>sizeof(buffer)) then
              i:=sizeof(Buffer)
            else
              i:=Count;
            i:=Source.Read(buffer,i);
            i:=Write(buffer,i);
            dec(count,i);
            CopyFrom:=CopyFrom+i;
            if i=0 then
              exit;
         end;
    end;

  function TStream.ReadComponent(Instance: TComponent): TComponent;

    var
       Reader : TReader;

    begin
       Reader.Create(Self,1024);
       if assigned(Instance) then
         ReadComponent:=Reader.ReadRootComponent(Instance)
       else
         begin
            {!!!!!}
         end;
       Reader.Destroy;
    end;

  function TStream.ReadComponentRes(Instance: TComponent): TComponent;

    begin
       {!!!!!}
    end;

  procedure TStream.WriteComponent(Instance: TComponent);

    var
       Writer : TWriter;

    begin
       Writer.Create(Self,1024);
       Writer.WriteRootComponent(Instance);
       Writer.Destroy;
    end;

  procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);

    var
       startpos,s : longint;

    begin
{$ifdef Win16Res}
       { Numeric resource type }
       WriteByte($ff);
       { Application defined data }
       WriteWord($0a);
       { write the name as asciiz }
       WriteBuffer(ResName[1],length(ResName));
       WriteByte(0);
       { Movable, Pure and Discardable }
       WriteWord($1030);
       { size isn't known yet }
       WriteDWord(0);
       startpos:=GetPosition;
       WriteComponent(Instance);
       { calculate size }
       s:=GetPosition-startpos;
       { back patch size }
       SetPosition(startpos-4);
       WriteDWord(s);
{$endif Win16Res}
    end;

  procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);

    begin
       {!!!!!}
    end;

  procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);

    begin
       {!!!!!}
    end;

  procedure TStream.ReadResHeader;

    begin
{$ifdef Win16Res}
       try
         { application specific resource ? }
         if ReadByte<>$ff then
           raise EInvalidImage;
         if ReadWord<>$000a then
           raise EInvalidImage;
         { read name }
         while ReadByte<>0 do
           ;
         { check the access specifier }
         if ReadWord<>$1030 then
           raise EInvalidImage;
         { ignore the size }
         ReadDWord;
       except
{/////
         on EInvalidImage do
           raise;
         else
           raise(EInvalidImage);
}
       end;
{$endif Win16Res}
    end;

  function TStream.ReadByte : Byte;

    var
       b : Byte;

    begin
       ReadBuffer(b,1);
       ReadByte:=b;
    end;

  function TStream.ReadWord : Word;

    var
       w : Word;

    begin
       ReadBuffer(w,2);
       ReadWord:=w;
    end;

  function TStream.ReadDWord : Cardinal;

    var
       d : Cardinal;

    begin
       ReadBuffer(d,4);
       ReadDWord:=d;
    end;

  Function TStream.ReadAnsiString : String;

    Type PByte = ^Byte;

    Var TheSize : Longint;
        P : PByte ;
  

  begin
    ReadBuffer (TheSize,SizeOf(TheSize));
    //!! SetLength(Result,Size);
    //!! Illegal typecast if no AnsiStrings defined.
    //!! ReadBuffer (Pointer (Result^),Size);
    //!! P:=Pointer(Result^)+Size;
    //!! p^:=0;
    end;

  Procedure TStream.WriteAnsiString (S : String);
  
  Var L : Longint;

  begin
    L:=Length(S);
    WriteBuffer (L,SizeOf(L));
    //!! WriteBuffer (Pointer(S)^,L);
  end;

  procedure TStream.WriteByte(b : Byte);

    begin
       WriteBuffer(b,1);
    end;

  procedure TStream.WriteWord(w : Word);

    begin
       WriteBuffer(w,2);
    end;

  procedure TStream.WriteDWord(d : Cardinal);

    begin
       WriteBuffer(d,4);
    end;







{****************************************************************************}
{*                             THandleStream                                *}
{****************************************************************************}

Procedure THandleStream.SetSize(NewSize: Longint);

begin
end;


Constructor THandleStream.Create(AHandle: Integer);

begin
end;


function THandleStream.Read(var Buffer; Count: Longint): Longint;

begin
end;


function THandleStream.Write(const Buffer; Count: Longint): Longint;

begin
end;


function THandleStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
end;


{****************************************************************************}
{*                             TFileStream                                  *}
{****************************************************************************}

constructor TFileStream.Create(const FileName: string; Mode: Word);

begin
end;


destructor TFileStream.Destroy;

begin
end;

{****************************************************************************}
{*                             TCustomMemoryStream                          *}
{****************************************************************************}

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; Size: Longint);

begin
end;


function TCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;

begin
end;


function TCustomMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
end;


procedure TCustomMemoryStream.SaveToStream(Stream: TStream);

begin
end;


procedure TCustomMemoryStream.SaveToFile(const FileName: string);

begin
end;


{****************************************************************************}
{*                             TMemoryStream                                *}
{****************************************************************************}


procedure TMemoryStream.SetCapacity(NewCapacity: Longint);

begin
end;


function TMemoryStream.Realloc(var NewCapacity: Longint): Pointer;

begin
end;


destructor TMemoryStream.Destroy;

begin
end;


procedure TMemoryStream.Clear;

begin
end;


procedure TMemoryStream.LoadFromStream(Stream: TStream);

begin
end;


procedure TMemoryStream.LoadFromFile(const FileName: string);

begin
end;


procedure TMemoryStream.SetSize(NewSize: Longint);

begin
end;


function TMemoryStream.Write(const Buffer; Count: Longint): Longint; 

begin
end;


{****************************************************************************}
{*                             TStringStream                                *}
{****************************************************************************}

procedure TStringStream.SetSize(NewSize: Longint); 

begin
end;


constructor TStringStream.Create(const AString: string);

begin
end;


function TStringStream.Read(var Buffer; Count: Longint): Longint; 

begin
end;


function TStringStream.ReadString(Count: Longint): string;

begin
end;


function TStringStream.Seek(Offset: Longint; Origin: Word): Longint; 

begin
end;


function TStringStream.Write(const Buffer; Count: Longint): Longint; 

begin
end;


procedure TStringStream.WriteString(const AString: string);

begin
end;



{****************************************************************************}
{*                             TResourceStream                              *}
{****************************************************************************}

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar);

begin
end;


constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);

begin
end;


constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);

begin
end;


destructor TResourceStream.Destroy; 

begin
end;


function TResourceStream.Write(const Buffer; Count: Longint): Longint; 

begin
end;


{
  $Log$
  Revision 1.3  1998-05-06 12:58:35  michael
  + Added WriteAnsiString method to TStream

  Revision 1.2  1998/05/05 15:25:04  michael
  + Fix to be able to compile from florian

  Revision 1.1  1998/05/04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
