unit futils;

{$mode objfpc}
{$h+}

Interface

Const
{$ifdef win32}
  PathSeparator='\';
{$else}
  PathSeparator='/';
{$endif}


Function StripTrailingSeparator(Const Dir : String) : String;
Function AddTrailingSeparator(Const Dir : String) : String;
Function FileSizeToString(Size: Int64) : String;
Function FileAttrsToString(FileAttrs : Integer) : String;

Implementation

Uses sysutils;

Function  StripTrailingSeparator(Const Dir : String) : String;

Var
  L : Integer;

begin
  Result:=Dir;
  L:=Length(result);
  If (L>1) and (Result[l]=PathSeparator) then
    SetLength(Result,L-1);
end;

Function  AddTraiLingSeparator(Const Dir : String) : String;

Var
  L : Integer;

begin
  Result:=Dir;
  L:=Length(Result);
  If (L>0) and (Result[l]<>PathSeparator) then
    Result:=Result+PathSeparator;
end;

Function  FileSizeToString(Size: Int64) : String;

Const
  Sizes : Array [0..4] of String =
     ('Bytes','Kb','Mb','Gb','Tb');
Var
    F : Double;
    I : longint;

begin
  If Size>1024 Then
    begin
    F:=Size;
    I:=0;
    While (F>1024) and (I<4) do
      begin
      F:=F / 1024;
      Inc(i);
      end;
    Result:=Format('%4.2f %s',[F,Sizes[i]]);
    end
  else
    Result:=Format('%d %s',[Size,Sizes[0]]);
end;

Function FileAttrsToString(FileAttrs : Integer) : String;

Const
  Attrs : Array[1..4] of integer =
          (faArchive,faReadOnly,faHidden,faSysfile);
  AttrChars : Array[1..4] of char =
          ('A','R','H','S');

Var
  i : longint;

begin
  Result:='';
  For I:=1 to 4 do
    If (Attrs[i] and FileAttrs)=Attrs[i] then
      Result:=Result+AttrChars[i];
end;

end.