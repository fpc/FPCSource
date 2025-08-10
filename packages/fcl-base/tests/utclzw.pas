unit utcLZW;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, punit, lzwstream;

Procedure RegisterTests;

implementation

{ TTestLZW }

{$i filec.inc}
{$i filed.inc}


function TestFileC : TTestString;

Var
  Z : TLZWDecompressionStream;
  C,D : TBytesStream;
  B : TBytes;
  I,R : Integer;

begin
  result:='';
  D:=Nil;
  Z:=Nil;
  C:=TBytesStream.Create([]);
  try
    C.WriteBuffer(filec,sizeof(filec));
    C.Position:=0;
    D:=TBytesStream.Create;
    D.WriteBuffer(filed,sizeof(filed));
    D.Position:=0;
    Z:=TLZWDecompressionStream.Create(C,[zoTIFFCodes]);
    Z.SourceOwner:=False;
    SetLength(B,D.Size);
    R:=Z.Read(B[0],D.Size);
    AssertEquals('Correct length read',D.Size,R);
    For I:=0 to Length(B)-1 do
      AssertEquals('Byte '+IntToStr(I),PByte(D.Memory)[i],B[i]);
  finally
    C.Free;
    D.Free;
    Z.Free;
  end;

end;

procedure RegisterTests;

begin
  AddSuite('TTestLZW');
  AddTest('TestFileC', @TestFileC, 'TTestLZW');
end;

end.

