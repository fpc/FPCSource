unit utclzw;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, lzwstream;

Type

  { TTestLZW }

  TTestLZW = CLass(TTestCase)
  Published
    Procedure TestFileC;
  end;

implementation

{ TTestLZW }

{$i filec.inc}
{$i filed.inc}


procedure TTestLZW.TestFileC;

Var
  Z : TLZWDecompressionStream;
  C,D : TBytesStream;
  B : TBytes;
  I,R : Integer;

begin
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

initialization
  RegisterTest(TTestLZW);
end.

