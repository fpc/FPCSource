program decodeascii85;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ascii85;

var
  B : TAscii85DecoderStream;
  Fin,Fout : TFileStream;
  Buf : Array[1..1024] of Byte;
  FN : String;
  Count : Integer;

begin
  If (ParamCount=0) then
    begin
    Writeln('usage: decodeascii85 filename');
    halt(1);
    end;
  FN:=ParamStr(1);
  FIn:=TFileStream.Create(FN,fmOpenRead);
  B:=TAscii85DecoderStream.Create(FIn);
  try
    FN:=ChangeFileExt(FN,'');
    FOut:=TFileStream.Create(FN,fmCreate);
    try
      Repeat
        Count:=B.Read(Buf,SizeOf(Buf));
        If Count>0 then
          FOut.WriteBuffer(Buf,Count);
      Until (Count<SizeOf(Buf));
    Finally
      Fout.Free;
    end;
  finally
    B.Free;
  end;
end.                                  