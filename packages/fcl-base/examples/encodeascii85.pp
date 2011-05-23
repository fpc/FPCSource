program encodeascii85;

{$mode objfpc}
{$H+}

uses SysUtils,Classes, ascii85;

Var
  FN : String;
  Enc : TAscii85EncoderStream;
  Src,Dest : TFileStream;

begin
  If (ParamCount=0) then
    begin
    Writeln('usage: encodeascii85 filename');
    halt(1);
    end;
  FN:=ParamStr(1);
  Src:=TFileStream.Create(FN,fmOpenRead);
  try
    FN:=FN+'.a85';
    Dest:=TFileStream.Create(FN,fmCreate);
    try
      Enc:=TAscii85EncoderStream.Create(Dest,72,True);
      try
        Enc.CopyFrom(Src,0);
      finally
        Enc.Free;
      end;
    finally
      Dest.Free;
    end;
  finally
    Src.Free;
  end;
end.
