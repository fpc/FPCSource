{$mode objfpc}
{$h+}
program b64;

uses classes,base64;

Function EncodeBase64(S : String) : String;

Var
  S1,S2 : TStringStream;

begin
  S1:=TStringStream.Create(S);
  Try
    S1.Position:=0;
    S2:=TStringStream.Create('');
    Try
      With TBase64EncodingStream.Create(S2) do
        Try
          CopyFrom(S1,S1.Size);
        Finally
          Free;
        end;
      Result:=S2.DataString;
    finally
      S2.Free;
    end;
 finally
   S1.Free;
 end;
end;

Var
  S : String;

begin
  S:='String to be encoded';
  Writeln('"',S,'" -> "',EncodeBase64(S),'"');
end.
