unit kbdutil;

{$MODE objfpc}{$H+}

interface

type
  TKey = record
    X, Y: Integer;
    YTop, YBottom: Integer;
    KeyLabel: string;
  end;
  TKeys = array of TKey;
  TKeyboard = record
    Keys: TKeys;
  end;

function ReadKeyboardFromFile(const FileName: string): TKeyboard;

implementation

function ReadKeyboardFromFile(const FileName: string): TKeyboard;
var
  SaveCtrlZMarksEOF: Boolean;
  InF: TextFile;
  KeyX, KeyY, KeyY1, KeyY2: Integer;
  KeyStr: string;
begin
  SaveCtrlZMarksEOF := CtrlZMarksEOF;
  try
    CtrlZMarksEOF := False;
    FillChar(Result, SizeOf(Result), 0);
    AssignFile(InF, FileName);
    Reset(InF);
    while not EoF(InF) do
    begin
      Read(InF, KeyX);
      if KeyX <> -1 then
      begin
        Readln(InF, KeyY, KeyStr);
        Delete(KeyStr, 1, 1);
        SetLength(Result.Keys, Length(Result.Keys) + 1);
        with Result.Keys[High(Result.Keys)] do
        begin
          X := KeyX;
          Y := KeyY;
          YTop := KeyY;
          YBottom := KeyY;
          KeyLabel := KeyStr;
        end;
      end
      else
      begin
        Readln(InF, KeyX, KeyY1, KeyY2, KeyY, KeyStr);
        Delete(KeyStr, 1, 1);
        SetLength(Result.Keys, Length(Result.Keys) + 1);
        with Result.Keys[High(Result.Keys)] do
        begin
          X := KeyX;
          Y := KeyY;
          YTop := KeyY1;
          YBottom := KeyY2;
          KeyLabel := KeyStr;
        end;
      end;
    end;
    CloseFile(InF);
  finally
    CtrlZMarksEOF := SaveCtrlZMarksEOF;
  end;
end;

end.
