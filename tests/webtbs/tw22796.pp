{ %norun }

program CompileError;

{$mode delphi}{$H+}

//uses Classes;

type
  TWordTagValue = record
  strict private
    FValue: Word;
    FMissingOrInvalid: Boolean; // Try to disable this line !
  public
    class function CreateFromString(const AString: string): TWordTagValue; static;
    property Value: Word read FValue;
  end;

class function TWordTagValue.CreateFromString(const AString: string): TWordTagValue;
begin
  Result.FValue := 0;
end;

begin
end.

