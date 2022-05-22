{ %OPT=-gw -godwarfproperties }
program tdwarfproperties2;

{$mode objfpc}{$H+}

type
  TClassProp=class(TObject)
  private
    class var FProcessorCount: LongWord;
  public
    class property ProcessorCount: LongWord read FProcessorCount;
  end;

var
  P: TClassProp;

begin
end.

