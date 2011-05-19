program TNP;
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
type
  generic GenericA<T>=class
   private
    type
      TIteratorfunction = procedure (const x:T) is nested;
      // Fatal: Syntax error, ";" expected but "is" found
   var private
  end;
  TSpec=specialize GenericA<integer>;

begin
end. 
