{ %FAIL }

{
 This is a small example program.
 The Function "getComment" is declared in the following way:
 in the class          :   function getComment : AnsiString;
 in the implementation :   function Parser.getComment : char;
}

{$mode delphi}
type
   Parser=class(TObject)
   public
      function getComment : AnsiString;
      function setComment(_text:AnsiString);
   private
      Comment: AnsiString;
   end;

   function Parser.setComment(_text:AnsiString);
   begin
      Comment := _text;
   end;

   function Parser.getComment : char;
   begin
      getComment := Comment;
   end;

{----- main program---------------------------------}

Var p:Parser;

var SourceBuffer : AnsiString;

begin
   sourceBuffer := 'Just some text.';
   WriteLn('The source buffer is:',sourceBuffer);
   p:=Parser.create;
   p.setComment(sourceBuffer);
   WriteLn(p.getComment);
   p.free;
end.
