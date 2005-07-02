{$Mode Delphi}
{$R-,X+,V-}

unit uw3969;

interface

   type testobj = object
      constructor init;
      destructor done;
      procedure setvar(value: string);
      function getvar: string;

      private

      myvar: string;
   end;

implementation

   constructor testobj.init;
   begin
      myvar := 'init';
   end;

   destructor testobj.done;
   begin
      myvar := 'done';
   end;

   procedure testobj.setvar;
   begin
      myvar := value;
   end;

   function testobj.getvar : string;
   begin
      result := myvar;
   end;

end.
