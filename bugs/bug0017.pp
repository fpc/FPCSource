  procedure init;

    var
       endofparas : boolean;

    procedure getparastring;

      procedure nextopt;

        begin
           getparastring;
           init;
           endofparas:=false;
        end;

      begin
         nextopt;
      end;
      
    begin
       getparastring;
    end;      
     
begin
   init;
end.

