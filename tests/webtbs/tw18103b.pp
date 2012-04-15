{ %NORUN }

program gentest;

// alias test generic class
// example is a subset of a working delphi class
// the nested types are there so that they can be used to defined types (e.g.  var p : bwimagespecialization.reft;)
//

{$mode objfpc}


{.define fpchere}

Type
  generic tbwimagegen<T> = Class(TObject)
                 Type
                    TLocalType = tbwimagegen;
                    BaseUnit = T;
                    RefT= ^BaseUnit;
                  procedure copylines(source:  tbwimagegen;xfrom,xto:integer;xdestline:integer=0);
                  function GetImagePointer( OriginX, OriginY : Integer ): reft;inline;
                  function GetLinePointer(originy:integer):reft;
                  property scanline[i:integer]:reft read getlinepointer;
                  end;


procedure tbwimagegen.copylines(source: tbwimagegen;xfrom,xto:integer;xdestline:integer=0);
begin
end;
function tbwimagegen.GetImagePointer( OriginX, OriginY : Integer ): reft;inline;
begin
end;
function tbwimagegen.GetLinePointer(originy:integer):reft;
begin
end;

begin
end.
