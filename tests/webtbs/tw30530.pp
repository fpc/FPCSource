unit tw30530;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils, }Generics.Collections;

type

  //generic TMyList<T> = class(specialize TObjectList<T>);
  generic TMyList<T: class> = class(specialize TObjectList<T>);
  //generic TMyList<T: class> = class(specialize TObjectList<T>) end;

implementation

end.
