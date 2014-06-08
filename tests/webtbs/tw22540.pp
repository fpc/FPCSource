unit tw22540;

interface
 
{$ifdef FPC}
 {$Mode Delphi}
{$endif}
{$pointermath on} // Be more objfpc like in overindexing and removing of ^.
{$inline on}

type
     {$ifndef FPC} Ptrint=integer; {$endif}
     TLightMap<tkey,tvalue> = class
                       Type
                         PKey  = ^TKey;
                         PValue= ^TValue;
                         TKeyCompareFunc  = function(const Key1, Key2: TKey): PtrInt; // returns ptrint in FPC, don't know about XE2
                         TFinalizeKeyFunc = Procedure(var Key1:TKey) of object;
                       private
                         keycomparefunc:TKeyCompareFunc;
                         finalizekeyfunc:TFinalizeKeyFunc;      // finalize keys. NIL if unused.
                       public
                         constructor Create; virtual;
                       end;

     TLightStringMap<tvalue> = class(TLightMap<string,tvalue>)
                                 constructor create; override;
                                 procedure finalizestring(var s:string);
                                 end;

     TLightStringMapInteger  = class(TLightStringMap<Integer>);

implementation

Uses Sysutils;

{ TLightMap<tkey, tvalue> }

constructor TLightMap<tkey, tvalue>.Create;
begin
end;
  
{ TLightStringMap<tvalue> }

constructor TLightStringMap<tvalue>.create;
begin
  inherited;
  keycomparefunc:=comparestr;
  finalizekeyfunc:=finalizestring;
end;

procedure TLightStringMap<tvalue>.finalizestring(var s: string);
begin
  finalize (s);
end;

 
end.
