unit wasmdef;

{$i fpcdefs.inc}

interface

uses
  symtype, symdef, symconst, constexp
  ,defutil;

    { returns whether a def always resides in memory,
      rather than in wasm local variables...) }
    function wasmAlwayInMem(def: tdef): boolean;

    function get_para_push_size(def: tdef): tdef;

implementation

  function get_para_push_size(def: tdef): tdef;
    begin
      result:=def;
      if def.typ=orddef then
        case torddef(def).ordtype of
          u8bit,uchar:
            if torddef(def).high>127 then
              result:=s8inttype;
          u16bit:
            begin
              if torddef(def).high>32767 then
                result:=s16inttype;
            end
          else
            ;
        end;
    end;

  function wasmAlwayInMem(def: tdef): boolean;
    begin
      case def.typ of
        arraydef,
        filedef,
        recorddef,
        objectdef,
        stringdef:
          result:=true;
        else
          result:=false;
      end;
    end;

end.
