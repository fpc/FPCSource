unit wasmdef;

{$i fpcdefs.inc}

interface

uses
  symtype, symdef, symconst, constexp
  ,defutil;

    { returns whether a def is emulated using an implicit pointer type on the
      WebAssembly target (e.g., records, regular arrays, ...) }
    function wasmimplicitpointertype(def: tdef): boolean;

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

  function wasmimplicitpointertype(def: tdef): boolean;
    begin
      case def.typ of
        arraydef:
          result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
              is_open_array(def) or
              is_array_of_const(def) or
              is_array_constructor(def);
        filedef,
        recorddef,
        setdef:
          result:=true;
        objectdef:
          result:=is_object(def);
        stringdef :
          result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
        procvardef:
          result:=not tprocvardef(def).is_addressonly;
        else
          result:=false;
      end;
    end;

end.
