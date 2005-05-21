{ %fail }

Type
  GLEnum = longint;
 tShader=Record
        TexturesUsed:Longint;
        Primitive:GLenum;
        Blended:Boolean;
        BlendSrc:GLenum;
        BlendDst:GLenum;
        ShaderName:String;
     End;

Const EmptyShader:tShader=(
         TexturesUsed:-1;
         Primitive:0;
         Blended:False;
         BlendSrc:0;
         BlendDst:0;
         Tag:'');

begin
end.
