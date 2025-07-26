{ %FAIL }

program tmultilinestring8;

{ Ryan's example from the mailing list }

{$modeswitch MultiLineStrings}

const lines: ansistring = `
  #version 150

  uniform sampler2D textures[8];
  in vec2 vertexTexCoord;
  in vec4 vertexColor;
  in float vertexUVMap;
  out vec4 fragColor;

  void main()
  {
    if (vertexUVMap == 255) {
      fragColor = vertexColor;
    } else {
      fragColor = texture(textures[int(vertexUVMap)], vertexTexCoord.st);
      if (vertexColor.a < fragColor.a) {
        fragColor.a = vertexColor.a;
      }
    }

    // TODO: testing
    fragColor = vec4(1,0,0,1);
  }
`;

var
  s: ansistring = lines;
begin
  writeln(b);
end.
