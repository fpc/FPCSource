
// These types are missing or wrong in MacGL
type
	GLenumPtr = ^GLenum;
	GLbooleanPtr = ^GLboolean;	
	GLbitfieldPtr = ^GLbitfield;	
	GLbytePtr = ^GLbyte;	
	GLshortPtr = ^GLshort;	
	GLintPtr = ^GLint;	
	GLsizeiPtr = ^GLsizei;	
	GLubytePtr = ^GLubyte;	
	GLushortPtr = ^GLushort;	
	GLuintPtr = ^GLuint;	
	//GLfloatPtr = ^GLfloat;	-- declared in UndefinedTypes.inc
	GLclampfPtr = ^GLclampf;	
	GLdoublePtr = ^GLdouble;	
	GLclampdPtr = ^GLclampd;	
	
// NSOpenGL.h
type
	_CGLPixelFormatObjectPtr = pointer;
	_CGLContextObjectPtr = pointer;	
	
type
	NSVisualEffectViewInternalPtr = pointer;
