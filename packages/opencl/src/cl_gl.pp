(**********************************************************************************
 * Copyright (c) 2008-2009 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **********************************************************************************)

// ported to FPC by Dmitry 'skalogryz' Boyarintsev: 28th apr 2009

unit cl_gl;

interface

uses
  cl, gl, ctypes;

 // NOTE:  Make sure that appropriate GL header file is included separately

type
  cl_gl_object_type   = cl_uint;
  cl_gl_texture_info  = cl_uint;
  cl_gl_platform_info = cl_uint;

const
// cl_gl_object_type
  CL_GL_OBJECT_BUFFER       = $2000;
  CL_GL_OBJECT_TEXTURE2D    = $2001;
  CL_GL_OBJECT_TEXTURE3D    = $2002;
  CL_GL_OBJECT_RENDERBUFFER = $2003;

  // cl_gl_texture_info
  CL_GL_TEXTURE_TARGET      = $2004;
  CL_GL_MIPMAP_LEVEL        = $2005;

  CL_GL_CONTEXT_KHR         = $2008;
  CL_EGL_DISPLAY_KHR        = $2009;
  CL_GLX_DISPLAY_KHR        = $200A;
  CL_WGL_HDC_KHR            = $200B;
  CL_CGL_SHAREGROUP_KHR     = $200C;


function clCreateFromGLBuffer(context: cl_context; falgs: cl_mem_flags;
  bufobj: GLuint; var errcode_ret: cl_int
  ): cl_mem; cdecl; external name 'clCreateFromGLBuffer';

function clCreateFromGLTexture2D(context: cl_context;
  flags: cl_mem_flags; target: GLenum; miplevel: GLint;
  texture: GLuint; var errcode_ret: cl_int
  ): cl_mem; cdecl; external name 'clCreateFromGLTexture2D';


function clCreateFromGLTexture3D(context: cl_context; flags: cl_mem_flags;
  target: GLenum; miplevel: GLint; texture: GLuint; var errorcode: cl_int
  ): cl_mem; cdecl; external name 'clCreateFromGLTexture3D';

function clCreateFromGLRenderbuffer(context: cl_context;
  flags: cl_mem_flags; renderbuffer: GLuint; var errcode: cl_int
  ): cl_mem; cdecl; external name 'clCreateFromGLRenderbuffer';

function clGetGLObjectInfo(memobj: cl_mem; gl_object_type: cl_gl_object_type;
  object_name: GLuint
  ): cl_int; cdecl; external name 'clGetGLObjectInfo';

function clGetGLTextureInfo(memobj: cl_mem; param_name: cl_gl_texture_info;
  value_size: csize_t; value: Pointer; var size_ret: pcsize_t
  ): cl_int; cdecl; external name 'clGetGLTextureInfo';

function clEnqueueAcquireGLObjects(command_queue: cl_command_queue;
  num_objects: cl_uint; mem_objects: Pcl_mem;
  num_events: cl_uint; events_list : Pcl_event; event: Pcl_event
  ): cl_int; cdecl; external name 'clEnqueueAcquireGLObjects';

function clEnqueueReleaseGLObjects(command_queue: cl_command_queue;
  num_objects: cl_uint; mem_objects: Pcl_mem;
  num_events: cl_uint; events_list : Pcl_event; event: Pcl_event
  ): cl_int; cdecl; external name 'clEnqueueReleaseGLObjects';

implementation

end.
