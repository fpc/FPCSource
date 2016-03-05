The fcl-pdf package contains a PDF generating unit fppdf that does not depend
on any external libraries.

The PDF generator has the following features:
- Support for basic shapes.
- Support for basic line styles.
- Dictionary support.
- Multi-page PDF.
- Image support.
- TTF Font support.
- Font embedding.
- Unicode font support.
- Stream Compression.
- Image embedding.
- Several paper types.
- Portrait/Landscape.
- Support for multiple units.
- Rotation matrix system.
- PDF creator information.
- Output validates by several PDF validators.

Todo:
- Implement TFPCustomCanvas descendent (TPDFCanvas) that draws on a PDF.
- Partial embedding of (unicode) fonts for smaller PDFs.
- On windows, allow to use native font mechanisms for extracting info from TTF files.

Optionally:
- PDF Forms.
- Archive format.
- Signature.
- File attachments.