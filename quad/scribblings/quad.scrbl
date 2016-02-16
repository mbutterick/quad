#lang scribble/manual

@title[#:style 'toc]{Quad: typesetter & document processor}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodulelang[quad]

@italic{This documentation explains software that is under development. It is therefore littered with optimistic claims and wishful thinking.}

Quad is a typesetter & document-processor. Quad is implemented as a programming language. A Quad program resembles text annontated with high-level layout-description commands (not unlike XML). Quad programs can be written directly, or generated as the output of other programs.

Quad compiles this program into a format-independent page layout. This layout can be used directly, or passed to a format-specific renderer (e.g., SVG, HTML, PDF). 
