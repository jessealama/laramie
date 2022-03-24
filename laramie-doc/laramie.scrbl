#lang scribble/manual

@require[@for-label[racket/base
		    laramie]]

@title[#:style "toc"]{Laramie—HTML5 for Racket}
@author[(author+email "Jesse Alama" "jesse@serverracket.com")]

Laramie is an HTML5 parser for Racket. Give it an HTML
document in the form of a (byte) string or URL, get an xexpr
as a result.

Laramie is implemented in Typed Racket. But that's an
implementation detail that you don't need to concern
yourself with.
