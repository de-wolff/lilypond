%% Do not edit this file; it is auto-generated from input/new
%% This file is in the public domain.
\version "2.13.0"
\header {
  texidoces = "
Se puede modificar el texto empleado para los crescendos y
decrescendos modificando las propiedades de contexto
@code{crescendoText} y @code{decrescendoText}.  El estiloo de la
línea de extensión se puede cambiar modificando la propiedad
@code{'style} de @code{DynamicTextSpanner}.  El valor
predeterminado es @code{'hairpin} (regulador), y entre otros
valores posibles se encuentran @code{'line} (línea),
@code{'dashed-line} (línea intermitente) y @code{'dotted-line}
(línea de puntos):

"
  doctitlees = "Cambiar el texto y los estilos de objeto de extensión para las indicaciones dinámicas textuales"
  
%% Translation of GIT committish :<6ce7f350682dfa99af97929be1dec6b9f1cbc01a>
texidocde = "
Der Text, der für Crescendo und Decrescendo gestzt wird, kann geändert
werden, indem man die Eigenschaften @code{crescendoText} und
@code{decrescendoText} verändert.  Der Stil des Streckers kann auch
geändert werden, indem die @code{'style}-Eigenschaft des
@code{DynamicTextSpanner} beeinflusst wird.  Der Standardwert ist
@code{'hairpin}, ander Möglichkeiten sind @code{'line}, @code{'dashed-line}
und @code{'dotted-line}:
"
  doctitlede = "Text und Strecker-Stile für Dynamik-Texte ändern"

  lsrtags = "rhythms,tweaks-and-overrides"
  texidoc = "
The text used for crescendos and decrescendos can be changed by
modifying the context properties @code{crescendoText} and
@code{decrescendoText}.  The style of the spanner line can be
changed by modifying the @code{'style} property of
@code{DynamicTextSpanner}.  The default value is @code{'hairpin},
and other possible values include @code{'line}, @code{'dashed-line}
and @code{'dotted-line}:
"
  doctitle = "Changing text and spanner styles for text dynamics"
} % begin verbatim


\relative c'' {
  \set crescendoText = \markup { \italic { cresc. poco } }
  \set crescendoSpanner = #'text
  \override DynamicTextSpanner #'style = #'dotted-line
  a2\< a
  a2 a
  a2 a
  a2 a\mf
}
