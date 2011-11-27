%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "expressive-marks"

%% Translation of GIT committish: 8b93de6ce951b7b14bc7818f31019524295b990f
  texidoces = "
Las abreviaturas se encuentran definidas dentro del archivo
@file{ly/script-init.ly}, donde las variables @code{dashHat},
@code{dashPlus}, @code{dashDash}, @code{dashBar},
@code{dashLarger}, @code{dashDot} y @code{dashUnderscore} reciben
valores predeterminados.  Se pueden modificar estos valores
predeterminados para las abreviaturas. Por ejemplo, para asociar
la abreviatura @w{@code{-+}} (@code{dashPlus}) con el símbolo del
semitrino en lugar del símbolo predeterminado +, asigne el valor
@code{trill} a la variable @code{dashPlus}:

"
  doctitlees = "Modificar los valores predeterminados para la notación abreviada de las articulaciones"


%% Translation of GIT committish: 0a868be38a775ecb1ef935b079000cebbc64de40
 texidocde = "
Die Abkürzungen sind in der Datei @file{ly/script-init.ly} definiert, wo
den Variablen @code{dashHat}, @code{dashPlus}, @code{dashDash},
@code{dashBar}, @code{dashLarger}, @code{dashDot} und
@code{dashUnderscore} Standardwerte zugewiesen werden.  Diese Standardwerte
können verändert werden.  Um zum Beispiel die Abkürzung
@code{-+} (@code{dashPlus}) mit dem Triller anstatt mit dem +-Symbol zu
assoziieren, muss der Wert @code{trill} der Variable
@code{dashPlus} zugewiesen werden:

"
  doctitlede = "Die Standardwerte der Abkürzungen von Artikulationen verändern"

%% Translation of GIT committish: a6b5eea83ddcdd8e1fd0760db385176ff9b2b1cd
  texidocfr = "
Les raccourcis sont répertoriés dans le fichier
@file{ly/script-init.ly}, dans lequel on retrouve les variables
@code{dashHat}, @code{dashPlus}, @code{dashDash}, @code{dashBar},
@code{dashLarger}, @code{dashDot}, et @code{dashUnderscore} ainsi que
leur valeur par défaut.  Ces valeurs peuvent être modifiées selon vos
besoins.  Il suffit par exemple, pour affecter au raccourci @w{@code{-+}}
(@code{dashPlus}) le symbole du trille en lieu et place du @code{+}
(caractère plus), d'assigner la valeur @code{trill} à la variable
@code{dashPlus} :

"
  doctitlefr = "Modification de la signification des raccourcis pour les signes d'articulation"


  texidoc = "
The shorthands are defined in @samp{ly/script-init.ly}, where the
variables @code{dashHat}, @code{dashPlus}, @code{dashDash},
@code{dashBar}, @code{dashLarger}, @code{dashDot}, and
@code{dashUnderscore} are assigned default values.  The default values
for the shorthands can be modified. For example, to associate the
@code{-+} (@code{dashPlus}) shorthand with the trill symbol instead of
the default + symbol, assign the value @code{trill} to the variable
@code{dashPlus}:

"
  doctitle = "Modifying default values for articulation shorthand notation"
} % begin verbatim

\relative c'' { c1-+ }

dashPlus = "trill"

\relative c'' { c1-+ }

