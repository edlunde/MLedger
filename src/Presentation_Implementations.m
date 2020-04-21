(* ::Package:: *)

(* ::Subsection:: *)
(*Presentation common*)


FormattedGrid[table_] /; Length@table > 0 && And@@(Length@# > 0 & /@ table) :=
 Grid[round@moveRowAndColumnNamesIntoTable@table,
  Dividers -> {False, {-2 -> True}},
  Alignment -> {{Right, 1 -> Left}}
 ]
moveRowAndColumnNamesIntoTable[tableIn_] :=
 Module[{table = tableIn},
  (* Remove column keys*)
  table = If[AssociationQ@#, Values@#, Flatten@{#}] & /@ table;
  (* Move row keys into rows *)
  If[AssociationQ@table, table = KeyValueMap[Prepend[#2, #1] &, table]];
  (* Add column keys as header list *)
  If[AssociationQ@tableIn[[1]], PrependTo[table, Keys@tableIn[[1]]]];
  (* If there where row and column keys, header is one item short, add empty string *)
  If[Length@table[[1]] < Length@table[[2]], table[[1]] = Prepend[table[[1]], ""]];
  
  table
 ]

SetAttributes[round, Listable]; round[s_String] := s; round[x_] := Round[x, 1];
