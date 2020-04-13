(* ::Package:: *)

(* ::Subsection:: *)
(*Dates*)


toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]


(* ::Subsection:: *)
(*Data structure functions*)


HasKeysQ[assoc_ /; ListQ@assoc || AssociationQ@assoc , keys_] := 
 And @@ (KeyExistsQ[assoc, #] & /@ keys)


(* ::Subsection:: *)
(*File/Directory handling*)


EnsureDirectoryExists[dir_String] := 
 If[Not@FileExistsQ@dir, CreateDirectory[dir]]


importCSV[filename_String] :=
 With[{imported = Import[filename, "CSV"]},
  AssociationThread[
   First@imported (* First row is header *) -> #] & /@ Rest@imported
 ]
