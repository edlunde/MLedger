(* ::Package:: *)

(* ::Subsection:: *)
(*Misc.*)


(* Only tested implicitly *)
SetAttributes[messageIfNot, HoldAll]
messageIfNot[condition_, message_, messageArgs___] :=
 If[condition, True, Message[message, messageArgs]; False]


(* ::Subsection:: *)
(*Dates*)


toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]


sortByDateDescending[list_] /; And @@ (KeyExistsQ[#, "date"] & /@ list):=
 list[[Ordering[getListOfDates@list, All, OrderedQ[{#2, #1}] &]]]
 
getListOfDates[list_] := 
 Normal[DateList /@ list[[All, "date"]]]


splitByYear[table_Dataset] := Dataset /@ splitByYear[Normal@table]
splitByYear[table_] :=
 KeySort@GroupBy[table, DateList[#[["date"]]][[1]] &]
 
splitByMonthAndYear[table_Dataset] := Dataset /@ splitByMonthAndYear[Normal@table]
splitByMonthAndYear[table_] :=
 KeySort@GroupBy[table, DateList[#[["date"]]][[;;2]] &]


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
