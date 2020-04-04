(* ::Package:: *)

(* ::Subsection:: *)
(*Directory handling*)


EnsureDirectoryExists[dir_String] := 
 If[Not@FileExistsQ@dir, CreateDirectory[dir]]
