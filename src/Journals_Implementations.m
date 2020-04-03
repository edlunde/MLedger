(* ::Package:: *)

(* ::Subsection:: *)
(*Journal objects*)


(* ::Subsubsection:: *)
(*Journal*)


IsJournal[dataset_Dataset] := And@@(IsJournalEntry /@ dataset)
IsJournal[___] := False

CreateJournal[entries : {___?IsJournalEntry}] := 
 addIDs@Dataset[CreateJournalEntry /@ entries] 
CreateJournal[] := CreateJournal[{}]

addIDs::duplicate = "Warning! Duplicate entries in { \n`1` \n... }";
addIDs[journal_?IsJournal] := (
 If[Not@DuplicateFreeQ[Normal@journal], 
  Message[addIDs::duplicate, ToString[Normal@journal[[ ;; 2]]]]];
  addID /@ journal)
 
addID[entry_?IsJournalEntry] := 
 If[KeyExistsQ[entry, "id"], entry,
  Append[entry, "id" -> 
   Hash[If[KeyExistsQ[#], entry[#]] & /@ 
    {"date", "description", "amount", "account", "FITID"},
    "MD5"]]]


(* ::Subsubsection::Closed:: *)
(*JournalEntry*)


CreateJournalEntry[] = CreateJournalEntry[{1, 1, 1}, "", 0., 0., "", "", ""];
CreateJournalEntry[{}] = CreateJournalEntry[];
CreateJournalEntry[date : {_Integer, _Integer, _Integer} | _String,
  description_String, amount_?NumberQ, balance_?NumberQ, account_String,
  currency_String, category_String : "", extra : (_ -> _)...] := <|
 "date" -> toDateString@date, "description" -> StringTrim@description,
 "amount" -> amount, "balance" -> balance, "account" -> account, 
 "currency" -> currency, "category" -> category, extra
 |>
CreateJournalEntry[journalEntry_?IsJournalEntry] := journalEntry

toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]


(* Fixes journalKeys at time of reading in package, so any fancy redefinitions
    afterwards of what a JournalEntry should look like needs to redefine IsJournalEntry
    too. *)
With[{journalKeys = Sort@Keys@CreateJournalEntry[]},
 IsJournalEntry[entry_Association] := Complement[journalKeys, Keys@entry] === {};
 IsJournalEntry[___] := False;
]


(* ::Subsubsection::Closed:: *)
(*SetCategories*)


SetCategories::length = "Journal `1` and categories `2` not of equal length.";
SetCategories[journalIn_?IsJournal, categories_List] /; If[
  Length@journalIn === Length@categories, True,
  Message[SetCategories::length, Short@journalIn, Short@categories]; False
 ] :=
 Module[{journal = Normal@journalIn}, 
  journal[[All, "category"]] = categories;
  Dataset@journal]


(* ::Subsection:: *)
(*Journal file handling*)


ReadJournal[filename_String] /; If[
 FileExistsQ@filename, True,
 Message[Import::nffil]; False
 ] := CreateJournal@importCSV[filename]
 
importCSV[filename_String] :=
 With[{imported = Import[filename]},
  AssociationThread[
   First@imported (* First row is header*) -> #] & /@ Rest@imported
 ]


WriteToJournal[filename_String, journal_?IsJournal] :=
 Export[filename, journal]
