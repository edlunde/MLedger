(* ::Package:: *)

(* ::Subsection:: *)
(*Categorization form*)


CategorizationForm[journal_?IsJournal] := 
  addCategorizationFormHeader[journal[1, "account"],
   Grid[categorizationFormRow /@ Normal@journal]
  ]
addCategorizationFormHeader[account_String, form_] :=
 Labeled[form, Style[account, "Subsubsection"], {Top}]


(* Note: not tested for Dynamic category-variable functionality 
   (most important part of course, but requires some arcane workaround to get right
   behavior when run without notebook?) *)
categorizationFormRow[entry_?IsJournalEntry] :=
 With[{category = Unique@"category", fieldSize = 11},
  Join[
   Values[entry[[{"date", "description", "amount"}]]], 
   {
    PopupMenu[Dynamic[category], getCategories[entry], FieldSize -> fieldSize],
    InputField[Dynamic[category], String, FieldSize -> fieldSize]
   }
  ]
 ]
 
(* Placeholder *)
getCategories[entry_?IsJournalEntry] := {entry[["category"]]}


ExtractSelectedCategories[categorizationForm_] :=
 Cases[categorizationForm, InputField[_[category_], __] :> category, All]


(* ::Subsection:: *)
(*Categorization prediction*)


TrainCategoryClassifier[journal_?IsJournal] :=
 Classify@formatTrainingData@TakeCategorized@journal


formatTrainingData[journal_?IsJournal] := 
 (* #1 becomes "category" due to Prepend *)
 {##2} -> #1 & @@@ Normal@journal[All, Prepend[featureKeys[], "category"]]
 
featureKeys[] := {"description", "amount", "account"}
