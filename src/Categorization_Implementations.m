(* ::Package:: *)

(* ::Subsection:: *)
(*Categorization form*)


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
