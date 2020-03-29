(* ::Package:: *)

(* ::Subsection:: *)
(*Journals*)


CreateJournalEntry[] := CreateJournalEntry[{1, 1, 1}, "", 0., 0., "", "", ""]
CreateJournalEntry[date : {_Integer, _Integer, _Integer} | _String,
  description_String, amount_?NumberQ, balance_?NumberQ, account_String,
  currency_String, category_String : "", extra : (_ -> _)...] := <|
 "date" -> toDateString@date, "description" -> StringTrim@description,
 "amount" -> amount, "balance" -> balance, "account" -> account, 
 "currency" -> currency, "category" -> category, extra
 |>

toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]


With[{journalKeys = Sort@Keys@CreateJournalEntry[]},
 IsJournalEntry[entry_Association] := Sort@Keys@entry === journalKeys;
 IsJournalEntry[___] := False;
]
