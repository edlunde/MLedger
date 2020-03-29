(* ::Package:: *)

(* ::Subsection:: *)
(*Journals*)


CreateJournalEntry[] := <|
 "date"->"1-01-01", "description"->"", "amount"->0.`, "balance"->0.`,
 "account"->"", "currency"->"", "category"->""
 |>
CreateJournalEntry[date : {_Integer, _Integer, _Integer} | _String,
  description_String, amount_?NumberQ, balance_?NumberQ, account_String,
  currency_String, category_String : "", extra : (_ -> _)...] := <|
 "date" -> toDateString@date, "description" -> StringTrim@description,
 "amount" -> amount, "balance" -> balance, "account" -> account, 
 "currency" -> currency, "category" -> category, extra
 |>

toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]
