(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Balances objects*)


CreateBalancesObject[date_String, accountBalances_?IsAccountBalances] :=
 <|"date" -> date, "accountBalances" -> accountBalances|>


CreateBalancesObject[date_String, assoc_?AssociationQ] /; 
  SubsetQ[ListBankAccounts[], Keys[assoc]] && And@@(NumericQ /@ Values@assoc) :=
 CreateBalancesObject[date,
  JoinAcross[
   Query[All, <| "account" -> "name", "currency" -> "currency"|>]@GetBankAccounts[],
   KeyValueMap[<|"account" -> #1, "balance" -> #2|> & , assoc],
   "account"]
  ]


CreateBalancesObject[date_, ledger_?IsLedger, incomingBalances_?IsBalances] := 
 addBalancesObjects[incomingBalances, balancesObjectFromLedger[ledger]]
 
addBalancesObjects[balances__?IsBalances] :=
 CreateBalancesObject[
  toDateString@Max[DateObject /@ {balances}[[All, 1]]],
  Merge[Query[All, #account -> #balance &][#[[2]]] & /@ {balances}, Total]]
  
balancesObjectFromLedger[emptyLedger_Dataset] /; Normal@emptyLedger === {} := {}
balancesObjectFromLedger[ledger_?IsLedger] :=
 CreateBalancesObject[ledger[1, "date"],
  Association@@Normal[
   Query[ListBankAccounts[], 1 /* fixMissing]@GetBalancesFromLedger[ledger]
   ]]


IsBalances[obj_Association] := HasKeysQ[obj, {"date", "accountBalances"}] && 
 IsAccountBalances@obj["accountBalances"]
IsBalances[___] := False

With[{keys = {"account", "balance", "currency"}},
 IsAccountBalances[lst : {__Association}] := And@@(HasKeysQ[#, keys] & /@ lst)
]
IsAccountBalances[___] := False


getAccountAssoc[balances_?IsBalances] :=
 Association[Query[All, #account -> #balance &][balances[["accountBalances"]]]]


(* ::Subsection::Closed:: *)
(*Balances input form*)


BalancesInputForm[date_String] /; checkDateHasBalances[date] := 
 labelBalancesInputForm[date, balancesForm[getPrecedingBalances@date]]

BalancesInputForm::noAccounts = "Error! No balances for `1` or earlier found.";
checkDateHasBalances[date_String] := 
 If[getPrecedingBalancesDate[date] === {}, 
  Message[BalancesInputForm::noAccounts, date]; False, 
  True]
  
labelBalancesInputForm[date_, form_] :=
 Labeled[form, Style[date, "Subsubsection"], {Top}, Spacings -> {0, 1.2}]
 
With[{header = {"Account", "Currency", "Balance"}},
 balancesForm[balances_?IsBalances] := 
  Grid[Prepend[#, header] & @
   (balanceFormRow /@ balances["accountBalances"])
  ]
]
(* Only partially tested as usual with Dynamic *)
balanceFormRow[entry_] :=
 With[{balance = Unique@"balance", fieldSize = 7},
  balance = entry["balance"];
  Append[Values[entry[[{"account", "currency"}]]],
   InputField[
    Dynamic[
     If[balance == Round@balance, balance, SetAccuracy[balance, 3]], 
      (balance = Round[#, 0.01])&], 
    Number, FieldSize -> fieldSize]
  ]
 ]


ExtractBalances[form_] :=
 CreateBalancesObject[
  extractDateFromBalancesForm@form,
  extractAccountBalancesFromBalancesForm@form
 ]
 
extractDateFromBalancesForm[balanceForm_] := 
 Cases[balanceForm, Labeled[___, Style[date_, __], ___] :> date, All][[1]]
extractAccountBalancesFromBalancesForm[balanceForm_] := 
 Cases[balanceForm, 
   {account_, currency_, InputField[_[_[_, balance_?NumericQ, ___], ___], __]} :> 
    <|"account" -> account, "balance" -> balance, "currency" -> currency|>, 
   All]


(* ::Subsection::Closed:: *)
(*Balances file handling*)


Module[{balancesDir = ""},
 SetBalancesDir[dir_String] := balancesDir = dir;
 GetBalancesDir[] := balancesDir
]


WriteToBalances[balances_?IsBalances] := 
 Export[formatBalancesFilename@balances["date"], Dataset@balances["accountBalances"]]
 
ReadBalances[date_String] /; 
 With[{filename = formatBalancesFilename@date},
  messageIfNot[FileExistsQ@filename, Import::nffil, filename]
 ] :=
  CreateBalancesObject[date, importCSV[formatBalancesFilename@date]]
 
formatBalancesFilename[date_String] := FileNameJoin[{GetBalancesDir[], date <> ".csv"}]


getPrecedingBalances[date_String] := 
 With[{precedingDate = getPrecedingBalancesDate@date},
  If[StringQ@precedingDate && FileExistsQ@formatBalancesFilename@precedingDate,
   ReadBalances@precedingDate,
   {}]
  ]
  
getPrecedingBalancesDate[date_String] := 
 {QuantityMagnitude@DateDifference[#, date], #} & /@ listBalancesDates[] //
   Select[#, #[[1]] >= 0 &] & // SortBy[#, #[[1]] &] & // If[#=={}, {}, #[[1, 2]]] &
listBalancesDates[] := 
 Flatten[extractDate /@ FileNames[__ ~~ ".csv", GetBalancesDir[]]]
extractDate[balancesFilename_String] := 
 StringCases[FileNameTake[balancesFilename], 
  year : NumberString ~~ "-" ~~ month : NumberString ~~ "-" ~~ 
  day : NumberString ~~ ".csv" :> year <> "-" <> month <> "-" <> day]
