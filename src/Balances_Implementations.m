(* ::Package:: *)

(* ::Subsection:: *)
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


IsBalances[obj_Association] := HasKeysQ[obj, {"date", "accountBalances"}] && 
 IsAccountBalances@obj["accountBalances"]
IsBalances[___] := False

With[{keys = {"account", "balance", "currency"}},
 IsAccountBalances[lst : {__Association}] := And@@(HasKeysQ[#, keys] & /@ lst)
]
IsAccountBalances[___] := False


(* ::Subsection:: *)
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
