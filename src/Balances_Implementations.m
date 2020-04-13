(* ::Package:: *)




IsBalances[obj_Association] := KeyExistsQ[obj, "date"] && 
 KeyExistsQ[obj, "accountBalances"] && IsAccountBalances@obj["accountBalances"]
IsBalances[___] := False

With[{keys = {"account", "balance", "currency"}},
 IsAccountBalances[lst : {__Association}] := And@@(Complement[keys, Keys@#] == {} & /@ lst)
]
IsAccountBalances[___] := False
