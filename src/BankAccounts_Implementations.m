(* ::Package:: *)

Module[{bankAccounts = {}},
 setBankAccounts[accounts_] := bankAccounts = accounts;
 getBankAccounts[] := bankAccounts;
 
 addBankAccount[name_String, currency_ /; MemberQ[{"SEK", "USD"}, currency], 
  filePattern_, importFunction_] := 
   AppendTo[bankAccounts, 
    <|"name" -> name, "currency" -> currency, 
      "filePattern" -> filePattern, "importFunction" -> importFunction |>
   ]
]
