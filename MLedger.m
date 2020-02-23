BeginPackage["MLedger`"];
(* ::Section:: *)
(*Declarations*)
(* ::Package:: *)

getBankAccounts::usage = "getBankAccounts[] lists current bank accounts.";
setBankAccounts::usage = "setBankAccounts[accounts] sets current bank accounts.\
	Should not typically be used directly.";
	
addBankAccount::usage = "addBankAccount[name, currency, filePattern, importFunction] \
	adds a new account to the list under name with given currency. filePattern determines \
	files to be recognized as belonging to this account and importFunction is used to \
	parse the files."
(* ::Section:: *)
(*Implementations*)
Begin["`Private`"];
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
(* ::Subsection::Closed:: *)
(*Tail*)
End[];
(* ::Section::Closed:: *)
(*Tail*)
EndPackage[]
