BeginPackage["MLedger`"];
(* ::Section:: *)
(*Declarations*)
(* ::Package:: *)

GetBankAccounts::usage = "GetBankAccounts[] lists current bank accounts.";
SetBankAccounts::usage = "SetBankAccounts[accounts] sets current bank accounts.\
Should not typically be used directly.";
	
AddBankAccount::usage = "AddBankAccount[name, currency, filePattern, importFunction] \
adds a new account to the list under name with given currency. filePattern determines \
files to be recognized as belonging to this account and importFunction is used to \
parse the files.";
	
BankAccountNameQ::usage = "BankAccountNameQ[str] checks whether str corresponds to \
the name of a bank account.";
ListBankAccounts::usage = "ListBankAccounts[] lists the names of bank accounts active.";
(* ::Section:: *)
(*Implementations*)
Begin["`Private`"];
(* ::Package:: *)

Module[{bankAccounts = {}},
 SetBankAccounts[accounts_] := bankAccounts = accounts;
 GetBankAccounts[] := bankAccounts;
 
 AddBankAccount[name_String, currency_ /; MemberQ[{"SEK", "USD"}, currency], 
  filePattern_, importFunction_] := 
   AppendTo[bankAccounts, 
    <|"name" -> name, "currency" -> currency, 
      "filePattern" -> filePattern, "importFunction" -> importFunction |>
   ];
   
]

BankAccountNameQ[account_] := StringQ@account && MemberQ[ListBankAccounts[], account]

ListBankAccounts[] := GetBankAccounts[][[All, "name"]]
(* ::Subsection::Closed:: *)
(*Tail*)
End[];
(* ::Section::Closed:: *)
(*Tail*)
EndPackage[]
