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


ListImportableFiles::usage = "ListImportableFiles[directory] returns a list of files in \
directory that match one or more filePattern among the active accounts.";


SelectAccountsForm
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


ListImportableFiles[directory_] := FileNames[file__ /; isImportableFile[file], directory]


isImportableFile[fileName_] := 
 StringQ[fileName] && Length@getMatchingAccounts[fileName] > 0
getMatchingAccounts[fileName_String] := 
 Select[GetBankAccounts[], StringMatchQ[FileNameTake[fileName], #["filePattern"]] &]
(* ::Subsection::Closed:: *)
(*Tail*)
End[];
(* ::Section::Closed:: *)
(*Tail*)
EndPackage[]
