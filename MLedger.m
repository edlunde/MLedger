BeginPackage["MLedger`"];
(* ::Chapter:: *)
(*Declarations*)
(* ::Section:: *)
(*BankAccounts*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Bank account objects*)


GetBankAccounts::usage = "GetBankAccounts[] lists current bank accounts.";
SetBankAccounts::usage = "SetBankAccounts[accounts] sets current bank accounts.\
Should not typically be used directly.";
	
AddBankAccount::usage = "AddBankAccount[name, currency, filePattern, importFunction] \
adds a new account to the list under name with given currency. filePattern determines \
files to be recognized as belonging to this account and importFunction is used to \
parse the files.";

ListBankAccounts::usage = "ListBankAccounts[] lists the names of bank accounts active.";
BankAccountNameQ::usage = "BankAccountNameQ[str] checks whether str corresponds to \
the name of a bank account.";


(* ::Subsection::Closed:: *)
(*Importing bank statements*)


ListImportableFiles::usage = "ListImportableFiles[directory] returns a list of files in \
directory that match one or more filePattern among the active accounts.";

SelectAccountsForm::usage = "form = SelectAccountsForm[files] creates a UI element \
for choosing for each file in files an account that can import it.";
ExtractSelectedAccounts::usage = "ExtractSelectedAccounts[form] extracts the chosen \
accounts from a SelectAccountsForm.";

ImportAccountFiles::usage = "ImportAccountFiles[files, accountNames] imports data from \
each files[[n]] assuming it belongs to the account with name accountNames[[n]]. ";


(* ::Subsection::Closed:: *)
(*Bank specific functions*)


(* ::Subsubsection::Closed:: *)
(*Bank of America*)


AddBoAAccount::"usage" = "AddBoAAccount[accountName] creates a new \
Bank of America account named accountName."


(* ::Subsubsection::Closed:: *)
(*Nordea*)


AddNordeaAccount::"usage" = "AddNewNordeaAccount[accountName] creates a new \
Nordea account named accountName."
(* ::Section:: *)
(*Journals*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Journal*)


IsJournalEntry::usage = "IsJournalEntry[obj_] returns True if obj is recognized as a JournalEntry, False otherwise."
CreateJournalEntry::usage = "CreateJournalEntry[date, description, amount,\
 balance, account, currency, category] creates a journal entry from data.\n
CreateJournalEntry[..., extra] appends extra information. Has to be given as\
 key/value-pairs.";
(* ::Chapter:: *)
(*Implementations*)
Begin["`Private`"];
(* ::Section:: *)
(*BankAccounts*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Bank account objects*)


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

ListBankAccounts[] := GetBankAccounts[][[All, "name"]]
BankAccountNameQ[account_] := StringQ@account && MemberQ[ListBankAccounts[], account]


(* ::Subsection::Closed:: *)
(*Importing bank statements*)


ListImportableFiles[directory_] := FileNames[file__ /; importableFileQ[file], directory]

importableFileQ[fileName_] := 
 StringQ[fileName] && Length@getMatchingAccounts[fileName] > 0
getMatchingAccounts[fileName_String] := 
 Select[GetBankAccounts[], StringMatchQ[FileNameTake[fileName], #["filePattern"]] &]


SelectAccountsForm[files : {__?importableFileQ}] := 
 {FileNameTake@#, 
  PopupMenu[Dynamic[Evaluate@Unique@"account"], 
            getMatchingAccounts[#][[All, "name"]], 
            FieldSize -> 15]
   } & /@ files // Grid
   
(* untested, depends on the form being evaluated in a notebook? *)
ExtractSelectedAccounts[accountsForm_Grid] :=
 Cases[accountsForm, PopupMenu[Dynamic[account_], __] :> account, All]


ImportAccountFiles[fileNames : {__?importableFileQ}, 
  accountNames : {__?BankAccountNameQ}] /; Length@fileNames == Length@accountNames := 
 MapThread[importFile, {fileNames, accountNames}]

ImportAccountFiles::mssAccount = "Missing account `1` for file `2`";
importFile[fileName_?importableFileQ, accountName_String] := 
 With[{importFunction = (Query[Select[#name == accountName &], "importFunction"] @ 
                           getMatchingAccounts[fileName])[[1]]},
 (*createJournal@*)importFunction[fileName, accountName]
]
importFile[fileName_String, accountName_String] /; Not@importableFileQ@fileName := 
 Message[ImportAccountFiles::mssAccount, accountName, fileName]


(* ::Subsection::Closed:: *)
(*Bank specific functions*)


(* ::Subsubsection::Closed:: *)
(*Bank of America*)


AddBoAAccount[accountName_String] := 
 AddBankAccount[accountName, "USD", BoAFilePattern, importBoA]


BoAFilePattern = "stmt" ~~ ___ ~~ ".txt";


importBoA[filename_String, account_String] := 
 handleBoALine[account] /@ extractTableBoA@Import[filename]
 
extractTableBoA[str_String] :=
 StringTrim /@ DeleteCases[StringSplit[#, "  "], ""] & /@ StringSplit[str, "\n"]
 
handleBoALine[account_String] := handleBoALine[#, account] &
handleBoALine[list_List /; Length@list < 4, account_String] := Sequence[]
handleBoALine[
  {dateString_String, description_String, 
   amount_?numberStringQ, balance_?numberStringQ}, 
  account_String
  ] := CreateJournalEntry[
    toDateString@DateList[{dateString, {"Month", "Day", "Year"}}], 
    description, ToExpression@amount, ToExpression@balance, account, "USD"
    ]
numberStringQ[str_String] := StringMatchQ[str, NumberString]
numberStringQ[obj___] := False


(* ::Subsubsection::Closed:: *)
(*Nordea*)


AddNordeaAccount[accountName_String] := 
 AddBankAccount[accountName, "SEK", nordeaFilePattern, importNordea]


nordeaFilePattern = "export" ~~ ___ ~~ ".csv";


importNordea[filename_String, account_String] := 
 handleNordeaLine[account] /@ Import[filename]
 
handleNordeaLine[account_String] := handleNordeaLine[#, account] &
handleNordeaLine[
  {"Datum", "Transaktion", "Kategori", "Belopp", "Saldo"}, account_String
  ] := Sequence[]
handleNordeaLine[
  {dateString_String, description_String, type_String, amount_?NumericQ, 
   balance_?NumericQ}, account_String
  ] := CreateJournalEntry[dateString, description, amount, balance, account, "SEK"]
(* ::Section:: *)
(*Journals*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
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
(* ::Section::Closed:: *)
(*Tail*)
End[];
(* ::Chapter::Closed:: *)
(*Tail*)
EndPackage[]
