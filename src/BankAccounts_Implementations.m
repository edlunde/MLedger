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


(* ::Subsection:: *)
(*Bank specific functions*)


(* ::Subsubsection::Closed:: *)
(*Bank of America*)


AddBoAAccount[accountName_String] := 
 AddBankAccount[accountName, "USD", BoAFilePattern, importBoA]


BoAFilePattern = "stmt" ~~ ___ ~~ ".txt";


importBoA[filename_String, account_String] := 
 CreateJournal[handleBoALine[account] /@ extractTableBoA@Import[filename]]
 
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


(* ::Subsubsection:: *)
(*Nordea*)


AddNordeaAccount[accountName_String] := 
 AddBankAccount[accountName, "SEK", nordeaFilePattern, importNordea]


nordeaFilePattern = "export" ~~ ___ ~~ ".csv";


importNordea[filename_String, account_String] := 
 CreateJournal[handleNordeaLine[account] /@ Import[filename]]
 
handleNordeaLine[account_String] := handleNordeaLine[#, account] &
handleNordeaLine[
  {"Datum", "Transaktion", "Kategori", "Belopp", "Saldo"}, account_String
  ] := Sequence[]
handleNordeaLine[
  {dateString_String, description_String, type_String, amount_?NumericQ, 
   balance_?NumericQ}, account_String
  ] := CreateJournalEntry[dateString, description, amount, balance, account, "SEK"]
