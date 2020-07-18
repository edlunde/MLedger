(* ::Package:: *)

(* ::Text:: *)
(*Built and tested with WolframScript 1.2.0 for MacOSX-x86-64*)

BeginPackage["MLedger`"];
(* ::Chapter:: *)
(*Declarations*)
(* ::Section:: *)
(*Common*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Directory handling*)


EnsureDirectoryExists::usage = 
 "EnsureDirectoryExists[dir] creates dir if it does not exist.";


SetDataDirectories::usage = "SetDataDirectories[dir] set the various data directories \
(using SetJournalDir etc.) to default names with dir as root (so GetJournalDir[] will \
return dir/Journals/ etc. afterwards).";


(* ::Subsection::Closed:: *)
(*Data structure functions*)


HasKeysQ::usage = 
 "HasKeysQ[assoc, keys] returns True if every key in keys exists among the keys of\[NonBreakingSpace]\
assoc. Also works if assoc is a list of rules.";
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


IsJournal::usage = "IsJournal[obj_] returns True if obj is recognized \
as a Journal, False otherwise.";
CreateJournal::usage = "CreateJournal[listOfJournalEntries] creates a Journal.";


IsJournalEntry::usage = "IsJournalEntry[obj_] returns True if obj is recognized \
as a JournalEntry, False otherwise.";
CreateJournalEntry::usage = "CreateJournalEntry[date, description, amount, \
balance, account, currency, category] creates a journal entry from data.
CreateJournalEntry[..., extra] appends extra information. Has to be given as\
 key/value-pairs.";


SetCategories::usage = "SetCategories[journal, categories] takes a journal and a list \
of categories with equal length and sets the \"category\"-field of the journal's \
entries to the given categories.";


ResetIDs::usage = "ResetIDs[journal] recalculates the hash used to identify journal \
entries.";


AddCalculatedBalances::usage = "AddCalculatedBalances[journal, incomingBalance] \
calculates and adds a field calcBalance to the given journal assuming the balance \
before first entry was incomingBalance.git";


TakeCategorized::usage = "TakeCategorized[journal] returns the journal with only \
the entries with a non-empty category field. Cf TakeUncategorized";
TakeUncategorized::usage = "TakeUncategorized[journal] returns the journal with only \
the entries with an empty category field. Cf TakeCategorized";


(* ::Subsection::Closed:: *)
(*Journal file handling*)


GetJournalDir::usage = "GetJournalDir[] returns the directory used for journals.";
SetJournalDir::usage = "SetJournalDir[directory] sets the directory used for journals.";


ReadJournal::usage = "ReadJournal[account, year] reads the journal for given year \
and account.
ReadJournal[journal] reads the saved journal with account and year corresponding to\[NonBreakingSpace]\
those of the argument journal. If the latter is with mixed years/accounts, \
will instead give ReadJournal[___, False, ___]. 

ReadJournal[] reads and merges all journals.
ReadJournal[account] reads and merges all journals for all years for the given account.
ReadJournal[year] reads and merges all journals for all accounts for the given year.";

WriteToJournal::usage = "WriteToJournal[journal] splits the journal by account \
and year and adds the entries to the existing journal file in \
Journals/accountName/year.csv.";


ListAccountsWithJournals::usage = "ListAccountsWithJournals[] lists the names of \
accounts that have journal files. Gives a warning if it finds journals whose accounts \
are not found by GetAccounts[].";
(* ::Section:: *)
(*Categorization*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Categorization form*)


CategorizationForm::usage = "CategorizationForm[journal] sets up a form for filling \
out categories for a journal.";
ExtractSelectedCategories::usage = "ExtractSelectedCategories[form] extracts the \
chosen categories from a CategorizationForm.";


(* ::Subsection::Closed:: *)
(*Categorization prediction*)


TrainCategoryClassifier::usage = "TrainCategoryClassifier[journal] returns a \
PredictionFunction trained to predict the category of a transaction from its \
description, amount, and account.";
(* ::Section:: *)
(*Ledger*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Ledger object*)


IsLedger::usage = "IsLedger[obj_] returns True if obj is recognized \
as a Ledger, False otherwise."
CreateLedger::usage = "CreateLedger[journal] creates a ledger from a journal.";


GetBalancesFromLedger::usage = "GetBalancesFromLedger[ledger] gives debit - credit \
for each account and category of expense in ledger.";


(* ::Subsection::Closed:: *)
(*Ledger file handling*)


GetLedgerDir::usage = "GetLedgerDir[] returns the directory used for ledgers.";
SetLedgerDir::usage = "SetLedgerDir[directory] sets the directory used for ledgers.";


ReadLedger::usage = "ReadLedger[year, month] reads the ledger for given year \
and month.";

WriteToLedger::usage = "WriteToLedger[ledger] splits the ledger by year \
and month and adds the entries to the existing ledger file in \
GetLedgerDir[] <> /year/month.csv.";
WriteLedgerFromJournalFiles::usage = "WriteLedgerFromJournalFiles[year] reads \
entries from all journals for the given year, creates corresponding ledger, and \
writes to file.";
(* ::Section:: *)
(*Balances*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Balances objects*)


CreateBalancesObject::usage = "CreateBalancesObject[date, accountBalances] creates \
a balances object with the given date and accountBalances.

CreateBalancesObject[date, <|accountName -> balance, ...|>] creates a balances object \
with balances for given accounts.";

IsBalances::usage = "IsBalances[obj] returns True if obj is recognized as a balances \
object, false otherwise.";
IsAccountBalances::usage = "IsAccountBalances[obj] returns True if obj is a list \
of account balances, false otherwise.";


(* ::Subsection::Closed:: *)
(*Balances input form*)


BalancesInputForm::usage = "form = BalancesInputForm[date] gives an input form \
for recording balances for a given date, prefilled with balances calculated from \
existing data. Use ExtractBalances[form] to get result when done filling out.";
ExtractBalances::usage = "ExtractBalances[form] returns a Balances object created \
from the given form. Expects a form from BalancesInputForm.";


(* ::Subsection::Closed:: *)
(*Balances file handling*)


GetBalancesDir::usage = "GetBalancesDir[] returns the directory used for balances.";
SetBalancesDir::usage = "SetBalancesDir[directory] sets the directory used for balances.";


ReadBalances::usage = "ReadBalances[date] reads the balance file for the given date.";

WriteToBalances::usage = "WriteToBalances[balances] writes the given balances object \
to GetBalancesDir[]/date.csv";
(* ::Section:: *)
(*Presentation*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Presentation common*)


FormattedGrid::usage = "Formats a two dimensional table of numbers into a Grid. \
Numbers are rounded to nearest integer. Handles list of lists, association of list \
(keys added as row names, list of associations (keys added as column names), \
and association of associations (interpreted as having both row and column names).";


(* ::Subsection::Closed:: *)
(*Budget sheet*)


CreateBudgetSheet::usage = "CreateBudgetSheet[ledger, budget, budgetCategories] \
groups the expenses in ledger according to budgetCategories and compares with the \
given budget in a budget sheet.";


(* ::Subsection::Closed:: *)
(*Year balance sheet*)


CreateYearBalanceSheet::usage = "CreateYearBalanceSheet[ledger, incomingBalances] \
presents the balance for all bank accounts (taken from ListBankAccounts) together \
with their changes by month and counting from incomingBalances, given the entries in\[NonBreakingSpace]\
ledger. Uses GetAccountCategories[].";

SetAccountCategories::usage = "SetAccountCategories[<|\"cat1\" -> {__accounts}, \
\"cat2\" -> {__accounts}, ...|>] 
 sets the current account categories explicitly, used for CreateYearBalanceSheet.
SetAccountCategories[{checkingAccounts, savingsAccounts}] sets the current accounts to \
<|\"Checking Accounts\" -> checkingAccounts, \"Savings Accounts\" -> savingsAccounts|>.";
GetAccountCategories::usage = "GetAccountCategories[] returns the current account \
categories, used for CreateYearBalanceSheet.
If categories are not set, gives a warning and returns a split of ListBankAccounts[] \
with placeholder category names.";
(* ::Chapter:: *)
(*Implementations*)
Begin["`Private`"];
(* ::Section:: *)
(*Common*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Misc.*)


(* Only tested implicitly *)
SetAttributes[messageIfNot, HoldAll]
messageIfNot[condition_, message_, messageArgs___] :=
 If[condition, True, Message[message, messageArgs]; False]


(* ::Subsection::Closed:: *)
(*Dates*)


toDateString[date_] := DateString[date, {"Year", "-", "Month", "-", "Day"}]


sortByDateDescending[list_] /; And @@ (KeyExistsQ[#, "date"] & /@ list):=
 list[[Ordering[getListOfDates@list, All, OrderedQ[{#2, #1}] &]]]
 
getListOfDates[list_] := 
 Normal[DateList /@ list[[All, "date"]]]


splitByYear[table_Dataset] := Dataset /@ splitByYear[Normal@table]
splitByYear[table_] :=
 KeySort@GroupBy[table, DateList[#[["date"]]][[1]] &]
 
splitByMonthAndYear[table_Dataset] := Dataset /@ splitByMonthAndYear[Normal@table]
splitByMonthAndYear[table_] :=
 KeySort@GroupBy[table, DateList[#[["date"]]][[;;2]] &]


(* ::Subsection::Closed:: *)
(*Data structure functions*)


HasKeysQ[assoc_ /; ListQ@assoc || AssociationQ@assoc , keys_] := 
 And @@ (KeyExistsQ[assoc, #] & /@ keys)


(* ::Subsection::Closed:: *)
(*File/Directory handling*)


EnsureDirectoryExists[dir_String] := 
 If[Not@FileExistsQ@dir, CreateDirectory[dir]]


SetDataDirectories[dir_String] := {
 SetJournalDir[FileNameJoin[{dir, "Journals"}] <> $PathnameSeparator],
 SetLedgerDir[FileNameJoin[{dir, "Ledger"}] <> $PathnameSeparator],
 SetBalancesDir[FileNameJoin[{dir, "Balances"}] <> $PathnameSeparator]
 }


importCSV[filename_String] :=
 With[{imported = Import[filename, "CSV"]},
  AssociationThread[
   First@imported (* First row is header *) -> #] & /@ Rest@imported
 ]
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
 AddBankAccount[accountName, "USD", filePatternBoA, importBoA]


filePatternBoA := (*Alternatives @@ *)fileTypesBoA[[All, 1]];
fileTypesBoA = 
 {{txtPatternBoA, importBoAtxt}, 
  {qfxPatternBoA, importBoAqfx}};
importBoA[fileName_String, account_String] := 
 With[{importFunction = 
  Select[fileTypesBoA, StringMatchQ[FileNameTake@fileName, #[[1]]] &][[1, 2]]
  },
  importFunction[fileName, account]
 ]


txtPatternBoA = "stmt" ~~ ___ ~~ ".txt";
importBoAtxt[filename_String, account_String] := 
 CreateJournal[
  handleBoALine[account] /@ Reverse@extractTableBoA@Import[filename]]
 
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


qfxPatternBoA = ___ ~~ ".qfx";
importBoAqfx[filename_String, account_String] := 
 CreateJournal[
  CreateJournalEntry[##2, 0.0, account, "USD", "", "FITID" -> #1] & @@@ (
   getXMLpart[Import[filename]] // addEndTags // 
    XML`Parser`XMLGetString // extractTransactions
   )
 ]
(* not tested individually *)
getXMLpart[str_String] := StringReplace[str, ___ ~~ xml : ("<OFX>" ~~ ___) :> xml]
getXMLpart[notString_] := 
 (* Sometimes Import on qfx doesn't give string as element, this probably fixes... *)
 getXMLpart@StringReplace[ToString@notString, 
  {"}, {" -> "\n", "{{" -> "", "}}" -> ""}]

addEndTags[str_String] := StringReplace[str, 
  white : WhitespaceCharacter... ~~ 
   "<" ~~ tag : Except[">"].. ~~ ">" ~~ 
   elem : Except["\n"]..
  :> white ~~ "<" ~~ tag ~~ ">" ~~ elem ~~ "</" ~~ tag ~~ ">"
]
extractTransactions[xml : XMLObject["Document"][__]] := Cases[xml,
 XMLElement["STMTTRN", {}, 
    {___, XMLElement["DTPOSTED", {}, {time_}], ___,
     XMLElement["TRNAMT", {}, {amount_}], ___,
     XMLElement["FITID", {}, {FITID_}], ___,
     XMLElement["NAME",{},{description_}],___}] :> 
   {FITID, DateList[StringTake[time, 8]][[ ;; 3]], description,
    ToExpression@StringReplace[amount, "," -> ""]}, Infinity]


(* ::Subsubsection::Closed:: *)
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
handleNordeaLine[{}, __] := Sequence[]
handleNordeaLine[
  {dateString_String, description_String, type_String, amount_String, 
   balance_String}, account_String
  ] := CreateJournalEntry[
   dateString, description, parseSweNumberString@amount, parseSweNumberString@balance, 
   account, "SEK"]
   
parseSweNumberString[amount_String] :=
 ToExpression@StringReplace[amount, {" kr"->"", "." -> "", " "->"", ","->"."}]
(* ::Section:: *)
(*Journals*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Journal objects*)


(* ::Subsubsection::Closed:: *)
(*Journal*)


IsJournal[dataset_Dataset] := And@@(IsJournalEntry /@ dataset)
IsJournal[___] := False

CreateJournal[entries : {___?IsJournalEntry}] := 
 addIDs@Dataset[CreateJournalEntry /@ entries] 
CreateJournal[] := CreateJournal[{}]

addIDs::duplicate = "Warning! Duplicate entries in { \n`1` \n... }";
addIDs[journal_?IsJournal] := (
 messageIfNot[DuplicateFreeQ[Normal@journal], 
  addIDs::duplicate, ToString[Normal@journal[[ ;; 2]]]
  ];
  addID /@ journal)
 
addID[entry_?IsJournalEntry] := 
 If[KeyExistsQ[entry, "id"], entry,
  Append[entry, "id" -> 
   Hash[If[KeyExistsQ[entry, #], entry[#]] & /@ 
    {"date", "description", "amount", "account", "FITID"},
    "MD5"]]
]


(* ::Subsubsection::Closed:: *)
(*JournalEntry*)


CreateJournalEntry[] = CreateJournalEntry[{1, 1, 1}, "", 0., 0., "", "", ""];
CreateJournalEntry[{}] = CreateJournalEntry[];
CreateJournalEntry[date : {_Integer, _Integer, _Integer} | _String,
  description_, amount_?NumberQ, balance_?NumberQ, account_,
  currency_String, category_ : "", extra : (_ -> _)...] := 
 ensureStringFields@<|
 "date" -> toDateString@date, "description" -> description,
 "amount" -> amount, "balance" -> balance, "account" -> account, 
 "currency" -> currency, "category" -> category, extra
 |>
CreateJournalEntry[journalEntry_?IsJournalEntry] :=
 ensureStringFields@journalEntry
 
ensureStringFields[journalEntry_] :=
 MapAt[StringTrim@ToString@# &, 
  journalEntry, 
  {{"description"}, {"account"}, {"category"}}]


(* Fixes journalKeys at time of reading in package, so any fancy redefinitions
    afterwards of what a JournalEntry should look like needs to redefine IsJournalEntry
    too. *)
With[{journalKeys = Sort@Keys@CreateJournalEntry[]},
 IsJournalEntry[entry_Association] := HasKeysQ[entry, journalKeys];
 IsJournalEntry[___] := False;
]


(* ::Subsubsection::Closed:: *)
(*SetCategories*)


SetCategories::length = "Journal `1` and categories `2` not of equal length.";
SetCategories[journalIn_?IsJournal, categories_List] /; messageIfNot[
  Length@journalIn === Length@categories,
  SetCategories::length, Short@journalIn, Short@categories
 ] :=
 Module[{journal = Normal@journalIn}, 
  journal[[All, "category"]] = categories;
  Dataset@journal]


(* ::Subsubsection::Closed:: *)
(*ResetIDs*)


ResetIDs[journal_] := CreateJournal[KeyDrop[#, "id"]& /@ Normal@journal]


(* ::Subsubsection::Closed:: *)
(*AddCalculatedBalances*)


AddCalculatedBalances[journal_?IsJournal, incomingBalance_?NumericQ] :=
 With[{balances = calculateBalances[journal, incomingBalance]},
  CreateJournal@MapThread[
   Insert[#1, "calcBalance" -> #2, calcBalancePosition[#1]] &,
   {Normal@journal, balances},
   1
  ]
 ]

calcBalancePosition[entry_?IsJournalEntry] := 
 (* If calcBalance column exists, we replace it by inserting after.
     If not, insert after balance column. *)
 If[KeyExistsQ[entry, "calcBalance"],
  Position[Keys@entry, "calcBalance"] + 1,
  Position[Keys@entry, "balance"] + 1
 ]
 
calculateBalances[journal_?IsJournal, incomingBalance_?NumericQ] := 
  (journal[Reverse, "amount"] // Normal // Accumulate // Reverse) + incomingBalance //
   specialRound

(* 
Can't get Round to chop off digits from accumulated rounding errors for some reason:
 ExportString[808.1800000000001, "CSV"] \[Rule] "808.1800000000001"
 ExportString[Round[808.1800000000001, 0.0001], "CSV"] \[Rule] "808.1800000000001"
 ExportString[specialRound[808.1800000000001], "CSV"] \[Rule] "808.18"
*)
SetAttributes[specialRound, Listable];
With[{nDecimalsToKeep = 4},
specialRound[r_?NumericQ] :=
 N@Round[#, 10^(-nDecimalsToKeep)]& @
  Total[Take[NumberExpand@r, 1 + nDecimalsToKeep + Last@MantissaExponent@r]]
]


(* ::Subsubsection::Closed:: *)
(*TakeCategorized*)


TakeCategorized[{}] := {}
TakeCategorized[journal_?IsJournal] := journal[Select[#category != "" &]]

TakeUncategorized[{}] := {}
TakeUncategorized[journal_?IsJournal] := journal[Select[#category == "" &]]


(* ::Subsection::Closed:: *)
(*Journal file handling*)


Module[{journalDir = ""},
 SetJournalDir[dir_String] := journalDir = dir;
 GetJournalDir[] := journalDir
]


ListAccountsWithJournals::extraFiles = "Warning: found files not recognized as beloning \
to journals - `1`";
ListAccountsWithJournals[] :=
 With[{journalFolders = 
   FileNameTake /@ FileNames[Except["."] ~~ __, GetJournalDir[]]},
  messageIfNot[Not@MemberQ[journalFolders, x_ /; Not@BankAccountNameQ@x],
   ListAccountsWithJournals::extraFiles, 
   Select[journalFolders, Not@BankAccountNameQ@# &]
  ];
  Select[journalFolders, BankAccountNameQ]
 ]


ReadJournal[journal_?IsJournal] := 
 (* If journal is with mixed years/accounts, will give ReadJournal[___, False, ___] *)
 ReadJournal[getJournalAccount@journal, getJournalYear@journal]
ReadJournal[account_String, year_Integer] := 
 With[{filename = formatJournalFilename[account, year]},
  If[FileExistsQ@filename,
   readJournalFile[filename],
   CreateJournal[]
   ]
 ]
 
ReadJournal[account_String] :=
 mergeJournals[readJournalFile /@ 
  FileNames[journalFilenamePattern, formatJournalDirectory@account]]
ReadJournal[year_Integer] :=
 mergeJournals[ReadJournal[#, year] & /@ ListAccountsWithJournals[]]
ReadJournal[] :=
 mergeJournals[ReadJournal /@ ListAccountsWithJournals[]]

readJournalFile[filename_String] := CreateJournal@importCSV[filename]


WriteToJournal[journal_?IsJournal] := 
 (writeToJournalSingleFile /@ splitJournalByYear[#]) & /@ splitJournalByAccount@journal

writeToJournalSingleFile[journalIn_?IsJournal] := 
 With[{journal = mergeJournals[journalIn, ReadJournal[journalIn]]},
  ensureJournalDirectoriesExists@journal;
  Export[formatJournalFilename@journal, journal]
 ]


formatJournalDirectory[account_String] :=
 GetJournalDir[] <> account <> $PathnameSeparator
formatJournalFilename[account_String, year_Integer] :=
 formatJournalDirectory@account <> ToString@year <> ".csv"
formatJournalFilename[journal_?IsJournal] := 
 With[{account = getJournalAccount@journal, year = getJournalYear@journal},
  If[account === False || year === False,
   False,
   formatJournalFilename[account, year]
   ]
 ]
journalFilenamePattern = NumberString ~~ ".csv"

(* Only indirectly tested *)
ensureJournalDirectoriesExists[journal_?IsJournal] := 
 ensureJournalDirectoriesExists[getJournalAccount@journal]
ensureJournalDirectoriesExists[account_String] := 
 EnsureDirectoryExists@formatJournalDirectory@account
 
getJournalAccount[journal_?IsJournal] :=
 With[{accounts = Union[Normal@journal[All, "account"]]},
  If[Length@accounts > 1, 
   False,
   First@accounts]
 ]

getJournalYear[journal_?IsJournal] :=
 With[{years = Union[yearFromDateString /@ Normal@journal[All, "date"]]},
  If[Length@years > 1, 
   False,
   First@years]
 ]
 
yearFromDateString[date_String] := First@DateList@date


splitJournalByAccount[journal_?IsJournal] := 
 Function[acc, journal[Select[#account == acc &]]] /@ Normal@journal[Union, "account"]

splitJournalByYear[journal_?IsJournal] := 
 Values@splitByYear@journal


mergeJournals[journal1_?IsJournal, journal2_?IsJournal] :=
 mergeJournals[{journal1, journal2}]
mergeJournals[journals : {__?IsJournal}] :=
 CreateJournal@sortByDateDescending[
  DeleteDuplicatesBy[#["id"]&][Join@@(Normal /@ journals)]
  ]
(* ::Section:: *)
(*Categorization*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Categorization form*)


CategorizationForm[journal_?IsJournal] := 
  addCategorizationFormHeader[journal[1, "account"],
   Grid[categorizationFormRow /@ Normal@journal]
  ]
addCategorizationFormHeader[account_String, form_] :=
 Labeled[form, Style[account, "Subsubsection"], {Top}]


(* Note: not tested for Dynamic category-variable functionality 
   (most important part of course, but requires some arcane workaround to get right
   behavior when run without notebook?) *)
categorizationFormRow[entry_?IsJournalEntry] :=
 With[{category = Unique@"category", fieldSize = 15},
  Join[
   Values[entry[[{"date", "description", "amount"}]]], 
   {
    PopupMenu[Dynamic[category], getCategories[entry], FieldSize -> fieldSize],
    InputField[Dynamic[category], String, FieldSize -> fieldSize]
   }
  ]
 ]
 
(* Placeholder *)
getCategories[entry_?IsJournalEntry] := {entry[["category"]]}


ExtractSelectedCategories[categorizationForm_] :=
 Cases[categorizationForm, InputField[_[category_], __] :> category, All]


(* ::Subsection::Closed:: *)
(*Categorization prediction*)


TrainCategoryClassifier[journal_?IsJournal] :=
 Classify@formatTrainingData@TakeCategorized@journal


formatTrainingData[journal_?IsJournal] := 
 (* #1 becomes "category" due to Prepend *)
 {##2} -> #1 & @@@ Normal@journal[All, Prepend[featureKeys[], "category"]]
 
featureKeys[] := {"description", "amount", "account"}
(* ::Section:: *)
(*Ledger*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Ledger object*)


(* ::Subsubsection::Closed:: *)
(*Ledger*)


CreateLedger[journal_?IsJournal] := 
 Dataset[Join@@(journalEntryToLedgerLines /@ Normal@journal)]
 
CreateLedger[] := CreateLedger[{}]
CreateLedger[ledgerLines : {___?isLedgerLine}] := Dataset@ledgerLines
 
IsLedger[dataset_Dataset] := And @@ (isLedgerLine /@ dataset)
IsLedger[___] := False


(* ::Subsubsection::Closed:: *)
(*LedgerLine*)


createLedgerLine[] := createLedgerLine["2001-01-01", "", "", 0, "", "", 0]

createLedgerLine[dateString_String,
  account_String, debit_?numberOrEmptyStringQ, credit_?numberOrEmptyStringQ, 
  currency_String, description_, id_Integer] :=
 Association["date" -> dateString, "account" -> ToString@account, 
  "debit" -> debit, "credit" -> credit, "currency" -> currency, 
  "description" -> ToString@description, "id" -> id]

numberOrEmptyStringQ[""] := True
numberOrEmptyStringQ[x_] := NumberQ@x
numberOrEmptyStringQ[___] := False


With[{ledgerLineKeys = Sort@Keys@createLedgerLine[]},
 isLedgerLine[entry_Association] := HasKeysQ[entry, ledgerLineKeys];
 isLedgerLine[___] := False;
]


journalEntryToLedgerLines[entry_?IsJournalEntry] := 
 Module[{debit, credit, ledgerLines},
  If[entry["amount"] > 0,
   debit = entry["amount"]; credit = "";,
   debit = ""; credit = Abs@entry["amount"];
  ];
  
  ledgerLines = List@createLedgerLine[
   entry["date"], entry["account"], debit, credit,
   entry["currency"], entry["description"], entry["id"]
   ];
   
  (* Entries with category "Internal" get only one ledger line, the matching line
    should come from the other account involved. *)
  If[entry["category"] != "Internal",
   AppendTo[ledgerLines, 
    createLedgerLine[
     entry["date"], entry["category"], credit, debit,
     entry["currency"], entry["description"], entry["id"]]]
  ];
  
  ledgerLines
]


(* ::Subsubsection::Closed:: *)
(*GetBalancesFromLedger*)


GetBalancesFromLedger[ledger_?IsLedger] :=
 Query[GroupBy[#account&], <|"balance" -> Total /* (Subtract@@# &)|>,
  {"debit", "credit"}]@setMissingDebitCreditToZero@ledger // Normal
  
setMissingDebitCreditToZero[ledger_?IsLedger] :=
 MapAt[If[# == "", 0, #] &, ledger, {{All, "debit"}, {All, "credit"}}]


(* ::Subsection::Closed:: *)
(*Ledger file handling*)


Module[{ledgerDir = ""},
 SetLedgerDir[dir_String] := ledgerDir = dir;
 GetLedgerDir[] := ledgerDir
]


ReadLedger[year_Integer, month_Integer] := 
 With[{filename = formatLedgerFilename[year, month]},
  If[FileExistsQ@filename,
   readLedgerFile[filename],
   CreateLedger[]
   ]
 ]
ReadLedger[year_Integer] := 
 mergeLedgers[ReadLedger[year, #] & /@ Range[12]]
 
readLedgerFile[filename_String] := CreateLedger@importCSV[filename]
mergeLedgers[ledgers: {__?IsLedger}] := Join@@Reverse[ledgers]


WriteLedgerFromJournalFiles[year_Integer] :=
 WriteToLedger@CreateLedger@ReadJournal[year]

WriteToLedger[ledger_?IsLedger] := 
 writeToLedgerSingleFile /@ splitLedgerByMonthAndYear@ledger
 
writeToLedgerSingleFile[ledger_?IsLedger] := (
 ensureLedgerDirectoriesExists@ledger;
 Export[formatLedgerFilename@ledger, ledger])


splitLedgerByMonthAndYear[ledger_?IsLedger] := 
 Values@splitByMonthAndYear@ledger


formatLedgerDirectory[year_Integer] := 
 FileNameJoin[{GetLedgerDir[], ToString@year}] <> $PathnameSeparator 
formatLedgerFilename[year_Integer, month_Integer] := 
 formatLedgerDirectory[year] <> DateString[{year, month}, {"MonthName"}] <> ".csv"
 
formatLedgerDirectory[ledger_?IsLedger] :=
 With[{yearMonthPairs = Union[getYearAndMonth /@ Normal@ledger]},
  If[Length@yearMonthPairs == 1,
   formatLedgerDirectory[First@First@yearMonthPairs],
   False
  ]
 ]
formatLedgerFilename[ledger_?IsLedger] :=
 With[{yearMonthPairs = Union[getYearAndMonth /@ Normal@ledger]},
  If[Length@yearMonthPairs == 1,
   formatLedgerFilename@@First@yearMonthPairs,
   False
  ]
 ]
getYearAndMonth[ledgerLine_?isLedgerLine] :=
 DateList[ledgerLine[["date"]]][[;;2]]


ensureLedgerDirectoriesExists[ledger_?IsLedger] := 
 EnsureDirectoryExists@formatLedgerDirectory@ledger
(* ::Section:: *)
(*Balances*)
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
(* ::Section:: *)
(*Presentation*)
(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Presentation common*)


FormattedGrid[table_] /; Length@table > 0 :=
 Grid[round@moveRowAndColumnNamesIntoTable@table,
  Dividers -> {False, {-2 -> True}},
  Alignment -> {{Right, 1 -> Left}}
 ]
moveRowAndColumnNamesIntoTable[tableIn_] :=
 Module[{table = tableIn},
  (* Remove column keys*)
  table = If[AssociationQ@#, Values@#, Flatten@{#}] & /@ table;
  (* Move row keys into rows *)
  If[AssociationQ@table, table = KeyValueMap[Prepend[#2, #1] &, table]];
  (* Add column keys as header list *)
  If[AssociationQ@tableIn[[1]], PrependTo[table, Keys@tableIn[[1]]]];
  (* If there where row and column keys, header is one item short, add empty string *)
  If[Length@table[[1]] < Length@table[[2]], table[[1]] = Prepend[table[[1]], ""]];
  
  table
 ]

SetAttributes[round, Listable]; round[s_String] := s; round[x_] := Round[x, 1];


addTotalFooter::usage = "addTotalFooter[table, label : Total] takes a table and \
appends a footer row giving the totals of each column. If the table is an association \
(has row labels), the given label is used for the footer. Non-numeric entries are \
treated as being zero.";
addTotalFooter[table_, label_ : "Total"]  /; Length@table > 0 := 
 Append[table, 
  If[AssociationQ@table, label -> #, #]&@Query[nonNumericToZero /* Total]@table]
  
nonNumericToZero[x_?NumericQ] := x
nonNumericToZero[x_ /; Length@x == 0] := 0
nonNumericToZero[lst_] := nonNumericToZero /@ lst


(* ::Subsection::Closed:: *)
(*Budget sheet*)


isCategoryGroups[categoryGroups_Association] := 
 And@@(AssociationQ /@ categoryGroups) && 
 And@@((And@@(ListQ /@ Values@# ))& /@ 
  categoryGroups)
isCategoryGroups[___] := False


CreateBudgetSheet[ledger_?IsLedger, budget_, categoryGroups_?isCategoryGroups] :=
 formatBudgetSheet[
  createBudgetData[ledger, budget, categoryGroups],
  formatBudgetSheetTitle@ledger
  ]

createBudgetData[ledger_?IsLedger, budget_, categoryGroups_?isCategoryGroups] :=
 addBalancesToCategoryGroups[
  ledger, 
  addNonListedCategories[addCategoryNamesToCategories@categoryGroups, ledger]
  ] // addBudgetAndDifferencesEntries[#, budget] & //
   changeSignOnIncome // addTotalFooter /@ # & //
    DeleteCases[#, addTotalFooter[__]] & // (* Remove missing tables *)
     addBudgetSummaryTable 
    
formatBudgetSheetTitle[ledger_?IsLedger] :=
 DateString[ledger[1, "date"], {"MonthName", " ", "Year"}]
 (* Add range if more months *)


formatBudgetSheet[categoriesWithBalances_, title_String] :=
 layoutBudgetSheet[
  FormattedGrid /@ categoriesWithBalances // addTableHeadings,
  title]
  
layoutBudgetSheet[formattedGrids_Association, title_String] :=
 Grid[{
   {Style[title, "Subsection"], SpanFromLeft, SpanFromLeft},
   ifKeyExistsElseSpanLeft[formattedGrids, #] & /@ {"Income", "Savings", "Summary"},
   {Item[formattedGrids["Expenses"], Alignment -> Left], 
    SpanFromLeft, 
    ifKeyExistsElseSpanLeft[formattedGrids, "Non-recurring costs", Left]}
   }, Spacings->{5, 3}]
   
ifKeyExistsElseSpanLeft[assoc_, key_, alignment_ : Bottom] :=
 If[KeyExistsQ[assoc, key], 
  Item[assoc[key], Alignment -> alignment],
  SpanFromLeft]
  
addTableHeadings[tables_Association] :=
 Association@KeyValueMap[
  #1 -> Insert[#2, Flatten[{#1, Array[""&, Length[#2[[1]]] - 1]}], {1, 1}] &,
  tables]


addBalancesToCategoryGroups[ledger_?IsLedger, categoryGroups_?isCategoryGroups] :=
 With[{balances = GetBalancesFromLedger[ledger]},
  sumBalancesPerCategoryGroup[balances, #] & /@ categoryGroups
 ]
 (* Add changing currency, normalizing for non-single-month ledgers*)

sumBalancesPerCategoryGroup[balances_, categoriesGroup_] :=
 Query[# /* nonNumericToZero /* Total, 1]@balances & /@ categoriesGroup


addNonListedCategories[categoryGroups_?isCategoryGroups, ledger_?IsLedger] := 
 Append[categoryGroups,
  "Non-recurring costs" -> Association[# -> {#} & /@ 
    getUncategorized[ledger, categoryGroups]]
  ]

getUncategorized[ledger_?IsLedger, categoryGroups_?isCategoryGroups] :=
 Complement[Normal@ledger[All, "account"],
  Flatten@Values[Join@@Values@categoryGroups],
  ListBankAccounts[]] 


addCategoryNamesToCategories[categoryGroups_?isCategoryGroups] :=
 Association@KeyValueMap[#1 -> Prepend[#2, #1] &, #] & /@ categoryGroups


addBudgetAndDifferencesEntries[categoriesWithBalances_, budget_] :=
 MapAt[
  Append[#, "Diff." -> Subtract@@#] & /@ mergeBudgetAndExpenses[budget, #] &
  , categoriesWithBalances, 1]
mergeBudgetAndExpenses[budget_, expenses_] :=
 Merge[{budget, expenses}, <|"Budget" -> #[[1]], "Result" -> #[[2]]|> &]


changeSignOnIncome[categoriesWithBalances_] :=
 MapAt[Query[All, Minus], categoriesWithBalances, "Income"]


addBudgetSummaryTable[categoriesWithBalances_] :=
 Append[categoriesWithBalances, 
  "Summary" -> addTotalFooter@
    <|"Income" -> categoriesWithBalances["Income", "Total"], 
      "Expenses" -> -categoriesWithBalances["Expenses", "Total", "Result"],
      If[KeyExistsQ[categoriesWithBalances, "Savings"],
       "Savings" -> -categoriesWithBalances["Savings", "Total"],
       <||>]
      |>]
(* Generalize to work with any number of category groups with whatever names *)


(* ::Subsection::Closed:: *)
(*Year balance sheet*)


CreateYearBalanceSheet[ledger_?IsLedger, incomingBalance_?IsBalances] := 
 CreateYearBalanceSheet[{ledger}, incomingBalance]

CreateYearBalanceSheet[ledgers : {__?IsLedger}, incomingBalance_?IsBalances] :=
 createYearBalanceSheetLedgerSplitByMonth[
  splitLedgerByMonthAndYear[Join@@ledgers], 
  incomingBalance]


Module[{
 accountCategories = "not set"
 },
 
GetAccountCategories::notSet = "Warning: Account categories not set, using \
placeholders. Use SetAccountCategories to set up.";
GetAccountCategories[] := 
 If[accountCategories === "not set",
  Message[GetAccountCategories::notSet];
   <|"Some accounts" -> #1, "Rest of accounts" -> #2|>& @@
    Partition[#, Ceiling[Length@#/2]]& @ ListBankAccounts[],
  accountCategories
 ];
 
SetAccountCategories[{checkingAccounts : {__String}, savingsAccounts : {__String}}]:=
 SetAccountCategories[<|
  "Checking Accounts" -> checkingAccounts, 
  "Savings Accounts" -> savingsAccounts|>];
SetAccountCategories[categories_Association] :=
 accountCategories = categories;

resetAccountCategories[] := accountCategories = "not set"
]


createYearBalanceSheetLedgerSplitByMonth[
  ledgers : {__?IsLedger}, incomingBalance_?IsBalances] :=
 formatYearBalanceSheet[
  createYearBalanceData[ledgers, incomingBalance],
  formatYearBalanceTitle@ledgers
  ]
 
createYearBalanceData[ledgers : {__?IsLedger}, incomingBalance_?IsBalances] := 
 createAccountBalancesByMonth[ledgers, incomingBalance] //
  divideByAccountCategory// addYearBalanceSheetSummary

formatYearBalanceTitle[ledgers : {__?IsLedger}] :=
 DateString[ledgers[[1]][1, "date"], {"Year"}]


formatYearBalanceSheet[yearBalanceData_, title_] :=
 Labeled[#, Style[title, "Section"], {Top}, Spacings -> {Automatic, 2}] & @
  Column[
   KeyValueMap[
    Labeled[FormattedGrid@#2, Style[#1, "Subsubsection"], {{Top,Left}}] &,
    yearBalanceData],
   Spacings -> 3]


createAccountBalancesByMonth[ledgers : {__?IsLedger}, incoming_?IsBalances] :=
 addIncomingAndTotal[getMonthBalances[ledgers], getAccountAssoc@incoming]

getMonthBalances[ledgers : {__?IsLedger}] :=
 getMonthShort@# -> 
  Query[ListBankAccounts[], 1]@GetBalancesFromLedger@# & /@ ledgers // Association
getMonthShort[ledger_?IsLedger] := DateString[ledger[1, "date"], {"MonthNameShort"}]

addIncomingAndTotal[monthBalances_, incomingAccountAssoc_] := 
 RotateRight[#, 2] & /@ (addTotalFooter[#, "Balance"] & /@ (*fixMissingIncoming /@*)
  Query[Transpose]@RotateLeft@Prepend[monthBalances, "Incoming" -> incomingAccountAssoc])


divideByAccountCategory[accountBalances_Association] :=
 addTotalFooter /@ DeleteMissing[Query[GetAccountCategories[]]@accountBalances, 2]
 
addYearBalanceSheetSummary[categoryBalances_] := 
 Append[categoryBalances,
  "Summary"->addTotalFooter@Query[All, "Total"]@categoryBalances]
(* ::Section::Closed:: *)
(*Tail*)
End[];
(* ::Chapter::Closed:: *)
(*Tail*)
EndPackage[]
