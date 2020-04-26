(* ::Package:: *)

(* ::Text:: *)
(*If you prefer working with regular notebooks, change the file ending of this file to .nb*)
(*To add your own accounts, edit setupBankAccounts[] in ./setupAccounts.m*)


(* ::Subsection::Closed:: *)
(*Setup*)


Needs["MLedger`", ParentDirectory[NotebookDirectory[]] <> "/MLedger.m"];
Get[NotebookDirectory[] <> "setupAccounts.m"]


setupBankAccounts[]
SetJournalDir[NotebookDirectory[] <> "Journals/"]
SetLedgerDir[NotebookDirectory[] <> "Ledger/"]
SetBalancesDir[NotebookDirectory[] <> "Balances/"]


(* ::Subsection::Closed:: *)
(*Importing from accounts*)


importForm = SelectAccountsForm[
 files = ListImportableFiles[NotebookDirectory[] <> "toImport/"]]


imported = ImportAccountFiles[files, ExtractSelectedAccounts[importForm]]


WriteToJournal /@ imported


(* ::Subsection::Closed:: *)
(*Categorization*)


journal = ReadJournal[GetBankAccounts[][[1, "name"]]];


categoriesForExampleJournal =
 {"Banking fees","Banking fees","Mortgage","","Insurance","Internal","Salary",
  "","Electronics","Groceries","","","","","","",""};
journalWithCategories = SetCategories[journal, categoriesForExampleJournal];


form = CategorizationForm@journalWithCategories


categories = ExtractSelectedCategories[form]


updatedJournal = SetCategories[journalWithCategories, categories]


WriteToJournal@updatedJournal


(* ::Subsection::Closed:: *)
(*Write to ledger*)


WriteLedgerFromJournalFiles[2003]


(* ::Subsection:: *)
(*Input balances*)


(* ::Text:: *)
(*Create example balances*)


EnsureDirectoryExists[GetBalancesDir[]]
WriteToBalances@CreateBalancesObject[
 "2003-10-09", <|"Example BoA account" -> 0.55, "Example Nordea account" -> 0.55|>]


form = BalancesInputForm["2004-01-01"]


balances = ExtractBalances@form


readyToWrite = 0;
If[readyToWrite == 1, WriteToBalances@balances]


(* ::Subsection::Closed:: *)
(*Presentation*)


(* ::Subsubsection:: *)
(*Budget*)


exampleCategoryGroups = <|
 "Expenses" -> <|
  "Mortgage" -> {}, "Groceries" -> {}, (* Leaving Electronics uncategorized*)
  "Insurance" -> {}, "Misc." -> {"Banking fees"}|>,
 "Income" -> <|"Salary" -> {}|>,
 "Savings" -> <|"Investments" -> {}, "Rainy day fund" -> {}|>
 |>;
 
exampleBudget = <|
 "Mortgage" -> 711, "Groceries" -> 30,
 "Insurance" -> 34, "Misc." -> 10
 |>;


CreateBudgetSheet[ReadLedger[2003, 11], exampleBudget, exampleCategoryGroups]
