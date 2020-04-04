(* ::Package:: *)

(* ::Text:: *)
(*If you prefer working with regular notebooks, change the file ending of this file to .nb*)
(*To add your own accounts, edit setupBankAccounts[] in ./setupAccounts.m*)


(* ::Subsection:: *)
(*Setup*)


Needs["MLedger`", ParentDirectory[NotebookDirectory[]] <> "/MLedger.m"];
Get[NotebookDirectory[]<>"setupAccounts.m"]


setupBankAccounts[]
SetJournalDir[NotebookDirectory[] <> "Journals/"]


(* ::Subsection:: *)
(*Importing from accounts*)


importForm = SelectAccountsForm[
 files = ListImportableFiles[NotebookDirectory[] <> "toImport/"]]


imported = ImportAccountFiles[files, ExtractSelectedAccounts[importForm]]


WriteToJournal /@ imported
