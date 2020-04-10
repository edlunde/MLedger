(* ::Package:: *)

(* ::Subsection:: *)
(*Ledger object*)


(* ::Subsubsection::Closed:: *)
(*Ledger*)


CreateLedger[journal_?IsJournal] := 
 Dataset[Join@@(journalEntryToLedgerLines /@ Normal@journal)]
 
CreateLedger[ledgerLines : {__?isLedgerLine}] := Dataset@ledgerLines
 
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
 isLedgerLine[entry_Association] := Complement[ledgerLineKeys, Keys@entry] === {};
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


(* ::Subsection:: *)
(*Ledger file handling*)


Module[{ledgerDir = ""},
 SetLedgerDir[dir_String] := ledgerDir = dir;
 GetLedgerDir[] := ledgerDir
]


ReadLedger[year_Integer, month_Integer] := {}


WriteToLedger[ledger_?IsLedger] := 
 writeToLedgerSingleFile /@ splitLedgerByMonthAndYear@ledger
 
writeToLedgerSingleFile[ledger_?IsLedger] := (
 ensureLedgerDirectoriesExists@ledger;
 Export[getLedgerFilename@ledger, ledger])


splitLedgerByMonthAndYear[ledger_?IsLedger] := 
 CreateLedger /@ GatherBy[Normal@ledger, getYearAndMonth]
getYearAndMonth[ledgerLine_?isLedgerLine] :=
 DateList[ledgerLine[["date"]]][[;;2]]


getLedgerFilename[ledger_?IsLedger] :=
 With[{yearMonthPairs = Union[getYearAndMonth /@ Normal@ledger]},
  If[Length@yearMonthPairs == 1,
   FileNameJoin[
    {GetLedgerDir[], 
     DateString[First@yearMonthPairs, {"Year", $PathnameSeparator, "MonthName"}]
      <> ".csv"}],
   False
  ]
 ]


ensureLedgerDirectoriesExists[ledger_?IsLedger] := 
 EnsureDirectoryExists[
  FileNameDrop[getLedgerFilename@ledger, -1] <> $PathnameSeparator
  ]
