(* ::Package:: *)

(* ::Subsection:: *)
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


(* ::Subsubsection:: *)
(*GetBalancesFromLedger*)


GetBalancesFromLedger[ledger_?IsLedger] :=
 Query[GroupBy[#account&], <|"balance" -> Total /* (Subtract@@# &)|>,
  {"debit", "credit"}]@setMissingDebitCreditToZero@ledger // Normal
  
setMissingDebitCreditToZero[ledger_?IsLedger] :=
 MapAt[If[# == "", 0, #] &, ledger, {{All, "debit"}, {All, "credit"}}]


(* ::Subsection:: *)
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

readLedgerFile[filename_String] := CreateLedger@importCSV[filename]


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
