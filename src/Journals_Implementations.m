(* ::Package:: *)

(* ::Subsection:: *)
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


(* ::Subsection:: *)
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
