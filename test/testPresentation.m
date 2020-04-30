(* ::Package:: *)

AddSuite[MLedgerTests, presentationTests];


(* ::Subsection::Closed:: *)
(*Test presentation common*)


AddSuite[presentationTests, presentationCommonTests];


AddTest[presentationCommonTests, "testFormattedGrid",
 Module[{justTable, rowNames, colNames, wRowNames, wColumnNames, wBoth, tableList},
  justTable = RandomReal[{-10, 10}, {5, 3}];
  justTable[[3, 2;;3]] = {1.0, 1.0001};
  rowNames = "row" <> ToString@# &/@ Range@Length@justTable;
  colNames = "col" <> ToString@# &/@ Range@Length@justTable[[1]];

  wRowNames = AssociationThread[rowNames -> justTable];
  wColumnNames = AssociationThread[colNames -> #] & /@ justTable;
  wBoth = AssociationThread[rowNames -> wColumnNames];
  
  tableList = {justTable, wRowNames, wColumnNames, wBoth};
  
  AssertMatch[
   Grid[List[List__], ___], FormattedGrid@#] & /@ tableList;
  AssertMatch[Grid[List[colNames, List__], ___], FormattedGrid@wColumnNames];
  AssertMatch[Grid[{#, __} & /@ rowNames, ___], FormattedGrid@wRowNames];
  AssertMatch[Grid[Prepend[{#, __} & /@ rowNames, Flatten[{"", colNames}]], ___], 
   FormattedGrid@wBoth];
  
  (* test rounding of values *)
  AssertEquals[{1, 1}, FormattedGrid[justTable][[1, 3, 2;;3]]];
  
  (* Check for dividers and alignment *)
  AssertMatch[Grid[List[List__], ___, Dividers -> {False, {-2 -> True}}, ___], 
   FormattedGrid[justTable]];
  AssertMatch[Grid[List[List__], ___, Alignment -> {{Right, 1 -> Left}}, ___], 
   FormattedGrid[justTable]];
   
  (* Test works for 1d associations as well *)
  AssertMatch[Grid[{#, __} & /@ colNames, ___], FormattedGrid@First@wBoth]
 ];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[presentationCommonTests, presentationCommonTestsInternal]
AddTest[presentationCommonTestsInternal, "testAddTotalFooter",
 Module[{justTable, rowNames, colNames, wRowNames, wColumnNames, wBoth, tableList},
  justTable = {{1, -5, 7}, {-5, "a", -1}, {"3", -8, 6}, {-6, 0, 1}, {5, 6, 6}};
  rowNames = "row" <> ToString@# &/@ Range@Length@justTable;
  colNames = "col" <> ToString@# &/@ Range@Length@justTable[[1]];

  wRowNames = AssociationThread[rowNames -> justTable];
  wColumnNames = AssociationThread[colNames -> #] & /@ justTable;
  wBoth = AssociationThread[rowNames -> wColumnNames];
  
  tableList = {justTable, wRowNames, wColumnNames, wBoth};
  
  AssertEquals[{-5, -7, 19}, Last@addTotalFooter@#] & /@ {justTable, wRowNames};
  AssertEquals[AssociationThread[colNames -> {-5, -7, 19}], 
   Last@addTotalFooter@#] & /@ {wColumnNames, wBoth};
   
  AssertEquals["Total", Last@Keys@addTotalFooter@wRowNames];
  AssertEquals["Sum", Last@Keys@addTotalFooter[wRowNames, "Sum"]];
  
  (* Test works for 1d associations as well *)
  AssertEquals[6, Last@Values@addTotalFooter@<|"a" -> 1, "b" -> 2, "c" -> 3|>]
 ];
];
End[]; (* End "MLedger`Private`" *)


(* ::Subsection::Closed:: *)
(*Test budget sheet*)


AddSuite[presentationTests, budgetSheetTests];


AddTest[budgetSheetTests, "testCreateBudgetSheet",
 With[{ledgerWCategories = 
   CreateLedger@SetCategories[
    CreateJournal[CreateJournalEntry@@@exampleJournalData],
    categoriesForExampleJournal]},
  AssertTrue@IsLedger@ledgerWCategories;
  
  With[{budgetSheet = 
    CreateBudgetSheet[ledgerWCategories, exampleBudget, exampleCategoryGroups]},
   AssertMatch[Grid[List[__], __], budgetSheet];
   (* Check title *)
   AssertMatch[Grid[List[List[Style["November 2003", __], __], __], __], 
    budgetSheet];
   (* Check tables have headings *)
   AssertEquals[{#}, Cases[budgetSheet, 
    Grid[{{heading: #, "", ___}, ___}, ___] :> heading, 
    All]] & /@ 
     {"Income", "Expenses", "Savings", "Summary", "Non-recurring costs"};
   (* Check some values *)
   AssertEquals[{-783}, 
    Cases[budgetSheet, {___, "Expenses", x_?NumericQ, ___} :> x, All]];
   AssertEquals[{10}, 
    Cases[budgetSheet, {___, "Misc.", _, x_?NumericQ, ___} :> x, All]];
  ];
  
  (* Check handling Savings category not present *)
  With[{budgetSheet = 
    CreateBudgetSheet[ledgerWCategories, exampleBudget, 
     KeyDrop[exampleCategoryGroups, "Savings"]]},
   AssertMatch[Grid[List[__], __], budgetSheet];
   (* Check no missing keys *)
   AssertEquals[{}, Cases[budgetSheet, Missing["KeyAbsent", __], All]];
   (* Check no mentions of Savings *)
   AssertEquals[{}, Cases[budgetSheet, "Savings", All]];
   (* Check some values *)
   AssertEquals[{-783}, 
    Cases[budgetSheet, {___, "Expenses", x_?NumericQ, ___} :> x, All]];
   AssertEquals[{10}, 
    Cases[budgetSheet, {___, "Misc.", _, x_?NumericQ, ___} :> x, All]];
  ];
  (* Check handling no uncategorized entries present *)
  With[{budgetSheet = 
    CreateBudgetSheet[
     ledgerWCategories[Select[MemberQ[ (* Select entries present in category groups *)
     KeyValueMap[List,#] & /@ Values@exampleCategoryGroups//Flatten, 
      #account]&]], 
     exampleBudget, exampleCategoryGroups]},
   AssertMatch[Grid[List[__], __], budgetSheet];
   (* If CreateBudgetSheet tries to create "Non-recurring costs"-table without 
      such being present, among other things there will be this present*)
   AssertEquals[{}, Cases[budgetSheet, MLedger`Private`addTotalFooter[{},{}], All]];
   (* Check no mentions of Non-recurring costs *)
   (*AssertEquals[{}, Cases[budgetSheet, "Non-recurring costs", All]];*)
   (* Check some values *)
   AssertEquals[{-783}, 
    Cases[budgetSheet, {___, "Expenses", x_?NumericQ, ___} :> x, All]];
   AssertEquals[{10}, 
    Cases[budgetSheet, {___, "Misc.", _, x_?NumericQ, ___} :> x, All]];
  ];
 ];
];


(* ::Subsection:: *)
(*Test year balance sheet*)


AddSuite[presentationTests, yearBalanceSheetTests];


AddTest[yearBalanceSheetTests, "Set Up",
 SetBankAccounts[{}];
 AddBoAAccount/@{"BoA Checking", "BoA Savings"};
 MLedger`Private`resetAccountCategories[];
];

AddTest[yearBalanceSheetTests, "Tear Down",
 SetBankAccounts[{}];
];


AddTest[yearBalanceSheetTests, "testCreateYearBalanceSheet",
 With[{
  ledger = CreateLedger@CreateJournal[CreateJournalEntry@@@
    (* Create ledger with several months but only a single year *)
    Select[exampleJournalData2, First@DateList@First@#1 == 2003 &]
    ],
  balances = CreateBalancesObject[exampleJournalData2[[1, 1]],
   AssociationThread[ListBankAccounts[] -> {0.55, 0.55}]]
  },
  
  AssertTrue@IsLedger@ledger;
  AssertTrue@IsBalances@balances;
  AssertEquals[{"BoA Checking", "BoA Savings"}, ListBankAccounts[]];
  
  SetAccountCategories[{{"BoA Checking"}, {"BoA Savings"}}];
  With[{balanceSheet = 
    CreateYearBalanceSheet[ledger, balances]},
   AssertMatch[Labeled[Column[List[__], ___], ___], balanceSheet];
   
   (* Check title *)
   AssertMatch[Labeled[___, Style["2003", ___], ___], 
    balanceSheet];
    
   (* Check tables have headings *)
   AssertEquals[
    {"Checking Accounts", "Savings Accounts", "Summary"},
    Cases[balanceSheet,
     Labeled[Grid[List[List["", "Incoming", "Balance", __],__],___],
      Style[header_String,"Subsubsection"], ___] :> header,
     All]
    ];
    
   (* Check some values *)
   AssertEquals[{{1, -106}}, 
    Cases[balanceSheet, {"BoA Savings", vals__?NumericQ} :> {vals}[[;;2]], All]];
    
   (* Check months ordered *)
   AssertEquals[{{"Oct", "Nov"}}, 
    Union@Cases[balanceSheet, {"", "Incoming", "Balance", months__} :> {months}, All]];
  ];
 ];
];


AddTest[yearBalanceSheetTests, "testSetAccountCategories",
 (* Test behavior when not set. *)
 Module[{defaultCategories = ""},
  AssertMessage[GetAccountCategories::notSet,
   defaultCategories = GetAccountCategories[]];
  AssertEquals[
   <|"Some accounts" -> {"BoA Checking"}, "Rest of accounts" -> {"BoA Savings"}|>, 
   defaultCategories];
 ];
 
 (* Test just giving a split of accounts *)
 SetAccountCategories[{{"BoA Checking"}, {"BoA Savings"}}];
 AssertEquals[
  <|"Checking Accounts" -> {"BoA Checking"}, "Savings Accounts" -> {"BoA Savings"}|>,
  GetAccountCategories[]];
  
 (* Test explicit categories *)
 SetAccountCategories[
  <|"First accounts" -> {"BoA Checking", "BoA Savings"}, "No accounts" -> {}|>];
 AssertEquals[
  <|"First accounts" -> {"BoA Checking", "BoA Savings"}, "No accounts" -> {}|>,
  GetAccountCategories[]];
];
