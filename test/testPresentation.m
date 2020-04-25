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


(* ::Subsection:: *)
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
   (* Check some values *)
   AssertEquals[{-783}, 
    Cases[budgetSheet, {___, "Expenses", x_?NumericQ, ___} :> x, All]];
   AssertEquals[{10}, 
    Cases[budgetSheet, {___, "Misc.", _, x_?NumericQ, ___} :> x, All]];
  ];
 ];
];
