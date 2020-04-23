(* ::Package:: *)

AddSuite[MLedgerTests, presentationTests];


(* ::Subsection:: *)
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
 ];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[presentationCommonTests, presentationCommonTestsInternal]
AddTest[presentationCommonTests, "testAddTotalFooter",
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
 ];
];
End[]; (* End "MLedger`Private`" *)
