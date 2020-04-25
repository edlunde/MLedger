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


(* ::Subsection:: *)
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
    addBudgetSummaryTable
    
formatBudgetSheetTitle[ledger_?IsLedger] :=
 DateString[ledger[1, "date"], {"MonthName", " ", "Year"}]
 (* Add range if more months *)


formatBudgetSheet[categoriesWithBalances_, title_String] :=
 layoutBudgetSheet[
  FormattedGrid /@ categoriesWithBalances,
  title]
  
layoutBudgetSheet[formattedGrids_Association, title_String] :=
 Grid[{
   {Style[title, "Subsection"], SpanFromLeft, SpanFromLeft},
   Item[#, Alignment -> Bottom]& /@ 
    Values@formattedGrids[[{"Income", "Savings", "Summary"}]],
   {Item[formattedGrids["Expenses"], Alignment -> Left], 
    SpanFromLeft, 
    Item[formattedGrids["Non-recurring costs"], Alignment -> Left]}
   }, Spacings->{5, 3}]


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
      "Savings" -> -categoriesWithBalances["Savings", "Total"]|>]
(* Generalize to work with any number of category groups with whatever names *)
