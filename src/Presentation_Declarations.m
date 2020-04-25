(* ::Package:: *)

(* ::Subsection:: *)
(*Presentation common*)


FormattedGrid::usage = "Formats a two dimensional table of numbers into a Grid. \
Numbers are rounded to nearest integer. Handles list of lists, association of list \
(keys added as row names, list of associations (keys added as column names), \
and association of associations (interpreted as having both row and column names).";


(* ::Subsection:: *)
(*Budget sheet*)


CreateBudgetSheet::usage = "CreateBudgetSheet[ledger, budget, budgetCategories] \
groups the expenses in ledger according to budgetCategories and compares with the \
given budget in a budget sheet.";
