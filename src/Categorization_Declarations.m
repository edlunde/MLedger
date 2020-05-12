(* ::Package:: *)

(* ::Subsection:: *)
(*Categorization form*)


CategorizationForm::usage = "CategorizationForm[journal] sets up a form for filling \
out categories for a journal.";
ExtractSelectedCategories::usage = "ExtractSelectedCategories[form] extracts the \
chosen categories from a CategorizationForm.";


(* ::Subsection:: *)
(*Categorization prediction*)


TrainCategoryClassifier::usage = "TrainCategoryClassifier[journal] returns a \
PredictionFunction trained to predict the category of a transaction from its \
description, amount, and account.";
