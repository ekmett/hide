{-# LANGUAGE OverloadedStrings #-}
module PluginFormCheck (checks) where
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.Form hiding (prepareForm)
import FormExtension
import Hide.Model
import Hide.GuestAccess (readableAt,guestKeyboardAllowed)

checks :: IO ()
checks=do
  withRegistry $ \registry->do
    prepared<-prepareForm registry (InputFormSpec "Rename" "Name" "Old" "Rename") (\() text->pure (Right text)) >>= right
    unopened<-refreshForm (formReference prepared) (InputFormSpec "New" "Name" "Reset" "Apply") >>= right
    check "escaped ref metadata cannot open an initial modal" (case unopened of Nothing->True; _->False)
    accepted<-admitForm False prepared
    check "prepared form opens once" accepted
    duplicate<-admitForm False prepared
    check "duplicate form open refuses" (not duplicate)
    old<-refreshForm (formReference prepared) (InputFormSpec "Old labels" "Name" "Reset" "Apply") >>= right >>= just
    latest<-refreshForm (formReference prepared) (InputFormSpec "Latest" "Name" "Reset" "Apply") >>= right >>= just
    stale<-admitFormRefresh prepared old
    check "newer metadata supersedes older publication" (case stale of Nothing->True; _->False)
    merged<-admitFormRefresh prepared latest >>= just
    check "metadata refresh retains exact form identity" (formReference merged==formReference prepared)
    claimed<-claimFormSubmission prepared
    check "label refresh does not revoke unchanged submission" claimed
    repeated<-claimFormSubmission merged
    check "form submission claims once" (not repeated)
    reply<-invokeFormAction merged () (TextValue "New")
    check "public typed form delivers submitted value on its worker" (reply==Right "New")
    pending<-submissionCurrent merged
    check "accepted submission survives its modal closing" pending
    consumed<-finishFormSubmission (formReference merged)
    repeatedResult<-finishFormSubmission (formReference merged)
    check "accepted result consumes once" (consumed && not repeatedResult)
    pure ()
  withRegistry $ \registry->do
    choices<-prepareForm registry (ChoiceFormSpec "Model" "Provider choices" [("small","Small"),("large","Large")] "small" "Apply") (\() value->pure (Right value)) >>= right
    _<-admitForm False choices
    let dg=Dialog "Private choices" (PluginChoiceForm (formReference choices) (formRevision choices)) [ListBox "Private values" ["Secret"] 0] 0 ["Apply","Cancel"] []
        desktop=(initialDesktop (80,25)) {dialog=Just dg}
        rectangle=dialogRect desktop dg
    check "generic choices keep private capture and human-only input"
      (formDisclosure (formReference choices)==PrivateForm && not (readableAt desktop (left rectangle+2) (top rectangle+2)) && not (guestKeyboardAllowed desktop))
    update<-refreshForm (formReference choices) (ChoiceFormSpec "Updated model" "Models" [("large","L"),("small","S")] "large" "Apply") >>= right >>= just
    refreshed<-admitFormRefresh choices update >>= just
    check "choice refresh preserves selected stable ID across reorder" (formChoiceAt choices 0==Just "small" && formChoiceIndex refreshed "small"==Just 1)
    changed<-refreshForm (formReference choices) (ChoiceFormSpec "Model" "Models" [("other","Other")] "other" "Apply")
    check "metadata refresh cannot replace the choice catalogue" (case changed of Left _->True; _->False)
    changedWidget<-refreshForm (formReference choices) (InputFormSpec "Model" "Name" "" "Apply")
    check "metadata refresh cannot replace the widget kind" (case changedWidget of Left _->True; _->False)
    invalid<-invokeFormAction refreshed () (TextValue "L")
    check "choice labels are not submit values" (case invalid of Left _->True; _->False)
    claimed<-claimFormSubmission refreshed
    value<-invokeFormAction refreshed () (TextValue "large")
    check "typed choices deliver the advertised stable ID" (claimed && value==Right "large")
    _<-finishFormSubmission (formReference refreshed)
    pure ()
  withRegistry $ \registry->do
    let spec=InputsFormSpec "Create" [InputField "task" "Task" "",InputField "name" "Name" ""] "Create"
        values=M.fromList [("name","Child"),("task","Count files")]
    prepared<-prepareInputsForm registry spec (\() submitted->pure (Right submitted)) >>= right
    _<-admitForm False prepared
    value<-invokeFormAction prepared () (InputValues values)
    check "named input actions bind stable IDs independently of presentation order" (value==Right values)
    changed<-refreshForm (formReference prepared) (InputsFormSpec "Create" [InputField "name" "Name" "",InputField "task" "Task" ""] "Create")
    check "input metadata cannot reorder its immutable schema" (case changed of Left _->True; _->False)
    missing<-invokeFormAction prepared () (InputValues (M.delete "task" values))
    extra<-invokeFormAction prepared () (InputValues (M.insert "other" "x" values))
    oversized<-invokeFormAction prepared () (InputValues (M.insert "task" (T.replicate 8193 "x") values))
    check "named values reject missing extra and overbound inputs" (all rejected [missing,extra,oversized])
    scalarShape<-prepareForm registry spec (\() value->pure (Right value))
    namedShape<-withRegistry $ \other->prepareInputsForm other (InputFormSpec "Single" "Name" "" "Apply") (\() values->pure (Right values))
    check "form preparation rejects spec action shape mismatches" (case (scalarShape,namedShape) of (Left InvalidArguments{},Left InvalidArguments{})->True; _->False)
    retireForm (formReference prepared)
  escaped<-withRegistry $ \registry->do
    unconsumed<-prepareForm registry (InputFormSpec "Live" "Name" "Still live" "Submit") (\() text->pure (Right text)) >>= right
    opened<-admitForm False unconsumed
    check "teardown fixture escapes a live open form" opened
    pure unconsumed
  live<-formCurrent escaped
  claimed<-claimFormSubmission escaped
  check "registration teardown rejects retained form and submission" (not live && not claimed)
  where
    rejected :: Either a b -> Bool
    rejected (Left _)=True
    rejected _=False
    check label ok=unless ok (fail label)
    right=either (fail.show) pure
    just :: Maybe a -> IO a
    just=maybe (fail "Missing form update") pure
