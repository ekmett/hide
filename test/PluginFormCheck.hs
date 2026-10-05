{-# LANGUAGE OverloadedStrings #-}
module PluginFormCheck (checks) where
import Control.Monad (unless)
import Hide.Plugin.Command
import Hide.Plugin.Form
import FormExtension

checks :: IO ()
checks=do
  withRegistry $ \registry->do
    prepared<-prepareForm registry (InputFormSpec "Rename" "Name" "Old" "Rename") (\() text->pure (Right text)) >>= right
    unopened<-refreshInputForm (formReference prepared) (InputFormSpec "New" "Name" "Reset" "Apply") >>= right
    check "escaped ref metadata cannot open an initial modal" (case unopened of Nothing->True; _->False)
    accepted<-admitInputForm False prepared
    check "prepared form opens once" accepted
    duplicate<-admitInputForm False prepared
    check "duplicate form open refuses" (not duplicate)
    old<-refreshInputForm (formReference prepared) (InputFormSpec "Old labels" "Name" "Reset" "Apply") >>= right >>= just
    latest<-refreshInputForm (formReference prepared) (InputFormSpec "Latest" "Name" "Reset" "Apply") >>= right >>= just
    stale<-admitFormRefresh prepared old
    check "newer metadata supersedes older publication" (case stale of Nothing->True; _->False)
    merged<-admitFormRefresh prepared latest >>= just
    check "metadata refresh retains exact form identity" (formReference merged==formReference prepared)
    claimed<-claimFormSubmission prepared
    check "label refresh does not revoke unchanged submission" claimed
    repeated<-claimFormSubmission merged
    check "form submission claims once" (not repeated)
    reply<-invokeFormAction merged () "New"
    check "public typed form delivers submitted value on its worker" (reply==Right "New")
    pending<-submissionCurrent merged
    check "accepted submission survives its modal closing" pending
    consumed<-finishFormSubmission (formReference merged)
    repeatedResult<-finishFormSubmission (formReference merged)
    check "accepted result consumes once" (consumed && not repeatedResult)
    pure ()
  escaped<-withRegistry $ \registry->do
    unconsumed<-prepareForm registry (InputFormSpec "Live" "Name" "Still live" "Submit") (\() text->pure (Right text)) >>= right
    opened<-admitInputForm False unconsumed
    check "teardown fixture escapes a live open form" opened
    pure unconsumed
  live<-formCurrent escaped
  claimed<-claimFormSubmission escaped
  check "registration teardown rejects retained form and submission" (not live && not claimed)
  where
    check label ok=unless ok (fail label)
    right=either (fail.show) pure
    just :: Maybe a -> IO a
    just=maybe (fail "Missing form update") pure
