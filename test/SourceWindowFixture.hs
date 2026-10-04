-- | Source-only fixtures require a real source identity; plugin windows fail here.
module SourceWindowFixture (sourceFixtureBuffer) where
import Hide.Model (Window,bufferId)
sourceFixtureBuffer :: Window -> Int
sourceFixtureBuffer window=case bufferId window of
  Just ident->ident
  Nothing->error "Source fixture unexpectedly received a plugin window"
